namespace FSharp.Data.Adaptive

open FSharp.Data.Traceable

/// Collection extensions for aset<_>, amap<_,_>, alist<_>
[<AutoOpen>]
module CollectionExtensions =

    [<AutoOpen>]
    module internal Readers =
        /// Reader for ASet.sortBy
        [<Sealed>]
        type SetSortByReader<'T1, 'T2 when 'T2 : comparison>(set: aset<'T1>, projection: 'T1 -> 'T2) =
            inherit AbstractReader<IndexListDelta<'T1>>(IndexListDelta.empty)

            let reader = set.GetReader()
            let mapping = IndexMapping<Unique<'T2>>()
            let cache = Cache<'T1, Unique<'T2>>(projection >> Unique)

            override x.Compute(token: AdaptiveToken) =
                reader.GetChanges token |> Seq.choose (fun op ->
                    match op with
                    | Add(_, v) ->
                        let k = cache.Invoke v
                        let idx = mapping.Invoke k
                        Some (idx, Set v)
                    | Rem(_, v) ->
                        let k = cache.Revoke v
                        match mapping.Revoke k with
                        | Some idx -> Some (idx, Remove)
                        | None -> None
                )
                |> IndexListDelta.ofSeq
        
        /// Reader for ASet.sortWith
        [<Sealed>]
        type SetSortWithReader<'T>(set: aset<'T>, compare: 'T -> 'T -> int) =
            inherit AbstractReader<IndexListDelta<'T>>(IndexListDelta.empty)

            let reader = set.GetReader()
            let mapping = CustomIndexMapping<'T>(compare)

            override x.Compute(token: AdaptiveToken) =
                reader.GetChanges token |> Seq.choose (fun op ->
                    match op with
                    | Add(_, v) ->
                        let idx = mapping.Invoke v
                        Some (idx, Set v)
                    | Rem(_, v) ->
                        match mapping.Revoke v with
                        | Some idx -> Some (idx, Remove)
                        | None -> None
                )
                |> IndexListDelta.ofSeq

        /// Reader for AMap.keys
        [<Sealed>]
        type MapKeysReader<'Key, 'Value>(map: amap<'Key, 'Value>) =
            inherit AbstractReader<HashSetDelta<'Key>>(HashSetDelta.empty)

            let reader = map.GetReader()

            override x.Compute(token: AdaptiveToken) =
                let old = reader.State
                let mutable cnt = 0
                let ops = 
                    reader.GetChanges(token).Store |> HashMap.choose (fun key op ->
                        match op with
                        | Set _ ->
                            if HashMap.containsKey key old then None
                            else cnt <- cnt + 1; Some 1
                        | Remove ->
                            if HashMap.containsKey key old then cnt <- cnt + 1; Some -1
                            else None
                    )
                HashSetDelta(ops)

        /// Reader for AList.toASet
        [<Sealed>]
        type ListSetReader<'T>(list: alist<'T>) =
            inherit AbstractReader<HashSetDelta<'T>>(HashSetDelta.empty)
            
            let reader = list.GetReader()

            override x.Compute(token: AdaptiveToken) =
                let old = reader.State.Content
                reader.GetChanges(token).Content |> Seq.collect (fun (KeyValue(i, op)) ->
                    match op with
                    | Remove -> 
                        match MapExt.tryFind i old with
                        | Some v -> Seq.singleton (Rem v)
                        | None -> Seq.empty
                    | Set v ->
                        match MapExt.tryFind i old with
                        | Some ov ->
                            if DefaultEquality.equals v ov then Seq.empty
                            else [Add v; Rem ov] :> seq<_>
                        | None ->
                            Seq.singleton (Add v)
                )
                |> HashSetDelta.ofSeq

        /// Reader for AList.toIndexedASet
        [<Sealed>]
        type IndexedListSetReader<'T>(list: alist<'T>) =
            inherit AbstractReader<HashSetDelta<Index * 'T>>(HashSetDelta.empty)
            
            let reader = list.GetReader()

            override x.Compute(token: AdaptiveToken) =
                let old = reader.State.Content
                reader.GetChanges(token).Content |> Seq.collect (fun (KeyValue(i, op)) ->
                    match op with
                    | Remove -> 
                        match MapExt.tryFind i old with
                        | Some v -> Seq.singleton (Rem(i, v))
                        | None -> Seq.empty
                    | Set v ->
                        match MapExt.tryFind i old with
                        | Some ov ->
                            if DefaultEquality.equals v ov then Seq.empty
                            else [Add(i,v); Rem(i,ov)] :> seq<_>
                        | None ->
                            Seq.singleton (Add(i,v))
                )
                |> HashSetDelta.ofSeq

        /// Reader for AList.ofASet
        [<Sealed>]
        type ToListReader<'a>(input : aset<'a>) =
            inherit AbstractReader<IndexListDelta<'a>>(IndexListDelta.empty)

            let reader = input.GetReader()
            let mutable last = Index.zero
            let newIndex (v : 'a) =
                let i = Index.after last
                last <- i
                i

            let newIndex = Cache newIndex

            override x.Compute(token) =
                reader.GetChanges token
                    |> HashSetDelta.toSeq
                    |> Seq.map (fun d ->
                    match d with
                        | Add(1,v) -> 
                            let i = newIndex.Invoke v
                            i, Set v
                        | Rem(1,v) ->
                            let i = newIndex.Revoke v
                            i, Remove
                        | _ ->
                            unexpected()
                    )
                    |> IndexListDelta.ofSeq
                    
        [<Sealed>]
        type MapToListReader<'T>(input : amap<Index, 'T>) =
            inherit AbstractReader<IndexList<'T>, IndexListDelta<'T>>(IndexList.trace)
            let reader = input.GetReader()
            override x.Compute(token : AdaptiveToken) =
                reader.GetChanges token 
                |> HashMapDelta.toSeq
                |> IndexListDelta.ofSeq
                
        [<Sealed>]
        type ListToMapReader<'T>(input : alist<'T>) =
            inherit AbstractReader<HashMap<Index, 'T>, HashMapDelta<Index, 'T>>(HashMap.trace)
            let reader = input.GetReader()
            override x.Compute(token : AdaptiveToken) =
                reader.GetChanges token 
                |> IndexListDelta.toSeq
                |> HashMapDelta.ofSeq


    /// Functional operators for amap<_,_>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module AMap =
        /// Gets the keys of the given map as aset<_>.
        let keys (map: amap<'Key, 'Value>) =
            if map.IsConstant then
                ASet.delay (fun () ->
                    map.Content 
                    |> AVal.force
                    |> HashMap.keys
                )
            else
                ASet.ofReader (fun () -> MapKeysReader(map))

        /// Creates an alist using the given amap<Index, 'T>.
        let toAList (map: amap<Index, 'Value>) =
            if map.IsConstant then
                AMap.force map |> IndexList.ofSeqIndexed |> AList.ofIndexList
            else
                { new alist<'Value> with
                    member x.IsConstant = false
                    member x.Content = map.Content |> AVal.map IndexList.ofSeqIndexed
                    member x.History = None
                    member x.GetReader() =
                        match  map.History with
                        | Some history -> 
                            history.NewReader(IndexList.trace, HashMapDelta.toSeq >> IndexListDelta.ofSeq)
                        | None ->
                            MapToListReader(map) :> _
                }
             
        /// Creates an amap using the given alist.
        let ofAList (list: alist<'T>) =
            if list.IsConstant then
                list |> AList.force |> IndexList.toSeqIndexed |> HashMap.ofSeq |> AMap.ofHashMap
            else
                { new amap<Index, 'T> with
                    member x.IsConstant = false
                    member x.Content = list.Content |> AVal.map (IndexList.toSeqIndexed >> HashMap.ofSeq)
                    member x.History = None
                    member x.GetReader() =
                        match list.History with
                        | Some history ->
                            history.NewReader(HashMap.trace, IndexListDelta.toSeq >> HashMapDelta.ofSeq)
                        | None ->
                            ListToMapReader(list) :> _
                }


    /// Functional operators for aset<_>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ASet =

        /// Creates an aset holding all key/value tuples from the map.
        let ofAMap (map: amap<'Key, 'Value>) = AMap.toASet map
        
        /// Creates an aset holding all distinct values from the map.
        let ofAMapValues (map: amap<'Key, 'Value>) = AMap.toASetValues map

        /// Creates an aset holding all elements of the given list.
        let ofAList (list: alist<'T>) =
            if list.IsConstant then
                list.Content
                |> AVal.force
                |> ASet.ofSeq
            else
                ASet.ofReader (fun () -> ListSetReader(list))
            
        /// Creates an aset holding all index/elements pairs of the given list.
        let ofAListIndexed (list: alist<'T>) =
            if list.IsConstant then
                list.Content
                |> AVal.force
                |> IndexList.toSeqIndexed
                |> ASet.ofSeq
            else
                ASet.ofReader (fun () -> IndexedListSetReader(list))
            
        /// Creates an amap with the keys from the set and the values given by mapping.
        let mapToAMap (mapping: 'Key -> 'Value) (set: aset<'Key>) = AMap.mapSet mapping set

        /// Sorts the set using the given compare function.
        let sortWith (compare: 'T -> 'T -> int) (set: aset<'T>) =
            if set.IsConstant then
                set.Content 
                |> AVal.force
                |> Seq.sortWith compare
                |> AList.ofSeq
            else
                AList.ofReader (fun () -> SetSortWithReader(set, compare))

        /// Sorts the set using the keys given by projection.
        let sortBy (projection: 'T1 -> 'T2) (set: aset<'T1>) =
            if set.IsConstant then
                set.Content 
                |> AVal.force
                |> Seq.indexed
                |> Seq.sortBy (fun (i,v) -> projection v, i) 
                |> Seq.map snd
                |> AList.ofSeq
            else
                AList.ofReader (fun () -> SetSortByReader(set, projection))

        /// Sorts the set.
        let inline sort (set: aset<'T>) = sortWith compare set
        
        /// Creates an alist from the set with undefined element order.
        let toAList (set: aset<'T>) =
            if set.IsConstant then
                set.Content
                |> AVal.force
                |> AList.ofSeq
            else
                AList.ofReader (fun () -> ToListReader(set))

        /// Groups the aset by the given mapping and returns an amap with potentially colliding entries in a HashSet<'T>.
        let groupBy (mapping: 'T -> 'K) (set: aset<'T>) =
            // TODO: better implementation.
            set |> ASet.map (fun v -> mapping v, v) |> AMap.ofASet


    /// Functional operators for alist<_>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module AList =
        
        /// Creates an aset holding all elements of the given list.
        let toASet (list: alist<'T>) = ASet.ofAList list
       
        /// Creates an aset holding all index/elements pairs of the given list.
        let toASetIndexed (list: alist<'T>) = ASet.ofAListIndexed list

        /// Creates an alist from the set with undefined element order.
        let ofASet (set: aset<'T>) = ASet.toAList set

        /// Creates an amap using the given alist.
        let toAMap (list: alist<'T>) = AMap.ofAList list

        /// Creates an alist using the given amap<Index, 'T>.
        let ofAMap (map: amap<Index, 'T>) = AMap.toAList map
        
        /// Groups the alist by the given mapping and returns an amap with potentially colliding entries in a IndexList<'T>.
        let groupBy (mapping: 'T -> 'K) (set: alist<'T>) =
            // TODO: better implementation.
            set 
            |> AList.mapi (fun i v -> mapping v, (i, v)) 
            |> toASet
            |> AMap.ofASet
            |> AMap.map (fun _ v -> IndexList.ofSeqIndexed v)
         


    module private BooleanOperators =
        [<Sealed>]
        type AdaptiveOr(values : list<aval<bool>>) =
            inherit AbstractVal<bool>()

            let mutable witness : option<aval<bool>> = None

            let findWitness (token : AdaptiveToken) =
                let rec find (l : list<aval<bool>>) =
                    match l with
                    | [] -> None, []
                    | a :: rest ->
                        if a.GetValue token then Some a, rest
                        else 
                            let (h, t) = find rest
                            h, a :: t
                find values


            override x.Compute(token : AdaptiveToken) =
                match witness with
                | Some w ->
                    if w.GetValue token then
                        true
                    else
                        let w, rest = findWitness token
                        witness <- w
                        match w with
                        | Some _ -> 
                            rest |> List.iter (fun r -> r.Outputs.Remove x |> ignore)
                            true
                        | None ->
                            false
                | None ->
                    let w, rest = findWitness token
                    witness <- w
                    match w with
                    | Some _ -> 
                        rest |> List.iter (fun r -> r.Outputs.Remove x |> ignore)
                        true
                    | None ->
                        false

        [<Sealed>]
        type AdaptiveAnd(values : list<aval<bool>>) =
            inherit AbstractVal<bool>()

            let mutable witness : option<aval<bool>> = None

            let findWitness (token : AdaptiveToken) =
                let rec find (l : list<aval<bool>>) =
                    match l with
                    | [] -> None, []
                    | a :: rest ->
                        if not (a.GetValue token) then Some a, rest
                        else 
                            let (h, t) = find rest
                            h, a :: t
                find values


            override x.Compute(token : AdaptiveToken) =
                match witness with
                | Some w ->
                    if w.GetValue token then
                        let w, rest = findWitness token
                        witness <- w
                        match w with
                        | Some _ -> 
                            rest |> List.iter (fun r -> r.Outputs.Remove x |> ignore)
                            false
                        | None ->
                            true
                    else
                        false
                | None ->
                    let w, rest = findWitness token
                    witness <- w
                    match w with
                    | Some _ -> 
                        rest |> List.iter (fun r -> r.Outputs.Remove x |> ignore)
                        false
                    | None ->
                        true

    module AVal =
        let logicalAnd (l : #seq<aval<bool>>) =
            Seq.toList l |> BooleanOperators.AdaptiveAnd :> aval<_>

        let logicalOr (l : #seq<aval<bool>>) =
            Seq.toList l |> BooleanOperators.AdaptiveOr :> aval<_>

    /// Adaptive operators for lists.
    module List =
        /// Adaptively checks whether one or more entries make the given predicate true.
        let existsA (predicate : 'T -> aval<bool>) (elements : list<'T>) =
            let constant, adaptive = elements |> List.map predicate |> List.partition (fun v -> v.IsConstant)

            if constant |> List.exists AVal.force then
                AVal.constant true
            else
                match adaptive with
                | [] -> AVal.constant false
                | _ -> BooleanOperators.AdaptiveOr adaptive :> aval<_>
                
        /// Adaptively checks whether all entries make the given predicate true.
        let forallA (predicate : 'T -> aval<bool>) (elements : list<'T>) =
            let constant, adaptive = elements |> List.map predicate |> List.partition (fun v -> v.IsConstant)

            if constant |> List.exists (AVal.force >> not) then
                AVal.constant false
            else
                match adaptive with
                | [] -> AVal.constant true
                | _ -> BooleanOperators.AdaptiveAnd adaptive :> aval<_>

    /// Adaptive operators for seq.
    module Seq =
        /// Adaptively checks whether one or more entries make the given predicate true.
        let existsA (predicate : 'T -> aval<bool>) (elements : seq<'T>) =
            List.existsA predicate (Seq.toList elements)

        /// Adaptively checks whether all entries make the given predicate true.
        let forallA (predicate : 'T -> aval<bool>) (elements : seq<'T>) =
            List.forallA predicate (Seq.toList elements)
       
    /// Adaptive operators for arrays.
    module Array =
        /// Adaptively checks whether one or more entries make the given predicate true.
        let existsA (predicate : 'T -> aval<bool>) (elements : 'T[]) =
            List.existsA predicate (Array.toList elements)

        /// Adaptively checks whether all entries make the given predicate true.
        let forallA (predicate : 'T -> aval<bool>) (elements : 'T[]) =
            List.forallA predicate (Array.toList elements)
            
    /// Adaptive operators for HashSet.
    module HashSet =
        /// Adaptively checks whether one or more entries make the given predicate true.
        let existsA (predicate : 'T -> aval<bool>) (elements : HashSet<'T>) =
            List.existsA predicate (HashSet.toList elements)

        /// Adaptively checks whether all entries make the given predicate true.
        let forallA (predicate : 'T -> aval<bool>) (elements : HashSet<'T>) =
            List.forallA predicate (HashSet.toList elements)
            
    /// Adaptive operators for HashMap.
    module HashMap =
        /// Adaptively checks whether one or more entries make the given predicate true.
        let existsA (predicate : 'K -> 'V -> aval<bool>) (elements : HashMap<'K, 'V>) =
            let predicate = OptimizedClosures.FSharpFunc<'K, 'V, aval<bool>>.Adapt predicate
            List.existsA (fun struct(k,v) -> predicate.Invoke(k,v)) (elements.ToListV())

        /// Adaptively checks whether all entries make the given predicate true.
        let forallA (predicate : 'K -> 'V -> aval<bool>) (elements : HashMap<'K, 'V>) =
            let predicate = OptimizedClosures.FSharpFunc<'K, 'V, aval<bool>>.Adapt predicate
            List.forallA (fun struct(k,v) -> predicate.Invoke(k,v)) (elements.ToListV())
            
    /// Adaptive operators for Map.
    module Map =
        /// Adaptively checks whether one or more entries make the given predicate true.
        let existsA (predicate : 'K -> 'V -> aval<bool>) (elements : Map<'K, 'V>) =
            let predicate = OptimizedClosures.FSharpFunc<'K, 'V, aval<bool>>.Adapt predicate
            List.existsA (fun(k,v) -> predicate.Invoke(k,v)) (Map.toList elements)

        /// Adaptively checks whether all entries make the given predicate true.
        let forallA (predicate : 'K -> 'V -> aval<bool>) (elements : Map<'K, 'V>) =
            let predicate = OptimizedClosures.FSharpFunc<'K, 'V, aval<bool>>.Adapt predicate
            List.forallA (fun(k,v) -> predicate.Invoke(k,v)) (Map.toList elements)

    /// Adaptive operators for IndexList.
    module IndexList =
        /// Adaptively checks whether one or more entries make the given predicate true.
        let existsA (predicate : 'T -> aval<bool>) (elements : IndexList<'T>) =
            List.existsA predicate (IndexList.toList elements)

        /// Adaptively checks whether all entries make the given predicate true.
        let forallA (predicate : 'T -> aval<bool>) (elements : IndexList<'T>) =
            List.forallA predicate (IndexList.toList elements)


