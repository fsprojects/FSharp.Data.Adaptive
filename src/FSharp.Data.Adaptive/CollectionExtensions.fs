namespace FSharp.Data.Adaptive

open FSharp.Data.Traceable

/// Collection extensions for aset<_>, amap<_,_>, alist<_>
[<AutoOpen>]
module CollectionExtensions =

    type IAdaptiveIndexList<'T> with
        member x.GetSlice(min : option<int>, max : option<int>) =
            match min with
            | Some min ->
                match max with
                | Some max -> AList.sub min (1 + max - min) x
                | None -> AList.skip min x
            | None ->
                match max with
                | Some max -> AList.take (1 + max) x
                | None -> x

        member x.GetSlice(min : option<aval<int>>, max : option<aval<int>>) =
            match min with
            | Some min ->
                match max with
                | Some max -> 
                    let cnt = (min, max) ||> AVal.map2 (fun min max -> 1 + max - min)
                    AList.subA min cnt x
                | None -> AList.skipA min x
            | None ->
                match max with
                | Some max -> 
                    let cnt = max |> AVal.map (fun max -> 1 + max)
                    AList.takeA cnt x
                | None -> x

    [<AutoOpen>]
    module internal Readers =
        /// Reader for ASet.sortBy
        [<Sealed>]
        type SetSortByReader<'T1, 'T2 when 'T2 : comparison>(set: aset<'T1>, projection: 'T1 -> 'T2, cmp : System.Collections.Generic.IComparer<'T2>) =
            inherit AbstractReader<IndexListDelta<'T1>>(IndexListDelta.empty)

            let reader = set.GetReader()
            let mapping = IndexMapping<Unique<'T2>>()
            let cache = Cache<'T1, Unique<'T2>>(fun v -> Unique(projection v, cmp))

            override x.Compute(token: AdaptiveToken) =
                reader.GetChanges token |> Seq.choose (fun op ->
                    match op with
                    | Add(_, v) ->
                        let k = cache.Invoke v
                        let idx = mapping.Invoke k
                        Some struct(idx, Set v)
                    | Rem(_, v) ->
                        let k = cache.Revoke v
                        match mapping.Revoke k with
                        | ValueSome idx -> Some struct(idx, Remove)
                        | ValueNone -> None
                )
                |> IndexListDelta.ofSeqV
        
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
                        Some struct(idx, Set v)
                    | Rem(_, v) ->
                        match mapping.Revoke v with
                        | Some idx -> Some struct(idx, Remove)
                        | None -> None
                )
                |> IndexListDelta.ofSeqV

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
                let changes = reader.GetChanges(token).Content
                let mutable delta = HashSetDelta.empty
                for KeyValue(i, op) in changes do
                    match op with
                    | Remove -> 
                        match MapExt.tryFindV i old with
                        | ValueSome v -> delta <- delta.Add (Rem v)
                        | ValueNone -> ()
                    | Set v ->
                        match MapExt.tryFindV i old with
                        | ValueSome ov ->
                            if not (DefaultEquality.equals v ov) then
                                delta <- delta.Add (Add v)
                                delta <- delta.Add (Rem ov)
                        | ValueNone ->
                            delta <- delta.Add (Add v)
                delta

        /// Reader for AList.mapToASet
        [<Sealed>]
        type ListSetMapReader<'T1, 'T2>(list: alist<'T1>, mapping : 'T1 -> 'T2) =
            inherit AbstractReader<HashSetDelta<'T2>>(HashSetDelta.empty)
            
            let reader = list.GetReader()
            let cache = Cache mapping

            override x.Compute(token: AdaptiveToken) =
                let old = reader.State.Content
                let changes = reader.GetChanges(token).Content
                let mutable delta = HashSetDelta.empty
                for KeyValue(i, op) in changes do
                    match op with
                    | Remove -> 
                        match MapExt.tryFindV i old with
                        | ValueSome v -> 
                            delta <- delta.Add (Rem (cache.Revoke v))
                        | ValueNone -> ()
                    | Set v ->
                        match MapExt.tryFindV i old with
                        | ValueSome ov ->
                            if not (DefaultEquality.equals v ov) then
                                delta <- delta.Add (Add (cache.Invoke v))
                                delta <- delta.Add (Rem (cache.Revoke ov))
                        | ValueNone ->
                            delta <- delta.Add (Add (cache.Invoke v))
                delta

        /// Reader for AList.toIndexedASet
        [<Sealed>]
        type IndexedListSetReader<'T>(list: alist<'T>) =
            inherit AbstractReader<HashSetDelta<Index * 'T>>(HashSetDelta.empty)
            
            let reader = list.GetReader()

            override x.Compute(token: AdaptiveToken) =
                let old = reader.State.Content
                let changes = reader.GetChanges(token).Content
                let mutable delta = HashSetDelta.empty
                for KeyValue(i, op) in changes do
                    match op with
                    | Remove -> 
                        match MapExt.tryFindV i old with
                        | ValueSome v -> delta <- delta.Add (Rem(i, v))
                        | ValueNone -> ()
                    | Set v ->
                        match MapExt.tryFindV i old with
                        | ValueSome ov ->
                            if not (DefaultEquality.equals v ov) then
                                delta <- delta.Add (Add(i,v))
                                delta <- delta.Add (Rem(i,ov))
                        | ValueNone ->
                            delta <- delta.Add (Add(i,v))
                delta

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
                            struct(i, Set v)
                        | Rem(1,v) ->
                            let i = newIndex.Revoke v
                            struct(i, Remove)
                        | _ ->
                            unexpected()
                    )
                    |> IndexListDelta.ofSeqV
                    
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

        [<Sealed>]
        type MapSortByReader<'K, 'V, 'T when 'T : comparison>(map : amap<'K, 'V>, cmp : System.Collections.Generic.IComparer<'T>, projection : OptimizedClosures.FSharpFunc<'K, 'V, 'T>) =
            inherit AbstractReader<IndexListDelta<'K * 'V>>(IndexListDelta.empty)

            let reader = map.GetReader()

            let mapping = 
                CustomIndexMapping<int * 'T>(fun (k0, t0) (k1, t1) -> 
                    let c = cmp.Compare(t0, t1)
                    if c <> 0 then c
                    else compare k0 k1
                )
            let mutable state = HashMap.empty<'K, int * 'T>
            let mutable currentId = 0

            let newId() =
                let i = currentId
                currentId <- i + 1
                i

            override x.Compute(token : AdaptiveToken) =
                let ops = reader.GetChanges token
                let mutable res = MapExt.empty
                for key, op in ops do
                    match op with
                    | Set v ->
                        let id =
                            match HashMap.tryFind key state with
                            | Some (i, _) -> i
                            | None -> newId()

                        let t = projection.Invoke(key, v)
                        let index = mapping.Invoke(id, t)
                        res <- MapExt.add index (Set (key, v)) res
                        state <- HashMap.add key (id, t) state
                    | Remove ->
                        match HashMap.tryRemove key state with
                        | Some ((id, t), rest) ->
                            state <- rest
                            match mapping.Revoke(id, t) with
                            | Some index -> 
                                res <- MapExt.add index (Remove) res
                            | None ->
                                ()
                        | None ->
                            ()

                IndexListDelta(res)

        
        /// Reader for ASet.ofListTree
        [<Sealed>]
        type ListTreeReader<'T>(list: alist<'T>, getChildren : 'T -> alist<'T>) =
            inherit AbstractDirtyReader<IIndexListReader<'T>, HashSetDelta<'T>>(HashSetDelta.monoid, isNull)
            
            let mutable initial = true
            let reader = list.GetReader() // NOTE: need to be held, otherwise it will be collected and no updates can be consumed
            let cache = System.Collections.Generic.Dictionary<struct(IIndexListReader<'T> * FSharp.Data.Adaptive.Index), struct('T * IIndexListReader<'T>)>() // TODO: refcounting

            member x.Invoke(token : AdaptiveToken, r : IIndexListReader<'T>, i : FSharp.Data.Adaptive.Index, n : 'T) =
                let mutable delta = HashSetDelta.empty
            
                if cache.ContainsKey (struct(r, i)) then
                    delta <- delta.Combine (x.Revoke(r, i))

                let subNodes = getChildren n
                let subReader = subNodes.GetReader()
                cache[struct(r, i)] <- (n, subReader)

                delta <- delta.Add (Add n)
                let content = subReader.GetChanges token
                for c in content do
                    match c with
                    | (si, Set sub) -> 
                        delta <- delta.Combine (x.Invoke(token, subReader, si, sub))
                    | _ -> unexpected()
            
                delta

            member x.Revoke(r : IIndexListReader<'T>, i : FSharp.Data.Adaptive.Index) =
                let mutable delta = HashSetDelta.empty
                match cache.TryGetValue (struct(r, i)) with
                | (true, struct(n, subReader)) -> 
                    cache.Remove (struct(r, i)) |> ignore
                    subReader.Outputs.Remove x |> ignore

                    delta <- delta.Add (Rem n)
                    subReader.State |> IndexList.iteri (fun i old ->
                        delta <- delta.Combine (x.Revoke(subReader, i)))

                | _ -> () // possible if parent set already removed all its sub nodes

                delta

            override x.Compute(token: AdaptiveToken, dirty : System.Collections.Generic.HashSet<IIndexListReader<'T>>) =
                let mutable delta = HashSetDelta.empty
                if initial then
                    initial <- false
                    let changes = reader.GetChanges token
                    for d in changes do
                        match d with
                        | (i, Set n) -> delta <- delta.Combine (x.Invoke(token, reader, i, n))
                        | _ -> unexpected()
                
                for d in dirty do
                    let inner = d.GetChanges token
                    for c in inner do
                        match c with
                        | (i, Set n) -> delta <- delta.Combine (x.Invoke(token, d, i, n))
                        | (i, Remove) -> delta <- delta.Combine (x.Revoke(d, i))
                    
                delta


        /// Reader for ASet.ofSetTree (delta combine)
        [<Sealed>]
        type SetTreeReader<'T>(set: aset<'T>, getChildren : 'T -> aset<'T>) =
            inherit AbstractDirtyReader<IHashSetReader<'T>, HashSetDelta<'T>>(HashSetDelta.monoid, isNull)
            
            let mutable initial = true
            let reader = set.GetReader() // NOTE: need to be held, otherwise it will be collected and no updates can be consumed
            let cache = DefaultDictionary.create<'T, struct(IHashSetReader<'T> * ref<int>)>()

            member x.Invoke(token : AdaptiveToken, n : 'T) =
                let mutable delta = HashSetDelta.empty
                match cache.TryGetValue n with
                | (true, (_, refCount)) -> refCount.Value <- refCount.Value + 1
                | _ ->
                    let subNodes = getChildren n
                    let reader = subNodes.GetReader()
                    cache[n] <- (reader, ref 1)

                    delta <- delta.Add (Add n)
                    let content = reader.GetChanges token
                    for c in content do
                        if c.Count <> 1 then unexpected()
                        delta <- delta.Combine (x.Invoke(token, c.Value))
                
                delta

            member x.Revoke(n : 'T) =
                let mutable delta = HashSetDelta.empty
                match cache.TryGetValue n with
                | (true, (reader, refCount)) -> 
                    if refCount.Value = 1 then
                        cache.Remove n |> ignore
                        reader.Outputs.Remove x |> ignore

                        delta <- delta.Add (Rem n)
                        for old in reader.State do
                            delta <- delta.Combine (x.Revoke(old))
                    else
                        refCount.Value <- refCount.Value - 1

                | _ -> () // possible if parent list already removed all its sub nodes

                delta

            override x.Compute(token: AdaptiveToken, dirty : System.Collections.Generic.HashSet<IHashSetReader<'T>>) =
                let mutable deltas = 
                    if initial then
                        initial <- false
                        reader.GetChanges token |> HashSetDelta.collect (fun d ->
                            let n = d.Value
                            if d.Count = 1 then x.Invoke(token, n)
                            else unexpected()
                        )
                    else
                        HashSetDelta.empty

                for d in dirty do
                    let inner = d.GetChanges token |> HashSetDelta.collect (fun d ->
                            let n = d.Value
                            if d.Count = 1 then x.Invoke(token, n)
                            elif d.Count = -1 then x.Revoke(n)
                            else unexpected()
                        )
                    deltas <- deltas.Combine inner
                deltas



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

        /// Creates a sorted alist holding Key/Value tuples from the amap using the given projection.
        let sortBy (projection : 'K -> 'V -> 'T) (map : amap<'K, 'V>) : alist<'K * 'V> =
            if map.IsConstant then
                map
                |> AMap.force
                |> HashMap.toList
                |> List.sortBy (fun (k, v) -> projection k v)
                |> AList.ofList
            else
                AList.ofReader <| fun () ->
                    MapSortByReader(map, LanguagePrimitives.FastGenericComparer, OptimizedClosures.FSharpFunc<_,_,_>.Adapt projection)

        /// Creates a sorted (descending order) alist holding Key/Value tuples from the amap using the given projection.
        let sortByDescending (projection : 'K -> 'V -> 'T) (map : amap<'K, 'V>) : alist<'K * 'V> =
            if map.IsConstant then
                map
                |> AMap.force
                |> HashMap.toList
                |> List.sortByDescending (fun (k, v) -> projection k v)
                |> AList.ofList
            else
                AList.ofReader <| fun () ->
                    let cmp =
                        let c = LanguagePrimitives.FastGenericComparer
                        { new System.Collections.Generic.IComparer<'T> with
                            member x.Compare(a, b) = c.Compare(b,a)
                        }
                    MapSortByReader(map, cmp, OptimizedClosures.FSharpFunc<_,_,_>.Adapt projection)




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
                AList.ofReader (fun () -> SetSortByReader(set, projection, LanguagePrimitives.FastGenericComparer<'T2>))

        /// Sorts the set.
        let inline sort (set: aset<'T>) = sortWith compare set
        
        /// Sorts the set in descending order.
        let inline sortDescending (set: aset<'T>) = sortWith (fun a b -> compare b a) set
        
        /// Sorts the set using the keys given by projection in descending order.
        let sortByDescending (projection: 'T1 -> 'T2) (set: aset<'T1>) =
            if set.IsConstant then
                set.Content 
                |> AVal.force
                |> Seq.indexed
                |> Seq.sortByDescending (fun (i,v) -> projection v, -i) 
                |> Seq.map snd
                |> AList.ofSeq
            else
                let cmp =
                    let c = LanguagePrimitives.FastGenericComparer<'T2>
                    { new System.Collections.Generic.IComparer<'T2> with
                        member x.Compare(a,b) = c.Compare(b,a)
                    }
                AList.ofReader (fun () -> SetSortByReader(set, projection, cmp))

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
            set |> AMap.ofASetMapped mapping

        /// Creates an aset from tree of alists
        /// NOTE: does not expect duplicates -> TODO
        let ofListTree<'T> (getChildren : 'T -> alist<'T>) (nodes : alist<'T>) =
            ASet.ofReader (fun () -> ListTreeReader(nodes, getChildren))

        /// Creates an aset from tree of sets
        let ofSetTree<'T> (getChildren : 'T -> aset<'T>) (nodes : aset<'T>) =
            ASet.ofReader (fun () -> SetTreeReader(nodes, getChildren))

        /// maps the set to amap with the given key mapping and duplicates ignored
        let toAMapIgnoreDuplicates (getKey: 'T -> 'K) (set: aset<'T>) =
            set |> AMap.ofASetMappedIgnoreDuplicates getKey

    /// Functional operators for alist<_>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module AList =
        
        /// Creates an aset holding all elements of the given list.
        let toASet (list: alist<'T>) = ASet.ofAList list

        /// Creates an aset holding all elements of the given list.
        let mapToASet (mapping : 'T1 -> 'T2) (list: alist<'T1>) = 
            if list.IsConstant then
                list.Content
                |> AVal.force
                |> Seq.map mapping 
                |> ASet.ofSeq
            else
                ASet.ofReader (fun () -> ListSetMapReader(list, mapping))
       
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


