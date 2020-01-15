namespace FSharp.Data.Adaptive

open FSharp.Data.Traceable

/// Collection extensions for aset<_>, amap<_,_>, alist<_>
[<AutoOpen>]
module CollectionExtensions =

    [<AutoOpen>]
    module internal Readers =
        /// Reader for ASet.sortBy
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

        /// Reader for AList.ofASet
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

        type MapToListReader<'T>(input : amap<Index, 'T>) =
            inherit AbstractReader<IndexList<'T>, IndexListDelta<'T>>(IndexList.trace)
            let reader = input.GetReader()
            override x.Compute(token : AdaptiveToken) =
                reader.GetChanges token 
                |> HashMapDelta.toSeq
                |> IndexListDelta.ofSeq

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
                map.Content 
                |> AVal.force
                |> HashMap.keys
                |> ASet.ofHashSet
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

        /// Creates an aset holding all elements of the given list.
        let ofAList (list: alist<'T>) =
            if list.IsConstant then
                list.Content
                |> AVal.force
                |> ASet.ofSeq
            else
                ASet.ofReader (fun () -> ListSetReader(list))
            
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

    /// Functional operators for alist<_>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module AList =
        
        /// Creates an aset holding all elements of the given list.
        let toASet (list: alist<'T>) = ASet.ofAList list
        
        /// Creates an alist from the set with undefined element order.
        let ofASet (set: aset<'T>) = ASet.toAList set

        /// Creates an amap using the given alist.
        let toAMap (list: alist<'T>) = AMap.ofAList list

        /// Creates an alist using the given amap<Index, 'T>.
        let ofAMap (map: amap<Index, 'T>) = AMap.toAList map


