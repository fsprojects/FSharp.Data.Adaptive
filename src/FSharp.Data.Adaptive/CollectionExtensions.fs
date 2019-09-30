namespace FSharp.Data.Adaptive

open FSharp.Data.Traceable

/// Collection extensions for aset<_>, amap<_,_>, alist<_>
[<AutoOpen>]
module CollectionExtensions =

    [<AutoOpen>]
    module internal Readers =
        /// Reader for ASet.sortBy
        type SetSortByReader<'T1, 'T2 when 'T2 : comparison>(set: aset<'T1>, projection: 'T1 -> 'T2) =
            inherit AbstractReader<IndexListDelta<'T1>>(IndexListDelta.monoid)

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

        /// Reader for AMap.keys
        type MapKeysReader<'Key, 'Value>(map: amap<'Key, 'Value>) =
            inherit AbstractReader<HashSetDelta<'Key>>(HashSetDelta.monoid)

            let reader = map.GetReader()

            override x.Compute(token: AdaptiveToken) =
                let old = reader.State
                let mutable cnt = 0
                let ops = 
                    reader.GetChanges(token).Store.Store |> IntMap.mapOption (fun entries ->
                        let result = 
                            entries |> List.choose (fun struct(key, op) ->
                                match op with
                                | Set _ ->
                                    if HashMap.containsKey key old then None
                                    else cnt <- cnt + 1; Some (struct (key, 1))
                                | Remove ->
                                    if HashMap.containsKey key old then cnt <- cnt + 1; Some (struct (key, -1))
                                    else None
                            )
                        match result with
                        | [] -> None
                        | _ -> Some result
                    )
                HashSetDelta(HashMap(cnt, ops))



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

    /// Functional operators for aset<_>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ASet =

        /// Sorts the set using the keys given by projection.
        let sortBy (projection: 'T1 -> 'T2) (set: aset<'T1>) : alist<'T1> =
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
        let inline sort (set: aset<'T>) = sortBy id set
