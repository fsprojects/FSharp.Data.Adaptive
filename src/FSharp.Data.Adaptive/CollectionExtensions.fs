namespace FSharp.Data.Adaptive

open FSharp.Data.Traceable

/// Collection extensions for aset<_>, amap<_,_>, alist<_>
[<AutoOpen>]
module CollectionExtensions =

    type internal SetSortByReader<'T1, 'T2 when 'T2 : comparison>(set: aset<'T1>, projection: 'T1 -> 'T2) =
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

        
    /// Functional operators for aset<_>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ASet =
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