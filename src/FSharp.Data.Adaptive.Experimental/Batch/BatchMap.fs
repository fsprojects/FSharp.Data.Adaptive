namespace FSharp.Data.Adaptive

open FSharp.Data.Traceable

[<AutoOpen>]
module BatchMapExtensions =
    module AMap =
        let batchMap (mapping : HashMap<'k, 'a> -> HashMap<'k, 'b>) (input : amap<'k, 'a>) =
            AMap.ofReader (fun () ->
                let r = input.GetReader()
                { new AbstractReader<HashMapDelta<'k, 'b>>(HashMapDelta.empty) with
                    member x.Compute(token : AdaptiveToken) =
                        let old = r.State
                        let ops = r.GetChanges token
                        
                        let removes =
                            HashMap.MapDelta(old, HashMapDelta.toHashMap ops, fun _k old op ->
                                match op with
                                | Remove ->
                                    match old with
                                    | ValueSome _ -> ValueSome Remove
                                    | _ -> ValueNone
                                | _ ->
                                    ValueNone
                            )
                        
                        let insertsAndUpdates =
                            HashMap.MapDelta(old, HashMapDelta.toHashMap ops, fun _k old op ->
                                match op with
                                | Set v ->
                                    match old with
                                    | ValueSome o when DefaultEquality.equals o v ->
                                        ValueNone
                                    | _ ->
                                        ValueSome v
                                | Remove ->
                                    ValueNone
                            )
                            
                        let newBs =
                            mapping insertsAndUpdates
                            |> HashMap.map (fun _ v -> Set v)
                           
                        let outOps = HashMap.union removes newBs
                        HashMapDelta.ofHashMap outOps
                }
            )

    module AList =
        let batchMap (mapping : IndexList<'a> -> IndexList<'b>) (input : alist<'a>) =
            AList.ofReader (fun () ->
                let r = input.GetReader()
                { new AbstractReader<IndexListDelta<'b>>(IndexListDelta.empty) with
                    member x.Compute(token : AdaptiveToken) =
                        let old = r.State
                        let ops = r.GetChanges token
                        
                        let removes =
                            ops.Content |> MapExt.chooseV (fun idx op ->
                                match op with
                                | Remove ->
                                    if old.Content.ContainsKey idx then ValueSome Remove
                                    else ValueNone
                                | _ ->
                                    ValueNone
                                
                            )
                            
                        let insertsAndUpdates =
                            ops.Content |> MapExt.chooseV (fun idx op ->
                                match op with
                                | Set v ->
                                    match MapExt.tryFindV idx old.Content with
                                    | ValueSome o when DefaultEquality.equals o v ->
                                        ValueNone
                                    | _ ->
                                        ValueSome v
                                        
                                | Remove ->
                                    ValueNone
                                
                            )
                           
                        let newBs =
                            let r = mapping (IndexList.ofMap insertsAndUpdates)
                            r.Content |> MapExt.map (fun _ v -> Set v)
                        let outOps = MapExt.union removes newBs
                        IndexListDelta.ofMap outOps
                }
            )
