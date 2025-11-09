namespace FSharp.Data.Traceable

open FSharp.Data.Adaptive

#nowarn "7331"

//[<CompilerMessage("ChangeableModelMap should not be used directly", 1337, IsHidden = true)>]
type ChangeableModelMap<'K, 'V, 'C, 'A>(map : HashMap<'K, 'V>, init : 'V -> 'C, update : 'C -> 'V -> 'C, view : 'C -> 'A) =
    let mutable map = map 
    let _current = AVal.custom (fun _ -> map)
    let history =
        let h = History<HashMap<'K, 'C>, HashMapDelta<'K, 'C>>(HashMap.trace)
        h.PerformUnsafe(HashMap.map (fun _ v -> init v) map, HashMapDelta.empty) |> ignore
        h

    let content = lazy (history |> AVal.map (HashMap.map (fun _ v -> view v)))
    member x.Current = _current :> aval<_>

    member x.Update(value : HashMap<'K, 'V>) = 
        if not (ShallowEqualityComparer<_>.ShallowEquals(map, value)) then
            let ops = HashMap.computeDelta map value
            map <- value
            _current.MarkOutdated()

            let inline apply (_key : 'K) (old : voption<'C>) (delta : ElementOperation<'V>) =
                match old with
                | ValueSome o ->
                    match delta with
                    | Set v ->
                        let n = update o v
                        if cheapEqual o n then struct (ValueSome n, ValueNone)
                        else struct (ValueSome n, ValueSome (Set n))
                    | Remove ->
                        struct(ValueNone, ValueSome Remove)
                | ValueNone ->
                    match delta with
                    | Set v ->
                        let n = init v
                        struct (ValueSome n, ValueSome (Set n))
                    | Remove ->
                        struct (ValueNone, ValueNone)

            let struct(s, ops) = HashMap.ApplyDeltaV(history.State, HashMapDelta.toHashMap ops, apply)
            history.PerformUnsafe(s, HashMapDelta.ofHashMap ops) |> ignore

    member x.GetReader() =
        history.NewReader(
            HashMap.trace, 
            HashMapDelta.toHashMap >> HashMap.map (fun _i op -> match op with | Set v -> Set (view v) | Remove -> Remove) >> HashMapDelta.ofHashMap
        )

    interface amap<'K, 'A> with
        member x.IsConstant = false
        member x.Content = content.Value
        member x.GetReader() = x.GetReader()
        member x.History = None

//[<CompilerMessage("ChangeableModelList should not be used directly", 1337, IsHidden = true)>]
type ChangeableModelList<'T, 'C, 'A>(list : IndexList<'T>, init : 'T -> 'C, update : 'C -> 'T -> 'C, view : 'C -> 'A) =
    let mutable list = list 
    let _current = AVal.custom (fun _ -> list)
    let history =
        let h = History<IndexList<'C>, IndexListDelta<'C>>(IndexList.trace)
        h.PerformUnsafe(IndexList.map init list, IndexListDelta.empty) |> ignore
        h

    let content = lazy (history |> AVal.map (IndexList.map view))

    member x.Current = _current :> aval<_>

    member x.Update(value : IndexList<'T>) = 
        if not (ShallowEqualityComparer<_>.ShallowEquals(list, value)) then
            let ops = IndexList.computeDelta list value
            list <- value
            _current.MarkOutdated()

            let inline apply (_key : Index) (old : voption<'C>) (delta : ElementOperation<'T>) =
                match old with
                | ValueSome o ->
                    match delta with
                    | Set v ->
                        let n = update o v
                        if cheapEqual o n then struct (ValueSome n, ValueNone)
                        else struct (ValueSome n, ValueSome (Set n))
                    | Remove ->
                        struct(ValueNone, ValueSome Remove)
                | ValueNone ->
                    match delta with
                    | Set v ->
                        let n = init v
                        struct (ValueSome n, ValueSome (Set n))
                    | Remove ->
                        struct (ValueNone, ValueNone)

            let s, ops = history.State.Content.ApplyDeltaAndGetEffective(ops.Content, apply)

            history.PerformUnsafe(IndexList.ofMap s, IndexListDelta ops) |> ignore

    member x.GetReader() =
        history.NewReader(
            IndexList.trace, 
            IndexListDelta.map (fun _i op -> match op with | Set v -> Set (view v) | Remove -> Remove)
        )
    
    interface alist<'A> with
        member x.IsConstant = false
        member x.Content = content.Value
        member x.GetReader() = x.GetReader()
        member x.History = None
 
type ChangeableLazyVal<'T>(compute : unit -> 'T) =
    inherit AbstractVal<'T>()

    let mutable compute = compute
    let mutable lastValue : voption<'T> = ValueNone
    
    member x.Update(computation : unit -> 'T) =
        let needsMarking = 
            lock x (fun () ->
                compute <- computation
                match lastValue with
                | ValueSome o ->
                    if x.OutOfDate then
                        // there was a value but no-one cared for it
                        lastValue <- ValueNone
                        false
                    else
                        // someone actually depends on the value
                        let n = computation()
                        lastValue <- ValueSome n
                        not (ShallowEqualityComparer.ShallowEquals(o, n))
                | ValueNone ->
                    assert x.OutOfDate
                    false
            )
        if needsMarking then x.MarkOutdated()

    override x.Compute(_t : AdaptiveToken) =
        let v = compute()
        lastValue <- ValueSome v
        v


type ChangeableModelListGeneric<'S, 'T, 'C, 'A>(list : 'S, toArray : 'S -> 'T[], cmp : 'T -> 'T -> bool, init : 'T -> 'C, update : 'C -> 'T -> 'C, view : 'C -> 'A) =
    let mutable lastValue = list
    let mutable list = IndexList.ofArray (toArray list)
    let _current = AVal.custom (fun _ -> list)
    let history =
        let h = History<IndexList<'C>, IndexListDelta<'C>>(IndexList.trace)
        h.PerformUnsafe(IndexList.map init list, IndexListDelta.empty) |> ignore
        h

    let content = lazy (history |> AVal.map (IndexList.map view))

    member x.Current = _current :> aval<_>

    member x.Update(dst : 'S) =
        if not (System.Object.ReferenceEquals(lastValue, dst)) then
            lastValue <- dst
            let dst = toArray dst
            if Array.isEmpty dst then
                list <- IndexList.empty
                history.Perform (IndexList.computeDelta history.State IndexList.empty) |> ignore
                
            elif history.State.IsEmpty then
                let newList = IndexList.ofArray dst
                list <- newList
                let v = newList |> IndexList.map init
                history.Perform (IndexList.computeDelta IndexList.empty v) |> ignore
                
            else
                // both are non-empty
                let src = IndexList.toArrayIndexed list
                
                let mutable steps = ComputeListDeltaHelpers.DeltaOperationList.ofArrayMyers (fun (_,a) b -> cmp a b) src dst
                let mutable si = 0
                let mutable di = 0
                let mutable delta = IndexListDelta.empty
                let mutable newList = list
                let mutable lastIndex = Index.zero

                while not steps.IsNil do
                    let struct(h, t) = steps.UnsafeUnconsV()

                    match h with
                    | ComputeListDeltaHelpers.DeltaOperation.Equal ->
                        steps <- t
                        // step both => no delta
                        
                        let (index, _) = src.[si]
                        let changeable = history.State.[index]
                        let newChangeable = update changeable dst.[di]
                        if not (CheapEquality.cheapEqual changeable newChangeable) then
                            delta <- delta |> IndexListDelta.add index (Set newChangeable)
                        
                        newList <- newList |> IndexList.set index dst.[di]
                        
                        lastIndex <- index
                        si <- si + 1
                        di <- di + 1
                    | _ ->
                        let mutable remCnt = 0
                        let mutable addCnt = 0
                        let mutable struct(h, t) = steps.UnsafeUnconsV()
                        steps <- t

                        while h <> ComputeListDeltaHelpers.DeltaOperation.Equal do
                            if h = ComputeListDeltaHelpers.DeltaOperation.Remove then remCnt <- remCnt + 1
                            else addCnt <- addCnt + 1

                            if steps.IsNil then 
                                h <- ComputeListDeltaHelpers.DeltaOperation.Equal
                            else 
                                let struct(hh, tt) = steps.UnsafeUnconsV()
                                if hh <> ComputeListDeltaHelpers.DeltaOperation.Equal then 
                                    h <- hh
                                    steps <- tt
                                else
                                    h <- ComputeListDeltaHelpers.DeltaOperation.Equal

                    
                        let replace = min remCnt addCnt
                        for _ in 0 .. replace - 1 do
                            let (idx,_) = src.[si]
                            delta <- delta |> IndexListDelta.add idx (Set (init dst.[di]))
                            newList <- newList |> IndexList.set idx dst.[di]
                            si <- si + 1
                            di <- di + 1
                            lastIndex <- idx

                        for _ in replace .. remCnt - 1 do
                            let (idx, _) = src.[si]
                            delta <- delta |> IndexListDelta.add idx Remove
                            newList <- newList |> IndexList.remove idx
                            si <- si + 1

                        if replace < addCnt then
                            let newIndex =
                                if si < src.Length then 
                                    let (ni,_) = src.[si]
                                    fun l -> Index.between l ni
                                else 
                                    Index.after

                            for _ in replace .. addCnt - 1 do
                                let idx = newIndex lastIndex
                                delta <- delta |> IndexListDelta.add idx (Set (init dst.[di]))
                                newList <- newList |> IndexList.set idx dst.[di]
                                lastIndex <- idx
                                di <- di + 1

                
                list <- newList
                history.Perform delta |> ignore
            
    member x.GetReader() =
        history.NewReader(
            IndexList.trace, 
            IndexListDelta.map (fun _i op -> match op with | Set v -> Set (view v) | Remove -> Remove)
        )
    
    interface alist<'A> with
        member x.IsConstant = false
        member x.Content = content.Value
        member x.GetReader() = x.GetReader()
        member x.History = None
        
type ChangeableModelListList<'T, 'C, 'A>(initial : list<'T>, cmp : 'T -> 'T -> bool, init : 'T -> 'C, update : 'C -> 'T -> 'C, view : 'C -> 'A) =
    inherit ChangeableModelListGeneric<list<'T>, 'T, 'C, 'A>(initial, List.toArray, cmp, init, update, view)
    
type ChangeableModelListArray<'T, 'C, 'A>(initial : array<'T>, cmp : 'T -> 'T -> bool, init : 'T -> 'C, update : 'C -> 'T -> 'C, view : 'C -> 'A) =
    inherit ChangeableModelListGeneric<array<'T>, 'T, 'C, 'A>(initial, id, cmp, init, update, view)
        
type ChangeableModelListSeq<'T, 'C, 'A>(initial : seq<'T>, cmp : 'T -> 'T -> bool, init : 'T -> 'C, update : 'C -> 'T -> 'C, view : 'C -> 'A) =
    inherit ChangeableModelListGeneric<seq<'T>, 'T, 'C, 'A>(initial, Seq.toArray, cmp, init, update, view)
        