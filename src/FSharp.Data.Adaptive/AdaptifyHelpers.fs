namespace FSharp.Data.Traceable

open FSharp.Data.Adaptive

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

            let s, ops = HashMap.ApplyDelta(history.State, HashMapDelta.toHashMap ops, apply)
            history.PerformUnsafe(s, HashMapDelta.ofHashMap ops) |> ignore

    member x.GetReader() =
        history.NewReader(
            HashMap.trace, 
            HashMapDelta.toHashMap >> HashMap.map (fun i op -> match op with | Set v -> Set (view v) | Remove -> Remove) >> HashMapDelta.ofHashMap
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
            IndexListDelta.map (fun i op -> match op with | Set v -> Set (view v) | Remove -> Remove)
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
