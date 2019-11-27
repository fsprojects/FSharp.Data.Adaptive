namespace FSharp.Data.Adaptive

open FSharp.Data.Traceable
open System.Runtime.CompilerServices

/// Callback Extensions for adaptive datatypes.
[<AbstractClass; Sealed; Extension>]
type EvaluationCallbackExtensions private() =

    [<Extension>]
    /// Adds a disposable callback to the aval that will be executed whenever the
    /// avals value changed.
    static member AddCallback(value : aval<'T>, action: 'T -> unit) =
        let last = ref ValueNone
        let sub = 
            value.AddMarkingCallback(fun () ->
                let t = Transaction.Running.Value
                t.AddFinalizer(fun () ->
                    let v = AVal.force value
                    match !last with
                    | ValueSome o when Unchecked.equals o v -> 
                        ()
                    | _ ->
                        last := ValueSome v
                        action v
                )
            )

        match Transaction.Running with
        | Some t -> t.AddFinalizer (fun () -> value |> AVal.force |> action)
        | None -> value |> AVal.force |> action

        sub

    [<Extension>]
    /// Adds a disposable callback to the reader that will be executed whenever
    /// changes exist on the reader. The given action will be called with the
    /// readers state prior to the change and all changes applied to that state.
    static member AddCallback(reader : IOpReader<'State, 'Delta>, action: 'State -> 'Delta -> unit) =
        let action = OptimizedClosures.FSharpFunc<_,_,_>.Adapt action
        let run() =
            let state = reader.State
            let changes = reader.GetChanges AdaptiveToken.Top
            if not (reader.Trace.tmonoid.misEmpty changes) then
                action.Invoke(state, changes)
        
        let sub = 
            reader.AddMarkingCallback(fun () ->
                let t = Transaction.Running.Value
                t.AddFinalizer run
            )
        
        match Transaction.Running with
        | Some t -> t.AddFinalizer run
        | None -> run()

        sub
    
    [<Extension>]
    /// Adds a disposable callback to the aset that will be executed whenever
    /// changes happen to the aset. The given action will be called with the
    /// asets state prior to the change and all changes applied to that state.
    static member AddCallback(value : aset<'T>, action: CountingHashSet<'T> -> HashSetDelta<'T> -> unit) =
        let reader = value.GetReader()
        reader.AddCallback(action)

    [<Extension>]
    /// Adds a disposable callback to the amap that will be executed whenever
    /// changes happen to the amap. The given action will be called with the
    /// amaps state prior to the change and all changes applied to that state.
    static member AddCallback(value : amap<'Key, 'Value>, action: HashMap<'Key, 'Value> -> HashMapDelta<'Key, 'Value> -> unit) =
        let reader = value.GetReader()
        reader.AddCallback(action)

    [<Extension>]
    /// Adds a disposable callback to the alist that will be executed whenever
    /// changes happen to the alist. The given action will be called with the
    /// alists state prior to the change and all changes applied to that state.
    static member AddCallback(value : alist<'T>, action: IndexList<'T> -> IndexListDelta<'T> -> unit) =
        let reader = value.GetReader()
        reader.AddCallback(action)

