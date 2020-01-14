namespace FSharp.Data.Adaptive

open FSharp.Data.Traceable
open System.Runtime.CompilerServices


[<AutoOpen>]
/// Callback Extensions for adaptive datatypes.
module EvaluationCallbackExtensions =
    type IAdaptiveValue with
        /// Adds a disposable callback to the aval that will be executed whenever the
        /// avals value changed.
        member value.AddCallback(action: obj -> unit) =
            let last = ref ValueNone
            let sub = 
                value.AddMarkingCallback(fun () ->
                    let t = Transaction.Running.Value
                    t.AddFinalizer(fun () ->
                        let v = value.GetValueUntyped AdaptiveToken.Top
                        match !last with
                        | ValueSome o when Unchecked.equals o v -> 
                            ()
                        | _ ->
                            last := ValueSome v
                            action v
                    )
                )

            match Transaction.Running with
            | Some t -> t.AddFinalizer (fun () -> value.GetValueUntyped AdaptiveToken.Top |> action)
            | None -> value.GetValueUntyped AdaptiveToken.Top |> action

            sub

    type IAdaptiveValue<'T> with
        /// Adds a disposable callback to the aval that will be executed whenever the
        /// avals value changed.
        member value.AddCallback(action: 'T -> unit) =
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

    type IOpReader<'State, 'Delta> with
        /// Adds a disposable callback to the reader that will be executed whenever
        /// changes exist on the reader. The given action will be called with the
        /// readers state prior to the change and all changes applied to that state.
        member reader.AddCallback(action: 'State -> 'Delta -> unit) =
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

    type IAdaptiveHashSet<'T> with
        /// Adds a disposable callback to the aset that will be executed whenever
        /// changes happen to the aset. The given action will be called with the
        /// asets state prior to the change and all changes applied to that state.
        member value.AddCallback(action: CountingHashSet<'T> -> HashSetDelta<'T> -> unit) =
            let reader = value.GetReader()
            reader.AddCallback(action)

        
    type IAdaptiveHashMap<'Key, 'Value> with
        /// Adds a disposable callback to the amap that will be executed whenever
        /// changes happen to the amap. The given action will be called with the
        /// amaps state prior to the change and all changes applied to that state.
        member value.AddCallback(action: HashMap<'Key, 'Value> -> HashMapDelta<'Key, 'Value> -> unit) =
            let reader = value.GetReader()
            reader.AddCallback(action)

    type IAdaptiveIndexList<'T> with
        /// Adds a disposable callback to the alist that will be executed whenever
        /// changes happen to the alist. The given action will be called with the
        /// alists state prior to the change and all changes applied to that state.
        member value.AddCallback(action: IndexList<'T> -> IndexListDelta<'T> -> unit) =
            let reader = value.GetReader()
            reader.AddCallback(action)

