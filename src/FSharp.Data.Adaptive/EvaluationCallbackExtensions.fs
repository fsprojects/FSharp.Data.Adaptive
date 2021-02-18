namespace FSharp.Data.Adaptive

open FSharp.Data.Traceable
open System.Runtime.CompilerServices


[<AutoOpen>]
/// Callback Extensions for adaptive datatypes.
module EvaluationCallbackExtensions =
    type IAdaptiveValue with
        /// Adds a disposable callback to the aval that will be executed whenever the
        /// avals value changed.
        member internal value.AddCallback(weak : bool, action: obj -> unit) =
            let last = ref ValueNone
            let sub = 
                value.AddMarkingCallback(weak, fun () ->
                    let t = Transaction.Running.Value
                    t.AddFinalizer(fun () ->
                        let v = value.GetValueUntyped AdaptiveToken.Top
                        match !last with
                        | ValueSome o when DefaultEquality.equals o v -> 
                            ()
                        | _ ->
                            last := ValueSome v
                            action v
                    )
                )

            match Transaction.Running with
            | ValueSome t -> t.AddFinalizer (fun () -> value.GetValueUntyped AdaptiveToken.Top |> action)
            | ValueNone -> value.GetValueUntyped AdaptiveToken.Top |> action

            sub
            
        /// Adds a disposable callback to the aval that will be executed whenever the
        /// avals value changed.
        member value.AddCallback(action: obj -> unit) = value.AddCallback(false, action)
        
        /// Adds a disposable callback to the aval that will be executed whenever the
        /// avals value changed.
        member value.AddWeakCallback(action: obj -> unit) = value.AddCallback(true, action)

    type IAdaptiveValue<'T> with
        /// Adds a disposable callback to the aval that will be executed whenever the
        /// avals value changed.
        member internal value.AddCallback(weak : bool, action: 'T -> unit) =
            let last = ref ValueNone
            let sub = 
                value.AddMarkingCallback(weak, fun () ->
                    let t = Transaction.Running.Value
                    t.AddFinalizer(fun () ->
                        let v = AVal.force value
                        match !last with
                        | ValueSome o when DefaultEquality.equals o v -> 
                            ()
                        | _ ->
                            last := ValueSome v
                            action v
                    )
                )

            match Transaction.Running with
            | ValueSome t -> t.AddFinalizer (fun () -> value |> AVal.force |> action)
            | ValueNone -> value |> AVal.force |> action

            sub
            
        /// Adds a disposable callback to the aval that will be executed whenever the
        /// avals value changed.
        member value.AddCallback(action: 'T -> unit) = value.AddCallback(false, action)
        
        /// Adds a disposable callback to the aval that will be executed whenever the
        /// avals value changed.
        member value.AddWeakCallback(action: 'T -> unit) = value.AddCallback(true, action)

    type IOpReader<'State, 'Delta> with
        /// Adds a disposable callback to the reader that will be executed whenever
        /// changes exist on the reader. The given action will be called with the
        /// readers state prior to the change and all changes applied to that state.
        member internal reader.AddCallback(weak : bool, action: 'State -> 'Delta -> unit) =
            let action = OptimizedClosures.FSharpFunc<_,_,_>.Adapt action
            let run() =
                let state = reader.State
                let changes = reader.GetChanges AdaptiveToken.Top
                if not (reader.Trace.tmonoid.misEmpty changes) then
                    action.Invoke(state, changes)
        
            let sub = 
                reader.AddMarkingCallback(weak, fun () ->
                    let t = Transaction.Running.Value
                    t.AddFinalizer run
                )
        
            match Transaction.Running with
            | ValueSome t -> t.AddFinalizer run
            | ValueNone -> run()

            sub
            
        /// Adds a disposable callback to the reader that will be executed whenever
        /// changes exist on the reader. The given action will be called with the
        /// readers state prior to the change and all changes applied to that state.
        member reader.AddCallback(action: 'State -> 'Delta -> unit) = reader.AddCallback(false, action)

        /// Adds a disposable callback to the reader that will be executed whenever
        /// changes exist on the reader. The given action will be called with the
        /// readers state prior to the change and all changes applied to that state.
        member reader.AddWeakCallback(action: 'State -> 'Delta -> unit) = reader.AddCallback(true, action)

    type IAdaptiveHashSet<'T> with
        /// Adds a disposable callback to the aset that will be executed whenever
        /// changes happen to the aset. The given action will be called with the
        /// asets state prior to the change and all changes applied to that state.
        member value.AddCallback(action: CountingHashSet<'T> -> HashSetDelta<'T> -> unit) =
            let reader = value.GetReader()
            reader.AddCallback(false, action)

        /// Adds a disposable callback to the aset that will be executed whenever
        /// changes happen to the aset. The given action will be called with the
        /// asets state prior to the change and all changes applied to that state.
        member value.AddWeakCallback(action: CountingHashSet<'T> -> HashSetDelta<'T> -> unit) =
            let reader = value.GetReader()
            reader.AddCallback(true, action)

    type IAdaptiveHashMap<'Key, 'Value> with
        /// Adds a disposable callback to the amap that will be executed whenever
        /// changes happen to the amap. The given action will be called with the
        /// amaps state prior to the change and all changes applied to that state.
        member value.AddCallback(action: HashMap<'Key, 'Value> -> HashMapDelta<'Key, 'Value> -> unit) =
            let reader = value.GetReader()
            reader.AddCallback(false, action)

        /// Adds a disposable callback to the amap that will be executed whenever
        /// changes happen to the amap. The given action will be called with the
        /// amaps state prior to the change and all changes applied to that state.
        member value.AddWeakCallback(action: HashMap<'Key, 'Value> -> HashMapDelta<'Key, 'Value> -> unit) =
            let reader = value.GetReader()
            reader.AddCallback(true, action)

    type IAdaptiveIndexList<'T> with
        /// Adds a disposable callback to the alist that will be executed whenever
        /// changes happen to the alist. The given action will be called with the
        /// alists state prior to the change and all changes applied to that state.
        member value.AddCallback(action: IndexList<'T> -> IndexListDelta<'T> -> unit) =
            let reader = value.GetReader()
            reader.AddCallback(false, action)

        /// Adds a disposable callback to the alist that will be executed whenever
        /// changes happen to the alist. The given action will be called with the
        /// alists state prior to the change and all changes applied to that state.
        member value.AddWeakCallback(action: IndexList<'T> -> IndexListDelta<'T> -> unit) =
            let reader = value.GetReader()
            reader.AddCallback(true,action)

