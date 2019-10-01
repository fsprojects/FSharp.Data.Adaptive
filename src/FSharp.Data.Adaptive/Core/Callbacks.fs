namespace FSharp.Data.Adaptive

open System
open System.Threading

/// Represents a callback in the dependency-tree
type internal CallbackObject(obj: IAdaptiveObject, callback: CallbackObject -> unit) =
    static let emptyOutputs = EmptyOutputSet() :> IWeakOutputSet
    let mutable level = obj.Level + 1
    let mutable live = 1
    let mutable obj = obj
    let mutable weak = null

    member x.Mark() =
        if live > 0 then callback x
        false

    member x.Dispose() =
        if Interlocked.Exchange(&live, 0) = 1 then
            obj.Outputs.Remove x |> ignore
            obj <- Unchecked.defaultof<_>
            weak <- null
            level <- 0

    interface IDisposable with
        member x.Dispose() = x.Dispose()

    interface IAdaptiveObject with
        member x.Weak =
            let w = weak
            if isNull w then 
                let w = WeakReference<IAdaptiveObject>(x)
                weak <- w
                w
            else
                w
        member x.InputChanged(_,_) = ()
        member x.AllInputsProcessed(_) = ()
        member x.IsConstant = false
        member x.OutOfDate
            with get() = false
            and set _ = ()
        member x.Outputs = emptyOutputs
        member x.Mark() = x.Mark()
        member x.Level
            with get() = level
            and set l = level <- l

/// IAdaptiveObject extensions for creating/removing callbacks.
[<AutoOpen>]
module CallbackExtensions =
    
    type IAdaptiveObject with
        /// Registers a callback with the given object that will be executed
        /// whenever the object gets marked out-of-date.
        /// Note that it does not trigger when the object is currently out-of-date.
        /// Returns a disposable for removing the callback.
        member x.AddMarkingCallback (callback: unit -> unit) =
            let cb =
                new CallbackObject(x, fun self ->
                    try callback ()
                    finally x.Outputs.Add self |> ignore
                )

            x.Outputs.Add cb |> ignore
            cb :> IDisposable
            
        /// Registers a callback with the given object that will be executed
        /// ONCE! when the next out-of-date marking visits the object.
        /// Note that it does not trigger when the object is currently out-of-date.
        /// Returns a disposable for removing the callback.
        member x.OnNextMarking (callback: unit -> unit) =
            let cb =
                new CallbackObject(x, fun self ->
                    try
                        callback ()
                        self.Dispose()
                    with :? LevelChangedException ->
                        x.Outputs.Add self |> ignore
                        reraise()
                )

            x.Outputs.Add cb |> ignore
            cb :> IDisposable
