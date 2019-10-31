namespace FSharp.Data.Adaptive

open System
open System.Threading
open System.Runtime.CompilerServices


/// Represents an object providing callbacks in the dependency-tree
type internal MultiCallbackObject(table : ConditionalWeakTable<IAdaptiveObject, MultiCallbackObject>, obj: IAdaptiveObject) = 
    static let mutable id = 0
    static let emptyOutputs = EmptyOutputSet() :> IWeakOutputSet
    let mutable level = obj.Level + 1
    let mutable cbs : HashMap<int, unit -> bool> = HashMap.empty
    let mutable obj = obj
    let mutable weak = null

    let check (x : MultiCallbackObject) =
        if cbs.Count = 0 && not (isNull (obj :> obj)) then
            obj.Outputs.Remove x |> ignore
            if lock table (fun () -> table.Remove obj) then
                level <- 0
                obj <- Unchecked.defaultof<_>
                weak <- null
                false
            else
                true
        else
            true

    let remove (x : MultiCallbackObject) (id : int) =
        lock x (fun () ->
            cbs <- HashMap.remove id cbs
            check x |> ignore
        )

    /// adds a callback to the object. the returned boolean indicates whether the callback
    /// should automatically re-subscribe after being fired.
    member x.Subscribe(cb : unit -> bool) =
        lock x (fun () ->
            let i = 
                #if !FABLE_COMPILER
                Interlocked.Increment(&id)
                #else
                let a = id in id <- a + 1; a
                #endif

            if cbs.Count = 0 then
                obj.Outputs.Add x |> ignore
                
            cbs <- HashMap.add i cb cbs
            { new IDisposable with member __.Dispose() = remove x i }
        )
    
    member x.Mark() =
        cbs <- cbs |> HashMap.filter (fun _ cb -> try cb() with _ -> true)
        if check x then
            obj.Outputs.Add x |> ignore

        false

    interface IAdaptiveObject with
        member x.Tag
            with get() = null
            and set _ = ()

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
    /// cache for MultiCallbackObjects per IAdaptiveObject
    let private callbackObjects = ConditionalWeakTable<IAdaptiveObject, MultiCallbackObject>()

    /// utility getting/creating a MultiCallbackObjects for the given IAdaptiveObject  
    let private getMultiCallback (o : IAdaptiveObject) =    
        lock callbackObjects (fun () ->
            match callbackObjects.TryGetValue o with
            | (true, cbo) -> cbo
            | _ -> 
                let cbo = MultiCallbackObject(callbackObjects, o)
                callbackObjects.Add(o, cbo)
                cbo
        )


    type IAdaptiveObject with
        /// Registers a callback with the given object that will be executed
        /// whenever the object gets marked out-of-date.
        /// Note that it does not trigger when the object is currently out-of-date.
        /// Returns a disposable for removing the callback.
        member x.AddMarkingCallback (callback: unit -> unit) =
            let cbo = getMultiCallback x
            cbo.Subscribe(fun () -> callback(); true)
            
        /// Registers a callback with the given object that will be executed
        /// ONCE! when the next out-of-date marking visits the object.
        /// Note that it does not trigger when the object is currently out-of-date.
        /// Returns a disposable for removing the callback.
        member x.OnNextMarking (callback: unit -> unit) =
            let cbo = getMultiCallback x
            cbo.Subscribe(fun () -> 
                try 
                    callback()
                    false
                with :? LevelChangedException ->
                    true
            )
          