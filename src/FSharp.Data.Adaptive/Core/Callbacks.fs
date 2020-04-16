namespace FSharp.Data.Adaptive

open System
open System.Threading
open System.Runtime.CompilerServices
open System.Runtime.InteropServices


type private CallbackDisposable(remove : unit -> unit, makeGCRoot : bool, callback : unit -> bool) as this =
    let mutable isDisposed = false
    let mutable callback = callback
    let mutable remove = remove
    #if !FABLE_COMPILER
    let mutable gc = if makeGCRoot then GCHandle.Alloc(this) else Unchecked.defaultof<GCHandle>
    #endif

    member x.Dispose() =
        if not isDisposed then
            isDisposed <- true
            remove()
            remove <- id
            callback <- fun () -> false
            #if !FABLE_COMPILER
            if gc.IsAllocated then 
                gc.Free()
                gc <- Unchecked.defaultof<GCHandle>
            #endif

    interface IDisposable with
        member x.Dispose() = x.Dispose()

/// Represents an object providing callbacks in the dependency-tree
type internal MultiCallbackObject(table : ConditionalWeakTable<IAdaptiveObject, MultiCallbackObject>, obj: IAdaptiveObject) = 
    let mutable id = 0

    static let emptyOutputs = EmptyOutputSet() :> IWeakOutputSet
    let mutable level = obj.Level + 1
    
    let cbs : ref<HashMap<int, WeakReference<unit -> bool>>> = ref HashMap.empty
    let mutable obj = obj
    let mutable weak = null

    let newId() =
        #if FABLE_COMPILER
        let i = id + 1
        id <- i
        i
        #else
        Interlocked.Increment(&id)
        #endif

    let check (x : MultiCallbackObject) =
        if cbs.Value.Count = 0 && not (isNull (obj :> obj)) then
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
            cbs := HashMap.remove id !cbs
            check x |> ignore
        )

    /// adds a callback to the object. the returned boolean indicates whether the callback
    /// should automatically re-subscribe after being fired.
    member x.Subscribe(weak : bool, cb : unit -> bool) =
        lock x (fun () ->

            if cbs.Value.Count = 0 then
                obj.Outputs.Add x |> ignore
                
            let id = newId()
            let weakCallback = WeakReference<_>(cb)
            cbs := HashMap.add id weakCallback !cbs

            #if !FABLE_COMPILER
            let remove() = remove x id
            new CallbackDisposable(remove, not weak, cb) :> IDisposable
            #else
            { new IDisposable with 
                member __.Dispose() = remove x id
            }
            #endif

        )
    
    member x.Mark() =
        cbs := 
            !cbs |> HashMap.filter (fun _ cb -> 
                try 
                    match cb.TryGetTarget() with
                    | (true, cb) -> cb()
                    | _ -> false
                with _ ->
                    false
            )
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
        member internal x.AddMarkingCallback (weak : bool, callback: unit -> unit) =
            let cbo = getMultiCallback x
            cbo.Subscribe(weak, fun () -> callback(); true)
            
        /// Registers a callback with the given object that will be executed
        /// whenever the object gets marked out-of-date.
        /// Note that it does not trigger when the object is currently out-of-date.
        /// Returns a disposable for removing the callback.
        member x.AddMarkingCallback (callback: unit -> unit) =
            let cbo = getMultiCallback x
            cbo.Subscribe(false, fun () -> callback(); true)
            
        /// Registers a callback with the given object that will be executed
        /// whenever the object gets marked out-of-date.
        /// Note that it does not trigger when the object is currently out-of-date.
        /// Returns a disposable for removing the callback.
        member x.AddWeakMarkingCallback (callback: unit -> unit) =
            let cbo = getMultiCallback x
            cbo.Subscribe(true, fun () -> callback(); true)

        /// Registers a callback with the given object that will be executed
        /// ONCE! when the next out-of-date marking visits the object.
        /// Note that it does not trigger when the object is currently out-of-date.
        /// Returns a disposable for removing the callback.
        member x.OnNextMarking (callback: unit -> unit) =
            let cbo = getMultiCallback x
            cbo.Subscribe(false, fun () -> 
                try 
                    callback()
                    false
                with :? LevelChangedException ->
                    true
            )

        /// Registers a callback with the given object that will be executed
        /// ONCE! when the next out-of-date marking visits the object.
        /// Note that it does not trigger when the object is currently out-of-date.
        /// Returns a disposable for removing the callback.
        member x.OnWeakNextMarking (callback: unit -> unit) =
            let cbo = getMultiCallback x
            cbo.Subscribe(true, fun () -> 
                try 
                    callback()
                    false
                with :? LevelChangedException ->
                    true
            )
          