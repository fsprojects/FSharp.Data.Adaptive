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
    
    #if !FABLE_COMPILER
    // we use a ConcurrentDictionary so that multiple threads can safely add and remove callbacks with out accidentally losing them
    // since this type is NON immutable it makes sense to use a datatype here that has the behavior we want to achieve
    // ConcurrentDictionary also has a few nice properties when used in our case here. Since we use a monotonically increasing integer
    // new callbacks will be available to the enumerator if we have finished it yet. This is because the values are returned in key order
    // with new keys that are greater than the current enumerator value being accessible.
    let cbs : System.Collections.Concurrent.ConcurrentDictionary<int, WeakReference<unit -> bool>> = System.Collections.Concurrent.ConcurrentDictionary()
    #else
    let cbs : ref<HashMap<int, WeakReference<unit -> bool>>> = ref HashMap.empty
    #endif
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

    /// this function checks to see if we need to release resources if there are no active callbacks anymore
    let check (x : MultiCallbackObject) =
        // we don't need a lock here because we are using obj being null to indicate this is a dead object
        if isNull (obj :> obj) then false
        else
            // we must take a table lock here to prevent us from
            // deciding to release this object before setMultiCallback can fetch this instance for a callback
            // the Count must be checked after the lock since we need to be sure no one is add a subscription
            // while we are in here.
            lock table (fun _ ->                 
                #if !FABLE_COMPILER
                if cbs.Count = 0 then // since there are not more live callbacks we'd like to release this object
                #else
                if cbs.Value.Count = 0 then
                #endif
                    obj.Outputs.Remove x |> ignore
                    table.Remove obj |> ignore  // it doesn't matter if the table has an entry for us or not
                                                // if table didn't have a value then we need to clean up and make sure
                                                // we return false because we're dead to the rest of the system, it if returns
                                                // true then we've removed ourselve and we are dead to the system so clean ourselves
                                                // up
                    level <- 0
                    obj <- Unchecked.defaultof<_>
                    weak <- null
                    false
                else
                    true
            )

    let remove (x : MultiCallbackObject) (id : int) =
        lock x (fun () ->
        #if !FABLE_COMPILER
            cbs.TryRemove id |> ignore // attempt to remove the callback id
        #else
            cbs.Value <- HashMap.remove id cbs.Value
        #endif
            check x |> ignore
        )

    /// adds a callback to the object. the returned boolean indicates whether the callback
    /// should automatically re-subscribe after being fired.
    member x.Subscribe(weak : bool, cb : unit -> bool) =
        lock x (fun () ->
            assert ((obj :> obj) <> null)   // we assert that obj is not null, since the locks taken by check and setMultiCallback should prevent
                                            // our getting to this point with an dead instance (obj is null). In debug mode we would see this
                                            // fire an assertion failure
            #if !FABLE_COMPILER
            if cbs.Count = 0 then
            #else
            if cbs.Value.Count = 0 then
            #endif
                obj.Outputs.Add x |> ignore
                
            let id = newId()
            let weakCallback = WeakReference<_>(cb)
            #if !FABLE_COMPILER
            let success = cbs.TryAdd(id,weakCallback)
            assert success                        // we assert success (this gets compiled out) here since by construction the lock on x
                                                  // here and the increment of newId guarentee a new value of id until it wraps after
                                                  // 4 billing subscriptions if that were to happen and the old subscriptions are still
                                                  // around then new callbacks would simply be dropped on the ground for those id already
                                                  // in the map.
            #else
            cbs.Value <- HashMap.add id weakCallback cbs.Value
            #endif

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
        #if !FABLE_COMPILER
        // this loop allows us to move through the callbacks even if more are being added
        // we check each callback to see if it is still alive and then if we are to retain
        // it after executing
        for (KeyValue(k,cb)) in cbs do // using a for loop here to indicate we doing some mutable code here
                         // since this is usually considered a code smell in F# this indicates
                         // that you should pay attention here. Please see the note where cbs
                         // is declared regarding the behavior an enumerator on the 
                         // ConcurrentDictionary
            let filter =
                match cb.TryGetTarget() with
                | (true, cb) -> cb()
                | _ -> false
            if not filter then
                // remove this entry
                let success,_ = cbs.TryRemove(k) 
                assert success // we assert that we successfully removed the entry, this shouldn't normally fail
        #else
        cbs.Value <- 
            cbs.Value |> HashMap.filter (fun _ cb -> 
                try 
                    match cb.TryGetTarget() with
                    | (true, cb) -> cb()
                    | _ -> false
                with _ ->
                    false
            )
        #endif
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
    let private setMultiCallback (o : IAdaptiveObject) weak callback =    
        lock callbackObjects (fun () ->
            let cbo =
                match callbackObjects.TryGetValue o with
                | (true, cbo) -> cbo
                | _ -> 
                    let cbo = MultiCallbackObject(callbackObjects, o)
                    callbackObjects.Add(o, cbo)
                    cbo
            cbo.Subscribe(weak,callback)
        )


    type IAdaptiveObject with

        /// Registers a callback with the given object that will be executed
        /// whenever the object gets marked out-of-date.
        /// Note that it does not trigger when the object is currently out-of-date.
        /// Returns a disposable for removing the callback.
        member internal x.AddMarkingCallback (weak : bool, callback: unit -> unit) =
            setMultiCallback x weak (fun () -> callback(); true)
            
        /// Registers a callback with the given object that will be executed
        /// whenever the object gets marked out-of-date.
        /// Note that it does not trigger when the object is currently out-of-date.
        /// Returns a disposable for removing the callback.
        member x.AddMarkingCallback (callback: unit -> unit) =
            setMultiCallback x false (fun () -> callback(); true)
            
        /// Registers a callback with the given object that will be executed
        /// whenever the object gets marked out-of-date.
        /// Note that it does not trigger when the object is currently out-of-date.
        /// Returns a disposable for removing the callback.
        member x.AddWeakMarkingCallback (callback: unit -> unit) =
            setMultiCallback x true (fun () -> callback(); true)

        /// Registers a callback with the given object that will be executed
        /// ONCE! when the next out-of-date marking visits the object.
        /// Note that it does not trigger when the object is currently out-of-date.
        /// Returns a disposable for removing the callback.
        member x.OnNextMarking (callback: unit -> unit) =
            setMultiCallback x false (fun () -> 
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
            setMultiCallback x true (fun () -> 
                try 
                    callback()
                    false
                with :? LevelChangedException ->
                    true
            )
          