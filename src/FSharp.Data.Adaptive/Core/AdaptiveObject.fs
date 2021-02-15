namespace FSharp.Data.Adaptive

open System
open System.Threading
open System.Runtime.CompilerServices

#nowarn "7331"



/// Core implementation of IAdaptiveObject containing tools for evaluation
/// and locking
[<AbstractClass>]
type AdaptiveObject() =


    [<DefaultValue; ThreadStatic>]
    static val mutable private CurrentEvaluationDepth : int

    let mutable outOfDate : bool = true
    let mutable level: int = 0
    let mutable outputs : WeakOutputSet = WeakOutputSet()
    let mutable weak : WeakReference<IAdaptiveObject> = null
    let mutable tag : obj = null

    /// Used for resetting EvaluationDepth in eager evaluation
    [<CompilerMessage("for internal use", 7331, IsHidden = true)>]
    static member UnsafeEvaluationDepth
        with get() = AdaptiveObject.CurrentEvaluationDepth
        and set v = AdaptiveObject.CurrentEvaluationDepth <- v

    /// See IAdaptiveObject.Weak
    member x.Weak =
        // Note that we accept the race conditon here since locking the object
        // would potentially cause deadlocks and the worst case is, that we
        // create two different WeakReferences for the same object
        let w = weak
        if isNull w then
            let w = WeakReference<_>(x :> IAdaptiveObject)
            weak <- w
            w
        else
            w
            
    /// See IAdaptiveObject.OutOfDate
    member x.OutOfDate
        with get() = outOfDate
        and set o = outOfDate <- o
        
    /// See IAdaptiveObject.Level
    member x.Level
        with get() = level
        and set l = level <- l
        
    /// See IAdaptiveObject.Outputs
    member x.Outputs = outputs :> IWeakOutputSet
    
    /// See IAdaptiveObject.Mark()
    abstract MarkObject : unit -> bool
    default x.MarkObject() = true
    
    /// See IAdaptiveObject.AllInputsProcessed(transaction)
    abstract AllInputProcessedObject : obj -> unit
    default x.AllInputProcessedObject _ = ()
    
    /// See IAdaptiveObject.InputChanged(transaction, object)
    abstract InputChangedObject : obj * IAdaptiveObject -> unit
    default x.InputChangedObject(_,_) = ()

    member x.Tag
        with get() = tag
        and set t = tag <- t

    interface IAdaptiveObject with
        member x.IsConstant = false
        member x.Weak = x.Weak
        member x.Outputs = x.Outputs
        member x.Mark() = x.MarkObject()
        member x.AllInputsProcessed(t) = x.AllInputProcessedObject(t)
        member x.InputChanged(t, o) = x.InputChangedObject(t, o)
        
        member x.Tag
            with get() = x.Tag
            and set o = x.Tag <- o

        member x.OutOfDate
            with get() = x.OutOfDate
            and set o = x.OutOfDate <- o

        member x.Level
            with get() = x.Level
            and set l = x.Level <- l


[<AutoOpen>]
module AdadptiveObjectExtensions =

    [<CompilerMessage("for internal use", 7331, IsHidden = true)>]
    type AfterEvaluateCallbacks =
    
        [<DefaultValue; ThreadStatic>]
        static val mutable private _Callbacks : list<unit -> unit>

        static member Callbacks
            with get() = AfterEvaluateCallbacks._Callbacks
            and set v = AfterEvaluateCallbacks._Callbacks <- v

        static member Add(action : unit -> unit) =
            if AdaptiveObject.UnsafeEvaluationDepth <= 0 then transact action
            elif isNull (AfterEvaluateCallbacks._Callbacks :> obj) then AfterEvaluateCallbacks._Callbacks <- [action]
            else AfterEvaluateCallbacks._Callbacks <- action :: AfterEvaluateCallbacks._Callbacks
            
        [<CompilerMessage("for internal use", 7331, IsHidden = true)>]
        static member Run() =
            if not (isNull (AfterEvaluateCallbacks._Callbacks :> obj)) then
                match AfterEvaluateCallbacks._Callbacks with
                | [] -> ()
                | cbs -> 
                    AfterEvaluateCallbacks._Callbacks <- []
                    transact (fun () ->
                        cbs |> List.iter (fun cb -> cb())
                    )

    type AdaptiveObject with

        /// Executes the given action after the (currently running) evaluation has finished (once).
        static member inline RunAfterEvaluate(action: unit -> unit) =
            AfterEvaluateCallbacks.Add action


        /// Utility function for evaluating an object even if it is not marked as outOfDate.
        /// This method takes care of appropriate locking
        member inline x.EvaluateAlways(token : AdaptiveToken) (f : AdaptiveToken -> 'T) =
            let caller = token.caller
            let depth = AdaptiveObject.UnsafeEvaluationDepth

            let mutable res = Unchecked.defaultof<_>
            Monitor.Enter x

            try
                AdaptiveObject.UnsafeEvaluationDepth <- depth + 1

                // this evaluation is performed optimistically
                // meaning that the "top-level" object needs to be allowed to
                // pull at least one value on every path.
                // This property must therefore be maintained for every
                // path in the entire system.
                let r = f(token.WithCaller x)
                x.OutOfDate <- false

                // if the object's level just got greater than or equal to
                // the level of the running transaction (if any)
                // we raise an exception since the evaluation
                // could be inconsistent atm.
                // the only exception to that is the top-level object itself
                let maxAllowedLevel =
                    if depth > 1 then Transaction.RunningLevel - 1
                    else Transaction.RunningLevel

                if x.Level > maxAllowedLevel then
                    //printfn "%A tried to pull from level %A but has level %A" top.Id level top.Level
                    // all greater pulls would be from the future
                    raise <| LevelChangedException(x.Level + depth)
                                                                     
                res <- r

                if not (Unchecked.isNull caller) then
                    x.Outputs.Add caller |> ignore
                    caller.Level <- max caller.Level (x.Level + 1)

            with _ ->
                AdaptiveObject.UnsafeEvaluationDepth <- depth
                Monitor.Exit x
                reraise()
                
            AdaptiveObject.UnsafeEvaluationDepth <- depth
            Monitor.Exit x

            if depth = 0 && Unchecked.isNull caller && not Transaction.HasRunning then
                AfterEvaluateCallbacks.Run()

            res

        /// Utility function for evaluating an object if it is marked as outOfDate.
        /// If the object is actually outOfDate the given function is executed and otherwise
        /// The given default value is returned. This method takes care of appropriate locking
        member inline x.EvaluateIfNeeded (token : AdaptiveToken) (otherwise : 'T) (f : AdaptiveToken -> 'T) =
            x.EvaluateAlways token (fun token ->
                if x.OutOfDate then 
                    f token
                else
                    otherwise
            )





   
/// Core implementation of IAdaptiveObject for constant objects.
/// The main goal of this implementation is to save memory when IAdaptiveObjects are known to be constant.
type ConstantObject() =
    let mutable weak : WeakReference<IAdaptiveObject> = null
    static let outputs = EmptyOutputSet() :> IWeakOutputSet

    interface IAdaptiveObject with
        member x.Tag
            with get() = null
            and set _ = ()

        member x.IsConstant = true
        member x.Weak =
            // Note that we accept the race conditon here since locking the object
            // would potentially cause deadlocks and the worst case is, that we
            // create two different WeakReferences for the same object
            let w = weak
            if isNull w then
                let w = WeakReference<_>(x :> IAdaptiveObject)
                weak <- w
                w
            else
                w
        member x.Outputs = outputs
        member x.Mark() = false
        member x.AllInputsProcessed(_) = ()
        member x.InputChanged(_, _) = ()

        member x.OutOfDate
            with get() = false
            and set o = ()

        member x.Level
            with get() = 0
            and set l = ()


#if !FABLE_COMPILER

/// A SynchronizationContext properly handling FSharp.Data.Adaptive's internal ThreadLocals in presense of
/// WPF/WinForms-style lock-interception (code being executed on the main thread while locks contend).
/// Due to our use of thread-locals/statics this interception breaks some of the system's internal state and 
/// AdaptiveSynchronizationContext.Install() can be used to prevent this.
type AdaptiveSynchronizationContext(captured : SynchronizationContext) =
    inherit SynchronizationContext()

    static let mutable installed = 0

    do base.SetWaitNotificationRequired()

    override x.CreateCopy() =
        AdaptiveSynchronizationContext((if isNull captured then null else captured.CreateCopy())) :> SynchronizationContext

    override x.Post(cp : SendOrPostCallback, s : Object) =
        if isNull captured then base.Post(cp,s)
        else captured.Post(cp,s)

    override x.Send(cp : SendOrPostCallback, s : Object) = 
        if isNull captured then base.Send(cp,s)
        else captured.Post(cp,s)

    override x.Wait(waitHandles : IntPtr[], waitAll : bool, millisecondsTimeout : int) =
        // evacuate all thread-locals/static
        let tc = Transaction.Current
        let tr = Transaction.Running
        let ad = AdaptiveObject.UnsafeEvaluationDepth
        let ac = AfterEvaluateCallbacks.Callbacks
        try
            // reset all thread-locals/static to their default values
            Transaction.Current <- ValueNone
            Transaction.Running <- ValueNone
            AdaptiveObject.UnsafeEvaluationDepth <- 0
            AfterEvaluateCallbacks.Callbacks <- []

            // run the inner code
            try
                if isNull captured then 
                    base.Wait(waitHandles, waitAll, millisecondsTimeout)
                else 
                    captured.Wait(waitHandles, waitAll, millisecondsTimeout)
            with _ -> 
                reraise()
        finally 
            // reset all thread-locals/static to their initial values
            Transaction.Current <- tc
            Transaction.Running <- tr
            AdaptiveObject.UnsafeEvaluationDepth <- ad
            AfterEvaluateCallbacks.Callbacks <- ac

    /// Installs the AdaptiveSynchronizationContext globally.  
    static member Install() =
        if Interlocked.Exchange(&installed, 1) = 0 then
            System.Threading.SynchronizationContext.SetSynchronizationContext(
                new AdaptiveSynchronizationContext(
                    SynchronizationContext.Current
                )
            )

#endif