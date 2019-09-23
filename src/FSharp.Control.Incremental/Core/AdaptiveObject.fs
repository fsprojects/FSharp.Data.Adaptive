namespace FSharp.Control.Incremental

open System

/// Core implementation of IAdaptiveObject containing tools for evaluation
/// and locking
type AdaptiveObject =
    
    [<DefaultValue; ThreadStatic>]
    static val mutable private CurrentEvaluationDepth : int

    val mutable private outOfDate : bool
    val mutable private level: int 
    val mutable private outputs : WeakOutputSet
    val mutable private weak : WeakReference<IAdaptiveObject>
    
    /// used for resetting EvaluationDepth in eager evaluation
    static member internal UnsafeEvaluationDepth
        with get() = AdaptiveObject.CurrentEvaluationDepth
        and set v = AdaptiveObject.CurrentEvaluationDepth <- v

    /// Utility function for evaluating an object even if it
    /// is not marked as outOfDate.
    /// this method takes care of appropriate locking
    member x.EvaluateAlways (token : AdaptiveToken) (f : AdaptiveToken -> 'T) =
        let caller = token.caller
        let depth = AdaptiveObject.CurrentEvaluationDepth

        let mutable res = Unchecked.defaultof<_>
        token.EnterRead x

        try
            AdaptiveObject.CurrentEvaluationDepth <- depth + 1

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
                raise <| LevelChangedException(x.Level + depth - 1)
                                                                     
            res <- r

            if not (Unchecked.isNull caller) then
                x.outputs.Add caller |> ignore
                caller.Level <- max caller.Level (x.Level + 1)

        with _ ->
            AdaptiveObject.CurrentEvaluationDepth <- depth
            token.ExitFaultedRead x
            reraise()
                
        AdaptiveObject.CurrentEvaluationDepth <- depth
        // downgrade to read
        token.Downgrade x

        if Unchecked.isNull caller then
            token.Release()

        res

    /// Utility function for evaluating an object if
    /// it is marked as outOfDate. If the object is actually
    /// outOfDate the given function is executed and otherwise
    /// the given default value is returned.
    /// this method takes care of appropriate locking
    member inline x.EvaluateIfNeeded (token : AdaptiveToken) (otherwise : 'T) (f : AdaptiveToken -> 'T) =
        x.EvaluateAlways token (fun token ->
            if x.OutOfDate then 
                f token
            else
                otherwise
        )

    /// see IAdaptiveObject.Weak
    member x.Weak =
        // Note that we accept the race conditon here since locking the object
        // would potentially cause deadlocks and the worst case is, that we
        // create two different WeakReferences for the same object
        let w = x.weak
        if isNull w then
            let w = WeakReference<_>(x :> IAdaptiveObject)
            x.weak <- w
            w
        else
            w
            
    /// see IAdaptiveObject.OutOfDate
    member x.OutOfDate
        with get() = x.outOfDate
        and set o = x.outOfDate <- o
        
    /// see IAdaptiveObject.Level
    member x.Level
        with get() = x.level
        and set l = x.level <- l
        
    /// see IAdaptiveObject.Outputs
    member x.Outputs = x.outputs :> IWeakOutputSet
    
    /// see IAdaptiveObject.Mark()
    abstract Mark : unit -> bool
    default x.Mark() = true
    
    /// see IAdaptiveObject.AllInputsProcessed(transaction)
    abstract AllInputsProcessed : obj -> unit
    default x.AllInputsProcessed _ = ()
    
    /// see IAdaptiveObject.InputChanged(transaction, object)
    abstract InputChanged : obj * IAdaptiveObject -> unit
    default x.InputChanged(_,_) = ()

    interface IAdaptiveObject with
        member x.IsConstant = false
        member x.Weak = x.Weak
        member x.Outputs = x.Outputs
        member x.Mark() = x.Mark()
        member x.AllInputsProcessed(t) = x.AllInputsProcessed(t)
        member x.InputChanged(t, o) = x.InputChanged(t, o)

        member x.OutOfDate
            with get() = x.OutOfDate
            and set o = x.OutOfDate <- o

        member x.Level
            with get() = x.Level
            and set l = x.Level <- l
        
    /// creates a new (out-of-date) AdaptiveObject
    new() =
        { 
            outOfDate = true
            level = 0
            outputs = WeakOutputSet()
            weak = null
        }

/// Core implementation of IAdaptiveObject for constant objects.
/// the main goal of this implementation is to save memory when IAdaptiveObjects are known to be constant.
type ConstantObject() =
    let mutable weak : WeakReference<IAdaptiveObject> = null
    static let outputs = EmptyOutputSet() :> IWeakOutputSet

    interface IAdaptiveObject with
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