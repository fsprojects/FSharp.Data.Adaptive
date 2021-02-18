namespace FSharp.Data.Adaptive

open System
open System.Threading

[<AutoOpen>]
module internal LockingExtensions =
    type IAdaptiveObject with
        /// Acquires a write-lock to an AdaptiveObject
        member inline o.EnterWrite() =
            Monitor.Enter o
            //while o.ReaderCount > 0 do
            //    Monitor.Wait o |> ignore
            
        /// Releases the write-lock to the AdaptiveObject
        member inline o.ExitWrite() =
            Monitor.Exit o
        
        /// Determines whether the object is locked and out-of-date
        member inline o.IsOutdatedCaller() =
            Monitor.IsEntered o && o.OutOfDate

/// When evaluating AdaptiveObjects inside a Transaction 
/// (aka eager evaluation) their level might be inconsistent when
/// attempting to evaluate. Therefore the evaluation may raise
/// this exception causing the evaluation to be delayed to a later
/// time in the Transaction.
exception LevelChangedException of 
    /// The new level for the top-level object.
    newLevel : int


/// Holds a set of adaptive objects which have been changed and shall
/// therefore be marked as outOfDate. Committing the transaction propagates
/// these changes into the dependency-graph, takes care of the correct
/// execution-order and acquires appropriate locks for all objects affected.
type Transaction() =

    // Each thread may have its own running transaction
    [<ThreadStatic; DefaultValue>]
    static val mutable private RunningTransaction : ValueOption<Transaction>

    [<ThreadStatic; DefaultValue>]
    static val mutable private CurrentTransaction : ValueOption<Transaction>

    static let cmp = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(fun struct(l,_) struct(r,_) -> compare l r)

    // We use a duplicate-queue here since we expect levels to be identical quite often
    let q = TransactQueue<IAdaptiveObject>()

    let mutable current : IAdaptiveObject = Unchecked.defaultof<_>
    let mutable currentLevel = 0
    let mutable finalizers : list<unit -> unit> = []
    let outputs = ref (Array.zeroCreate 8)

    let runFinalizers () =
        #if FABLE_COMPILER 
        let fs = let v = finalizers in finalizers <- []; v
        #else
        let fs = Interlocked.Exchange(&finalizers, [])
        #endif
        for f in fs do f()
        
    member x.AddFinalizer (f : unit->unit) =
        #if FABLE_COMPILER 
        finalizers <- f :: finalizers
        #else
        Interlocked.Change(&finalizers, (fun a -> f::a) ) |> ignore
        #endif

    member x.IsContained e = q.Contains e

    /// Gets or sets the transaction currently running on this thread (if any)
    static member Running
        with get() = Transaction.RunningTransaction
        and set r = Transaction.RunningTransaction <- r

    /// Gets or sets the transaction currently being built on this thread (via transact (fun () -> ...))
    static member Current
        with get() = Transaction.CurrentTransaction
        and set r = Transaction.CurrentTransaction <- r

    /// Indicates if inside a running Transaction
    static member HasRunning =
        Transaction.RunningTransaction.IsSome
       
    /// Gets the level of the currently running Transaction or
    /// Int32.MaxValue when no Transaction is running
    static member RunningLevel =
        match Transaction.RunningTransaction with
            | ValueSome t -> t.CurrentLevel
            | _ -> Int32.MaxValue - 1

    /// Gets the current Level the Transaction operates on
    member x.CurrentLevel = currentLevel

    /// Enqueues an adaptive object for marking
    member x.Enqueue(e : IAdaptiveObject) =
        q.Enqueue(e.Level, e)

    /// Gets the current AdaptiveObject being marked
    member x.CurrentAdapiveObject = 
        if Unchecked.isNull current then None
        else Some current

    /// Performs the entire marking process, causing all affected objects to
    /// be made consistent with the enqueued changes.
    member x.Commit() =

        // cache the currently running transaction (if any)
        // and make ourselves current.
        let old = Transaction.RunningTransaction
        Transaction.RunningTransaction <- ValueSome x
        let mutable outputCount = 0

        while not q.IsEmpty do
            // dequeue the next element (having the minimal level)
            let struct(l, e) = q.Dequeue()
            current <- e
            currentLevel <- l

            // since we're about to access the outOfDate flag
            // for this object we must acquire a lock here.
            // Note that the transaction will at most hold one
            // lock at a time.
            if e.IsOutdatedCaller() then
                e.AllInputsProcessed(x)

            else
                e.EnterWrite()
                try
                    outputCount <- 0

                    // if the element is already outOfDate we
                    // do not traverse the graph further.
                    if e.OutOfDate then
                        e.AllInputsProcessed(x)
                    else
                        // if the object's level has changed since it
                        // was added to the queue we re-enqueue it with the new level
                        // Note that this may of course cause runtime overhead and
                        // might even change the asymptotic runtime behaviour of the entire
                        // system in the worst case but we opted for this approach since
                        // it is relatively simple to implement.
                        if currentLevel <> e.Level then
                            q.Enqueue(e.Level, e)
                        else
                            // however if the level is consistent we may proceed
                            // by marking the object as outOfDate
                            e.OutOfDate <- true
                            e.AllInputsProcessed(x)
                
                            try 
                                // here mark and the callbacks are allowed to evaluate
                                // the adaptive object but must expect any call to AddOutput to 
                                // raise a LevelChangedException whenever a level has been changed
                                if e.Mark() then
                                    // if everything succeeded we return all current outputs
                                    // which will cause them to be enqueued 
                                    outputCount <- e.Outputs.Consume(outputs)

                                else
                                    e.OutOfDate <- false
                                    // if Mark told us not to continue we're done here
                                    ()

                            with LevelChangedException newLevel ->
                                // if the level was changed either by a callback
                                // or Mark we re-enqueue the object with the new level and
                                // mark it upToDate again (since it would otherwise not be processed again)
                                e.Level <- max e.Level newLevel
                                e.OutOfDate <- false
                                q.Enqueue(e.Level, e)
                
                finally 
                    e.ExitWrite()

                // finally we enqueue all returned outputs
                let outputs = outputs.Value
                for i in 0 .. outputCount - 1 do
                    let o = outputs.[i]
                    o.InputChanged(x, e)
                    x.Enqueue o

            current <- Unchecked.defaultof<_>
            
        // when the commit is over we restore the old
        // running transaction (if any)
        Transaction.RunningTransaction <- old
        currentLevel <- 0

    /// Disposes the transaction running all of its "Finalizers"
    member x.Dispose() = 
        runFinalizers()

    interface IDisposable with
        member x.Dispose() = x.Dispose()

/// Module for transaction related functions. (e.g. transact)
[<AutoOpen>]
module Transaction =
    /// Returns the currently running transaction or (if none)
    /// the current transaction for the calling thread
    let getCurrentTransaction() =
        match Transaction.Running with
        | ValueSome r -> ValueSome r
        | ValueNone ->
            match Transaction.Current with
            | ValueSome c -> ValueSome c
            | ValueNone -> ValueNone

    let useTransaction (t : Transaction) (action : unit -> 'T) =
        let old = Transaction.Current
        try
            Transaction.Current <- ValueSome t
            action()
        finally
            Transaction.Current <- old

    let makeCurrent (t : Transaction) : IDisposable =
        let old = Transaction.Current
        Transaction.Current <- ValueSome t

        { new IDisposable with
            member x.Dispose() = 
                Transaction.Current <- old
        }

    // TODO: identical to useTransaction ?
    let inline internal useCurrent (t : Transaction) (action : unit -> 'T) =
        let old = Transaction.Current
        try
            Transaction.Current <- ValueSome t
            action()
        finally
            Transaction.Current <- old

    /// Executes a function "inside" a newly created
    /// transaction and commits the transaction.
    let transact (action : unit -> 'T) =
        use t = new Transaction()
        let r = useCurrent t action
        t.Commit() 
        // TODO: no t.Dispose() intentional? C# API does
        r
        
    /// Executes a function "inside" the current transaction
    /// or creates and commits a new one whenever none was current.
    let internal transactIfNecessary (action : unit -> 'T) =
        match Transaction.Current with
        | ValueSome _ -> action()
        | ValueNone -> transact action
            

    // Defines some extension utilites for IAdaptiveObjects
    type IAdaptiveObject with
        /// Utility for marking adaptive object as outOfDate.
        /// Note that this function will actually enqueue the
        /// object to the current transaction and will fail if
        /// no current transaction can be found.
        /// However objects which are already outOfDate might
        /// also be "marked" when not having a current transaction.
        member x.MarkOutdated () =
            match getCurrentTransaction() with
                | ValueSome t -> t.Enqueue(x)
                | ValueNone -> 
                    lock x (fun () -> 
                        if x.OutOfDate then ()
                        elif x.Outputs.IsEmpty then x.OutOfDate <- true
                        else failwith "cannot mark object without transaction"
                    )
                    
        /// Utility for marking adaptive object as outOfDate.
        /// Note that this function will actually enqueue the
        /// object to the current transaction and will fail if
        /// no current transaction can be found.
        /// However objects which are already outOfDate might
        /// Also be "marked" when not having a current transaction.
        member x.MarkOutdated (fin : unit -> unit) =
            match getCurrentTransaction() with
                | ValueSome t -> 
                    t.Enqueue(x)
                    t.AddFinalizer(fin)
                | ValueNone -> 
                    lock x (fun () -> 
                        if x.OutOfDate then ()
                        elif x.Outputs.IsEmpty then x.OutOfDate <- true
                        else failwith "cannot mark object without transaction"
                    )
                    fin()
