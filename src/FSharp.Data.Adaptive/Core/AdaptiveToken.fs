namespace FSharp.Control.Incremental

open System.Threading

/// AdaptiveToken represents a token that can be passed to
/// inner AdaptiveObjects for evaluation.
/// When passing an AdaptiveToken to the evaluation-function of 
/// a cell the system will create a dependency edge internally and
/// future marking of the inner cell will also cause the calling cell to
/// be marked.
[<Struct>]
type AdaptiveToken =

    /// Represents the calling IAdaptiveObject or null if none.
    ///
    /// Note, this is only mutable because that exposes the underlying field
    /// for (reportedly) more performant access.
    val mutable internal caller : IAdaptiveObject

    member x.Caller =
        if Unchecked.isNull x.caller then None
        else Some x.caller

    /// enters the read-lock on the given object
    member inline internal x.EnterRead(o : IAdaptiveObject) =
        Monitor.Enter o
                
    /// exits the read-lock on the given object when evaluation faulted
    member inline internal x.ExitFaultedRead(o : IAdaptiveObject) =
        Monitor.Exit o

    /// exits the read-lock on the given object downgrading it to a weak-lock
    member inline internal x.Downgrade(o : IAdaptiveObject) =
        //if x.Locked.Add o then
        //    o.ReaderCount <- o.ReaderCount + 1
        Monitor.Exit o

    /// exits the read-lock on the given object
    member inline internal x.ExitRead(o : IAdaptiveObject) =
        //if x.Locked.Remove o then
        //    lock o (fun () ->
        //        let rc = o.ReaderCount - 1
        //        o.ReaderCount <- rc
        //        if rc = 0 then Monitor.PulseAll o
        //    )
        Monitor.Exit o

    /// releases all held weak-locks
    member inline internal x.Release() =
        //for o in x.Locked do
        //    lock o (fun () ->
        //        let rc = o.ReaderCount - 1
        //        o.ReaderCount <- rc
        //        if rc = 0 then Monitor.PulseAll o
        //    )
        //x.Locked.Clear()
        ()

    /// creates a new AdaptiveToken with the given caller
    member inline internal x.WithCaller (c : IAdaptiveObject) =
        AdaptiveToken(c)

    /// the top-level AdaptiveToken without a calling IAdaptiveObject
    static member Top = AdaptiveToken(Unchecked.defaultof<_>)

    /// creates a new AdaptiveToken using the given caller
    internal new(caller : IAdaptiveObject) =
        {
            caller = caller
        }
