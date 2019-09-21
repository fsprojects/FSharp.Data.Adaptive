namespace FSharp.Control.Incremental

open System.Threading
open System.Collections.Generic

/// AdaptiveToken represents a token that can be passed to
/// inner AdaptiveObjects for evaluation.
/// When passing an AdaptiveToken to the evaluation-function of 
/// a cell the system will create a dependency edge internally and
/// future marking of the inner cell will also cause the calling cell to
/// be marked.
[<Struct>]
type AdaptiveToken =
    val mutable public Caller : IAdaptiveObject
    //val mutable public Locked : HashSet<IAdaptiveObject>

    member inline internal x.EnterRead(o : IAdaptiveObject) =
        Monitor.Enter o
                
    member inline internal x.ExitFaultedRead(o : IAdaptiveObject) =
        Monitor.Exit o

    member inline internal x.Downgrade(o : IAdaptiveObject) =
        //if x.Locked.Add o then
        //    o.ReaderCount <- o.ReaderCount + 1
        Monitor.Exit o

    member inline internal x.ExitRead(o : IAdaptiveObject) =
        //if x.Locked.Remove o then
        //    lock o (fun () ->
        //        let rc = o.ReaderCount - 1
        //        o.ReaderCount <- rc
        //        if rc = 0 then Monitor.PulseAll o
        //    )
        Monitor.Exit o

    member inline internal x.Release() =
        //for o in x.Locked do
        //    lock o (fun () ->
        //        let rc = o.ReaderCount - 1
        //        o.ReaderCount <- rc
        //        if rc = 0 then Monitor.PulseAll o
        //    )
        //x.Locked.Clear()
        ()

    member inline internal x.WithCaller (c : IAdaptiveObject) =
        AdaptiveToken(c)

    static member Top = AdaptiveToken(null)

    internal new(caller : IAdaptiveObject) =
        {
            Caller = caller
        }
