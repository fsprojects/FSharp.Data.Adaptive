namespace FSharp.Data.Adaptive

open System.Threading

#nowarn "7331"

/// AdaptiveToken represents a token that can be passed to
/// inner AdaptiveObjects for evaluation.
/// when passing an AdaptiveToken to the evaluation-function of 
/// a cell the system will create a dependency edge internally and
/// future marking of the inner cell will also cause the calling cell to
/// be marked.
[<Struct>]
type AdaptiveToken =

    /// Represents the calling IAdaptiveObject or null if none.
    ///
    /// Note, this is only mutable because that exposes the underlying field
    /// for (reportedly) more performant access.
    [<CompilerMessage("for internal use", 7331, IsHidden = true)>]
    val mutable caller : IAdaptiveObject

    member x.Caller =
        if Unchecked.isNull x.caller then None
        else Some x.caller

    /// Creates a new AdaptiveToken with the given caller
    member x.WithCaller (c : IAdaptiveObject) =
        AdaptiveToken(c)

    /// The top-level AdaptiveToken without a calling IAdaptiveObject
    static member Top = AdaptiveToken(Unchecked.defaultof<_>)

    /// Creates a new AdaptiveToken using the given caller
    internal new(caller : IAdaptiveObject) =
        {
            caller = caller
        }
