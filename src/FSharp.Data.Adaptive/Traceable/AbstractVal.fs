namespace FSharp.Data.Traceable

open System
open FSharp.Data.Adaptive

/// Base class for standard avals
[<AbstractClass; StructuredFormatDisplay("{AsString}")>]
type AbstractVal<'T>() =
    inherit AdaptiveObject()

    let mutable valueCache = Unchecked.defaultof<'T>

    abstract member Compute: AdaptiveToken -> 'T

    member x.GetValue(token: AdaptiveToken) =
        x.EvaluateAlways token (fun token ->
            if x.OutOfDate then
                let v = x.Compute token
                valueCache <- v
                v
            else
                valueCache                
        )

    member private x.AsString =
        if x.OutOfDate then sprintf "aval*(%A)" valueCache
        else sprintf "aval(%A)" valueCache

    override x.ToString() =
        if x.OutOfDate then String.Format("aval*({0})", valueCache)
        else String.Format("aval({0})", valueCache)
            
    interface IAdaptiveValue with
        member x.Accept (v : IAdaptiveValueVisitor<'R>) = v.Visit x
        member x.GetValueUntyped t = x.GetValue t :> obj
        member x.ContentType =
            #if FABLE_COMPILER
            typeof<obj>
            #else
            typeof<'T>
            #endif

    interface IAdaptiveValue<'T> with
        member x.GetValue t = x.GetValue t  