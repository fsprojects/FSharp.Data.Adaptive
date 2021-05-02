namespace FSharp.Data.Adaptive

open System

#nowarn "7331"

[<AbstractClass>]
type DecoratorObject(input : IAdaptiveObject) =
        
    // need to keep all decorators alive since they "live" in WeakOutputSets
    let decorators = System.Collections.Generic.Dictionary<WeakReference<IAdaptiveObject>, IndirectOutputObject>()

    let removeDecorator (d : IndirectOutputObject) =
        lock decorators (fun () ->
            decorators.Remove d.Real |> ignore
        )

    let getDecorator (caller : IAdaptiveObject) (self : IAdaptiveObject) =
        match caller with
        | :? IndirectOutputObject as caller ->
            caller
        | _ -> 
            lock decorators (fun () ->
                match decorators.TryGetValue caller.Weak with
                | (true, d) -> d
                | _ -> 
                    let o = IndirectOutputObject.Create(caller, self, removeDecorator)
                    decorators.[caller.Weak] <- o
                    o
            )
            
    interface IAdaptiveObject with
        member x.AllInputsProcessed(a) = input.AllInputsProcessed(a)
        member x.InputChanged(a,b) = input.InputChanged(a,b)
        member x.Mark() = input.Mark()
        member x.IsConstant = input.IsConstant
        member x.Level
            with get() = input.Level
            and set v = input.Level <- v
        member x.OutOfDate
            with get() = input.OutOfDate
            and set v = input.OutOfDate <- v
        member x.Outputs = input.Outputs
        member x.Tag 
            with get() = input.Tag
            and set t = input.Tag <- t
        member x.Weak = input.Weak

    member x.EvaluateAlways (token : AdaptiveToken) (action : AdaptiveToken -> 'a) =
        if Unchecked.isNull token.caller then
            action token
        else
            let c = getDecorator token.caller x
            action (token.WithCaller c)
                
    member x.EvaluateIfNeeded (token : AdaptiveToken) (whenUptoDate : 'a) (action : AdaptiveToken -> 'a) =
        x.EvaluateAlways token (fun token ->
            if input.OutOfDate then
                action token
            else
                whenUptoDate
        )
