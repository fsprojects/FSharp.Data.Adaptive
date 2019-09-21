namespace FSharp.Control.Incremental

open System

type aref<'T> =
    inherit IAdaptiveObject
    abstract member GetValue : AdaptiveToken -> 'T

type cref<'T> =
    inherit AdaptiveObject
    val mutable private value : 'T

    member x.Value
        with get() = x.value
        and set v =
            if not (cheapEqual x.value v) then
                x.value <- v
                x.MarkOutdated()
                
    member x.GetValue (token : AdaptiveToken) =
        x.EvaluateAlways token (fun _ ->
            x.value
        )

    interface aref<'T> with
        member x.GetValue t = x.GetValue t
        
    new(value : 'T) = { value = value }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ARef =
    type internal ConstantRef<'a>(value : Choice<'a, Lazy<'a>>) =
        inherit ConstantObject()

        member x.GetValue(_token : AdaptiveToken) = 
            match value with
            | Choice1Of2 v -> v
            | Choice2Of2 l -> l.Value

        interface aref<'a> with
            member x.GetValue t = x.GetValue t    

        new (value : 'a) = ConstantRef<'a>(Choice1Of2 value)      
        new (value : Lazy<'a>) = ConstantRef<'a>(Choice2Of2 value)        

    [<AbstractClass>]
    type internal AbstractRef<'a>() =
        inherit AdaptiveObject()

        let mutable cache = Unchecked.defaultof<'a>

        abstract member Compute : AdaptiveToken -> 'a

        member x.GetValue(token : AdaptiveToken) =
            x.EvaluateAlways token (fun token ->
                if x.OutOfDate then
                    let v = x.Compute token
                    cache <- v
                    v
                else
                    cache                
            )

        interface aref<'a> with
            member x.GetValue t = x.GetValue t        

    type internal MapRef<'a, 'b>(mapping : 'a -> 'b, input : aref<'a>) =
        inherit AbstractRef<'b>()

        // can we avoid double caching (here and in AbstractRef)
        let mutable cache : Option<'a * 'b> = None

        override x.Compute(token : AdaptiveToken) =
            let i = input.GetValue token
            match cache with
            | Some (a, b) when cheapEqual a i ->
                b
            | _ ->
                let b = mapping i
                cache <- Some (i, b)
                b

        interface aref<'b> with
            member x.GetValue t = x.GetValue t

    type internal BindRef<'a, 'b>(mapping : 'a -> aref<'b>, input : aref<'a>) =
        inherit AbstractRef<'b>()

        let mutable inner : Option<'a * aref<'b>> = None
        let mutable inputDirty = 1

        override x.InputChanged(_, o) =
            if Object.ReferenceEquals(o, input) then 
                inputDirty <- 1

        override x.Compute(token : AdaptiveToken) =
            let inputDirty = System.Threading.Interlocked.Exchange(&inputDirty, 0) <> 0
            let value = input.GetValue token

            match inner with
            | Some (a, res) when not inputDirty -> // || cheapEqual a value ->
                res.GetValue token
            | _ ->
                let ref = mapping value
                match inner with
                | Some (_, old) when old <> ref ->
                    old.Outputs.Remove x |> ignore
                | _ ->
                    ()

                inner <- Some (value, ref)  
                ref.GetValue token     


    let inline force (ref : aref<'a>) =
        ref.GetValue AdaptiveToken.Top

    let inline init (value : 'a) =
        cref value

    let constant (value : 'a) =
        ConstantRef<'a>(value) :> aref<_>

    let map (mapping : 'a -> 'b) (ref : aref<'a>) =
        if ref.IsConstant then 
            ConstantRef<'b>(lazy (ref |> force |> mapping)) :> aref<_>
        else
            MapRef(mapping, ref) :> aref<_>

    let bind (mapping : 'a -> aref<'b>) (ref : aref<'a>) =
        if ref.IsConstant then
            ref |> force |> mapping
        else
            BindRef<'a, 'b>(mapping, ref) :> aref<_>            
