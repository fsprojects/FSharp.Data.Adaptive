namespace FSharp.Control.Incremental

open System

type aref<'T> =
    inherit IAdaptiveObject
    abstract member GetValue : AdaptiveToken -> 'T

[<Sealed>]
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
    type LazyOrValue<'a> =
        val mutable public Create   : unit -> 'a
        val mutable public Value    : 'a
        val mutable public IsValue  : bool

        new(value : 'a) = { Create = Unchecked.defaultof<_>; Value = value; IsValue = true }
        new(create : unit -> 'a) = { Create = create; Value = Unchecked.defaultof<_>; IsValue = false }
        
    type ConstantRef<'a> private(data : LazyOrValue<'a>) =
        inherit ConstantObject()
        let mutable data = data

        member x.GetValue(_token : AdaptiveToken) : 'a = 
            if data.IsValue then 
                data.Value
            else
                let v = Unchecked.defaultof<'a> //data.Create()
                data.IsValue <- true
                data.Value <- v
                data.Create <- Unchecked.defaultof<_>
                v

        interface aref<'a> with
            member x.GetValue t = x.GetValue t    

        static member Lazy (create : unit -> 'a) =
            ConstantRef<'a>(LazyOrValue<'a> create) :> aref<_>

        static member Value (value : 'a) =
            ConstantRef<'a>(LazyOrValue<'a> value) :> aref<_>

    [<AbstractClass>]
    type AbstractRef<'a>() =
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

    type MapRef<'a, 'b>(mapping : 'a -> 'b, input : aref<'a>) =
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

    type BindRef<'a, 'b>(mapping : 'a -> aref<'b>, input : aref<'a>) =
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
        ConstantRef.Value value

    let map (mapping : 'a -> 'b) (ref : aref<'a>) =
        if ref.IsConstant then 
            ConstantRef.Lazy (fun () -> ref |> force |> mapping)
        else
            MapRef(mapping, ref) :> aref<_>
            
    let bind (mapping : 'a -> aref<'b>) (ref : aref<'a>) =
        if ref.IsConstant then
            ref |> force |> mapping
        else
            BindRef<'a, 'b>(mapping, ref) :> aref<_>            
