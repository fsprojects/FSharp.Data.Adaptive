namespace FSharp.Control.Incremental

type aref<'T> =
    inherit IAdaptiveObject
    abstract member GetValue : AdaptiveToken -> 'T

type cref<'T> =
    inherit AdaptiveObject
    val mutable private value : 'T

    member x.Value
        with get() = x.value
        and set v =
            if not (Unchecked.equals x.value v) then
                x.value <- v
                x.MarkOutdated()
                
    member x.GetValue (token : AdaptiveToken) =
        x.EvaluateAlways token (fun _ ->
            x.value
        )

    interface aref<'T> with
        member x.GetValue t = x.GetValue t
        
    new(value : 'T) = { value = value }

module ARef =
    type internal MapRef<'a, 'b>(mapping : 'a -> 'b, input : aref<'a>) =
        inherit AdaptiveObject()

        let mutable cache : Option<'a * 'b> = None

        member x.GetValue(token : AdaptiveToken) =
            x.EvaluateAlways token (fun token ->
                if x.OutOfDate then
                    let i = input.GetValue token
                    match cache with
                    | Some (a, b) when Unchecked.equals a i ->
                        b
                    | _ ->
                        let b = mapping i
                        cache <- Some (i, b)
                        b
                else
                    match cache with
                    | Some (_, b) -> b
                    | None -> failwith "inconsistent"

            )

        interface aref<'b> with
            member x.GetValue t = x.GetValue t

    let inline init (value : 'a) =
        cref value

    let map (mapping : 'a -> 'b) (ref : aref<'a>) =
        MapRef(mapping, ref) :> aref<_>

    let inline force (ref : aref<'a>) =
        ref.GetValue AdaptiveToken.Top
