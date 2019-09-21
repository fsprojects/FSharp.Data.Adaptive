namespace FSharp.Control.Incremental.Reference

type aref<'a> =
    abstract member GetValue : unit -> 'a

type cref<'a>(value : 'a) =
    let mutable value = value

    member x.GetValue() =
        value

    member x.Value 
        with get() = value
        and set v = value <- v

    interface aref<'a> with
        member x.GetValue() = x.GetValue()

module ARef =
    let force (ref : aref<'a>) = ref.GetValue()

    let init (value : 'a) =
        cref value

    let constant (value : 'a) =
        { new aref<'a> with
            member x.GetValue() = value
        }

    let map (mapping : 'a -> 'b) (input : aref<'a>) =
        { new aref<'b> with
            member x.GetValue() = input.GetValue() |> mapping
        }

    let bind (mapping : 'a -> aref<'b>) (input : aref<'a>) =
        { new aref<'b> with
            member x.GetValue() =
                input.GetValue() |> mapping |> force
        }
    
