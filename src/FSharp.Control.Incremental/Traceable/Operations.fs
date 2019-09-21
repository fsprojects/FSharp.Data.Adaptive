namespace FSharp.Control.Traceable

[<Struct>]
type SetOperation<'a>(value : 'a, cnt : int) =
    member x.Value = value
    member x.Count = cnt
    member x.Inverse = SetOperation(value, -cnt)

    override x.ToString() =
        if cnt = 1 then sprintf "Add(%A)" value
        elif cnt = -1 then sprintf "Rem(%A)" value
        elif cnt > 0 then sprintf "Add%d(%A)" cnt value
        elif cnt < 0 then sprintf "Rem%d(%A)" -cnt value
        else "Nop"

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SetOperation =
    let inline create (cnt : int) (v : 'a) = SetOperation(v, cnt)
    let inline add (v : 'a) = SetOperation(v, 1)   
    let inline rem (v : 'a) = SetOperation(v, -1)     
    let inline inverse (d : SetOperation<'a>) = d.Inverse
    let map (f : 'a -> 'b) (d : SetOperation<'a>) = SetOperation<'b>(f d.Value, d.Count)

[<AutoOpen>]
module SetDeltaExtensions =
    let inline Add(v : 'a) = SetOperation(v, 1)
    let inline Rem(v : 'a) = SetOperation(v, -1)

    type SetOperation<'a> with
        static member inline Add v = SetOperation(v, 1)
        static member inline Rem v = SetOperation(v, -1)

    let inline (|Add|Rem|) (d : SetOperation<'a>) =
        if d.Count > 0 then Add(d.Count, d.Value)
        else Rem(-d.Count, d.Value)