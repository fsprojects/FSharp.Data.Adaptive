namespace FSharp.Control.Incremental

// TODO: documentation

[<Sealed>]
type cset<'T> =
    interface aset<'T>

    member Count : int
    member IsEmpty : bool
    member Contains : value : 'T -> bool
    member Value : HashSet<'T> with get, set

    member Add : value : 'T -> bool
    member Remove : value : 'T -> bool
    member Clear : unit -> unit
    member UnionWith : other : seq<'T> -> unit
    member ExceptWith : other : seq<'T> -> unit

    member GetReader : unit -> ISetReader<'T>

    new : unit -> cset<'T>
    new : seq<'T> -> cset<'T>
    new : HashSet<'T> -> cset<'T>


