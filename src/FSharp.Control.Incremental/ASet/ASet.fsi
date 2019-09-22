namespace FSharp.Control.Incremental

// TODO: documentation

open FSharp.Control.Traceable

type ISetReader<'T> = 
    IOpReader<CountingHashSet<'T>, DHashSet<'T>>

[<Interface>]
type aset<'T> =
    abstract member IsConstant : bool
    abstract member Content : aref<HashSet<'T>>
    abstract member GetReader : unit -> ISetReader<'T>

module ASet =
    [<GeneralizableValue>]
    val empty<'T> : aset<'T> 
    val single : value : 'T -> aset<'T>
    val ofSeq : elements : seq<'T> -> aset<'T>
    val ofList : elements : list<'T> -> aset<'T>
    val ofArray : elements : 'T[] -> aset<'T>
    val ofHashSet : elements : HashSet<'T> -> aset<'T>
    val toARef : set : aset<'T> -> aref<HashSet<'T>>
    val map : mapping : ('A -> 'B) -> set : aset<'A> -> aset<'B>



