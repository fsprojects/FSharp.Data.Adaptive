namespace FSharp.Control.Incremental

open FSharp.Control.Traceable

/// an incremental reader for aset that allows to pull operations and exposes its current state.
type ISetReader<'T> = 
    IOpReader<CountingHashSet<'T>, DHashSet<'T>>

/// incremental set datastructure.
[<Interface>]
type aset<'T> =
    /// is the set constant?
    abstract member IsConstant : bool

    /// the current content of the set as aref.
    abstract member Content : aref<HashSet<'T>>
    
    /// gets a new reader to the set.
    abstract member GetReader : unit -> ISetReader<'T>

/// functional operators for aset<_>
module ASet =
    /// the empty aset.
    [<GeneralizableValue>]
    val empty<'T> : aset<'T> 

    /// a constant aset holding a single value.
    val single : value : 'T -> aset<'T>
    
    /// creates an aset holding the given values.
    val ofSeq : elements : seq<'T> -> aset<'T>
    
    /// creates an aset holding the given values.
    val ofList : elements : list<'T> -> aset<'T>
    
    /// creates an aset holding the given values.
    val ofArray : elements : 'T[] -> aset<'T>

    /// creates an aset holding the given values. `O(1)`
    val ofHashSet : elements : HashSet<'T> -> aset<'T>

    /// creates an aref providing access to the current content of the set.
    val toARef : set : aset<'T> -> aref<HashSet<'T>>

    /// incrementally maps over the given set.
    val map : mapping : ('A -> 'B) -> set : aset<'A> -> aset<'B>



