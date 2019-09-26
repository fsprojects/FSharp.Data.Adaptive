namespace FSharp.Data.Adaptive

open FSharp.Data.Traceable

/// An adaptive reader for alist that allows to pull operations and exposes its current state.
type IIndexListReader<'T> = 
    IOpReader<IndexList<'T>, IndexListDelta<'T>>

/// Adaptive list datastructure.
[<Interface>]
type AdaptiveIndexList<'T> =
    /// Is the list constant?
    abstract member IsConstant : bool

    /// The current content of the list as aval.
    abstract member Content : aval<IndexList<'T>>
    
    /// Gets a new reader to the list.
    abstract member GetReader : unit -> IIndexListReader<'T>

/// Adaptive list datastructure.
type alist<'T> = AdaptiveIndexList<'T>


/// Functional operators for the alist<_> type.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AList =

    /// The empty alist.
    [<GeneralizableValue>]
    val empty<'T> : alist<'T>

    /// A constant alist holding a single value.
    val single : value : 'T -> alist<'T>

    /// Creates an alist holding the given values.
    val ofSeq : elements : seq<'T> -> alist<'T>

    /// Creates an alist holding the given values.
    val ofList : elements : list<'T> -> alist<'T>

    /// Creates an alist holding the given values.
    val ofArray : elements : 'T[] -> alist<'T>

    /// Creates an alist holding the given values. `O(1)`
    val ofIndexList : elements : IndexList<'T> -> alist<'T>

    val mapi : mapping: (Index -> 'T1 -> 'T2) -> list: alist<'T1> -> alist<'T2>
    
    val map : mapping: ('T1 -> 'T2) -> list: alist<'T1> -> alist<'T2>
    
    val collecti : mapping: (Index -> 'T1 -> alist<'T2>) -> list: alist<'T1> -> alist<'T2>

    /// Creates an aval providing access to the current content of the list.
    val toAVal : list : alist<'T> -> aval<IndexList<'T>>