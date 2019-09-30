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
    val single : value: 'T -> alist<'T>

    /// Creates an alist holding the given values.
    val ofSeq : elements: seq<'T> -> alist<'T>

    /// Creates an alist holding the given values.
    val ofList : elements: list<'T> -> alist<'T>

    /// Creates an alist holding the given values.
    val ofArray : elements: 'T[] -> alist<'T>

    /// Creates an alist holding the given values. `O(1)`
    val ofIndexList : elements: IndexList<'T> -> alist<'T>
    
    /// Creates an alist using the given reader-creator.
    val ofReader : create : (unit -> #IOpReader<IndexListDelta<'T>>) -> aset<'T>
    
    /// Adaptively applies the given mapping function to all elements and returns a new alist containing the results.
    val mapi : mapping: (Index -> 'T1 -> 'T2) -> list: alist<'T1> -> alist<'T2>

    /// Adaptively applies the given mapping function to all elements and returns a new alist containing the results.
    val map : mapping: ('T1 -> 'T2) -> list: alist<'T1> -> alist<'T2>
    
    /// Adaptively chooses all elements returned by mapping.  
    val choosei : mapping: (Index -> 'T1 -> option<'T2>) -> list: alist<'T1> -> alist<'T2>

    /// Adaptively chooses all elements returned by mapping.  
    val choose : mapping: ('T1 -> option<'T2>) -> list: alist<'T1> -> alist<'T2>
        
    /// Adaptively filters the list using the given predicate.
    val filteri : mapping: (Index -> 'T -> bool) -> list: alist<'T> -> alist<'T>

    /// Adaptively filters the list using the given predicate.
    val filter : mapping: ('T -> bool) -> list: alist<'T> -> alist<'T>
        

    /// Adaptively applies the given mapping function to all elements and returns a new alist holding the concatenated results.
    val collecti : mapping: (Index -> 'T1 -> alist<'T2>) -> list: alist<'T1> -> alist<'T2>

    /// Adaptively applies the given mapping function to all elements and returns a new alist holding the concatenated results.
    val collect : mapping: ('T1 -> alist<'T2>) -> list: alist<'T1> -> alist<'T2>

    /// Adaptively concatenates the given lists.
    val concat : lists: #seq<alist<'T>> -> alist<'T>
    
    /// Adaptively concatenates the given lists.
    val append : l: alist<'T> -> r: alist<'T> -> alist<'T>

    /// Creates an aval providing access to the current content of the list.
    val toAVal : list : alist<'T> -> aval<IndexList<'T>>

    /// Adaptively maps over the given aval and returns the resulting list.
    val bind : mapping: ('T1 -> alist<'T2>) -> list: alist<'T1> -> alist<'T2>

    /// Sorts the list using the keys given by projection.
    /// Note that the sorting is stable.
    val sortByi : mapping: (Index -> 'T1 -> 'T2) -> list: alist<'T1> -> alist<'T1>
        when 'T2 : comparison
 
    /// Sorts the list using the keys given by projection.
    /// Note that the sorting is stable.
    val sortBy : mapping: ('T1 -> 'T2) -> list: alist<'T1> -> alist<'T1>
        when 'T2 : comparison

    /// Sorts the list using the given compare function.
    /// Note that the sorting is stable.
    val sortWith : compare: ('T -> 'T -> int) -> list: alist<'T> -> alist<'T>


    /// Adaptively folds over the list using add for additions and trySubtract for removals.
    /// Note the trySubtract may return None indicating that the result needs to be recomputed.
    /// Also note that the order of elements given to add/trySubtract is undefined.
    val foldHalfGroup : add : ('S -> 'A -> 'S) -> trySubtract : ('S -> 'A -> option<'S>) -> zero : 'S -> list : alist<'A> -> aval<'S>
    
    /// Adaptively folds over the list using add for additions and subtract for removals.
    /// Note that the order of elements given to add/subtract is undefined.
    val foldGroup : add : ('S -> 'A -> 'S) -> subtract : ('S -> 'A -> 'S) -> zero : 'S -> list : alist<'A> -> aval<'S>

    /// Adaptively folds over the list using add for additions and recomputes the value on every removal.
    /// Note that the order of elements given to add is undefined.
    val fold : add : ('S -> 'A -> 'S) -> zero : 'S -> list : alist<'A> -> aval<'S>

    /// Adaptively computes the sum all entries in the list.
    val inline sum : list : alist<'T> -> aval<'S>
        when ('T or 'S) : (static member (+) : 'S -> 'T -> 'S) 
        and  ('T or 'S) : (static member (-) : 'S -> 'T -> 'S) 
        and   'S : (static member Zero : 'S)

    /// Adaptively computes the product of all entries in the list.
    val inline product : list : alist<'T> -> aval<'S>
        when ('T or 'S) : (static member (*) : 'S -> 'T -> 'S) 
        and  ('T or 'S) : (static member (/) : 'S -> 'T -> 'S) 
        and   'S : (static member One : 'S)
