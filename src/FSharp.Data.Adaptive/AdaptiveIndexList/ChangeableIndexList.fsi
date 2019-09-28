namespace FSharp.Data.Adaptive

open FSharp.Data.Traceable

/// Changeable adaptive list that allows mutation by user-code and implements alist.
[<Sealed>]
type ChangeableIndexList<'T> =
    interface AdaptiveIndexList<'T>

    /// is the list currently empty?
    member IsEmpty : bool

    /// the number of elements currently in the list.
    member Count : int

    /// Gets or sets the value for the list.
    member Value : IndexList<'T> with get, set

    /// Gets or sets an element in the list at the given index.
    member Item : index: int -> 'T with get, set

    /// Appends an element to the list and returns its Index.
    member Add : element: 'T -> Index
    
    /// Appends an element to the list and returns its Index.
    member Append : element: 'T -> Index

    /// Prepends an element to the list and returns its Index.
    member Prepend : element: 'T -> Index

    /// Inserts an element at the given position in the list and returns its Index.
    /// Note that the position can be equal to the count of the list.
    member InsertAt : index: int * element: 'T -> Index

    /// Removes the element at the given position and returns its Index.
    member RemoveAt : index: int -> Index

    /// Clears the list.
    member Clear : unit -> unit

    /// Tries to get the Index associated to the given position.
    member TryGetIndex : index: int -> option<Index>

    /// Tries to get the element at the given position.
    member TryAt : index: int -> option<'T>
    
    /// The smallest index contained in the list (or Index.zero if empty)
    member MinIndex : Index

    /// The largest index contained in the list (or Index.zero if empty)
    member MaxIndex : Index

    /// Gets or sets the element associated to index.
    member Item : index: Index -> 'T with get, set

    /// Removes the given index from the list and returns true if the element was deleted.
    member Remove : index: Index -> bool

    /// Inserts an element directly after the given index and returns the new index for the element.
    member InsertAfter : index: Index * element: 'T -> Index

    /// Inserts an element directly before the given index and returns the new index for the element.
    member InsertBefore : index: Index * element: 'T -> Index

    /// Gets the (optional) element associated to the given Index.
    member TryGet : index: Index -> option<'T>

    /// Creates a new list initially holding the given elements.
    new : elements: IndexList<'T> -> ChangeableIndexList<'T>

    /// Creates a new list initially holding the given elements.
    new : elements: seq<'T> -> ChangeableIndexList<'T>

    /// Creates a new empty list.
    new : unit -> ChangeableIndexList<'T>

/// Changeable adaptive list that allows mutation by user-code and implements alist.
type clist<'T> = ChangeableIndexList<'T>