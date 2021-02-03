namespace FSharp.Data.Adaptive

/// Changeable adaptive set that allows mutation by user-code and implements aset.
[<Sealed>]
type ChangeableHashSet<'T> =
    interface IAdaptiveHashSet<'T>
    interface System.Collections.Generic.ICollection<'T>
    #if !FABLE_COMPILER
    interface System.Collections.Generic.ISet<'T>
    #endif

    /// The number of entries currently in the set.
    member Count: int

    /// Is the set currently empty?
    member IsEmpty: bool

    /// Checks whether the given value is contained.
    member Contains: value: 'T -> bool

    /// Gets or sets the current state as HashSet.
    member Value: HashSet<'T> with get, set
    
    /// Sets the current state as HashSet.
    member UpdateTo: HashSet<'T> -> bool

    /// Performs the given Operations on the Set.
    member Perform: operations : HashSetDelta<'T> -> unit

    /// Adds a value and returns whether the element was new.
    member Add: value: 'T -> bool

    /// Removes a value and returns whether the element was deleted.
    member Remove: value: 'T -> bool

    /// Clears the set.
    member Clear: unit -> unit

    /// Adds all the given values to the set.
    member UnionWith: other: seq<'T> -> unit

    /// Removes all the given elements from the set.
    member ExceptWith: other: seq<'T> -> unit

    /// Removes all elements from the set that are not also contained in other.
    member IntersectWith: other: seq<'T> -> unit

    /// Creates an adaptive reader for the set.
    member GetReader: unit -> IHashSetReader<'T>

    member GetEnumerator : unit -> HashSetEnumerator<'T>

    /// Creates a new empty cset.
    new: unit -> cset<'T>

    /// Creates a new cset containing all the given elements.
    new: seq<'T> -> cset<'T>

    /// Creates a new cset containing all the given elements.
    new: HashSet<'T> -> cset<'T>

/// An abbreviation for ChangeableHashSet
and cset<'T> = ChangeableHashSet<'T>
