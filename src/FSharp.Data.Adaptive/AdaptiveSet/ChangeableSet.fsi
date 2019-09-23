namespace FSharp.Data.Adaptive

/// changeable adaptive set that allows mutation by user-code and implements aset.
[<Sealed>]
type ChangeableHashSet<'T> =
    interface AdaptiveHashSet<'T>

    /// the number of entries currently in the set.
    member Count: int

    /// is the set currently empty?
    member IsEmpty: bool

    /// checks whether the given value is contained.
    member Contains: value: 'T -> bool

    /// gets or sets the current state as HashSet.
    member Value: HashSet<'T> with get, set

    /// adds a value and returns whether the element was new.
    member Add: value: 'T -> bool

    /// removes a value and returns whether the element was deleted.
    member Remove: value: 'T -> bool

    /// clears the set.
    member Clear: unit -> unit

    /// adds all the given values to the set.
    member UnionWith: other: seq<'T> -> unit

    /// removes all the given elements from the set.
    member ExceptWith: other: seq<'T> -> unit

    /// creates an adaptive reader for the set.
    member GetReader: unit -> IHashSetReader<'T>

    /// creates a new empty cset.
    new: unit -> cset<'T>

    /// creates a new cset containing all the given elements.
    new: seq<'T> -> cset<'T>

    /// creates a new cset containing all the given elements.
    new: HashSet<'T> -> cset<'T>

/// An abbreviation for ChangeableHashSet
and cset<'T> = ChangeableHashSet<'T>
