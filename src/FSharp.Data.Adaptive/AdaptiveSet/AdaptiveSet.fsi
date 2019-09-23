namespace FSharp.Data.Adaptive

open FSharp.Data.Traceable

/// an adaptive reader for aset that allows to pull operations and exposes its current state.
type IHashSetReader<'T> = 
    IOpReader<CountingHashSet<'T>, HashSetDelta<'T>>

/// adaptive set datastructure.
[<Interface>]
type AdaptiveHashSet<'T> =
    /// is the set constant?
    abstract member IsConstant : bool

    /// the current content of the set as aref.
    abstract member Content : aref<HashSet<'T>>
    
    /// gets a new reader to the set.
    abstract member GetReader : unit -> IHashSetReader<'T>

/// adaptive set datastructure.
and aset<'T> = AdaptiveHashSet<'T>

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

    /// adaptively maps over the given set.
    val map : mapping : ('A -> 'B) -> set : aset<'A> -> aset<'B>

    /// adaptively chooses all elements returned by mapping.  
    val choose : mapping : ('A -> option<'B>) -> set : aset<'A> -> aset<'B>
    
    /// adaptively filters the set using the given predicate.
    val filter : predicate : ('A -> bool) -> set : aset<'A> -> aset<'A>

    /// adaptively unions all the given sets
    val union : sets : aset<aset<'A>> -> aset<'A>

    /// adaptively maps over the given set and unions all resulting sets.
    val collect : mapping : ('A -> aset<'B>) -> set : aset<'A> -> aset<'B>

    /// creates an aset for the given aref.
    val ofARef : ref : aref<#seq<'A>> -> aset<'A>

    /// adaptively maps over the given ref and returns the resulting set.
    val bind : mapping : ('A -> aset<'B>) -> ref : aref<'A> -> aset<'B>

    /// adaptively flattens the set of adaptive refs.
    val flattenA : set : aset<aref<'A>> -> aset<'A>

    /// adaptively maps over the set and also respects inner changes.
    val mapA : mapping : ('A -> aref<'B>) -> set : aset<'A> -> aset<'B>

    /// adaptively maps over the set and also respects inner changes.
    val chooseA : mapping : ('A -> aref<option<'B>>) -> set : aset<'A> -> aset<'B>

    /// adaptively filters the set and also respects inner changes.
    val filterA : mapping : ('A -> aref<bool>) -> set : aset<'A> -> aset<'A>

    /// adaptively folds over the set using add for additions and trySubtract for removals.
    /// note the trySubtract may return None indicating that the result needs to be recomputed.
    /// also note that the order of elements given to add/trySubtract is undefined.
    val foldHalfGroup : add : ('S -> 'A -> 'S) -> trySubtract : ('S -> 'A -> option<'S>) -> zero : 'S -> set : aset<'A> -> aref<'S>
    
    /// adaptively folds over the set using add for additions and subtract for removals.
    /// note that the order of elements given to add/subtract is undefined.
    val foldGroup : add : ('S -> 'A -> 'S) -> subtract : ('S -> 'A -> 'S) -> zero : 'S -> set : aset<'A> -> aref<'S>

    /// adaptively folds over the set using add for additions and recomputes the value on every removal.
    /// note that the order of elements given to add is undefined.
    val fold : add : ('S -> 'A -> 'S) -> zero : 'S -> set : aset<'A> -> aref<'S>

    /// adaptively computes the sum all entries in the set.
    val inline sum<'A, 'B 
                    when ('A or 'B) : (static member (+) : 'B -> 'A -> 'B) 
                    and ('A or 'B) : (static member (-) : 'B -> 'A -> 'B) 
                    and 'B : (static member Zero : 'B)> : set : aset<'A> -> aref<'B>

    /// adaptively computes the product of all entries in the set.
    val inline product<'A, 'B 
                        when ('A or 'B) : (static member (*) : 'B -> 'A -> 'B) 
                        and ('A or 'B) : (static member (/) : 'B -> 'A -> 'B) 
                        and 'B : (static member One : 'B)> : set : aset<'A> -> aref<'B>