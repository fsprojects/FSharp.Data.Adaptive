namespace FSharp.Data.Adaptive

open FSharp.Data.Traceable

/// An adaptive reader for aset that allows to pull operations and exposes its current state.
type IHashSetReader<'T> = 
    IOpReader<CountingHashSet<'T>, HashSetDelta<'T>>

/// Adaptive set datastructure.
[<Interface>]
type AdaptiveHashSet<'T> =
    /// Is the set constant?
    abstract member IsConstant : bool

    /// The current content of the set as aval.
    abstract member Content : aval<HashSet<'T>>
    
    /// Gets a new reader to the set.
    abstract member GetReader : unit -> IHashSetReader<'T>

/// Adaptive set datastructure.
and aset<'T> = AdaptiveHashSet<'T>

/// Functional operators for aset<_>
module ASet =
    /// The empty aset.
    [<GeneralizableValue>]
    val empty<'T> : aset<'T> 

    /// A constant aset holding a single value.
    val single : value : 'T -> aset<'T>
    
    /// Creates an aset holding the given values.
    val ofSeq : elements : seq<'T> -> aset<'T>
    
    /// Creates an aset holding the given values.
    val ofList : elements : list<'T> -> aset<'T>
    
    /// Creates an aset holding the given values.
    val ofArray : elements : 'T[] -> aset<'T>

    /// Creates an aset holding the given values. `O(1)`
    val ofHashSet : elements : HashSet<'T> -> aset<'T>

    /// Creates an aval providing access to the current content of the set.
    val toAVal : set : aset<'T> -> aval<HashSet<'T>>

    /// Adaptively maps over the given set.
    val map : mapping : ('A -> 'B) -> set : aset<'A> -> aset<'B>

    /// Adaptively chooses all elements returned by mapping.  
    val choose : mapping : ('A -> option<'B>) -> set : aset<'A> -> aset<'B>
    
    /// Adaptively filters the set using the given predicate.
    val filter : predicate : ('A -> bool) -> set : aset<'A> -> aset<'A>
    
    /// Adaptively unions the given sets
    val union : a : aset<'A> -> b : aset<'A> -> aset<'A>

    /// Adaptively unions all the given sets
    val unionMany : sets : aset<aset<'A>> -> aset<'A>

    /// Adaptively maps over the given set and unions all resulting sets.
    val collect : mapping : ('A -> aset<'B>) -> set : aset<'A> -> aset<'B>

    /// Creates an aset for the given aval.
    val ofAVal : value : aval<#seq<'A>> -> aset<'A>

    /// Adaptively maps over the given aval and returns the resulting set.
    val bind : mapping : ('A -> aset<'B>) -> value : aval<'A> -> aset<'B>

    /// Adaptively flattens the set of adaptive refs.
    val flattenA : set : aset<aval<'A>> -> aset<'A>

    /// Adaptively maps over the set and also respects inner changes.
    val mapA : mapping : ('A -> aval<'B>) -> set : aset<'A> -> aset<'B>

    /// Adaptively maps over the set and also respects inner changes.
    val chooseA : mapping : ('A -> aval<option<'B>>) -> set : aset<'A> -> aset<'B>

    /// Adaptively filters the set and also respects inner changes.
    val filterA : mapping : ('A -> aval<bool>) -> set : aset<'A> -> aset<'A>

    /// Adaptively folds over the set using add for additions and trySubtract for removals.
    /// Note the trySubtract may return None indicating that the result needs to be recomputed.
    /// Also note that the order of elements given to add/trySubtract is undefined.
    val foldHalfGroup : add : ('S -> 'A -> 'S) -> trySubtract : ('S -> 'A -> option<'S>) -> zero : 'S -> set : aset<'A> -> aval<'S>
    
    /// Adaptively folds over the set using add for additions and subtract for removals.
    /// Note that the order of elements given to add/subtract is undefined.
    val foldGroup : add : ('S -> 'A -> 'S) -> subtract : ('S -> 'A -> 'S) -> zero : 'S -> set : aset<'A> -> aval<'S>

    /// Adaptively folds over the set using add for additions and recomputes the value on every removal.
    /// Note that the order of elements given to add is undefined.
    val fold : add : ('S -> 'A -> 'S) -> zero : 'S -> set : aset<'A> -> aval<'S>

    /// Creates an aset using the given reader-creator.
    val ofReader : create : (unit -> #IOpReader<HashSetDelta<'T>>) -> aset<'T>
    
    /// Creates a constant aset lazy content.
    val delay : create : (unit -> HashSet<'T>) -> aset<'T>

    /// Adaptively computes the sum all entries in the set.
    val inline sum<'A, 'B 
                    when ('A or 'B) : (static member (+) : 'B -> 'A -> 'B) 
                    and ('A or 'B) : (static member (-) : 'B -> 'A -> 'B) 
                    and 'B : (static member Zero : 'B)> : set : aset<'A> -> aval<'B>

    /// Adaptively computes the product of all entries in the set.
    val inline product<'A, 'B 
                        when ('A or 'B) : (static member (*) : 'B -> 'A -> 'B) 
                        and ('A or 'B) : (static member (/) : 'B -> 'A -> 'B) 
                        and 'B : (static member One : 'B)> : set : aset<'A> -> aval<'B>