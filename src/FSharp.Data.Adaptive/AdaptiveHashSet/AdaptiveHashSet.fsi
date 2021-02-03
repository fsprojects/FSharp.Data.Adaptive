namespace FSharp.Data.Adaptive

open System
open FSharp.Data.Traceable

/// An adaptive reader for aset that allows to pull operations and exposes its current state.
type IHashSetReader<'T> = 
    IOpReader<CountingHashSet<'T>, HashSetDelta<'T>>

/// Adaptive set datastructure.
[<Interface>]
type IAdaptiveHashSet<'T> =
    /// Is the set constant?
    abstract member IsConstant : bool

    /// The current content of the set as aval.
    abstract member Content : aval<HashSet<'T>>
    
    /// Gets a new reader to the set.
    abstract member GetReader : unit -> IHashSetReader<'T>

    /// Gets the underlying History instance for the aset (if any)
    abstract member History : option<History<CountingHashSet<'T>, HashSetDelta<'T>>>

/// Adaptive set datastructure.
and aset<'T> = IAdaptiveHashSet<'T>

/// Functional operators for aset<_>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ASet =
    /// The empty aset.
    [<GeneralizableValue>]
    val empty<'T> : aset<'T> 

    /// Creates an aset using the given set generator
    val constant: value: (unit -> HashSet<'T>) -> aset<'T>
    
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

    /// Generate an adaptive range of items based on lower/upper bound
    val inline range: lowerBound: aval< ^T > -> upperBound: aval< ^T > -> aset< ^T >
                                when ^T : (static member (+)   : ^T * ^T -> ^T) 
                                and ^T : (static member (-)   : ^T * ^T -> ^T) 
                                and ^T : (static member (~-)   : ^T -> ^T) 
                                and ^T : (static member One  : ^T)
                                and ^T : (static member Zero : ^T)
                                and ^T : equality
                                and ^T : comparison 

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
    
    /// Adaptively subtracts the given sets.
    val difference : a : aset<'T> -> b : aset<'T> -> aset<'T>
    
    /// Adaptively 'xors' the given sets.
    val xor : a : aset<'T> -> b : aset<'T> -> aset<'T>

    /// Adaptively intersects the given sets
    val intersect : a : aset<'T> -> b : aset<'T> -> aset<'T>

    /// Adaptively maps over the given set and unions all resulting sets.
    val collect : mapping : ('A -> aset<'B>) -> set : aset<'A> -> aset<'B>

    /// Adaptively maps over the given set and unions all resulting seqs.
    val collect' : mapping : ('A -> seq<'B>) -> set : aset<'A> -> aset<'B>

    /// Creates an aset for the given aval.
    val ofAVal : value : aval<#seq<'A>> -> aset<'A>

    /// Adaptively maps over the given aval and returns the resulting set.
    val bind : mapping : ('A -> aset<'B>) -> value : aval<'A> -> aset<'B>
    
    /// Adaptively maps over the given avals and returns the resulting set.
    val bind2 : mapping : ('A -> 'B -> aset<'C>) -> valueA : aval<'A> -> valueB : aval<'B> -> aset<'C>

    /// Adaptively maps over the given avals and returns the resulting set.
    val bind3 : mapping : ('A -> 'B -> 'C -> aset<'D>) -> valueA : aval<'A> -> valueB : aval<'B> -> valueC : aval<'C> -> aset<'D>
    
    /// Adaptively maps over the given set and disposes all removed values while active.
    /// Additionally the returned Disposable disposes all currently existing values and clears the resulting set.
    val mapUse : mapping : ('A -> 'B) -> set : aset<'A> -> IDisposable * aset<'B> when 'B :> IDisposable

    /// Adaptively flattens the set of adaptive refs.
    val flattenA : set : aset<aval<'A>> -> aset<'A>

    /// Adaptively maps over the set and also respects inner changes.
    val mapA : mapping : ('A -> aval<'B>) -> set : aset<'A> -> aset<'B>

    /// Adaptively maps over the set and also respects inner changes.
    val chooseA : mapping : ('A -> aval<option<'B>>) -> set : aset<'A> -> aset<'B>

    /// Adaptively filters the set and also respects inner changes.
    val filterA : predicate : ('A -> aval<bool>) -> set : aset<'A> -> aset<'A>

    /// Creates an aset using the given reader-creator.
    val ofReader : create : (unit -> #IOpReader<HashSetDelta<'T>>) -> aset<'T>

    /// Creates an aset using the given compute function
    val custom : compute : (AdaptiveToken -> CountingHashSet<'T> -> HashSetDelta<'T>) -> aset<'T>

    /// Creates a constant aset lazy content.
    val delay : create : (unit -> HashSet<'T>) -> aset<'T>

    /// Evaluates the given adaptive set and returns its current content.
    /// This should not be used inside the adaptive evaluation
    /// of other AdaptiveObjects since it does not track dependencies.
    val force: aset<'T> -> HashSet<'T>
    
    /// Adaptively tests if the set is empty.
    val isEmpty: aset<'T> -> aval<bool>

    /// Adaptively gets the number of elements in the set.
    val count: aset<'T> -> aval<int>

    /// Reduces the set using the given `AdaptiveReduction` and returns
    /// the resulting adaptive value.
    val reduce : reduction: AdaptiveReduction<'T, 'S, 'V> -> set: aset<'T> -> aval<'V>

    /// Applies the mapping function to all elements of the set and reduces the results
    /// using the given `AdaptiveReduction`.
    /// Returns the resulting adaptive value.
    val reduceBy : reduction: AdaptiveReduction<'T2, 'S, 'V> -> mapping: ('T1 -> 'T2) -> set: aset<'T1> -> aval<'V>
    
    /// Applies the mapping function to all elements of the set and reduces the results
    /// using the given `AdaptiveReduction`.
    /// Returns the resulting adaptive value.
    val reduceByA : reduction: AdaptiveReduction<'T2, 'S, 'V> -> mapping: ('T1 -> aval<'T2>) -> set: aset<'T1> -> aval<'V>

    /// Adaptively folds over the set using add for additions and trySubtract for removals.
    /// Note the trySubtract may return None indicating that the result needs to be recomputed.
    /// Also note that the order of elements given to add/trySubtract is undefined.
    val foldHalfGroup : add: ('S -> 'A -> 'S) -> trySubtract: ('S -> 'A -> option<'S>) -> zero: 'S -> set: aset<'A> -> aval<'S>
    
    /// Adaptively folds over the set using add for additions and subtract for removals.
    /// Note that the order of elements given to add/subtract is undefined.
    val foldGroup : add: ('S -> 'A -> 'S) -> subtract: ('S -> 'A -> 'S) -> zero : 'S -> set: aset<'A> -> aval<'S>

    /// Adaptively folds over the set using add for additions and recomputes the value on every removal.
    /// Note that the order of elements given to add is undefined.
    val fold : add : ('S -> 'A -> 'S) -> zero : 'S -> set : aset<'A> -> aval<'S>
    
    /// Adaptively checks whether the predicate holds for all entries.
    val forall: predicate: ('T -> bool) -> set: aset<'T> -> aval<bool> 
    
    /// Adaptively checks whether the predicate holds for at least one entry.
    val exists: predicate: ('T -> bool) -> set: aset<'T> -> aval<bool> 

    /// Adaptively checks whether the aset contains the given entry.
    val contains: value : 'T -> set: aset<'T> -> aval<bool>

    /// Adaptively tries to find the smallest element.
    val inline tryMin : set : aset<'T> -> aval<option<'T>>
        when 'T : comparison
        
    /// Adaptively tries to find the largest element.
    val inline tryMax : set : aset<'T> -> aval<option<'T>>
        when 'T : comparison

    /// Adaptively computes the sum of all entries in the set.
    val inline sum : set: aset<'T> -> aval<'S>
        when ('T or 'S) : (static member (+) : 'S -> 'T -> 'S) 
        and  ('T or 'S) : (static member (-) : 'S -> 'T -> 'S) 
        and   'S : (static member Zero : 'S)
        
    /// Adaptively computes the average of all entries in the set.
    val inline average: set: aset<'T> -> aval<'S>
        when ('T or 'S) : (static member (+) : 'S -> 'T -> 'S) 
        and  ('T or 'S) : (static member (-) : 'S -> 'T -> 'S) 
        and   'S : (static member Zero : 'S)
        and   'S : (static member DivideByInt : ^S * int -> ^S) 

    /// Adaptively computes the sum of all values returned by mapping for the set.
    val inline sumBy: mapping: ('T1 -> 'T2) -> set: aset<'T1> -> aval<'S>
        when ('T2 or 'S) : (static member (+) : 'S -> 'T2 -> 'S) 
        and  ('T2 or 'S) : (static member (-) : 'S -> 'T2 -> 'S) 
        and   'S : (static member Zero : 'S)
        
    /// Adaptively computes the average of all values returned by mapping for the set.
    val inline averageBy: mapping: ('T1 -> 'T2) -> set: aset<'T1> -> aval<'S>
        when ('T2 or 'S) : (static member (+) : 'S -> 'T2 -> 'S) 
        and  ('T2 or 'S) : (static member (-) : 'S -> 'T2 -> 'S) 
        and   'S : (static member Zero : 'S)
        and   'S : (static member DivideByInt : ^S * int -> ^S) 
        
    /// Adaptively checks whether the predicate holds for all entries.
    val forallA: predicate: ('T -> aval<bool>) -> set: aset<'T> -> aval<bool> 
    
    /// Adaptively checks whether the predicate holds for at least one entry.
    val existsA: predicate: ('T -> aval<bool>) -> set: aset<'T> -> aval<bool> 
    
    /// Adaptively counts all elements fulfilling the predicate
    val countBy : predicate: ('a -> bool) -> set: aset<'a> -> aval<int>

    /// Adaptively counts all elements fulfilling the predicate
    val countByA : predicate: ('a -> aval<bool>) -> set: aset<'a> -> aval<int>

    /// Adaptively computes the sum of all values returned by mapping for the set.
    val inline sumByA: mapping : ('T1 -> aval<'T2>) -> set : aset<'T1> -> aval<'S>
        when ('T2 or 'S) : (static member (+) : 'S -> 'T2 -> 'S) 
        and  ('T2 or 'S) : (static member (-) : 'S -> 'T2 -> 'S) 
        and   'S : (static member Zero : 'S)
        
    /// Adaptively computes the average of all values returned by mapping for the set.
    val inline averageByA: mapping : ('T1 -> aval<'T2>) -> set : aset<'T1> -> aval<'S>
        when ('T2 or 'S) : (static member (+) : 'S -> 'T2 -> 'S) 
        and  ('T2 or 'S) : (static member (-) : 'S -> 'T2 -> 'S) 
        and   'S : (static member Zero : 'S)
        and   'S : (static member DivideByInt : ^S * int -> ^S) 