namespace FSharp.Data.Adaptive

open System
open FSharp.Data.Traceable

/// An adaptive reader for aset that provides incremental access to changes.
/// Readers track deltas (additions and removals) instead of full set snapshots,
/// enabling efficient incremental processing of set changes.
type IHashSetReader<'T> =
    IOpReader<CountingHashSet<'T>, HashSetDelta<'T>>

/// Adaptive set datastructure providing efficient incremental updates.
///
/// An aset is an unordered collection of unique elements that:
/// - Maintains uniqueness (no duplicates)
/// - Has no defined order (unlike alist)
/// - Tracks changes as deltas (additions and removals)
/// - Supports incremental operations (map, filter, union, etc.)
/// - Recomputes only affected elements when inputs change
///
/// Use aset when:
/// - You need a collection of unique values
/// - Order doesn't matter
/// - You need efficient set operations (union, intersection, etc.)
/// - You're working with relations or graph structures
[<Interface>]
type IAdaptiveHashSet<'T> =
    /// Returns true if this set is constant (never changes).
    /// Constant sets have optimized implementations with minimal overhead.
    abstract member IsConstant : bool

    /// The current content of the set as an adaptive value.
    /// This provides access to the full set state, but using readers is more efficient
    /// for incremental consumption of changes.
    abstract member Content : aval<HashSet<'T>>

    /// Creates a new reader for incremental access to set changes.
    /// Multiple readers can exist for the same set, each tracking changes independently.
    /// Readers are the recommended way to consume aset changes efficiently.
    abstract member GetReader : unit -> IHashSetReader<'T>

    /// Gets the underlying History instance for the aset (if any).
    /// History enables time-travel debugging and undo/redo functionality.
    /// Most user code doesn't need to access this directly.
    abstract member History : option<History<CountingHashSet<'T>, HashSetDelta<'T>>>

/// Type abbreviation for IAdaptiveHashSet.
/// This is the primary unordered collection type in FSharp.Data.Adaptive.
and aset<'T> = IAdaptiveHashSet<'T>

/// Functional operators for aset<_>.
/// All operations are incremental and recompute only affected elements when inputs change.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ASet =
    /// The empty aset. Constant with no overhead.
    /// Time complexity: O(1)
    [<GeneralizableValue>]
    val empty<'T> : aset<'T>

    /// Creates a constant aset using the given set generator.
    /// The generator is called once lazily.
    val constant: value: (unit -> HashSet<'T>) -> aset<'T>

    /// A constant aset holding a single value.
    /// Time complexity: O(1)
    ///
    /// Example:
    ///     let item = ASet.single 42
    val single : value : 'T -> aset<'T>

    /// Creates a constant aset holding the given values.
    /// Duplicates are automatically removed.
    /// Time complexity: O(n)
    val ofSeq : elements : seq<'T> -> aset<'T>

    /// Creates a constant aset holding the given values.
    /// Duplicates are automatically removed.
    /// Time complexity: O(n)
    val ofList : elements : list<'T> -> aset<'T>

    /// Creates a constant aset holding the given values.
    /// Duplicates are automatically removed.
    /// Time complexity: O(n)
    val ofArray : elements : 'T[] -> aset<'T>

    /// Creates a constant aset holding the given values.
    /// This is the most efficient way to create an aset from HashSet.
    /// Time complexity: O(1)
    val ofHashSet : elements : HashSet<'T> -> aset<'T>

    /// Creates an aval providing access to the current content of the set.
    /// This is the same as accessing the Content property.
    /// Time complexity: O(1)
    val toAVal : set : aset<'T> -> aval<HashSet<'T>>

    /// Generates an adaptive range of items based on lower and upper bounds.
    /// The range updates incrementally when bounds change.
    ///
    /// Time complexity: O(k) where k = change in range size
    ///
    /// Example:
    ///     let lower = cval 0
    ///     let upper = cval 10
    ///     let range = ASet.range lower upper  // {0..10}
    val inline range: lowerBound: aval< ^T > -> upperBound: aval< ^T > -> aset< ^T >
                                when ^T : (static member (+)   : ^T * ^T -> ^T)
                                and ^T : (static member (-)   : ^T * ^T -> ^T)
                                and ^T : (static member (~-)   : ^T -> ^T)
                                and ^T : (static member One  : ^T)
                                and ^T : (static member Zero : ^T)
                                and ^T : equality
                                and ^T : comparison

    /// Adaptively applies the mapping function to all elements.
    /// Only changed elements are remapped on updates.
    /// Note: Mapping may produce duplicates which are automatically merged.
    ///
    /// Time complexity: O(k) where k = number of changed elements
    ///
    /// Example:
    ///     let numbers = cset [1; 2; 3]
    ///     let squares = numbers |> ASet.map (fun x -> x * x)
    ///     // => {1; 4; 9}
    val map : mapping : ('A -> 'B) -> set : aset<'A> -> aset<'B>

    /// Adaptively chooses elements based on the mapping function.
    /// Returns only Some values. Only changed elements are reprocessed.
    ///
    /// Time complexity: O(k) where k = number of changed elements
    ///
    /// Example:
    ///     let items = cset [1; 2; 3; 4]
    ///     let evens = items |> ASet.choose (fun x ->
    ///         if x % 2 = 0 then Some x else None)
    ///     // => {2; 4}
    val choose : mapping : ('A -> option<'B>) -> set : aset<'A> -> aset<'B>

    /// Adaptively filters the set using the given predicate.
    /// Only changed elements are re-evaluated.
    ///
    /// Time complexity: O(k) where k = number of changed elements
    ///
    /// Example:
    ///     let numbers = cset [1; 2; 3; 4; 5]
    ///     let evens = numbers |> ASet.filter (fun x -> x % 2 = 0)
    ///     // => {2; 4}
    val filter : predicate : ('A -> bool) -> set : aset<'A> -> aset<'A>

    /// Adaptively computes the union of two sets.
    /// Only processes changes from either input set.
    ///
    /// Time complexity: O(k) where k = number of changes
    ///
    /// Example:
    ///     let a = cset [1; 2; 3]
    ///     let b = cset [3; 4; 5]
    ///     let combined = ASet.union a b  // => {1; 2; 3; 4; 5}
    val union : a : aset<'A> -> b : aset<'A> -> aset<'A>

    /// Adaptively computes the union of all sets in the input set of sets.
    /// Efficiently handles adding/removing entire sets.
    ///
    /// Time complexity: O(k) where k = number of changes across all sets
    val unionMany : sets : aset<aset<'A>> -> aset<'A>

    /// Adaptively computes the set difference (a \ b).
    /// Returns elements in 'a' that are not in 'b'.
    ///
    /// Time complexity: O(k) where k = number of changes
    ///
    /// Example:
    ///     let a = cset [1; 2; 3; 4]
    ///     let b = cset [3; 4; 5]
    ///     let diff = ASet.difference a b  // => {1; 2}
    val difference : a : aset<'T> -> b : aset<'T> -> aset<'T>

    /// Adaptively computes the symmetric difference (XOR) of two sets.
    /// Returns elements in either set but not in both.
    ///
    /// Time complexity: O(k) where k = number of changes
    ///
    /// Example:
    ///     let a = cset [1; 2; 3]
    ///     let b = cset [2; 3; 4]
    ///     let xor = ASet.xor a b  // => {1; 4}
    val xor : a : aset<'T> -> b : aset<'T> -> aset<'T>

    /// Adaptively computes the intersection of two sets.
    /// Returns elements that are in both sets.
    ///
    /// Time complexity: O(k) where k = number of changes
    ///
    /// Example:
    ///     let a = cset [1; 2; 3]
    ///     let b = cset [2; 3; 4]
    ///     let common = ASet.intersect a b  // => {2; 3}
    val intersect : a : aset<'T> -> b : aset<'T> -> aset<'T>

    /// Adaptively maps over the set and unions all resulting sets (flatMap).
    /// Only changed elements are remapped on updates.
    ///
    /// Time complexity: O(k * m) where k = changed elements, m = avg result set size
    ///
    /// Example:
    ///     let ranges = cset [1..3]
    ///     let expanded = ranges |> ASet.collect (fun n ->
    ///         ASet.ofList [n; n*10])
    ///     // => {1; 10; 2; 20; 3; 30}
    val collect : mapping : ('A -> aset<'B>) -> set : aset<'A> -> aset<'B>

    /// Adaptively maps over the set and unions all resulting sequences.
    /// Only changed elements are remapped on updates.
    ///
    /// Time complexity: O(k * m) where k = changed elements, m = avg sequence length
    val collect' : mapping : ('A -> seq<'B>) -> set : aset<'A> -> aset<'B>

    /// Creates an aset from an adaptive value containing a sequence.
    /// When the aval changes, the entire set is recomputed.
    ///
    /// Example:
    ///     let items = cval [1; 2; 3]
    ///     let set = ASet.ofAVal items
    val ofAVal : value : aval<#seq<'A>> -> aset<'A>

    /// Adaptively maps over an aval and returns the resulting set.
    /// When the value changes, the mapping is recomputed.
    ///
    /// Example:
    ///     let mode = cval "evens"
    ///     let numbers = mode |> ASet.bind (fun m ->
    ///         if m = "evens" then cset [2; 4; 6]
    ///         else cset [1; 3; 5])
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