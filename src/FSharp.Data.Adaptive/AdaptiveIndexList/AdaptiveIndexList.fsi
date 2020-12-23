namespace FSharp.Data.Adaptive

open System
open FSharp.Data.Traceable
open FSharp.Data.Adaptive

/// An adaptive reader for alist that allows to pull operations and exposes its current state.
type IIndexListReader<'T> = 
    IOpReader<IndexList<'T>, IndexListDelta<'T>>

/// Adaptive list datastructure.
[<Interface>]
type IAdaptiveIndexList<'T> =
    /// Is the list constant?
    abstract member IsConstant : bool

    /// The current content of the list as aval.
    abstract member Content : aval<IndexList<'T>>
    
    /// Gets a new reader to the list.
    abstract member GetReader : unit -> IIndexListReader<'T>
    
    /// Gets the underlying History instance for the alist (if any)
    abstract member History : option<History<IndexList<'T>, IndexListDelta<'T>>>

/// Adaptive list datastructure.
type alist<'T> = IAdaptiveIndexList<'T>


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
    val ofReader : create : (unit -> #IOpReader<IndexListDelta<'T>>) -> alist<'T>

    /// Creates an alist using the given compute function
    val custom : compute : (AdaptiveToken -> IndexList<'T> -> IndexListDelta<'T>) -> alist<'T>

    /// Creates an alist using the given value generator
    val constant: value: (unit -> IndexList<'T>) -> alist<'T>
    
    /// Generate a list of adaptive length using the given intializer
    val init: length: aval<int> -> initializer: (int -> 'T) -> alist<'T>

    /// Generate an adaptive range of items based on lower/upper bound
    val inline range: lowerBound: aval< ^T > -> upperBound: aval< ^T > -> alist< ^T >
                                when ^T : (static member (+)   : ^T * ^T -> ^T) 
                                and ^T : (static member (-)   : ^T * ^T -> ^T) 
                                and ^T : (static member (~-)   : ^T -> ^T) 
                                and ^T : (static member One  : ^T)
                                and ^T : (static member Zero : ^T)
                                and ^T : equality
                                and ^T : comparison 

    /// Adaptively applies the given mapping function to all elements and returns a new alist containing the results.
    val mapi : mapping: (Index -> 'T1 -> 'T2) -> list: alist<'T1> -> alist<'T2>

    /// Adaptively applies the given mapping function to all elements and returns a new alist containing the results.
    val map : mapping: ('T1 -> 'T2) -> list: alist<'T1> -> alist<'T2>
    
    /// Adaptively chooses all elements returned by mapping.  
    val choosei : mapping: (Index -> 'T1 -> option<'T2>) -> list: alist<'T1> -> alist<'T2>

    /// Adaptively chooses all elements returned by mapping.  
    val choose : mapping: ('T1 -> option<'T2>) -> list: alist<'T1> -> alist<'T2>
        
    /// Adaptively filters the list using the given predicate.
    val filteri : predicate: (Index -> 'T -> bool) -> list: alist<'T> -> alist<'T>
    
    /// Adaptively filters the list using the given predicate.
    val filter : predicate: ('T -> bool) -> list: alist<'T> -> alist<'T>
        
    /// Adaptively applies the given mapping function to all elements and returns a new alist containing the results.
    val mapAi : mapping: (Index -> 'T1 -> aval<'T2>) -> list: alist<'T1> -> alist<'T2>

    /// Adaptively applies the given mapping function to all elements and returns a new alist containing the results.
    val mapA : mapping: ('T1 -> aval<'T2>) -> list: alist<'T1> -> alist<'T2>
    
    /// Adaptively chooses all elements returned by mapping.  
    val chooseAi : mapping: (Index -> 'T1 -> aval<option<'T2>>) -> list: alist<'T1> -> alist<'T2>

    /// Adaptively chooses all elements returned by mapping.  
    val chooseA : mapping: ('T1 -> aval<option<'T2>>) -> list: alist<'T1> -> alist<'T2>
        
    /// Adaptively filters the list using the given predicate.
    val filterAi : predicate: (Index -> 'T -> aval<bool>) -> list: alist<'T> -> alist<'T>
    
    /// Adaptively filters the list using the given predicate.
    val filterA : predicate: ('T -> aval<bool>) -> list: alist<'T> -> alist<'T>

    /// Adaptively applies the given mapping function to all elements and returns a new alist holding the concatenated results.
    val collecti : mapping: (Index -> 'T1 -> alist<'T2>) -> list: alist<'T1> -> alist<'T2>

    /// Adaptively applies the given mapping function to all elements and returns a new alist holding the concatenated results.
    val collect : mapping: ('T1 -> alist<'T2>) -> list: alist<'T1> -> alist<'T2>
    
    /// Adaptively applies the given mapping function to all elements and returns a new alist holding the concatenated results.
    val collect' : mapping: ('T1 -> seq<'T2>) -> list: alist<'T1> -> alist<'T2>

    /// Adaptively creates an alist with the source-indices.
    val indexed : list: alist<'T> -> alist<Index * 'T>

    /// Adaptively reverses the list
    val rev : list: alist<'T> -> alist<'T>

    /// Adaptively concatenates the given lists.
    val concat : lists: #seq<alist<'T>> -> alist<'T>
    
    /// Adaptively concatenates the given lists.
    val append : l: alist<'T> -> r: alist<'T> -> alist<'T>

    /// Creates an aval providing access to the current content of the list.
    val toAVal : list : alist<'T> -> aval<IndexList<'T>>
    
    /// Creates an alist from the given adaptive content
    val ofAVal : value : aval<#seq<'T>> -> alist<'T>

    /// Adaptively maps over the given aval and returns the resulting list.
    val bind : mapping: ('T1 -> alist<'T2>) -> value: aval<'T1> -> alist<'T2>
        
    /// Adaptively maps over the given avals and returns the resulting list.
    val bind2 : mapping : ('A -> 'B -> alist<'C>) -> valueA : aval<'A> -> valueB : aval<'B> -> alist<'C>

    /// Adaptively maps over the given avals and returns the resulting list.
    val bind3 : mapping : ('A -> 'B -> 'C -> alist<'D>) -> valueA : aval<'A> -> valueB : aval<'B> -> valueC : aval<'C> -> alist<'D>
    
    /// Adaptively maps over the given list and disposes all removed values while active.
    /// Additionally the returned Disposable disposes all currently existing values and clears the resulting list.
    val mapUsei : mapping : (Index -> 'A -> 'B) -> list : alist<'A> -> IDisposable * alist<'B> when 'B :> IDisposable
    
    /// Adaptively maps over the given list and disposes all removed values while active.
    /// Additionally the returned Disposable disposes all currently existing values and clears the resulting list.
    val mapUse : mapping : ('A -> 'B) -> list : alist<'A> -> IDisposable * alist<'B> when 'B :> IDisposable

    /// Sorts the list using the keys given by projection.
    /// Note that the sorting is stable.
    val sortByi : mapping: (Index -> 'T1 -> 'T2) -> list: alist<'T1> -> alist<'T1>
        when 'T2 : comparison
 
    /// Sorts the list using the keys given by projection in descending order.
    /// Note that the sorting is stable.
    val sortByDescendingi : mapping: (Index -> 'T1 -> 'T2) -> list: alist<'T1> -> alist<'T1>
        when 'T2 : comparison

    /// Sorts the list using the keys given by projection.
    /// Note that the sorting is stable.
    val sortBy : mapping: ('T1 -> 'T2) -> list: alist<'T1> -> alist<'T1>
        when 'T2 : comparison
        
    /// Sorts the list using the keys given by projection in descending order.
    /// Note that the sorting is stable.
    val sortByDescending : mapping: ('T1 -> 'T2) -> list: alist<'T1> -> alist<'T1>
        when 'T2 : comparison

    /// Sorts the list using the given compare function.
    /// Note that the sorting is stable.
    val sortWith : compare: ('T -> 'T -> int) -> list: alist<'T> -> alist<'T>

    /// Sorts the list.
    val inline sort : list: alist<'T> -> alist<'T>
        when 'T : comparison

    /// Sorts the list in descending order.
    val inline sortDescending : list: alist<'T> -> alist<'T>
        when 'T : comparison

    /// Returns a list containing all elements tupled with their successor.
    val pairwise : list: alist<'T> -> alist<'T * 'T>
    
    /// Returns a list of each element tupled with its successor and the last element tupled with the first.
    val pairwiseCyclic : list: alist<'T> -> alist<'T * 'T>

    /// Tries to get the element associated to a specific Index from the list.
    /// Note that this operation should not be used extensively since its resulting
    /// aval will be re-evaluated upon every change of the list.
    val tryGet : index: Index -> list: alist<'T> -> aval<option<'T>>
    
    /// Tries to get the element at a specific position from the list.
    /// Note that this operation should not be used extensively since its resulting
    /// aval will be re-evaluated upon every change of the list.
    val tryAt : index: int -> list: alist<'T> -> aval<option<'T>>

    /// Tries to get the first element from the list.
    val tryFirst : list: alist<'T> -> aval<option<'T>>
    
    /// Tries to get the last element from the list.
    val tryLast : list: alist<'T> -> aval<option<'T>>

    /// Adaptively tests if the list is empty.
    val isEmpty: alist<'T> -> aval<bool>

    /// Adaptively gets the number of elements in the list.
    val count: alist<'T> -> aval<int>

    /// Evaluates the given adaptive list and returns its current content.
    /// This should not be used inside the adaptive evaluation
    /// of other AdaptiveObjects since it does not track dependencies.
    val force: alist<'T> -> IndexList<'T>

    /// Reduces the list using the given `AdaptiveReduction` and returns
    /// the resulting adaptive value.
    val reduce : reduction: AdaptiveReduction<'T, 'State, 'Value> -> list: alist<'T> -> aval<'Value>

    /// Applies the mapping function to all elements of the list and reduces the results
    /// using the given `AdaptiveReduction`.
    /// Returns the resulting adaptive value.
    val reduceBy : reduction: AdaptiveReduction<'T2, 'State, 'Value> -> mapping: (Index -> 'T1 -> 'T2) -> list: alist<'T1> -> aval<'Value>
    
    /// Applies the mapping function to all elements of the list and reduces the results
    /// using the given `AdaptiveReduction`.
    /// Returns the resulting adaptive value.
    val reduceByA : reduction: AdaptiveReduction<'T2, 'State, 'Value> -> mapping: (Index -> 'T1 -> aval<'T2>) -> list: alist<'T1> -> aval<'Value>

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

    /// Adaptively checks whether the predicate holds for all entries.
    val forall: predicate: ('T -> bool) -> list: alist<'T> -> aval<bool> 
    
    /// Adaptively checks whether the predicate holds for at least one entry.
    val exists: predicate: ('T -> bool) -> list: alist<'T> -> aval<bool> 

    /// Adaptively tries to find the smallest element.
    val inline tryMin : list : alist<'T> -> aval<option<'T>>
        when 'T : comparison
        
    /// Adaptively tries to find the largest element.
    val inline tryMax : list : alist<'T> -> aval<option<'T>>
        when 'T : comparison

    /// Adaptively computes the sum of all entries in the list.
    val inline sum : list : alist<'T> -> aval<'S>
        when ('T or 'S) : (static member (+) : 'S -> 'T -> 'S) 
        and  ('T or 'S) : (static member (-) : 'S -> 'T -> 'S) 
        and   'S : (static member Zero : 'S)
        
    /// Adaptively computes the average of all entries in the list.
    val inline average: list : alist<'T> -> aval<'S>
        when ('T or 'S) : (static member (+) : 'S -> 'T -> 'S) 
        and  ('T or 'S) : (static member (-) : 'S -> 'T -> 'S) 
        and   'S : (static member Zero : 'S)
        and   'S : (static member DivideByInt : ^S * int -> ^S) 

    /// Adaptively computes the sum of all values returned by mapping for the list.
    val inline sumBy: mapping : ('T1 -> 'T2) -> list : alist<'T1> -> aval<'S>
        when ('T2 or 'S) : (static member (+) : 'S -> 'T2 -> 'S) 
        and  ('T2 or 'S) : (static member (-) : 'S -> 'T2 -> 'S) 
        and   'S : (static member Zero : 'S)
        
    /// Adaptively computes the average of all values returned by mapping for the list.
    val inline averageBy: mapping : ('T1 -> 'T2) -> list : alist<'T1> -> aval<'S>
        when ('T2 or 'S) : (static member (+) : 'S -> 'T2 -> 'S) 
        and  ('T2 or 'S) : (static member (-) : 'S -> 'T2 -> 'S) 
        and   'S : (static member Zero : 'S)
        and   'S : (static member DivideByInt : ^S * int -> ^S) 
        
    /// Adaptively checks whether the predicate holds for all entries.
    val forallA: predicate: ('T -> aval<bool>) -> list: alist<'T> -> aval<bool> 
    
    /// Adaptively checks whether the predicate holds for at least one entry.
    val existsA: predicate: ('T -> aval<bool>) -> list: alist<'T> -> aval<bool> 
    
    /// Adaptively counts all elements fulfilling the predicate
    val countBy : predicate: ('a -> bool) -> list: alist<'a> -> aval<int>

    /// Adaptively counts all elements fulfilling the predicate
    val countByA : predicate: ('a -> aval<bool>) -> list: alist<'a> -> aval<int>

    /// Adaptively computes the sum of all values returned by mapping for the list.
    val inline sumByA: mapping : ('T1 -> aval<'T2>) -> list : alist<'T1> -> aval<'S>
        when ('T2 or 'S) : (static member (+) : 'S -> 'T2 -> 'S) 
        and  ('T2 or 'S) : (static member (-) : 'S -> 'T2 -> 'S) 
        and   'S : (static member Zero : 'S)
        
    /// Adaptively computes the average of all values returned by mapping for the list.
    val inline averageByA: mapping : ('T1 -> aval<'T2>) -> list : alist<'T1> -> aval<'S>
        when ('T2 or 'S) : (static member (+) : 'S -> 'T2 -> 'S) 
        and  ('T2 or 'S) : (static member (-) : 'S -> 'T2 -> 'S) 
        and   'S : (static member Zero : 'S)
        and   'S : (static member DivideByInt : ^S * int -> ^S) 