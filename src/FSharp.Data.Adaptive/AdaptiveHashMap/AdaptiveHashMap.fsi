namespace FSharp.Data.Adaptive

open System
open FSharp.Data.Traceable

/// An adaptive reader for amap that allows to pull operations and exposes its current state.
type IHashMapReader<'Key,'Value> = IOpReader<HashMap<'Key,'Value>,HashMapDelta<'Key,'Value>>

/// Adaptive map datastructure.
[<Interface>]
type IAdaptiveHashMap<'Key,'Value> =
    /// is the map constant?
    abstract member IsConstant : bool

    /// the current content of the map as aval.
    abstract member Content : aval<HashMap<'Key,'Value>>

    /// gets a new reader to the map.
    abstract member GetReader : unit -> IHashMapReader<'Key,'Value>
    
    /// Gets the underlying History instance for the amap (if any)
    abstract member History : option<History<HashMap<'Key, 'Value>, HashMapDelta<'Key, 'Value>>>

/// Adaptive map datastructure.
type amap<'Key,'Value> = IAdaptiveHashMap<'Key,'Value>

/// Functional operators for amap<_,_>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AMap =

    /// The empty map.
    val empty<'Key,'Value> : amap<'Key,'Value>

    /// A constant amap holding a single key/value.
    val single : key:'Key -> value:'Value -> amap<'Key,'Value>

    /// Creates an amap holding the given entries.
    val ofSeq : elements:seq<'Key * 'Value> -> amap<'Key,'Value>

    /// Creates an amap holding the given entries.
    val ofList : elements:('Key * 'Value) list -> amap<'Key,'Value>

    /// Creates an amap holding the given entries.
    val ofArray : elements:('Key * 'Value) array -> amap<'Key,'Value>
    
    /// Creates an amap holding the given entries.
    val ofHashMap : elements:HashMap<'Key, 'Value> -> amap<'Key, 'Value>

    /// Creates an amap for the given aval.
    val ofAVal : value:aval<#seq<'Key * 'Value>> -> amap<'Key,'Value>
 
    /// Creates an amap from the given set while keeping all duplicate values for a key in a HashSet.
    val ofASet : elements:aset<'Key * 'Value> -> amap<'Key, HashSet<'Value>>
    
    /// Creates an amap from the given set and takes an arbitrary value for duplicate entries.
    val ofASetIgnoreDuplicates : elements:aset<'Key * 'Value> -> amap<'Key, 'Value>
    
    /// Creates an amap using the given reader-creator.
    val ofReader : creator: (unit -> #IOpReader<HashMapDelta<'Key, 'Value>>) -> amap<'Key, 'Value>
    
    /// Creates an amap using the given compute function
    val custom : compute : (AdaptiveToken -> HashMap<'Key, 'Value> -> HashMapDelta<'Key, 'Value>) -> amap<'Key, 'Value>

    /// Creates an aval providing access to the current content of the map.
    val toAVal : map:amap<'Key, 'Value> -> aval<HashMap<'Key,'Value>>

    /// Adaptively maps over the given map.
    val map : mapping:('Key -> 'Value1 -> 'Value2) -> map:amap<'Key,'Value1> -> amap<'Key,'Value2>

    /// Creates an amap with the keys from the set and the values given by mapping.
    val mapSet : mapping:('Key -> 'Value) -> set:aset<'Key> -> amap<'Key,'Value>

    /// Adaptively maps over the given map without exposing keys.
    val map' : mapping:('Value1 -> 'Value2) -> map:amap<'Key,'Value1> -> amap<'Key,'Value2>

    /// Adaptively chooses all elements returned by mapping.  
    val choose : mapping:('Key -> 'Value1 -> 'Value2 option) -> map:amap<'Key,'Value1> -> amap<'Key,'Value2>
    
    /// Adaptively chooses all elements returned by mapping without exposing keys.  
    val choose' : mapping:('Value1 -> 'Value2 option) -> map:amap<'Key,'Value1> -> amap<'Key,'Value2>

    /// Adaptively filters the set using the given predicate.
    val filter : predicate:('Key -> 'Value -> bool) -> map:amap<'Key,'Value> -> amap<'Key,'Value>

    /// Adaptively filters the set using the given predicate without exposing keys.
    val filter' : predicate:('Value -> bool) -> map:amap<'Key,'Value> -> amap<'Key,'Value>

    /// Adaptively applies the given mapping function to all elements and returns a new amap containing the results.
    val mapA : mapping: ('K -> 'V -> aval<'T>) -> map: amap<'K, 'V> -> amap<'K, 'T>

    /// Adaptively chooses all elements returned by mapping.  
    val chooseA : mapping: ('K -> 'V -> aval<option<'T>>) -> map: amap<'K, 'V> -> amap<'K, 'T>
    
    /// Adaptively filters the list using the given predicate.
    val filterA : predicate: ('K -> 'V -> aval<bool>) -> map: amap<'K, 'V> -> amap<'K, 'V>

    /// Adaptively unions both maps using the given resolve functions when colliding entries are found.
    val unionWith : resolve:('Key -> 'Value -> 'Value -> 'Value) -> a:amap<'Key,'Value> -> b:amap<'Key,'Value> -> amap<'Key,'Value>
    
    /// Adaptively unions both maps preferring the right value when colliding entries are found.
    val union : a:amap<'Key,'Value> -> b:amap<'Key,'Value> -> amap<'Key,'Value>

    /// Adaptively maps over the given aval and returns the resulting map.
    val bind : mapping:('T -> amap<'Key,'Value>) -> value:aval<'T> -> amap<'Key,'Value>
        
    /// Adaptively maps over the given avals and returns the resulting map.
    val bind2 : mapping : ('A -> 'B -> amap<'Key,'Value>) -> valueA : aval<'A> -> valueB : aval<'B> -> amap<'Key,'Value>

    /// Adaptively maps over the given avals and returns the resulting map.
    val bind3 : mapping : ('A -> 'B -> 'C -> amap<'Key,'Value>) -> valueA : aval<'A> -> valueB : aval<'B> -> valueC : aval<'C> -> amap<'Key,'Value>

    /// Adaptively maps over the given map and disposes all removed values while active.
    /// Additionally the returned Disposable disposes all currently existing values and clears the resulting map.
    val mapUse : mapping : ('K -> 'A -> 'B) -> map : amap<'K, 'A> -> IDisposable * amap<'K, 'B> when 'B :> IDisposable

    /// Creates an aset holding all key/value tuples from the map.
    val toASet : map:amap<'Key,'Value> -> aset<'Key * 'Value>
    
    /// Creates an aset holding all distinct values from the map.
    val toASetValues : map:amap<'Key,'Value> -> aset<'Value>
    

    /// Adaptively looks up the given key in the map.
    /// Note that this operation should not be used extensively since its resulting
    /// aval will be re-evaluated upon every change of the map.
    val tryFind : key:'K -> map:amap<'K, 'V> -> aval<option<'V>>

    /// Adaptively looks up the given key in the map.
    /// Note that this operation should not be used extensively since its resulting
    /// aval will be re-evaluated upon every change of the map.
    /// WARNING: causes KeyNotFoundException when the key is not present at evaluation-time
    val find : key:'K -> map:amap<'K, 'V> -> aval<'V>

    /// Evaluates the given adaptive map and returns its current content.
    /// This should not be used inside the adaptive evaluation
    /// of other AdaptiveObjects since it does not track dependencies.
    val force: amap<'K, 'V> -> HashMap<'K, 'V>
    
    /// Adaptively tests if the map is empty.
    val isEmpty: amap<'K, 'V> -> aval<bool>

    /// Adaptively gets the number of elements in the map.
    val count: amap<'K, 'V> -> aval<int>

    /// Reduces the map using the given `AdaptiveReduction` and returns
    /// the resulting adaptive value.
    val reduce : reduction: AdaptiveReduction<'V, 'State, 'Value> -> map: amap<'K, 'V> -> aval<'Value>

    /// Applies the mapping function to all elements of the map and reduces the results
    /// using the given `AdaptiveReduction`.
    /// Returns the resulting adaptive value.
    val reduceBy : reduction: AdaptiveReduction<'T2, 'State, 'Value> -> mapping: ('K -> 'T1 -> 'T2) -> map: amap<'K, 'T1> -> aval<'Value>
    
    /// Applies the mapping function to all elements of the map and reduces the results
    /// using the given `AdaptiveReduction`.
    /// Returns the resulting adaptive value.
    val reduceByA : reduction: AdaptiveReduction<'T2, 'State, 'Value> -> mapping: ('K -> 'T1 -> aval<'T2>) -> map: amap<'K, 'T1> -> aval<'Value>


    /// Adaptively folds over the map using add for additions and trySubtract for removals.
    /// Note the trySubtract may return None indicating that the result needs to be recomputed.
    /// Also note that the order of elements given to add/trySubtract is undefined.
    val foldHalfGroup : add : ('S -> 'K -> 'V -> 'S) -> trySubtract : ('S -> 'K -> 'V -> option<'S>) -> zero : 'S -> map: amap<'K, 'V> -> aval<'S>
    
    /// Adaptively folds over the map using add for additions and subtract for removals.
    /// Note that the order of elements given to add/subtract is undefined.
    val foldGroup : add : ('S -> 'K -> 'V -> 'S) -> subtract : ('S -> 'K -> 'V -> 'S) -> zero : 'S -> map: amap<'K, 'V> -> aval<'S>

    /// Adaptively folds over the map using add for additions and recomputes the value on every removal.
    /// Note that the order of elements given to add is undefined.
    val fold : add : ('S -> 'K -> 'V -> 'S) -> zero : 'S -> map: amap<'K, 'V> -> aval<'S>
    
    /// Adaptively checks whether the predicate holds for all entries.
    val forall: predicate: ('K -> 'V -> bool) -> map: amap<'K, 'V> -> aval<bool> 
    
    /// Adaptively checks whether the predicate holds for at least one entry.
    val exists: predicate: ('K -> 'V -> bool) -> map: amap<'K, 'V> -> aval<bool> 

    /// Adaptively computes the sum of all values returned by mapping for the map.
    val inline sumBy: mapping : ('K -> 'V -> 'T) -> map : amap<'K, 'V> -> aval<'S>
        when ('T or 'S) : (static member (+) : 'S -> 'T -> 'S) 
        and  ('T or 'S) : (static member (-) : 'S -> 'T -> 'S) 
        and   'S : (static member Zero : 'S)
        
    /// Adaptively computes the average of all values returned by mapping for the map.
    val inline averageBy: mapping : ('K -> 'V -> 'T) -> map : amap<'K, 'V> -> aval<'S>
        when ('T or 'S) : (static member (+) : 'S -> 'T -> 'S) 
        and  ('T or 'S) : (static member (-) : 'S -> 'T -> 'S) 
        and   'S : (static member Zero : 'S)
        and   'S : (static member DivideByInt : ^S * int -> ^S) 
        
    /// Adaptively checks whether the predicate holds for all entries.
    val forallA: predicate: ('K -> 'V -> aval<bool>) -> map: amap<'K, 'V> -> aval<bool> 
    
    /// Adaptively checks whether the predicate holds for at least one entry.
    val existsA: predicate: ('K -> 'V -> aval<bool>) -> map: amap<'K, 'V> -> aval<bool> 
    
    /// Adaptively counts all elements fulfilling the predicate
    val countBy : predicate: ('K -> 'V -> bool) -> map: amap<'K, 'V> -> aval<int>

    /// Adaptively counts all elements fulfilling the predicate
    val countByA : predicate: ('K -> 'V -> aval<bool>) -> map: amap<'K, 'V> -> aval<int>

    /// Adaptively computes the sum of all values returned by mapping for the map.
    val inline sumByA: mapping : ('K -> 'V -> aval<'T>) -> map: amap<'K, 'V> -> aval<'S>
        when ('T or 'S) : (static member (+) : 'S -> 'T -> 'S) 
        and  ('T or 'S) : (static member (-) : 'S -> 'T -> 'S) 
        and   'S : (static member Zero : 'S)
        
    /// Adaptively computes the average of all values returned by mapping for the map.
    val inline averageByA: mapping : ('K -> 'V -> aval<'T>) -> map: amap<'K, 'V> -> aval<'S>
        when ('T or 'S) : (static member (+) : 'S -> 'T -> 'S) 
        and  ('T or 'S) : (static member (-) : 'S -> 'T -> 'S) 
        and   'S : (static member Zero : 'S)
        and   'S : (static member DivideByInt : ^S * int -> ^S) 

