namespace FSharp.Data.Adaptive

open FSharp.Data.Traceable

/// An adaptive reader for amap that allows to pull operations and exposes its current state.
type IHashMapReader<'Key,'Value> = IOpReader<HashMap<'Key,'Value>,HashMapDelta<'Key,'Value>>

/// Adaptive map datastructure.
[<Interface>]
type AdaptiveHashMap<'Key,'Value> =
    /// is the map constant?
    abstract member IsConstant : bool

    /// the current content of the map as aval.
    abstract member Content : aval<HashMap<'Key,'Value>>

    /// gets a new reader to the map.
    abstract member GetReader : unit -> IHashMapReader<'Key,'Value>

/// Adaptive map datastructure.
type amap<'Key,'Value> = AdaptiveHashMap<'Key,'Value>

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

    /// Adaptively unions both maps using the given resolve functions when colliding entries are found.
    val unionWith : resolve:('Key -> 'Value -> 'Value -> 'Value) -> a:amap<'Key,'Value> -> b:amap<'Key,'Value> -> amap<'Key,'Value>
    
    /// Adaptively unions both maps preferring the right value when colliding entries are found.
    val union : a:amap<'Key,'Value> -> b:amap<'Key,'Value> -> amap<'Key,'Value>

    /// Creates an amap for the given aval.
    val ofAVal : value:aval<#seq<'Key * 'Value>> -> amap<'Key,'Value>
 
    /// Adaptively maps over the given aval and returns the resulting map.
    val bind : mapping:('T -> amap<'Key,'Value>) -> value:aval<'T> -> amap<'Key,'Value>

    /// Creates an aset holding all key/value tuples from the map.
    val toASet : map:amap<'Key,'Value> -> aset<'Key * 'Value>

    /// Adaptively looks up the given key in the map.
    val tryFind : key:'K -> map:amap<'K, 'V> -> aval<option<'V>>

    /// Adaptively folds over the map using add for additions and trySubtract for removals.
    /// Note the trySubtract may return None indicating that the result needs to be recomputed.
    /// Also note that the order of elements given to add/trySubtract is undefined.
    val foldHalfGroup : add : ('S -> 'K -> 'V -> 'S) -> trySubtract : ('S -> 'K -> 'V -> option<'S>) -> zero : 'S -> set : amap<'K, 'V> -> aval<'S>
    
    /// Adaptively folds over the map using add for additions and subtract for removals.
    /// Note that the order of elements given to add/subtract is undefined.
    val foldGroup : add : ('S -> 'K -> 'V -> 'S) -> subtract : ('S -> 'K -> 'V -> 'S) -> zero : 'S -> set : amap<'K, 'V> -> aval<'S>

    /// Adaptively folds over the map using add for additions and recomputes the value on every removal.
    /// Note that the order of elements given to add is undefined.
    val fold : add : ('S -> 'K -> 'V -> 'S) -> zero : 'S -> set : amap<'K, 'V> -> aval<'S>


