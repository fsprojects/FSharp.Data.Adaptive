namespace FSharp.Data.Adaptive

open FSharp.Data.Traceable

/// an adaptive reader for amap that allows to pull operations and exposes its current state.
type IHashMapReader<'Key,'Value> = IOpReader<HashMap<'Key,'Value>,HashMapDelta<'Key,'Value>>

/// adaptive map datastructure.
[<Interface>]
type AdaptiveHashMap<'Key,'Value> =
    /// is the map constant?
    abstract member IsConstant : bool

    /// the current content of the map as aval.
    abstract member Content : aval<HashMap<'Key,'Value>>

    /// gets a new reader to the map.
    abstract member GetReader : unit -> IHashMapReader<'Key,'Value>

/// adaptive map datastructure.
type amap<'Key,'Value> = AdaptiveHashMap<'Key,'Value>

/// functional operators for amap<_,_>
module AMap =
    /// the empty map.
    val empty<'Key,'Value> : amap<'Key,'Value>

    /// a constant amap holding a single key/value.
    val single : key:'Key -> value:'Value -> amap<'Key,'Value>

    /// creates an amap holding the given entries.
    val ofSeq : elements:seq<'Key * 'Value> -> amap<'Key,'Value>

    /// creates an amap holding the given entries.
    val ofList : elements:('Key * 'Value) list -> amap<'Key,'Value>

    /// creates an amap holding the given entries.
    val ofArray : elements:('Key * 'Value) array -> amap<'Key,'Value>

    /// creates an aval providing access to the current content of the map.
    val toAVal : map:amap<'Key, 'Value> -> aval<HashMap<'Key,'Value>>

    /// adaptively maps over the given map.
    val map : mapping:('Key -> 'Value1 -> 'Value2) -> map:amap<'Key,'Value1> -> amap<'Key,'Value2>

    /// creates an amap with the keys from the set and the values given by mapping.
    val mapSet : mapping:('Key -> 'Value) -> set:aset<'Key> -> amap<'Key,'Value>

    /// adaptively maps over the given map without exposing keys.
    val map' : mapping:('Value1 -> 'Value2) -> map:amap<'Key,'Value1> -> amap<'Key,'Value2>

    /// adaptively chooses all elements returned by mapping.  
    val choose : mapping:('Key -> 'Value1 -> 'Value2 option) -> map:amap<'Key,'Value1> -> amap<'Key,'Value2>
    
    /// adaptively chooses all elements returned by mapping without exposing keys.  
    val choose' : mapping:('Value1 -> 'Value2 option) -> map:amap<'Key,'Value1> -> amap<'Key,'Value2>

    /// adaptively filters the set using the given predicate.
    val filter : predicate:('Key -> 'Value -> bool) -> map:amap<'Key,'Value> -> amap<'Key,'Value>

    /// adaptively filters the set using the given predicate without exposing keys.
    val filter' : predicate:('Value -> bool) -> map:amap<'Key,'Value> -> amap<'Key,'Value>

    /// adaptively unions both maps using the given resolve functions when colliding entries are found.
    val unionWith : resolve:('Key -> 'Value -> 'Value -> 'Value) -> a:amap<'Key,'Value> -> b:amap<'Key,'Value> -> amap<'Key,'Value>
    
    /// adaptively unions both maps preferring the right value when colliding entries are found.
    val union : a:amap<'Key,'Value> -> b:amap<'Key,'Value> -> amap<'Key,'Value>

    /// creates an amap for the given aval.
    val ofAVal : value:aval<#seq<'Key * 'Value>> -> amap<'Key,'Value>
 
    /// adaptively maps over the given aval and returns the resulting map.
    val bind : mapping:('T -> amap<'Key,'Value>) -> value:aval<'T> -> amap<'Key,'Value>

    /// creates an aset holding all key/value tuples from the map.
    val toASet : map:amap<'Key,'Value> -> aset<'Key * 'Value>

