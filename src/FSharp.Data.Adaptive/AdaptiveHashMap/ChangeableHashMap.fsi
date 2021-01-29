namespace FSharp.Data.Adaptive

open FSharp.Data.Traceable

/// Changeable adaptive map that allows mutation by user-code and implements amap.
[<Sealed>]
type ChangeableHashMap<'Key,'Value> =
    interface IAdaptiveHashMap<'Key,'Value>
    interface System.Collections.Generic.IEnumerable<'Key * 'Value>

    /// Creates a new empty cmap.
    new : unit -> ChangeableHashMap<'Key,'Value>

    /// Creates a new cmap containing all the given elements.
    new : initial:HashMap<'Key,'Value> -> ChangeableHashMap<'Key,'Value>

    /// Creates a new cmap containing all the given elements.
    new : elements:seq<'Key * 'Value> -> ChangeableHashMap<'Key,'Value>
    
    #if !FABLE_COMPILER
    /// Creates a new cmap containing all the given elements.
    new : elements:seq<struct('Key * 'Value)> -> ChangeableHashMap<'Key, 'Value>
    #endif


    /// The number of entries currently in the map.
    member Count : int
    
    /// True if the map contains the given key.
    member ContainsKey : key : 'Key -> bool
    
    /// Returns the (optional) value associated to key.
    member TryGetValue : key : 'Key -> option<'Value>

    /// Is the map currently empty?
    member IsEmpty : bool

    /// Adds the given key/value pair to the map and returns true when the map changed. (overrides existing values)
    member Add : key:'Key * value:'Value -> bool

    /// Removes the entry for the given key and returns whether the element was deleted.
    member Remove : key:'Key -> bool

    /// Clears the map.
    member Clear : unit -> unit

    /// Creates an adaptive reader for the map.
    member GetReader : unit -> IOpReader<HashMap<'Key,'Value>, HashMapDelta<'Key,'Value>>

    /// Gets or sets the value associated to key.
    member Item : key:'Key -> 'Value with get, set

    /// Gets or sets the current state as HashMap.
    member Value : HashMap<'Key,'Value> with get, set
    
    /// Sets the current state as HashMap applying the init function to new elements and the update function to
    /// existing ones.
    member UpdateTo : target : HashMap<'Key, 'T2> * init : ('T2 -> 'Value) * update : ('Value -> 'T2 -> 'Value) -> unit
    
    /// Sets the current state as HashMap.
    member UpdateTo : target : HashMap<'Key, 'Value> -> bool

    /// Performs the given Operations on the Map.
    member Perform: operations : HashMapDelta<'Key, 'Value> -> unit

    member GetEnumerator : unit -> HashMapEnumerator<'Key, 'Value, 'Key * 'Value>

/// Changeable adaptive map that allows mutation by user-code and implements amap.
type cmap<'Key,'Value> = ChangeableHashMap<'Key,'Value>
