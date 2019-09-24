namespace FSharp.Data.Adaptive

open FSharp.Data.Traceable

/// changeable adaptive map that allows mutation by user-code and implements amap.
[<Sealed>]
type ChangeableMap<'Key,'Value> =
    interface AdaptiveHashMap<'Key,'Value>

    /// creates a new empty cmap.
    new : unit -> ChangeableMap<'Key,'Value>

    /// creates a new cmap containing all the given elements.
    new : initial:HashMap<'Key,'Value> -> ChangeableMap<'Key,'Value>

    /// creates a new cmap containing all the given elements.
    new : elements:seq<'Key * 'Value> -> ChangeableMap<'Key,'Value>

    /// the number of entries currently in the map.
    member Count : int
    
    /// true if the map contains the given key.
    member ContainsKey : key : 'Key -> bool
    
    /// returns the (optional) value associated to key.
    member TryGetValue : key : 'Key -> option<'Value>

    /// is the map currently empty?
    member IsEmpty : bool

    /// adds the given key/value pair to the map and returns true when the map changed. (overrides existing values)
    member Add : key:'Key * value:'Value -> bool

    /// removes the entry for the given key and returns whether the element was deleted.
    member Remove : key:'Key -> bool

    /// clears the map.
    member Clear : unit -> unit

    /// creates an adaptive reader for the map.
    member GetReader : unit -> IOpReader<HashMap<'Key,'Value>, HashMapDelta<'Key,'Value>>

    /// gets or sets the value associated to key.
    member Item : key:'Key -> 'Value with get, set

    /// gets or sets the current state as HashMap.
    member Value : HashMap<'Key,'Value> with get, set

/// changeable adaptive map that allows mutation by user-code and implements amap.
type cmap<'Key,'Value> = ChangeableMap<'Key,'Value>
