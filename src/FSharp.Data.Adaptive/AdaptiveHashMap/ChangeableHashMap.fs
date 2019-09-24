namespace FSharp.Data.Adaptive

open FSharp.Data.Traceable

/// Changeable adaptive map that allows mutation by user-code and implements amap.
[<Sealed>]
type ChangeableMap<'Key, 'Value>(initial : HashMap<'Key, 'Value>) =
    let history = 
        let h = History(HashMap.trace)
        h.Perform(HashMap.differentiate HashMap.empty initial) |> ignore
        h

    /// The number of entries currently in the map.
    member x.Count = 
        history.State.Count

    /// Is the map currently empty?
    member x.IsEmpty =
        history.State.IsEmpty

    /// True if the map contains the given key.
    member x.ContainsKey key =
        HashMap.containsKey key history.State

    /// Returns the (optional) value associated to key.
    member x.TryGetValue key =
        HashMap.tryFind key history.State

    /// Clears the map.
    member x.Clear() =
        if not (HashMap.isEmpty history.State) then
            let ops = HashMap.differentiate history.State HashMap.empty
            history.Perform ops |> ignore
        

    /// Gets or sets the current state as HashMap.
    member x.Value
        with get() = 
            history.State
        and set value =
            let ops = HashMap.differentiate history.State value
            history.Perform ops |> ignore
        

    /// Removes the entry for the given key and returns whether the element was deleted.
    member x.Remove(key : 'Key) =
        history.Perform (HashMap.single key Remove |> HashMapDelta)
        
    /// Adds the given key/value pair to the map and returns true when the map changed. (overrides existing values)
    member x.Add(key : 'Key, value : 'Value) =
        history.Perform (HashMap.single key (Set value) |> HashMapDelta) 
        
    /// Gets or sets the value associated to key.
    member x.Item
        with get (key : 'Key) =     
            history.State.[key]

        and set (key : 'Key) (value : 'Value) = 
            history.Perform (HashMap.single key (Set value) |> HashMapDelta) |> ignore
        
    /// Creates an adaptive reader for the map.
    member x.GetReader() =
        history.NewReader()

    /// Creates a new empty cmap.
    new() = ChangeableMap(HashMap.empty)

    /// Creates a new cmap containing all the given elements.
    new(elements : seq<'Key * 'Value>) = ChangeableMap(HashMap.ofSeq elements)

    interface AdaptiveHashMap<'Key, 'Value> with
        member x.IsConstant = false
        member x.GetReader() = x.GetReader()
        member x.Content = history :> aval<_>

/// Changeable adaptive map that allows mutation by user-code and implements amap.
and cmap<'Key, 'Value> = ChangeableMap<'Key, 'Value>



