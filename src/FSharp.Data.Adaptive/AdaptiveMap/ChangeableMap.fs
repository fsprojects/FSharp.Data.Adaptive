namespace FSharp.Data.Adaptive

open FSharp.Data.Traceable

/// changeable adaptive map that allows mutation by user-code and implements amap.
[<Sealed>]
type ChangeableMap<'Key, 'Value>(initial : HashMap<'Key, 'Value>) =
    let history = 
        let h = History(HashMap.trace)
        h.Perform(HashMap.differentiate HashMap.empty initial) |> ignore
        h

    /// the number of entries currently in the map.
    member x.Count = 
        history.State.Count

    /// is the map currently empty?
    member x.IsEmpty =
        history.State.IsEmpty

    /// true if the map contains the given key.
    member x.ContainsKey key =
        HashMap.containsKey key history.State

    /// returns the (optional) value associated to key.
    member x.TryGetValue key =
        HashMap.tryFind key history.State

    /// clears the map.
    member x.Clear() =
        if not (HashMap.isEmpty history.State) then
            let ops = HashMap.differentiate history.State HashMap.empty
            history.Perform ops |> ignore
        

    /// gets or sets the current state as HashMap.
    member x.Value
        with get() = 
            history.State
        and set value =
            let ops = HashMap.differentiate history.State value
            history.Perform ops |> ignore
        

    /// removes the entry for the given key and returns whether the element was deleted.
    member x.Remove(key : 'Key) =
        history.Perform (HashMap.single key Remove |> HashMapDelta)
        
    /// adds the given key/value pair to the map and returns true when the map changed. (overrides existing values)
    member x.Add(key : 'Key, value : 'Value) =
        history.Perform (HashMap.single key (Set value) |> HashMapDelta) 
        
    /// gets or sets the value associated to key.
    member x.Item
        with get (key : 'Key) =     
            history.State.[key]

        and set (key : 'Key) (value : 'Value) = 
            history.Perform (HashMap.single key (Set value) |> HashMapDelta) |> ignore
        
    /// creates an adaptive reader for the map.
    member x.GetReader() =
        history.NewReader()

    /// creates a new empty cmap.
    new() = ChangeableMap(HashMap.empty)

    /// creates a new cmap containing all the given elements.
    new(elements : seq<'Key * 'Value>) = ChangeableMap(HashMap.ofSeq elements)

    interface amap<'Key, 'Value> with
        member x.IsConstant = false
        member x.GetReader() = x.GetReader()
        member x.Content = history :> aval<_>

/// changeable adaptive map that allows mutation by user-code and implements amap.
and cmap<'Key, 'Value> = ChangeableMap<'Key, 'Value>



