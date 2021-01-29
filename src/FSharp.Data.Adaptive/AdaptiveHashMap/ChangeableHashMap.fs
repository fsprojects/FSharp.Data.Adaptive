namespace FSharp.Data.Adaptive

open FSharp.Data.Traceable

/// Changeable adaptive map that allows mutation by user-code and implements amap.
[<Sealed>]
type ChangeableHashMap<'Key, 'Value>(initial : HashMap<'Key, 'Value>) =
    let history = 
        let h = History(HashMap.trace)
        h.Perform(HashMap.computeDelta HashMap.empty initial) |> ignore
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
            let ops = HashMap.computeDelta history.State HashMap.empty
            history.Perform ops |> ignore
        

    /// Gets or sets the current state as HashMap.
    member x.Value
        with get() = 
            history.State
        and set value = 
            x.UpdateTo value |> ignore

    /// Sets the current state as HashMap applying the init function to new elements and the update function to
    /// existing ones.
    member x.UpdateTo(target : HashMap<'Key, 'T2>, init : 'T2 -> 'Value, update : 'Value -> 'T2 -> 'Value) =
        let current = history.State

        let store = 
            (current, target) ||> HashMap.choose2V (fun i l r ->
                match l with
                | ValueNone -> 
                    match r with
                    | ValueSome r -> ValueSome (Set (init r))
                    | ValueNone -> ValueNone
                | ValueSome l ->
                    match r with
                    | ValueSome r -> 
                        let nl = update l r
                        if cheapEqual l nl then 
                            ValueNone
                        else 
                            ValueSome (Set nl)
                    | ValueNone ->
                        ValueSome Remove
            )

        let ops = HashMapDelta(store)
        history.Perform ops |> ignore
        
    /// Sets the current state as HashMap.
    member x.UpdateTo(target : HashMap<'Key, 'Value>) =
        if not (cheapEqual history.State target) then
            let delta = HashMap.computeDelta history.State target
            if HashMapDelta.isEmpty delta then 
                false
            else
                history.PerformUnsafe(target, delta)
        else
            false

    /// Performs the given Operations on the Map.
    member x.Perform(operations : HashMapDelta<'Key, 'Value>) = 
        if not (HashMapDelta.isEmpty operations) then
            history.Perform operations |> ignore
        
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
    new() = ChangeableHashMap(HashMap.empty)

    /// Creates a new cmap containing all the given elements.
    new(elements : seq<'Key * 'Value>) = ChangeableHashMap(HashMap.ofSeq elements)
    
    #if !FABLE_COMPILER
    /// Creates a new cmap containing all the given elements.
    new(elements : seq<struct('Key * 'Value)>) = ChangeableHashMap(HashMap.OfSeq elements)
    #endif


    member x.GetEnumerator() = history.State.GetEnumerator()

    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = (history.State :> System.Collections.IEnumerable).GetEnumerator()
        
    interface System.Collections.Generic.IEnumerable<'Key * 'Value> with
        member x.GetEnumerator() = (history.State :> seq<_>).GetEnumerator()

    interface IAdaptiveHashMap<'Key, 'Value> with
        member x.IsConstant = false
        member x.GetReader() = x.GetReader()
        member x.Content = history :> aval<_>
        member x.History = Some history

/// Changeable adaptive map that allows mutation by user-code and implements amap.
and cmap<'Key, 'Value> = ChangeableHashMap<'Key, 'Value>



