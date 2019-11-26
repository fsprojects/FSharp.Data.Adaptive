namespace FSharp.Data.Adaptive

open FSharp.Data.Traceable

/// Changeable adaptive set that allows mutation by user-code and implements aset.
[<Sealed>]
type ChangeableHashSet<'T>(initial : HashSet<'T>) =
    let history = 
        let h = History(CountingHashSet.traceNoRefCount)
        if not initial.IsEmpty then 
            let delta = HashSet.computeDelta HashSet.empty initial
            h.Perform delta |> ignore
        h

    let content = history |> AVal.map CountingHashSet.toHashSet
    
    /// The number of entries currently in the set.
    member x.Count =
        history.State.Count

    /// Is the set currently empty?
    member x.IsEmpty =
        history.State.IsEmpty

    /// Checks whether the given value is contained.
    member x.Contains (value : 'T) =
        CountingHashSet.contains value history.State

    /// Gets or sets the current state as HashSet.
    member x.Value
        with get() = 
            AVal.force content
        and set newSet = 
            x.UpdateTo newSet
                
    /// Sets the current state as HashSet.
    member x.UpdateTo(newSet : HashSet<'T>) =
        let nonCountingSet = AVal.force content
        if not (cheapEqual nonCountingSet newSet) then
            let delta = HashSet.computeDelta nonCountingSet newSet
            history.Perform(delta) |> ignore

    /// Adds a value and returns whether the element was new.
    member x.Add(value : 'T) =
        history.Perform (HashSetDelta.single (Add value))

    /// Removes a value and returns whether the element was deleted.
    member x.Remove(value : 'T) =
        history.Perform (HashSetDelta.single (Rem value))

    /// Clears the set.
    member x.Clear() =
        if not history.State.IsEmpty then
            let ops = CountingHashSet.computeDelta history.State CountingHashSet.empty
            history.Perform ops |> ignore

    /// Adds all the given values to the set.
    member x.UnionWith (other : seq<'T>) =
        let ops = other |> Seq.map (fun v -> v, 1) |> HashMap.ofSeq |> HashSetDelta
        history.Perform ops |> ignore
        
    /// Removes all the given elements from the set.
    member x.ExceptWith (other : seq<'T>) =
        let ops = other |> Seq.map (fun v -> v, -1) |> HashMap.ofSeq |> HashSetDelta
        history.Perform ops |> ignore

    /// Creates an adaptive reader for the set.
    member x.GetReader() : IHashSetReader<'T> =
        history.NewReader()

    interface AdaptiveHashSet<'T> with
        member x.IsConstant = false
        member x.GetReader() = x.GetReader()
        member x.Content = content
        member x.History = Some history
        
    override x.ToString() =
        let suffix =
            if x.Count > 5 then "; ..."
            else ""

        let content =
            history.State |> Seq.truncate 5 |> Seq.map (sprintf "%A") |> String.concat "; "

        "cset [" + content + suffix + "]"

    member private x.AsString = x.ToString()

    /// Creates a new empty cset.
    new() = cset<'T>(HashSet.empty)

    /// Creates a new cset containing all the given elements.
    new(elements : seq<'T>) = cset<'T>(HashSet.ofSeq elements)

and cset<'T> = ChangeableHashSet<'T>
