namespace FSharp.Control.Incremental

open FSharp.Control.Traceable

/// changeable incremental set that allows mutation by user-code and implements aset.
[<Sealed>]
type cset<'a>(initial : HashSet<'a>) =
    let history = 
        let h = History(CountingHashSet.traceNoRefCount)
        if not initial.IsEmpty then 
            let delta = HashSet.differentiate HashSet.empty initial
            h.Perform delta |> ignore
        h

    let content = history |> ARef.map CountingHashSet.toHashSet
    
    /// the number of entries currently in the set.
    member x.Count =
        history.State.Count

    /// is the set currently empty?
    member x.IsEmpty =
        history.State.IsEmpty

    /// checks whether the given value is contained.
    member x.Contains (value : 'a) =
        CountingHashSet.contains value history.State

    /// gets or sets the current state as HashSet.
    member x.Value
        with get() = 
            ARef.force content
        and set newSet =
            HashSet.differentiate (ARef.force content) newSet
            |> history.Perform
            |> ignore

    /// adds a value and returns whether the element was new.
    member x.Add(value : 'a) =
        history.Perform (DHashSet.single (Add value))

    /// removes a value and returns whether the element was deleted.
    member x.Remove(value : 'a) =
        history.Perform (DHashSet.single (Rem value))

    /// clears the set.
    member x.Clear() =
        if not history.State.IsEmpty then
            let ops = CountingHashSet.differentiate history.State CountingHashSet.empty
            history.Perform ops |> ignore

    /// adds all the given values to the set.
    member x.UnionWith (other : seq<'a>) =
        let ops = other |> Seq.map (fun v -> v, 1) |> HashMap.ofSeq |> DHashSet
        history.Perform ops |> ignore
        
    /// removes all the given elements from the set.
    member x.ExceptWith (other : seq<'a>) =
        let ops = other |> Seq.map (fun v -> v, -1) |> HashMap.ofSeq |> DHashSet
        history.Perform ops |> ignore

    /// creates an incremental reader for the set.
    member x.GetReader() : ISetReader<'a> =
        history.NewReader()

    interface aset<'a> with
        member x.IsConstant = false
        member x.GetReader() = x.GetReader()
        member x.Content = content
        
    override x.ToString() =
        let suffix =
            if x.Count > 5 then "; ..."
            else ""

        let content =
            history.State |> Seq.truncate 5 |> Seq.map (sprintf "%A") |> String.concat "; "

        "cset [" + content + suffix + "]"

    member private x.AsString = x.ToString()

    /// creates a new empty cset.
    new() = cset<'a>(HashSet.empty)

    /// creates a new cset containing all the given elements.
    new(elements : seq<'a>) = cset<'a>(HashSet.ofSeq elements)
    