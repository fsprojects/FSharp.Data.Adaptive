namespace FSharp.Data.Adaptive

open FSharp.Data.Traceable

/// Changeable adaptive set that allows mutation by user-code and implements aset.
[<Sealed>]
type ChangeableHashSet<'T>(initial : HashSet<'T>) =
    let history = 
        let h = History(CountingHashSet.traceNoRefCount)
        if not initial.IsEmpty then 
            let delta = HashSet.differentiate HashSet.empty initial
            h.Perform delta |> ignore
        h

    let content = history |> AVal.map CountingHashSet.toHashSet
    
    member x.Count =
        history.State.Count

    member x.IsEmpty =
        history.State.IsEmpty

    member x.Contains (value : 'T) =
        CountingHashSet.contains value history.State

    member x.Value
        with get() = 
            AVal.force content
        and set newSet =
            HashSet.differentiate (AVal.force content) newSet
            |> history.Perform
            |> ignore

    member x.Add(value : 'T) =
        history.Perform (HashSetDelta.single (Add value))

    member x.Remove(value : 'T) =
        history.Perform (HashSetDelta.single (Rem value))

    member x.Clear() =
        if not history.State.IsEmpty then
            let ops = CountingHashSet.differentiate history.State CountingHashSet.empty
            history.Perform ops |> ignore

    member x.UnionWith (other : seq<'T>) =
        let ops = other |> Seq.map (fun v -> v, 1) |> HashMap.ofSeq |> HashSetDelta
        history.Perform ops |> ignore
        
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
        
    override x.ToString() =
        let suffix =
            if x.Count > 5 then "; ..."
            else ""

        let content =
            history.State |> Seq.truncate 5 |> Seq.map (sprintf "%A") |> String.concat "; "

        "cset [" + content + suffix + "]"

    member private x.AsString = x.ToString()

    new() = cset<'T>(HashSet.empty)

    new(elements : seq<'T>) = cset<'T>(HashSet.ofSeq elements)

and cset<'T> = ChangeableHashSet<'T>
