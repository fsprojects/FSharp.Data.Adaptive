namespace FSharp.Control.Incremental

// TODO: documentation

open FSharp.Control.Traceable

[<Sealed>]
type cset<'a>(initial : HashSet<'a>) =
    let history = 
        let h = History(CountingHashSet.traceNoRefCount)
        if not initial.IsEmpty then 
            let delta = HashSet.differentiate HashSet.empty initial
            h.Perform delta |> ignore
        h

    let content = history |> ARef.map CountingHashSet.toHashSet
    
    member x.Add(value : 'a) =
        history.Perform (DHashSet.single (Add value))

    member x.Remove(value : 'a) =
        history.Perform (DHashSet.single (Rem value))

    member x.UnionWith (other : seq<'a>) =
        let ops = other |> Seq.map (fun v -> v, 1) |> HashMap.ofSeq |> DHashSet
        history.Perform ops |> ignore
        
    member x.ExceptWith (other : seq<'a>) =
        let ops = other |> Seq.map (fun v -> v, -1) |> HashMap.ofSeq |> DHashSet
        history.Perform ops |> ignore

    member x.Clear() =
        if not history.State.IsEmpty then
            let ops = CountingHashSet.differentiate history.State CountingHashSet.empty
            history.Perform ops |> ignore

    member x.IsEmpty =
        history.State.IsEmpty

    member x.Count =
        history.State.Count

    member x.Contains (value : 'a) =
        CountingHashSet.contains value history.State

    member x.GetReader() : ISetReader<'a> =
        history.NewReader()

    member x.Value
        with get() = 
            ARef.force content
        and set newSet =
            HashSet.differentiate (ARef.force content) newSet
            |> history.Perform
            |> ignore

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

    new() = 
        cset<'a>(HashSet.empty)

    new(elements : seq<'a>) = 
        cset<'a>(HashSet.ofSeq elements)
    