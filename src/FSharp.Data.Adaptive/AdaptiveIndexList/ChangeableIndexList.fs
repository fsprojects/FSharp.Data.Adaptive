namespace FSharp.Data.Adaptive

open FSharp.Data.Traceable

/// Changeable adaptive map that allows mutation by user-code and implements amap.
[<Sealed>]
type ChangeableIndexList<'T>(initial: IndexList<'T>) =
    let history = 
        let h = History(IndexList.trace)
        h.Perform(IndexList.differentiate IndexList.empty initial) |> ignore
        h

    override x.ToString() =
        history.State |> Seq.map (sprintf "%A") |> String.concat "; " |> sprintf "clist [%s]"

    member x.Count = history.State.Count

    member x.IsEmpty = history.State.IsEmpty

    member x.Value
        with get() = 
            history.State

        and set (state: IndexList<'T>) =
            let delta = IndexList.differentiate history.State state
            if not (IndexListDelta.isEmpty delta) then
                history.Perform delta |> ignore

    member x.Append (element: 'T) =
        let index = Index.after history.State.MaxIndex
        history.Perform(IndexListDelta.single index (Set element)) |> ignore
        index
       
    member x.Prepend (element: 'T) =
        let index = Index.before history.State.MinIndex
        history.Perform(IndexListDelta.single index (Set element)) |> ignore
        index
       
    member x.Item
        with get (index: Index) = 
            history.State.[index]
        and set (index: Index) (value: 'T) = 
            history.Perform (IndexListDelta.single index (Set value)) |> ignore
        
    member x.Item
        with get (index: int) = 
            if index < 0 || index >= history.State.Count then raise <| System.IndexOutOfRangeException()
            history.State.[index]

        and set (index: int) (value: 'T) = 
            if index < 0 || index > history.State.Content.Count then raise <| System.IndexOutOfRangeException()
            let left, self, right = MapExt.neighboursAt index history.State.Content

            let right = match self with | Some s -> Some s | None -> right
            let index = 
                match left, right with
                | Some (before,_), Some (after,_) -> Index.between before after
                | None,            Some (after,_) -> Index.before after
                | Some (before,_), None           -> Index.after before
                | None,            None           -> Index.after Index.zero

            history.Perform (IndexListDelta.single index (Set value)) |> ignore

    member x.Remove (index: Index) =
        history.Perform(IndexListDelta.single index Remove)
        
    member x.RemoveAt (index: int) =
        if index < 0 || index >= history.State.Count then raise <| System.IndexOutOfRangeException()
        let (index, _) = history.State.Content |> MapExt.item index
        history.Perform(IndexListDelta.single index Remove) |> ignore

    member x.MinIndex = history.State.MinIndex
    member x.MaxIndex = history.State.MaxIndex

    new() = ChangeableIndexList IndexList.empty
    new(elements : seq<'T>) = ChangeableIndexList (IndexList.ofSeq elements)

    interface alist<'T> with
        member x.IsConstant = false
        member x.Content = history :> _
        member x.GetReader() = history.NewReader()


type clist<'T> = ChangeableIndexList<'T>