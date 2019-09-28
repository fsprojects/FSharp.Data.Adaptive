namespace FSharp.Data.Adaptive

open FSharp.Data.Traceable

/// Changeable adaptive list that allows mutation by user-code and implements alist.
[<Sealed>]
type ChangeableIndexList<'T>(initial: IndexList<'T>) =
    let history = 
        let h = History(IndexList.trace)
        h.Perform(IndexList.computeDelta IndexList.empty initial) |> ignore
        h

    override x.ToString() =
        history.State |> Seq.map (sprintf "%A") |> String.concat "; " |> sprintf "clist [%s]"

    /// is the list currently empty?
    member x.IsEmpty = history.State.IsEmpty

    /// the number of elements currently in the list.
    member x.Count = history.State.Count

    /// Gets or sets the value for the list.
    member x.Value
        with get() = 
            history.State

        and set (state: IndexList<'T>) =
            let delta = IndexList.computeDelta history.State state
            if not (IndexListDelta.isEmpty delta) then
                history.Perform delta |> ignore

    /// Appends an element to the list and returns its Index.
    member x.Append (element: 'T) =
        let index = Index.after history.State.MaxIndex
        history.Perform(IndexListDelta.single index (Set element)) |> ignore
        index
       
    /// Appends an element to the list and returns its Index.
    member x.Add (element: 'T) = x.Append(element)

    /// Prepends an element to the list and returns its Index.
    member x.Prepend (element: 'T) =
        let index = Index.before history.State.MinIndex
        history.Perform(IndexListDelta.single index (Set element)) |> ignore
        index
       
    /// Inserts an element at the given position in the list and returns its Index.
    /// Note that the position can be equal to the count of the list.
    member x.InsertAt(index : int, element : 'T) =
        if index < 0 || index > history.State.Count then raise <| System.IndexOutOfRangeException()
        let l, s, r = MapExt.neighboursAt index history.State.Content
        let r = 
            match s with
                | Some s -> Some s
                | None -> r

        let index = 
            match l, r with
            | Some (before,_), Some (after,_) -> Index.between before after
            | None,            Some (after,_) -> Index.before after
            | Some (before,_), None           -> Index.after before
            | None,            None           -> Index.after Index.zero

        history.Perform (IndexListDelta.single index (Set element)) |> ignore
        index

    /// Inserts an element directly after the given index and returns the new index for the element.
    member x.InsertAfter(index : Index, element : 'T) =
        let _l, s, r = MapExt.neighbours index history.State.Content
        match s with
        | None ->
            history.Perform (IndexListDelta.single index (Set element)) |> ignore
            index
        | Some (s, _) ->
            let index =
                match r with
                | Some (r,_) -> Index.between s r
                | None -> Index.after index
            history.Perform (IndexListDelta.single index (Set element)) |> ignore
            index
            
    /// Inserts an element directly before the given index and returns the new index for the element.
    member x.InsertBefore(index : Index, element : 'T) =
        let l, s, _r = MapExt.neighbours index history.State.Content
        match s with
        | None ->
            history.Perform (IndexListDelta.single index (Set element)) |> ignore
            index
        | Some (s, _) ->
            let index =
                match l with
                | Some (l,_) -> Index.between l s
                | None -> Index.after index
            history.Perform (IndexListDelta.single index (Set element)) |> ignore
            index

    /// Gets or sets an element in the list at the given index.
    member x.Item
        with get (index: Index) = 
            history.State.[index]
        and set (index: Index) (value: 'T) = 
            history.Perform (IndexListDelta.single index (Set value)) |> ignore
        
    /// Gets or sets the element associated to index.
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

    /// Clears the list.
    member x.Clear() =  
        if not history.State.IsEmpty then
            let ops = IndexList.computeDelta history.State IndexList.empty
            history.Perform ops |> ignore        

    /// Removes the given index from the list and returns true if the element was deleted.
    member x.Remove (index: Index) =
        history.Perform(IndexListDelta.single index Remove)
        
    /// Removes the element at the given position and returns its Index.
    member x.RemoveAt (index: int) =
        if index < 0 || index >= history.State.Count then raise <| System.IndexOutOfRangeException()
        let (index, _) = history.State.Content |> MapExt.item index
        history.Perform(IndexListDelta.single index Remove) |> ignore
        index

    /// Tries to get the Index associated to the given position.
    member x.TryGetIndex(index : int) =
        history.State.TryGetIndex index

    /// Gets the (optional) element associated to the given Index.
    member x.TryGet(index : Index) =
        IndexList.tryGet index history.State

    /// Tries to get the element at the given position.
    member x.TryAt(index : int) =
        IndexList.tryAt index history.State


    /// The smallest index contained in the list (or Index.zero if empty)
    member x.MinIndex = history.State.MinIndex

    /// The largest index contained in the list (or Index.zero if empty)
    member x.MaxIndex = history.State.MaxIndex

    /// Creates a new list initially holding the given elements.
    new(elements : seq<'T>) = ChangeableIndexList (IndexList.ofSeq elements)

    /// Creates a new empty list.
    new() = ChangeableIndexList IndexList.empty

    interface alist<'T> with
        member x.IsConstant = false
        member x.Content = history :> _
        member x.GetReader() = history.NewReader()

/// Changeable adaptive list that allows mutation by user-code and implements alist.
type clist<'T> = ChangeableIndexList<'T>