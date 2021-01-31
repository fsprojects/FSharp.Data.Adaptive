namespace FSharp.Data.Adaptive

open System
open System.Threading
open System.Collections
open System.Collections.Generic
open FSharp.Data.Adaptive
open System.Runtime.CompilerServices

[<Struct; CustomComparison; CustomEquality>]
type internal ReversedCompare<'a when 'a : comparison>(value : 'a) =
    member x.Value = value

    override x.GetHashCode() =
        DefaultEquality.hash value

    override x.Equals o =
        match o with
        | :? ReversedCompare<'a> as o -> 
            DefaultEquality.equals value o.Value
        | _ -> 
            false

    interface IComparable with
        member x.CompareTo o =
            match o with
            | :? ReversedCompare<'a> as o -> compare o.Value value
            | _ -> 0

    #if !FABLE_COMPILER
    interface IComparable<ReversedCompare<'a>> with
        member x.CompareTo o =  compare o.Value x.Value
    #endif

/// A persitent array-like structure that allows lookup/insertion/deletion of entries in O(log N).
/// Note that datastructure uses Index instead of int as index type which allows for these efficient implementations.
/// However the datastructure also has accessors that allow getting/setting/deleting entries via an int-index in O(log N).
[<Struct; StructuralEquality; NoComparison>]
[<StructuredFormatDisplay("{AsString}")>]
type IndexList< [<EqualityConditionalOn>] 'T> internal(l : Index, h : Index, content : MapExt<Index, 'T>) =
    
    static let empty = IndexList<'T>(Index.zero, Index.zero, MapExt.empty)

    /// The empty list.
    static member Empty = empty

    /// The smallest Index contained in the list or Index.zero if the list is empty.
    member x.MinIndex = l

    /// The largest Index contained in the list or Index.zero if the list is empty.
    member x.MaxIndex = h

    /// Is the list empty?
    member x.IsEmpty = content.Count = 0

    /// The number of entries in the list.
    member x.Count = content.Count

    /// Internal for getting the underlying store.
    member internal x.Content = content

    /// Gets the entry associated to the given index (if any).
    member x.TryGet (i : Index) =
        MapExt.tryFind i content
        
    /// Gets the entry associated to the given index (if any).
    member x.TryGetV (i : Index) =
        MapExt.tryFindV i content
        
    /// Gets the entry at the given index (if any).
    member x.TryGet (i : int) =
        let mutable index = Index.zero
        let mutable value = Unchecked.defaultof<_>
        if content.TryGetItem(i, &index, &value) then Some value
        else None
        
    /// Gets the entry at the given index (if any).
    member x.TryGetV (i : int) =
        let mutable index = Index.zero
        let mutable value = Unchecked.defaultof<_>
        if content.TryGetItem(i, &index, &value) then ValueSome value
        else ValueNone

    /// Gets the entry associated to the given index or fails if not existing.
    member x.Item
        with get(i : Index) = MapExt.find i content
        
    /// Gets the entry at the given index or fails if not existing.
    member x.Item
        with get(i : int) = 
            let struct(_,v) = MapExt.itemV i content
            v


    member x.GetSlice(min : option<int>, max : option<int>) =
        match min with
        | None ->
            match max with
            | None -> x
            | Some max -> 
                let c = content.Take(max + 1)
                if c.IsEmpty then IndexList.Empty
                else IndexList(MapExt.minKey c, MapExt.maxKey c, c)
        | Some min ->
            match max with
            | Some max -> 
                let c = content.SliceAt(min, max)
                if c.IsEmpty then IndexList.Empty
                else IndexList(MapExt.minKey c, MapExt.maxKey c, c)
            | None -> 
                let c = content.Skip(min)
                if c.IsEmpty then IndexList.Empty
                else IndexList(MapExt.minKey c, MapExt.maxKey c, c)

    member x.GetSlice(min : option<Index>, max : option<Index>) =
        match min with
        | None ->
            match max with
            | None -> x
            | Some max -> 
                let c = MapExt.withMax max content
                if c.IsEmpty then IndexList.Empty
                else IndexList(MapExt.minKey c, MapExt.maxKey c, c)
        | Some min ->
            match max with
            | Some max -> 
                let c = MapExt.slice min max content
                if c.IsEmpty then IndexList.Empty
                else IndexList(MapExt.minKey c, MapExt.maxKey c, c)
            | None -> 
                let c = MapExt.withMin min content
                if c.IsEmpty then IndexList.Empty
                else IndexList(MapExt.minKey c, MapExt.maxKey c, c)

    /// Appends the given element to the list.
    member x.Add(element : 'T) =
        if content.Count = 0 then
            let t = Index.after Index.zero
            IndexList(t, t, MapExt.singleton t element)
        else
            let t = Index.after h
            IndexList(l, t, MapExt.add t element content)
        
    /// Prepends the given element to the list.
    member x.Prepend(element : 'T) =
        if content.Count = 0 then
            let t = Index.after Index.zero
            IndexList(t, t, MapExt.ofList [t, element])
        else
            let t = Index.before l
            IndexList(t, h, MapExt.add t element content)

    /// Adds or updates the element associated to index.
    member x.Set(index : Index, value : 'T) =
        if content.Count = 0 then
            IndexList(index, index, MapExt.singleton index value)

        elif index < l then
            IndexList(index, h, MapExt.add index value content)

        elif index > h then
            IndexList(l, index, MapExt.add index value content)

        else 
            IndexList(l, h, MapExt.add index value content)

    /// Updates the element at the given position or returns the unmodified list if the index was out of bounds.
    member x.Set(index : int, value : 'T) =
        if index < 0 || index >= content.Count then
            x
        else
            match MapExt.tryItemV index content with
            | ValueSome (id,_) -> x.Set(id, value)
            | ValueNone -> x

    /// Updates the element at the given position or returns the unmodified list if the index was out of bounds.
    member x.Update(index : int, update : 'T -> 'T) =
        match MapExt.tryItemV index content with
        | ValueSome (id,v) -> 
            let newContent = MapExt.add id (update v) content
            IndexList(l, h, newContent)
        | ValueNone -> 
            x

    /// Inserts the element at the given position or returns the unmodified list if the index is not in [0..count].
    /// Note that InsertAt works with index = count.
    member x.InsertAt(index : int, value : 'T) =
        if index < 0 || index > content.Count then
            x
        else
            let struct(l, s, r) = MapExt.neighboursAtV index content

            let r = 
                match s with
                | ValueSome s -> ValueSome s
                | ValueNone -> r

            let index = 
                match struct(l, r) with
                | ValueSome (struct(before,_)), ValueSome (struct(after,_)) -> Index.between before after
                | ValueNone,                    ValueSome (struct(after,_)) -> Index.before after
                | ValueSome (struct(before,_)), ValueNone                   -> Index.after before
                | ValueNone,                    ValueNone                   -> Index.after Index.zero
            x.Set(index, value)

    /// Inserts the element directly before the given index.
    member x.InsertBefore(index : Index, value : 'T) =
        let struct(l, s, _r) = MapExt.neighboursV index content
        match s with
        | ValueNone ->
            x.Set(index, value)
        | ValueSome _ ->
            let index = 
                match l with
                | ValueSome (l,_) -> Index.between l index
                | ValueNone -> Index.before index
            x.Set(index, value)
                
    /// Inserts the element directly after the given index.
    member x.InsertAfter(index : Index, value : 'T) =
        let struct(_l, s, r) = MapExt.neighboursV index content
        match s with
            | ValueNone ->
                x.Set(index, value)
            | ValueSome _ ->
                let index =
                    match r with
                    | ValueSome (r,_) -> Index.between index r
                    | ValueNone -> Index.after index
                x.Set(index, value)

    /// Gets the index for the given position or None if the index is out of bounds.
    member x.TryGetIndex(index : int) =
        match MapExt.tryItemV index content with
        | ValueSome (id,_) -> Some id
        | ValueNone -> None
        
    /// Gets the index for the given position or None if the index is out of bounds.
    member x.TryGetIndexV(index : int) =
        match MapExt.tryItemV index content with
        | ValueSome (struct(id,_)) -> ValueSome id
        | ValueNone -> ValueNone

    /// Gets the position for the given index or None if the index is not contained in the list.
    member x.TryGetPosition(index : Index) =
        MapExt.tryGetIndex index content
        
    /// Gets the position for the given index or None if the index is not contained in the list.
    member x.TryGetPositionV(index : Index) =
        MapExt.tryGetIndexV index content

    /// Removes the entry associated to the given index.
    member x.Remove(index : Index) =
        let c = MapExt.remove index content
        if c.Count = 0 then empty
        elif l = index then IndexList(c.GetMinKey(), h, c)
        elif h = index then IndexList(l, c.GetMaxKey(), c)
        else IndexList(l, h, c)
        
    /// Removes the entry at the given position (if any).
    member x.RemoveAt(index : int) =
        match MapExt.tryRemoveAtV index content with
        | ValueSome struct(id, _, c) ->
            if c.Count = 0 then empty
            elif id = l then IndexList(c.GetMinKey(), h, c)
            elif id = h then IndexList(l, c.GetMaxKey(), c)
            else IndexList(l, h, c)
        | ValueNone ->
            x

    /// Applies the mapping function to all elements of the list and returns a new list containing the results.
    member x.Map<'T2>(mapping : Index -> 'T -> 'T2) : IndexList<'T2> =
        IndexList(l, h, MapExt.map mapping content)
        
    /// Applies the mapping function to all elements of the list and returns a new list containing all Some entries.
    member x.Choose(mapping : Index -> 'T -> option<'T2>) =
        let res = MapExt.choose mapping content
        if res.IsEmpty then 
            IndexList.Empty
        else
            // TODO: min/max could be maintained during mapping
            IndexList(res.GetMinKey(), res.GetMaxKey(), res)

    /// Filters the list using the given predicate.
    member x.Filter(predicate : Index -> 'T -> bool) =
        let res = MapExt.filter predicate content
        if res.IsEmpty then 
            IndexList.Empty
        else
            // TODO: min/max could be maintained during mapping
            IndexList(res.GetMinKey(), res.GetMaxKey(), res)

    /// Tries to find the smallest index for the given element.
    member x.TryFind(element : 'T) : option<Index> =
        content |> MapExt.tryPick (fun i v -> if DefaultEquality.equals v element then Some i else None)

    /// Tries to find the smallest index for the given element.
    member x.TryFindV(element : 'T) : voption<Index> =
        content |> MapExt.tryPickV (fun i v -> if DefaultEquality.equals v element then ValueSome i else ValueNone)
        
    /// Removes the first occurrence of the given element (if any).
    member x.Remove(item : 'T) : IndexList<'T> =
        match x.TryFindV(item) with
        | ValueSome index -> x.Remove(index)
        | ValueNone -> x
          
    /// Returns all entres from the list in back-to-front order.
    member x.AsSeqBackward =
        content |> MapExt.toValueSeqBack
        
    /// Returns all entres from the list in back-to-front order.
    member x.AsListBackward =
        content |> MapExt.toValueListBack
        
    /// Returns all entres from the list in back-to-front order.
    member x.AsArrayBackward =
        let mutable i = content.Count
        let res = Array.zeroCreate i
        for (KeyValue(_k, v)) in content do
            i <- i - 1
            res.[i] <- v

        res
        
    /// Returns all entres from the list.
    member x.AsSeq =
        content |> MapExt.toValueSeq
        
    /// Returns all entres from the list.
    member x.AsList =
        content |> MapExt.toValueList
        
    /// Returns all entres from the list.
    member x.AsArray =
        content |> MapExt.toValueArray
        
    /// Conservatively determines whether the two IndexLists are equal.
    /// `O(1)`
    member x.ConservativeEquals(other : IndexList<'T>) =
        System.Object.ReferenceEquals(content, other.Content)

    /// Like choose2 but with existing right values.
    member x.UpdateTo(other : IndexList<'T2>, mapping : Index -> option<'T> -> 'T2 -> 'T3) =
        if other.Count * 5 < content.Count then
            let content = content
            let newStore = 
                other.Content |> MapExt.map (fun idx r ->
                    let l = MapExt.tryFind idx content
                    mapping idx l r
                )
            IndexList(other.MinIndex, other.MaxIndex, newStore)
        else
            let newStore =
                (content, other.Content) ||> MapExt.choose2 (fun i l r ->
                    match r with
                    | Some r -> mapping i l r |> Some
                    | None -> None
                )
            IndexList(other.MinIndex, other.MaxIndex, newStore)

    /// Removes the entry associated to the given index, returns the (optional) value and the list without the specific element.
    member x.TryRemove(index : Index) =
        match MapExt.tryRemoveV index content with
        | ValueSome (value, rest) ->
            if rest.IsEmpty then Some (value, IndexList.Empty)
            else Some (value, IndexList(MapExt.minKey rest, MapExt.maxKey rest, rest))
        | ValueNone ->
            None

    /// Removes the entry associated to the given index, returns the (optional) value and the list without the specific element.
    member x.TryRemoveV(index : Index) =
        match MapExt.tryRemoveV index content with
        | ValueSome struct (value, rest) ->
            if rest.IsEmpty then ValueSome struct (value, IndexList.Empty)
            else ValueSome struct (value, IndexList(MapExt.minKey rest, MapExt.maxKey rest, rest))
        | ValueNone ->
            ValueNone

    /// Tries to deconstruct the list. returns its head and tail if successful and None otherwise.
    member x.TryDeconstruct() =
        match MapExt.tryRemoveMinV content with
        | ValueSome(struct(i0, v0, rest)) ->
            let restList =  
                match rest.TryMinKeyV() with
                | ValueNone -> IndexList.Empty
                | ValueSome next -> IndexList(next, h, rest)
            Some ((i0, v0), restList)
        | ValueNone ->
            None
        
    /// deconstructs the list. returns its head and tail if successful and fails otherwise.
    member x.Deconstruct() =
        match x.TryDeconstruct() with
        | Some v -> v
        | None -> failwith "cannot deconstruct empty IndexList"
        
    /// Tries to deconstruct the list. returns its head and tail if successful and None otherwise.
    member x.TryDeconstructV() =
        match MapExt.tryRemoveMinV content with
        | ValueSome(struct(i0, v0, rest)) ->
            let restList = 
                match rest.TryMinKeyV() with
                | ValueNone -> IndexList.Empty
                | ValueSome next -> IndexList(next, h, rest)
            ValueSome (struct(struct(i0, v0), restList))
        | ValueNone ->
            ValueNone
        
    /// deconstructs the list. returns its head and tail if successful and fails otherwise.
    member x.DeconstructV() =
        match x.TryDeconstructV() with
        | ValueSome v -> v
        | ValueNone -> failwith "cannot deconstruct empty IndexList"
        
    /// Removes the entry associated to the given index, returns the value and the list without the specific element or fails
    /// if the given index was not found in the list.
    member x.RemoveAndGetV(index : Index) =
        match MapExt.tryRemoveV index content with
        | ValueSome (struct(value, rest)) ->
            if rest.IsEmpty then struct (value, IndexList.Empty)
            else struct (value, IndexList(MapExt.minKey rest, MapExt.maxKey rest, rest))
        | ValueNone ->
            failwithf "[IndexList] cannot get and remove element at index %A" index
        

    /// Finds the optional neighbour elements in the list for the given index.
    member x.Neighbours(index : Index) =
        MapExt.neighbours index content
        
    /// Finds the optional neighbour elements in the list for the given index.
    member x.NeighboursV(index : Index) =
        MapExt.neighboursV index content

    /// Gets an unused index directly after the given one.
    member x.NewIndexAfter (index : Index) =
        let struct(l, s, r) = MapExt.neighboursV index content
        let l =
            match s with
            | ValueSome _ -> ValueSome index
            | ValueNone -> match l with | ValueSome (li, _) -> ValueSome li | _ -> ValueNone

        match l with
        | ValueSome li ->
            match r with
            | ValueSome (ri, _) -> Index.between li ri
            | ValueNone -> Index.after li
        | ValueNone ->
            match r with
            | ValueSome (ri, _) -> Index.before ri
            | ValueNone -> Index.after Index.zero

    /// Gets an unused index directly before the given one.
    member x.NewIndexBefore (index : Index) =
        let struct(l, s, r) = MapExt.neighboursV index content
        let r =
            match s with
            | ValueSome _ -> ValueSome index
            | ValueNone -> match r with | ValueSome (ri, _) -> ValueSome ri | _ -> ValueNone

        match l with
        | ValueSome (li, _) ->
            match r with
            | ValueSome ri -> Index.between li ri
            | ValueNone -> Index.after li
        | ValueNone ->
            match r with
            | ValueSome ri -> Index.before ri
            | ValueNone -> Index.after Index.zero

    /// Gets the element directly after index in the list (if any).
    member x.TryGetNext (index : Index) =
        let struct(_,_,n) = MapExt.neighboursV index content
        match n with
        | ValueSome (struct(k,v)) -> Some(k,v)
        | ValueNone -> None
        
    /// Gets the element directly after index in the list (if any).
    member x.TryGetNextV (index : Index) =
        let struct(_,_,n) = MapExt.neighboursV index content
        n
        
    /// Gets the element directly before index in the list (if any).
    member x.TryGetPrev (index : Index) =
        let struct(p,_,_) = MapExt.neighboursV index content
        match p with
        | ValueSome(struct(k,v)) -> Some(k,v)
        | ValueNone -> None
        

    /// Gets the element directly before index in the list (if any).
    member x.TryGetPrevV (index : Index) =
        let struct(p,_,_) = MapExt.neighboursV index content
        p

    /// Returns a list of each element tupled with its successor. 
    member x.Pairwise() =
        match MapExt.tryRemoveMinV content with
        | ValueSome (struct(i0, v0, rest)) ->
            let mutable res = MapExt.empty
            let mutable rest = rest
            let mutable lastIndex = l
            let mutable i0 = i0
            let mutable v0 = v0
            while not (MapExt.isEmpty rest) do
                match MapExt.tryRemoveMinV rest with
                | ValueSome (i1, v1, r) ->
                    res <- MapExt.add i0 (v0, v1) res
                    lastIndex <- i0
                    i0 <- i1
                    v0 <- v1
                    rest <- r
                | ValueNone ->
                    ()

            IndexList(l, lastIndex, res)
        | ValueNone ->
            IndexList.Empty
           
    /// Returns a list of each element tupled with its successor. 
    member x.PairwiseV() =
        match MapExt.tryRemoveMinV content with
        | ValueSome (struct(i0, v0, rest)) ->
            let mutable res = MapExt.empty
            let mutable rest = rest
            let mutable lastIndex = l
            let mutable i0 = i0
            let mutable v0 = v0
            while not (MapExt.isEmpty rest) do
                match MapExt.tryRemoveMinV rest with
                | ValueSome (i1, v1, r) ->
                    res <- MapExt.add i0 (struct(v0, v1)) res
                    lastIndex <- i0
                    i0 <- i1
                    v0 <- v1
                    rest <- r
                | ValueNone ->
                    ()

            IndexList(l, lastIndex, res)
        | ValueNone ->
            IndexList.Empty
             
    /// Returns a list of each element tupled with its successor and the last element tupled with the first.
    member x.PairwiseCyclic() =
        match MapExt.tryRemoveMinV content with
        | ValueSome (struct(i0, initial, rest)) ->
            let mutable res = MapExt.empty
            let mutable rest = rest
            let mutable i0 = i0
            let mutable v0 = initial
            while not (MapExt.isEmpty rest) do
                match MapExt.tryRemoveMinV rest with
                | ValueSome (i1, v1, r) ->
                    res <- MapExt.add i0 (v0, v1) res
                    i0 <- i1
                    v0 <- v1
                    rest <- r
                | ValueNone ->
                    ()

            res <- MapExt.add i0 (v0, initial) res
            IndexList(l, i0, res)
        | ValueNone ->
            IndexList.Empty
      
    /// Returns a list of each element tupled with its successor and the last element tupled with the first.
    member x.PairwiseCyclicV() =
        match MapExt.tryRemoveMinV content with
        | ValueSome (struct(i0, initial, rest)) ->
            let mutable res = MapExt.empty
            let mutable rest = rest
            let mutable i0 = i0
            let mutable v0 = initial
            while not (MapExt.isEmpty rest) do
                match MapExt.tryRemoveMinV rest with
                | ValueSome (i1, v1, r) ->
                    res <- MapExt.add i0 struct(v0, v1) res
                    i0 <- i1
                    v0 <- v1
                    rest <- r
                | ValueNone ->
                    ()

            res <- MapExt.add i0 (v0, initial) res
            IndexList(l, i0, res)
        | ValueNone ->
            IndexList.Empty
    
    override x.ToString() =
        let suffix =
            if x.Count > 5 then "; ..."
            else ""
        let elements = 
            content 
            |> MapExt.toSeq 
            |> Seq.truncate 5
            |> Seq.map (snd >> sprintf "%A") 
            |> String.concat "; "

        sprintf "IndexList [%s%s]" elements suffix

    member private x.AsString = x.ToString()
    
    /// Copies the list to the given array (starting at index)
    member x.CopyTo(dst : 'T[], dstIndex : int) = 
        content.CopyValuesTo(dst, dstIndex)

    /// Tries to find the position for the given entry or -1 if the entry does not exist. O(N)
    member x.IndexOf(item : 'T) =
        x |> Seq.tryFindIndex (DefaultEquality.equals item) |> Option.defaultValue -1

    /// Tries to find the position for the given Index or -1 if the Index does not exist. O(log N)
    member x.IndexOf(index : Index) =
        MapExt.getIndex index content
        
    interface ICollection<'T> with 
        member x.Add(v) = raise (NotSupportedException("IndexList cannot be mutated"))
        member x.Clear() = raise (NotSupportedException("IndexList cannot be mutated"))
        member x.Remove(v) = raise (NotSupportedException("IndexList cannot be mutated"))
        member x.Contains(v) = content |> MapExt.exists (fun _ vi -> DefaultEquality.equals vi v)
        member x.CopyTo(arr,i) = x.CopyTo(arr, i)
        member x.IsReadOnly = true
        member x.Count = x.Count

    interface IList<'T> with
        member x.RemoveAt(i) = raise (NotSupportedException("IndexList cannot be mutated"))
        member x.IndexOf(item : 'T) = x.IndexOf item
        member x.Item
            with get(i : int) = x.[i]
            and set (i : int) (v : 'T) = raise (NotSupportedException("IndexList cannot be mutated"))
        member x.Insert(i,v) = raise (NotSupportedException("IndexList cannot be mutated"))
        
    member x.GetEnumerator() = new IndexListEnumerator<'T>(content.Root)

    interface IEnumerable with
        member x.GetEnumerator() = new IndexListEnumerator<'T>(content.Root) :> _

    interface IEnumerable<'T> with
        member x.GetEnumerator() = new IndexListEnumerator<'T>(content.Root) :> _

/// Enumerator for IndexList.
and IndexListEnumerator<'Value> =
    struct
        val mutable internal Root : MapExtImplementation.Node<Index, 'Value>
        val mutable internal Head : MapExtImplementation.Node<Index, 'Value>
        val mutable internal Tail : list<MapExtImplementation.Node<Index, 'Value>>
        val mutable internal Next : 'Value
        val mutable internal Index : int
        val mutable internal BufferValueCount : int
        val mutable internal Buffer : 'Value[]
    
        member private x.Flatten(start : int, node : MapExtImplementation.Node<_,_>) =
            if isNull node then
                ()
            elif node.Height = 1uy then
                x.Buffer.[start] <- node.Value
            else
                let node = node :?> MapExtImplementation.Inner<Index, 'Value>
                let sl = MapExtImplementation.count node.Left
                let si = start + sl
                x.Buffer.[si] <- node.Value
                x.Flatten(start, node.Left)
                x.Flatten(si + 1, node.Right)
           

        member x.Collect() = 
            if isNull x.Head then
                false
            elif x.Head.Height = 1uy then
                x.Next <- x.Head.Value
                if x.Tail.IsEmpty then
                    x.Head <- Unchecked.defaultof<_>
                    x.Tail <- []
                else
                    x.Head <- x.Tail.Head
                    x.Tail <- x.Tail.Tail
                true

            else
                let n = x.Head :?> MapExtImplementation.Inner<Index, 'Value>
                if typesize<'Value> <= 64 && n.Count <= 16 then
                    let sl = MapExtImplementation.count n.Left
                    if sl > 0 then
                        x.Flatten(0, n.Left)
                        x.Buffer.[sl] <- n.Value
                        x.Next <- x.Buffer.[0]
                    else
                        x.Next <- n.Value
                        
                    x.Flatten(sl+1, n.Right)

                    x.Index <- 1
                    x.BufferValueCount <- n.Count

                    if x.Tail.IsEmpty then
                        x.Head <- Unchecked.defaultof<_>
                        x.Tail <- []
                    else
                        x.Head <- x.Tail.Head
                        x.Tail <- x.Tail.Tail
                    true
                else
                    if isNull n.Left then
                        if isNull n.Right then
                            if x.Tail.IsEmpty then
                                x.Head <- Unchecked.defaultof<_>
                                x.Tail <- []
                            else
                                x.Head <- x.Tail.Head
                                x.Tail <- x.Tail.Tail
                        else
                            x.Head <- n.Right

                        x.Next <- n.Value
                        true
                    else
                        x.Head <- n.Left
                        if isNull n.Right then
                            x.Tail <- MapExtImplementation.Node(n.Key, n.Value) :: x.Tail
                        else
                            x.Tail <- MapExtImplementation.Node(n.Key, n.Value) :: n.Right :: x.Tail
                        x.Collect()


        member x.MoveNext() =
            if x.Index < x.BufferValueCount then
                x.Next <- x.Buffer.[x.Index]
                x.Index <- x.Index + 1
                true
            else
                x.Collect()

        member x.Current 
            with get() = x.Next
                
        member x.Dispose() = ()

        interface System.Collections.IEnumerator with
            member x.Current = x.Next :> obj
            member x.MoveNext() = x.MoveNext()
            member x.Reset() =
                x.Head <- x.Root
                x.Tail <- []
                x.Index <- -1
                x.BufferValueCount <- -1
                x.Next <- Unchecked.defaultof<_>
                    
        interface IEnumerator<'Value> with
            member x.Current = x.Next
            member x.Dispose() =
                x.Root <- Unchecked.defaultof<_>
                x.Head <- Unchecked.defaultof<_>
                x.Tail <- []
                x.Next <- Unchecked.defaultof<_>

        internal new(root : MapExtImplementation.Node<Index, 'Value>) =
            {
                Root = root
                Head = root
                Tail = []
                Index = -1
                BufferValueCount = -1
                Next = Unchecked.defaultof<_>
                Buffer =
                    if typesize<'Value> <= 64 then
                        if isNull root || root.Height = 1uy then null
                        else 
                            let cnt = (root :?> MapExtImplementation.Inner<Index, 'Value>).Count
                            Array.zeroCreate (min cnt 16)
                    else 
                        null
            }
    end


/// Functional operators for IndexList.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module IndexList =

    /// Internal utility for creating IndexLists from MapExt.
    let internal ofMap (m : MapExt<Index, 'T>) =
        if MapExt.isEmpty m then
            IndexList<'T>.Empty
        else
            let min = MapExt.minKey m
            let max = MapExt.maxKey m
            IndexList<'T>(min, max, m)
       

    /// the empty list.
    let empty<'T> = IndexList<'T>.Empty

    /// is the list empty?
    let inline isEmpty (list : IndexList<'T>) = 
        list.IsEmpty

    /// the number of entries in the list.
    let inline count (list : IndexList<'T>) = 
        list.Count

    /// adds an element at the end of the list.
    let inline add (v : 'T) (list : IndexList<'T>) = 
        list.Add v

    /// adds an element at the beginning of the list.
    let inline prepend (v : 'T) (list : IndexList<'T>) = 
        list.Prepend v

    /// updates or creates the element at the given index.
    let inline set (index : Index) (v : 'T) (list : IndexList<'T>) = 
        list.Set(index, v)

    /// removes the element at the given index. (if any)
    let inline remove (index : Index) (list : IndexList<'T>) = 
        list.Remove(index)

    /// Removes the entry associated to the given index, returns the (optional) value and the list without the specific element.
    let inline tryRemove (index : Index) (list : IndexList<'T>) = 
        list.TryRemove(index)

    /// Finds the optional neighbour elements in the list for the given index.
    let inline neighbours (index : Index) (list : IndexList<'T>) = 
        list.Neighbours(index)
        
    /// Gets an unused index directly after the given one.
    let inline newIndexAfter (index : Index) (list : IndexList<'T>) = 
        list.NewIndexAfter(index)

    /// Gets an unused index directly after the given one.
    let inline newIndexBefore (index : Index) (list : IndexList<'T>) = 
        list.NewIndexBefore(index)
        
    /// Gets the element directly after index in the list (if any).
    let inline tryGetNext (index : Index) (list : IndexList<'T>) = 
        list.TryGetNext(index)

    /// Gets the element directly before index in the list (if any).
    let inline tryGetPrev (index : Index) (list : IndexList<'T>) = 
        list.TryGetPrev(index)

    /// inserts an element directly after the given index.
    let inline insertAfter (index : Index) (value : 'T) (list : IndexList<'T>) = 
        list.InsertAfter(index, value)

    /// inserts an element directly before the given index.
    let inline insertBefore (index : Index) (value : 'T) (list : IndexList<'T>) = 
        list.InsertBefore(index, value)

    /// gets the element for the given index (if any).
    let inline tryGet (index : Index) (list : IndexList<'T>) = 
        list.TryGet index

    /// adds, deletes or updates the element for the given index.
    /// the update functions gets the optional old value and may optionally return
    /// a new value (or None for deleting the entry).
    let alter (index : Index) (mapping : option<'T> -> option<'T>) (l : IndexList<'T>) =
        // TODO: maintain min/max here
        MapExt.change index mapping l.Content |> ofMap

    /// updates the element at the given index (if any).
    let update (index : Index) (mapping : 'T -> 'T) (l : IndexList<'T>) =
        alter index (Option.map mapping) l

    /// splits the list at the given index and returns both (possibly empty) halves and (optional) splitting element.
    let split (index : Index) (list : IndexList<'T>) =
        let struct(l, s, r) = list.Content.SplitV(index)
        let lmax = MapExt.tryMaxKeyV l
        let rmin = MapExt.tryMinKeyV l

        let s =
            match s with
            | ValueSome v -> Some v
            | ValueNone -> None

        match struct(lmax, rmin) with
        | struct(ValueNone, ValueNone) -> 
            empty, s, empty

        | struct(ValueNone, ValueSome rmin) -> 
            empty, s, IndexList<'T>(rmin, list.MaxIndex, r)

        | struct(ValueSome lmax, ValueNone) ->
            IndexList<'T>(list.MinIndex, lmax, l), s, empty

        | struct(ValueSome lmax, ValueSome rmin) ->
            IndexList<'T>(list.MinIndex, lmax, l), s, IndexList<'T>(rmin, list.MaxIndex, r)

    /// updates or creates the element at the given index.
    /// note that out-of-bounds-indices will be ineffective.
    let inline setAt (index : int) (v : 'T) (list : IndexList<'T>) = 
        list.Set(index, v)

    /// removes the element at the given index.
    let inline removeAt (index : int) (list : IndexList<'T>) = 
        list.RemoveAt(index)
        
    /// inserts a new element at the given index.
    let inline insertAt (index : int) (value : 'T) (list : IndexList<'T>) = 
        list.InsertAt(index, value)

    /// gets the (optional) element for the index.
    let inline tryAt (index : int) (list : IndexList<'T>) = 
        list.TryGet index
        
    /// gets the position for the given index or None if the index is not contained in the list.
    let inline tryGetPosition (index : Index) (list : IndexList<'T>) =
        list.TryGetPosition index

    /// adds, deletes or updates the element for the given index.
    /// the update functions gets the optional old value and may optionally return
    /// a new value (or None for deleting the entry).
    let alterAt (i : int) (mapping : option<'T> -> option<'T>) (list : IndexList<'T>) =
        if i < -1 || i > list.Count then
            list
        else
            let l, s, r = MapExt.neighboursAt i list.Content
            match s with
            | Some (si, sv) ->
                match mapping (Some sv) with
                | Some r ->
                    IndexList<'T>(list.MinIndex, list.MaxIndex, MapExt.add si r list.Content)
                | None ->
                    let m = MapExt.remove si list.Content
                    let min = match l with | None -> MapExt.minKey m | Some _ -> list.MinIndex
                    let max = match r with | None -> MapExt.maxKey m | Some _ -> list.MaxIndex
                    IndexList<'T>(min, max, m)
            | None ->
                match mapping None with
                | Some res ->
                    let mutable minChanged = false
                    let mutable maxChanged = false
                    let idx =
                        match l, r with
                        | None, None -> 
                            minChanged <- true
                            maxChanged <- true
                            Index.zero
                        | Some (l,_), None -> 
                            maxChanged <- true
                            Index.after l
                        | None, Some (r,_) -> 
                            minChanged <- true
                            Index.before r
                        | Some (l,_), Some (r,_) -> 
                            Index.between l r

                    let min = if minChanged then idx else list.MinIndex
                    let max = if maxChanged then idx else list.MaxIndex
                    IndexList<'T>(min, max, MapExt.add idx res list.Content)
                | None ->
                    list

    /// updates the element at the given index (if any).
    let updateAt (index : int) (mapping : 'T -> 'T) (l : IndexList<'T>) =
        alterAt index (Option.map mapping) l

    /// splits the list at the given index and returns both (possibly empty) halves and (optional) splitting element.
    let splitAt (index : int) (list : IndexList<'T>) =
        if index < 0 then
            empty, None, list
        elif index >= list.Count then
            list, None, empty
        else
            let struct(index,_) = list.Content.GetItemV(index)
            split index list

      
        
    /// gets the optional min-index used by the list.
    let tryFirstIndex (list : IndexList<'T>) = list.Content.TryMinKey()

    /// gets the optional max-index used by the list.
    let tryLastIndex (list : IndexList<'T>) = list.Content.TryMaxKey()

    /// gets the min-index used by the list or fails if empty.
    let firstIndex (list : IndexList<'T>) = MapExt.minKey list.Content
    
    /// gets the max-index used by the list or fails if empty.
    let lastIndex (list : IndexList<'T>) = MapExt.maxKey list.Content

    /// gets the optional first element from the list.
    let tryFirst (list : IndexList<'T>) = list.Content.TryMinValue()

    /// gets the optional last element from the list.
    let tryLast (list : IndexList<'T>) = list.Content.TryMaxValue()

    
    /// gets the first element from the list or fails if empty.
    let first (list : IndexList<'T>) = tryFirst list |> Option.get

    /// gets the last element from the list or fails if empty.
    let last (list : IndexList<'T>) = tryLast list |> Option.get

    /// tries to get the index for the first occurrence of element in the list.
    let tryFindIndex (element : 'T) (list : IndexList<'T>) = 
        list.Content |> MapExt.tryPick (fun k v -> if v = element then Some k else None)
        
    /// tries to get the index for the last occurrence of element in the list.
    let tryFindIndexBack (element : 'T) (list : IndexList<'T>) = 
        list.Content |> MapExt.tryPickBack (fun k v -> if v = element then Some k else None)
        
    /// gets the index for the first occurrence of element in the list or fails if not existing.
    let findIndex (element : 'T) (list : IndexList<'T>) = 
        tryFindIndex element list |> Option.get

    /// gets the index for the last occurrence of element in the list or fails if not existing.
    let findIndexBack (element : 'T) (list : IndexList<'T>) = 
        tryFindIndexBack element list |> Option.get

    /// checks whether any element in the list fulfills the given predicate.
    let exists (predicate : Index -> 'T -> bool) (list : IndexList<'T>) = 
        list.Content |> MapExt.exists predicate

    /// checks if all elements in the list fulfill the given predicate.
    let forall (predicate : Index -> 'T -> bool) (list : IndexList<'T>) =   
        list.Content |> MapExt.forall predicate
    
    /// tries to find the first entry satisfying the predicate.
    let tryFind (predicate : Index -> 'T -> bool) (list : IndexList<'T>) = 
        list.Content |> MapExt.tryPick (fun k v -> if predicate k v then Some v else None)
        
    /// tries to find the last entry satisfying the predicate.
    let tryFindBack (predicate : Index -> 'T -> bool) (list : IndexList<'T>) = 
        list.Content |> MapExt.tryPickBack (fun k v -> if predicate k v then Some v else None)
        
    /// finds the first entry satisfying the predicate or fails if none.
    let find (predicate : Index -> 'T -> bool) (list : IndexList<'T>) = 
        tryFind predicate list |> Option.get
        
    /// finds the last entry satisfying the predicate or fails if none.
    let findBack (predicate : Index -> 'T -> bool) (list : IndexList<'T>) = 
        tryFindBack predicate list |> Option.get
    
    /// tries to pick the first Some - value.
    let tryPick (mapping : Index -> 'T1 -> option<'T2>) (list : IndexList<'T1>) = 
        list.Content |> MapExt.tryPick mapping

    /// tries to pick the last Some - value.
    let tryPickBack (mapping : Index -> 'T1 -> option<'T2>) (list : IndexList<'T1>) = 
        list.Content |> MapExt.tryPickBack mapping
    
    
    /// Returns a list of each element tupled with its successor.
    let inline pairwise (l : IndexList<'T>) =
        l.Pairwise()

    /// Returns a list of each element tupled with its successor and the last element tupled with the first.
    let inline pairwiseCyclic (l : IndexList<'T>) =
        l.PairwiseCyclic()

    /// splits a list of pairs into two lists.
    let unzip (l : IndexList<'T1 * 'T2>) =
        let mutable a = MapExt.empty
        let mutable b = MapExt.empty
        let mutable e = new MapExtEnumerator<_,_>(l.Content.Root)
        while e.MoveNext() do
            let kvp = e.Current
            let (va, vb) = kvp.Value
            a <- MapExt.add kvp.Key va a
            b <- MapExt.add kvp.Key vb b

        IndexList<'T1>(l.MinIndex, l.MaxIndex, a), 
        IndexList<'T2>(l.MinIndex, l.MaxIndex, b)
        
    /// splits a list of triples into three lists.
    let unzip3 (l : IndexList<'T1 * 'T2* 'T3>) =
        let mutable a = MapExt.empty
        let mutable b = MapExt.empty
        let mutable c = MapExt.empty
        let mutable e = new MapExtEnumerator<_,_>(l.Content.Root)
        while e.MoveNext() do
            let kvp = e.Current
            let (va, vb, vc) = kvp.Value
            a <- MapExt.add kvp.Key va a
            b <- MapExt.add kvp.Key vb b
            c <- MapExt.add kvp.Key vc c

        IndexList<'T1>(l.MinIndex, l.MaxIndex, a), 
        IndexList<'T2>(l.MinIndex, l.MaxIndex, b), 
        IndexList<'T3>(l.MinIndex, l.MaxIndex, c)

    /// concats the given lists.
    let append (l : IndexList<'T>) (r : IndexList<'T>) =
        if l.Count = 0 then r
        elif r.Count = 0 then l
        elif l.MaxIndex < r.MinIndex then
            IndexList<'T>(l.MinIndex, r.MaxIndex, MapExt.union l.Content r.Content)
            
        elif l.Count < r.Count then
            let mutable res = r
            for lv in l.AsSeqBackward do
                res <- res.Prepend(lv)
            res
        else
            let mutable res = l
            for rv in r.AsSeq do
                res <- res.Add(rv)
            res

    /// concats the given lists.
    let concat (s : #seq<IndexList<'T>>) =
        s |> Seq.fold append empty
        
    /// takes the first n elements from the list.
    let take (n : int) (list : IndexList<'T>) =
        if n <= 0 then empty
        elif n >= list.Count then list
        else 
            let c = list.Content.Take n
            IndexList<'T>(list.MinIndex, MapExt.maxKey c, c)

    /// skips the first n elements from the list.
    let skip (n : int) (list : IndexList<'T>) =
        if n <= 0 then list
        elif n >= list.Count then empty
        else
            let c = list.Content.Skip n
            IndexList<'T>(MapExt.minKey c, list.MaxIndex, c)

    /// creates a list containing a single element.
    let single (v : 'T) =
        let t = Index.after Index.zero
        IndexList(t, t, MapExt.ofList [t, v])

    /// all elements from the list with their respective Index.
    let toSeqIndexed (list: IndexList<'T>) = list.Content |> MapExt.toSeq

    /// all elements from the list with their respective Index.
    let toListIndexed (list: IndexList<'T>) = list.Content |> MapExt.toList

    /// all elements from the list with their respective Index.
    let toArrayIndexed (list: IndexList<'T>) = list.Content |> MapExt.toArray

    /// all elements from the list with their respective Index in reveresed order.
    let toSeqIndexedBack (list: IndexList<'T>) = list.Content |> MapExt.toSeqBack

    /// all elements from the list with their respective Index in reveresed order.
    let toListIndexedBack (list: IndexList<'T>) = list.Content |> MapExt.toSeqBack |> Seq.toList
    
    /// all elements from the list with their respective Index in reveresed order.
    let toArrayIndexedBack (list: IndexList<'T>) = list.Content |> MapExt.toSeqBack |> Seq.toArray

    /// creates a new IndexList containing all the given elements at their respective Index.
    let ofSeqIndexed (elements: seq<Index * 'T>) = MapExt.ofSeq elements |> ofMap

    /// creates a new IndexList containing all the given elements at their respective Index.
    let ofListIndexed (elements: list<Index * 'T>) = MapExt.ofList elements |> ofMap

    /// creates a new IndexList containing all the given elements at their respective Index.
    let ofArrayIndexed (elements: array<Index * 'T>) = MapExt.ofArray elements |> ofMap

    /// all elements from the list.
    let inline toSeq (list : IndexList<'T>) = list :> seq<_>

    /// all elements from the list.
    let inline toList (list : IndexList<'T>) = list.AsList

    /// all elements from the list.
    let inline toArray (list : IndexList<'T>) = list.AsArray
    
    /// all elements from the list in reversed order.
    let inline toSeqBack (list : IndexList<'T>) = 
        list.AsSeqBackward :> seq<_>

    /// all elements from the list in reversed order.
    let inline toListBack (list : IndexList<'T>) = 
        list.AsListBackward

    /// all elements from the list in reversed order.
    let inline toArrayBack (list : IndexList<'T>) = 
        list.AsArrayBackward

    /// creates a list from the given elements.
    let ofSeq (seq : seq<'T>) =
        let mutable res = empty
        for e in seq do res <- add e res
        res
        
    /// creates a list from the given elements.
    let inline ofList (list : list<'T>) = 
        let mutable res = empty
        for e in list do res <- add e res
        res
        
    /// creates a list from the given elements.
    let inline ofArray (arr : 'T[]) = 
        let mutable res = empty
        for e in arr do res <- add e res
        res

    /// applies the mapping function to all elements and concats the resulting lists.
    let collecti (mapping : Index -> 'T1 -> IndexList<'T2>) (l : IndexList<'T1>) = 
        let mapping = OptimizedClosures.FSharpFunc<Index, 'T1, IndexList<'T2>>.Adapt(mapping)
        let mutable e = l.Content.GetEnumerator()
        if e.MoveNext() then 
            let mutable res = mapping.Invoke(e.Current.Key, e.Current.Value)
            while e.MoveNext() do
                let v = mapping.Invoke(e.Current.Key, e.Current.Value)
                res <- append res v
            res
        else
            IndexList.Empty
        
    /// applies the mapping function to all elements and concats the resulting lists.
    let collect (mapping : 'T1 -> IndexList<'T2>) (l : IndexList<'T1>) = 
        let mutable e = l.GetEnumerator()
        if e.MoveNext() then 
            let mutable res = mapping e.Current
            while e.MoveNext() do
                let v = mapping e.Current
                res <- append res v
            res
        else
            IndexList.Empty

    /// applies the mapping function to all elements in the list.
    let inline mapi (mapping : Index -> 'T1 -> 'T2) (list : IndexList<'T1>) = 
        list.Map mapping

    /// applies the mapping function to all elements in the list.
    let inline map (mapping : 'T1 -> 'T2) (list : IndexList<'T1>) = 
        list.Map (fun _ v -> mapping v)
        
    /// Create a list of the given length using the given initializer
    let inline init len (initializer : int -> 'T) = 
        ofList (List.init len initializer)
        
    /// Create a list of the given length using the given initializer
    let inline range lowerBound upperBound = 
        ofList [ lowerBound .. upperBound]
        
    /// applies the mapping function to all elements in the list and drops None results.
    let inline choosei (mapping : Index -> 'T1 -> option<'T2>) (list : IndexList<'T1>) = 
        list.Choose mapping

    /// applies the mapping function to all elements in the list and drops None results.
    let inline choose (mapping : 'T1 -> option<'T2>) (list : IndexList<'T1>) = 
        list.Choose (fun _ v -> mapping v)
    
    /// Creates a new IndexList by applying the mapping function to all entries.
    /// The respective option-arguments are some whenever the left/right list has an entry for the current Index.
    /// Note that one of the options will always be some.
    let choose2 (mapping : Index -> option<'T1> -> option<'T2> -> option<'T3>) (l : IndexList<'T1>) (r : IndexList<'T2>) =
        MapExt.choose2 mapping l.Content r.Content |> ofMap

    /// filters the list using the given predicate.
    let inline filteri (predicate : Index -> 'T -> bool) (list : IndexList<'T>) = 
        list.Filter predicate
        
    /// filters the list using the given predicate.
    let inline filter (predicate : 'T -> bool) (list : IndexList<'T>) = 
        list.Filter (fun _ v -> predicate v)

    /// sorts the list by the given mapping.
    let sortByi (mapping : Index -> 'T1 -> 'T2) (l : IndexList<'T1>) =
        if l.Count <= 1 then
            l
        else
            let mapping = OptimizedClosures.FSharpFunc<Index, 'T1, 'T2>.Adapt mapping
            let arr = l.Content |> MapExt.toArrayV
            let sorted = Array.sortBy (fun struct (i,v) -> struct(mapping.Invoke(i, v), i)) arr
        
            let mutable res = MapExt.empty
            for i in 0 .. arr.Length - 1  do
                let struct(index, _) = arr.[i]
                let struct(_, value) = sorted.[i]
                res <- MapExt.add index value res

            IndexList<'T1>(l.MinIndex, l.MaxIndex, res)

    /// sorts the list by the given mapping.
    let sortBy (mapping : 'T1 -> 'T2) (l : IndexList<'T1>) =
        if l.Count <= 1 then
            l
        else
            let arr = l.Content |> MapExt.toArrayV
            let sorted = Array.sortBy (fun struct (i,v) -> struct(mapping v, i)) arr
        
            let mutable res = MapExt.empty
            for i in 0 .. arr.Length - 1  do
                let struct(index, _) = arr.[i]
                let struct(_, value) = sorted.[i]
                res <- MapExt.add index value res

            IndexList<'T1>(l.MinIndex, l.MaxIndex, res)

    /// sorts the list by the given mapping in descending order.
    let sortByDescendingi (mapping : Index -> 'T1 -> 'T2) (l : IndexList<'T1>) =
        if l.Count <= 1 then
            l
        else
            let mapping = OptimizedClosures.FSharpFunc<Index, 'T1, 'T2>.Adapt mapping
            let arr = l.Content |> MapExt.toArrayV
            let sorted = Array.sortBy (fun struct (i,v) -> struct(ReversedCompare(mapping.Invoke(i, v)), i)) arr
        
            let mutable res = MapExt.empty
            for i in 0 .. arr.Length - 1  do
                let struct(index, _) = arr.[i]
                let struct(_, value) = sorted.[i]
                res <- MapExt.add index value res

            IndexList<'T1>(l.MinIndex, l.MaxIndex, res)

    /// sorts the list by the given mapping in descending order.
    let sortByDescending (mapping : 'T1 -> 'T2) (l : IndexList<'T1>) =
        if l.Count <= 1 then
            l
        else
            let arr = l.Content |> MapExt.toArrayV
            let sorted = Array.sortBy (fun struct (i,v) -> struct(ReversedCompare(mapping v), i)) arr
        
            let mutable res = MapExt.empty
            for i in 0 .. arr.Length - 1  do
                let struct(index, _) = arr.[i]
                let struct(_, value) = sorted.[i]
                res <- MapExt.add index value res

            IndexList<'T1>(l.MinIndex, l.MaxIndex, res)
        
    /// sorts the list using the given compare function.
    let sortWith (cmp : 'T -> 'T -> int) (l : IndexList<'T>) =
        if l.Count <= 1 then
            l
        else
            let ic = Comparer<Index>.Default
            let cmp = OptimizedClosures.FSharpFunc<'T,'T,int>.Adapt(cmp)
            let inline comparer struct(li, lv) struct(ri, rv) =
                let c = cmp.Invoke(lv, rv)
                if c = 0 then ic.Compare(li, ri)
                else c

            let arr = l.Content |> MapExt.toArrayV
            let sorted = Array.sortWith comparer arr
        
            let mutable res = MapExt.empty
            for i in 0 .. arr.Length - 1  do
                let struct(index, _) = arr.[i]
                let struct(_, value) = sorted.[i]
                res <- MapExt.add index value res

            IndexList<'T>(l.MinIndex, l.MaxIndex, res)

    /// sorts the list.
    let sort<'T when 'T : comparison> (l : IndexList<'T>) =
        if l.Count <= 1 then
            l
        else
            let arr = l.Content |> MapExt.toArrayV
            let sorted = Array.sortBy (fun struct (i,v) -> struct(v, i)) arr
        
            let mutable res = MapExt.empty
            for i in 0 .. arr.Length - 1  do
                let struct(index, _) = arr.[i]
                let struct(_, value) = sorted.[i]
                res <- MapExt.add index value res

            IndexList<'T>(l.MinIndex, l.MaxIndex, res)
            
    /// sorts the list in descending order
    let sortDescending<'T when 'T : comparison> (l : IndexList<'T>) =
        if l.Count <= 1 then
            l
        else
            let arr = l.Content |> MapExt.toArrayV
            let sorted = Array.sortBy (fun struct (i,v) -> struct(ReversedCompare v, i)) arr
        
            let mutable res = MapExt.empty
            for i in 0 .. arr.Length - 1  do
                let struct(index, _) = arr.[i]
                let struct(_, value) = sorted.[i]
                res <- MapExt.add index value res

            IndexList<'T>(l.MinIndex, l.MaxIndex, res)

    /// reverses the given list.
    let rev (l : IndexList<'T>) =
        if l.Count <= 1 then
            l
        else
            let arr = MapExt.toArrayV l.Content
            let mutable res = MapExt.empty
            let mutable o = arr.Length - 1
            for i in 0 .. arr.Length - 1 do
                let struct(k, _) = arr.[i]
                let struct(_, v) = arr.[o]
                res <- MapExt.add k v res
                o <- o - 1
            IndexList<'T>(l.MinIndex, l.MaxIndex, res)


    /// folds over all list entries.
    let fold (folder : 'S -> 'T -> 'S) (seed : 'S) (l : IndexList<'T>) : 'S =
        let folder = OptimizedClosures.FSharpFunc<'S, 'T, 'S>.Adapt folder
        l.Content |> MapExt.fold (fun s _ v -> folder.Invoke(s, v)) seed
        
    /// invokes the given action for all list elements in list order.
    let iter (action : 'T -> unit) (l : IndexList<'T>) =
        l.Content |> MapExt.iterValue action
        
    /// invokes the given action for all list elements in list order.
    let iteri (action : Index -> 'T -> unit) (l : IndexList<'T>) =
        let action = OptimizedClosures.FSharpFunc<Index, 'T, unit>.Adapt action
        l.Content |> MapExt.iter (fun k v -> action.Invoke(k, v))
        
    /// calculates the sum of all elements in the list.
    let inline sum (l : IndexList<'T>) =
        fold (+) LanguagePrimitives.GenericZero l
        
    /// calculates the sum of all elements returned by mapping using the values from the list.
    let inline sumBy (mapping : 'T1 -> 'T2) (l : IndexList<'T1>) =
        (LanguagePrimitives.GenericZero, l) ||> fold (fun s v -> s + mapping v)
        
    /// calculates the average of all elements in the list.
    let inline average (l : IndexList<'T>) =
        let sum = fold (+) LanguagePrimitives.GenericZero l
        LanguagePrimitives.DivideByInt sum l.Count
        
    /// calculates the average of all elements returned by mapping using the values from the list.
    let inline averageBy (mapping : 'T1 -> 'T2) (l : IndexList<'T1>) =
        let sum = (LanguagePrimitives.GenericZero, l) ||> fold (fun s v -> s + mapping v)
        LanguagePrimitives.DivideByInt sum l.Count

