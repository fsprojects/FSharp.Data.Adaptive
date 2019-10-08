namespace FSharp.Data.Adaptive

open System
open System.Threading
open System.Collections
open System.Collections.Generic


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
    member x.IsEmpty = content.IsEmpty

    /// The number of entries in the list.
    member x.Count = content.Count

    /// Internal for getting the underlying store.
    member internal x.Content = content

    /// Gets the entry associated to the given index (if any).
    member x.TryGet (i : Index) =
        MapExt.tryFind i content
        
    /// Gets the entry at the given index (if any).
    member x.TryGet (i : int) =
        match MapExt.tryItem i content with
            | Some (_,v) -> Some v
            | None -> None

    /// Gets the entry associated to the given index or fails if not existing.
    member x.Item
        with get(i : Index) = MapExt.find i content
        
    /// Gets the entry at the given index or fails if not existing.
    member x.Item
        with get(i : int) = MapExt.item i content |> snd

    /// Appends the given element to the list.
    member x.Add(element : 'T) =
        if content.Count = 0 then
            let t = Index.after Index.zero
            IndexList(t, t, MapExt.ofList [t, element])
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
            IndexList(index, index, MapExt.ofList [index, value])

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
            match MapExt.tryItem index content with
                | Some (id,_) -> x.Set(id, value)
                | None -> x

    /// Updates the element at the given position or returns the unmodified list if the index was out of bounds.
    member x.Update(index : int, update : 'T -> 'T) =
        match MapExt.tryItem index content with
            | Some (id,v) -> 
                let newContent = MapExt.add id (update v) content
                IndexList(l, h, newContent)
            | None -> 
                x

    /// Inserts the element at the given position or returns the unmodified list if the index is not in [0..count].
    /// Note that InsertAt works with index = count.
    member x.InsertAt(index : int, value : 'T) =
        if index < 0 || index > content.Count then
            x
        else
            let l, s, r = MapExt.neighboursAt index content

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
            x.Set(index, value)

    /// Inserts the element directly before the given index.
    member x.InsertBefore(index : Index, value : 'T) =
        let l, s, _r = MapExt.neighbours index content
        match s with
            | None ->
                x.Set(index, value)
            | Some (s, _) ->
                let index = 
                    match l with
                    | Some (l,_) -> Index.between l s
                    | None -> Index.before index
                x.Set(index, value)
                
    /// Inserts the element directly after the given index.
    member x.InsertAfter(index : Index, value : 'T) =
        let _l, s, r = MapExt.neighbours index content
        match s with
            | None ->
                x.Set(index, value)
            | Some (s, _) ->
                let index =
                    match r with
                        | Some (r,_) -> Index.between s r
                        | None -> Index.after index
                x.Set(index, value)

    /// Gets the index for the given position or None if the index is out of bounds.
    member x.TryGetIndex(index : int) =
        match MapExt.tryItem index content with
            | Some (id,_) -> Some id
            | None -> None

    /// Removes the entry associated to the given index.
    member x.Remove(index : Index) =
        let c = MapExt.remove index content
        if c.Count = 0 then empty
        elif l = index then IndexList(MapExt.min c, h, c)
        elif h = index then IndexList(l, MapExt.max c, c)
        else IndexList(l, h, c)
        
    /// Removes the entry at the given position (if any).
    member x.RemoveAt(index : int) =
        match MapExt.tryItem index content with
            | Some (id, _) -> x.Remove id
            | _ -> x

    /// Applies the mapping function to all elements of the list and returns a new list containing the results.
    member x.Map<'T2>(mapping : Index -> 'T -> 'T2) : IndexList<'T2> =
        IndexList(l, h, MapExt.map mapping content)
        
    /// Applies the mapping function to all elements of the list and returns a new list containing all Some entries.
    member x.Choose(mapping : Index -> 'T -> option<'T2>) =
        let res = MapExt.choose mapping content
        if res.IsEmpty then 
            IndexList.Empty
        else
            IndexList(MapExt.min res, MapExt.max res, res)

    /// Filters the list using the given predicate.
    member x.Filter(predicate : Index -> 'T -> bool) =
        let res = MapExt.filter predicate content
        if res.IsEmpty then 
            IndexList.Empty
        else
            IndexList(MapExt.min res, MapExt.max res, res)

    /// Tries to find the smallest index for the given element.
    member x.TryFind(element : 'T) : option<Index> =
        match content |> MapExt.toSeq |> Seq.tryFind (fun (k,v) -> Unchecked.equals v element) with
        | Some (k, v) -> Some k
        | _ -> None

    /// Removes the first occurrence of the given element (if any).
    member x.Remove(item : 'T) : IndexList<'T> =
        match x.TryFind(item) with
        | Some index -> x.Remove(index)
        | None -> x
          
    /// Returns all entres from the list in back-to-front order.
    member x.AsSeqBackward =
        content |> MapExt.toSeqBack |> Seq.map snd
        
    /// Returns all entres from the list in back-to-front order.
    member x.AsListBackward =
        x.AsSeqBackward |> Seq.toList
        
    /// Returns all entres from the list in back-to-front order.
    member x.AsArrayBackward =
        x.AsSeqBackward |> Seq.toArray
        
    /// Returns all entres from the list.
    member x.AsSeq =
        content |> MapExt.toSeq |> Seq.map snd
        
    /// Returns all entres from the list.
    member x.AsList =
        content |> MapExt.toList |> List.map snd
        
    /// Returns all entres from the list.
    member x.AsArray =
        content |> MapExt.toArray |> Array.map snd
        
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
        let mutable i = dstIndex
        content |> MapExt.iter (fun k v -> dst.[i] <- v; i <- i + 1)

    /// Tries to find the position for the given entry or -1 if the entry does not exist. O(N)
    member x.IndexOf(item : 'T) =
        x |> Seq.tryFindIndex (Unchecked.equals item) |> Option.defaultValue -1

    /// Tries to find the position for the given Index or -1 if the Index does not exist. O(N)
    member x.IndexOf(index : Index) =
        MapExt.tryIndexOf index content |> Option.defaultValue -1
        
    interface ICollection<'T> with 
        member x.Add(v) = raise (NotSupportedException("IndexList cannot be mutated"))
        member x.Clear() = raise (NotSupportedException("IndexList cannot be mutated"))
        member x.Remove(v) = raise (NotSupportedException("IndexList cannot be mutated"))
        member x.Contains(v) = content |> MapExt.exists (fun _ vi -> Unchecked.equals vi v)
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

    interface IEnumerable with
        member x.GetEnumerator() = new IndexListEnumerator<'T>(content :> seq<_>) :> _

    interface IEnumerable<'T> with
        member x.GetEnumerator() = new IndexListEnumerator<'T>(content :> seq<_>) :> _

/// Enumerator for IndexList.
and private IndexListEnumerator<'T>(content : IEnumerable<KeyValuePair<Index, 'T>>) =
    let r = content.GetEnumerator()

    member x.Current =
        r.Current.Value

    interface IEnumerator with
        member x.MoveNext() = r.MoveNext()
        member x.Current = x.Current :> obj
        member x.Reset() = r.Reset()

    interface IEnumerator<'T> with
        member x.Current = x.Current
        member x.Dispose() = r.Dispose()

/// Functional operators for IndexList.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module IndexList =

    /// Internal utility for creating IndexLists from MapExt.
    let internal ofMap (m : MapExt<Index, 'T>) =
        if MapExt.isEmpty m then
            IndexList<'T>.Empty
        else
            let min = m.TryMinKey
            let max = m.TryMaxKey
            IndexList<'T>(min.Value, max.Value, m)
       

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
        MapExt.alter index mapping l.Content |> ofMap

    /// updates the element at the given index (if any).
    let update (index : Index) (mapping : 'T -> 'T) (l : IndexList<'T>) =
        alter index (Option.map mapping) l

    /// splits the list at the given index and returns both (possibly empty) halves and (optional) splitting element.
    let split (index : Index) (list : IndexList<'T>) =
        let (l,s,r) = list.Content.Split(index)

        match MapExt.isEmpty l, MapExt.isEmpty r with
        | true, true -> empty, s, empty
        | true, false -> 
            let rmin = r.TryMinKey |> Option.defaultValue Index.zero
            empty, s, IndexList<'T>(rmin, list.MaxIndex, r)
        | false, true ->
            let lmax = l.TryMaxKey |> Option.defaultValue Index.zero
            IndexList<'T>(list.MinIndex, lmax, l), s, empty
        | false, false ->
            let lmax = l.TryMaxKey |> Option.defaultValue Index.zero
            let rmin = r.TryMinKey |> Option.defaultValue Index.zero
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
                    let min = match l with | None -> MapExt.tryMin m |> Option.get | Some _ -> list.MinIndex
                    let max = match r with | None -> MapExt.tryMax m |> Option.get | Some _ -> list.MaxIndex
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
            let index,_ = list.Content.TryAt(index) |> Option.get
            split index list

      
        
    /// gets the optional min-index used by the list.
    let tryFirstIndex (list : IndexList<'T>) = list.Content.TryMinKey

    /// gets the optional max-index used by the list.
    let tryLastIndex (list : IndexList<'T>) = list.Content.TryMaxKey

    /// gets the min-index used by the list or fails if empty.
    let firstIndex (list : IndexList<'T>) = tryFirstIndex list |> Option.get
    
    /// gets the max-index used by the list or fails if empty.
    let lastIndex (list : IndexList<'T>) = tryLastIndex list |> Option.get

    /// gets the optional first element from the list.
    let tryFirst (list : IndexList<'T>) = list.Content.TryMinValue

    /// gets the optional last element from the list.
    let tryLast (list : IndexList<'T>) = list.Content.TryMaxValue

    
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
    let exists (f : Index -> 'T -> bool) (list : IndexList<'T>) = 
        list.Content |> Seq.exists (fun kv -> f kv.Key kv.Value)

    /// checks if all elements in the list fulfill the given predicate.
    let forall (f : Index -> 'T -> bool) (list : IndexList<'T>) =   
        list.Content |> Seq.forall (fun kv -> f kv.Key kv.Value)
    
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
        elif n > list.Count then list
        else
            let l,_,_ = splitAt n list
            l

    /// skips the first n elements from the list.
    let skip (n : int) (list : IndexList<'T>) =
        if n <= 0 then list
        elif n > list.Count then empty
        else
            let _,_,r = splitAt (n - 1) list
            r

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
        ofSeq list
        
    /// creates a list from the given elements.
    let inline ofArray (arr : 'T[]) = 
        ofSeq arr

    /// applies the mapping function to all elements and concats the resulting lists.
    let collecti (mapping : Index -> 'T1 -> IndexList<'T2>) (l : IndexList<'T1>) = 
        use e = (l.Content :> seq<_>).GetEnumerator()
        if e.MoveNext() then 
            let mutable res = mapping e.Current.Key e.Current.Value
            while e.MoveNext() do
                let v = mapping e.Current.Key e.Current.Value
                res <- append res v
            res
        else
            IndexList.Empty
        
    /// applies the mapping function to all elements and concats the resulting lists.
    let collect (mapping : 'T1 -> IndexList<'T2>) (l : IndexList<'T1>) = 
        collecti (fun _ v -> mapping v) l

    /// applies the mapping function to all elements in the list.
    let inline mapi (mapping : Index -> 'T1 -> 'T2) (list : IndexList<'T1>) = 
        list.Map mapping

    /// applies the mapping function to all elements in the list.
    let inline map (mapping : 'T1 -> 'T2) (list : IndexList<'T1>) = 
        list.Map (fun _ v -> mapping v)
        
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
        let arr = l.Content |> MapExt.toArray
        Array.sortInPlaceBy (fun (i,v) -> mapping i v, i) arr
        ofArray (Array.map snd arr)

    /// sorts the list by the given mapping.
    let sortBy (mapping : 'T1 -> 'T2) (l : IndexList<'T1>) =
        let arr = l.Content |> MapExt.toArray
        Array.sortInPlaceBy (fun (i, v) -> mapping v, i) arr
        ofArray (Array.map snd arr)
        
    /// sorts the list using the given compare function.
    let sortWith (cmp : 'T -> 'T -> int) (l : IndexList<'T>) =
        let arr = l.Content |> MapExt.toArray
        let cmp (li: Index, lv: 'T) (ri: Index, rv: 'T) =
            let c = cmp lv rv
            if c = 0 then compare li ri
            else c

        Array.sortInPlaceWith cmp arr
        ofArray (Array.map snd arr)
        