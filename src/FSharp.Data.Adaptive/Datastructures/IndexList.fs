namespace FSharp.Data.Adaptive

open System
open System.Threading
open System.Collections
open System.Collections.Generic


[<Struct; StructuralEquality; NoComparison>]
[<StructuredFormatDisplay("{AsString}")>]
type IndexList< [<EqualityConditionalOn>] 'T> internal(l : Index, h : Index, content : MapExt<Index, 'T>) =
    
    static let empty = IndexList<'T>(Index.zero, Index.zero, MapExt.empty)


    static member Empty = empty


    member x.MinIndex = l
    member x.MaxIndex = h

    member x.IsEmpty = content.IsEmpty
    member x.Count = content.Count

    member internal x.Content = content

    member x.TryGet (i : Index) =
        MapExt.tryFind i content
        
    member x.TryGet (i : int) =
        match MapExt.tryItem i content with
            | Some (_,v) -> Some v
            | None -> None

    member x.Item
        with get(i : Index) = MapExt.find i content
        
    // O(log(n))
    member x.Item
        with get(i : int) = MapExt.item i content |> snd

    member x.Add(v : 'T) =
        if content.Count = 0 then
            let t = Index.after Index.zero
            IndexList(t, t, MapExt.ofList [t, v])
        else
            let t = Index.after h
            IndexList(l, t, MapExt.add t v content)
        
    member x.Prepend(v : 'T) =
        if content.Count = 0 then
            let t = Index.after Index.zero
            IndexList(t, t, MapExt.ofList [t, v])
        else
            let t = Index.before l
            IndexList(t, h, MapExt.add t v content)

    member x.Set(key : Index, value : 'T) =
        if content.Count = 0 then
            IndexList(key, key, MapExt.ofList [key, value])

        elif key < l then
            IndexList(key, h, MapExt.add key value content)

        elif key > h then
            IndexList(l, key, MapExt.add key value content)

        else 
            IndexList(l, h, MapExt.add key value content)

    member x.Set(i : int, value : 'T) =
        match MapExt.tryItem i content with
            | Some (id,_) -> x.Set(id, value)
            | None -> x

    member x.Update(i : int, f : 'T -> 'T) =
        match MapExt.tryItem i content with
            | Some (id,v) -> 
                let newContent = MapExt.add id (f v) content
                IndexList(l, h, newContent)
            | None -> 
                x

    member x.InsertAt(i : int, value : 'T) =
        if i < 0 || i > content.Count then
            x
        else
            let l, s, r = MapExt.neighboursAt i content

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

    member x.InsertBefore(i : Index, value : 'T) =
        let str = Guid.NewGuid() |> string
        let l, s, r = MapExt.neighbours i content
        match s with
            | None ->
                x.Set(i, value)
            | Some _ ->
                let index = 
                    match l with
                        | Some (before,_) -> Index.between before i
                        | None -> Index.before i
                x.Set(index, value)

    member x.InsertAfter(i : Index, value : 'T) =
        let str = Guid.NewGuid() |> string
        let l, s, r = MapExt.neighbours i content
        match s with
            | None ->
                x.Set(i, value)
            | Some _ ->
                let index =
                    match r with
                        | Some (after,_) -> Index.between i after
                        | None -> Index.after i
                x.Set(index, value)

    member x.TryGetIndex(i : int) =
        match MapExt.tryItem i content with
            | Some (id,_) -> Some id
            | None -> None

    member x.Remove(key : Index) =
        let c = MapExt.remove key content
        if c.Count = 0 then empty
        elif l = key then IndexList(MapExt.min c, h, c)
        elif h = key then IndexList(l, MapExt.max c, c)
        else IndexList(l, h, c)

    member x.RemoveAt(i : int) =
        match MapExt.tryItem i content with
            | Some (id, _) -> x.Remove id
            | _ -> x

    member x.Map<'b>(mapping : Index -> 'T -> 'b) : IndexList<'b> =
        IndexList(l, h, MapExt.map mapping content)
        
    member x.Choose(mapping : Index -> 'T -> Option<'b>) =
        let res = MapExt.choose mapping content
        if res.IsEmpty then 
            IndexList.Empty
        else
            IndexList(MapExt.min res, MapExt.max res, res)

    member x.Filter(predicate : Index -> 'T -> bool) =
        let res = MapExt.filter predicate content
        if res.IsEmpty then 
            IndexList.Empty
        else
            IndexList(MapExt.min res, MapExt.max res, res)

    // O(n)
    member x.TryFind(item : 'T) : Option<Index> =
        match content |> MapExt.toSeq |> Seq.tryFind (fun (k,v) -> Unchecked.equals v item) with
        | Some (k, v) -> Some k
        | _ -> None

    // O(n)
    member x.Remove(item : 'T) : IndexList<'T> =
        match x.TryFind(item) with
        | Some index -> x.Remove(index)
        | None -> x
          
    member x.AsSeqBackward =
        content |> MapExt.toSeqBack |> Seq.map snd
        
    member x.AsListBackward =
        x.AsSeqBackward |> Seq.toList

    member x.AsArrayBackward =
        x.AsSeqBackward |> Seq.toArray

    member x.AsSeq =
        content |> MapExt.toSeq |> Seq.map snd

    member x.AsList =
        content |> MapExt.toList |> List.map snd

    member x.AsArray =
        content |> MapExt.toArray |> Array.map snd
        

    override x.ToString() =
        content |> MapExt.toSeq |> Seq.map (snd >> sprintf "%A") |> String.concat "; " |> sprintf "IndexList [%s]"

    member private x.AsString = x.ToString()
    
    member x.CopyTo(arr : 'T[], i : int) = 
        let mutable i = i
        content |> MapExt.iter (fun k v -> arr.[i] <- v; i <- i + 1)

    member x.IndexOf(item : 'T) =
        x |> Seq.tryFindIndex (Unchecked.equals item) |> Option.defaultValue -1

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

/// functional operators for IndexList.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module IndexList =

    let internal ofMap (m : MapExt<Index, 'a>) =
        if MapExt.isEmpty m then
            IndexList<'a>.Empty
        else
            let min = m.TryMinKey
            let max = m.TryMaxKey
            IndexList<'a>(min.Value, max.Value, m)
       

    /// the empty list.
    let empty<'T> = IndexList<'T>.Empty

    /// is the list empty?
    let inline isEmpty (list : IndexList<'a>) = 
        list.IsEmpty

    /// the number of entries in the list.
    let inline count (list : IndexList<'a>) = 
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
    let alter (index : Index) (mapping : Option<'a> -> Option<'a>) (l : IndexList<'a>) =
        MapExt.alter index mapping l.Content |> ofMap

    /// updates the element at the given index (if any).
    let update (index : Index) (mapping : 'a -> 'a) (l : IndexList<'a>) =
        alter index (Option.map mapping) l

    /// splits the list at the given index and returns both (possibly empty) halves and (optional) splitting element.
    let split (index : Index) (list : IndexList<'a>) =
        let (l,s,r) = list.Content.Split(index)

        match MapExt.isEmpty l, MapExt.isEmpty r with
        | true, true -> empty, s, empty
        | true, false -> 
            let rmin = r.TryMinKey |> Option.defaultValue Index.zero
            empty, s, IndexList<'a>(rmin, list.MaxIndex, r)
        | false, true ->
            let lmax = l.TryMaxKey |> Option.defaultValue Index.zero
            IndexList<'a>(list.MinIndex, lmax, l), s, empty
        | false, false ->
            let lmax = l.TryMaxKey |> Option.defaultValue Index.zero
            let rmin = r.TryMinKey |> Option.defaultValue Index.zero
            IndexList<'a>(list.MinIndex, lmax, l), s, IndexList<'a>(rmin, list.MaxIndex, r)

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
    let alterAt (i : int) (mapping : Option<'a> -> Option<'a>) (list : IndexList<'a>) =
        if i < -1 || i > list.Count then
            list
        else
            let l, s, r = MapExt.neighboursAt i list.Content
            match s with
            | Some (si, sv) ->
                match mapping (Some sv) with
                | Some r ->
                    IndexList<'a>(list.MinIndex, list.MaxIndex, MapExt.add si r list.Content)
                | None ->
                    let m = MapExt.remove si list.Content
                    let min = match l with | None -> MapExt.tryMin m |> Option.get | Some _ -> list.MinIndex
                    let max = match r with | None -> MapExt.tryMax m |> Option.get | Some _ -> list.MaxIndex
                    IndexList<'a>(min, max, m)
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
                    IndexList<'a>(min, max, MapExt.add idx res list.Content)
                | None ->
                    list

    /// updates the element at the given index (if any).
    let updateAt (index : int) (mapping : 'a -> 'a) (l : IndexList<'a>) =
        alterAt index (Option.map mapping) l

    /// splits the list at the given index and returns both (possibly empty) halves and (optional) splitting element.
    let splitAt (index : int) (list : IndexList<'a>) =
        if index < 0 then
            empty, None, list
        elif index >= list.Count then
            list, None, empty
        else
            let index,_ = list.Content.TryAt(index) |> Option.get
            split index list

      
        
    /// gets the optional min-index used by the list.
    let inline tryFirstIndex (list : IndexList<'T>) = list.Content.TryMinKey

    /// gets the optional max-index used by the list.
    let inline tryLastIndex (list : IndexList<'a>) = list.Content.TryMaxKey

    /// gets the min-index used by the list or fails if empty.
    let inline firstIndex (list : IndexList<'a>) = tryFirstIndex list |> Option.get
    
    /// gets the max-index used by the list or fails if empty.
    let inline lastIndex (list : IndexList<'a>) = tryLastIndex list |> Option.get

    /// gets the optional first element from the list.
    let inline tryFirst (list : IndexList<'a>) = list.Content.TryMinValue

    /// gets the optional last element from the list.
    let inline tryLast (list : IndexList<'a>) = list.Content.TryMaxValue

    
    /// gets the first element from the list or fails if empty.
    let inline first (list : IndexList<'a>) = tryFirst list |> Option.get

    /// gets the last element from the list or fails if empty.
    let inline last (list : IndexList<'a>) = tryLast list |> Option.get

    /// tries to get the index for the first occurrence of element in the list.
    let tryFindIndex (element : 'a) (list : IndexList<'a>) = 
        list.Content |> MapExt.tryPick (fun k v -> if v = element then Some k else None)
        
    /// tries to get the index for the last occurrence of element in the list.
    let tryFindIndexBack (element : 'a) (list : IndexList<'a>) = 
        list.Content |> MapExt.tryPickBack (fun k v -> if v = element then Some k else None)
        
    /// gets the index for the first occurrence of element in the list or fails if not existing.
    let findIndex (element : 'a) (list : IndexList<'a>) = 
        tryFindIndex element list |> Option.get

    /// gets the index for the last occurrence of element in the list or fails if not existing.
    let findIndexBack (element : 'a) (list : IndexList<'a>) = 
        tryFindIndexBack element list |> Option.get

    /// checks whether any element in the list fulfills the given predicate.
    let exists (f : Index -> 'a -> bool) (list : IndexList<'a>) = 
        list.Content |> Seq.exists (fun kv -> f kv.Key kv.Value)

    /// checks if all elements in the list fulfill the given predicate.
    let forall (f : Index -> 'a -> bool) (list : IndexList<'a>) =   
        list.Content |> Seq.forall (fun kv -> f kv.Key kv.Value)
    
    /// tries to find the first entry satisfying the predicate.
    let tryFind (predicate : Index -> 'a -> bool) (list : IndexList<'a>) = 
        list.Content |> MapExt.tryPick (fun k v -> if predicate k v then Some v else None)
        
    /// tries to find the last entry satisfying the predicate.
    let tryFindBack (predicate : Index -> 'a -> bool) (list : IndexList<'a>) = 
        list.Content |> MapExt.tryPickBack (fun k v -> if predicate k v then Some v else None)
        
    /// finds the first entry satisfying the predicate or fails if none.
    let find (predicate : Index -> 'a -> bool) (list : IndexList<'a>) = 
        tryFind predicate list |> Option.get
        
    /// finds the last entry satisfying the predicate or fails if none.
    let findBack (predicate : Index -> 'a -> bool) (list : IndexList<'a>) = 
        tryFindBack predicate list |> Option.get
    
    /// tries to pick the first Some - value.
    let tryPick (mapping : Index -> 'a -> Option<'b>) (list : IndexList<'a>) = 
        list.Content |> MapExt.tryPick mapping

    /// tries to pick the last Some - value.
    let tryPickBack (mapping : Index -> 'a -> Option<'b>) (list : IndexList<'a>) = 
        list.Content |> MapExt.tryPickBack mapping
    
    /// concats the given lists.
    let append (l : IndexList<'a>) (r : IndexList<'a>) =
        if l.Count = 0 then r
        elif r.Count = 0 then l
        elif l.MaxIndex < r.MinIndex then
            IndexList<'a>(l.MinIndex, r.MaxIndex, MapExt.union l.Content r.Content)
            
        elif r.MaxIndex < l.MinIndex then
            IndexList<'a>(r.MinIndex, l.MaxIndex, MapExt.union l.Content r.Content)

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
    let concat (s : #seq<IndexList<'a>>) =
        s |> Seq.fold append empty
        
    /// takes the first n elements from the list.
    let take (n : int) (list : IndexList<'a>) =
        if n <= 0 then empty
        elif n > list.Count then list
        else
            let l,_,_ = splitAt n list
            l

    /// skips the first n elements from the list.
    let skip (n : int) (list : IndexList<'a>) =
        if n <= 0 then list
        elif n > list.Count then empty
        else
            let _,_,r = splitAt (n - 1) list
            r

    /// creates a list containing a single element.
    let single (v : 'a) =
        let t = Index.after Index.zero
        IndexList(t, t, MapExt.ofList [t, v])

    /// all elements from the list.
    let inline toSeq (list : IndexList<'a>) = list :> seq<_>

    /// all elements from the list.
    let inline toList (list : IndexList<'a>) = list.AsList

    /// all elements from the list.
    let inline toArray (list : IndexList<'a>) = list.AsArray
    
    /// all elements from the list in reversed order.
    let inline toSeqBack (list : IndexList<'a>) = 
        list.AsSeqBackward :> seq<_>

    /// all elements from the list in reversed order.
    let inline toListBack (list : IndexList<'a>) = 
        list.AsListBackward

    /// all elements from the list in reversed order.
    let inline toArrayBack (list : IndexList<'a>) = 
        list.AsArrayBackward

    /// creates a list from the given elements.
    let ofSeq (seq : seq<'a>) =
        let mutable res = empty
        for e in seq do res <- add e res
        res
        
    /// creates a list from the given elements.
    let inline ofList (list : list<'a>) = 
        ofSeq list
        
    /// creates a list from the given elements.
    let inline ofArray (arr : 'a[]) = 
        ofSeq arr

    /// applies the mapping function to all elements and concats the resulting lists.
    let collecti (mapping : Index -> 'a -> IndexList<'b>) (l : IndexList<'a>) = 
        l.Map(mapping) |> concat
        
    /// applies the mapping function to all elements and concats the resulting lists.
    let collect (mapping : 'a -> IndexList<'b>) (l : IndexList<'a>) = 
        l.Map(fun _ v -> mapping v) |> concat

    /// applies the mapping function to all elements in the list.
    let inline mapi (mapping : Index -> 'a -> 'b) (list : IndexList<'a>) = 
        list.Map mapping

    /// applies the mapping function to all elements in the list.
    let inline map (mapping : 'a -> 'b) (list : IndexList<'a>) = 
        list.Map (fun _ v -> mapping v)
        
    /// applies the mapping function to all elements in the list and drops None results.
    let inline choosei (mapping : Index -> 'a -> Option<'b>) (list : IndexList<'a>) = 
        list.Choose mapping

    /// applies the mapping function to all elements in the list and drops None results.
    let inline choose (mapping : 'a -> Option<'b>) (list : IndexList<'a>) = 
        list.Choose (fun _ v -> mapping v)
    
    /// filters the list using the given predicate.
    let inline filteri (predicate : Index -> 'a -> bool) (list : IndexList<'a>) = 
        list.Filter predicate
        
    /// filters the list using the given predicate.
    let inline filter (predicate : 'a -> bool) (list : IndexList<'a>) = 
        list.Filter (fun _ v -> predicate v)

    /// sorts the list by the given mapping.
    let sortBy (mapping : 'a -> 'b) (l : IndexList<'a>) =
        let arr = l.AsArray
        Array.sortInPlaceBy mapping arr
        ofArray arr
        
    /// sorts the list using the given compare function.
    let sortWith (compare : 'a -> 'a -> int) (l : IndexList<'a>) =
        let arr = l.AsArray
        Array.sortInPlaceWith compare arr
        ofArray arr
