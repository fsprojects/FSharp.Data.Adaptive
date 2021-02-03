namespace FSharp.Data.Adaptive

open System.Collections
open System.Collections.Generic

/// Represents the difference of two IndexLists.
[<StructuredFormatDisplay("{AsString}")>]
[<Struct; StructuralEquality; NoComparison>]
type IndexListDelta< [<EqualityConditionalOn>] 'T> internal(content : MapExt<Index, ElementOperation<'T>>) =
    static let empty = IndexListDelta<'T>(MapExt.empty)

    /// Internally used for getting the underlying store.
    member internal x.Content = content

    /// The empty delta.
    static member Empty = empty

    /// The number of deltas in the list.
    member x.Count = content.Count

    /// Is the list empty?
    member x.IsEmpty = content.IsEmpty

    /// Inserts an operation.
    member x.Add(index : Index, op : ElementOperation<'T>) =
        IndexListDelta(MapExt.add index op content)

    /// Removes the operation associated to index. (if any)
    member x.Remove(index : Index) =
        IndexListDelta(MapExt.remove index content)

    /// Returns all Index/Operation tuples from the list.
    member x.ToSeq() = content |> MapExt.toSeq

    /// Returns all Index/Operation tuples from the list.
    member x.ToList() = content |> MapExt.toList

    /// Returns all Index/Operation tuples from the list.
    member x.ToArray() = content |> MapExt.toArray

    /// Combines to IndexListDeltas to one.
    member x.Combine(r : IndexListDelta<'T>) =
        if x.IsEmpty then r
        elif r.IsEmpty then x
        else MapExt.union x.Content r.Content |> IndexListDelta

    /// Applies the given mapping function to all deltas in the list and returns a new list containing the results.
    member x.Map(mapping : Index -> ElementOperation<'T> -> ElementOperation<'T2>) =
        IndexListDelta(MapExt.map mapping content)

    /// Applies the given mapping function to all deltas in the list and returns a new list containing the 'Some'-results.
    member x.Choose(mapping : Index -> ElementOperation<'T> -> option<ElementOperation<'T2>>) =
        IndexListDelta(MapExt.choose mapping content)

    /// Applies the given mapping function to all deltas in the list and returns a new list containing the 'Some'-results.
    /// Note that the indices need to be monotonic.
    member x.MapMonotonic(mapping : Index -> ElementOperation<'T> -> Index * ElementOperation<'T2>) =
        IndexListDelta(content.MapMonotonic mapping)
        
    ///// Applies the given mapping function to all deltas in the list and returns a new list containing the 'Some'-results.
    ///// Note that the indices need to be monotonic.
    //member x.ChooseMonotonic(mapping : Index -> ElementOperation<'T> -> option<Index * ElementOperation<'T2>>) =
    //    IndexListDelta(MapExt.chooseMonotonic mapping content)

    /// Filters the delta list using the given predicate.
    member x.Filter(mapping : Index -> ElementOperation<'T> -> bool) =
        IndexListDelta(MapExt.filter mapping content)
        
    override x.ToString() =
        let suffix =
            if content.Count > 5 then "; ..."
            else ""
        
        let content =
            content |> Seq.truncate 5 |> Seq.map (fun (KeyValue(i,op)) ->
                match op with
                    | Set v -> sprintf "[%A]<-%A" i v
                    | Remove -> sprintf "Rem(%A)" i
            ) |> String.concat "; "

        "IndexListDelta [" + content + suffix + "]"

    member private x.AsString = x.ToString()

    /// Applies the mapping function to all elements and combines the resulting lists.
    member x.Collect (mapping : Index -> ElementOperation<'T> -> IndexListDelta<'T2>) =
        let mutable res = IndexListDelta<'T2>.Empty
        for (KeyValue(i,v)) in content do
            res <- res.Combine(mapping i v)
        res

    member x.GetEnumerator() = new IndexListDeltaEnumerator<'T>(x.Content.Root)

    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = new IndexListDeltaEnumerator<'T>(x.Content.Root) :> _

    interface System.Collections.Generic.IEnumerable<Index * ElementOperation<'T>> with
        member x.GetEnumerator() = new IndexListDeltaEnumerator<'T>(x.Content.Root) :> _

and IndexListDeltaEnumerator<'T> =
    struct
        val mutable internal Root : MapExtImplementation.Node<Index, ElementOperation<'T>>
        val mutable internal Head : struct(MapExtImplementation.Node<Index, ElementOperation<'T>> * bool)
        val mutable internal Tail : list<struct(MapExtImplementation.Node<Index, ElementOperation<'T>> * bool)>
        val mutable internal CurrentNode : MapExtImplementation.Node<Index, ElementOperation<'T>>
        
        member x.MoveNext() =
            let struct(n, deep) = x.Head
            if not (isNull n) then

                if n.Height > 1uy && deep then
                    let inner = n :?> MapExtImplementation.Inner<Index, ElementOperation<'T>>

                    if isNull inner.Left then
                        if isNull inner.Right then
                            if x.Tail.IsEmpty then
                                x.Head <- Unchecked.defaultof<_>
                                x.Tail <- []
                            else
                                x.Head <- x.Tail.Head
                                x.Tail <- x.Tail.Tail
                        else
                            x.Head <- struct(inner.Right, true)

                        x.CurrentNode <- n
                        true
                    else
                        x.Head <- struct(inner.Left, true)
                        if isNull inner.Right then
                            x.Tail <- struct(n, false) :: x.Tail
                        else
                            x.Tail <- struct(n, false) :: struct(inner.Right, true) :: x.Tail
                        x.MoveNext()
                else
                    x.CurrentNode <- n
                    if x.Tail.IsEmpty then 
                        x.Head <- Unchecked.defaultof<_>
                        x.Tail <- []
                    else
                        x.Head <- x.Tail.Head
                        x.Tail <- x.Tail.Tail
                    true

            else
                false


        member x.Reset() =
            x.Head <- if isNull x.Root then Unchecked.defaultof<_> else struct(x.Root, true)                
            x.Tail <- []
            x.CurrentNode <- null

        member x.Dispose() =
            x.Root <- null
            x.CurrentNode <- null
            x.Head <- Unchecked.defaultof<_>
            x.Tail <- []

        member x.Current =
            (x.CurrentNode.Key, x.CurrentNode.Value)

        interface System.Collections.IEnumerator with
            member x.MoveNext() = x.MoveNext()
            member x.Reset() = x.Reset()
            member x.Current = x.Current :> obj

        interface System.Collections.Generic.IEnumerator<Index * ElementOperation<'T>> with
            member x.Current = x.Current
            member x.Dispose() = x.Dispose()

        internal new(root : MapExtImplementation.Node<Index, ElementOperation<'T>>) =
            {
                Root = root
                Head = if isNull root then Unchecked.defaultof<_> else struct(root, true)
                Tail = []
                CurrentNode = null
            }
    end

/// Functional operators for IndexListDelta.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module IndexListDelta =
    /// The empty delta.
    let empty<'T> = 
        IndexListDelta<'T>.Empty

    /// Is the list empty?
    let inline isEmpty (list : IndexListDelta<'T>) = 
        list.IsEmpty

    /// Inserts an operation.
    let inline add (index : Index) (delta : ElementOperation<'T>) (list : IndexListDelta<'T>) = 
        list.Add(index, delta)

    /// Removes the operation associated to index. (if any)
    let inline remove (index : Index) (list : IndexListDelta<'T>) = 
        list.Remove(index)

    /// Internal creating an IndexListDelta from the given map.
    let internal ofMap (map : MapExt<Index, ElementOperation<'T>>) = 
        IndexListDelta(map)
    
    /// Internal creating an IndexListDelta from the given list IndexList.
    let ofIndexList (list : IndexList<ElementOperation<'T>>) =
        IndexListDelta(list.Content)

    /// Creates a delta containing a single operation.
    let single (i : Index) (op : ElementOperation<'T>) = 
        IndexListDelta(MapExt.singleton i op)

    /// Creates an IndexListDelta containing all the given operations.
    let ofSeq (operations : seq<Index * ElementOperation<'T>>) = 
        IndexListDelta(MapExt.ofSeq operations)

    /// Creates an IndexListDelta containing all the given operations.
    let ofList (operations : list<Index * ElementOperation<'T>>) = 
        IndexListDelta(MapExt.ofList operations)

    /// Creates an IndexListDelta containing all the given operations.
    let ofArray (operations : array<Index * ElementOperation<'T>>) = 
        IndexListDelta(MapExt.ofArray operations)

    /// Returns all the operations contained in the list.
    let inline toSeq (list : IndexListDelta<'T>) = 
        list.ToSeq()

    /// Returns all the operations contained in the list.
    let inline toList (list : IndexListDelta<'T>) = 
        list.ToList()

    /// Returns all the operations contained in the list.
    let inline toArray (list : IndexListDelta<'T>) = 
        list.ToArray()

    /// Applies the given mapping function to all deltas in the list and returns a new list containing the 'Some'-results.
    /// Note that the indices need to be monotonic.
    let mapIndexed (mapping : Index -> ElementOperation<'T1> -> Index * ElementOperation<'T2>) (l : IndexListDelta<'T1>) = 
        let mapping = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(mapping)
        (MapExt.empty, l.Content) ||> MapExt.fold (fun s i v ->
            let (i, v) = mapping.Invoke(i, v)
            MapExt.add i v s
        ) |> ofMap
        
    /// Applies the given mapping function to all deltas in the list and returns a new list containing the 'Some'-results.
    /// Note that the indices need to be monotonic.
    let chooseIndexed (mapping : Index -> ElementOperation<'T1> -> option<Index * ElementOperation<'T2>>) (l : IndexListDelta<'T1>) = 
        let mapping = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(mapping)
        (MapExt.empty, l.Content) ||> MapExt.fold (fun s i v ->
            match mapping.Invoke(i, v) with
            | Some(i, v) -> MapExt.add i v s
            | None -> s
        ) |> ofMap

    /// Applies the given mapping function to all deltas in the list and returns a new list containing the results.
    let inline map (mapping : Index -> ElementOperation<'T1> -> ElementOperation<'T2>) (l : IndexListDelta<'T1>) = 
        l.Map mapping
        
    /// Applies the given mapping function to all deltas in the list and returns a new list containing the 'Some'-results.
    let inline choose (mapping : Index -> ElementOperation<'T1> -> option<ElementOperation<'T2>>) (l : IndexListDelta<'T1>) = 
        l.Choose mapping

    /// Filters the delta list using the given predicate.
    let inline filter (predicate : Index -> ElementOperation<'T> -> bool) (l : IndexListDelta<'T>) =
        l.Filter predicate

    /// Combines to IndexListDeltas to one.
    let inline combine (l : IndexListDelta<'T1>) (r : IndexListDelta<'T1>) =
        l.Combine(r)

    /// Applies the mapping function to all elements and combines the resulting lists.
    let inline collect (mapping : Index -> ElementOperation<'T1> -> IndexListDelta<'T2>) (l : IndexListDelta<'T1>) = 
        l.Collect mapping
        
