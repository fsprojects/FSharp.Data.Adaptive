namespace FSharp.Data.Traceable

open System
open System.Collections
open System.Collections.Generic
open FSharp.Data.Adaptive

/// set comparison result.
type internal SetCmp =
    | Distinct          = 0
    | ProperSubset      = 1
    | ProperSuperset    = 2
    | Overlap           = 3
    | Equal             = 4

/// a reference counting set.
[<Struct; StructuralEquality; NoComparison>]
[<StructuredFormatDisplay("{AsString}")>]
type CountingHashSet<'a>(store : HashMap<'a, int>) =
    
    /// Traceable instance.
    static let trace =
        {
            tmonoid = HashSetDelta.monoid
            tempty = CountingHashSet<'a>(HashMap.empty)
            tintegrate = fun s d -> s.Integrate d
            tdifferentiate = fun l r -> l.Differentiate r
            tprune = None
            tsize = fun s -> s.Count
        }
        
    /// Traceable instance without ref-counting.
    static let traceNoRefCount =
        {
            tmonoid = HashSetDelta.monoid
            tempty = CountingHashSet<'a>(HashMap.empty)
            tintegrate = fun s d -> s.IntegrateNoRefCount d
            tdifferentiate = fun l r -> l.Differentiate r
            tprune = None
            tsize = fun s -> s.Count
        }

    /// the empty set.
    static member Empty = CountingHashSet<'a>(HashMap.empty)


    /// Traceable instance.
    static member Trace = trace

    /// Traceable instance without ref-counting.
    static member TraceNoRefCount = traceNoRefCount

    /// is the set empty?
    member x.IsEmpty = store.IsEmpty

    /// the number of entries in the set (excluding ref-counts).
    member x.Count = store.Count

    /// internal for getting the store
    member internal x.Store = store

    /// creates a HashSet with the same entries.
    member x.ToHashSet() =
        let setStore = store.Store |> IntMap.map (List.map (fun struct(k,_) -> k))
        HashSet(x.Count, setStore)

    /// checks whether the given value is contained in the set.
    member x.Contains (value : 'a) =
        HashMap.containsKey value store
    
    /// gets the reference-count for the given value (0 if not contained)
    member x.GetRefCount (value : 'a) =
        HashMap.tryFind value store |> Option.defaultValue 0

    /// adds the given value to the set. (one reference)
    member x.Add(value : 'a) =
        store
        |> HashMap.update value (fun o -> 
            match o with
                | Some o -> o + 1
                | None -> 1
        )
        |> CountingHashSet

    /// removes the given value from the set. (one reference)
    member x.Remove(value : 'a) =
        store
        |> HashMap.alter value (fun o ->
            match o with
                | Some 1 -> None
                | Some c -> Some (c - 1)
                | None -> None
        )
        |> CountingHashSet

    /// changes the reference-count for the given element.
    member x.Alter(value : 'a, f : int -> int) =
        store
        |> HashMap.alter value (fun o ->
            let o = defaultArg o 0
            let n = f o
            if n > 0 then
                Some n
            else
                None
        )
        |> CountingHashSet

    /// unions the two sets.
    member x.Union(other : CountingHashSet<'a>) =
        HashMap.map2 (fun k l r ->
            match l, r with 
                | Some l, Some r -> l + r
                | Some l, None -> l
                | None, Some r -> r
                | None, None -> 0
        ) store other.Store
        |> CountingHashSet

    /// computes the set difference for both sets. (this - other)
    member x.Difference(other : CountingHashSet<'a>) =
        HashMap.choose2 (fun k l r ->
            let newRefCount = 
                match l, r with 
                    | Some l, Some r -> l - r
                    | Some l, None -> l
                    | None, Some r -> 0
                    | None, None -> 0

            if newRefCount > 0 then Some newRefCount
            else None
        ) store other.Store
        |> CountingHashSet

    /// computes the intersection of both sets.
    member x.Intersect(other : CountingHashSet<'a>) =
        HashMap.choose2 (fun k l r ->
            match l, r with 
                | Some l, Some r -> Some (min l r)
                | _ -> None
        ) store other.Store
        |> CountingHashSet

    /// unions both sets using resolve to aggregate ref-counts.
    member x.UnionWith(other : CountingHashSet<'a>, resolve : int -> int -> int) =
        HashMap.map2 (fun k l r ->
            match l, r with 
                | Some l, Some r -> resolve l r
                | Some l, None -> resolve l 0
                | None, Some r -> resolve 0 r
                | None, None -> resolve 0 0
        ) store other.Store
        |> CountingHashSet

    /// gets the HashMap representation of the set.
    member x.ToHashMap() =
        store

    /// all elements in the set.
    member x.ToSeq() =
        store.ToSeq() |> Seq.map fst
        
    /// all elements in the set.
    member x.ToList() =
        store.ToList() |> List.map fst
        
    /// all elements in the set.
    member x.ToArray() =
        let result = Array.zeroCreate store.Count
        let mutable i = 0
        for (key, _value) in store do
            result.[i] <- key
            i <- i + 1
        result

    /// creates a new set by applying the given function to all elements.
    member x.Map(mapping : 'a -> 'b) =
        let mutable res = HashMap.empty
        for (k,v) in HashMap.toSeq store do
            let k = mapping k
            res <- res |> HashMap.update k (fun o -> defaultArg o 0 + v) 

        CountingHashSet res
        
    /// creates a new set by applying the given function to all elements.
    member x.Choose(mapping : 'a -> option<'b>) =
        let mutable res = HashMap.empty
        for (k,v) in HashMap.toSeq store do
            match mapping k with
                | Some k ->
                    res <- res |> HashMap.update k (fun o -> defaultArg o 0 + v) 
                | None ->
                    ()

        CountingHashSet res
        
    /// creates a new set filtered by the given predicate.
    member x.Filter(predicate : 'a -> bool) =
        store |> HashMap.filter (fun k _ -> predicate k) |> CountingHashSet

    /// creates a new set with all elements from all created sets.  (respecting ref-counts)
    member x.Collect(mapping : 'a -> CountingHashSet<'b>) =
        let mutable res = CountingHashSet<'b>.Empty
        for (k,ro) in store.ToSeq() do
            let r = mapping k
            if ro = 1 then
                res <- res.Union r
            else
                res <- res.UnionWith(r, fun li ri -> li + ro * ri)
        res

    /// iterates over all set elements. (once)
    member x.Iter (iterator : 'a -> unit) =
        store |> HashMap.iter (fun k _ -> iterator k)

    /// checks whether an element fulfilling the predicate exists.
    member x.Exists (predicate : 'a -> bool) =
        store |> HashMap.exists (fun k _ -> predicate k)
        
    /// checks whether all elements fulfill the predicate.
    member x.Forall (predicate : 'a -> bool) =
        store |> HashMap.forall (fun k _ -> predicate k)

    /// folds over all elements in the set.
    member x.Fold (seed : 's, folder : 's -> 'a -> 's) =
        store |> HashMap.fold (fun s k _ -> folder s k) seed 

    /// creates a set holding all the given values. (with reference counts)
    static member OfSeq (seq : seq<'a>) =
        let mutable res = CountingHashSet<'a>.Empty
        for e in seq do
            res <- res.Add e
        res
        
    /// creates a set holding all the given values. (with reference counts)
    static member OfList (list : list<'a>) =
        CountingHashSet<'a>.OfSeq list
        
    /// creates a set holding all the given values. (with reference counts)
    static member OfArray (arr : 'a[]) =
        CountingHashSet<'a>.OfSeq arr
        
    /// creates a set holding all the given values. (with reference counts)
    static member OfHashMap (map : HashMap<'a, int>) =
        CountingHashSet map
        
    /// creates a set holding all the given values.
    static member OfHashSet (set : HashSet<'a>) =
        let mapStore = set.Store |> IntMap.map (List.map (fun a -> struct(a,1)))
        CountingHashSet(HashMap(set.Count, mapStore))

    /// differentiates two sets returning a HashSetDelta.
    member x.Differentiate(other : CountingHashSet<'a>) =
        // O(1)
        if Object.ReferenceEquals(store.Store, other.Store.Store) then
            HashSetDelta.empty

        // O(other)
        elif store.IsEmpty then 
            other.Store |> HashMap.map (fun _ _ -> 1) |> HashSetDelta.ofHashMap

        // O(N)
        elif other.IsEmpty then
            store |> HashMap.map (fun _ _ -> -1) |> HashSetDelta.ofHashMap
        
        // O(N + other)
        else
            let mutable cnt = 0

            let del (l : list<struct ('a * int)>) =
                l |> List.map (fun struct(v,_) -> cnt <- cnt + 1; struct (v, -1))
            
            let add (l : list<struct('a * int)>) =
                l |> List.map (fun struct(v,_) -> cnt <- cnt + 1; struct (v, 1))

            let both (_hash : int) (l : list<struct('a * int)>) (r : list<struct('a * int)>) =
                HashMapList.mergeWithOption' (fun v l r ->
                    match l, r with
                        | Some l, None ->  cnt <- cnt + 1; Some -1
                        | None, Some r ->  cnt <- cnt + 1; Some 1
                        | _ -> None
                ) l r

            let store = IntMap.computeDelta both (IntMap.map del) (IntMap.map add) store.Store other.Store.Store
            HashSetDelta (HashMap(cnt, store))

    /// same as x.Differentiate(empty)
    member x.RemoveAll() =
        store |> HashMap.map (fun _ v -> -v) |> HashSetDelta
        
    /// same as empty.Differentiate(x)
    member x.AddAll() =
        store |> HashSetDelta

    /// integrates the given delta into the set, returns a new set and the effective deltas.
    member x.Integrate (deltas : HashSetDelta<'a>) =
        // O(1)
        if deltas.IsEmpty then
            x, deltas

        // O(Delta)
        elif store.IsEmpty then
            let mutable maxDelta = 0
            let state = deltas |> HashSetDelta.toHashMap |> HashMap.filter (fun _ d -> maxDelta <- max maxDelta d; d > 0)
            let delta = 
                if maxDelta > 1 then state |> HashMap.map (fun _ _ -> 1)
                else state

            CountingHashSet state, HashSetDelta delta

        // O(Delta * log N)
        elif deltas.Count * 5 < store.Count then
            let mutable res = store

            let effective =
                deltas |> HashSetDelta.choose (fun d ->
                    let mutable delta = Unchecked.defaultof<SetOperation<'a>>
                    let value = d.Value
                    res <- res |> HashMap.alter value (fun cnt ->
                        let o = defaultArg cnt 0
                        let n = o + d.Count
                        if n > 0 && o = 0 then 
                            delta <- Add(value)
                        elif n = 0 && o > 0 then
                            delta <- Rem(value)

                        if n <= 0 then None
                        else Some n
                    )

                    if delta.Count <> 0 then Some delta
                    else None
                )

            CountingHashSet res, effective

        // O(Delta + N)
        else
            let mutable effective = HashSetDelta.empty
            let deltas = HashSetDelta.toHashMap deltas
            let newStore = 
                HashMap.choose2 (fun k s d ->
                    match d with
                        | Some d ->
                            let o = Option.defaultValue 0 s 
                            let n = d + o
                            if o = 0 && n > 0 then
                                effective <- HashSetDelta.add (Add k) effective
                            elif o > 0 && n = 0 then
                                effective <- HashSetDelta.add (Rem k) effective
                            
                            if n <= 0 then None
                            else Some n
                        | None ->
                            s
                ) store deltas

            CountingHashSet newStore, effective

    /// integrates the given delta into the set without ref-counting, returns a new set and the effective deltas.
    member x.IntegrateNoRefCount (deltas : HashSetDelta<'a>) =
        // O(1)
        if deltas.IsEmpty then
            x, deltas

        // O(Delta)
        elif store.IsEmpty then
            let state = deltas |> HashSetDelta.toHashMap |> HashMap.choose (fun _ d -> if d > 0 then Some 1 else None)
            CountingHashSet state, HashSetDelta state

        // O(Delta * log N)
        elif deltas.Count * 5 < store.Count then
            let mutable res = store

            let effective =
                deltas |> HashSetDelta.choose (fun d ->
                    let mutable delta = Unchecked.defaultof<SetOperation<'a>>
                    let value = d.Value
                    res <- res |> HashMap.alter value (fun cnt ->
                        let o = defaultArg cnt 0
                        let n = 
                            if d.Count > 0 then 1 
                            elif d.Count < 0 then 0
                            else o

                        if n > 0 && o = 0 then 
                            delta <- Add(value)
                        elif n = 0 && o > 0 then
                            delta <- Rem(value)

                        if n <= 0 then None
                        else Some n
                    )

                    if delta.Count <> 0 then Some delta
                    else None
                )

            CountingHashSet res, effective
        
        // O(Delta + N)
        else
            let mutable effective = HashSetDelta.empty
            let deltas = HashSetDelta.toHashMap deltas
            let newStore = 
                HashMap.choose2 (fun k s d ->
                    match d with
                        | Some d ->
                            let o = Option.defaultValue 0 s 
                            let n = if d > 0 then 1 elif d < 0 then 0 else o

                            if o = 0 && n > 0 then
                                effective <- HashSetDelta.add (Add k) effective
                            elif o > 0 && n = 0 then
                                effective <- HashSetDelta.add (Rem k) effective
                            
                            if n <= 0 then None
                            else Some n
                        | None ->
                            s
                ) store deltas

            CountingHashSet newStore, effective

    /// comares two sets.
    static member internal Compare(l : CountingHashSet<'a>, r : CountingHashSet<'a>) =
        let i = l.Intersect r
        let b = i.Count
        let lo = l.Count - b
        let ro = r.Count - b

        match lo, b, ro with
            | 0, _, 0 -> SetCmp.Equal
            | _, 0, _ -> SetCmp.Distinct
            | a, _, 0 -> SetCmp.ProperSuperset
            | 0, _, a -> SetCmp.ProperSubset
            | _, _, _ -> SetCmp.Overlap


    override x.ToString() =
        let suffix =
            if x.Count > 5 then "; ..."
            else ""

        let content =
            x.ToSeq() |> Seq.truncate 5 |> Seq.map (sprintf "%A") |> String.concat "; "

        "CountingHashSet [" + content + suffix + "]"

    member private x.AsString = x.ToString()

    interface IEnumerable with
        member x.GetEnumerator() = new CountingHashSetEnumerator<_>(store) :> _

    interface IEnumerable<'a> with
        member x.GetEnumerator() = new CountingHashSetEnumerator<_>(store) :> _

/// an enumerator for CountingHashSet.
and private CountingHashSetEnumerator<'a>(store : HashMap<'a, int>) =
    let e = (store :> seq<_>).GetEnumerator()

    member x.Current = 
        let (v,_) = e.Current
        v

    interface IEnumerator with
        member x.MoveNext() = e.MoveNext()
        member x.Current = x.Current :> obj
        member x.Reset() = e.Reset()

    interface IEnumerator<'a> with
        member x.Dispose() = e.Dispose()
        member x.Current = x.Current

/// functional operators for CountingHashSet.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CountingHashSet =

    /// the empty set.
    let inline empty<'a> = CountingHashSet<'a>.Empty

    /// a set holding a single value.
    let single v = CountingHashSet (HashMap.single v 1)

    // creates a HashMap with all the contained values and ref-counts.
    let inline toHashMap (set : CountingHashSet<'a>) = set.ToHashMap()

    // a seq containing all elements from the set. (once)
    let inline toSeq (set : CountingHashSet<'a>) = set.ToSeq()
    
    // a list containing all elements from the set. (once)
    let inline toList (set : CountingHashSet<'a>) = set.ToList()
    
    // an array containing all elements from the set. (once)
    let inline toArray (set : CountingHashSet<'a>) = set.ToArray()
    
    // a HashSet containing all elements from the set.
    let inline toHashSet (set : CountingHashSet<'a>) = set.ToHashSet()

    // creates a set from the given HashMap containing ref-counts.
    let inline ofHashMap (map : HashMap<'a, int>) = CountingHashSet.OfHashMap map
    
    /// creates a set holding all the given values.
    let inline ofHashSet (set : HashSet<'a>) = CountingHashSet.OfHashSet set

    /// creates a set holding all the given values.
    let inline ofSeq (seq : seq<'a>) = CountingHashSet.OfSeq seq

    /// creates a set holding all the given values.
    let inline ofList (list : list<'a>) = CountingHashSet.OfList list

    /// creates a set holding all the given values.
    let inline ofArray (arr : 'a[]) = CountingHashSet.OfArray arr

    /// is the set empty?
    let inline isEmpty (set : CountingHashSet<'a>) =
        set.IsEmpty

    /// the number of entries in the set (excluding ref-counts).
    let inline count (set : CountingHashSet<'a>) =
        set.Count

    /// gets the reference-count for the given value (0 if not contained)
    let inline refcount (value : 'a) (set : CountingHashSet<'a>) =
        set.GetRefCount value

    /// checks whether the given value is contained in the set.
    let inline contains (value : 'a) (set : CountingHashSet<'a>) =
        set.Contains value

    /// adds the given value to the set. (one reference)
    let inline add (value : 'a) (set : CountingHashSet<'a>) =
        set.Add value
        
    /// removes the given value from the set. (one reference)
    let inline remove (value : 'a) (set : CountingHashSet<'a>) =
        set.Remove value

    /// unions the two sets.
    let inline union (l : CountingHashSet<'a>) (r : CountingHashSet<'a>) =
        l.Union r

    /// computes the set difference for both sets. (l - r)
    let inline difference (l : CountingHashSet<'a>) (r : CountingHashSet<'a>) =
        l.Difference r

    /// computes the intersection of both sets.
    let inline intersect (l : CountingHashSet<'a>) (r : CountingHashSet<'a>) =
        l.Intersect r

    /// changes the reference-count for the given element.
    let inline alter (value : 'a) (f : int -> int) (set : CountingHashSet<'a>) =
        set.Alter(value, f)

    /// creates a new set by applying the given function to all elements.
    let inline map (mapping : 'a -> 'b) (set : CountingHashSet<'a>) =
        set.Map mapping

    /// creates a new set by applying the given function to all elements.
    let inline choose (mapping : 'a -> option<'b>) (set : CountingHashSet<'a>) =
        set.Choose mapping

    /// creates a new set filtered by the given predicate.
    let inline filter (predicate : 'a -> bool) (set : CountingHashSet<'a>) =
        set.Filter predicate

    /// creates a new set with all elements from all created sets. (respecting ref-counts)
    let inline collect (mapping : 'a -> CountingHashSet<'b>) (set : CountingHashSet<'a>) =
        set.Collect mapping

    /// iterates over all set elements. (once)    
    let inline iter (iterator : 'a -> unit) (set : CountingHashSet<'a>) =
        set.Iter iterator

    /// checks whether an element fulfilling the predicate exists.
    let inline exists (predicate : 'a -> bool) (set : CountingHashSet<'a>) =
        set.Exists predicate

    /// checks whether all elements fulfill the predicate.
    let inline forall (predicate : 'a -> bool) (set : CountingHashSet<'a>) =
        set.Forall predicate

    /// folds over all elements in the set.
    let inline fold (folder : 's -> 'a -> 's) (seed : 's) (set : CountingHashSet<'a>) =
        set.Fold(seed, folder)

    /// Traceable instance.
    let inline trace<'a> = CountingHashSet<'a>.Trace

    /// Traceable instance without ref-counting.
    let inline traceNoRefCount<'a> = CountingHashSet<'a>.TraceNoRefCount

    /// differentiates two sets returning a HashSetDelta.
    let inline differentiate (src : CountingHashSet<'a>) (dst : CountingHashSet<'a>) =
        src.Differentiate dst

    /// same as differentiate src empty.
    let inline removeAll (src : CountingHashSet<'a>) =
        src.RemoveAll()
        
    /// same as differentiate empty src.
    let inline addAll (src : CountingHashSet<'a>) =
        src.AddAll()


    /// integrates the given delta into the set, returns a new set and the effective deltas.
    let inline integrate (set : CountingHashSet<'a>) (delta : HashSetDelta<'a>) =
        set.Integrate delta

    /// integrates the given delta into the set without ref-counting, returns a new set and the effective deltas.
    let inline integrateNoRefCount (set : CountingHashSet<'a>) (delta : HashSetDelta<'a>) =
        set.IntegrateNoRefCount delta
        
    /// comares two sets.
    let internal compare (l : CountingHashSet<'a>) (r : CountingHashSet<'a>) =
        CountingHashSet.Compare(l,r)

