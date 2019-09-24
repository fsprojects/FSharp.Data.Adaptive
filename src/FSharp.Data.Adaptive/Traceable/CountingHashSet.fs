namespace FSharp.Data.Traceable

open System
open System.Collections
open System.Collections.Generic
open FSharp.Data.Adaptive

/// Set comparison result.
type internal SetCmp =
    | Distinct          = 0
    | ProperSubset      = 1
    | ProperSuperset    = 2
    | Overlap           = 3
    | Equal             = 4

/// A reference counting set, used for tracing the unions of sets with elements in common.
//
// For example:
//
//  a={1,2,3}
//  b={1}
//  c=union a b // {1,2,3}
//
//  a.remove 1
// c={1,2,3} // still because 1 was contained twice
//
// We could have solved these problems a different way, but using refcounting simply works and we don't have to care about duplicates in reader implementations...
[<Struct; StructuralEquality; NoComparison>]
[<StructuredFormatDisplay("{AsString}")>]
type CountingHashSet<'T>(store : HashMap<'T, int>) =
    
    /// Traceable instance.
    static let trace =
        {
            tmonoid = HashSetDelta.monoid
            tempty = CountingHashSet<'T>(HashMap.empty)
            tintegrate = fun s d -> s.Integrate d
            tdifferentiate = fun l r -> l.Differentiate r
            tprune = None
            tsize = fun s -> s.Count
        }
        
    /// Traceable instance without ref-counting.
    static let traceNoRefCount =
        {
            tmonoid = HashSetDelta.monoid
            tempty = CountingHashSet<'T>(HashMap.empty)
            tintegrate = fun s d -> s.IntegrateNoRefCount d
            tdifferentiate = fun l r -> l.Differentiate r
            tprune = None
            tsize = fun s -> s.Count
        }

    /// The empty set.
    static member Empty = CountingHashSet<'T>(HashMap.empty)

    /// Traceable instance.
    static member Trace = trace

    /// Traceable instance without ref-counting.
    static member TraceNoRefCount = traceNoRefCount

    /// Is the set empty?
    member x.IsEmpty = store.IsEmpty

    /// The number of entries in the set (excluding ref-counts).
    member x.Count = store.Count

    /// Internal for getting the store
    member internal x.Store = store

    /// Creates a HashSet with the same entries.
    member x.ToHashSet() =
        let setStore = store.Store |> IntMap.map (List.map (fun struct(k,_) -> k))
        HashSet(x.Count, setStore)

    /// Checks whether the given value is contained in the set.
    member x.Contains (value : 'T) =
        HashMap.containsKey value store
    
    /// Gets the reference-count for the given value (0 if not contained)
    member x.GetRefCount (value : 'T) =
        HashMap.tryFind value store |> Option.defaultValue 0

    /// Adds the given value to the set. (one reference)
    member x.Add(value : 'T) =
        store
        |> HashMap.update value (fun o -> 
            match o with
                | Some o -> o + 1
                | None -> 1
        )
        |> CountingHashSet

    /// Removes the given value from the set. (one reference)
    member x.Remove(value : 'T) =
        store
        |> HashMap.alter value (fun o ->
            match o with
                | Some 1 -> None
                | Some c -> Some (c - 1)
                | None -> None
        )
        |> CountingHashSet

    /// Changes the reference-count for the given element.
    member x.Alter(value : 'T, f : int -> int) =
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

    /// Unions the two sets.
    member x.Union(other : CountingHashSet<'T>) =
        HashMap.map2 (fun k l r ->
            match l, r with 
                | Some l, Some r -> l + r
                | Some l, None -> l
                | None, Some r -> r
                | None, None -> 0
        ) store other.Store
        |> CountingHashSet

    /// Computes the set difference for both sets. (this - other)
    member x.Difference(other : CountingHashSet<'T>) =
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

    /// Computes the intersection of both sets.
    member x.Intersect(other : CountingHashSet<'T>) =
        HashMap.choose2 (fun k l r ->
            match l, r with 
                | Some l, Some r -> Some (min l r)
                | _ -> None
        ) store other.Store
        |> CountingHashSet

    /// Unions both sets using resolve to aggregate ref-counts.
    member x.UnionWith(other : CountingHashSet<'T>, resolve : int -> int -> int) =
        HashMap.map2 (fun k l r ->
            match l, r with 
                | Some l, Some r -> resolve l r
                | Some l, None -> resolve l 0
                | None, Some r -> resolve 0 r
                | None, None -> resolve 0 0
        ) store other.Store
        |> CountingHashSet

    /// Gets the HashMap representation of the set.
    member x.ToHashMap() =
        store

    /// All elements in the set.
    member x.ToSeq() =
        store.ToSeq() |> Seq.map fst
        
    /// All elements in the set.
    member x.ToList() =
        store.ToList() |> List.map fst
        
    /// All elements in the set.
    member x.ToArray() =
        let result = Array.zeroCreate store.Count
        let mutable i = 0
        for (key, _value) in store do
            result.[i] <- key
            i <- i + 1
        result

    /// Creates a new set by applying the given function to all elements.
    member x.Map(mapping : 'T -> 'B) =
        let mutable res = HashMap.empty
        for (k,v) in HashMap.toSeq store do
            let k = mapping k
            res <- res |> HashMap.update k (fun o -> defaultArg o 0 + v) 

        CountingHashSet res
        
    /// Creates a new set by applying the given function to all elements.
    member x.Choose(mapping : 'T -> option<'B>) =
        let mutable res = HashMap.empty
        for (k,v) in HashMap.toSeq store do
            match mapping k with
                | Some k ->
                    res <- res |> HashMap.update k (fun o -> defaultArg o 0 + v) 
                | None ->
                    ()

        CountingHashSet res
        
    /// Creates a new set filtered by the given predicate.
    member x.Filter(predicate : 'T -> bool) =
        store |> HashMap.filter (fun k _ -> predicate k) |> CountingHashSet

    /// Creates a new set with all elements from all created sets.  (respecting ref-counts)
    member x.Collect(mapping : 'T -> CountingHashSet<'B>) =
        let mutable res = CountingHashSet<'B>.Empty
        for (k,ro) in store.ToSeq() do
            let r = mapping k
            if ro = 1 then
                res <- res.Union r
            else
                res <- res.UnionWith(r, fun li ri -> li + ro * ri)
        res

    /// Iterates over all set elements. (once)
    member x.Iter (iterator : 'T -> unit) =
        store |> HashMap.iter (fun k _ -> iterator k)

    /// Checks whether an element fulfilling the predicate exists.
    member x.Exists (predicate : 'T -> bool) =
        store |> HashMap.exists (fun k _ -> predicate k)
        
    /// Checks whether all elements fulfill the predicate.
    member x.Forall (predicate : 'T -> bool) =
        store |> HashMap.forall (fun k _ -> predicate k)

    /// Folds over all elements in the set.
    member x.Fold (seed : 'S, folder : 'S -> 'T -> 'S) =
        store |> HashMap.fold (fun s k _ -> folder s k) seed 

    /// Creates a set holding all the given values. (with reference counts)
    static member OfSeq (seq : seq<'T>) =
        let mutable res = CountingHashSet<'T>.Empty
        for e in seq do
            res <- res.Add e
        res
        
    /// Creates a set holding all the given values. (with reference counts)
    static member OfList (list : list<'T>) =
        CountingHashSet<'T>.OfSeq list
        
    /// Creates a set holding all the given values. (with reference counts)
    static member OfArray (arr : 'T[]) =
        CountingHashSet<'T>.OfSeq arr
        
    /// Creates a set holding all the given values. (with reference counts)
    static member OfHashMap (map : HashMap<'T, int>) =
        CountingHashSet map
        
    /// Creates a set holding all the given values.
    static member OfHashSet (set : HashSet<'T>) =
        let mapStore = set.Store |> IntMap.map (List.map (fun a -> struct(a,1)))
        CountingHashSet(HashMap(set.Count, mapStore))

    /// Differentiates two sets returning a HashSetDelta.
    member x.Differentiate(other : CountingHashSet<'T>) =
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

            let del (l : list<struct ('T * int)>) =
                l |> List.map (fun struct(v,_) -> cnt <- cnt + 1; struct (v, -1))
            
            let add (l : list<struct('T * int)>) =
                l |> List.map (fun struct(v,_) -> cnt <- cnt + 1; struct (v, 1))

            let both (_hash : int) (l : list<struct('T * int)>) (r : list<struct('T * int)>) =
                HashMapList.mergeWithOption' (fun v l r ->
                    match l, r with
                    | Some l, None ->  cnt <- cnt + 1; Some -1
                    | None, Some r ->  cnt <- cnt + 1; Some 1
                    | _ -> None
                ) l r

            let store = IntMap.computeDelta both (IntMap.map del) (IntMap.map add) store.Store other.Store.Store
            HashSetDelta (HashMap(cnt, store))

    /// Same as x.Differentiate(empty)
    member x.RemoveAll() =
        store |> HashMap.map (fun _ v -> -v) |> HashSetDelta
        
    /// Same as empty.Differentiate(x)
    member x.AddAll() =
        store |> HashSetDelta

    /// Integrates the given delta into the set, returns a new set and the effective deltas.
    member x.Integrate (deltas : HashSetDelta<'T>) =
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
                    let mutable delta = Unchecked.defaultof<SetOperation<'T>>
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
                (store, deltas) ||> HashMap.choose2 (fun k s d ->
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
                ) 

            CountingHashSet newStore, effective

    /// Integrates the given delta into the set without ref-counting, returns a new set and the effective deltas.
    member x.IntegrateNoRefCount (deltas : HashSetDelta<'T>) =
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
                    let mutable delta = Unchecked.defaultof<SetOperation<'T>>
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

    /// Compares two sets.
    static member internal Compare(l : CountingHashSet<'T>, r : CountingHashSet<'T>) =
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

    interface IEnumerable<'T> with
        member x.GetEnumerator() = new CountingHashSetEnumerator<_>(store) :> _

/// An enumerator for CountingHashSet.
and private CountingHashSetEnumerator<'T>(store : HashMap<'T, int>) =
    let e = (store :> seq<_>).GetEnumerator()

    member x.Current = 
        let (v,_) = e.Current
        v

    interface IEnumerator with
        member x.MoveNext() = e.MoveNext()
        member x.Current = x.Current :> obj
        member x.Reset() = e.Reset()

    interface IEnumerator<'T> with
        member x.Dispose() = e.Dispose()
        member x.Current = x.Current

/// Functional operators for CountingHashSet.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CountingHashSet =

    /// The empty set.
    let inline empty<'T> = CountingHashSet<'T>.Empty

    /// A set holding a single value.
    let single v = CountingHashSet (HashMap.single v 1)

    /// Creates a HashMap with all the contained values and ref-counts.
    let inline toHashMap (set : CountingHashSet<'T>) = set.ToHashMap()

    /// A seq containing all elements from the set. (once)
    let inline toSeq (set : CountingHashSet<'T>) = set.ToSeq()
    
    /// A list containing all elements from the set. (once)
    let inline toList (set : CountingHashSet<'T>) = set.ToList()
    
    /// An array containing all elements from the set. (once)
    let inline toArray (set : CountingHashSet<'T>) = set.ToArray()
    
    /// A HashSet containing all elements from the set.
    let inline toHashSet (set : CountingHashSet<'T>) = set.ToHashSet()

    /// Creates a set from the given HashMap containing ref-counts.
    let inline ofHashMap (map : HashMap<'T, int>) = CountingHashSet.OfHashMap map
    
    /// Creates a set holding all the given values.
    let inline ofHashSet (set : HashSet<'T>) = CountingHashSet.OfHashSet set

    /// Creates a set holding all the given values.
    let inline ofSeq (seq : seq<'T>) = CountingHashSet.OfSeq seq

    /// Creates a set holding all the given values.
    let inline ofList (list : list<'T>) = CountingHashSet.OfList list

    /// Creates a set holding all the given values.
    let inline ofArray (arr : 'T[]) = CountingHashSet.OfArray arr

    /// Is the set empty?
    let inline isEmpty (set : CountingHashSet<'T>) =
        set.IsEmpty

    /// The number of entries in the set (excluding ref-counts).
    let inline count (set : CountingHashSet<'T>) =
        set.Count

    /// Gets the reference-count for the given value (0 if not contained)
    let inline refcount (value : 'T) (set : CountingHashSet<'T>) =
        set.GetRefCount value

    /// Checks whether the given value is contained in the set.
    let inline contains (value : 'T) (set : CountingHashSet<'T>) =
        set.Contains value

    /// Adds the given value to the set. (one reference)
    let inline add (value : 'T) (set : CountingHashSet<'T>) =
        set.Add value
        
    /// Removes the given value from the set. (one reference)
    let inline remove (value : 'T) (set : CountingHashSet<'T>) =
        set.Remove value

    /// Unions the two sets.
    let inline union (l : CountingHashSet<'T>) (r : CountingHashSet<'T>) =
        l.Union r

    /// Computes the set difference for both sets. (l - r)
    let inline difference (l : CountingHashSet<'T>) (r : CountingHashSet<'T>) =
        l.Difference r

    /// Computes the intersection of both sets.
    let inline intersect (l : CountingHashSet<'T>) (r : CountingHashSet<'T>) =
        l.Intersect r

    /// Changes the reference-count for the given element.
    let inline alter (value : 'T) (f : int -> int) (set : CountingHashSet<'T>) =
        set.Alter(value, f)

    /// Creates a new set by applying the given function to all elements.
    let inline map (mapping : 'A -> 'B) (set : CountingHashSet<'A>) =
        set.Map mapping

    /// Creates a new set by applying the given function to all elements.
    let inline choose (mapping : 'A -> option<'B>) (set : CountingHashSet<'A>) =
        set.Choose mapping

    /// Creates a new set filtered by the given predicate.
    let inline filter (predicate : 'T -> bool) (set : CountingHashSet<'T>) =
        set.Filter predicate

    /// Creates a new set with all elements from all created sets. (respecting ref-counts)
    let inline collect (mapping : 'A -> CountingHashSet<'B>) (set : CountingHashSet<'A>) =
        set.Collect mapping

    /// Iterates over all set elements. (once)    
    let inline iter (iterator : 'T -> unit) (set : CountingHashSet<'T>) =
        set.Iter iterator

    /// Checks whether an element fulfilling the predicate exists.
    let inline exists (predicate : 'T -> bool) (set : CountingHashSet<'T>) =
        set.Exists predicate

    /// Checks whether all elements fulfill the predicate.
    let inline forall (predicate : 'T -> bool) (set : CountingHashSet<'T>) =
        set.Forall predicate

    /// Folds over all elements in the set.
    let inline fold (folder : 'S -> 'T -> 'S) (seed : 'S) (set : CountingHashSet<'T>) =
        set.Fold(seed, folder)

    /// Traceable instance.
    let inline trace<'T> = CountingHashSet<'T>.Trace

    /// Traceable instance without ref-counting.
    let inline traceNoRefCount<'T> = CountingHashSet<'T>.TraceNoRefCount

    /// Differentiates two sets returning a HashSetDelta.
    let inline differentiate (src : CountingHashSet<'T>) (dst : CountingHashSet<'T>) =
        src.Differentiate dst

    /// Same as differentiate src empty.
    let inline removeAll (src : CountingHashSet<'T>) =
        src.RemoveAll()
        
    /// Same as differentiate empty src.
    let inline addAll (src : CountingHashSet<'T>) =
        src.AddAll()

    /// Integrates the given delta into the set, returns a new set and the effective deltas.
    let inline integrate (set : CountingHashSet<'T>) (delta : HashSetDelta<'T>) =
        set.Integrate delta

    /// Integrates the given delta into the set without ref-counting, returns a new set and the effective deltas.
    let inline integrateNoRefCount (set : CountingHashSet<'T>) (delta : HashSetDelta<'T>) =
        set.IntegrateNoRefCount delta
        
    /// Compares two sets.
    let internal compare (l : CountingHashSet<'T>) (r : CountingHashSet<'T>) =
        CountingHashSet.Compare(l,r)

