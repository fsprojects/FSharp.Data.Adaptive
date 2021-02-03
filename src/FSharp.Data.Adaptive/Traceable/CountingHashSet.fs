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
//  c={1,2,3} // still because 1 was contained twice
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
            tapplyDelta = fun s d -> s.ApplyDelta d
            tcomputeDelta = fun l r -> l.ComputeDelta r
            tprune = None
            tsize = fun s -> s.Count
        }
        
    /// Traceable instance without ref-counting.
    static let traceNoRefCount =
        {
            tmonoid = HashSetDelta.monoid
            tempty = CountingHashSet<'T>(HashMap.empty)
            tapplyDelta = fun s d -> s.ApplyDeltaNoRefCount d
            tcomputeDelta = fun l r -> l.ComputeDelta r
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
    member x.ToHashSet() = store.GetKeys()

    /// Checks whether the given value is contained in the set.
    member x.Contains (value : 'T) =
        HashMap.containsKey value store
    
    /// Gets the reference-count for the given value (0 if not contained)
    member x.GetRefCount (value : 'T) =
        match HashMap.tryFindV value store with
        | ValueSome r -> r
        | ValueNone -> 0

    /// Adds the given value to the set. (one reference)
    member x.Add(value : 'T) =
        store.AlterV(value, function 
            | ValueSome o -> ValueSome (o + 1)
            | ValueNone -> ValueSome 1
        )
        |> CountingHashSet

    /// Removes the given value from the set. (one reference)
    member x.Remove(value : 'T) =
        store.AlterV(value, function
            | ValueSome v when v > 1 ->
                ValueSome (v - 1)
            | _ ->
                ValueNone
        )
        |> CountingHashSet

    /// Changes the reference-count for the given element.
    member x.Alter(value : 'T, f : int -> int) =
        store.AlterV(value, fun o ->
            let o = match o with | ValueSome o -> o | ValueNone -> 0
            let n = f o
            if n > 0 then ValueSome n
            else ValueNone
        )
        |> CountingHashSet

    /// Unions the two sets.
    member x.Union(other : CountingHashSet<'T>) =
        HashMap.unionWith (fun _ l r -> l + r) store other.Store |> CountingHashSet

    /// Computes the set difference for both sets. (this - other)
    member x.Difference(other : CountingHashSet<'T>) =
        (store, other.Store) ||> HashMap.choose2V (fun _ l r ->
            match l with
            | ValueNone -> ValueNone
            | ValueSome l ->
                match r with
                | ValueNone -> ValueSome l
                | ValueSome r ->
                    let n = l - r
                    if n > 0 then ValueSome n
                    else ValueNone
        )
        |> CountingHashSet

    /// Computes the intersection of both sets.
    member x.Intersect(other : CountingHashSet<'T>) =
        (store, other.Store) ||> HashMap.choose2V (fun _ l r ->
            match l with 
            | ValueSome l ->
                match r with
                | ValueSome r -> ValueSome (min l r)
                | ValueNone -> ValueNone
            | ValueNone -> 
                ValueNone
        )
        |> CountingHashSet
        
    /// Computes the exclusive or of both sets.
    member x.Xor(other : CountingHashSet<'T>) =
        (store, other.Store) ||> HashMap.choose2V (fun _ l r ->
            match l with 
            | ValueSome l ->
                match r with
                | ValueSome _ -> ValueNone
                | ValueNone -> ValueSome l
            | ValueNone -> 
                r
        )
        |> CountingHashSet
        

    /// Unions both sets using resolve to aggregate ref-counts.
    member x.UnionWith(other : CountingHashSet<'T>, resolve : int -> int -> int) =
        (store, other.Store) ||> HashMap.choose2V (fun _k l r ->
            let l = match l with | ValueSome l -> l | ValueNone -> 0
            let r = match r with | ValueSome r -> r | ValueNone -> 0
            let res = resolve l r
            if res > 0 then ValueSome res
            else ValueNone
        ) 
        |> CountingHashSet
        
    /// Gets the HashMap representation of the set.
    member x.ToHashMap() =
        store

    /// All elements in the set.
    member x.ToSeq() =
        store.GetKeys() :> seq<_>
        
    /// All elements in the set.
    member x.ToList() =
        store.ToKeyList()
        
    /// All elements in the set.
    member x.ToArray() = 
        store.ToKeyArray()

    /// Creates a new set by applying the given function to all elements.
    member x.Map(mapping : 'T -> 'B) =
        let mutable res = HashMap.empty
        for (k,v) in store do
            let k = mapping k
            res <- res.AlterV(k, function ValueSome o -> ValueSome (o + v) | ValueNone -> ValueSome v)

        CountingHashSet res
        
    /// Creates a new set by applying the given function to all elements.
    member x.Choose(mapping : 'T -> option<'B>) =
        let mutable res = HashMap.empty
        for (k,v) in store do
            match mapping k with
                | Some k ->
                    res <- res.AlterV(k, function ValueSome o -> ValueSome (o + v) | _ -> ValueSome v) 
                | None ->
                    ()

        CountingHashSet res
        
    /// Creates a new set filtered by the given predicate.
    member x.Filter(predicate : 'T -> bool) =
        store |> HashMap.filter (fun k _ -> predicate k) |> CountingHashSet

    /// Creates a new set with all elements from all created sets.  (respecting ref-counts)
    member x.Collect(mapping : 'T -> CountingHashSet<'B>) =
        let mutable res = HashMap<'B, int>.Empty
        for (k,ro) in store do
            let r = mapping k
            let rr, _ =
                HashMap<'B, int>.ApplyDelta(res, r.Store, fun _ oldRef delta ->
                    let oldRef = match oldRef with | ValueSome o -> o | ValueNone -> 0
                    struct (ValueSome (oldRef + ro * delta), ValueNone)
                )
            res <- rr
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
        let mutable res = CountingHashSet<'T>.Empty
        for e in list do
            res <- res.Add e
        res
        
    /// Creates a set holding all the given values. (with reference counts)
    static member OfArray (arr : 'T[]) =
        let mutable res = CountingHashSet<'T>.Empty
        for e in arr do
            res <- res.Add e
        res
        
    /// Creates a set holding all the given values. (with reference counts)
    static member OfHashMap (map : HashMap<'T, int>) =
        map |> HashMap.filter (fun _ v -> v > 0) |> CountingHashSet
        
    /// Creates a set holding all the given values.
    static member OfHashSet (set : HashSet<'T>) =
        set.MapToMap(fun _ -> 1) |> CountingHashSet

    /// Differentiates two sets returning a HashSetDelta.
    member x.ComputeDelta(other : CountingHashSet<'T>) =
        let inline add (_ : 'T) (_ : int) = ValueSome 1
        let inline rem (_ : 'T) (_ : int) = ValueSome -1
        let inline update (_ : 'T) (_ : int) (_ : int) = ValueNone


        let delta = 
            HashImplementation.MapNode.computeDelta 
                store.Comparer 
                (OptimizedClosures.FSharpFunc<_,_,_>.Adapt rem)
                (OptimizedClosures.FSharpFunc<_,_,_>.Adapt add)
                (OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt update)
                store.Root
                other.Store.Root

        HashMap<'T, int>(store.Comparer, delta) |> HashSetDelta

    /// Same as x.ComputeDelta(empty)
    member x.RemoveAll() =
        store |> HashMap.map (fun _ v -> -1) |> HashSetDelta
        
    /// Same as empty.ComputeDelta(x)
    member x.AddAll() =
        store |> HashMap.map (fun _ v -> 1) |> HashSetDelta

    /// Integrates the given delta into the set, returns a new set and the effective deltas.
    member x.ApplyDelta (deltas : HashSetDelta<'T>) =
        let apply (_k : 'T) (o : voption<int>) (d : int) =
            let o = match o with | ValueSome o -> o | ValueNone -> 0
            let n = d + o

            let delta = 
                if o <= 0 && n > 0 then ValueSome 1
                elif o > 0 && n <= 0 then ValueSome -1
                else ValueNone
             
            let value = 
                 if n <= 0 then ValueNone
                 else ValueSome n

            struct(value, delta)
            
        let s, d = HashMap.ApplyDelta(store, deltas.Store, apply)
        CountingHashSet s, HashSetDelta d

    
    /// Integrates the given delta into the set, returns a new set and the effective deltas.
    member x.ApplyDeltaNoRefCount (deltas : HashSetDelta<'T>) =
        let apply (_k : 'T) (o : voption<int>) (d : int) =
            let o = match o with | ValueSome _ -> 1 | ValueNone -> 0
            let n = 
                if d > 0 then 1 
                elif d < 0 then 0
                else o

            let delta = 
                if o = 0 && n > 0 then ValueSome 1
                elif o > 0 && n = 0 then ValueSome -1
                else ValueNone
             
            let value = 
                 if n <= 0 then ValueNone
                 else ValueSome n

            struct(value, delta)
            
        let s, d = HashMap.ApplyDelta(store, deltas.Store, apply)
        CountingHashSet s, HashSetDelta d

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

    member x.GetEnumerator() = store.GetKeys().GetEnumerator()

    interface IEnumerable with
        member x.GetEnumerator() = store.GetKeys().GetEnumerator() :> _

    interface IEnumerable<'T> with
        member x.GetEnumerator() = store.GetKeys().GetEnumerator() :> _

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
        
    /// Computes the exclusive or for both sets.
    let inline xor (l : CountingHashSet<'T>) (r : CountingHashSet<'T>) =
        l.Xor r

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
    let inline computeDelta (src : CountingHashSet<'T>) (dst : CountingHashSet<'T>) =
        src.ComputeDelta dst

    /// Same as computeDelta src empty.
    let inline removeAll (src : CountingHashSet<'T>) =
        src.RemoveAll()
        
    /// Same as computeDelta empty src.
    let inline addAll (src : CountingHashSet<'T>) =
        src.AddAll()

    /// Integrates the given delta into the set, returns a new set and the effective deltas.
    let inline applyDelta (set : CountingHashSet<'T>) (delta : HashSetDelta<'T>) =
        set.ApplyDelta delta

    /// Integrates the given delta into the set without ref-counting, returns a new set and the effective deltas.
    let inline applyDeltaNoRefCount (set : CountingHashSet<'T>) (delta : HashSetDelta<'T>) =
        set.ApplyDeltaNoRefCount delta
        
    /// Compares two sets.
    let internal compare (l : CountingHashSet<'T>) (r : CountingHashSet<'T>) =
        CountingHashSet.Compare(l,r)

