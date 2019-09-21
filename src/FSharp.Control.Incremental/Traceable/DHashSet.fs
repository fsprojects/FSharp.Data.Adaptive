namespace FSharp.Control.Traceable

open System
open System.Collections
open System.Collections.Generic
open FSharp.Control.Incremental

/// represents the difference of two HashSets.
/// internally uses reference counts to represent deltas and provides 
/// convenient combine functions.
[<Struct; CustomEquality; NoComparison>]
[<StructuredFormatDisplay("{AsString}")>]
type DHashSet<'a>(store : HashMap<'a, int>) =

    /// the monoid instance for DHashSet
    static let monoid =
        {
            mempty = DHashSet<'a>(HashMap.empty)
            mappend = fun l r -> l.Combine r
            misEmpty = fun s -> s.IsEmpty
        }

    /// the empty set.
    static member Empty = DHashSet<'a>(HashMap.empty)
    
    /// the monoid instance for DHashSet
    static member Monoid = monoid

    /// the internal store used by the DHashSet.
    member internal x.Store = store

    /// the number of operations contained in the DHashSet.
    member x.Count = store.Count

    /// is the set empty?
    member x.IsEmpty = store.IsEmpty

    /// adds a SetOperation to the DHashSet.
    member x.Add (op : SetOperation<'a>) =
        if op.Count <> 0 then
            store |> HashMap.alter op.Value (fun o ->
                let n = defaultArg o 0 + op.Count
                if n = 0 then None
                else Some n
            )
            |> DHashSet
        else
            x

    /// removes a SetOperation from the DHashSet.
    member x.Remove (op : SetOperation<'a>) =
        x.Add op.Inverse

    /// combines two DHashSets to one using a reference counting implementation.
    member x.Combine (other : DHashSet<'a>) =
        if store.IsEmpty then 
            other

        elif other.IsEmpty then 
            x

        // factor 5 heuristically determined
        elif store.Count * 5 < other.Count then
            let mutable big = other
            for d in x do
                big <- big.Add d
            big

        elif other.Count * 5 < store.Count then
            let mutable big = x
            for d in other do
                big <- big.Add d
            big
                
        else
            HashMap.choose2 (fun k l r -> 
                let r = Option.defaultValue 0 l + Option.defaultValue 0 r
                if r <> 0 then Some r
                else None
            ) store other.Store
            |> DHashSet

    /// applies the mapping function to all operations in the set.
    member x.Map (mapping : SetOperation<'a> -> SetOperation<'b>) =
        let mutable res = DHashSet<'b>.Empty
        for (k,v) in store do
            res <- res.Add (mapping (SetOperation(k,v)))
        res
        
    /// applies the mapping function to all operations in the set.
    member x.Choose (f : SetOperation<'a> -> Option<SetOperation<'b>>) =
        let mutable res = DHashSet<'b>.Empty
        for (k,v) in store do
            match f (SetOperation(k,v)) with
                | Some r -> res <- res.Add r
                | _ -> ()
        res
        
    /// filters the operations contains using the given predicate.
    member x.Filter (f : SetOperation<'a> -> bool) =
        store |> HashMap.filter (fun k v -> SetOperation(k,v) |> f) |> DHashSet

        
    /// applies the mapping function to all operations in the set and combines all the results.
    member x.Collect (f : SetOperation<'a> -> DHashSet<'b>) =
        let mutable res = DHashSet<'b>.Empty
        for (k,v) in store do
            res <- res.Combine (f (SetOperation(k,v)))
        res

    /// iterates over all operations in the set.
    member x.Iter (f : SetOperation<'a> -> unit) =
        store |> HashMap.iter (fun k v ->
            f (SetOperation(k,v))
        )

    /// folds over the set.
    member x.Fold (seed : 's, f : 's -> SetOperation<'a> -> 's) =
        store |> HashMap.fold (fun s k v ->
            f s (SetOperation(k,v))
        ) seed

    /// checks whether a entry fulfilling the predicate exists.
    member x.Exists (f : SetOperation<'a> -> bool) =
        store |> HashMap.exists (fun k v -> f (SetOperation(k,v)))
        
    /// checks whether all entries fulfill the predicate exists.
    member x.Forall (f : SetOperation<'a> -> bool) =
        store |> HashMap.forall (fun k v -> f (SetOperation(k,v)))

    /// creates a seq containing all operations from the set.
    member x.ToSeq() =
        store.Store |> IntMap.toSeq |> Seq.collect (fun (_hash, values) ->
            values |> Seq.map (fun struct(k,v) -> SetOperation(k,v))
        )

    /// creates a list containing all operations from the set.
    member x.ToList() =
        store.Store |> IntMap.toList |> List.collect (fun (_hash, values) ->
            values |> List.map (fun struct(k,v) -> SetOperation(k,v))
        )
        
    /// creates an array containing all operations from the set.
    member x.ToArray() =
        store |> HashMap.toArray |> Array.map SetOperation

    
    /// creates a HashMap containing all operations from the set.
    /// note that this works in O(1).
    member x.ToMap() = store

    /// creates a DHashSet using the given operations.
    static member OfSeq (seq : seq<SetOperation<'a>>) =
        let mutable res = DHashSet<'a>.Empty
        for e in seq do
            res <- res.Add e
        res
        
    /// creates a DHashSet using the given operations.
    static member OfList (list : list<SetOperation<'a>>) =
        list |> DHashSet.OfSeq
        
    /// creates a DHashSet using the given operations.
    static member OfArray (arr : array<SetOperation<'a>>) =
        arr |> DHashSet.OfSeq
        

    override x.GetHashCode() = store.GetHashCode()
    override x.Equals o =
        match o with
            | :? DHashSet<'a> as o -> store.Equals(o.Store)
            | _ -> false
            

    override x.ToString() =
        let suffix =
            if x.Count > 5 then "; ..."
            else ""

        let content =
            x.ToSeq() |> Seq.truncate 5 |> Seq.map (sprintf "%A") |> String.concat "; "

        "DHashSet [" + content + suffix + "]"

    member private x.AsString = x.ToString()

    interface IEnumerable with
        member x.GetEnumerator() = new DHashSetEnumerator<_>(store) :> _

    interface IEnumerable<SetOperation<'a>> with
        member x.GetEnumerator() = new DHashSetEnumerator<_>(store) :> _

/// special enumerator for DHashSet.
and private DHashSetEnumerator<'a>(store : HashMap<'a, int>) =
    let e = (store :> seq<_>).GetEnumerator()

    member x.Current = 
        let (v,c) = e.Current
        SetOperation(v,c)

    interface IEnumerator with
        member x.MoveNext() = e.MoveNext()
        member x.Current = x.Current :> obj
        member x.Reset() = e.Reset()

    interface IEnumerator<SetOperation<'a>> with
        member x.Dispose() = e.Dispose()
        member x.Current = x.Current

/// functional operators for DHashSet.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DHashSet =
    let inline monoid<'a> = DHashSet<'a>.Monoid

    let inline empty<'a> = DHashSet<'a>.Empty

    let inline isEmpty (set : DHashSet<'a>) = set.IsEmpty
    
    let inline count (set : DHashSet<'a>) = set.Count

    let inline single (op : SetOperation<'a>) =
        DHashSet(HashMap.single op.Value op.Count)

    let inline ofSeq (seq : seq<SetOperation<'a>>) =
        DHashSet.OfSeq seq

    let inline ofList (list : list<SetOperation<'a>>) =
        DHashSet.OfList list

    let inline ofArray (arr : array<SetOperation<'a>>) =
        DHashSet.OfArray arr
        
    let inline ofHashMap (map : HashMap<'a, int>) =
        DHashSet map


    let inline toSeq (set : DHashSet<'a>) =
        set.ToSeq()

    let inline toList (set : DHashSet<'a>) =
        set.ToList()
        
    let inline toArray (set : DHashSet<'a>) =
        set.ToArray()

    let inline toHashMap (set : DHashSet<'a>) =
        set.ToMap()


    let inline add (value : SetOperation<'a>) (set : DHashSet<'a>) =
        set.Add value

    let inline remove (value : SetOperation<'a>) (set : DHashSet<'a>) =
        set.Remove value

    let inline combine (l : DHashSet<'a>) (r : DHashSet<'a>) =
        l.Combine r


    let inline map (f : SetOperation<'a> -> SetOperation<'b>) (set : DHashSet<'a>) =
        set.Map f

    let inline choose (f : SetOperation<'a> -> Option<SetOperation<'b>>) (set : DHashSet<'a>) =
        set.Choose f

    let inline filter (f : SetOperation<'a> -> bool) (set : DHashSet<'a>) =
        set.Filter f

    let inline collect (f : SetOperation<'a> -> DHashSet<'b>) (set : DHashSet<'a>) =
        set.Collect f


    let inline iter (iterator : SetOperation<'a> -> unit) (set : DHashSet<'a>) =
        set.Iter iterator

    let inline exists (predicate : SetOperation<'a> -> bool) (set : DHashSet<'a>) =
        set.Exists predicate

    let inline forall (predicate : SetOperation<'a> -> bool) (set : DHashSet<'a>) =
        set.Forall predicate

    let inline fold (folder : 's -> SetOperation<'a> -> 's) (seed : 's) (set : DHashSet<'a>) =
        set.Fold(seed, folder)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HashSet =
    /// determines the operations needed to transform l into r.
    /// returns a DHashSet containing all the needed operations.
    let differentiate (l : HashSet<'a>) (r : HashSet<'a>) =
        // O(1)
        if Object.ReferenceEquals(l.Store, r.Store) then
            DHashSet.empty

        // O(|r|)
        elif l.IsEmpty then 
            let delta = r.Store |> IntMap.map (List.map (fun v -> struct (v, 1)))
            HashMap(r.Count, delta) |> DHashSet

        // O(|l|)
        elif r.IsEmpty then
            let delta = l.Store |> IntMap.map (List.map (fun v -> struct (v, -1)))
            HashMap(l.Count, delta) |> DHashSet
        
        // TODO: |l|*log|r| and |r|*log|l| implementations should exists
        // which will most likely be faster than |l|+|r| when one of the sets is small.

        // O(|l| + |r|)
        else
            let mutable cnt = 0

            let del (l : list<'a>) =
                l |> List.map (fun v -> cnt <- cnt + 1; struct (v, -1))
            
            let add (l : list<'a>) =
                l |> List.map (fun v -> cnt <- cnt + 1; struct (v, 1))

            let both (_hash : int) (l : list<'a>) (r : list<'a>) =
                HashSetList.mergeWithOption (fun v l r ->
                    if l && not r then cnt <- cnt + 1; Some -1
                    elif r && not l then cnt <- cnt + 1; Some 1
                    else None
                ) l r

            let store = IntMap.computeDelta both (IntMap.map del) (IntMap.map add) l.Store r.Store
            DHashSet(HashMap(cnt, store))
            
    /// applies the given operations to the set. 
    /// returns the new set and the 'effective' operations.
    let integrate (value : HashSet<'a>) (delta : DHashSet<'a>) =
        // O(1)
        if delta.IsEmpty then
            value, delta

        // O(Delta)
        elif value.IsEmpty then
            let mutable maxDelta = 0

            let state = 
                delta.Store.Store |> IntMap.map (
                    List.map (fun struct (k, delta) ->
                        if delta > maxDelta then maxDelta <- delta
                        k
                    )
                )

            let delta = 
                if maxDelta > 1 then delta.Store |> HashMap.map (fun _ _ -> 1)
                else delta.Store

            HashSet(delta.Count, state), DHashSet delta

        // O(Delta * log N)
        else
            let mutable res = value
            let effective =
                delta |> DHashSet.choose (fun d ->
                    let value = d.Value
                    let contained = HashSet.contains value res
                    if contained && d.Count < 0 then
                        res <- HashSet.remove value res
                        Some (Rem value)
                    elif not contained && d.Count > 0 then
                        res <- HashSet.add value res
                        Some (Add value)
                    else
                        None
                )

            res, effective

    type private Traceable<'a> private() =
        static let trace : Traceable<HashSet<'a>, DHashSet<'a>> =
            {
                tempty = HashSet.empty
                tdifferentiate = differentiate
                tintegrate = integrate
                tmonoid = DHashSet.monoid
                tcollapse = fun _set _delta -> false
            }
        static member Instance = trace

    /// the traceable instance for HashSet.
    let trace<'a> = Traceable<'a>.Instance
     