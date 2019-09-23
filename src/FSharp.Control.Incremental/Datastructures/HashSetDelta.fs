namespace FSharp.Control.Incremental

open System
open System.Collections
open System.Collections.Generic
open FSharp.Control.Incremental

/// represents the difference of two HashSets.
/// internally uses reference counts to represent deltas and provides 
/// convenient combine functions.
[<Struct; CustomEquality; NoComparison>]
[<StructuredFormatDisplay("{AsString}")>]
type HashSetDelta<'a>(store : HashMap<'a, int>) =

    /// the empty set.
    static member Empty = HashSetDelta<'a>(HashMap.empty)

    /// the internal store used by the HashSetDelta.
    member internal x.Store = store

    /// the number of operations contained in the HashSetDelta.
    member x.Count = store.Count

    /// is the set empty?
    member x.IsEmpty = store.IsEmpty

    /// adds a SetOperation to the HashSetDelta.
    member x.Add (op : SetOperation<'a>) =
        if op.Count <> 0 then
            store |> HashMap.alter op.Value (fun o ->
                let n = defaultArg o 0 + op.Count
                if n = 0 then None
                else Some n
            )
            |> HashSetDelta
        else
            x

    /// removes a SetOperation from the HashSetDelta.
    member x.Remove (op : SetOperation<'a>) =
        x.Add op.Inverse

    /// the inverse operations for the given set.
    member x.Inverse =
        store |> HashMap.map (fun _ v -> -v) |> HashSetDelta

    /// combines two DHashSets to one using a reference counting implementation.
    member x.Combine (other : HashSetDelta<'a>) =
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
            |> HashSetDelta

    /// applies the mapping function to all operations in the set.
    member x.Map (mapping : SetOperation<'a> -> SetOperation<'b>) =
        let mutable res = HashSetDelta<'b>.Empty
        for (k,v) in store do
            res <- res.Add (mapping (SetOperation(k,v)))
        res
        
    /// applies the mapping function to all operations in the set.
    member x.Choose (f : SetOperation<'a> -> option<SetOperation<'b>>) =
        let mutable res = HashSetDelta<'b>.Empty
        for (k,v) in store do
            match f (SetOperation(k,v)) with
                | Some r -> res <- res.Add r
                | _ -> ()
        res
        
    /// filters the operations contains using the given predicate.
    member x.Filter (f : SetOperation<'a> -> bool) =
        store |> HashMap.filter (fun k v -> SetOperation(k,v) |> f) |> HashSetDelta

        
    /// applies the mapping function to all operations in the set and combines all the results.
    member x.Collect (f : SetOperation<'a> -> HashSetDelta<'b>) =
        let mutable res = HashSetDelta<'b>.Empty
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

    /// checks whether an entry fulfilling the predicate exists.
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

    /// creates a HashSetDelta using the given operations.
    static member OfSeq (seq : seq<SetOperation<'a>>) =
        let mutable res = HashSetDelta<'a>.Empty
        for e in seq do
            res <- res.Add e
        res
        
    /// creates a HashSetDelta using the given operations.
    static member OfList (list : list<SetOperation<'a>>) =
        list |> HashSetDelta.OfSeq
        
    /// creates a HashSetDelta using the given operations.
    static member OfArray (arr : array<SetOperation<'a>>) =
        arr |> HashSetDelta.OfSeq
        

    override x.GetHashCode() = store.GetHashCode()
    override x.Equals o =
        match o with
            | :? HashSetDelta<'a> as o -> store.Equals(o.Store)
            | _ -> false
            

    override x.ToString() =
        let suffix =
            if x.Count > 5 then "; ..."
            else ""

        let content =
            x.ToSeq() |> Seq.truncate 5 |> Seq.map (sprintf "%A") |> String.concat "; "

        "HashSetDelta [" + content + suffix + "]"

    member private x.AsString = x.ToString()

    interface IEnumerable with
        member x.GetEnumerator() = new DHashSetEnumerator<_>(store) :> _

    interface IEnumerable<SetOperation<'a>> with
        member x.GetEnumerator() = new DHashSetEnumerator<_>(store) :> _

/// special enumerator for HashSetDelta.
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

/// functional operators for HashSetDelta.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HashSetDelta =

    /// the empty set.
    [<GeneralizableValue>]
    let inline empty<'a> = HashSetDelta<'a>.Empty

    /// the inverse operations for the given set.
    let inline inverse (set : HashSetDelta<'a>) = set.Inverse

    /// is the set empty?
    let inline isEmpty (set : HashSetDelta<'a>) = set.IsEmpty
    
    /// the number of operations contained in the HashSetDelta.
    let inline count (set : HashSetDelta<'a>) = set.Count

    /// creates a set from a single operation.
    let inline single (op : SetOperation<'a>) =
        HashSetDelta(HashMap.single op.Value op.Count)

    /// creates a HashSetDelta using the given operations.
    let inline ofSeq (seq : seq<SetOperation<'a>>) =
        HashSetDelta.OfSeq seq

    /// creates a HashSetDelta using the given operations.
    let inline ofList (list : list<SetOperation<'a>>) =
        HashSetDelta.OfList list

    /// creates a HashSetDelta using the given operations.
    let inline ofArray (arr : array<SetOperation<'a>>) =
        HashSetDelta.OfArray arr
        
    /// creates a HashSetDelta using the given operations.
    /// note that the values from the map are interpreted as reference-deltas and should therefore not be 0.
    let inline ofHashMap (map : HashMap<'a, int>) =
        HashSetDelta map

    /// creates a seq containing all operations from the set.
    let inline toSeq (set : HashSetDelta<'a>) =
        set.ToSeq()

    /// creates a list containing all operations from the set.
    let inline toList (set : HashSetDelta<'a>) =
        set.ToList()
        
    /// creates an array containing all operations from the set.
    let inline toArray (set : HashSetDelta<'a>) =
        set.ToArray()

    /// creates a HashMap containing all operations from the set.
    /// note that this works in O(1).
    let inline toHashMap (set : HashSetDelta<'a>) =
        set.ToMap()

    /// adds a SetOperation to the HashSetDelta.
    let inline add (value : SetOperation<'a>) (set : HashSetDelta<'a>) =
        set.Add value

    /// removes a SetOperation from the HashSetDelta.
    let inline remove (value : SetOperation<'a>) (set : HashSetDelta<'a>) =
        set.Remove value

    /// combines two DHashSets to one using a reference counting implementation.
    let inline combine (l : HashSetDelta<'a>) (r : HashSetDelta<'a>) =
        l.Combine r

    /// applies the mapping function to all operations in the set.
    let inline map (f : SetOperation<'a> -> SetOperation<'b>) (set : HashSetDelta<'a>) =
        set.Map f

    /// applies the mapping function to all operations in the set.
    let inline choose (f : SetOperation<'a> -> option<SetOperation<'b>>) (set : HashSetDelta<'a>) =
        set.Choose f

    /// filters the operations contains using the given predicate.
    let inline filter (f : SetOperation<'a> -> bool) (set : HashSetDelta<'a>) =
        set.Filter f

    /// applies the mapping function to all operations in the set and combines all the results.
    let inline collect (f : SetOperation<'a> -> HashSetDelta<'b>) (set : HashSetDelta<'a>) =
        set.Collect f

    /// iterates over all operations in the set.
    let inline iter (iterator : SetOperation<'a> -> unit) (set : HashSetDelta<'a>) =
        set.Iter iterator

    /// checks whether an entry fulfilling the predicate exists.
    let inline exists (predicate : SetOperation<'a> -> bool) (set : HashSetDelta<'a>) =
        set.Exists predicate

    /// checks whether all entries fulfill the predicate exists.
    let inline forall (predicate : SetOperation<'a> -> bool) (set : HashSetDelta<'a>) =
        set.Forall predicate

    /// folds over the set.
    let inline fold (folder : 's -> SetOperation<'a> -> 's) (seed : 's) (set : HashSetDelta<'a>) =
        set.Fold(seed, folder)
