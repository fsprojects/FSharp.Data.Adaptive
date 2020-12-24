namespace FSharp.Data.Adaptive

open System
open System.Collections
open System.Collections.Generic
open FSharp.Data.Adaptive

/// Represents the difference of two HashSets.
/// Internally uses reference counts to represent deltas and provides 
/// convenient combine functions.
[<Struct; CustomEquality; NoComparison>]
[<StructuredFormatDisplay("{AsString}"); CompiledName("FSharpHashSetDelta`1")>]
type HashSetDelta<'T>(store: HashMap<'T, int>) =

    /// The empty set.
    static member Empty = HashSetDelta<'T>(HashMap.empty)

    /// The internal store used by the HashSetDelta.
    member internal x.Store = store

    /// The number of operations contained in the HashSetDelta.
    member x.Count = store.Count

    /// Is the set empty?
    member x.IsEmpty = store.IsEmpty

    /// Adds a SetOperation to the HashSetDelta.
    member x.Add (op: SetOperation<'T>) =
        if op.Count <> 0 then
            store |> HashMap.alter op.Value (fun o ->
                let n = defaultArg o 0 + op.Count
                if n = 0 then None
                else Some n
            )
            |> HashSetDelta
        else
            x

    /// Removes a SetOperation from the HashSetDelta.
    member x.Remove (op: SetOperation<'T>) =
        x.Add op.Inverse

    /// The inverse operations for the given set.
    member x.Inverse =
        store |> HashMap.map (fun _ v -> -v) |> HashSetDelta

    /// Combines two DHashSets to one using a reference counting implementation.
    member x.Combine (other: HashSetDelta<'T>) =
        HashMap<'T, int>.UnionWithValueOption(store, other.Store, fun _ ld rd -> 
            let n = ld + rd
            if n <> 0 then ValueSome n
            else ValueNone
        ) |> HashSetDelta
        //if store.IsEmpty then 
        //    other

        //elif other.IsEmpty then 
        //    x

        //// factor 5 heuristically determined
        //elif store.Count * 5 < other.Count then
        //    let mutable big = other
        //    for d in x do
        //        big <- big.Add d
        //    big

        //elif other.Count * 5 < store.Count then
        //    let mutable big = x
        //    for d in other do
        //        big <- big.Add d
        //    big
                
        //else
        //    HashMap.choose2 (fun k l r -> 
        //        let r = Option.defaultValue 0 l + Option.defaultValue 0 r
        //        if r <> 0 then Some r
        //        else None
        //    ) store other.Store
        //    |> HashSetDelta

    /// Applies the mapping function to all operations in the set.
    member x.Map (mapping: SetOperation<'T> -> SetOperation<'T2>) =
        let mutable res = HashSetDelta<'T2>.Empty
        for (k,v) in store do
            res <- res.Add (mapping (SetOperation(k,v)))
        res
        
    /// Applies the mapping function to all operations in the set.
    member x.Choose (f: SetOperation<'T> -> option<SetOperation<'T2>>) =
        let mutable res = HashSetDelta<'T2>.Empty
        for (k,v) in store do
            match f (SetOperation(k,v)) with
            | Some r -> res <- res.Add r
            | _ -> ()
        res
        
    /// Filters the operations contains using the given predicate.
    member x.Filter (f: SetOperation<'T> -> bool) =
        store |> HashMap.filter (fun k v -> SetOperation(k,v) |> f) |> HashSetDelta
        
    /// Applies the mapping function to all operations in the set and combines all the results.
    member x.Collect (f: SetOperation<'T> -> HashSetDelta<'T2>) =
        let mutable res = HashSetDelta<'T2>.Empty
        for (k,v) in store do
            res <- res.Combine (f (SetOperation(k,v)))
        res

    /// Iterates over all operations in the set.
    member x.Iter (f: SetOperation<'T> -> unit) =
        store |> HashMap.iter (fun k v ->
            f (SetOperation(k,v))
        )

    /// Folds over the set.
    member x.Fold (seed: 'State, f: 'State -> SetOperation<'T> -> 'State) =
        (seed, store) ||> HashMap.fold (fun s k v ->
            f s (SetOperation(k,v))
        ) 

    /// Checks whether an entry fulfilling the predicate exists.
    member x.Exists (f: SetOperation<'T> -> bool) =
        store |> HashMap.exists (fun k v -> f (SetOperation(k,v)))
        
    /// Checks whether all entries fulfill the predicate exists.
    member x.Forall (f: SetOperation<'T> -> bool) =
        store |> HashMap.forall (fun k v -> f (SetOperation(k,v)))

    /// Creates a seq containing all operations from the set.
    member x.ToSeq() =
        store |> Seq.map (fun (k, v) -> SetOperation(k,v))

    /// Creates a list containing all operations from the set.
    member x.ToList() =
        store |> HashMap.toList |> List.map (fun (k, v) -> SetOperation(k,v))

        
    /// Creates an array containing all operations from the set.
    member x.ToArray() =
        store |> HashMap.toArray |> Array.map SetOperation

    
    /// Creates a HashMap containing all operations from the set.
    /// Note that this works in O(1).
    member x.ToMap() = store

    /// Creates a HashSetDelta using the given operations.
    static member OfSeq (seq: seq<SetOperation<'T>>) =
        let mutable res = HashSetDelta<'T>.Empty
        for e in seq do
            res <- res.Add e
        res
        
    /// Creates a HashSetDelta using the given operations.
    static member OfList (list: list<SetOperation<'T>>) =
        list |> HashSetDelta.OfSeq
        
    /// Creates a HashSetDelta using the given operations.
    static member OfArray (arr: array<SetOperation<'T>>) =
        arr |> HashSetDelta.OfSeq
        

    override x.GetHashCode() = store.GetHashCode()
    override x.Equals o =
        match o with
        | :? HashSetDelta<'T> as o -> DefaultEquality.equals store o.Store
        | _ -> false
            

    override x.ToString() =
        let suffix =
            if x.Count > 5 then "; ..."
            else ""

        let content =
            x.ToSeq() |> Seq.truncate 5 |> Seq.map (sprintf "%A") |> String.concat "; "

        "HashSetDelta [" + content + suffix + "]"

    member private x.AsString = x.ToString()

    member x.GetEnumerator() = new HashSetDeltaEnumerator<_>(store) 

    interface IEnumerable with
        member x.GetEnumerator() = new HashSetDeltaEnumerator<_>(store) :> _

    interface IEnumerable<SetOperation<'T>> with
        member x.GetEnumerator() = new HashSetDeltaEnumerator<_>(store) :> _

/// Special enumerator for HashSetDelta.
and HashSetDeltaEnumerator<'T> =
    struct // Array Buffer (with re-use) + Inline Stack Head
        val mutable private Root : HashMapNode<'T, int>
        val mutable private Head : HashMapNode<'T, int>
        val mutable private Tail : list<HashMapNode<'T, int>>
        val mutable private BufferValueCount : int
        val mutable private Values : struct('T * int)[]
        val mutable private Index : int
        
        member x.Collect() =
            match x.Head with
            | :? HashMapNoCollisionLeaf<'T, int> as h ->
                x.Values.[0] <- struct(h.Key, h.Value)
                x.Index <- 0
                x.BufferValueCount <- 1
                if x.Tail.IsEmpty then
                    x.Head <- Unchecked.defaultof<_>
                    x.Tail <- []
                else
                    x.Head <- x.Tail.Head
                    x.Tail <- x.Tail.Tail    
                true

            | :? HashMapCollisionLeaf<'T, int> as h ->
                                    
                let cnt = h.Count
                if x.Values.Length < cnt then
                    x.Values <- Array.zeroCreate cnt
            
                x.Values.[0] <- struct(h.Key, h.Value)
                let mutable c = h.Next
                let mutable i = 0
                while not (isNull c) do
                    x.Values.[i] <- struct(c.Key, c.Value)
                    c <- c.Next
                    i <- i + 1
                x.BufferValueCount <- cnt
                x.Index <- 0
                if x.Tail.IsEmpty then
                    x.Head <- Unchecked.defaultof<_>
                    x.Tail <- []
                else
                    x.Head <- x.Tail.Head
                    x.Tail <- x.Tail.Tail
                true

            | :? HashMapInner<'T, int> as h ->
                if h._Count <= 16 then
                    h.CopyToV(x.Values, 0) |> ignore
                    x.BufferValueCount <- h._Count
                    x.Index <- 0
                    if x.Tail.IsEmpty then
                        x.Head <- Unchecked.defaultof<_>
                        x.Tail <- []
                    else
                        x.Head <- x.Tail.Head
                        x.Tail <- x.Tail.Tail
                    true
                else
                    x.Head <- h.Left
                    x.Tail <- h.Right :: x.Tail
                    x.Collect()
                
            | _ -> false

        member x.MoveNext() =
            x.Index <- x.Index + 1
            if x.Index < x.BufferValueCount then
                true
            else
                x.Collect()

        member x.Reset() =
            x.Index <- -1
            x.Head <- x.Root
            x.Tail <- []
            x.BufferValueCount <- -1

        member x.Dispose() =
            x.Values <- null
            x.Index <- -1
            x.Head <- Unchecked.defaultof<_>
            x.Tail <- []
            x.Root <- Unchecked.defaultof<_>

        member x.Current : SetOperation<'T> = 
            let struct(v,c) = x.Values.[x.Index]
            SetOperation(v,c)

        interface System.Collections.IEnumerator with
            member x.MoveNext() = x.MoveNext()
            member x.Reset() = x.Reset()
            member x.Current = x.Current :> obj
            
        interface System.Collections.Generic.IEnumerator<SetOperation<'T>> with
            member x.Current = x.Current
            member x.Dispose() = x.Dispose()

        new (map : HashMap<'T, int>) =
            let cnt = map.Count
            {
                Root = map.Root
                Head = map.Root
                Tail = []
                Values = if cnt > 0 then Array.zeroCreate (min cnt 16) else null
                Index = -1
                BufferValueCount = -1
            }
    end


/// Functional operators for HashSetDelta.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); CompiledName("FSharpHashSetDeltaModule")>]
module HashSetDelta =

    /// The empty set.
    [<GeneralizableValue>]
    let inline empty<'T> = HashSetDelta<'T>.Empty

    /// The inverse operations for the given set.
    let inline inverse (set: HashSetDelta<'T>) = set.Inverse

    /// Is the set empty?
    let inline isEmpty (set: HashSetDelta<'T>) = set.IsEmpty
    
    /// The number of operations contained in the HashSetDelta.
    let inline count (set: HashSetDelta<'T>) = set.Count

    /// Creates a set from a single operation.
    let inline single (op: SetOperation<'T>) =
        HashSetDelta(HashMap.single op.Value op.Count)

    /// Creates a HashSetDelta using the given operations.
    let inline ofSeq (seq: seq<SetOperation<'T>>) =
        HashSetDelta.OfSeq seq

    /// Creates a HashSetDelta using the given operations.
    let inline ofList (list: list<SetOperation<'T>>) =
        HashSetDelta.OfList list

    /// Creates a HashSetDelta using the given operations.
    let inline ofArray (arr: array<SetOperation<'T>>) =
        HashSetDelta.OfArray arr
        
    /// Creates a HashSetDelta using the given operations.
    /// Note that the values from the map are interpreted as reference-deltas and should therefore not be 0.
    let inline ofHashMap (map: HashMap<'T, int>) =
        HashSetDelta map

    /// Creates a seq containing all operations from the set.
    let inline toSeq (set: HashSetDelta<'T>) =
        set.ToSeq()

    /// Creates a list containing all operations from the set.
    let inline toList (set: HashSetDelta<'T>) =
        set.ToList()
        
    /// Creates an array containing all operations from the set.
    let inline toArray (set: HashSetDelta<'T>) =
        set.ToArray()

    /// Creates a HashMap containing all operations from the set.
    /// Note that this works in O(1).
    let inline toHashMap (set: HashSetDelta<'T>) =
        set.ToMap()

    /// Adds a SetOperation to the HashSetDelta.
    let inline add (value: SetOperation<'T>) (set: HashSetDelta<'T>) =
        set.Add value

    /// Removes a SetOperation from the HashSetDelta.
    let inline remove (value: SetOperation<'T>) (set: HashSetDelta<'T>) =
        set.Remove value

    /// Combines two DHashSets to one using a reference counting implementation.
    let inline combine (l: HashSetDelta<'T>) (r: HashSetDelta<'T>) =
        l.Combine r

    /// Applies the mapping function to all operations in the set.
    let inline map (f: SetOperation<'T> -> SetOperation<'T2>) (set: HashSetDelta<'T>) =
        set.Map f

    /// Applies the mapping function to all operations in the set.
    let inline choose (f: SetOperation<'T> -> option<SetOperation<'T2>>) (set: HashSetDelta<'T>) =
        set.Choose f

    /// Filters the operations contains using the given predicate.
    let inline filter (f: SetOperation<'T> -> bool) (set: HashSetDelta<'T>) =
        set.Filter f

    /// Applies the mapping function to all operations in the set and combines all the results.
    let inline collect (f: SetOperation<'T> -> HashSetDelta<'T2>) (set: HashSetDelta<'T>) =
        set.Collect f

    /// Iterates over all operations in the set.
    let inline iter (iterator: SetOperation<'T> -> unit) (set: HashSetDelta<'T>) =
        set.Iter iterator

    /// Checks whether an entry fulfilling the predicate exists.
    let inline exists (predicate: SetOperation<'T> -> bool) (set: HashSetDelta<'T>) =
        set.Exists predicate

    /// Checks whether all entries fulfill the predicate exists.
    let inline forall (predicate: SetOperation<'T> -> bool) (set: HashSetDelta<'T>) =
        set.Forall predicate

    /// Folds over the set.
    let inline fold (folder: 'State -> SetOperation<'T> -> 'State) (seed: 'State) (set: HashSetDelta<'T>) =
        set.Fold(seed, folder)
