namespace FsIncremental

open System
open System.Threading
open System.Runtime.CompilerServices
open System.Collections
open System.Collections.Generic

/// represents a weak reference to an object with proper hash/equality implementation
type WeakRef<'a when 'a : not struct> private(store : WeakReference<'a>, hash : int) =
    
    /// tries to dereference the WeakRef and returns the (optional) result
    member x.Object =
        match store.TryGetTarget() with
        | (true, v) -> Some v
        | _ -> None


    /// creates a new WeakRef holding the given object
    new(obj : 'a) =
        // important to use a reference-hash here
        let hash = RuntimeHelpers.GetHashCode(obj)
        WeakRef<'a>(WeakReference<'a>(obj), hash)

    override x.ToString() =
        match store.TryGetTarget() with
        | (true, v) -> sprintf "WeakRef(%A)" v
        | _ -> sprintf "WeakRef(%d)" hash

    override x.GetHashCode() = 
        hash

    override x.Equals o =
        if Object.ReferenceEquals(x, o) then
            true
        else
            match o with
            | :? WeakRef<'a> as o -> 
                if o.GetHashCode() = hash then
                    match x.Object with
                    | Some a ->
                        match o.Object with
                        | Some b -> Object.ReferenceEquals(a, b)
                        | None -> false
                    | None ->
                        match o.Object with
                        | Some b -> false
                        | None -> true
                else
                    false
            | _ -> 
                false


/// represents a weak reference to an object with proper hash/equality implementation
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module WeakRef =
    /// creates a new WeakRef holding the given object
    let inline create (obj : 'a) = WeakRef obj

    /// tries to dereference the WeakRef and returns the (optional) result
    let inline deref (ref : WeakRef<'a>) = ref.Object

/// represents a set of weakly referenced objects.
/// NOTE that WeakSet uses reference-equality for objects
type WeakSet<'a when 'a : not struct> private(store : HashSet<WeakRef<'a>>) =

    /// checks whether the WeakSet contains the given object
    member x.Contains(value : 'a) =
        let w = WeakRef<'a>(value)
        lock store (fun () -> store.Contains w)

    /// adds an object to the WeakSet
    member x.Add(obj : 'a) =
        let w = WeakRef<'a>(obj)
        lock store (fun () -> store.Add w)
        
    /// removes an object from the WeakSet
    member x.Remove(obj : 'a) =
        let w = WeakRef<'a>(obj)
        lock store (fun () -> store.Remove w)
        
    /// removes all objects from the WeakSet
    member x.Clear() =
        lock store (fun () -> store.Clear())

    /// returns a list holding all living objects from the WeakSet
    member x.ToList() =
        let mutable res = []
        for w in store do
            match w.Object with
            | Some t -> res <- t :: res
            | None -> ()
        res
        
    /// returns an array holding all living objects from the WeakSet
    member x.ToArray() =
        let mutable res = [||]
        let mutable cnt = 0
        for w in store do
            match w.Object with
            | Some t -> 
                if cnt >= res.Length then 
                    Array.Resize(&res, res.Length <<< 1)
                res.[cnt] <- t
            | None -> 
                ()
        if res.Length > cnt then 
            Array.Resize(&res, cnt)
        res

    member x.GetEnumerator() = 
        new WeakSetEnumerator<'a>(store) :> IEnumerator<'a>

    interface IEnumerable with
        member x.GetEnumerator() = x.GetEnumerator() :> IEnumerator

    interface IEnumerable<'a> with
        member x.GetEnumerator() = x.GetEnumerator()

    /// creates a new empty WeakSet
    new() = 
        WeakSet<'a>(HashSet())
        
    /// creates a new WeakSet holding the given objects
    new(objs : seq<'a>) = 
        WeakSet<'a>(
            HashSet<WeakRef<'a>>(objs |> Seq.map WeakRef)
        )

and private WeakSetEnumerator<'a when 'a : not struct>(set : HashSet<WeakRef<'a>>) =
    let mutable e = set.GetEnumerator()
    let mutable current = Unchecked.defaultof<'a>

    member x.MoveNext() =
        if e.MoveNext() then
            match e.Current.Object with
            | Some v -> 
                current <- v
                true
            | None ->
                x.MoveNext()
        else
            false

    member x.Current = current

    member x.Reset() =
        (e :> IEnumerator).Reset()
        current <- Unchecked.defaultof<'a>

    member x.Dispose() =
        e.Dispose()
        current <- Unchecked.defaultof<'a>

    interface IEnumerator with
        member x.MoveNext() = x.MoveNext()
        member x.Reset() = x.Reset()
        member x.Current = x.Current :> obj

    interface IEnumerator<'a> with
        member x.Dispose() = x.Dispose()
        member x.Current = x.Current

/// represents a set of weakly referenced objects.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module WeakSet =
    /// creates a new empty WeakSet
    let inline create() = WeakSet()

    /// creates a new WeakSet holding the given objects
    let inline ofSeq (s : seq<'a>) = WeakSet s

    /// creates a new WeakSet holding the given objects
    let inline ofList (s : list<'a>) = WeakSet s

    /// creates a new WeakSet holding the given objects
    let inline ofArray (s : 'a[]) = WeakSet s

    /// creates a seq holding all living objects from the WeakSet
    let inline toSeq (set : WeakSet<'a>) = set :> seq<'a>

    /// creates a list holding all living objects from the WeakSet
    let inline toList (set : WeakSet<'a>) = set.ToList()

    /// creates an array holding all living objects from the WeakSet
    let inline toArray (set : WeakSet<'a>) = set.ToArray()

    /// adds an object to the WeakSet
    let inline add (obj : 'a) (set : WeakSet<'a>) = set.Add obj

    /// removes an object from the WeakSet
    let inline remove (obj : 'a) (set : WeakSet<'a>) = set.Remove obj

    /// removes all objects from the WeakSet
    let inline clear (set : WeakSet<'a>) = set.Clear()
