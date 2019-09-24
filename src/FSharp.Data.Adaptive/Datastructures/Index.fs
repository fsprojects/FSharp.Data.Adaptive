namespace FSharp.Data.Adaptive

open System
open System.Threading
open System.Collections.Generic

/// the internal implementation of our order-maintenance structure.
[<StructuredFormatDisplay("{AsString}")>]
type IndexNode =
    class
        /// root-node for this cycle
        val mutable public Root : IndexNode
        /// prev node in the cycle
        val mutable public Prev : IndexNode
        /// next node in the cycle
        val mutable public Next : IndexNode
        /// the current tag (must only be modified when holding a lock to the Value)
        val mutable public Tag : uint64
        /// the current reference count of the value (used for tracking disposal)
        val mutable public RefCount : int

        /// relabel a part of the list starting at start until distance(start, a) >= cnt^2 + 1.
        /// this should ensure amortized costs of O(log N) for insert.
        static member private Relabel(start : IndexNode) =
            let all = List<IndexNode>()

            let distance (l : IndexNode) (r : IndexNode) =
                if l = r then UInt64.MaxValue
                else r.Tag - l.Tag

            // assuming distance start start.Next == 1
            let mutable current = start.Next
            all.Add start.Next
            Monitor.Enter start.Next

            let mutable cnt = 1UL
            while distance start current < 1UL + cnt * cnt do
                current <- current.Next
                cnt <- cnt + 1UL
                all.Add current
                Monitor.Enter current

            let space = distance start current

            // the last node does not get relabeled
            current <- current.Prev
            all.RemoveAt (all.Count - 1)
            Monitor.Exit current.Next
            cnt <- cnt - 1UL

            let step = space / (1UL + cnt)
            let mutable current = start.Tag + step
            for n in all do
                n.Tag <- current
                current <- current + step
                Monitor.Exit n
                    
            step

        /// the current sort-label for the node
        member x.Key = x.Tag - x.Root.Tag

        /// insert a node directly after this one.
        member x.InsertAfter() =
            lock x (fun () ->
                let next = x.Next
                    
                /// initially we increment the distance by Uint64.MaxValue/2
                let mutable distance = 
                    if next = x then UInt64.MaxValue
                    else next.Tag - x.Tag

                // we're out of luck and there's no space for an additional node.
                if distance = 1UL then
                    distance <- IndexNode.Relabel x
                        
                // put the new node in between me and the next.
                let key = x.Tag + (distance / 2UL)
                let res = IndexNode(x.Root, Prev = x, Next = x.Next, Tag = key)

                // link the node
                next.Prev <- res
                x.Next <- res

                res
            )

        /// delete a node from the cycle.
        member x.Delete() =
            let prev = x.Prev
            Monitor.Enter prev
            if prev.Next <> x then
                Monitor.Exit prev
                x.Delete()
            else
                Monitor.Enter x
                try
                    if x.RefCount = 1 then
                        prev.Next <- x.Next
                        x.Next.Prev <- prev
                        x.RefCount <- 0
                    else
                        x.RefCount <- x.RefCount - 1

                finally
                    Monitor.Exit x
                    Monitor.Exit prev

        /// add a reference to the node.
        member x.AddRef() =
            lock x (fun () ->
                x.RefCount <- x.RefCount + 1
            )

        /// compare me to another node.
        member x.CompareTo(o : IndexNode) =
            match Monitor.TryEnter x, Monitor.TryEnter o with
                | true, true ->
                    try 
                        compare x.Key o.Key
                    finally
                        Monitor.Exit x
                        Monitor.Exit o

                | true, false ->
                    Monitor.Exit x
                    x.CompareTo o

                | false, true ->
                    Monitor.Exit o
                    x.CompareTo o

                | false, false ->
                    x.CompareTo o

        interface IComparable with
            member x.CompareTo (o : obj) =
                match o with
                    | :? IndexNode as o -> x.CompareTo o
                    | _ -> failwithf "[Real] cannot compare real to %A" o

        override x.GetHashCode() = System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(x)
        override x.Equals o = System.Object.ReferenceEquals(x,o)
        override x.ToString() = sprintf "%f" (float x.Key / float UInt64.MaxValue)
        member private x.AsString = x.ToString()

        new(root : IndexNode) = { Root = root; Prev = Unchecked.defaultof<_>; Next = Unchecked.defaultof<_>; Tag = 0UL; RefCount = 0 }
    end
        

/// datastructure representing an abstract index.
/// supported operations are: Index.zero, Index.after(index), Index.before(index), Index.between(l, r).
/// this is a 'simple' solution to the order-maintenance problem that has insert in O(log N), delete in O(1) and compare in O(1).
/// Note that the implementation is quite obfuscated due to concurrency.
[<Sealed; StructuredFormatDisplay("{AsString}")>]
type Index private(real : IndexNode) =
    do real.AddRef()

    /// we create a thread for deleting the underlying values when Index goes out of scope.
    /// this way we avoid blocking the finalizer for Index.
    static let startThread (run : unit -> unit) =
        let start = System.Threading.ThreadStart(run)
        let thread = System.Threading.Thread(start, IsBackground = true, Name = "Index cleanup thread")
        thread.Start()
        thread

    static let queue = new System.Collections.Concurrent.BlockingCollection<IndexNode>()
    static let runner =
        startThread (fun () -> 
            while true do
                try queue.Take().Delete()
                with e -> printfn "Index cleanup thread failed with: %A" e
        )

    member private x.Value = real

    member x.After() =
        lock real (fun () ->
            if real.Next <> real.Root then Index real.Next 
            else Index (real.InsertAfter()) 
        )   

    member x.Before() =
        let prev = real.Prev
        Monitor.Enter prev
        if prev.Next <> real then
            Monitor.Exit prev
            x.Before()
        else
            try
                if prev = real.Root then 
                    prev.InsertAfter() |> Index
                else
                    prev |> Index
            finally
                Monitor.Exit prev

    member l.Between(r : Index) =
        let l = l.Value
        let r = r.Value
        Monitor.Enter l
        try
            if l.Next = r then l.InsertAfter() |> Index
            else l.Next |> Index
        finally
            Monitor.Exit l

    override x.Finalize() =
        queue.Add real

    member x.CompareTo (o : Index) = real.CompareTo(o.Value)

    override x.GetHashCode() = real.GetHashCode()
    override x.Equals o =
        match o with
            | :? Index as o -> real.Equals o.Value
            | _ -> false
                
    override x.ToString() = real.ToString()
    member private x.AsString = x.ToString()

    interface IComparable with
        member x.CompareTo(o : obj) = 
            match o with
            | :? Index as o -> x.CompareTo o
            | _ -> 0
        
    interface IComparable<Index> with
        member x.CompareTo(o : Index) = x.CompareTo o

    new() = 
        let r = IndexNode(Unchecked.defaultof<_>)
        r.Root <- r
        r.Next <- r
        r.Prev <- r
        Index(r)

/// functional operators for the Index datastructure.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Index =
    let private root = Index()

    /// the root index.
    let zero = root.After()

    /// gets an index after the given one.
    let after (r : Index) = r.After()

    /// gets an index before the given one.
    let before (r : Index) = r.Before()

    /// gets an index between the given ones.
    let between (l : Index) (r : Index) = l.Between r
