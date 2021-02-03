namespace FSharp.Data.Adaptive

open System
open System.Threading
open System.Collections.Generic

/// the internal implementation of our order-maintenance structure.
[<StructuredFormatDisplay("{AsString}")>]
type IndexNode =
    class

        /// Root-node for this cycle
        val mutable public Root : IndexNode

        /// Prev node in the cycle
        val mutable public Prev : IndexNode

        /// Next node in the cycle
        val mutable public Next : IndexNode

        /// the current tag (must only be modified when holding a lock to the Value)
        val mutable public Tag : uint64

        /// The current reference count of the value (used for tracking disposal)
        val mutable public RefCount : int

        /// Relabel a part of the list starting at start until distance(start, a) >= cnt^2 + 1.
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

        /// Delete a node from the cycle.
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

        /// Compare me to another node.
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
        
#if !FABLE_COMPILER
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

#else

type largeuint(data : uint32[]) =
    static let ceilDiv32 (v : int) =
        if v < 0 then 0
        elif v &&& 31 = 0 then v  /32
        else 1 + v / 32

    static let trim (arr : uint32[]) =
        let mutable off = 0
        while off < arr.Length && arr.[off] = 0u do
            off <- off + 1
        if off > 0 then Array.skip off arr
        else arr

    let data = trim data
    member x.Data = data
    member x.Bits = 32 * data.Length

    static member Zero = largeuint([||])
    static member One = largeuint([| 1u |])
    
    new (v : uint8) = largeuint [| uint32 v |]
    new (v : int8) = largeuint [| uint32 v |]
    new (v : uint16) = largeuint [| uint32 v |]
    new (v : int16) = largeuint [| uint32 v |]
    new (v : uint32) = largeuint [| v |]
    new (v : int) = largeuint [| uint32 v |]
    new (v : uint64) = largeuint [| uint32 (v >>> 32); uint32 v |]
    new (v : int64) = largeuint [| uint32 (v >>> 32); uint32 v |]

    member x.IsZero = data.Length = 0

    member x.GetBit(i : int) : uint32 =
        let slot = data.Length - 1 - (i >>> 5)
        let inSlot = i &&& 31

        if slot >= 0 && slot <= data.Length then
            (data.[slot] >>> inSlot) &&& 1u
        else
            0u

    override x.ToString() =
        data |> Seq.mapi (fun i b -> if i = 0 then sprintf "%X" b else sprintf "%08X" b) |> String.concat ""

    static member (<<<) (l : largeuint, r : int) : largeuint =
        if r = 0 then l
        elif r < 0 then l >>> -r
        else
            let maxBits = 32 * l.Data.Length + r
            let res = Array.zeroCreate (ceilDiv32 maxBits)
            let shift = r &&& 31
            if shift = 0 then
                let mutable ri = res.Length - 1 - (r >>> 5)
                let mutable li = l.Data.Length - 1
                while li >= 0 && ri >= 0 do 
                    res.[ri] <- l.Data.[li]
                    ri <- ri - 1
                    li <- li - 1
                largeuint res

            else
                let mutable ri = res.Length - 1 - (r >>> 5)
                let mutable li = l.Data.Length - 1
                let mutable c = 0u
                while li >= 0 && ri >= 0 do 
                    res.[ri] <- (l.Data.[li] <<< shift) ||| c
                    c <- l.Data.[li] >>> (32-shift)
                    ri <- ri - 1
                    li <- li - 1

                if ri >= 0 && c <> 0u then
                    res.[ri] <- c

                largeuint res

    static member (>>>) (l : largeuint, r : int) =
        if r = 0 then l
        elif r < 0 then l <<< -r
        else
            let maxBits = 32 * l.Data.Length - r
            let res = Array.zeroCreate (ceilDiv32 maxBits)
            let shift = r &&& 31
            if shift = 0 then
                let mutable ri = 0
                let mutable li = 0
                while li < l.Data.Length && ri < res.Length do 
                    res.[ri] <- l.Data.[li]
                    ri <- ri + 1
                    li <- li + 1
                
                largeuint res
                
            else
                let mask = (1u <<< shift) - 1u
                let mutable ri = 0
                let mutable li = 0
                let mutable c = 0u
                while li < l.Data.Length && ri < res.Length do 
                    res.[ri] <- (l.Data.[li] >>> shift) ||| c
                    c <- (l.Data.[li] &&& mask) <<< (32 - shift)
                    ri <- ri + 1
                    li <- li + 1
                

                largeuint res

    static member (+) (l : largeuint, r : largeuint) : largeuint =
        let bits = 1 + max l.Bits r.Bits
        let res = Array.zeroCreate (ceilDiv32 bits)

        let mutable li = l.Data.Length-1
        let mutable ri = r.Data.Length-1
        let mutable oi = res.Length-1
        let mutable c = 0u
        while li >= 0 && ri >= 0 do
            let v = float l.Data.[li] + float r.Data.[ri] + float c

            res.[oi] <- uint32 v
            c <- if v > 4294967295.0 then 1u else 0u
            li <- li - 1
            ri <- ri - 1
            oi <- oi - 1

        while li >= 0 do
            let v = float l.Data.[li] + float c
            res.[oi] <- uint32 v
            c <- if v > 4294967295.0 then 1u else 0u
            li <- li - 1
            oi <- oi - 1

        while ri >= 0 do
            let v = float r.Data.[ri] + float c
            res.[oi] <- uint32 v
            c <- if v > 4294967295.0 then 1u else 0u
            ri <- ri - 1
            oi <- oi - 1

        while oi >= 0 do
            let v = float c
            c <- if v > 4294967295.0 then 1u else 0u
            res.[oi] <- uint32 v
            oi <- oi - 1

        largeuint res

    /// l + 1 =?= r
    static member DistanceIsOne(l : largeuint, r : largeuint) =
        let res = ceilDiv32 (1 + max l.Bits r.Bits)
        let mutable lPlusOne = true
        let mutable rPlusOne = true
        let mutable li = l.Data.Length-1
        let mutable ri = r.Data.Length-1
        let mutable oi = res - 1
        let mutable lc = 1u
        let mutable rc = 1u

        let inline getl (i : int) =
            if i >= 0 then l.Data.[i]
            else 0u

        let inline getr (i : int) =
            if i >= 0 then r.Data.[i]
            else 0u
            

        while oi >= 0 && (lPlusOne || rPlusOne) do
            let ld = getl li
            let rd = getr ri

            if lPlusOne then
                let l1 = float ld + float lc
                if uint32 l1 <> getr ri then 
                    //printfn "l + 1 <> r (%A <> %A) %d" (uint32 l1) (getr ri) oi
                    lPlusOne <- false
                else
                    lc <- if l1 > 4294967295.0 then 1u else 0u

            if rPlusOne then
                let r1 = float rd + float rc
                if uint32 r1 <> getl li then 
                    //printfn "r + 1 <> l (%A <> %A) %d" (uint32 r1) (getl ri) oi
                    rPlusOne <- false
                else
                    rc <- if r1 > 4294967295.0 then 1u else 0u

            li <- li - 1
            ri <- ri - 1
            oi <- oi - 1

        lPlusOne || rPlusOne

    override x.GetHashCode() =
        let inline combine a b =
            uint32 a ^^^ uint32 b + 0x9e3779b9u + (uint32 a <<< 6) + (uint32 a >>> 2) |> int
        data |> Array.fold (fun c v -> combine c (DefaultEquality.hash v)) 0

    override x.Equals o =
        match o with
        | :? largeuint as o -> data.Length = o.Data.Length && Array.forall2 (=) data o.Data
        | _ -> false

    interface System.IComparable with
        member x.CompareTo o =
            match o with
            | :? largeuint as o ->
                let c = compare data.Length o.Data.Length
                if c <> 0 then c
                else
                    let rec compareArray (i : int) (l : uint32[]) (r : uint32[]) =
                        if i < l.Length then 
                            let c = compare l.[i] r.[i]
                            if c <> 0 then c
                            else compareArray (i+1) l r
                        else
                            0
                    compareArray 0 data o.Data
            | _ ->
                failwith "uncomparable"

[<Sealed>]
type Index private(number : largeuint, dexp : int) =
    
    let number, dexp =
        if number.IsZero then
            number, 0
        else
            let mutable number = number
            let mutable dexp = dexp
            while number.GetBit 0 = 0u do
                number <- number >>> 1
                dexp <- dexp - 1
            number, dexp

    member private x.Number = number
    member private x.DenomiatorExp = dexp

    static member Zero = Index(largeuint.Zero, 0)
    static member One = Index(largeuint.One, 0)
        
    override x.ToString() =
        sprintf "0x%s/0x%X" (string number) (1L <<< dexp)

    override x.GetHashCode() =
        let a = number.GetHashCode() 
        let b = dexp.GetHashCode()
        uint32 a ^^^ uint32 b + 0x9e3779b9u + (uint32 a <<< 6) + (uint32 a >>> 2) |> int

    override x.Equals o =
        match o with
        | :? Index as o -> number = o.Number && dexp = o.DenomiatorExp
        | _ -> false

    static member Between(l : Index, r : Index) =
        let le = l.DenomiatorExp
        let re = r.DenomiatorExp
        let c = compare le re
        let mutable a = Unchecked.defaultof<_>
        let mutable b = Unchecked.defaultof<_>
        let mutable e = 0
        if c < 0 then
            a <- l.Number <<< (re - le)
            b <- r.Number
            e <- re

        elif c > 0 then
            a <- l.Number
            b <- r.Number <<< (le - re)
            e <- le
            
        else
            a <- l.Number
            b <- r.Number
            e <- le

        if a = b then failwith "equal indices"
        elif largeuint.DistanceIsOne(a, b) then
            Index(a + b, e + 1)
        else
            Index((a + b) >>> 1, e)
        
    member inline l.Between(r : Index) = Index.Between(l, r)

    member x.After() = Index.Between(x, Index.One)
    member x.Before() = Index.Between(Index.Zero, x)

    interface System.IComparable with
        member x.CompareTo o =
            match o with
            | :? Index as o -> 
                if dexp < o.DenomiatorExp then
                    let a = number <<< (o.DenomiatorExp - dexp)
                    let b = o.Number
                    compare a b
                elif o.DenomiatorExp < dexp then
                    let a = number
                    let b = o.Number <<< (dexp - o.DenomiatorExp)
                    compare a b
                else
                    compare number o.Number
            | _ ->
                failwith "uncomparable"

    new() = Index(largeuint.Zero, 0)

#endif



/// functional operators for the Index datastructure.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Index =
    let zero = Index()

    let after (index : Index) = index.After()

    let before (index : Index) = index.Before()

    let between (a : Index) (b : Index) = a.Between b
