namespace Benchmarks

open System
open FSharp.Data.Adaptive
open FSharp.Data.Traceable
open BenchmarkDotNet.Attributes
open System.Runtime.CompilerServices
open BenchmarkDotNet.Configs
open System.Collections.Generic


module private ComputeListDeltaHelpers = 
    open System
    open System.Collections.Generic

    type DeltaOperation =
        | Remove = 0
        | Add = 1
        | Equal = 2
    [<Struct>]
    type DeltaOperationListNaive private(store : list<DeltaOperation>) =
    
        member x.IsNil = List.isEmpty store
            
        member x.Head = List.head store

        static member Empty = DeltaOperationListNaive []

        member x.Prepend(value : DeltaOperation) = DeltaOperationListNaive(value :: store)

        member x.UnsafeUnconsV() =
            let h = List.head store
            let t = List.tail store
            struct(h, DeltaOperationListNaive(t))

    [<Struct>]
    type DeltaOperationList64 private(store : uint64, next : ref<DeltaOperationList64>) =

        // 6 bit count
        // 29 entries * 2bit = 58 bit

        static let extractCount (v : uint64) =
            v >>> 58 |> int

        static let extractItem (index : int) (v : uint64) =
            (v >>> (index <<< 1)) &&& 3UL |> int

        static let create (cnt : int) (value : uint64) =
            let mask = (1UL <<< (cnt <<< 1)) - 1UL
            (uint64 cnt <<< 58) ||| (value &&& mask)

        static let add (v : int) (oldCnt : int) (value : uint64) =
            let newCnt = oldCnt + 1
            let mask = (1UL <<< (oldCnt <<< 1)) - 1UL
            (uint64 newCnt <<< 58) ||| 
            ((uint64 v &&& 3UL) <<< (oldCnt <<< 1)) |||
            (value &&& mask)

        static let empty = DeltaOperationList64(0UL, Unchecked.defaultof<_>)

        member x.IsNil = isNull (next :> obj)
            
        member x.Head =
            let c = extractCount store
            extractItem (c - 1) store |> unbox<DeltaOperation>

        static member Empty = empty

        member x.Prepend(value : DeltaOperation) =
            let v = int value &&& 3
            if isNull (next :> obj) then
                DeltaOperationList64(create 1 (uint64 v), ref x)
            else
                let cnt = extractCount store
                if cnt >= 29 then
                    DeltaOperationList64(create 1 (uint64 v), ref x)
                else
                    DeltaOperationList64(add v cnt store, next)

        member x.UnsafeUnconsV() =
            let cnt = extractCount store
            let v = extractItem (cnt - 1) store
            if cnt = 1 then
                struct(unbox<DeltaOperation> v, !next)
            else
                struct(unbox<DeltaOperation> v, DeltaOperationList64(create (cnt - 1) store, next))

    [<Struct>]
    type DeltaOperationList32 private(store : uint32, next : ref<DeltaOperationList32>) =

        // 4 bit count
        // 14 entries * 2bit = 28 bit

        static let extractCount (v : uint32) =
            v >>> 28 |> int

        static let extractItem (index : int) (v : uint32) =
            (v >>> (index <<< 1)) &&& 3u |> int

        static let create (cnt : int) (value : uint32) =
            let mask = (1u <<< (cnt <<< 1)) - 1u
            (uint32 cnt <<< 28) ||| (value &&& mask)

        static let add (v : int) (oldCnt : int) (value : uint32) =
            let newCnt = oldCnt + 1
            let mask = (1u <<< (oldCnt <<< 1)) - 1u
            (uint32 newCnt <<< 28) ||| 
            ((uint32 v &&& 3u) <<< (oldCnt <<< 1)) |||
            (value &&& mask)

        static let empty = DeltaOperationList32(0u, Unchecked.defaultof<_>)

        member x.IsNil = isNull (next :> obj)
            
        member x.Head =
            let c = extractCount store
            extractItem (c - 1) store |> unbox<DeltaOperation>

        static member Empty = empty

        member x.Prepend(value : DeltaOperation) =
            let v = int value &&& 3
            if isNull (next :> obj) then
                DeltaOperationList32(create 1 (uint32 v), ref x)
            else
                let cnt = extractCount store
                if cnt >= 14 then
                    DeltaOperationList32(create 1 (uint32 v), ref x)
                else
                    DeltaOperationList32(add v cnt store, next)

        member x.UnsafeUnconsV() =
            let cnt = extractCount store
            let v = extractItem (cnt - 1) store
            if cnt = 1 then
                struct(unbox<DeltaOperation> v, !next)
            else
                struct(unbox<DeltaOperation> v, DeltaOperationList32(create (cnt - 1) store, next))


    module DeltaOperationList = 

        let ofArrayMyersBackup (equal : 'a -> 'b -> bool) (src : 'a[]) (dst : 'b[]) : DeltaOperationListNaive =
            let equal = OptimizedClosures.FSharpFunc<_,_,_>.Adapt equal
            
            let max = src.Length + dst.Length
            let vs = Array.zeroCreate<int> (2 * max + 1)
            let ps = Array.zeroCreate<DeltaOperationListNaive> vs.Length

            let inline setv (k : int) value =
                vs.[k + max] <- value

            let inline getv (k : int) =
                vs.[k + max]

            let inline setp (k : int) value =
                ps.[k + max] <- value
                
            let inline getp (k : int) =
                ps.[k + max]
                
            let mutable x = 0
            let mutable y = 0
            let mutable d = 0
            let mutable result = Unchecked.defaultof<_>

            while d <= max do
                let mutable k = -d
                while k <= d do
                    let down = k = -d || (k <> d && getv (k-1) < getv (k+1))
                    
                    let mutable p = Unchecked.defaultof<_>
                    if down then
                        x <- getv (k+1)
                        p <- getp (k+1)
                    else    
                        x <- getv(k-1) + 1
                        p <- getp(k-1)

                    y <- x - k
                    let kPrev = if down then k + 1 else k - 1

                    let xStart = getv kPrev
                    let yStart = xStart - kPrev

                    let xMid = if down then xStart else xStart + 1
                    let yMid = xMid - k

                    if xStart >= 0 && xMid <> xStart then p <- p.Prepend DeltaOperation.Remove 
                    elif yStart >= 0 && yMid <> yStart then p <- p.Prepend DeltaOperation.Add

                    while x < src.Length && y < dst.Length && equal.Invoke(src.[x], dst.[y]) do
                        x <- x + 1
                        y <- y + 1
                        p <- p.Prepend DeltaOperation.Equal

                    setv k x
                    setp k p
                    if x >= src.Length && y >= dst.Length then
                        // terminate
                        result <- p
                        d <- max
                        k <- max

                    k <- k + 2
                d <- d + 1

            let rec reverse (acc : DeltaOperationListNaive) (l : DeltaOperationListNaive) =
                if l.IsNil then
                    acc
                else
                    let struct(h, t) = l.UnsafeUnconsV()
                    reverse (acc.Prepend h) t
            let res = reverse DeltaOperationListNaive.Empty result
            res

        type DeltaOperationList = DeltaOperationList64
        
        /// inspired by this [paper](https://neil.fraser.name/writing/diff/myers.pdf)
        let ofArrayMyers (equal : 'a -> 'b -> bool) (src : 'a[]) (dst : 'b[]) : DeltaOperationList =
            let equal = OptimizedClosures.FSharpFunc<_,_,_>.Adapt equal
            
            let max = src.Length + dst.Length
            let vs = Array.zeroCreate<int> (2 * max + 1)
            let ps = Array.zeroCreate<DeltaOperationList> vs.Length

            let inline setv (k : int) value =
                vs.[k + max] <- value

            let inline getv (k : int) =
                vs.[k + max]

            let inline setp (k : int) value =
                ps.[k + max] <- value
                
            let inline getp (k : int) =
                ps.[k + max]
                
            let mutable x = 0
            let mutable y = 0
            let mutable d = 0
            let mutable result = Unchecked.defaultof<_>

            let eSrc = src.Length - 1
            let eDst = dst.Length - 1

            let inline equal x y =
                // reversed arrays
                equal.Invoke(src.[eSrc - x], dst.[eDst - y])

            while d <= max do
                let mutable k = -d
                while k <= d do
                    let down = k = -d || (k <> d && getv (k-1) < getv (k+1))
                    
                    let mutable p = Unchecked.defaultof<_>
                    if down then
                        x <- getv (k+1)
                        p <- getp (k+1)
                    else    
                        x <- getv(k-1) + 1
                        p <- getp(k-1)

                    y <- x - k
                    let kPrev = if down then k + 1 else k - 1

                    let xStart = getv kPrev
                    let yStart = xStart - kPrev

                    let xMid = if down then xStart else xStart + 1
                    let yMid = xMid - k

                    if xStart >= 0 && xMid <> xStart then p <- p.Prepend DeltaOperation.Remove 
                    elif yStart >= 0 && yMid <> yStart then p <- p.Prepend DeltaOperation.Add

                    while x < src.Length && y < dst.Length && equal x y do
                        x <- x + 1
                        y <- y + 1
                        p <- p.Prepend DeltaOperation.Equal

                    setv k x
                    setp k p
                    if x >= src.Length && y >= dst.Length then
                        // terminate
                        result <- p
                        d <- max
                        k <- max

                    k <- k + 2
                d <- d + 1

            result
       

module IndexList = 
    let computeDeltaToArrayBaseline (cmp : IEqualityComparer<'a>) (src : IndexList<'a>) (dst : 'a[]) =
        if dst.Length = 0 then
            IndexList.computeDelta src IndexList.empty
        else
            if src.Count = 0 then
                IndexList.computeDelta src (IndexList.ofArray dst)
            else
                // both are non-empty
                let src = IndexList.toArrayIndexed src

                
                //let cmp = System.Collections.Generic.EqualityComparer<'a>.Default
                let mutable steps = ComputeListDeltaHelpers.DeltaOperationList.ofArrayMyersBackup (fun (_,a) b -> cmp.Equals(a, b)) src dst
                let mutable si = 0
                let mutable di = 0
                let mutable delta = IndexListDelta.empty
                let mutable lastIndex = Index.zero

                while not steps.IsNil do
                    let struct(h, t) = steps.UnsafeUnconsV()

                    match h with
                    | ComputeListDeltaHelpers.DeltaOperation.Equal ->
                        steps <- t
                        // step both => no delta
                        lastIndex <- fst src.[si]
                        si <- si + 1
                        di <- di + 1
                    | _ ->
                       
                        let mutable remCnt = 0
                        let mutable addCnt = 0
                        let mutable struct(h, t) = steps.UnsafeUnconsV()
                        steps <- t

                        while h <> ComputeListDeltaHelpers.DeltaOperation.Equal do
                            if h = ComputeListDeltaHelpers.DeltaOperation.Remove then remCnt <- remCnt + 1
                            else addCnt <- addCnt + 1

                            if steps.IsNil then 
                                h <- ComputeListDeltaHelpers.DeltaOperation.Equal
                            else 
                                let struct(hh, tt) = steps.UnsafeUnconsV()
                                if hh <> ComputeListDeltaHelpers.DeltaOperation.Equal then 
                                    h <- hh
                                    steps <- tt
                                else
                                    h <- ComputeListDeltaHelpers.DeltaOperation.Equal

                        
                        let replace = min remCnt addCnt
                        for _ in 0 .. replace - 1 do
                            let (idx,_) = src.[si]
                            delta <- delta |> IndexListDelta.add idx (Set dst.[di])
                            si <- si + 1
                            di <- di + 1
                            lastIndex <- idx

                        for _ in replace .. remCnt - 1 do
                            let (idx, _) = src.[si]
                            delta <- delta |> IndexListDelta.add idx Remove
                            si <- si + 1

                        if replace < addCnt then
                            let newIndex =
                                if si < src.Length then 
                                    let (ni,_) = src.[si]
                                    fun l -> Index.between l ni
                                else 
                                    Index.after

                            for _ in replace .. addCnt - 1 do
                                let idx = newIndex lastIndex
                                delta <- delta |> IndexListDelta.add idx (Set dst.[di])
                                lastIndex <- idx
                                di <- di + 1

                delta


[<PlainExporter; MemoryDiagnoser>]
type ListDeltaBenchmark() =
    static let rand = Random()
    static do DefaultEqualityComparer.SetProvider DefaultEqualityComparer.System
    
    static let randomInsert (l : list<int>) =
        let cnt = List.length l
        let pos = rand.Next cnt
        let len = rand.Next 9 + 1
        List.take pos l @ List.init len (fun _ ->rand.Next 1000) @ List.skip pos l

    static let randomRemove (l : list<int>) =
        match l with
        | [] ->
            []
        | _ ->
            let cnt = List.length l
            let pos = rand.Next cnt
            let len = rand.Next(cnt - pos) + 1
            List.take pos l @ List.skip (pos + len) l


    [<Params(10000); DefaultValue>]
    val mutable public Count : int
    
    [<Params(100); DefaultValue>]
    val mutable public Edits : int

    let mutable src = IndexList.empty
    let mutable dst = [||]

    static member Profile() =
        let lSrc = List.init 10000 (fun _ -> rand.Next 1000)
        let mutable lDst = lSrc
        for i in 1..1000 do
            match lDst with
            | [] ->
                lDst <- [rand.Next 10000]
            | _ ->
                let idx = rand.Next(List.length lDst)
                let l, r = List.splitAt idx lDst
                match rand.Next 2 with
                | 0 -> lDst <- List.concat [l; [9999]; r]
                | _ -> 
                    match r with
                    | [] -> lDst <- List.append (List.take (List.length l - 1) l) r
                    | _ -> lDst <- List.append l (List.tail r)

        let src = IndexList.ofList lSrc
        let dst = List.toArray lDst

        let cmp = EqualityComparer<_>.Default
        while true do
            IndexList.computeDeltaToArray cmp src dst |> ignore


    [<GlobalSetup>]
    member x.Setup() =
        let lSrc = List.init x.Count (fun _ -> rand.Next 1000)
        let mutable lDst = lSrc
        for i in 1..x.Edits do
            match lDst with
            | [] ->
                lDst <- [rand.Next 10000]
            | _ ->
                let idx = rand.Next(List.length lDst)
                let l, r = List.splitAt idx lDst
                match rand.Next 2 with
                | 0 -> lDst <- List.concat [l; [9999]; r]
                | _ -> 
                    match r with
                    | [] -> lDst <- List.append (List.take (List.length l - 1) l) r
                    | _ -> lDst <- List.append l (List.tail r)

        src <- IndexList.ofList lSrc
        dst <- List.toArray lDst


    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("computeDeltaToArray")>]
    member x.MyersBase() =
        IndexList.computeDeltaToArrayBaseline EqualityComparer.Default src dst
        
    [<Benchmark>]
    [<BenchmarkCategory("computeDeltaToArray")>]
    member x.Myers() =
        IndexList.computeDeltaToArray EqualityComparer.Default src dst
        
    [<Benchmark>]
    [<BenchmarkCategory("computeDeltaToArray")>]
    member x.MyersUncheckedEquals() =
        IndexList.computeDeltaToArray DefaultEqualityComparer.Instance src dst
        