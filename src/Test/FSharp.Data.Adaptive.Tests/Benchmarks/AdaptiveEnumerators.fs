namespace Benchmarks

open System
open FSharp.Data.Adaptive
open FSharp.Data.Traceable
open BenchmarkDotNet.Attributes
open System.Runtime.CompilerServices

type HashSetEnumeratorBaseline<'T> =
    struct 
        val mutable private root : HashSetNode<'T>
        val mutable private head : HashSetNode<'T>
        val mutable private tail : list<HashSetNode<'T>>
        val mutable private linked : HashSetLinked<'T>
        val mutable private current : 'T

        internal new(root: HashSetNode<'T>) = 
            {
                root = root
                head = root
                tail = []
                linked = null
                current = Unchecked.defaultof<'T>    
            }

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member x.MoveNext() =
           if isNull x.linked then

                match x.head with
                | :? HashSetNoCollisionLeaf<'T> as l ->
                    x.current <- l.Value
                    if x.tail.IsEmpty then
                        x.head <- Unchecked.defaultof<_>
                    else
                        x.head <- x.tail.Head
                        x.tail <- x.tail.Tail
                    true
                | :? HashSetCollisionLeaf<'T> as l -> 
                    x.current <- l.Value
                    x.linked <- l.Next
                    if x.tail.IsEmpty then
                        x.head <- Unchecked.defaultof<_>
                    else
                        x.head <- x.tail.Head
                        x.tail <- x.tail.Tail
                    true
                | :? HashSetInner<'T> as n ->
                    x.head <- n.Left
                    x.tail <- n.Right:: x.tail
                    x.MoveNext()
                | _ ->
                    false

            else
                x.current <- x.linked.Value
                x.linked <- x.linked.Next
                true

        member x.Current
            with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = x.current

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member x.Reset() =
            x.head <- x.root
            x.tail <- []
            x.linked <- null
            x.current <- Unchecked.defaultof<_>

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member x.Dispose() =
            x.root <- Unchecked.defaultof<_>
            x.tail <- []
            x.head <- Unchecked.defaultof<_>
            x.linked <- null
            x.current <- Unchecked.defaultof<_>

        interface System.Collections.IEnumerator with
            member x.MoveNext() = x.MoveNext()
            member x.Current = x.Current:> obj
            member x.Reset() = x.Reset()
        
        interface System.Collections.Generic.IEnumerator<'T> with
            member x.Dispose() = x.Dispose()
            member x.Current = x.Current
    end

[<Struct>]
type Struct32 =
    {
        mutable a : float
        mutable b : float
        mutable c : float
        mutable d : float
    }

[<Struct>]
type Struct128 = 
    {
        mutable x : Struct32
        mutable y : Struct32
        mutable z : Struct32
        mutable w : Struct32
    }

[<Struct>] 
type Struct384 = 
    {
        mutable r : Struct128
        mutable s : Struct128
        mutable t : Struct128
    }

module BigStruct =
    let create32 (seed : int) =
        let f = (float)seed
        { a = f; b = f + 1.0; c = f * 2.0; d = f - 1.0 }

    // 128 byte (M44d)
    let createBig(seed : int) =
        { x = create32(seed); y = create32(seed * 2); z = create32(seed * 3); w = create32(seed + 1) }

    // 384 bytes (Trafo3d + M44d)
    let createBigger(seed : int) =
        { r = createBig(seed); s = createBig(seed * 2); t = createBig(seed * 3) }


//BenchmarkDotNet=v0.12.0, OS=Windows 10.0.19041
//Intel Core i7-8700K CPU 3.70GHz (Coffee Lake), 1 CPU, 12 logical and 6 physical cores
//.NET Core SDK=5.0.100
//  [Host]     : .NET Core 3.1.9 (CoreCLR 4.700.20.47201, CoreFX 4.700.20.47203), X64 RyuJIT DEBUG
//  DefaultJob : .NET Core 3.1.9 (CoreCLR 4.700.20.47201, CoreFX 4.700.20.47203), X64 RyuJIT


//|             Method | Count |         Mean |        Error |       StdDev |       Median |   Gen 0 |  Gen 1 | Gen 2 | Allocated |
//|------------------- |------ |-------------:|-------------:|-------------:|-------------:|--------:|-------:|------:|----------:|
//|     Baseline_4byte |     0 |     12.98 ns |     0.060 ns |     0.053 ns |     12.95 ns |       - |      - |     - |         - |
//|    Baseline_32byte |     0 |     14.75 ns |     0.164 ns |     0.145 ns |     14.68 ns |       - |      - |     - |         - |
//|   Baseline_128byte |     0 |     25.77 ns |     0.120 ns |     0.112 ns |     25.74 ns |       - |      - |     - |         - |
//|   Baseline_384byte |     0 |     45.13 ns |     2.330 ns |     4.202 ns |     42.78 ns |       - |      - |     - |         - |
//|   Collection_4byte |     0 |     30.31 ns |     0.116 ns |     0.103 ns |     30.33 ns |  0.0076 |      - |     - |      48 B |
//|  Collection_32byte |     0 |     29.91 ns |     0.230 ns |     0.215 ns |     29.90 ns |  0.0076 |      - |     - |      48 B |
//| Collection_128byte |     0 |     36.45 ns |     0.759 ns |     1.425 ns |     36.86 ns |  0.0076 |      - |     - |      48 B |
//| Collection_384byte |     0 |     35.78 ns |     0.709 ns |     0.788 ns |     36.06 ns |  0.0076 |      - |     - |      48 B |
//|     Baseline_4byte |     1 |     14.98 ns |     0.074 ns |     0.066 ns |     14.96 ns |       - |      - |     - |         - |
//|    Baseline_32byte |     1 |     16.87 ns |     0.037 ns |     0.031 ns |     16.87 ns |       - |      - |     - |         - |
//|   Baseline_128byte |     1 |     36.64 ns |     0.282 ns |     0.250 ns |     36.54 ns |       - |      - |     - |         - |
//|   Baseline_384byte |     1 |     73.09 ns |     3.055 ns |     4.478 ns |     70.88 ns |       - |      - |     - |         - |
//|   Collection_4byte |     1 |     32.48 ns |     0.129 ns |     0.120 ns |     32.49 ns |  0.0089 |      - |     - |      56 B |
//|  Collection_32byte |     1 |     35.49 ns |     0.577 ns |     0.540 ns |     35.39 ns |  0.0127 |      - |     - |      80 B |
//| Collection_128byte |     1 |     58.66 ns |     0.352 ns |     0.294 ns |     58.69 ns |  0.0280 |      - |     - |     176 B |
//| Collection_384byte |     1 |     95.72 ns |     0.955 ns |     0.846 ns |     95.84 ns |  0.0688 |      - |     - |     432 B |
//|     Baseline_4byte |    10 |    174.47 ns |     1.295 ns |     1.148 ns |    174.12 ns |  0.0458 |      - |     - |     288 B |
//|    Baseline_32byte |    10 |    178.84 ns |     0.359 ns |     0.280 ns |    178.82 ns |  0.0458 |      - |     - |     288 B |
//|   Baseline_128byte |    10 |    341.33 ns |     3.845 ns |     3.597 ns |    339.12 ns |  0.0458 |      - |     - |     288 B |
//|   Baseline_384byte |    10 |    480.03 ns |     1.377 ns |     1.150 ns |    480.24 ns |  0.0458 |      - |     - |     288 B |
//|   Collection_4byte |    10 |     79.46 ns |     0.611 ns |     0.571 ns |     79.35 ns |  0.0139 |      - |     - |      88 B |
//|  Collection_32byte |    10 |    105.21 ns |     0.420 ns |     0.351 ns |    105.28 ns |  0.0587 |      - |     - |     368 B |
//| Collection_128byte |    10 |    357.40 ns |     1.459 ns |     1.364 ns |    357.46 ns |  0.2112 |      - |     - |    1328 B |
//| Collection_384byte |    10 |    706.40 ns |     6.832 ns |     6.391 ns |    707.93 ns |  0.6189 |      - |     - |    3888 B |
//|     Baseline_4byte |   100 |  2,269.84 ns |    12.283 ns |    10.257 ns |  2,266.39 ns |  0.5035 |      - |     - |    3168 B |
//|    Baseline_32byte |   100 |  2,255.84 ns |    11.618 ns |    10.868 ns |  2,253.45 ns |  0.5035 |      - |     - |    3168 B |
//|   Baseline_128byte |   100 |  3,241.46 ns |    17.759 ns |    16.612 ns |  3,243.49 ns |  0.5035 |      - |     - |    3168 B |
//|   Baseline_384byte |   100 |  5,095.53 ns |    43.932 ns |    41.094 ns |  5,093.84 ns |  0.5035 |      - |     - |    3168 B |
//|   Collection_4byte |   100 |    806.41 ns |     7.991 ns |     6.673 ns |    805.38 ns |  0.1945 |      - |     - |    1224 B |
//|  Collection_32byte |   100 |  1,112.80 ns |    20.943 ns |    21.507 ns |  1,109.66 ns |  0.6237 |      - |     - |    3920 B |
//| Collection_128byte |   100 |  3,684.57 ns |    72.150 ns |    80.195 ns |  3,686.29 ns |  2.1515 | 0.0076 |     - |   13520 B |
//| Collection_384byte |   100 |  7,828.77 ns |    69.564 ns |    61.667 ns |  7,823.32 ns |  6.2103 | 0.0916 |     - |   39040 B |
//|     Baseline_4byte |  1000 | 24,409.25 ns |    79.678 ns |    66.535 ns | 24,402.84 ns |  5.0659 |      - |     - |   31968 B |
//|    Baseline_32byte |  1000 | 22,290.28 ns |    48.992 ns |    45.827 ns | 22,296.44 ns |  5.0659 |      - |     - |   31968 B |
//|   Baseline_128byte |  1000 | 35,486.47 ns |   114.133 ns |   101.176 ns | 35,471.98 ns |  5.0659 |      - |     - |   31968 B |
//|   Baseline_384byte |  1000 | 59,970.33 ns |   281.730 ns |   249.746 ns | 59,994.19 ns |  5.0659 |      - |     - |   31968 B |
//|   Collection_4byte |  1000 | 11,806.29 ns |    53.757 ns |    50.284 ns | 11,801.55 ns |  1.7853 |      - |     - |   11232 B |
//|  Collection_32byte |  1000 | 15,629.16 ns |    78.019 ns |    65.149 ns | 15,626.96 ns |  6.1951 |      - |     - |   39040 B |
//| Collection_128byte |  1000 | 41,979.60 ns |   496.932 ns |   464.830 ns | 41,937.07 ns | 21.5454 | 0.1221 |     - |  135280 B |
//| Collection_384byte |  1000 | 94,500.83 ns | 1,437.677 ns | 1,344.804 ns | 94,210.69 ns | 62.2559 | 0.8545 |     - |  391280 B |

//NEW:
//|             Method | Count |         Mean |      Error |     StdDev |  Gen 0 | Gen 1 | Gen 2 | Allocated |
//|------------------- |------ |-------------:|-----------:|-----------:|-------:|------:|------:|----------:|
//|   Collection_4byte |     0 |     23.27 ns |   0.087 ns |   0.077 ns |      - |     - |     - |         - |
//|  Collection_32byte |     0 |     34.75 ns |   0.125 ns |   0.117 ns |      - |     - |     - |         - |
//| Collection_128byte |     0 |     57.63 ns |   0.300 ns |   0.266 ns |      - |     - |     - |         - |
//| Collection_384byte |     0 |     98.98 ns |   0.230 ns |   0.204 ns |      - |     - |     - |         - |
//|   Collection_4byte |     1 |     26.64 ns |   0.138 ns |   0.129 ns |      - |     - |     - |         - |
//|  Collection_32byte |     1 |     38.52 ns |   0.139 ns |   0.130 ns |      - |     - |     - |         - |
//| Collection_128byte |     1 |     65.93 ns |   0.146 ns |   0.130 ns |      - |     - |     - |         - |
//| Collection_384byte |     1 |    109.62 ns |   0.549 ns |   0.514 ns |      - |     - |     - |         - |
//|   Collection_4byte |    10 |     81.06 ns |   1.286 ns |   1.203 ns | 0.0139 |     - |     - |      88 B |
//|  Collection_32byte |    10 |    119.39 ns |   1.025 ns |   0.959 ns | 0.0587 |     - |     - |     368 B |
//| Collection_128byte |    10 |    468.05 ns |   1.428 ns |   1.336 ns | 0.0458 |     - |     - |     288 B |
//| Collection_384byte |    10 |    661.74 ns |   2.072 ns |   1.837 ns | 0.0458 |     - |     - |     288 B |
//|   Collection_4byte |   100 |    711.99 ns |   4.331 ns |   4.051 ns | 0.0973 |     - |     - |     616 B |
//|  Collection_32byte |   100 |  1,135.33 ns |   2.548 ns |   2.383 ns | 0.1602 |     - |     - |    1008 B |
//| Collection_128byte |   100 |  4,495.34 ns |  20.492 ns |  17.112 ns | 0.5035 |     - |     - |    3168 B |
//| Collection_384byte |   100 |  6,420.26 ns |  27.941 ns |  26.136 ns | 0.5035 |     - |     - |    3168 B |
//|   Collection_4byte |  1000 | 11,107.25 ns |  84.302 ns |  78.856 ns | 0.7935 |     - |     - |    4984 B |
//|  Collection_32byte |  1000 | 13,079.25 ns |  76.962 ns |  71.990 ns | 0.8545 |     - |     - |    5432 B |
//| Collection_128byte |  1000 | 46,346.29 ns | 168.816 ns | 157.910 ns | 5.0659 |     - |     - |   31968 B |
//| Collection_384byte |  1000 | 72,511.11 ns | 433.272 ns | 405.282 ns | 5.0049 |     - |     - |   31968 B |

[<PlainExporter; MemoryDiagnoser>]
type HashSetEnumeratorBenchmark() =

    let mutable collection4 = HashSet.empty<int>
    let mutable collection32 = HashSet.empty<Struct32>
    let mutable collection128 = HashSet.empty<Struct128>
    let mutable collection384 = HashSet.empty<Struct384>
   
    [<Params(0, 1, 10, 100, 1000); DefaultValue>]
    val mutable public Count : int

    [<GlobalSetup>]
    member x.Setup() =
        let rand = Random(123)
        collection4 <- HashSet.ofArray (Array.init x.Count (fun i -> rand.Next() ))
        collection32 <- HashSet.ofArray (Array.init x.Count (fun i -> BigStruct.create32(rand.Next()) ))
        collection128 <- HashSet.ofArray (Array.init x.Count (fun i -> BigStruct.createBig(rand.Next()) ))
        collection384 <- HashSet.ofArray (Array.init x.Count (fun i -> BigStruct.createBigger(rand.Next()) ))

    [<Benchmark>]
    member x.Baseline_4byte() =
        let mutable e = new HashSetEnumeratorBaseline<_>(collection4.Root)
        let mutable sum = 0
        while e.MoveNext() do
            sum <- sum + e.Current
        sum

    [<Benchmark>]
    member x.Baseline_32byte() =
        let mutable e = new HashSetEnumeratorBaseline<_>(collection32.Root)
        let mutable sum = 0.0
        while e.MoveNext() do
            sum <- sum + e.Current.a
        sum

    [<Benchmark>]
    member x.Baseline_128byte() =
        let mutable e = new HashSetEnumeratorBaseline<_>(collection128.Root)
        let mutable sum = 0.0
        while e.MoveNext() do
            sum <- sum + e.Current.w.c
        sum

    [<Benchmark>]
    member x.Baseline_384byte() =
        let mutable e = new HashSetEnumeratorBaseline<_>(collection384.Root)
        let mutable sum = 0.0
        while e.MoveNext() do
            sum <- sum + e.Current.s.y.c
        sum

    [<Benchmark>]
    member x.HashSet_4byte() =
        let mutable sum = 0
        for e in collection4 do sum <- sum + e
        sum

    [<Benchmark>]
    member x.HashSet_32byte() =
        let mutable sum = 0.0
        for e in collection32 do sum <- sum + e.a
        sum

    [<Benchmark>]
    member x.HashSet_128byte() =
        let mutable sum = 0.0
        for e in collection128 do sum <- sum + e.w.c
        sum

    [<Benchmark>]
    member x.HashSet_384byte() =
        let mutable sum = 0.0
        for e in collection384 do sum <- sum + e.s.y.c
        sum


[<PlainExporter; MemoryDiagnoser>]
type IndexListEnumeratorBenchmark() =

    let mutable collection4 = IndexList.empty<int>
    let mutable collection32 = IndexList.empty<Struct32>
    let mutable collection128 = IndexList.empty<Struct128>
    let mutable collection384 = IndexList.empty<Struct384>
   
    [<Params(0, 1, 10, 100, 1000); DefaultValue>]
    val mutable public Count : int

    [<GlobalSetup>]
    member x.Setup() =
        let rand = Random(123)
        collection4 <- IndexList.ofArray (Array.init x.Count (fun i -> rand.Next() ))
        collection32 <- IndexList.ofArray (Array.init x.Count (fun i -> BigStruct.create32(rand.Next()) ))
        collection128 <- IndexList.ofArray (Array.init x.Count (fun i -> BigStruct.createBig(rand.Next()) ))
        collection384 <- IndexList.ofArray (Array.init x.Count (fun i -> BigStruct.createBigger(rand.Next()) ))

    [<Benchmark>]
    member x.IndexList_4byte() =
        let mutable sum = 0
        for e in collection4 do sum <- sum + e
        sum

    [<Benchmark>]
    member x.IndexList_32byte() =
        let mutable sum = 0.0
        for e in collection32 do sum <- sum + e.a
        sum

    [<Benchmark>]
    member x.IndexList_128byte() =
        let mutable sum = 0.0
        for e in collection128 do sum <- sum + e.w.c
        sum

    [<Benchmark>]
    member x.IndexList_384byte() =
        let mutable sum = 0.0
        for e in collection384 do sum <- sum + e.s.y.c
        sum


[<PlainExporter; MemoryDiagnoser>]
type HashMapEnumeratorBenchmark() =

    let mutable collection4 = HashMap.empty<int,int>
    let mutable collection32 = HashMap.empty<Struct32,Struct32>
    let mutable collection128 = HashMap.empty<Struct128,Struct128>
    let mutable collection384 = HashMap.empty<Struct384,Struct384>
   
    [<Params(0, 1, 10, 100, 1000); DefaultValue>]
    val mutable public Count : int

    [<GlobalSetup>]
    member x.Setup() =
        let rand = Random(123)
        collection4 <- HashMap.ofArray (Array.init x.Count (fun i -> (rand.Next(), rand.Next()) ))
        collection32 <- HashMap.ofArray (Array.init x.Count (fun i -> (BigStruct.create32(rand.Next()), BigStruct.create32(rand.Next())) ))
        collection128 <- HashMap.ofArray (Array.init x.Count (fun i -> (BigStruct.createBig(rand.Next()), BigStruct.createBig(rand.Next())) ))
        collection384 <- HashMap.ofArray (Array.init x.Count (fun i -> (BigStruct.createBigger(rand.Next()), BigStruct.createBigger(rand.Next()))  ))

    [<Benchmark>]
    member x.HashMap_4byte() =
        let mutable sum = 0
        for (k,v) in collection4 do sum <- sum + k + v
        sum

    [<Benchmark>]
    member x.HashMap_32byte() =
        let mutable sum = 0.0
        for (k,v) in collection32 do sum <- sum + k.a + v.b
        sum

    [<Benchmark>]
    member x.HashMap_128byte() =
        let mutable sum = 0.0
        for (k,v) in collection128 do sum <- sum + k.x.b + v.y.c
        sum

    [<Benchmark>]
    member x.HashMap_384byte() =
        let mutable sum = 0.0
        for (k,v) in collection384 do sum <- sum + k.r.z.d + v.r.x.a
        sum

[<PlainExporter; MemoryDiagnoser>]
type CountingHashSetEnumeratorBenchmark() =

    let mutable collection4 = CountingHashSet.empty<int>
    let mutable collection32 = CountingHashSet.empty<Struct32>
    let mutable collection128 = CountingHashSet.empty<Struct128>
    let mutable collection384 = CountingHashSet.empty<Struct384>
   
    [<Params(0, 1, 10, 100, 1000); DefaultValue>]
    val mutable public Count : int

    [<GlobalSetup>]
    member x.Setup() =
        let rand = Random(123)
        collection4 <- CountingHashSet.ofArray (Array.init x.Count (fun i -> rand.Next() ))
        collection32 <- CountingHashSet.ofArray (Array.init x.Count (fun i -> BigStruct.create32(rand.Next()) ))
        collection128 <- CountingHashSet.ofArray (Array.init x.Count (fun i -> BigStruct.createBig(rand.Next()) ))
        collection384 <- CountingHashSet.ofArray (Array.init x.Count (fun i -> BigStruct.createBigger(rand.Next()) ))

    [<Benchmark>]
    member x.CountingHashSet_4byte() =
        let mutable sum = 0
        for e in collection4 do sum <- sum + e
        sum

    [<Benchmark>]
    member x.CountingHashSet_32byte() =
        let mutable sum = 0.0
        for e in collection32 do sum <- sum + e.a
        sum

    [<Benchmark>]
    member x.CountingHashSet_128byte() =
        let mutable sum = 0.0
        for e in collection128 do sum <- sum + e.x.b
        sum

    [<Benchmark>]
    member x.CountingHashSet_384byte() =
        let mutable sum = 0.0
        for e in collection384 do sum <- sum + e.r.z.d
        sum


[<PlainExporter; MemoryDiagnoser>]
type IndexListDeltaEnumeratorBenchmark() =

    let mutable collection4 = IndexListDelta.empty<int>
    let mutable collection32 = IndexListDelta.empty<Struct32>
    let mutable collection128 = IndexListDelta.empty<Struct128>
    let mutable collection384 = IndexListDelta.empty<Struct384>
   
    [<Params(0, 1, 10, 100, 1000); DefaultValue>]
    val mutable public Count : int

    [<GlobalSetup>]
    member x.Setup() =
        let rand = Random(123)
        collection4 <- IndexList.computeDelta IndexList.empty (IndexList.ofArray (Array.init x.Count (fun i -> rand.Next() )))
        collection32 <- IndexList.computeDelta IndexList.empty (IndexList.ofArray (Array.init x.Count (fun i -> BigStruct.create32(rand.Next()) )))
        collection128 <- IndexList.computeDelta IndexList.empty (IndexList.ofArray (Array.init x.Count (fun i -> BigStruct.createBig(rand.Next()) )))
        collection384 <- IndexList.computeDelta IndexList.empty (IndexList.ofArray (Array.init x.Count (fun i -> BigStruct.createBigger(rand.Next()) )))

    [<Benchmark>]
    member x.IndexListDelta_4byte() =
        let mutable sum = 0
        for e in collection4 do sum <- sum + 1
        sum

    [<Benchmark>]
    member x.IndexListDelta_32byte() =
        let mutable sum = 0
        for e in collection32 do sum <- sum + 1
        sum

    [<Benchmark>]
    member x.IndexListDelta_128byte() =
        let mutable sum = 0
        for e in collection128 do sum <- sum + 1
        sum

    [<Benchmark>]
    member x.IndexListDelta_384byte() =
        let mutable sum = 0
        for e in collection384 do sum <- sum + 1
        sum


[<PlainExporter; MemoryDiagnoser>]
type HashSetDeltaEnumeratorBenchmark() =

    let mutable collection4 = HashSetDelta.empty<int>
    let mutable collection32 = HashSetDelta.empty<Struct32>
    let mutable collection128 = HashSetDelta.empty<Struct128>
    let mutable collection384 = HashSetDelta.empty<Struct384>
   
    [<Params(0, 1, 10, 100, 1000); DefaultValue>]
    val mutable public Count : int

    [<GlobalSetup>]
    member x.Setup() =
        let rand = Random(123)
        collection4 <- HashSet.computeDelta HashSet.empty (HashSet.ofArray (Array.init x.Count (fun i -> rand.Next() )))
        collection32 <- HashSet.computeDelta HashSet.empty (HashSet.ofArray (Array.init x.Count (fun i -> BigStruct.create32(rand.Next()) )))
        collection128 <- HashSet.computeDelta HashSet.empty (HashSet.ofArray (Array.init x.Count (fun i -> BigStruct.createBig(rand.Next()) )))
        collection384 <- HashSet.computeDelta HashSet.empty (HashSet.ofArray (Array.init x.Count (fun i -> BigStruct.createBigger(rand.Next()) )))

    [<Benchmark>]
    member x.HashSetDelta_4byte() =
        let mutable sum = 0
        for e in collection4 do sum <- sum + 1
        sum

    [<Benchmark>]
    member x.HashSetDelta_32byte() =
        let mutable sum = 0
        for e in collection32 do sum <- sum + 1
        sum

    [<Benchmark>]
    member x.HashSetDelta_128byte() =
        let mutable sum = 0
        for e in collection128 do sum <- sum + 1
        sum

    [<Benchmark>]
    member x.HashSetDelta_384byte() =
        let mutable sum = 0
        for e in collection384 do sum <- sum + 1
        sum


[<PlainExporter; MemoryDiagnoser>]
type HashMapDeltaEnumeratorBenchmark() =

    let mutable collection4 = HashMapDelta.empty<int,int>
    let mutable collection32 = HashMapDelta.empty<Struct32,Struct32>
    let mutable collection128 = HashMapDelta.empty<Struct128,Struct128>
    let mutable collection384 = HashMapDelta.empty<Struct384,Struct384>
   
    [<Params(0, 1, 10, 100, 1000); DefaultValue>]
    val mutable public Count : int

    [<GlobalSetup>]
    member x.Setup() =
        let rand = Random(123)
        collection4 <- HashMap.computeDelta HashMap.empty (HashMap.ofArray (Array.init x.Count (fun i -> (rand.Next(), rand.Next()) )))
        collection32 <- HashMap.computeDelta HashMap.empty (HashMap.ofArray (Array.init x.Count (fun i -> (BigStruct.create32(rand.Next()), BigStruct.create32(rand.Next())) )))
        collection128 <- HashMap.computeDelta HashMap.empty (HashMap.ofArray (Array.init x.Count (fun i -> (BigStruct.createBig(rand.Next()), BigStruct.createBig(rand.Next())) )))
        collection384 <- HashMap.computeDelta HashMap.empty (HashMap.ofArray (Array.init x.Count (fun i -> (BigStruct.createBigger(rand.Next()), BigStruct.createBigger(rand.Next()))  )))

    [<Benchmark>]
    member x.HashMapDelta_4byte() =
        let mutable sum = 0
        for (k,v) in collection4 do sum <- sum + 1
        sum

    [<Benchmark>]
    member x.HashMapDelta_32byte() =
        let mutable sum = 0
        for (k,v) in collection32 do sum <- sum + 1
        sum

    [<Benchmark>]
    member x.HashMapDelta_128byte() =
        let mutable sum = 0
        for (k,v) in collection128 do sum <- sum + 1
        sum

    [<Benchmark>]
    member x.HashMapDelta_384byte() =
        let mutable sum = 0
        for (k,v) in collection384 do sum <- sum + 1
        sum
