namespace Benchmarks

open System
open FSharp.Data.Adaptive
open FSharp.Data.Traceable
open BenchmarkDotNet.Attributes
open System.Runtime.CompilerServices
open BenchmarkDotNet.Configs

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

// Traversal Enumerator:
//|             Method | Count |         Mean |        Error |       StdDev |       Median |   Gen 0 |  Gen 1 | Gen 2 | Allocated |
//|------------------- |------ |-------------:|-------------:|-------------:|-------------:|--------:|-------:|------:|----------:|
//|     Baseline_4byte |     0 |     12.98 ns |     0.060 ns |     0.053 ns |     12.95 ns |       - |      - |     - |         - |
//|    Baseline_32byte |     0 |     14.75 ns |     0.164 ns |     0.145 ns |     14.68 ns |       - |      - |     - |         - |
//|   Baseline_128byte |     0 |     25.77 ns |     0.120 ns |     0.112 ns |     25.74 ns |       - |      - |     - |         - |
//|   Baseline_384byte |     0 |     45.13 ns |     2.330 ns |     4.202 ns |     42.78 ns |       - |      - |     - |         - |
//|     Baseline_4byte |     1 |     14.98 ns |     0.074 ns |     0.066 ns |     14.96 ns |       - |      - |     - |         - |
//|    Baseline_32byte |     1 |     16.87 ns |     0.037 ns |     0.031 ns |     16.87 ns |       - |      - |     - |         - |
//|   Baseline_128byte |     1 |     36.64 ns |     0.282 ns |     0.250 ns |     36.54 ns |       - |      - |     - |         - |
//|   Baseline_384byte |     1 |     73.09 ns |     3.055 ns |     4.478 ns |     70.88 ns |       - |      - |     - |         - |
//|     Baseline_4byte |    10 |    174.47 ns |     1.295 ns |     1.148 ns |    174.12 ns |  0.0458 |      - |     - |     288 B |
//|    Baseline_32byte |    10 |    178.84 ns |     0.359 ns |     0.280 ns |    178.82 ns |  0.0458 |      - |     - |     288 B |
//|   Baseline_128byte |    10 |    341.33 ns |     3.845 ns |     3.597 ns |    339.12 ns |  0.0458 |      - |     - |     288 B |
//|   Baseline_384byte |    10 |    480.03 ns |     1.377 ns |     1.150 ns |    480.24 ns |  0.0458 |      - |     - |     288 B |
//|     Baseline_4byte |   100 |  2,269.84 ns |    12.283 ns |    10.257 ns |  2,266.39 ns |  0.5035 |      - |     - |    3168 B |
//|    Baseline_32byte |   100 |  2,255.84 ns |    11.618 ns |    10.868 ns |  2,253.45 ns |  0.5035 |      - |     - |    3168 B |
//|   Baseline_128byte |   100 |  3,241.46 ns |    17.759 ns |    16.612 ns |  3,243.49 ns |  0.5035 |      - |     - |    3168 B |
//|   Baseline_384byte |   100 |  5,095.53 ns |    43.932 ns |    41.094 ns |  5,093.84 ns |  0.5035 |      - |     - |    3168 B |
//|     Baseline_4byte |  1000 | 24,409.25 ns |    79.678 ns |    66.535 ns | 24,402.84 ns |  5.0659 |      - |     - |   31968 B |
//|    Baseline_32byte |  1000 | 22,290.28 ns |    48.992 ns |    45.827 ns | 22,296.44 ns |  5.0659 |      - |     - |   31968 B |
//|   Baseline_128byte |  1000 | 35,486.47 ns |   114.133 ns |   101.176 ns | 35,471.98 ns |  5.0659 |      - |     - |   31968 B |
//|   Baseline_384byte |  1000 | 59,970.33 ns |   281.730 ns |   249.746 ns | 59,994.19 ns |  5.0659 |      - |     - |   31968 B |

// ArrayBuffer Enumerator:
//|             Method | Count |         Mean |        Error |       StdDev |       Median |   Gen 0 |  Gen 1 | Gen 2 | Allocated |
//|------------------- |------ |-------------:|-------------:|-------------:|-------------:|--------:|-------:|------:|----------:|
//|      HashSet_4byte |     0 |     30.31 ns |     0.116 ns |     0.103 ns |     30.33 ns |  0.0076 |      - |     - |      48 B |
//|     HashSet_32byte |     0 |     29.91 ns |     0.230 ns |     0.215 ns |     29.90 ns |  0.0076 |      - |     - |      48 B |
//|    HashSet_128byte |     0 |     36.45 ns |     0.759 ns |     1.425 ns |     36.86 ns |  0.0076 |      - |     - |      48 B |
//|    HashSet_384byte |     0 |     35.78 ns |     0.709 ns |     0.788 ns |     36.06 ns |  0.0076 |      - |     - |      48 B |
//|      HashSet_4byte |     1 |     32.48 ns |     0.129 ns |     0.120 ns |     32.49 ns |  0.0089 |      - |     - |      56 B |
//|     HashSet_32byte |     1 |     35.49 ns |     0.577 ns |     0.540 ns |     35.39 ns |  0.0127 |      - |     - |      80 B |
//|    HashSet_128byte |     1 |     58.66 ns |     0.352 ns |     0.294 ns |     58.69 ns |  0.0280 |      - |     - |     176 B |
//|    HashSet_384byte |     1 |     95.72 ns |     0.955 ns |     0.846 ns |     95.84 ns |  0.0688 |      - |     - |     432 B |
//|      HashSet_4byte |    10 |     79.46 ns |     0.611 ns |     0.571 ns |     79.35 ns |  0.0139 |      - |     - |      88 B |
//|     HashSet_32byte |    10 |    105.21 ns |     0.420 ns |     0.351 ns |    105.28 ns |  0.0587 |      - |     - |     368 B |
//|    HashSet_128byte |    10 |    357.40 ns |     1.459 ns |     1.364 ns |    357.46 ns |  0.2112 |      - |     - |    1328 B |
//|    HashSet_384byte |    10 |    706.40 ns |     6.832 ns |     6.391 ns |    707.93 ns |  0.6189 |      - |     - |    3888 B |
//|      HashSet_4byte |   100 |    806.41 ns |     7.991 ns |     6.673 ns |    805.38 ns |  0.1945 |      - |     - |    1224 B |
//|     HashSet_32byte |   100 |  1,112.80 ns |    20.943 ns |    21.507 ns |  1,109.66 ns |  0.6237 |      - |     - |    3920 B |
//|    HashSet_128byte |   100 |  3,684.57 ns |    72.150 ns |    80.195 ns |  3,686.29 ns |  2.1515 | 0.0076 |     - |   13520 B |
//|    HashSet_384byte |   100 |  7,828.77 ns |    69.564 ns |    61.667 ns |  7,823.32 ns |  6.2103 | 0.0916 |     - |   39040 B |
//|      HashSet_4byte |  1000 | 11,806.29 ns |    53.757 ns |    50.284 ns | 11,801.55 ns |  1.7853 |      - |     - |   11232 B |
//|     HashSet_32byte |  1000 | 15,629.16 ns |    78.019 ns |    65.149 ns | 15,626.96 ns |  6.1951 |      - |     - |   39040 B |
//|    HashSet_128byte |  1000 | 41,979.60 ns |   496.932 ns |   464.830 ns | 41,937.07 ns | 21.5454 | 0.1221 |     - |  135280 B |
//|    HashSet_384byte |  1000 | 94,500.83 ns | 1,437.677 ns | 1,344.804 ns | 94,210.69 ns | 62.2559 | 0.8545 |     - |  391280 B |

// Combined:
//|          Method | Count |         Mean |      Error |     StdDev |       Median |  Gen 0 | Gen 1 | Gen 2 | Allocated |
//|---------------- |------ |-------------:|-----------:|-----------:|-------------:|-------:|------:|------:|----------:|
//|   HashSet_4byte |     0 |     25.11 ns |   0.043 ns |   0.040 ns |     25.11 ns |      - |     - |     - |         - |
//|  HashSet_32byte |     0 |     34.74 ns |   0.094 ns |   0.084 ns |     34.72 ns |      - |     - |     - |         - |
//| HashSet_128byte |     0 |     53.08 ns |   3.352 ns |   4.474 ns |     50.63 ns |      - |     - |     - |         - |
//| HashSet_384byte |     0 |     82.80 ns |   2.496 ns |   4.238 ns |     80.45 ns |      - |     - |     - |         - |
//|   HashSet_4byte |     1 |     28.68 ns |   0.060 ns |   0.053 ns |     28.67 ns |      - |     - |     - |         - |
//|  HashSet_32byte |     1 |     39.33 ns |   0.036 ns |   0.032 ns |     39.33 ns |      - |     - |     - |         - |
//| HashSet_128byte |     1 |     64.45 ns |   0.320 ns |   0.299 ns |     64.30 ns |      - |     - |     - |         - |
//| HashSet_384byte |     1 |    110.64 ns |   2.294 ns |   3.896 ns |    108.46 ns |      - |     - |     - |         - |
//|   HashSet_4byte |    10 |     75.74 ns |   0.328 ns |   0.256 ns |     75.75 ns | 0.0139 |     - |     - |      88 B |
//|  HashSet_32byte |    10 |    111.50 ns |   0.657 ns |   0.582 ns |    111.44 ns | 0.0587 |     - |     - |     368 B |
//| HashSet_128byte |    10 |    361.93 ns |   1.007 ns |   0.893 ns |    361.66 ns | 0.0458 |     - |     - |     288 B |
//| HashSet_384byte |    10 |    496.92 ns |   2.708 ns |   2.533 ns |    496.08 ns | 0.0458 |     - |     - |     288 B |
//|   HashSet_4byte |   100 |    702.91 ns |   9.011 ns |   7.988 ns |    701.19 ns | 0.0973 |     - |     - |     616 B |
//|  HashSet_32byte |   100 |    807.20 ns |   4.128 ns |   3.861 ns |    806.99 ns | 0.1602 |     - |     - |    1008 B |
//| HashSet_128byte |   100 |  3,473.62 ns |   9.920 ns |   7.745 ns |  3,472.46 ns | 0.5035 |     - |     - |    3168 B |
//| HashSet_384byte |   100 |  4,747.73 ns |   7.972 ns |   7.067 ns |  4,745.35 ns | 0.5035 |     - |     - |    3168 B |
//|   HashSet_4byte |  1000 | 10,656.21 ns |  39.046 ns |  36.524 ns | 10,647.60 ns | 0.7935 |     - |     - |    4984 B |
//|  HashSet_32byte |  1000 | 12,434.85 ns |  34.983 ns |  32.723 ns | 12,424.54 ns | 0.8545 |     - |     - |    5432 B |
//| HashSet_128byte |  1000 | 35,954.38 ns |  89.003 ns |  74.321 ns | 35,958.55 ns | 5.0659 |     - |     - |   31968 B |
//| HashSet_384byte |  1000 | 56,970.45 ns | 341.421 ns | 285.102 ns | 57,072.92 ns | 5.0659 |     - |     - |   31968 B |

[<PlainExporter; MemoryDiagnoser; IterationTime(100.0); MaxIterationCount(20)>]
[<GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByCategory)>]
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

    [<Benchmark(Baseline = true); BenchmarkCategory("4byte")>]
    member x.Baseline_4byte() =
        let mutable e = new HashSetEnumeratorBaseline<_>(collection4.Root)
        let mutable sum = 0
        while e.MoveNext() do
            sum <- sum + e.Current
        sum

    [<Benchmark(Baseline = true); BenchmarkCategory("32byte")>]
    member x.Baseline_32byte() =
        let mutable e = new HashSetEnumeratorBaseline<_>(collection32.Root)
        let mutable sum = 0.0
        while e.MoveNext() do
            sum <- sum + e.Current.a
        sum

    [<Benchmark(Baseline = true); BenchmarkCategory("128byte")>]
    member x.Baseline_128byte() =
        let mutable e = new HashSetEnumeratorBaseline<_>(collection128.Root)
        let mutable sum = 0.0
        while e.MoveNext() do
            sum <- sum + e.Current.w.c
        sum

    [<Benchmark(Baseline = true); BenchmarkCategory("384byte")>]
    member x.Baseline_384byte() =
        let mutable e = new HashSetEnumeratorBaseline<_>(collection384.Root)
        let mutable sum = 0.0
        while e.MoveNext() do
            sum <- sum + e.Current.s.y.c
        sum

    [<Benchmark; BenchmarkCategory("4byte")>]
    member x.HashSet_4byte() =
        let mutable sum = 0
        for e in collection4 do sum <- sum + e
        sum

    [<Benchmark; BenchmarkCategory("32byte")>]
    member x.HashSet_32byte() =
        let mutable sum = 0.0
        for e in collection32 do sum <- sum + e.a
        sum

    [<Benchmark; BenchmarkCategory("128byte")>]
    member x.HashSet_128byte() =
        let mutable sum = 0.0
        for e in collection128 do sum <- sum + e.w.c
        sum

    [<Benchmark; BenchmarkCategory("384byte")>]
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

//BenchmarkDotNet=v0.12.1, OS=Windows 10.0.19042
//Intel Core i7-8700K CPU 3.70GHz (Coffee Lake), 1 CPU, 12 logical and 6 physical cores
//.NET Core SDK=5.0.101
//  [Host]     : .NET Core 3.1.10 (CoreCLR 4.700.20.51601, CoreFX 4.700.20.51901), X64 RyuJIT DEBUG
//  DefaultJob : .NET Core 3.1.10 (CoreCLR 4.700.20.51601, CoreFX 4.700.20.51901), X64 RyuJIT

// Baseline: Array Buffer (c2bc22c9)
//|          Method | Count |          Mean |        Error |       StdDev |    Gen 0 |  Gen 1 | Gen 2 | Allocated |
//|---------------- |------ |--------------:|-------------:|-------------:|---------:|-------:|------:|----------:|
//|   HashMap_4byte |     0 |      24.62 ns |     0.074 ns |     0.061 ns |   0.0038 |      - |     - |      24 B |
//|  HashMap_32byte |     0 |      24.83 ns |     0.049 ns |     0.043 ns |   0.0038 |      - |     - |      24 B |
//| HashMap_128byte |     0 |      30.58 ns |     0.636 ns |     0.870 ns |   0.0038 |      - |     - |      24 B |
//| HashMap_384byte |     0 |      35.97 ns |     0.074 ns |     0.069 ns |   0.0038 |      - |     - |      24 B |
//|   HashMap_4byte |     1 |      33.57 ns |     0.097 ns |     0.081 ns |   0.0089 |      - |     - |      56 B |
//|  HashMap_32byte |     1 |      39.14 ns |     0.138 ns |     0.122 ns |   0.0178 |      - |     - |     112 B |
//| HashMap_128byte |     1 |      77.85 ns |     0.184 ns |     0.154 ns |   0.0484 |      - |     - |     304 B |
//| HashMap_384byte |     1 |     145.08 ns |     0.391 ns |     0.327 ns |   0.1299 | 0.0002 |     - |     816 B |
//|   HashMap_4byte |    10 |     121.78 ns |     0.184 ns |     0.172 ns |   0.0548 |      - |     - |     344 B |
//|  HashMap_32byte |    10 |     216.97 ns |     0.698 ns |     0.653 ns |   0.1440 | 0.0002 |     - |     904 B |
//| HashMap_128byte |    10 |     603.70 ns |     3.769 ns |     3.525 ns |   0.4501 | 0.0038 |     - |    2824 B |
//| HashMap_384byte |    10 |   1,293.39 ns |     3.204 ns |     2.997 ns |   1.2646 | 0.0362 |     - |    7944 B |
//|   HashMap_4byte |   100 |   1,472.21 ns |     1.957 ns |     1.735 ns |   0.5894 |      - |     - |    3704 B |
//|  HashMap_32byte |   100 |   2,554.01 ns |    10.132 ns |     9.477 ns |   1.4801 | 0.0076 |     - |    9304 B |
//| HashMap_128byte |   100 |   6,500.95 ns |    29.817 ns |    26.432 ns |   4.5395 | 0.0992 |     - |   28504 B |
//| HashMap_384byte |   100 |  13,733.24 ns |    60.002 ns |    50.105 ns |  12.6953 | 0.7477 |     - |   79704 B |
//|   HashMap_4byte |  1000 |  17,582.06 ns |   102.279 ns |    85.408 ns |   5.8289 |      - |     - |   36704 B |
//|  HashMap_32byte |  1000 |  29,200.80 ns |    64.734 ns |    54.056 ns |  14.8315 | 0.0916 |     - |   93208 B |
//| HashMap_128byte |  1000 |  71,812.17 ns |   613.578 ns |   543.921 ns |  45.2881 | 1.0986 |     - |  284816 B |
//| HashMap_384byte |  1000 | 155,848.97 ns | 1,450.227 ns | 1,285.588 ns | 126.9531 | 7.8125 |     - |  796928 B |

// Array Re-Use + Inline Stack Head
//|          Method | Count |          Mean |        Error |       StdDev |    Gen 0 |   Gen 1 | Gen 2 | Allocated |
//|---------------- |------ |--------------:|-------------:|-------------:|---------:|--------:|------:|----------:|
//|   HashMap_4byte |     0 |      25.03 ns |     0.035 ns |     0.031 ns |        - |       - |     - |         - |
//|  HashMap_32byte |     0 |      30.38 ns |     0.054 ns |     0.045 ns |        - |       - |     - |         - |
//| HashMap_128byte |     0 |      36.11 ns |     0.056 ns |     0.050 ns |        - |       - |     - |         - |
//| HashMap_384byte |     0 |      44.83 ns |     0.140 ns |     0.117 ns |        - |       - |     - |         - |
//|   HashMap_4byte |     1 |      39.66 ns |     0.110 ns |     0.097 ns |   0.0089 |       - |     - |      56 B |
//|  HashMap_32byte |     1 |      53.17 ns |     0.146 ns |     0.114 ns |   0.0178 |       - |     - |     112 B |
//| HashMap_128byte |     1 |      88.65 ns |     0.497 ns |     0.441 ns |   0.0484 |       - |     - |     304 B |
//| HashMap_384byte |     1 |     162.11 ns |     0.578 ns |     0.512 ns |   0.1299 |  0.0002 |     - |     816 B |
//|   HashMap_4byte |    10 |     118.16 ns |     0.538 ns |     0.449 ns |   0.0548 |       - |     - |     344 B |
//|  HashMap_32byte |    10 |     224.68 ns |     0.643 ns |     0.602 ns |   0.1440 |  0.0002 |     - |     904 B |
//| HashMap_128byte |    10 |     587.83 ns |     1.861 ns |     1.554 ns |   0.4501 |  0.0038 |     - |    2824 B |
//| HashMap_384byte |    10 |   1,284.98 ns |     4.560 ns |     4.266 ns |   1.2646 |  0.0362 |     - |    7944 B |
//|   HashMap_4byte |   100 |   1,291.43 ns |     3.800 ns |     3.368 ns |   0.4463 |       - |     - |    2808 B |
//|  HashMap_32byte |   100 |   2,442.68 ns |    12.110 ns |    11.328 ns |   1.3390 |  0.0114 |     - |    8408 B |
//| HashMap_128byte |   100 |   6,280.08 ns |    20.067 ns |    15.667 ns |   4.3945 |  0.1144 |     - |   27608 B |
//| HashMap_384byte |   100 |  13,786.82 ns |    68.897 ns |    61.075 ns |  12.5580 |  0.9003 |     - |   78808 B |
//|   HashMap_4byte |  1000 |  16,149.69 ns |    89.500 ns |    74.736 ns |   4.2725 |       - |     - |   26808 B |
//|  HashMap_32byte |  1000 |  28,088.65 ns |    92.151 ns |    81.689 ns |  13.2446 |  0.1221 |     - |   83096 B |
//| HashMap_128byte |  1000 |  70,710.24 ns |   682.394 ns |   569.830 ns |  43.7012 |  1.3428 |     - |  274872 B |
//| HashMap_384byte |  1000 | 158,957.35 ns | 1,504.544 ns | 1,407.352 ns | 125.2441 | 10.0098 |     - |  786936 B |

[<PlainExporter; MemoryDiagnoser>]
type HashMapEnumeratorBenchmark() =

    let mutable collection4 = HashMap.empty<int,int>
    let mutable collection32 = HashMap.empty<Struct32,Struct32>
    let mutable collection128 = HashMap.empty<Struct128,Struct128>
    let mutable collection384 = HashMap.empty<Struct384,Struct384>
   
    [<Params(0, 1, 10, 100, 1000); DefaultValue>]
    //[<Params(0, 1, 10); DefaultValue>]
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
