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

    member x.Sum() = 
        x.a + x.b + x.c + x.d

[<Struct>]
type Struct128 = 
    {
        mutable x : Struct32
        mutable y : Struct32
        mutable z : Struct32
        mutable w : Struct32
    }

    member x.Sum() = 
        x.x.Sum() + x.y.Sum() + x.z.Sum() + x.w.Sum()

[<Struct>] 
type Struct384 = 
    {
        mutable r : Struct128
        mutable s : Struct128
        mutable t : Struct128
    }

    member x.Sum() = 
        x.r.Sum() + x.s.Sum() + x.t.Sum()

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
//|          Method | Count |          Mean |        Error |       StdDev |   Gen 0 | Gen 1 | Gen 2 | Allocated |
//|---------------- |------ |--------------:|-------------:|-------------:|--------:|------:|------:|----------:|
//|   HashSet_4byte |     0 |      25.08 ns |     0.137 ns |     0.128 ns |       - |     - |     - |         - |
//|  HashSet_32byte |     0 |      34.42 ns |     0.051 ns |     0.039 ns |       - |     - |     - |         - |
//| HashSet_128byte |     0 |      51.65 ns |     0.278 ns |     0.260 ns |       - |     - |     - |         - |
//| HashSet_384byte |     0 |      82.35 ns |     0.140 ns |     0.124 ns |       - |     - |     - |         - |
//|   HashSet_4byte |     1 |      28.66 ns |     0.014 ns |     0.013 ns |       - |     - |     - |         - |
//|  HashSet_32byte |     1 |      39.38 ns |     0.089 ns |     0.079 ns |       - |     - |     - |         - |
//| HashSet_128byte |     1 |      65.44 ns |     0.138 ns |     0.129 ns |       - |     - |     - |         - |
//| HashSet_384byte |     1 |     113.45 ns |     0.564 ns |     0.528 ns |       - |     - |     - |         - |
//|   HashSet_4byte |    10 |      67.41 ns |     0.197 ns |     0.165 ns |  0.0101 |     - |     - |      64 B |
//|  HashSet_32byte |    10 |      99.69 ns |     0.335 ns |     0.280 ns |  0.0548 |     - |     - |     344 B |
//| HashSet_128byte |    10 |     363.62 ns |     0.894 ns |     0.747 ns |  0.0458 |     - |     - |     288 B |
//| HashSet_384byte |    10 |     493.11 ns |     0.992 ns |     0.828 ns |  0.0458 |     - |     - |     288 B |
//|   HashSet_4byte |   100 |     590.98 ns |     9.345 ns |     8.742 ns |  0.0591 |     - |     - |     376 B |
//|  HashSet_32byte |   100 |     739.51 ns |     2.837 ns |     2.653 ns |  0.1259 |     - |     - |     792 B |
//| HashSet_128byte |   100 |   3,314.66 ns |    10.699 ns |     8.934 ns |  0.5035 |     - |     - |    3168 B |
//| HashSet_384byte |   100 |   4,726.22 ns |     9.861 ns |     9.224 ns |  0.5035 |     - |     - |    3168 B |
//|   HashSet_4byte |  1000 |   9,464.27 ns |    44.243 ns |    36.945 ns |  0.4578 |     - |     - |    2872 B |
//|  HashSet_32byte |  1000 |  11,474.19 ns |    38.816 ns |    34.409 ns |  0.5188 |     - |     - |    3320 B |
//| HashSet_128byte |  1000 |  34,324.34 ns |   105.490 ns |    93.514 ns |  5.0659 |     - |     - |   31968 B |
//| HashSet_384byte |  1000 |  55,399.44 ns |   326.819 ns |   272.909 ns |  5.0659 |     - |     - |   31968 B |
//|   HashSet_4byte | 10000 | 153,310.89 ns |   536.039 ns |   475.185 ns |  4.6387 |     - |     - |   29180 B |
//|  HashSet_32byte | 10000 | 179,191.35 ns |   307.093 ns |   287.255 ns |  4.6387 |     - |     - |   29880 B |
//| HashSet_128byte | 10000 | 408,175.31 ns |   682.289 ns |   604.831 ns | 50.7813 |     - |     - |  319969 B |
//| HashSet_384byte | 10000 | 627,705.97 ns | 2,943.169 ns | 2,609.042 ns | 50.7813 |     - |     - |  319969 B |

// No large struct / single value optimization
//|          Method | Count |          Mean |        Error |       StdDev |  Gen 0 | Gen 1 | Gen 2 | Allocated |
//|---------------- |------ |--------------:|-------------:|-------------:|-------:|------:|------:|----------:|
//|   HashSet_4byte |     0 |      25.43 ns |     0.026 ns |     0.023 ns |      - |     - |     - |         - |
//|  HashSet_32byte |     0 |      25.76 ns |     0.040 ns |     0.033 ns |      - |     - |     - |         - |
//| HashSet_128byte |     0 |      30.39 ns |     0.055 ns |     0.046 ns |      - |     - |     - |         - |
//| HashSet_384byte |     0 |      31.66 ns |     0.035 ns |     0.031 ns |      - |     - |     - |         - |
//|   HashSet_4byte |     1 |      33.91 ns |     0.284 ns |     0.266 ns | 0.0051 |     - |     - |      32 B |
//|  HashSet_32byte |     1 |      35.62 ns |     0.114 ns |     0.101 ns | 0.0089 |     - |     - |      56 B |
//| HashSet_128byte |     1 |      51.14 ns |     0.296 ns |     0.247 ns | 0.0242 |     - |     - |     152 B |
//| HashSet_384byte |     1 |      87.34 ns |     0.334 ns |     0.312 ns | 0.0650 |     - |     - |     408 B |
//|   HashSet_4byte |    10 |      65.04 ns |     0.289 ns |     0.241 ns | 0.0101 |     - |     - |      64 B |
//|  HashSet_32byte |    10 |      83.05 ns |     0.488 ns |     0.433 ns | 0.0548 |     - |     - |     344 B |
//| HashSet_128byte |    10 |     321.82 ns |     1.641 ns |     1.454 ns | 0.2074 |     - |     - |    1304 B |
//| HashSet_384byte |    10 |     593.02 ns |     2.109 ns |     1.870 ns | 0.6151 |     - |     - |    3864 B |
//|   HashSet_4byte |   100 |     568.22 ns |     3.463 ns |     3.239 ns | 0.0591 |     - |     - |     376 B |
//|  HashSet_32byte |   100 |     647.95 ns |     7.292 ns |     6.821 ns | 0.1259 |     - |     - |     792 B |
//| HashSet_128byte |   100 |   2,799.24 ns |     5.333 ns |     4.728 ns | 0.3700 |     - |     - |    2328 B |
//| HashSet_384byte |   100 |   5,235.66 ns |    10.270 ns |     9.104 ns | 1.0147 |     - |     - |    6392 B |
//|   HashSet_4byte |  1000 |   9,341.03 ns |   146.538 ns |   114.407 ns | 0.4578 |     - |     - |    2872 B |
//|  HashSet_32byte |  1000 |  10,938.12 ns |    43.668 ns |    38.710 ns | 0.5188 |     - |     - |    3320 B |
//| HashSet_128byte |  1000 |  29,332.99 ns |    47.806 ns |    44.717 ns | 0.7629 |     - |     - |    4952 B |
//| HashSet_384byte |  1000 |  63,014.94 ns |   290.430 ns |   257.458 ns | 1.3428 |     - |     - |    9048 B |
//|   HashSet_4byte | 10000 | 151,950.23 ns |   190.814 ns |   159.338 ns | 4.6387 |     - |     - |   29176 B |
//|  HashSet_32byte | 10000 | 179,635.07 ns |   692.861 ns |   648.103 ns | 4.6387 |     - |     - |   29880 B |
//| HashSet_128byte | 10000 | 357,065.97 ns | 1,280.317 ns | 1,197.610 ns | 4.8828 |     - |     - |   31449 B |
//| HashSet_384byte | 10000 | 749,718.22 ns | 1,554.635 ns | 1,298.191 ns | 4.8828 |     - |     - |   35545 B |


[<PlainExporter; MemoryDiagnoser>]
type HashSetEnumeratorBenchmark() =

    let mutable collection4 = HashSet.empty<int>
    let mutable collection32 = HashSet.empty<Struct32>
    let mutable collection128 = HashSet.empty<Struct128>
    let mutable collection384 = HashSet.empty<Struct384>
   
    [<Params(0, 1, 10, 100, 1000, 10000); DefaultValue>]
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

//BenchmarkDotNet=v0.12.1, OS=Windows 10.0.19042
//Intel Core i7-8700K CPU 3.70GHz (Coffee Lake), 1 CPU, 12 logical and 6 physical cores
//.NET Core SDK=5.0.101
//  [Host]     : .NET Core 3.1.10 (CoreCLR 4.700.20.51601, CoreFX 4.700.20.51901), X64 RyuJIT DEBUG
//  DefaultJob : .NET Core 3.1.10 (CoreCLR 4.700.20.51601, CoreFX 4.700.20.51901), X64 RyuJIT

// Baseline: Array Buffer (c2bc22c9)
//|          Method | Count |          Mean |        Error |       StdDev |        Median |    Gen 0 |  Gen 1 | Gen 2 | Allocated |
//|---------------- |------ |--------------:|-------------:|-------------:|--------------:|---------:|-------:|------:|----------:|
//|   HashMap_4byte |     0 |      19.92 ns |     0.055 ns |     0.052 ns |      19.92 ns |   0.0038 |      - |     - |      24 B |
//|  HashMap_32byte |     0 |      20.79 ns |     0.044 ns |     0.041 ns |      20.80 ns |   0.0038 |      - |     - |      24 B |
//| HashMap_128byte |     0 |      26.23 ns |     0.535 ns |     0.767 ns |      26.63 ns |   0.0038 |      - |     - |      24 B |
//| HashMap_384byte |     0 |      31.14 ns |     0.076 ns |     0.067 ns |      31.14 ns |   0.0038 |      - |     - |      24 B |
//|   HashMap_4byte |     1 |      23.50 ns |     0.050 ns |     0.041 ns |      23.50 ns |   0.0051 |      - |     - |      32 B |
//|  HashMap_32byte |     1 |      29.23 ns |     0.142 ns |     0.133 ns |      29.22 ns |   0.0140 |      - |     - |      88 B |
//| HashMap_128byte |     1 |      91.50 ns |     0.275 ns |     0.244 ns |      91.50 ns |   0.0446 |      - |     - |     280 B |
//| HashMap_384byte |     1 |     171.74 ns |     0.881 ns |     0.824 ns |     171.85 ns |   0.1261 |      - |     - |     792 B |
//|   HashMap_4byte |    10 |      66.84 ns |     0.370 ns |     0.328 ns |      66.80 ns |   0.0166 |      - |     - |     104 B |
//|  HashMap_32byte |    10 |     125.46 ns |     1.000 ns |     0.835 ns |     125.85 ns |   0.1056 |      - |     - |     664 B |
//| HashMap_128byte |    10 |     730.88 ns |     3.934 ns |     3.680 ns |     730.56 ns |   0.4110 |      - |     - |    2584 B |
//| HashMap_384byte |    10 |   1,615.65 ns |     9.048 ns |     8.464 ns |   1,616.65 ns |   1.2264 |      - |     - |    7704 B |
//|   HashMap_4byte |   100 |     699.99 ns |     2.121 ns |     1.984 ns |     699.88 ns |   0.2069 |      - |     - |    1304 B |
//|  HashMap_32byte |   100 |   1,377.93 ns |    27.511 ns |    34.793 ns |   1,356.60 ns |   1.0986 | 0.0019 |     - |    6904 B |
//| HashMap_128byte |   100 |   7,791.09 ns |    17.078 ns |    15.975 ns |   7,790.51 ns |   4.1504 | 0.0305 |     - |   26104 B |
//| HashMap_384byte |   100 |  15,651.79 ns |    54.366 ns |    50.854 ns |  15,644.78 ns |  12.2986 | 0.3357 |     - |   77304 B |
//|   HashMap_4byte |  1000 |  12,020.42 ns |    23.089 ns |    19.281 ns |  12,013.31 ns |   2.0142 |      - |     - |   12704 B |
//|  HashMap_32byte |  1000 |  19,519.09 ns |   251.869 ns |   223.276 ns |  19,597.36 ns |  11.0168 | 0.0305 |     - |   69208 B |
//| HashMap_128byte |  1000 |  89,152.10 ns |   374.365 ns |   350.182 ns |  89,087.29 ns |  41.5039 | 0.3662 |     - |  260816 B |
//| HashMap_384byte |  1000 | 181,187.04 ns | 1,167.433 ns | 1,034.899 ns | 181,628.78 ns | 123.0469 | 4.1504 |     - |  772928 B |

// Array Buffer (with re-use) + Inline Stack Head + Single Value / Large Struct optimization
//|          Method | Count |          Mean |        Error |       StdDev |        Median |  Gen 0 | Gen 1 | Gen 2 | Allocated |
//|---------------- |------ |--------------:|-------------:|-------------:|--------------:|-------:|------:|------:|----------:|
//|   HashMap_4byte |     0 |      17.35 ns |     0.212 ns |     0.199 ns |      17.36 ns |      - |     - |     - |         - |
//|  HashMap_32byte |     0 |      34.53 ns |     0.037 ns |     0.029 ns |      34.53 ns |      - |     - |     - |         - |
//| HashMap_128byte |     0 |      49.96 ns |     0.108 ns |     0.106 ns |      49.98 ns |      - |     - |     - |         - |
//| HashMap_384byte |     0 |      78.87 ns |     0.138 ns |     0.129 ns |      78.89 ns |      - |     - |     - |         - |
//|   HashMap_4byte |     1 |      23.54 ns |     0.024 ns |     0.019 ns |      23.54 ns |      - |     - |     - |         - |
//|  HashMap_32byte |     1 |      43.74 ns |     0.155 ns |     0.145 ns |      43.69 ns |      - |     - |     - |         - |
//| HashMap_128byte |     1 |     113.04 ns |     2.000 ns |     1.871 ns |     114.03 ns |      - |     - |     - |         - |
//| HashMap_384byte |     1 |     194.90 ns |     0.350 ns |     0.310 ns |     194.96 ns |      - |     - |     - |         - |
//|   HashMap_4byte |    10 |      65.70 ns |     0.181 ns |     0.151 ns |      65.63 ns | 0.0166 |     - |     - |     104 B |
//|  HashMap_32byte |    10 |     161.99 ns |     3.246 ns |     5.424 ns |     164.33 ns | 0.1056 |     - |     - |     664 B |
//| HashMap_128byte |    10 |     854.63 ns |     1.181 ns |     1.047 ns |     854.71 ns | 0.0458 |     - |     - |     288 B |
//| HashMap_384byte |    10 |   1,385.36 ns |     2.491 ns |     1.945 ns |   1,385.47 ns | 0.0458 |     - |     - |     288 B |
//|   HashMap_4byte |   100 |     590.63 ns |     2.323 ns |     2.059 ns |     590.72 ns | 0.0648 |     - |     - |     408 B |
//|  HashMap_32byte |   100 |   1,214.72 ns |    24.045 ns |    48.021 ns |   1,241.19 ns | 0.2060 |     - |     - |    1304 B |
//| HashMap_128byte |   100 |   7,892.31 ns |    14.081 ns |    12.482 ns |   7,895.36 ns | 0.5035 |     - |     - |    3168 B |
//| HashMap_384byte |   100 |  14,025.49 ns |    22.458 ns |    19.908 ns |  14,028.80 ns | 0.5035 |     - |     - |    3168 B |
//|   HashMap_4byte |  1000 |  10,000.71 ns |    29.997 ns |    26.592 ns |   9,992.02 ns | 0.4425 |     - |     - |    2808 B |
//|  HashMap_32byte |  1000 |  16,933.68 ns |   335.652 ns |   359.143 ns |  17,116.61 ns | 0.6104 |     - |     - |    3992 B |
//| HashMap_128byte |  1000 |  87,796.68 ns |   216.238 ns |   168.824 ns |  87,778.74 ns | 5.0049 |     - |     - |   31968 B |
//| HashMap_384byte |  1000 | 156,444.82 ns | 1,315.265 ns | 1,098.306 ns | 156,779.03 ns | 4.8828 |     - |     - |   31968 B |

// Array Buffer (with re-use) + Inline Stack Head 
//|          Method | Count |          Mean |        Error |       StdDev |        Median |  Gen 0 | Gen 1 | Gen 2 | Allocated |
//|---------------- |------ |--------------:|-------------:|-------------:|--------------:|-------:|------:|------:|----------:|
//|   HashMap_4byte |     0 |      16.75 ns |     0.160 ns |     0.149 ns |      16.77 ns |      - |     - |     - |         - |
//|  HashMap_32byte |     0 |      18.30 ns |     0.038 ns |     0.033 ns |      18.30 ns |      - |     - |     - |         - |
//| HashMap_128byte |     0 |      24.37 ns |     0.470 ns |     0.440 ns |      24.61 ns |      - |     - |     - |         - |
//| HashMap_384byte |     0 |      43.12 ns |     1.160 ns |     3.271 ns |      43.98 ns |      - |     - |     - |         - |
//|   HashMap_4byte |     1 |      25.16 ns |     0.094 ns |     0.088 ns |      25.16 ns | 0.0051 |     - |     - |      32 B |
//|  HashMap_32byte |     1 |      35.27 ns |     0.296 ns |     0.247 ns |      35.19 ns | 0.0140 |     - |     - |      88 B |
//| HashMap_128byte |     1 |      96.77 ns |     0.403 ns |     0.336 ns |      96.90 ns | 0.0446 |     - |     - |     280 B |
//| HashMap_384byte |     1 |     183.44 ns |     0.793 ns |     0.703 ns |     183.38 ns | 0.1261 |     - |     - |     792 B |
//|   HashMap_4byte |    10 |      67.69 ns |     0.284 ns |     0.266 ns |      67.63 ns | 0.0166 |     - |     - |     104 B |
//|  HashMap_32byte |    10 |     131.11 ns |     1.131 ns |     0.883 ns |     131.10 ns | 0.1056 |     - |     - |     664 B |
//| HashMap_128byte |    10 |     741.13 ns |     3.314 ns |     2.587 ns |     741.58 ns | 0.4110 |     - |     - |    2584 B |
//| HashMap_384byte |    10 |   1,570.57 ns |     6.044 ns |     5.047 ns |   1,571.85 ns | 1.2264 |     - |     - |    7704 B |
//|   HashMap_4byte |   100 |     565.74 ns |     3.009 ns |     2.667 ns |     564.93 ns | 0.0648 |     - |     - |     408 B |
//|  HashMap_32byte |   100 |   1,054.24 ns |    20.941 ns |    41.335 ns |   1,028.40 ns | 0.2060 |     - |     - |    1304 B |
//| HashMap_128byte |   100 |   7,078.56 ns |   104.425 ns |    87.200 ns |   7,039.31 ns | 0.6943 |     - |     - |    4376 B |
//| HashMap_384byte |   100 |  13,672.17 ns |    35.821 ns |    33.507 ns |  13,665.58 ns | 1.9989 |     - |     - |   12568 B |
//|   HashMap_4byte |  1000 |   9,823.24 ns |    37.427 ns |    31.253 ns |   9,811.80 ns | 0.4425 |     - |     - |    2808 B |
//|  HashMap_32byte |  1000 |  15,889.38 ns |   302.858 ns |   268.476 ns |  15,926.79 ns | 0.6104 |     - |     - |    3992 B |
//| HashMap_128byte |  1000 |  78,504.07 ns |   291.097 ns |   272.292 ns |  78,527.56 ns | 0.9766 |     - |     - |    6840 B |
//| HashMap_384byte |  1000 | 155,439.85 ns | 1,907.683 ns | 1,784.448 ns | 154,764.11 ns | 2.1973 |     - |     - |   15096 B |

[<PlainExporter; MemoryDiagnoser>]
type HashMapStructEnumeratorBenchmark() =

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
        let mutable e = collection4.GetStructEnumerator()
        while e.MoveNext() do
            let struct(k, v) = e.Current
            sum <- sum + k + v
        sum

    [<Benchmark>]
    member x.HashMap_32byte() =
        let mutable sum = 0.0
        let mutable e = collection32.GetStructEnumerator()
        while e.MoveNext() do
            let struct(k, v) = e.Current
            sum <- sum + k.a + v.b
        sum

    [<Benchmark>]
    member x.HashMap_128byte() =
        let mutable sum = 0.0
        let mutable e = collection128.GetStructEnumerator()
        while e.MoveNext() do
            let struct(k, v) = e.Current
            sum <- sum + k.x.b + v.y.c
        sum

    [<Benchmark>]
    member x.HashMap_384byte() =
        let mutable sum = 0.0
        let mutable e = collection384.GetStructEnumerator()
        while e.MoveNext() do
            let struct(k, v) = e.Current
            sum <- sum + k.r.z.d + v.r.x.a
        sum

//BenchmarkDotNet=v0.12.1, OS=Windows 10.0.19042
//Intel Core i7-8700K CPU 3.70GHz (Coffee Lake), 1 CPU, 12 logical and 6 physical cores
//.NET Core SDK=5.0.101
//  [Host]     : .NET Core 3.1.10 (CoreCLR 4.700.20.51601, CoreFX 4.700.20.51901), X64 RyuJIT DEBUG
//  DefaultJob : .NET Core 3.1.10 (CoreCLR 4.700.20.51601, CoreFX 4.700.20.51901), X64 RyuJIT

// Baseline: HashMapStructEnumerator with Array Buffer (c2bc22c9)
//|                  Method | Count |          Mean |      Error |     StdDev |   Gen 0 |  Gen 1 | Gen 2 | Allocated |
//|------------------------ |------ |--------------:|-----------:|-----------:|--------:|-------:|------:|----------:|
//|   CountingHashSet_4byte |     0 |      29.39 ns |   0.610 ns |   0.599 ns |  0.0114 |      - |     - |      72 B |
//|  CountingHashSet_32byte |     0 |      35.37 ns |   0.080 ns |   0.067 ns |  0.0114 |      - |     - |      72 B |
//| CountingHashSet_128byte |     0 |      38.26 ns |   0.612 ns |   0.572 ns |  0.0114 |      - |     - |      72 B |
//| CountingHashSet_384byte |     0 |      45.38 ns |   0.285 ns |   0.267 ns |  0.0114 |      - |     - |      72 B |
//|   CountingHashSet_4byte |     1 |      30.85 ns |   0.130 ns |   0.121 ns |  0.0127 |      - |     - |      80 B |
//|  CountingHashSet_32byte |     1 |      46.14 ns |   0.920 ns |   1.259 ns |  0.0178 |      - |     - |     112 B |
//| CountingHashSet_128byte |     1 |      92.82 ns |   0.163 ns |   0.127 ns |  0.0331 |      - |     - |     208 B |
//| CountingHashSet_384byte |     1 |     147.31 ns |   0.438 ns |   0.388 ns |  0.0739 |      - |     - |     464 B |
//|   CountingHashSet_4byte |    10 |      72.10 ns |   0.255 ns |   0.226 ns |  0.0242 |      - |     - |     152 B |
//|  CountingHashSet_32byte |    10 |     190.34 ns |   0.383 ns |   0.359 ns |  0.0751 |      - |     - |     472 B |
//| CountingHashSet_128byte |    10 |     632.53 ns |   1.582 ns |   1.402 ns |  0.2279 |      - |     - |    1432 B |
//| CountingHashSet_384byte |    10 |   1,199.98 ns |   4.507 ns |   3.995 ns |  0.6351 |      - |     - |    3992 B |
//|   CountingHashSet_4byte |   100 |     758.01 ns |  13.489 ns |  12.617 ns |  0.2241 |      - |     - |    1408 B |
//|  CountingHashSet_32byte |   100 |   1,833.84 ns |   5.602 ns |   4.966 ns |  0.7248 |      - |     - |    4552 B |
//| CountingHashSet_128byte |   100 |   6,552.79 ns |  16.171 ns |  14.335 ns |  2.2507 | 0.0076 |     - |   14152 B |
//| CountingHashSet_384byte |   100 |  12,158.45 ns |  38.292 ns |  35.818 ns |  6.3171 | 0.0916 |     - |   39696 B |
//|   CountingHashSet_4byte |  1000 |  10,878.33 ns |  22.418 ns |  18.720 ns |  2.0599 |      - |     - |   12976 B |
//|  CountingHashSet_32byte |  1000 |  20,539.22 ns | 106.657 ns |  94.548 ns |  7.1411 |      - |     - |   44976 B |
//| CountingHashSet_128byte |  1000 |  68,235.60 ns | 241.701 ns | 201.831 ns | 22.4609 | 0.1221 |     - |  141144 B |
//| CountingHashSet_384byte |  1000 | 134,144.08 ns | 807.431 ns | 755.272 ns | 63.2324 | 0.9766 |     - |  397144 B |

// Array Buffer (with re-use) + Inline Stack Head: with field access
//|                  Method | Count |         Mean |      Error |     StdDev |       Median |  Gen 0 | Gen 1 | Gen 2 | Allocated |
//|------------------------ |------ |-------------:|-----------:|-----------:|-------------:|-------:|------:|------:|----------:|
//|   CountingHashSet_4byte |     0 |     19.47 ns |   0.023 ns |   0.020 ns |     19.47 ns |      - |     - |     - |         - |
//|  CountingHashSet_32byte |     0 |     19.76 ns |   0.046 ns |   0.040 ns |     19.77 ns |      - |     - |     - |         - |
//| CountingHashSet_128byte |     0 |     25.96 ns |   0.054 ns |   0.048 ns |     25.94 ns |      - |     - |     - |         - |
//| CountingHashSet_384byte |     0 |     30.98 ns |   0.636 ns |   0.891 ns |     30.37 ns |      - |     - |     - |         - |
//|   CountingHashSet_4byte |     1 |     28.28 ns |   0.318 ns |   0.298 ns |     28.41 ns | 0.0051 |     - |     - |      32 B |
//|  CountingHashSet_32byte |     1 |     30.90 ns |   0.126 ns |   0.112 ns |     30.89 ns | 0.0089 |     - |     - |      56 B |
//| CountingHashSet_128byte |     1 |     48.63 ns |   0.233 ns |   0.218 ns |     48.67 ns | 0.0242 |     - |     - |     152 B |
//| CountingHashSet_384byte |     1 |     84.61 ns |   0.672 ns |   0.629 ns |     84.78 ns | 0.0650 |     - |     - |     408 B |
//|   CountingHashSet_4byte |    10 |     62.23 ns |   0.215 ns |   0.201 ns |     62.16 ns | 0.0101 |     - |     - |      64 B |
//|  CountingHashSet_32byte |    10 |     77.25 ns |   0.706 ns |   0.626 ns |     77.24 ns | 0.0548 |     - |     - |     344 B |
//| CountingHashSet_128byte |    10 |    316.63 ns |   1.951 ns |   1.729 ns |    316.51 ns | 0.2074 |     - |     - |    1304 B |
//| CountingHashSet_384byte |    10 |    588.88 ns |   4.476 ns |   4.187 ns |    589.65 ns | 0.6151 |     - |     - |    3864 B |
//|   CountingHashSet_4byte |   100 |    564.24 ns |   2.037 ns |   1.806 ns |    563.90 ns | 0.0591 |     - |     - |     376 B |
//|  CountingHashSet_32byte |   100 |    761.92 ns |   2.605 ns |   2.175 ns |    762.47 ns | 0.1259 |     - |     - |     792 B |
//| CountingHashSet_128byte |   100 |  2,661.46 ns |   7.418 ns |   6.939 ns |  2,660.48 ns | 0.3700 |     - |     - |    2328 B |
//| CountingHashSet_384byte |   100 |  5,131.63 ns |   8.108 ns |   7.187 ns |  5,131.11 ns | 1.0147 |     - |     - |    6392 B |
//|   CountingHashSet_4byte |  1000 |  9,486.32 ns | 159.084 ns | 148.808 ns |  9,433.27 ns | 0.4578 |     - |     - |    2872 B |
//|  CountingHashSet_32byte |  1000 | 10,735.50 ns |  69.511 ns |  58.045 ns | 10,714.00 ns | 0.5188 |     - |     - |    3320 B |
//| CountingHashSet_128byte |  1000 | 27,918.00 ns | 188.222 ns | 157.174 ns | 27,918.36 ns | 0.7629 |     - |     - |    4952 B |
//| CountingHashSet_384byte |  1000 | 64,257.46 ns | 226.609 ns | 200.883 ns | 64,279.79 ns | 1.3428 |     - |     - |    9048 B |

// Array Buffer (with re-use) + Inline Stack Head: with x.Sum()
//|                  Method | Count |         Mean |      Error |     StdDev |  Gen 0 | Gen 1 | Gen 2 | Allocated |
//|------------------------ |------ |-------------:|-----------:|-----------:|-------:|------:|------:|----------:|
//|   CountingHashSet_4byte |     0 |     19.45 ns |   0.025 ns |   0.022 ns |      - |     - |     - |         - |
//|  CountingHashSet_32byte |     0 |     21.19 ns |   0.056 ns |   0.049 ns |      - |     - |     - |         - |
//| CountingHashSet_128byte |     0 |     26.02 ns |   0.063 ns |   0.059 ns |      - |     - |     - |         - |
//| CountingHashSet_384byte |     0 |     30.35 ns |   0.063 ns |   0.056 ns |      - |     - |     - |         - |
//|   CountingHashSet_4byte |     1 |     29.35 ns |   0.491 ns |   0.459 ns | 0.0051 |     - |     - |      32 B |
//|  CountingHashSet_32byte |     1 |     33.45 ns |   0.112 ns |   0.099 ns | 0.0089 |     - |     - |      56 B |
//| CountingHashSet_128byte |     1 |     57.01 ns |   0.137 ns |   0.128 ns | 0.0242 |     - |     - |     152 B |
//| CountingHashSet_384byte |     1 |    106.96 ns |   0.477 ns |   0.446 ns | 0.0650 |     - |     - |     408 B |
//|   CountingHashSet_4byte |    10 |     60.33 ns |   0.345 ns |   0.288 ns | 0.0101 |     - |     - |      64 B |
//|  CountingHashSet_32byte |    10 |     88.85 ns |   0.388 ns |   0.362 ns | 0.0548 |     - |     - |     344 B |
//| CountingHashSet_128byte |    10 |    374.72 ns |   1.594 ns |   1.491 ns | 0.2074 |     - |     - |    1304 B |
//| CountingHashSet_384byte |    10 |    800.62 ns |   4.620 ns |   3.607 ns | 0.6151 |     - |     - |    3864 B |
//|   CountingHashSet_4byte |   100 |    570.74 ns |   8.542 ns |   7.133 ns | 0.0591 |     - |     - |     376 B |
//|  CountingHashSet_32byte |   100 |    745.51 ns |   2.607 ns |   2.439 ns | 0.1259 |     - |     - |     792 B |
//| CountingHashSet_128byte |   100 |  3,276.92 ns |  20.769 ns |  19.427 ns | 0.3700 |     - |     - |    2328 B |
//| CountingHashSet_384byte |   100 |  7,174.83 ns |  32.239 ns |  30.156 ns | 1.0147 |     - |     - |    6392 B |
//|   CountingHashSet_4byte |  1000 |  9,190.37 ns |  49.697 ns |  41.500 ns | 0.4578 |     - |     - |    2872 B |
//|  CountingHashSet_32byte |  1000 | 11,571.38 ns |  32.859 ns |  30.737 ns | 0.5188 |     - |     - |    3320 B |
//| CountingHashSet_128byte |  1000 | 34,489.90 ns | 122.531 ns | 114.616 ns | 0.7324 |     - |     - |    4952 B |
//| CountingHashSet_384byte |  1000 | 81,543.80 ns | 681.020 ns | 637.027 ns | 1.3428 |     - |     - |    9048 B |

// Array Buffer (with re-use) + Inline Stack Head + Single / Large Value optimization: with field access
//|                  Method | Count |         Mean |      Error |     StdDev |       Median |  Gen 0 | Gen 1 | Gen 2 | Allocated |
//|------------------------ |------ |-------------:|-----------:|-----------:|-------------:|-------:|------:|------:|----------:|
//|   CountingHashSet_4byte |     0 |     19.66 ns |   0.270 ns |   0.252 ns |     19.55 ns |      - |     - |     - |         - |
//|  CountingHashSet_32byte |     0 |     29.91 ns |   0.169 ns |   0.158 ns |     29.85 ns |      - |     - |     - |         - |
//| CountingHashSet_128byte |     0 |     57.14 ns |   0.315 ns |   0.295 ns |     57.14 ns |      - |     - |     - |         - |
//| CountingHashSet_384byte |     0 |     90.67 ns |   0.206 ns |   0.161 ns |     90.64 ns |      - |     - |     - |         - |
//|   CountingHashSet_4byte |     1 |     26.28 ns |   0.132 ns |   0.123 ns |     26.29 ns |      - |     - |     - |         - |
//|  CountingHashSet_32byte |     1 |     34.77 ns |   0.536 ns |   0.501 ns |     34.57 ns |      - |     - |     - |         - |
//| CountingHashSet_128byte |     1 |     70.50 ns |   0.462 ns |   0.386 ns |     70.72 ns |      - |     - |     - |         - |
//| CountingHashSet_384byte |     1 |    117.88 ns |   2.317 ns |   2.845 ns |    119.75 ns |      - |     - |     - |         - |
//|   CountingHashSet_4byte |    10 |     64.96 ns |   0.258 ns |   0.215 ns |     64.92 ns | 0.0101 |     - |     - |      64 B |
//|  CountingHashSet_32byte |    10 |    143.20 ns |   1.704 ns |   1.594 ns |    142.77 ns | 0.0548 |     - |     - |     344 B |
//| CountingHashSet_128byte |    10 |    376.66 ns |   1.826 ns |   1.525 ns |    376.96 ns | 0.0458 |     - |     - |     288 B |
//| CountingHashSet_384byte |    10 |    509.28 ns |   7.090 ns |   6.285 ns |    506.15 ns | 0.0458 |     - |     - |     288 B |
//|   CountingHashSet_4byte |   100 |    572.76 ns |   6.446 ns |   5.033 ns |    572.71 ns | 0.0591 |     - |     - |     376 B |
//|  CountingHashSet_32byte |   100 |    711.78 ns |   3.453 ns |   3.230 ns |    711.21 ns | 0.1259 |     - |     - |     792 B |
//| CountingHashSet_128byte |   100 |  3,318.92 ns |  46.577 ns |  43.568 ns |  3,297.08 ns | 0.5035 |     - |     - |    3168 B |
//| CountingHashSet_384byte |   100 |  4,849.69 ns |  96.358 ns |  94.637 ns |  4,868.56 ns | 0.5035 |     - |     - |    3168 B |
//|   CountingHashSet_4byte |  1000 |  9,606.45 ns |  34.864 ns |  32.612 ns |  9,606.77 ns | 0.4578 |     - |     - |    2872 B |
//|  CountingHashSet_32byte |  1000 | 11,322.49 ns |  21.717 ns |  16.955 ns | 11,328.99 ns | 0.5188 |     - |     - |    3320 B |
//| CountingHashSet_128byte |  1000 | 34,252.31 ns | 122.533 ns | 114.617 ns | 34,282.18 ns | 5.0659 |     - |     - |   31968 B |
//| CountingHashSet_384byte |  1000 | 54,322.16 ns | 354.314 ns | 314.090 ns | 54,447.69 ns | 5.0659 |     - |     - |   31968 B |

// Array Buffer (with re-use) + Inline Stack Head + Single / Large Value optimization: with x.Sum()
//|                  Method | Count |         Mean |      Error |     StdDev |  Gen 0 | Gen 1 | Gen 2 | Allocated |
//|------------------------ |------ |-------------:|-----------:|-----------:|-------:|------:|------:|----------:|
//|   CountingHashSet_4byte |     0 |     19.42 ns |   0.055 ns |   0.046 ns |      - |     - |     - |         - |
//|  CountingHashSet_32byte |     0 |     32.18 ns |   0.050 ns |   0.045 ns |      - |     - |     - |         - |
//| CountingHashSet_128byte |     0 |     62.07 ns |   0.079 ns |   0.070 ns |      - |     - |     - |         - |
//| CountingHashSet_384byte |     0 |     89.04 ns |   0.231 ns |   0.205 ns |      - |     - |     - |         - |
//|   CountingHashSet_4byte |     1 |     25.62 ns |   0.164 ns |   0.145 ns |      - |     - |     - |         - |
//|  CountingHashSet_32byte |     1 |     40.70 ns |   0.180 ns |   0.151 ns |      - |     - |     - |         - |
//| CountingHashSet_128byte |     1 |     82.15 ns |   0.364 ns |   0.341 ns |      - |     - |     - |         - |
//| CountingHashSet_384byte |     1 |    144.05 ns |   0.365 ns |   0.324 ns |      - |     - |     - |         - |
//|   CountingHashSet_4byte |    10 |     64.12 ns |   0.210 ns |   0.186 ns | 0.0101 |     - |     - |      64 B |
//|  CountingHashSet_32byte |    10 |    113.51 ns |   0.445 ns |   0.394 ns | 0.0548 |     - |     - |     344 B |
//| CountingHashSet_128byte |    10 |    438.42 ns |   0.711 ns |   0.631 ns | 0.0458 |     - |     - |     288 B |
//| CountingHashSet_384byte |    10 |    736.63 ns |   2.028 ns |   1.798 ns | 0.0458 |     - |     - |     288 B |
//|   CountingHashSet_4byte |   100 |    576.36 ns |   2.835 ns |   2.513 ns | 0.0591 |     - |     - |     376 B |
//|  CountingHashSet_32byte |   100 |    806.46 ns |   3.312 ns |   2.765 ns | 0.1259 |     - |     - |     792 B |
//| CountingHashSet_128byte |   100 |  4,095.94 ns |   8.931 ns |   7.917 ns | 0.5035 |     - |     - |    3168 B |
//| CountingHashSet_384byte |   100 |  6,577.52 ns |   9.530 ns |   8.448 ns | 0.5035 |     - |     - |    3168 B |
//|   CountingHashSet_4byte |  1000 |  9,458.28 ns |  35.547 ns |  33.251 ns | 0.4578 |     - |     - |    2872 B |
//|  CountingHashSet_32byte |  1000 | 11,997.03 ns |  60.856 ns |  50.817 ns | 0.5188 |     - |     - |    3320 B |
//| CountingHashSet_128byte |  1000 | 41,867.02 ns | 149.785 ns | 132.780 ns | 5.0659 |     - |     - |   31969 B |
//| CountingHashSet_384byte |  1000 | 73,062.84 ns | 264.914 ns | 247.801 ns | 5.0049 |     - |     - |   31968 B |

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
        for e in collection32 do sum <- sum + e.Sum()
        sum

    [<Benchmark>]
    member x.CountingHashSet_128byte() =
        let mutable sum = 0.0
        for e in collection128 do sum <- sum + e.Sum()
        sum

    [<Benchmark>]
    member x.CountingHashSet_384byte() =
        let mutable sum = 0.0
        for e in collection384 do sum <- sum + e.Sum()
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
