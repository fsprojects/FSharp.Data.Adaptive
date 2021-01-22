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

//|               Method | Count |           Mean |         Error |        StdDev |  Gen 0 | Gen 1 | Gen 2 | Allocated |
//|--------------------- |------ |---------------:|--------------:|--------------:|-------:|------:|------:|----------:|
//|   HashSet_4byte_iter |     0 |       7.871 ns |     0.1751 ns |     0.1637 ns | 0.0076 |     - |     - |      48 B |
//|  HashSet_32byte_iter |     0 |       9.160 ns |     0.2082 ns |     0.3241 ns | 0.0076 |     - |     - |      48 B |
//| HashSet_128byte_iter |     0 |       8.941 ns |     0.1718 ns |     0.1607 ns | 0.0076 |     - |     - |      48 B |
//| HashSet_384byte_iter |     0 |       8.826 ns |     0.0852 ns |     0.0797 ns | 0.0076 |     - |     - |      48 B |
//|   HashSet_4byte_iter |     1 |      10.691 ns |     0.2341 ns |     0.3358 ns | 0.0076 |     - |     - |      48 B |
//|  HashSet_32byte_iter |     1 |      10.294 ns |     0.2348 ns |     0.3791 ns | 0.0076 |     - |     - |      48 B |
//| HashSet_128byte_iter |     1 |      12.930 ns |     0.2839 ns |     0.4743 ns | 0.0076 |     - |     - |      48 B |
//| HashSet_384byte_iter |     1 |      21.127 ns |     0.2199 ns |     0.1949 ns | 0.0076 |     - |     - |      48 B |
//|   HashSet_4byte_iter |    10 |      35.447 ns |     0.4979 ns |     0.4657 ns | 0.0076 |     - |     - |      48 B |
//|  HashSet_32byte_iter |    10 |      42.524 ns |     0.4315 ns |     0.3825 ns | 0.0076 |     - |     - |      48 B |
//| HashSet_128byte_iter |    10 |     135.675 ns |     0.9248 ns |     0.8198 ns | 0.0076 |     - |     - |      48 B |
//| HashSet_384byte_iter |    10 |     181.059 ns |     1.6915 ns |     1.4995 ns | 0.0076 |     - |     - |      48 B |
//|   HashSet_4byte_iter |   100 |     435.037 ns |     7.6653 ns |     6.7951 ns | 0.0076 |     - |     - |      48 B |
//|  HashSet_32byte_iter |   100 |     460.340 ns |     8.0539 ns |     7.5336 ns | 0.0076 |     - |     - |      48 B |
//| HashSet_128byte_iter |   100 |   1,360.282 ns |    12.7734 ns |    11.3233 ns | 0.0076 |     - |     - |      48 B |
//| HashSet_384byte_iter |   100 |   2,328.173 ns |    21.2734 ns |    19.8992 ns | 0.0076 |     - |     - |      48 B |
//|   HashSet_4byte_iter |  1000 |   8,942.912 ns |    78.8151 ns |    73.7237 ns |      - |     - |     - |      48 B |
//|  HashSet_32byte_iter |  1000 |  10,683.773 ns |    77.8126 ns |    72.7859 ns |      - |     - |     - |      48 B |
//| HashSet_128byte_iter |  1000 |  17,174.762 ns |    99.9052 ns |    93.4513 ns |      - |     - |     - |      48 B |
//| HashSet_384byte_iter |  1000 |  33,805.589 ns |   415.7795 ns |   347.1947 ns |      - |     - |     - |      48 B |
//|   HashSet_4byte_iter | 10000 | 130,367.648 ns |   809.8241 ns |   717.8880 ns |      - |     - |     - |      48 B |
//|  HashSet_32byte_iter | 10000 | 155,753.701 ns | 1,032.8234 ns |   966.1036 ns |      - |     - |     - |      48 B |
//| HashSet_128byte_iter | 10000 | 247,249.251 ns | 1,789.9260 ns | 1,674.2979 ns |      - |     - |     - |      48 B |
//| HashSet_384byte_iter | 10000 | 409,446.966 ns | 6,435.6232 ns | 6,019.8860 ns |      - |     - |     - |      49 B |

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

    [<Benchmark; BenchmarkCategory("4byte")>]
    member x.HashSet_4byte_iter() =
        let mutable sum = 0
        collection4 |> HashSet.iter (fun e -> sum <- sum + e)
        sum

    [<Benchmark; BenchmarkCategory("32byte")>]
    member x.HashSet_32byte_iter() =
        let mutable sum = 0.0
        collection32 |> HashSet.iter (fun e -> sum <- sum + e.a)
        sum

    [<Benchmark; BenchmarkCategory("128byte")>]
    member x.HashSet_128byte_iter() =
        let mutable sum = 0.0
        collection128 |> HashSet.iter (fun e -> sum <- sum + e.w.c)
        sum

    [<Benchmark; BenchmarkCategory("384byte")>]
    member x.HashSet_384byte_iter() =
        let mutable sum = 0.0
        collection384 |> HashSet.iter (fun e -> sum <- sum + e.s.y.c)
        sum

//BenchmarkDotNet=v0.12.1, OS=Windows 10.0.19042
//Intel Core i7-8700K CPU 3.70GHz (Coffee Lake), 1 CPU, 12 logical and 6 physical cores
//.NET Core SDK=5.0.101
//    [Host]     : .NET Core 3.1.10 (CoreCLR 4.700.20.51601, CoreFX 4.700.20.51901), X64 RyuJIT DEBUG
//    DefaultJob : .NET Core 3.1.10 (CoreCLR 4.700.20.51601, CoreFX 4.700.20.51901), X64 RyuJIT
 
// Previous (cdaa5f8c): MapExt Traversal (Stack)
//|            Method | Count |         Mean |      Error |     StdDev |       Median |   Gen 0 |  Gen 1 | Gen 2 | Allocated |
//|------------------ |------ |-------------:|-----------:|-----------:|-------------:|--------:|-------:|------:|----------:|
//|   IndexList_4byte |     0 |     21.00 ns |   0.219 ns |   0.205 ns |     21.09 ns |  0.0115 |      - |     - |      72 B |
//|  IndexList_32byte |     0 |     23.55 ns |   0.494 ns |   0.643 ns |     23.66 ns |  0.0153 |      - |     - |      96 B |
//| IndexList_128byte |     0 |     59.56 ns |   1.209 ns |   1.918 ns |     60.03 ns |  0.0305 |      - |     - |     192 B |
//| IndexList_384byte |     0 |     96.86 ns |   1.932 ns |   3.435 ns |     94.92 ns |  0.0714 |      - |     - |     448 B |
//|   IndexList_4byte |     1 |     22.67 ns |   0.169 ns |   0.158 ns |     22.62 ns |  0.0115 |      - |     - |      72 B |
//|  IndexList_32byte |     1 |     25.41 ns |   0.466 ns |   0.436 ns |     25.50 ns |  0.0153 |      - |     - |      96 B |
//| IndexList_128byte |     1 |     76.94 ns |   0.322 ns |   0.269 ns |     76.86 ns |  0.0305 |      - |     - |     192 B |
//| IndexList_384byte |     1 |    138.87 ns |   1.182 ns |   1.106 ns |    138.48 ns |  0.0713 |      - |     - |     448 B |
//|   IndexList_4byte |    10 |    185.17 ns |   0.922 ns |   0.719 ns |    185.10 ns |  0.1082 |      - |     - |     680 B |
//|  IndexList_32byte |    10 |    197.17 ns |   0.886 ns |   0.829 ns |    197.04 ns |  0.1311 |      - |     - |     824 B |
//| IndexList_128byte |    10 |    511.09 ns |   2.115 ns |   1.978 ns |    511.38 ns |  0.2232 |      - |     - |    1400 B |
//| IndexList_384byte |    10 |    907.30 ns |   6.465 ns |   5.731 ns |    906.66 ns |  0.4673 |      - |     - |    2936 B |
//|   IndexList_4byte |   100 |  1,825.69 ns |  11.721 ns |  10.964 ns |  1,826.71 ns |  1.0262 |      - |     - |    6440 B |
//|  IndexList_32byte |   100 |  2,075.86 ns |  19.533 ns |  18.271 ns |  2,067.26 ns |  1.2207 |      - |     - |    7664 B |
//| IndexList_128byte |   100 |  5,177.58 ns |  54.479 ns |  50.959 ns |  5,177.13 ns |  1.9989 |      - |     - |   12560 B |
//| IndexList_384byte |   100 |  8,465.16 ns |  30.295 ns |  26.855 ns |  8,453.93 ns |  4.0741 | 0.0153 |     - |   25616 B |
//|   IndexList_4byte |  1000 | 22,283.35 ns | 170.694 ns | 159.667 ns | 22,265.40 ns | 10.1929 |      - |     - |   64040 B |
//|  IndexList_32byte |  1000 | 22,253.35 ns | 161.684 ns | 143.329 ns | 22,200.70 ns | 12.1155 |      - |     - |   76064 B |
//| IndexList_128byte |  1000 | 50,774.44 ns | 264.672 ns | 234.625 ns | 50,697.92 ns | 19.7754 | 0.0610 |     - |  124160 B |
//| IndexList_384byte |  1000 | 87,921.30 ns | 362.902 ns | 321.703 ns | 87,822.04 ns | 40.1611 | 0.2441 |     - |  252416 B |

// MapExt Traversal with Struct Enumerator + Inline Stack Head
//|            Method | Count |          Mean |       Error |      StdDev |        Median |   Gen 0 |  Gen 1 | Gen 2 | Allocated |
//|------------------ |------ |--------------:|------------:|------------:|--------------:|--------:|-------:|------:|----------:|
//|   IndexList_4byte |     0 |      6.306 ns |   0.1515 ns |   0.1803 ns |      6.346 ns |       - |      - |     - |         - |
//|  IndexList_32byte |     0 |     17.602 ns |   0.1250 ns |   0.1169 ns |     17.574 ns |       - |      - |     - |         - |
//| IndexList_128byte |     0 |     49.097 ns |   0.4557 ns |   0.4263 ns |     49.263 ns |       - |      - |     - |         - |
//| IndexList_384byte |     0 |     88.861 ns |   1.7986 ns |   3.7145 ns |     90.427 ns |       - |      - |     - |         - |
//|   IndexList_4byte |     1 |     11.922 ns |   0.0630 ns |   0.0526 ns |     11.911 ns |       - |      - |     - |         - |
//|  IndexList_32byte |     1 |     22.510 ns |   0.3261 ns |   0.3051 ns |     22.423 ns |       - |      - |     - |         - |
//| IndexList_128byte |     1 |     74.690 ns |   0.5277 ns |   0.4678 ns |     74.588 ns |       - |      - |     - |         - |
//| IndexList_384byte |     1 |    136.682 ns |   0.9359 ns |   0.8754 ns |    136.641 ns |       - |      - |     - |         - |
//|   IndexList_4byte |    10 |    203.043 ns |   1.4271 ns |   1.2651 ns |    202.483 ns |  0.0713 |      - |     - |     448 B |
//|  IndexList_32byte |    10 |    209.698 ns |   2.1047 ns |   1.9688 ns |    209.033 ns |  0.0904 |      - |     - |     568 B |
//| IndexList_128byte |    10 |    532.511 ns |   2.9255 ns |   2.7365 ns |    532.692 ns |  0.1669 |      - |     - |    1048 B |
//| IndexList_384byte |    10 |    900.652 ns |   7.8936 ns |   7.3837 ns |    897.281 ns |  0.3710 |      - |     - |    2328 B |
//|   IndexList_4byte |   100 |  2,057.142 ns |  11.1465 ns |  10.4264 ns |  2,054.970 ns |  0.7591 |      - |     - |    4768 B |
//|  IndexList_32byte |   100 |  2,113.725 ns |  32.4277 ns |  30.3329 ns |  2,117.673 ns |  0.9499 |      - |     - |    5968 B |
//| IndexList_128byte |   100 |  5,233.972 ns |  48.0007 ns |  44.8999 ns |  5,235.715 ns |  1.7090 |      - |     - |   10768 B |
//| IndexList_384byte |   100 |  8,735.019 ns | 135.8084 ns | 127.0353 ns |  8,702.292 ns |  3.7537 |      - |     - |   23568 B |
//|   IndexList_4byte |  1000 | 21,614.283 ns | 239.3638 ns | 223.9010 ns | 21,576.672 ns |  7.6294 |      - |     - |   47968 B |
//|  IndexList_32byte |  1000 | 21,523.706 ns | 245.4555 ns | 217.5899 ns | 21,546.075 ns |  9.5520 |      - |     - |   59968 B |
//| IndexList_128byte |  1000 | 51,933.494 ns | 490.9562 ns | 435.2199 ns | 51,737.250 ns | 17.1509 |      - |     - |  107968 B |
//| IndexList_384byte |  1000 | 89,822.576 ns | 985.7413 ns | 873.8340 ns | 90,022.729 ns | 37.5977 | 0.2441 |     - |  235968 B |

// Baseline: ToArray
//|            Method | Count |          Mean |        Error |       StdDev |        Median |    Gen 0 |    Gen 1 |    Gen 2 | Allocated |
//|------------------ |------ |--------------:|-------------:|-------------:|--------------:|---------:|---------:|---------:|----------:|
//|   IndexList_4byte |     0 |      16.63 ns |     0.108 ns |     0.096 ns |      16.62 ns |   0.0038 |        - |        - |      24 B |
//|  IndexList_32byte |     0 |      23.38 ns |     0.169 ns |     0.149 ns |      23.43 ns |   0.0038 |        - |        - |      24 B |
//| IndexList_128byte |     0 |      28.12 ns |     0.199 ns |     0.166 ns |      28.09 ns |   0.0038 |        - |        - |      24 B |
//| IndexList_384byte |     0 |      33.65 ns |     0.211 ns |     0.302 ns |      33.58 ns |   0.0038 |        - |        - |      24 B |
//|   IndexList_4byte |     1 |      17.44 ns |     0.155 ns |     0.145 ns |      17.34 ns |   0.0051 |        - |        - |      32 B |
//|  IndexList_32byte |     1 |      25.97 ns |     0.125 ns |     0.111 ns |      25.97 ns |   0.0089 |        - |        - |      56 B |
//| IndexList_128byte |     1 |      46.43 ns |     0.849 ns |     0.794 ns |      46.90 ns |   0.0242 |        - |        - |     152 B |
//| IndexList_384byte |     1 |      90.72 ns |     1.830 ns |     3.481 ns |      92.28 ns |   0.0650 |        - |        - |     408 B |
//|   IndexList_4byte |    10 |      76.26 ns |     0.583 ns |     0.456 ns |      76.40 ns |   0.0139 |        - |        - |      88 B |
//|  IndexList_32byte |    10 |     122.65 ns |     1.002 ns |     0.888 ns |     122.62 ns |   0.0587 |        - |        - |     368 B |
//| IndexList_128byte |    10 |     316.59 ns |     4.775 ns |     4.466 ns |     317.55 ns |   0.2112 |        - |        - |    1328 B |
//| IndexList_384byte |    10 |     643.50 ns |    11.330 ns |    10.043 ns |     645.49 ns |   0.6189 |        - |        - |    3888 B |
//|   IndexList_4byte |   100 |   1,010.72 ns |     8.411 ns |     7.457 ns |   1,011.54 ns |   0.1812 |        - |        - |    1144 B |
//|  IndexList_32byte |   100 |   1,147.85 ns |     3.972 ns |     3.521 ns |   1,148.79 ns |   0.6275 |        - |        - |    3944 B |
//| IndexList_128byte |   100 |   3,281.35 ns |    20.382 ns |    18.068 ns |   3,277.45 ns |   2.1515 |        - |        - |   13544 B |
//| IndexList_384byte |   100 |   6,693.57 ns |    48.170 ns |    42.701 ns |   6,682.28 ns |   6.2103 |        - |        - |   39144 B |
//|   IndexList_4byte |  1000 |  10,021.10 ns |    86.600 ns |    72.315 ns |  10,010.63 ns |   1.6479 |   0.0153 |        - |   10344 B |
//|  IndexList_32byte |  1000 |  11,178.42 ns |    84.479 ns |    79.022 ns |  11,179.39 ns |   6.0883 |        - |        - |   38344 B |
//| IndexList_128byte |  1000 |  87,514.29 ns |   646.150 ns |   504.471 ns |  87,365.21 ns |  39.9170 |  39.9170 |  39.9170 |  134344 B |
//| IndexList_384byte |  1000 | 255,008.64 ns | 4,871.563 ns | 4,556.863 ns | 253,265.92 ns | 110.8398 | 110.8398 | 110.8398 |  390345 B |

// Array Buffer with re-use + Inline Stack Head + Single/Large Value Optimization
//|            Method | Count |         Mean |      Error |     StdDev |   Gen 0 |  Gen 1 | Gen 2 | Allocated |
//|------------------ |------ |-------------:|-----------:|-----------:|--------:|-------:|------:|----------:|
//|   IndexList_4byte |     0 |     19.60 ns |   0.080 ns |   0.075 ns |       - |      - |     - |         - |
//|  IndexList_32byte |     0 |     29.01 ns |   0.130 ns |   0.122 ns |       - |      - |     - |         - |
//| IndexList_128byte |     0 |     60.40 ns |   1.197 ns |   1.330 ns |       - |      - |     - |         - |
//| IndexList_384byte |     0 |     94.87 ns |   0.409 ns |   0.383 ns |       - |      - |     - |         - |
//|   IndexList_4byte |     1 |     27.84 ns |   0.076 ns |   0.064 ns |       - |      - |     - |         - |
//|  IndexList_32byte |     1 |     37.08 ns |   0.172 ns |   0.153 ns |       - |      - |     - |         - |
//| IndexList_128byte |     1 |     86.51 ns |   0.220 ns |   0.205 ns |       - |      - |     - |         - |
//| IndexList_384byte |     1 |    151.75 ns |   0.804 ns |   0.712 ns |       - |      - |     - |         - |
//|   IndexList_4byte |    10 |    108.85 ns |   0.532 ns |   0.498 ns |  0.0101 |      - |     - |      64 B |
//|  IndexList_32byte |    10 |    129.01 ns |   1.149 ns |   1.075 ns |  0.0548 |      - |     - |     344 B |
//| IndexList_128byte |    10 |    552.45 ns |   6.520 ns |   6.099 ns |  0.1669 |      - |     - |    1048 B |
//| IndexList_384byte |    10 |    922.30 ns |   5.239 ns |   4.644 ns |  0.3710 |      - |     - |    2328 B |
//|   IndexList_4byte |   100 |  1,076.44 ns |   9.982 ns |   9.337 ns |  0.1049 |      - |     - |     664 B |
//|  IndexList_32byte |   100 |  1,163.73 ns |   7.669 ns |   6.798 ns |  0.1984 |      - |     - |    1256 B |
//| IndexList_128byte |   100 |  5,303.43 ns |  42.895 ns |  40.124 ns |  1.7090 |      - |     - |   10768 B |
//| IndexList_384byte |   100 |  8,700.39 ns |  55.701 ns |  46.513 ns |  3.7537 |      - |     - |   23568 B |
//|   IndexList_4byte |  1000 | 10,130.44 ns |  74.327 ns |  62.067 ns |  0.9613 |      - |     - |    6040 B |
//|  IndexList_32byte |  1000 | 10,389.70 ns |  86.601 ns |  81.007 ns |  1.2665 |      - |     - |    7976 B |
//| IndexList_128byte |  1000 | 52,893.96 ns | 361.517 ns | 338.164 ns | 17.1509 |      - |     - |  107968 B |
//| IndexList_384byte |  1000 | 90,581.25 ns | 732.793 ns | 685.455 ns | 37.5977 | 0.2441 |     - |  235968 B |

//|                 Method | Count |          Mean |        Error |       StdDev |        Median |  Gen 0 | Gen 1 | Gen 2 | Allocated |
//|----------------------- |------ |--------------:|-------------:|-------------:|--------------:|-------:|------:|------:|----------:|
//|   IndexList_4byte_iter |     0 |      19.08 ns |     0.159 ns |     0.133 ns |      19.09 ns | 0.0115 |     - |     - |      72 B |
//|  IndexList_32byte_iter |     0 |      20.91 ns |     0.445 ns |     0.920 ns |      21.07 ns | 0.0115 |     - |     - |      72 B |
//| IndexList_128byte_iter |     0 |      23.87 ns |     0.506 ns |     0.846 ns |      23.48 ns | 0.0115 |     - |     - |      72 B |
//| IndexList_384byte_iter |     0 |      27.53 ns |     0.315 ns |     0.263 ns |      27.51 ns | 0.0115 |     - |     - |      72 B |
//|   IndexList_4byte_iter |     1 |      19.18 ns |     0.210 ns |     0.196 ns |      19.13 ns | 0.0115 |     - |     - |      72 B |
//|  IndexList_32byte_iter |     1 |     274.72 ns |     3.613 ns |     3.017 ns |     274.69 ns | 0.0114 |     - |     - |      72 B |
//| IndexList_128byte_iter |     1 |     291.33 ns |     2.006 ns |     1.566 ns |     291.00 ns | 0.0114 |     - |     - |      72 B |
//| IndexList_384byte_iter |     1 |     332.89 ns |     3.396 ns |     3.177 ns |     333.85 ns | 0.0114 |     - |     - |      72 B |
//|   IndexList_4byte_iter |    10 |      74.16 ns |     0.648 ns |     0.574 ns |      74.20 ns | 0.0114 |     - |     - |      72 B |
//|  IndexList_32byte_iter |    10 |   2,532.90 ns |    33.640 ns |    31.467 ns |   2,518.75 ns | 0.0114 |     - |     - |      72 B |
//| IndexList_128byte_iter |    10 |   2,742.00 ns |    21.923 ns |    19.434 ns |   2,732.40 ns | 0.0114 |     - |     - |      72 B |
//| IndexList_384byte_iter |    10 |   3,071.55 ns |    28.586 ns |    25.341 ns |   3,075.46 ns | 0.0114 |     - |     - |      72 B |
//|   IndexList_4byte_iter |   100 |     701.39 ns |     6.113 ns |     5.419 ns |     701.27 ns | 0.0114 |     - |     - |      72 B |
//|  IndexList_32byte_iter |   100 |  25,489.76 ns |   411.084 ns |   403.740 ns |  25,313.35 ns |      - |     - |     - |      72 B |
//| IndexList_128byte_iter |   100 |  27,488.42 ns |   222.877 ns |   208.479 ns |  27,466.42 ns |      - |     - |     - |      72 B |
//| IndexList_384byte_iter |   100 |  30,060.68 ns |    98.711 ns |    92.334 ns |  30,032.52 ns |      - |     - |     - |      72 B |
//|   IndexList_4byte_iter |  1000 |   7,903.95 ns |    64.442 ns |    57.126 ns |   7,895.62 ns |      - |     - |     - |      72 B |
//|  IndexList_32byte_iter |  1000 | 260,269.83 ns | 4,678.587 ns | 5,387.868 ns | 258,960.69 ns |      - |     - |     - |      72 B |
//| IndexList_128byte_iter |  1000 | 269,012.94 ns | 1,033.680 ns | 1,578.538 ns | 268,805.03 ns |      - |     - |     - |      72 B |
//| IndexList_384byte_iter |  1000 | 314,246.72 ns | 6,170.715 ns | 6,858.733 ns | 313,346.39 ns |      - |     - |     - |      72 B |

// using MapExt.iterV
//|                 Method | Count |         Mean |      Error |     StdDev |  Gen 0 | Gen 1 | Gen 2 | Allocated |
//|----------------------- |------ |-------------:|-----------:|-----------:|-------:|------:|------:|----------:|
//|   IndexList_4byte_iter |     0 |     11.39 ns |   0.255 ns |   0.238 ns | 0.0076 |     - |     - |      48 B |
//|  IndexList_32byte_iter |     0 |     11.44 ns |   0.195 ns |   0.191 ns | 0.0076 |     - |     - |      48 B |
//| IndexList_128byte_iter |     0 |     16.33 ns |   0.303 ns |   0.284 ns | 0.0076 |     - |     - |      48 B |
//| IndexList_384byte_iter |     0 |     22.97 ns |   0.484 ns |   0.898 ns | 0.0076 |     - |     - |      48 B |
//|   IndexList_4byte_iter |     1 |     11.53 ns |   0.141 ns |   0.132 ns | 0.0076 |     - |     - |      48 B |
//|  IndexList_32byte_iter |     1 |     11.97 ns |   0.268 ns |   0.251 ns | 0.0076 |     - |     - |      48 B |
//| IndexList_128byte_iter |     1 |     20.58 ns |   0.286 ns |   0.268 ns | 0.0076 |     - |     - |      48 B |
//| IndexList_384byte_iter |     1 |     35.62 ns |   0.583 ns |   0.546 ns | 0.0076 |     - |     - |      48 B |
//|   IndexList_4byte_iter |    10 |     69.62 ns |   0.341 ns |   0.284 ns | 0.0076 |     - |     - |      48 B |
//|  IndexList_32byte_iter |    10 |     67.70 ns |   1.009 ns |   0.944 ns | 0.0076 |     - |     - |      48 B |
//| IndexList_128byte_iter |    10 |    173.24 ns |   0.466 ns |   0.436 ns | 0.0076 |     - |     - |      48 B |
//| IndexList_384byte_iter |    10 |    314.95 ns |   0.355 ns |   0.297 ns | 0.0076 |     - |     - |      48 B |
//|   IndexList_4byte_iter |   100 |    674.58 ns |   8.352 ns |   7.813 ns | 0.0076 |     - |     - |      48 B |
//|  IndexList_32byte_iter |   100 |    668.89 ns |   4.098 ns |   3.833 ns | 0.0076 |     - |     - |      48 B |
//| IndexList_128byte_iter |   100 |  1,735.54 ns |   6.709 ns |   5.602 ns | 0.0076 |     - |     - |      48 B |
//| IndexList_384byte_iter |   100 |  2,971.03 ns |   6.529 ns |   6.107 ns | 0.0076 |     - |     - |      48 B |
//|   IndexList_4byte_iter |  1000 |  6,892.76 ns |  13.451 ns |  11.924 ns | 0.0076 |     - |     - |      48 B |
//|  IndexList_32byte_iter |  1000 |  7,106.19 ns |  32.649 ns |  28.942 ns | 0.0076 |     - |     - |      48 B |
//| IndexList_128byte_iter |  1000 | 18,113.95 ns | 147.287 ns | 130.566 ns |      - |     - |     - |      48 B |
//| IndexList_384byte_iter |  1000 | 33,745.08 ns | 167.130 ns | 156.333 ns |      - |     - |     - |      49 B |

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

    [<Benchmark>]
    member x.IndexList_4byte_iter() =
        let mutable sum = 0
        collection4 |> IndexList.iter (fun e -> sum <- sum + e)
        sum

    [<Benchmark>]
    member x.IndexList_32byte_iter() =
        let mutable sum = 0.0
        collection32 |> IndexList.iter (fun e -> sum <- sum + e.a)
        sum

    [<Benchmark>]
    member x.IndexList_128byte_iter() =
        let mutable sum = 0.0
        collection128 |> IndexList.iter (fun e -> sum <- sum + e.w.c)
        sum

    [<Benchmark>]
    member x.IndexList_384byte_iter() =
        let mutable sum = 0.0
        collection384 |> IndexList.iter (fun e -> sum <- sum + e.s.y.c)
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

//|               Method | Count |          Mean |        Error |       StdDev |        Median |    Gen 0 |   Gen 1 | Gen 2 | Allocated |
//|--------------------- |------ |--------------:|-------------:|-------------:|--------------:|---------:|--------:|------:|----------:|
//|   HashMap_4byte_iter |     0 |      12.27 ns |     0.271 ns |     0.459 ns |      12.29 ns |   0.0076 |       - |     - |      48 B |
//|  HashMap_32byte_iter |     0 |      11.56 ns |     0.141 ns |     0.110 ns |      11.58 ns |   0.0076 |       - |     - |      48 B |
//| HashMap_128byte_iter |     0 |      11.88 ns |     0.260 ns |     0.381 ns |      11.82 ns |   0.0076 |       - |     - |      48 B |
//| HashMap_384byte_iter |     0 |      10.95 ns |     0.081 ns |     0.068 ns |      10.96 ns |   0.0076 |       - |     - |      48 B |
//|   HashMap_4byte_iter |     1 |      12.17 ns |     0.253 ns |     0.248 ns |      12.15 ns |   0.0076 |       - |     - |      48 B |
//|  HashMap_32byte_iter |     1 |      12.21 ns |     0.244 ns |     0.216 ns |      12.22 ns |   0.0076 |       - |     - |      48 B |
//| HashMap_128byte_iter |     1 |      20.72 ns |     0.292 ns |     0.259 ns |      20.69 ns |   0.0076 |       - |     - |      48 B |
//| HashMap_384byte_iter |     1 |      35.93 ns |     0.444 ns |     0.415 ns |      35.92 ns |   0.0076 |       - |     - |      48 B |
//|   HashMap_4byte_iter |    10 |      40.44 ns |     0.428 ns |     0.401 ns |      40.45 ns |   0.0076 |       - |     - |      48 B |
//|  HashMap_32byte_iter |    10 |      50.93 ns |     0.590 ns |     0.523 ns |      50.97 ns |   0.0076 |       - |     - |      48 B |
//| HashMap_128byte_iter |    10 |     172.29 ns |     2.444 ns |     2.286 ns |     172.14 ns |   0.0076 |       - |     - |      48 B |
//| HashMap_384byte_iter |    10 |     342.73 ns |     2.641 ns |     2.341 ns |     341.73 ns |   0.0076 |       - |     - |      48 B |
//|   HashMap_4byte_iter |   100 |     470.73 ns |     6.855 ns |     5.724 ns |     471.11 ns |   0.0076 |       - |     - |      48 B |
//|  HashMap_32byte_iter |   100 |     534.75 ns |     4.938 ns |     4.377 ns |     535.34 ns |   0.0076 |       - |     - |      48 B |
//| HashMap_128byte_iter |   100 |   2,158.46 ns |     4.171 ns |     3.257 ns |   2,158.41 ns |   0.0076 |       - |     - |      48 B |
//| HashMap_384byte_iter |   100 |   3,826.45 ns |    16.730 ns |    13.970 ns |   3,823.40 ns |   0.0076 |       - |     - |      48 B |
//|   HashMap_4byte_iter |  1000 |   9,541.01 ns |   140.057 ns |   131.009 ns |   9,515.51 ns |        - |       - |     - |      48 B |
//|  HashMap_32byte_iter |  1000 |  11,570.42 ns |   111.070 ns |   103.895 ns |  11,557.67 ns |        - |       - |     - |      48 B |
//| HashMap_128byte_iter |  1000 |  26,462.19 ns |   215.937 ns |   191.423 ns |  26,524.16 ns |        - |       - |     - |      48 B |
//| HashMap_384byte_iter |  1000 |  58,190.76 ns |   842.497 ns |   703.523 ns |  58,124.75 ns |        - |       - |     - |      48 B |

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

    [<Benchmark>]
    member x.HashMap_4byte_iter() =
        let mutable sum = 0
        collection4 |> HashMap.iter (fun k v -> sum <- sum + k + v)
        sum

    [<Benchmark>]
    member x.HashMap_32byte_iter() =
        let mutable sum = 0.0
        collection32 |> HashMap.iter (fun k v -> sum <- sum + k.a + v.b)
        sum

    [<Benchmark>]
    member x.HashMap_128byte_iter() =
        let mutable sum = 0.0
        collection128 |> HashMap.iter (fun k v -> sum <- sum + k.x.b + v.y.c)
        sum

    [<Benchmark>]
    member x.HashMap_384byte_iter() =
        let mutable sum = 0.0
        collection384 |> HashMap.iter (fun k v -> sum <- sum + k.r.z.d + v.r.x.a)
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

//|               Method | Count |          Mean |        Error |       StdDev |        Median |    Gen 0 |   Gen 1 | Gen 2 | Allocated |
//|--------------------- |------ |--------------:|-------------:|-------------:|--------------:|---------:|--------:|------:|----------:|
//|   HashMap_4byte_iter |     0 |      12.27 ns |     0.271 ns |     0.459 ns |      12.29 ns |   0.0076 |       - |     - |      48 B |
//|  HashMap_32byte_iter |     0 |      11.56 ns |     0.141 ns |     0.110 ns |      11.58 ns |   0.0076 |       - |     - |      48 B |
//| HashMap_128byte_iter |     0 |      11.88 ns |     0.260 ns |     0.381 ns |      11.82 ns |   0.0076 |       - |     - |      48 B |
//| HashMap_384byte_iter |     0 |      10.95 ns |     0.081 ns |     0.068 ns |      10.96 ns |   0.0076 |       - |     - |      48 B |
//|   HashMap_4byte_iter |     1 |      12.17 ns |     0.253 ns |     0.248 ns |      12.15 ns |   0.0076 |       - |     - |      48 B |
//|  HashMap_32byte_iter |     1 |      12.21 ns |     0.244 ns |     0.216 ns |      12.22 ns |   0.0076 |       - |     - |      48 B |
//| HashMap_128byte_iter |     1 |      20.72 ns |     0.292 ns |     0.259 ns |      20.69 ns |   0.0076 |       - |     - |      48 B |
//| HashMap_384byte_iter |     1 |      35.93 ns |     0.444 ns |     0.415 ns |      35.92 ns |   0.0076 |       - |     - |      48 B |
//|   HashMap_4byte_iter |    10 |      40.44 ns |     0.428 ns |     0.401 ns |      40.45 ns |   0.0076 |       - |     - |      48 B |
//|  HashMap_32byte_iter |    10 |      50.93 ns |     0.590 ns |     0.523 ns |      50.97 ns |   0.0076 |       - |     - |      48 B |
//| HashMap_128byte_iter |    10 |     172.29 ns |     2.444 ns |     2.286 ns |     172.14 ns |   0.0076 |       - |     - |      48 B |
//| HashMap_384byte_iter |    10 |     342.73 ns |     2.641 ns |     2.341 ns |     341.73 ns |   0.0076 |       - |     - |      48 B |
//|   HashMap_4byte_iter |   100 |     470.73 ns |     6.855 ns |     5.724 ns |     471.11 ns |   0.0076 |       - |     - |      48 B |
//|  HashMap_32byte_iter |   100 |     534.75 ns |     4.938 ns |     4.377 ns |     535.34 ns |   0.0076 |       - |     - |      48 B |
//| HashMap_128byte_iter |   100 |   2,158.46 ns |     4.171 ns |     3.257 ns |   2,158.41 ns |   0.0076 |       - |     - |      48 B |
//| HashMap_384byte_iter |   100 |   3,826.45 ns |    16.730 ns |    13.970 ns |   3,823.40 ns |   0.0076 |       - |     - |      48 B |
//|   HashMap_4byte_iter |  1000 |   9,541.01 ns |   140.057 ns |   131.009 ns |   9,515.51 ns |        - |       - |     - |      48 B |
//|  HashMap_32byte_iter |  1000 |  11,570.42 ns |   111.070 ns |   103.895 ns |  11,557.67 ns |        - |       - |     - |      48 B |
//| HashMap_128byte_iter |  1000 |  26,462.19 ns |   215.937 ns |   191.423 ns |  26,524.16 ns |        - |       - |     - |      48 B |
//| HashMap_384byte_iter |  1000 |  58,190.76 ns |   842.497 ns |   703.523 ns |  58,124.75 ns |        - |       - |     - |      48 B |

[<PlainExporter; MemoryDiagnoser>]
type HashMapStructEnumeratorBenchmark() =

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

//BenchmarkDotNet=v0.12.1, OS=Windows 10.0.19042
//Intel Core i7-8700K CPU 3.70GHz (Coffee Lake), 1 CPU, 12 logical and 6 physical cores
//.NET Core SDK=5.0.101
//  [Host]     : .NET Core 3.1.10 (CoreCLR 4.700.20.51601, CoreFX 4.700.20.51901), X64 RyuJIT DEBUG
//  DefaultJob : .NET Core 3.1.10 (CoreCLR 4.700.20.51601, CoreFX 4.700.20.51901), X64 RyuJIT

// Array Buffer (with re-use) + Inline Stack Head
//|               Method | Count |         Mean |      Error |     StdDev |  Gen 0 | Gen 1 | Gen 2 | Allocated |
//|--------------------- |------ |-------------:|-----------:|-----------:|-------:|------:|------:|----------:|
//|   HashSetDelta_4byte |     0 |     19.56 ns |   0.035 ns |   0.029 ns |      - |     - |     - |         - |
//|  HashSetDelta_32byte |     0 |     19.70 ns |   0.045 ns |   0.040 ns |      - |     - |     - |         - |
//| HashSetDelta_128byte |     0 |     19.78 ns |   0.083 ns |   0.073 ns |      - |     - |     - |         - |
//| HashSetDelta_384byte |     0 |     19.73 ns |   0.069 ns |   0.061 ns |      - |     - |     - |         - |
//|   HashSetDelta_4byte |     1 |     32.34 ns |   0.217 ns |   0.203 ns | 0.0051 |     - |     - |      32 B |
//|  HashSetDelta_32byte |     1 |     34.22 ns |   0.448 ns |   0.419 ns | 0.0102 |     - |     - |      64 B |
//| HashSetDelta_128byte |     1 |     60.42 ns |   0.230 ns |   0.215 ns | 0.0254 |     - |     - |     160 B |
//| HashSetDelta_384byte |     1 |     93.81 ns |   0.866 ns |   0.768 ns | 0.0663 |     - |     - |     416 B |
//|   HashSetDelta_4byte |    10 |    102.11 ns |   0.431 ns |   0.360 ns | 0.0166 |     - |     - |     104 B |
//|  HashSetDelta_32byte |    10 |    124.27 ns |   0.471 ns |   0.441 ns | 0.0675 |     - |     - |     424 B |
//| HashSetDelta_128byte |    10 |    412.49 ns |   2.250 ns |   2.104 ns | 0.2203 |     - |     - |    1384 B |
//| HashSetDelta_384byte |    10 |    710.35 ns |   3.179 ns |   2.974 ns | 0.6285 |     - |     - |    3944 B |
//|   HashSetDelta_4byte |   100 |    917.26 ns |  15.746 ns |  14.729 ns | 0.0696 |     - |     - |     440 B |
//|  HashSetDelta_32byte |   100 |  1,029.33 ns |  15.315 ns |  13.576 ns | 0.1450 |     - |     - |     920 B |
//| HashSetDelta_128byte |   100 |  3,847.37 ns |  19.511 ns |  17.296 ns | 0.3891 |     - |     - |    2456 B |
//| HashSetDelta_384byte |   100 |  6,263.23 ns |  16.441 ns |  15.379 ns | 1.0376 |     - |     - |    6520 B |
//|   HashSetDelta_4byte |  1000 | 12,021.92 ns |  35.964 ns |  28.078 ns | 0.4578 |     - |     - |    2936 B |
//|  HashSetDelta_32byte |  1000 | 12,325.90 ns | 209.099 ns | 205.364 ns | 0.5493 |     - |     - |    3448 B |
//| HashSetDelta_128byte |  1000 | 39,528.23 ns | 262.127 ns | 232.369 ns | 0.7935 |     - |     - |    5080 B |
//| HashSetDelta_384byte |  1000 | 68,683.09 ns |  99.583 ns |  77.748 ns | 1.3428 |     - |     - |    9176 B |

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

//BenchmarkDotNet=v0.12.1, OS=Windows 10.0.19042
//Intel Core i7-8700K CPU 3.70GHz (Coffee Lake), 1 CPU, 12 logical and 6 physical cores
//.NET Core SDK=5.0.101
//  [Host]     : .NET Core 3.1.10 (CoreCLR 4.700.20.51601, CoreFX 4.700.20.51901), X64 RyuJIT DEBUG
//  DefaultJob : .NET Core 3.1.10 (CoreCLR 4.700.20.51601, CoreFX 4.700.20.51901), X64 RyuJIT

// using HashMapEnumerator with Array Re-Use + Inline Stack Head
//|               Method | Count |          Mean |      Error |     StdDev |    Gen 0 |   Gen 1 | Gen 2 | Allocated |
//|--------------------- |------ |--------------:|-----------:|-----------:|---------:|--------:|------:|----------:|
//|   HashMapDelta_4byte |     0 |      22.03 ns |   0.150 ns |   0.140 ns |        - |       - |     - |         - |
//|  HashMapDelta_32byte |     0 |      27.54 ns |   0.117 ns |   0.109 ns |        - |       - |     - |         - |
//| HashMapDelta_128byte |     0 |      34.26 ns |   0.056 ns |   0.050 ns |        - |       - |     - |         - |
//| HashMapDelta_384byte |     0 |      38.44 ns |   0.100 ns |   0.089 ns |        - |       - |     - |         - |
//|   HashMapDelta_4byte |     1 |      34.89 ns |   0.055 ns |   0.052 ns |   0.0102 |       - |     - |      64 B |
//|  HashMapDelta_32byte |     1 |      46.93 ns |   0.106 ns |   0.094 ns |   0.0191 |       - |     - |     120 B |
//| HashMapDelta_128byte |     1 |      91.65 ns |   0.383 ns |   0.359 ns |   0.0497 |       - |     - |     312 B |
//| HashMapDelta_384byte |     1 |     135.45 ns |   0.762 ns |   0.675 ns |   0.1311 |  0.0002 |     - |     824 B |
//|   HashMapDelta_4byte |    10 |     114.53 ns |   0.630 ns |   0.589 ns |   0.0675 |       - |     - |     424 B |
//|  HashMapDelta_32byte |    10 |     210.21 ns |   1.200 ns |   1.123 ns |   0.1566 |  0.0005 |     - |     984 B |
//| HashMapDelta_128byte |    10 |     621.10 ns |   1.354 ns |   1.131 ns |   0.4625 |  0.0048 |     - |    2904 B |
//| HashMapDelta_384byte |    10 |   1,079.82 ns |   3.336 ns |   2.957 ns |   1.2779 |  0.0362 |     - |    8024 B |
//|   HashMapDelta_4byte |   100 |   1,321.77 ns |   5.968 ns |   5.290 ns |   0.5741 |  0.0019 |     - |    3608 B |
//|  HashMapDelta_32byte |   100 |   2,354.86 ns |   6.418 ns |   5.359 ns |   1.4648 |  0.0114 |     - |    9208 B |
//| HashMapDelta_128byte |   100 |   6,550.54 ns |  35.941 ns |  30.012 ns |   4.5242 |  0.1221 |     - |   28408 B |
//| HashMapDelta_384byte |   100 |  11,375.15 ns |  19.324 ns |  18.076 ns |  12.6801 |  0.9460 |     - |   79608 B |
//|   HashMapDelta_4byte |  1000 |  15,710.54 ns |  51.273 ns |  45.452 ns |   5.5237 |       - |     - |   34808 B |
//|  HashMapDelta_32byte |  1000 |  27,304.23 ns |  85.427 ns |  79.908 ns |  14.4958 |  0.1526 |     - |   91096 B |
//| HashMapDelta_128byte |  1000 |  69,801.44 ns | 238.503 ns | 223.096 ns |  45.0439 |  1.3428 |     - |  282872 B |
//| HashMapDelta_384byte |  1000 | 126,103.01 ns | 593.341 ns | 555.012 ns | 126.7090 | 10.3760 |     - |  794936 B |

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

//BenchmarkDotNet=v0.12.1, OS=Windows 10.0.19042
//Intel Core i7-8700K CPU 3.70GHz (Coffee Lake), 1 CPU, 12 logical and 6 physical cores
//.NET Core SDK=5.0.101
//  [Host]     : .NET Core 3.1.10 (CoreCLR 4.700.20.51601, CoreFX 4.700.20.51901), X64 RyuJIT DEBUG
//  DefaultJob : .NET Core 3.1.10 (CoreCLR 4.700.20.51601, CoreFX 4.700.20.51901), X64 RyuJIT

//|              Method | Count |          Mean |        Error |       StdDev |    Gen 0 |    Gen 1 |    Gen 2 | Allocated |
//|-------------------- |------ |--------------:|-------------:|-------------:|---------:|---------:|---------:|----------:|
//|        MapExt_4byte |     0 |      23.51 ns |     0.223 ns |     0.198 ns |   0.0115 |        - |        - |      72 B |
//|       MapExt_32byte |     0 |      43.69 ns |     0.216 ns |     0.180 ns |   0.0114 |        - |        - |      72 B |
//|      MapExt_128byte |     0 |      50.82 ns |     0.575 ns |     0.538 ns |   0.0114 |        - |        - |      72 B |
//|      MapExt_384byte |     0 |      76.16 ns |     0.435 ns |     0.386 ns |   0.0114 |        - |        - |      72 B |
//|        MapExt_4byte |     1 |      30.25 ns |     0.200 ns |     0.187 ns |   0.0127 |        - |        - |      80 B |
//|       MapExt_32byte |     1 |      60.06 ns |     0.393 ns |     0.328 ns |   0.0216 |        - |        - |     136 B |
//|      MapExt_128byte |     1 |     128.62 ns |     0.578 ns |     0.482 ns |   0.0522 |        - |        - |     328 B |
//|      MapExt_384byte |     1 |     233.63 ns |     2.229 ns |     1.976 ns |   0.1335 |        - |        - |     840 B |
//|        MapExt_4byte |    10 |     162.87 ns |     0.530 ns |     0.496 ns |   0.0279 |        - |        - |     176 B |
//|       MapExt_32byte |    10 |     352.81 ns |     1.992 ns |     1.766 ns |   0.1173 |        - |        - |     736 B |
//|      MapExt_128byte |    10 |     997.32 ns |     4.649 ns |     4.349 ns |   0.4215 |        - |        - |    2656 B |
//|      MapExt_384byte |    10 |   1,916.92 ns |    28.313 ns |    25.099 ns |   1.2360 |        - |        - |    7776 B |
//|        MapExt_4byte |   100 |   1,745.67 ns |    23.166 ns |    21.670 ns |   0.2537 |        - |        - |    1592 B |
//|       MapExt_32byte |   100 |   3,193.77 ns |    46.820 ns |    65.636 ns |   1.1330 |   0.0267 |        - |    7112 B |
//|      MapExt_128byte |   100 |  10,333.66 ns |   161.492 ns |   151.060 ns |   4.1962 |        - |        - |   26432 B |
//|      MapExt_384byte |   100 |  19,101.21 ns |   240.584 ns |   225.043 ns |  12.3291 |        - |        - |   77712 B |
//|        MapExt_4byte |  1000 |  19,895.09 ns |   152.911 ns |   135.551 ns |   2.4414 |   0.0305 |        - |   15432 B |
//|       MapExt_32byte |  1000 |  34,439.82 ns |   502.863 ns |   470.378 ns |  11.2305 |   1.4648 |        - |   70832 B |
//|      MapExt_128byte |  1000 | 224,175.82 ns | 3,372.224 ns | 2,989.389 ns |  76.9043 |  76.9043 |  76.9043 |  263592 B |
//|      MapExt_384byte |  1000 | 533,279.51 ns | 9,258.838 ns | 8,660.723 ns | 199.2188 | 199.2188 | 199.2188 |  775713 B |

// HashMap style
//|         Method | Count |          Mean |      Error |     StdDev |   Gen 0 |  Gen 1 | Gen 2 | Allocated |
//|--------------- |------ |--------------:|-----------:|-----------:|--------:|-------:|------:|----------:|
//|   MapExt_4byte |     0 |      20.69 ns |   0.224 ns |   0.209 ns |       - |      - |     - |         - |
//|  MapExt_32byte |     0 |      27.06 ns |   0.107 ns |   0.100 ns |       - |      - |     - |         - |
//| MapExt_128byte |     0 |      32.53 ns |   0.104 ns |   0.087 ns |       - |      - |     - |         - |
//| MapExt_384byte |     0 |      41.38 ns |   0.111 ns |   0.109 ns |       - |      - |     - |         - |
//|   MapExt_4byte |     1 |      33.13 ns |   0.483 ns |   0.428 ns |  0.0051 |      - |     - |      32 B |
//|  MapExt_32byte |     1 |      47.33 ns |   0.205 ns |   0.171 ns |  0.0140 |      - |     - |      88 B |
//| MapExt_128byte |     1 |     117.25 ns |   1.844 ns |   1.635 ns |  0.0446 |      - |     - |     280 B |
//| MapExt_384byte |     1 |     197.87 ns |   0.457 ns |   0.405 ns |  0.1261 |      - |     - |     792 B |
//|   MapExt_4byte |    10 |     113.34 ns |   0.593 ns |   0.555 ns |  0.0166 |      - |     - |     104 B |
//|  MapExt_32byte |    10 |     227.40 ns |   0.882 ns |   0.825 ns |  0.1056 |      - |     - |     664 B |
//| MapExt_128byte |    10 |     857.97 ns |  10.821 ns |  10.122 ns |  0.4110 |      - |     - |    2584 B |
//| MapExt_384byte |    10 |   1,641.90 ns |   9.246 ns |   8.196 ns |  1.2264 |      - |     - |    7704 B |
//|   MapExt_4byte |   100 |   1,087.27 ns |   8.431 ns |   6.582 ns |  0.1221 |      - |     - |     768 B |
//|  MapExt_32byte |   100 |   1,754.92 ns |   2.264 ns |   2.007 ns |  0.3262 |      - |     - |    2056 B |
//| MapExt_128byte |   100 |   8,010.54 ns |  69.432 ns |  61.549 ns |  1.1292 |      - |     - |    7144 B |
//| MapExt_384byte |   100 |  15,737.02 ns |  57.085 ns |  50.604 ns |  3.1738 | 0.1221 |     - |   19944 B |
//|   MapExt_4byte |  1000 |  13,273.61 ns |  28.930 ns |  24.158 ns |  1.1749 |      - |     - |    7456 B |
//|  MapExt_32byte |  1000 |  20,953.47 ns |  75.376 ns |  66.819 ns |  2.0447 |      - |     - |   12856 B |
//| MapExt_128byte |  1000 |  82,573.80 ns | 149.581 ns | 124.907 ns |  5.2490 |      - |     - |   33016 B |
//| MapExt_384byte |  1000 | 161,273.27 ns | 632.929 ns | 494.149 ns | 13.6719 | 0.4883 |     - |   86088 B |

// New MapExt
//|              Method | Count |          Mean |      Error |     StdDev |  Gen 0 | Gen 1 | Gen 2 | Allocated |
//|-------------------- |------ |--------------:|-----------:|-----------:|-------:|------:|------:|----------:|
//|        MapExt_4byte |     0 |      20.25 ns |   0.110 ns |   0.103 ns |      - |     - |     - |         - |
//|       MapExt_32byte |     0 |      20.07 ns |   0.174 ns |   0.162 ns |      - |     - |     - |         - |
//|      MapExt_128byte |     0 |      24.16 ns |   0.245 ns |   0.217 ns |      - |     - |     - |         - |
//|      MapExt_384byte |     0 |      29.86 ns |   0.249 ns |   0.233 ns |      - |     - |     - |         - |
//|        MapExt_4byte |     1 |      39.74 ns |   0.114 ns |   0.095 ns | 0.0063 |     - |     - |      40 B |
//|       MapExt_32byte |     1 |      41.52 ns |   0.791 ns |   0.701 ns | 0.0063 |     - |     - |      40 B |
//|      MapExt_128byte |     1 |      84.02 ns |   0.720 ns |   0.638 ns | 0.0063 |     - |     - |      40 B |
//|      MapExt_384byte |     1 |     146.14 ns |   1.691 ns |   1.412 ns | 0.0062 |     - |     - |      40 B |
//|        MapExt_4byte |    10 |     267.56 ns |   1.492 ns |   1.246 ns | 0.0825 |     - |     - |     520 B |
//|       MapExt_32byte |    10 |     282.90 ns |   2.451 ns |   2.293 ns | 0.1016 |     - |     - |     640 B |
//|      MapExt_128byte |    10 |     718.53 ns |   2.900 ns |   2.264 ns | 0.0954 |     - |     - |     600 B |
//|      MapExt_384byte |    10 |   1,226.67 ns |  10.080 ns |   9.429 ns | 0.0954 |     - |     - |     600 B |
//|        MapExt_4byte |   100 |   2,684.88 ns |   7.500 ns |   6.648 ns | 0.9155 |     - |     - |    5760 B |
//|       MapExt_32byte |   100 |   2,782.52 ns |   8.826 ns |   8.256 ns | 0.9422 |     - |     - |    5920 B |
//|      MapExt_128byte |   100 |   7,138.39 ns |  16.557 ns |  13.825 ns | 0.9689 |     - |     - |    6080 B |
//|      MapExt_384byte |   100 |  12,250.25 ns |  36.062 ns |  33.733 ns | 0.9460 |     - |     - |    5960 B |
//|        MapExt_4byte |  1000 |  28,732.30 ns |  60.264 ns |  53.423 ns | 9.5825 |     - |     - |   60240 B |
//|       MapExt_32byte |  1000 |  29,268.32 ns | 106.015 ns |  99.166 ns | 9.5215 |     - |     - |   59920 B |
//|      MapExt_128byte |  1000 |  75,812.64 ns | 145.360 ns | 135.970 ns | 9.5215 |     - |     - |   60200 B |
//|      MapExt_384byte |  1000 | 147,433.92 ns | 429.604 ns | 380.833 ns | 9.5215 |     - |     - |   60200 B |

// New MapExt + Inline Stack Head
//|         Method | Count |          Mean |        Error |       StdDev |        Median |  Gen 0 | Gen 1 | Gen 2 | Allocated |
//|--------------- |------ |--------------:|-------------:|-------------:|--------------:|-------:|------:|------:|----------:|
//|   MapExt_4byte |     0 |      21.85 ns |     0.264 ns |     0.247 ns |      21.78 ns |      - |     - |     - |         - |
//|  MapExt_32byte |     0 |      25.96 ns |     0.201 ns |     0.188 ns |      25.95 ns |      - |     - |     - |         - |
//| MapExt_128byte |     0 |      34.19 ns |     0.696 ns |     1.124 ns |      33.51 ns |      - |     - |     - |         - |
//| MapExt_384byte |     0 |      63.06 ns |     0.272 ns |     0.254 ns |      63.00 ns |      - |     - |     - |         - |
//|   MapExt_4byte |     1 |      37.06 ns |     0.102 ns |     0.090 ns |      37.05 ns |      - |     - |     - |         - |
//|  MapExt_32byte |     1 |      40.00 ns |     0.516 ns |     0.482 ns |      40.07 ns |      - |     - |     - |         - |
//| MapExt_128byte |     1 |      93.80 ns |     0.527 ns |     0.493 ns |      93.64 ns |      - |     - |     - |         - |
//| MapExt_384byte |     1 |     153.73 ns |     0.978 ns |     0.915 ns |     153.70 ns |      - |     - |     - |         - |
//|   MapExt_4byte |    10 |     241.27 ns |     0.736 ns |     0.652 ns |     241.30 ns | 0.0381 |     - |     - |     240 B |
//|  MapExt_32byte |    10 |     283.38 ns |     2.567 ns |     2.401 ns |     283.81 ns | 0.0505 |     - |     - |     320 B |
//| MapExt_128byte |    10 |     726.01 ns |     7.546 ns |     6.689 ns |     723.14 ns | 0.0505 |     - |     - |     320 B |
//| MapExt_384byte |    10 |   1,315.53 ns |     9.653 ns |     9.030 ns |   1,314.18 ns | 0.0496 |     - |     - |     320 B |
//|   MapExt_4byte |   100 |   2,525.72 ns |     7.634 ns |     6.768 ns |   2,525.51 ns | 0.5074 |     - |     - |    3200 B |
//|  MapExt_32byte |   100 |   2,818.99 ns |     8.779 ns |     7.782 ns |   2,818.42 ns | 0.5646 |     - |     - |    3560 B |
//| MapExt_128byte |   100 |   7,348.50 ns |    17.240 ns |    14.396 ns |   7,344.23 ns | 0.5493 |     - |     - |    3480 B |
//| MapExt_384byte |   100 |  12,378.46 ns |    31.644 ns |    26.425 ns |  12,376.79 ns | 0.5493 |     - |     - |    3520 B |
//|   MapExt_4byte |  1000 |  27,756.36 ns |   116.382 ns |   108.864 ns |  27,720.52 ns | 5.6152 |     - |     - |   35280 B |
//|  MapExt_32byte |  1000 |  29,570.79 ns |   197.684 ns |   184.914 ns |  29,510.33 ns | 5.5542 |     - |     - |   35000 B |
//| MapExt_128byte |  1000 |  75,430.54 ns |   316.716 ns |   280.760 ns |  75,397.19 ns | 5.4932 |     - |     - |   35160 B |
//| MapExt_384byte |  1000 | 143,500.88 ns | 1,558.978 ns | 1,381.994 ns | 143,476.61 ns | 5.3711 |     - |     - |   35080 B |


// Old MapExt Iter
//|              Method | Count |          Mean |        Error |       StdDev |    Gen 0 |    Gen 1 |    Gen 2 | Allocated |
//|-------------------- |------ |--------------:|-------------:|-------------:|---------:|---------:|---------:|----------:|
//|   MapExt_4byte_iter |     0 |      13.06 ns |     0.173 ns |     0.153 ns |   0.0076 |        - |        - |      48 B |
//|  MapExt_32byte_iter |     0 |      19.91 ns |     0.187 ns |     0.175 ns |   0.0076 |        - |        - |      48 B |
//| MapExt_128byte_iter |     0 |      21.60 ns |     0.172 ns |     0.152 ns |   0.0076 |        - |        - |      48 B |
//| MapExt_384byte_iter |     0 |      23.62 ns |     0.175 ns |     0.164 ns |   0.0076 |        - |        - |      48 B |
//|   MapExt_4byte_iter |     1 |      13.86 ns |     0.191 ns |     0.179 ns |   0.0076 |        - |        - |      48 B |
//|  MapExt_32byte_iter |     1 |      20.91 ns |     0.110 ns |     0.098 ns |   0.0076 |        - |        - |      48 B |
//| MapExt_128byte_iter |     1 |      39.08 ns |     0.237 ns |     0.222 ns |   0.0076 |        - |        - |      48 B |
//| MapExt_384byte_iter |     1 |      72.12 ns |     0.273 ns |     0.256 ns |   0.0076 |        - |        - |      48 B |
//|   MapExt_4byte_iter |    10 |      74.14 ns |     0.411 ns |     0.364 ns |   0.0076 |        - |        - |      48 B |
//|  MapExt_32byte_iter |    10 |     136.49 ns |     1.274 ns |     1.191 ns |   0.0076 |        - |        - |      48 B |
//| MapExt_128byte_iter |    10 |     334.11 ns |     2.747 ns |     2.435 ns |   0.0076 |        - |        - |      48 B |
//| MapExt_384byte_iter |    10 |     633.21 ns |     3.563 ns |     3.333 ns |   0.0076 |        - |        - |      48 B |
//|   MapExt_4byte_iter |   100 |     645.07 ns |     6.064 ns |     5.673 ns |   0.0076 |        - |        - |      48 B |
//|  MapExt_32byte_iter |   100 |   1,008.03 ns |     9.431 ns |     8.360 ns |   0.0076 |        - |        - |      48 B |
//| MapExt_128byte_iter |   100 |   3,397.18 ns |    29.835 ns |    27.908 ns |   0.0076 |        - |        - |      48 B |
//| MapExt_384byte_iter |   100 |   6,513.71 ns |    52.599 ns |    49.201 ns |   0.0076 |        - |        - |      48 B |
//|   MapExt_4byte_iter |  1000 |   7,948.47 ns |   111.848 ns |   104.623 ns |        - |        - |        - |      48 B |
//|  MapExt_32byte_iter |  1000 |  12,814.60 ns |   104.300 ns |    97.562 ns |        - |        - |        - |      48 B |
//| MapExt_128byte_iter |  1000 |  36,012.77 ns |   345.215 ns |   306.024 ns |        - |        - |        - |      48 B |
//| MapExt_384byte_iter |  1000 |  77,497.61 ns |   835.327 ns |   740.496 ns |        - |        - |        - |      48 B |

// New MapExt Iter
//|              Method | Count |          Mean |      Error |     StdDev |  Gen 0 | Gen 1 | Gen 2 | Allocated |
//|-------------------- |------ |--------------:|-----------:|-----------:|-------:|------:|------:|----------:|
//|   MapExt_4byte_iter |     0 |      11.14 ns |   0.188 ns |   0.176 ns | 0.0076 |     - |     - |      48 B |
//|  MapExt_32byte_iter |     0 |      10.64 ns |   0.116 ns |   0.109 ns | 0.0076 |     - |     - |      48 B |
//| MapExt_128byte_iter |     0 |      10.69 ns |   0.043 ns |   0.040 ns | 0.0076 |     - |     - |      48 B |
//| MapExt_384byte_iter |     0 |      11.16 ns |   0.064 ns |   0.060 ns | 0.0076 |     - |     - |      48 B |
//|   MapExt_4byte_iter |     1 |      13.08 ns |   0.285 ns |   0.427 ns | 0.0076 |     - |     - |      48 B |
//|  MapExt_32byte_iter |     1 |      12.78 ns |   0.076 ns |   0.067 ns | 0.0076 |     - |     - |      48 B |
//| MapExt_128byte_iter |     1 |      22.39 ns |   0.205 ns |   0.192 ns | 0.0076 |     - |     - |      48 B |
//| MapExt_384byte_iter |     1 |      35.95 ns |   0.091 ns |   0.085 ns | 0.0076 |     - |     - |      48 B |
//|   MapExt_4byte_iter |    10 |      40.30 ns |   0.474 ns |   0.443 ns | 0.0076 |     - |     - |      48 B |
//|  MapExt_32byte_iter |    10 |      45.61 ns |   0.458 ns |   0.428 ns | 0.0076 |     - |     - |      48 B |
//| MapExt_128byte_iter |    10 |     135.86 ns |   0.550 ns |   0.514 ns | 0.0076 |     - |     - |      48 B |
//| MapExt_384byte_iter |    10 |     270.50 ns |   1.235 ns |   1.031 ns | 0.0076 |     - |     - |      48 B |
//|   MapExt_4byte_iter |   100 |     295.36 ns |   0.944 ns |   0.837 ns | 0.0076 |     - |     - |      48 B |
//|  MapExt_32byte_iter |   100 |     325.92 ns |   1.696 ns |   1.587 ns | 0.0076 |     - |     - |      48 B |
//| MapExt_128byte_iter |   100 |   1,451.89 ns |   3.873 ns |   3.623 ns | 0.0076 |     - |     - |      48 B |
//| MapExt_384byte_iter |   100 |   2,864.64 ns |  14.252 ns |  13.332 ns | 0.0076 |     - |     - |      48 B |
//|   MapExt_4byte_iter |  1000 |   3,264.44 ns |  38.527 ns |  34.154 ns | 0.0076 |     - |     - |      48 B |
//|  MapExt_32byte_iter |  1000 |   4,308.38 ns |  52.744 ns |  49.337 ns | 0.0076 |     - |     - |      48 B |
//| MapExt_128byte_iter |  1000 |  18,250.31 ns | 123.401 ns | 115.429 ns |      - |     - |     - |      48 B |
//| MapExt_384byte_iter |  1000 |  44,354.94 ns | 421.347 ns | 394.128 ns |      - |     - |     - |      48 B |

[<PlainExporter; MemoryDiagnoser>]
type MapExtEnumeratorBenchmark() =

    let mutable collection4 = MapExt.empty<int,int>
    let mutable collection32 = MapExt.empty<Struct32,Struct32>
    let mutable collection128 = MapExt.empty<Struct128,Struct128>
    let mutable collection384 = MapExt.empty<Struct384,Struct384>
   
    [<Params(0, 1, 10, 100, 1000); DefaultValue>]
    val mutable public Count : int

    [<GlobalSetup>]
    member x.Setup() =
        let rand = Random(123)
        collection4 <- MapExt.ofArray (Array.init x.Count (fun i -> (rand.Next(), rand.Next()) ))
        collection32 <- MapExt.ofArray (Array.init x.Count (fun i -> (BigStruct.create32(rand.Next()), BigStruct.create32(rand.Next())) ))
        collection128 <- MapExt.ofArray (Array.init x.Count (fun i -> (BigStruct.createBig(rand.Next()), BigStruct.createBig(rand.Next())) ))
        collection384 <- MapExt.ofArray (Array.init x.Count (fun i -> (BigStruct.createBigger(rand.Next()), BigStruct.createBigger(rand.Next()))  ))

    [<Benchmark>]
    member x.MapExt_4byte() =
        let mutable sum = 0
        for KeyValue(k,v) in collection4 do sum <- sum + k + v
        sum

    [<Benchmark>]
    member x.MapExt_32byte() =
        let mutable sum = 0.0
        for KeyValue(k,v) in collection32 do sum <- sum + k.a + v.b
        sum

    [<Benchmark>]
    member x.MapExt_128byte() =
        let mutable sum = 0.0
        for KeyValue(k,v) in collection128 do sum <- sum + k.x.b + v.y.c
        sum

    [<Benchmark>]
    member x.MapExt_384byte() =
        let mutable sum = 0.0
        for KeyValue(k,v) in collection384 do sum <- sum + k.r.z.d + v.r.x.a
        sum

    [<Benchmark>]
    member x.MapExt_4byte_iter() =
        let mutable sum = 0
        collection4 |> MapExt.iter (fun k v -> sum <- sum + k + v)
        sum

    [<Benchmark>]
    member x.MapExt_32byte_iter() =
        let mutable sum = 0.0
        collection32 |> MapExt.iter (fun k v -> sum <- sum + k.a + v.b)
        sum

    [<Benchmark>]
    member x.MapExt_128byte_iter() =
        let mutable sum = 0.0
        collection128 |> MapExt.iter (fun k v -> sum <- sum + k.x.b + v.y.c)
        sum

    [<Benchmark>]
    member x.MapExt_384byte_iter() =
        let mutable sum = 0.0
        collection384 |> MapExt.iter (fun k v -> sum <- sum + k.r.z.d + v.r.x.a)
        sum