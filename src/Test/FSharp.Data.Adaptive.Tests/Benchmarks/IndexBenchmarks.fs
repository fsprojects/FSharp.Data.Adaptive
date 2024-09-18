namespace Benchmarks

open BenchmarkDotNet.Attributes
open FSharp.Data.Adaptive

//BenchmarkDotNet v0.14.0, Windows 10 (10.0.19045.4894/22H2/2022Update)
//Intel Core i7-8700K CPU 3.70GHz (Coffee Lake), 1 CPU, 12 logical and 6 physical cores
//.NET SDK 8.0.400
//  [Host]     : .NET 8.0.8 (8.0.824.36612), X64 RyuJIT AVX2 DEBUG
//  DefaultJob : .NET 8.0.8 (8.0.824.36612), X64 RyuJIT AVX2

//| Method        | CreateCount | Mean     | Error   | StdDev  | Allocated |
//|-------------- |------------ |---------:|--------:|--------:|----------:|
//| HashSetAdd    | 10000       | 221.6 μs | 2.46 μs | 2.05 μs |         - |
//| DictionaryAdd | 10000       | 230.2 μs | 0.59 μs | 0.55 μs |         - |

// with Index IEquatable
//| Method        | CreateCount | Mean     | Error   | StdDev  | Allocated |
//|-------------- |------------ |---------:|--------:|--------:|----------:|
//| HashSetAdd    | 10000       | 204.5 us | 0.37 us | 0.34 us |         - |
//| DictionaryAdd | 10000       | 215.2 us | 2.46 us | 2.18 us |         - |

// + inlined IndexNode Equals
//| Method        | CreateCount | Mean     | Error   | StdDev  | Allocated |
//|-------------- |------------ |---------:|--------:|--------:|----------:|
//| HashSetAdd    | 10000       | 199.1 us | 3.85 us | 4.12 us |         - |
//| DictionaryAdd | 10000       | 205.4 us | 1.43 us | 1.33 us |         - |

[<MemoryDiagnoser>]
type IndexEqualsBenchmarks() =

    [<Params(10000); DefaultValue>]
    val mutable public Count : int

    let mutable indexSet = Unchecked.defaultof<_>
    let mutable indexDict = Unchecked.defaultof<_>
    let mutable indices = Array.empty

    [<GlobalSetup>]
    member x.Setup() =
        indexSet <- System.Collections.Generic.HashSet<Index>(x.Count)
        indexDict <- System.Collections.Generic.Dictionary<Index, int>(x.Count)
        indices <- Array.zeroCreate x.Count
        let mutable h = Index.zero
        for i in 0..x.Count-1 do
            h <- Index.after h
            indices.[i] <- h

    [<Benchmark>]
    member x.HashSetAdd() =
        for i in indices do
            indexSet.Add(i) |> ignore

    [<Benchmark>]
    member x.DictionaryAdd() =
        let mutable ii = 0
        for i in indices do
            indexDict.[i] <- ii
            ii <- ii + 1

// NOTE: wait benchmarked with GC.Collect + GC.WaitForPendingFinalizers + loop checking delete queue/alive count via exposed counters
// Reference: overhead 536 bytes/item (~CPU 30%)
//| Method            | GarbageCount | ListCount | Mean     | Error    | StdDev   | Gen0        | Gen1        | Gen2        | Allocated |
//|------------------ |------------- |---------- |---------:|---------:|---------:|------------:|------------:|------------:|----------:|
//| GarbageRnd (wait) | 1000000      | 100       | 629.6 ms | 12.59 ms | 25.71 ms | 290000.0000 | 205000.0000 | 154000.0000 | 826.82 MB |
//| GarbageRnd        | 1000000      | 100       | 539.2 ms |  7.52 ms |  7.03 ms | 138000.0000 |  70000.0000 |   1000.0000 | 822.73 MB |
//| GarbageRnd        | 5000000      | 100       | 2.592 s  | 0.0370 s | 0.0346 s | 695000.0000 | 358000.0000 |   2000.0000 |   4.02 GB |
//| Garbage0          | 5000000      | 100       | 2.315 s  | 0.0305 s | 0.0255 s | 574000.0000 | 280000.0000 |           - |   3.47 GB |

// BlockingCollection: overhead: 183 bytes/item (~CPU 27%)
//| Method            | GarbageCount | ListCount | Mean     | Error    | StdDev   | Gen0        | Gen1        | Gen2        | Allocated |
//|------------------ |------------- |---------- |---------:|---------:|---------:|------------:|------------:|------------:|----------:|
//| GarbageRnd (wait) | 1000000      | 100       | 624.4 ms | 12.36 ms | 28.15 ms | 285000.0000 | 270000.0000 | 207000.0000 | 490.43 MB |
//| GarbageRnd        | 1000000      | 100       | 520.2 ms |  9.25 ms |  8.20 ms |  82000.0000 |  81000.0000 |           - | 488.39 MB |

// BlockingCollection + background thread delete: overhead 0 bytes (~CPU 13%)
//| Method            | GarbageCount | ListCount | Mean     | Error   | StdDev  | Gen0       | Gen1       | Gen2       | Allocated |
//|-----------------  |------------- |---------- |---------:|--------:|--------:|-----------:|-----------:|-----------:|----------:|
//| GarbageRnd (wait) | 1000000      | 100       | 535.8 ms | 2.34 ms | 1.83 ms | 57000.0000 | 56000.0000 | 11000.0000 | 312.96 MB |
//| GarbageRnd        | 1000000      | 100       | 521.8 ms | 9.87 ms | 8.75 ms | 53000.0000 | 52000.0000 |          - | 318.79 MB |

// finalizer Delete (infinite loop fastest): overhead 0 bytes (~CPU 9%)
//| Method            | GarbageCount | ListCount | Mean     | Error    | StdDev   | Gen0        | Gen1        | Gen2      | Allocated  |
//|------------------ |------------- |---------- |---------:|---------:|---------:|------------:|------------:|----------:|-----------:|
//| GarbageRnd (wait) | 1000000      | 100       | 507.7 ms | 2.46 ms  | 2.05 ms  |  53000.0000 |  52000.0000 | 4000.0000 |  315.57 MB |
//| GarbageRnd        | 1000000      | 100       | 471.0 ms | 3.16 ms  | 2.96 ms  |  52000.0000 |  51000.0000 |         - |  315.67 MB |
//| GarbageRnd        | 5000000      | 100       | 2.338 s  | 0.0159 s | 0.0133 s | 262000.0000 | 261000.0000 |         - | 1572.10 MB |
//| Garbage0          | 5000000      | 100       | 1.526 s  | 0.0058 s | 0.0045 s | 169000.0000 | 168000.0000 |         - | 1017.00 MB |


//| Method   | GarbageCount | ListCount | Mean    | Error    | StdDev   | Gen0        | Gen1        | Allocated  |
//|--------- |------------- |---------- |--------:|---------:|---------:|------------:|------------:|-----------:|
//| Garbage0 | 5000000      | 100       | 1.595 s | 0.0173 s | 0.0162 s | 170000.0000 | 169000.0000 | 1017.29 MB |

// explicit RefCount tracking + avoid double locking
//| Method   | GarbageCount | ListCount | Mean    | Error    | StdDev   | Gen0        | Gen1        | Allocated  |
//|--------- |------------- |---------- |--------:|---------:|---------:|------------:|------------:|-----------:|
//| Garbage0 | 5000000      | 100       | 1.531 s | 0.0040 s | 0.0035 s | 169000.0000 | 168000.0000 | 1016.93 MB |

// + inlined node equality (and other small changes, however those do not have any significant effect)
//| Method   | GarbageCount | ListCount | Mean    | Error    | StdDev   | Gen0        | Gen1        | Allocated  |
//|--------- |------------- |---------- |--------:|---------:|---------:|------------:|------------:|-----------:|
//| Garbage0 | 5000000      | 100       | 1.479 s | 0.0052 s | 0.0044 s | 170000.0000 | 169000.0000 | 1017.98 MB |

[<MemoryDiagnoser>]
[<InProcess>]
type IndexGarbageBenchmarks() =

    [<Params(5000000); DefaultValue>]
    val mutable public GarbageCount : int

    [<Params(100); DefaultValue>]
    val mutable public ListCount : int

    let mutable list = IndexList.empty
    let rnd = System.Random(12345678)
    
    [<Benchmark>]
    member x.GarbageRnd() =
        for i in 1..x.GarbageCount do
            let op = rnd.NextDouble()
            if list.Count = 0 || op < 0.01 then
                list <- IndexList.single i
            elif list.Count < x.ListCount && op < 0.5 then // add
                list <- list.InsertAt(rnd.Next(list.Count), i)
            else // remove
                list <- list.RemoveAt(rnd.Next(list.Count))

        list <- IndexList.empty

    [<Benchmark>]
    member x.Garbage0() =
        for i in 1..x.GarbageCount do
            let op = rnd.NextDouble()
            if list.Count = 0 || op < 0.01 then
                list <- IndexList.single i
            elif list.Count < x.ListCount && op < 0.5 then // add
                list <- list.Prepend i
            else // remove
                list <- list.RemoveAt(0)

        list <- IndexList.empty

