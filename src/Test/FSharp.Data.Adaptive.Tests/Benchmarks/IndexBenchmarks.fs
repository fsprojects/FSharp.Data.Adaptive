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
