﻿namespace Benchmarks

open BenchmarkDotNet.Attributes
open FSharp.Data.Adaptive
open System
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Jobs

//BenchmarkDotNet v0.14.0, Windows 10 (10.0.19045.4780/22H2/2022Update)
//Intel Core i7-8700K CPU 3.70GHz (Coffee Lake), 1 CPU, 12 logical and 6 physical cores
//.NET SDK 8.0.400
//  [Host]     : .NET 8.0.8 (8.0.824.36612), X64 RyuJIT AVX2 DEBUG
//  DefaultJob : .NET 8.0.8 (8.0.824.36612), X64 RyuJIT AVX2

//| Method         | Categories      | Count | Mean            | Error         | StdDev       | Ratio | Gen0    | Allocated | Alloc Ratio |
//|--------------- |---------------- |------ |----------------:|--------------:|-------------:|------:|--------:|----------:|------------:|
//| TryGet_32Val   | (1) 4 byte val  | 1     |        48.25 ns |      0.096 ns |     0.080 ns |  1.00 |  0.0038 |      24 B |        1.00 |
//| TryGetV_32Val  | (1) 4 byte val  | 1     |        50.65 ns |      0.151 ns |     0.134 ns |  1.05 |       - |         - |        0.00 |
//|                |                 |       |                 |               |              |       |         |           |             |
//| TryGet_32Val   | (1) 4 byte val  | 10    |       354.84 ns |      0.928 ns |     0.868 ns |  1.00 |  0.0114 |      72 B |        1.00 |
//| TryGetV_32Val  | (1) 4 byte val  | 10    |       365.27 ns |      1.098 ns |     1.027 ns |  1.03 |       - |         - |        0.00 |
//|                |                 |       |                 |               |              |       |         |           |             |
//| TryGet_32Val   | (1) 4 byte val  | 500   |    50,223.13 ns |    167.372 ns |   148.371 ns |  1.00 |  0.5493 |    3552 B |        1.00 |
//| TryGetV_32Val  | (1) 4 byte val  | 500   |    51,237.16 ns |    158.610 ns |   140.603 ns |  1.02 |       - |         - |        0.00 |
//|                |                 |       |                 |               |              |       |         |           |             |
//| TryGet_32Val   | (1) 4 byte val  | 10000 | 1,735,799.18 ns |  5,738.214 ns | 5,367.529 ns |  1.00 | 11.7188 |   78625 B |       1.000 |
//| TryGetV_32Val  | (1) 4 byte val  | 10000 | 1,755,901.65 ns | 10,525.554 ns | 9,845.610 ns |  1.01 |       - |       1 B |       0.000 |
//|                |                 |       |                 |               |              |       |         |           |             |
//| TryGet_64Val   | (2) 8 byte val  | 1     |        47.93 ns |      0.125 ns |     0.117 ns |  1.00 |  0.0038 |      24 B |        1.00 |
//| TryGetV_64Val  | (2) 8 byte val  | 1     |        47.40 ns |      0.258 ns |     0.216 ns |  0.99 |       - |         - |        0.00 |
//|                |                 |       |                 |               |              |       |         |           |             |
//| TryGet_64Val   | (2) 8 byte val  | 10    |       361.18 ns |      1.473 ns |     1.306 ns |  1.00 |  0.0114 |      72 B |        1.00 |
//| TryGetV_64Val  | (2) 8 byte val  | 10    |       353.09 ns |      1.231 ns |     1.092 ns |  0.98 |       - |         - |        0.00 |
//|                |                 |       |                 |               |              |       |         |           |             |
//| TryGet_64Val   | (2) 8 byte val  | 500   |    50,480.46 ns |    138.964 ns |   123.188 ns |  1.00 |  0.5493 |    3552 B |        1.00 |
//| TryGetV_64Val  | (2) 8 byte val  | 500   |    50,533.77 ns |    142.511 ns |   126.332 ns |  1.00 |       - |         - |        0.00 |
//|                |                 |       |                 |               |              |       |         |           |             |
//| TryGet_64Val   | (2) 8 byte val  | 10000 | 1,737,443.82 ns |  7,825.304 ns | 7,319.794 ns |  1.00 | 11.7188 |   78625 B |       1.000 |
//| TryGetV_64Val  | (2) 8 byte val  | 10000 | 1,737,081.43 ns |  6,858.967 ns | 6,415.882 ns |  1.00 |       - |       1 B |       0.000 |
//|                |                 |       |                 |               |              |       |         |           |             |
//| TryGet_128Val  | (3) 16 byte val | 1     |        48.46 ns |      0.191 ns |     0.179 ns |  1.00 |  0.0051 |      32 B |        1.00 |
//| TryGetV_128Val | (3) 16 byte val | 1     |        47.94 ns |      0.291 ns |     0.273 ns |  0.99 |       - |         - |        0.00 |
//|                |                 |       |                 |               |              |       |         |           |             |
//| TryGet_128Val  | (3) 16 byte val | 10    |       353.66 ns |      1.426 ns |     1.264 ns |  1.00 |  0.0153 |      96 B |        1.00 |
//| TryGetV_128Val | (3) 16 byte val | 10    |       351.66 ns |      0.971 ns |     0.909 ns |  0.99 |       - |         - |        0.00 |
//|                |                 |       |                 |               |              |       |         |           |             |
//| TryGet_128Val  | (3) 16 byte val | 500   |    51,943.24 ns |    181.379 ns |   160.788 ns |  1.00 |  0.7324 |    4736 B |        1.00 |
//| TryGetV_128Val | (3) 16 byte val | 500   |    50,406.11 ns |    167.637 ns |   148.606 ns |  0.97 |       - |         - |        0.00 |
//|                |                 |       |                 |               |              |       |         |           |             |
//| TryGet_128Val  | (3) 16 byte val | 10000 | 1,737,977.43 ns |  7,508.261 ns | 6,269.738 ns |  1.00 | 15.6250 |  104833 B |       1.000 |
//| TryGetV_128Val | (3) 16 byte val | 10000 | 1,737,448.34 ns |  2,927.987 ns | 2,285.979 ns |  1.00 |       - |       1 B |       0.000 |
//|                |                 |       |                 |               |              |       |         |           |             |
//| TryGet_Ref     | (4) ref         | 1     |        49.51 ns |      0.131 ns |     0.109 ns |  1.00 |  0.0038 |      24 B |        1.00 |
//| TryGetV_Ref    | (4) ref         | 1     |        59.33 ns |      0.221 ns |     0.184 ns |  1.20 |       - |         - |        0.00 |
//|                |                 |       |                 |               |              |       |         |           |             |
//| TryGet_Ref     | (4) ref         | 10    |       356.60 ns |      1.158 ns |     1.083 ns |  1.00 |  0.0114 |      72 B |        1.00 |
//| TryGetV_Ref    | (4) ref         | 10    |       388.15 ns |      0.983 ns |     0.920 ns |  1.09 |       - |         - |        0.00 |
//|                |                 |       |                 |               |              |       |         |           |             |
//| TryGet_Ref     | (4) ref         | 500   |    50,594.42 ns |    134.428 ns |   125.744 ns |  1.00 |  0.5493 |    3552 B |        1.00 |
//| TryGetV_Ref    | (4) ref         | 500   |    52,137.39 ns |    180.493 ns |   160.002 ns |  1.03 |       - |         - |        0.00 |
//|                |                 |       |                 |               |              |       |         |           |             |
//| TryGet_Ref     | (4) ref         | 10000 | 1,738,875.00 ns |  5,090.775 ns | 4,761.914 ns |  1.00 | 11.7188 |   78625 B |       1.000 |
//| TryGetV_Ref    | (4) ref         | 10000 | 1,777,254.84 ns |  5,872.989 ns | 5,493.597 ns |  1.02 |       - |       1 B |       0.000 |

[<MemoryDiagnoser>]
[<CategoriesColumn; GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByCategory)>]
type IndexListBenchmarks() =

    [<Params(1, 10, 500, 10000); DefaultValue>]
    val mutable public Count : int

    let mutable indices32 = Array.zeroCreate 0
    let mutable ilist32 = IndexList.empty<_>

    let mutable indices64 = Array.zeroCreate 0
    let mutable ilist64 = IndexList.empty<_>

    let mutable indices128 = Array.zeroCreate 0
    let mutable ilist128 = IndexList.empty<_>

    let mutable indicesRef = Array.zeroCreate 0
    let mutable ilistRef = IndexList.empty<_>

    [<GlobalSetup>]
    member x.Setup() =
        let rnd = Random(101)
        let stuff = Array.init x.Count (fun i -> i)
        ilist32 <- IndexList.ofArray stuff
        let mutable some = stuff |> Array.where (fun _ -> rnd.NextDouble() < 0.33)
        if some.Length = 0 then some <- [| Array.head stuff |]
        indices32 <- some |> Array.map (fun o -> IndexList.findIndex o ilist32)

        let rnd = Random(101)
        let stuff = Array.init x.Count (fun i -> struct(i, i*i))
        ilist64 <- IndexList.ofArray stuff
        let mutable some = stuff |> Array.where (fun _ -> rnd.NextDouble() < 0.33)
        if some.Length = 0 then some <- [| Array.head stuff |]
        indices64 <- some |> Array.map (fun o -> IndexList.findIndex o ilist64)

        let rnd = Random(101)
        let stuff = Array.init x.Count (fun i -> struct(int64 i, int64 (i * i)))
        ilist128 <- IndexList.ofArray stuff
        let mutable some = stuff |> Array.where (fun _ -> rnd.NextDouble() < 0.33)
        if some.Length = 0 then some <- [| Array.head stuff |]
        indices128 <- some |> Array.map (fun o -> IndexList.findIndex o ilist128)

        let rnd = Random(101)
        let stuff = Array.init x.Count (fun i -> Object())
        ilistRef <- IndexList.ofArray stuff
        let mutable some = stuff |> Array.where (fun _ -> rnd.NextDouble() < 0.33)
        if some.Length = 0 then some <- [| Array.head stuff |]
        indicesRef <- some |> Array.map (fun o -> IndexList.findIndex o ilistRef)


    [<Benchmark(Baseline=true); BenchmarkCategory("(1) 4 byte val")>]
    member x.TryGet_32Val() =
        let mutable res = 0
        for i in indices32 do
            match IndexList.tryGet i ilist32 with
            | Some l -> res <- res ^^^ l
            | None -> ()
        res

    [<Benchmark; BenchmarkCategory("(1) 4 byte val")>]
    member x.TryGetV_32Val() =
        let mutable res = 0
        for i in indices32 do
            match IndexList.tryGetV i ilist32 with
            | ValueSome l -> res <- res ^^^ l
            | ValueNone -> ()
        res

    [<Benchmark(Baseline=true); BenchmarkCategory("(2) 8 byte val")>]
    member x.TryGet_64Val() =
        let mutable res = 0
        for i in indices64 do
            match IndexList.tryGet i ilist64 with
            | Some (l,l2) -> res <- (res + l) ^^^ l2
            | None -> ()
        res

    [<Benchmark; BenchmarkCategory("(2) 8 byte val")>]
    member x.TryGetV_64Val() =
        let mutable res = 0
        for i in indices64 do
            match IndexList.tryGetV i ilist64 with
            | ValueSome (l,l2) -> res <- (res + l) ^^^ l2
            | ValueNone -> ()
        res

    [<Benchmark(Baseline=true); BenchmarkCategory("(3) 16 byte val")>]
    member x.TryGet_128Val() =
        let mutable res = 0L
        for i in indices128 do
            match IndexList.tryGet i ilist128 with
            | Some (l,l2) -> res <- (res + l) ^^^ l2
            | None -> ()
        res

    [<Benchmark; BenchmarkCategory("(3) 16 byte val")>]
    member x.TryGetV_128Val() =
        let mutable res = 0L
        for i in indices128 do
            match IndexList.tryGetV i ilist128 with
            | ValueSome (l,l2) -> res <- (res + l) ^^^ l2
            | ValueNone -> ()
        res

    [<Benchmark(Baseline=true); BenchmarkCategory("(4) ref")>]
    member x.TryGet_Ref() =
        let mutable res = 0
        for i in indicesRef do
            match IndexList.tryGet i ilistRef with
            | Some o -> res <- res ^^^ o.GetHashCode()
            | None -> ()
        res

    [<Benchmark; BenchmarkCategory("(4) ref")>]
    member x.TryGetV_Ref() =
        let mutable res = 0
        for i in indicesRef do
            match IndexList.tryGetV i ilistRef with
            | ValueSome o -> res <- res ^^^ o.GetHashCode()
            | ValueNone -> ()
        res


