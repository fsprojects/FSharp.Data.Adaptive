namespace Benchmarks

open FSharp.Data.Adaptive
open System
open BenchmarkDotNet.Attributes

//BenchmarkDotNet=v0.12.0, OS=Windows 10.0.19041
//Intel Core i7-8700K CPU 3.70GHz (Coffee Lake), 1 CPU, 12 logical and 6 physical cores
//.NET Core SDK=5.0.100
//  [Host]     : .NET Core 3.1.9 (CoreCLR 4.700.20.47201, CoreFX 4.700.20.47203), X64 RyuJIT DEBUG
//  DefaultJob : .NET Core 3.1.9 (CoreCLR 4.700.20.47201, CoreFX 4.700.20.47203), X64 RyuJIT

//|                     Method |         Mean |      Error |     StdDev |       Median |    Gen 0 | Gen 1 | Gen 2 | Allocated |
//|--------------------------- |-------------:|-----------:|-----------:|-------------:|---------:|------:|------:|----------:|
//|            ReferenceEquals |     11.03 us |   0.404 us |   1.190 us |     10.71 us |        - |     - |     - |         - |
//|              HashSetEquals |    788.45 us |  27.317 us |  80.545 us |    800.21 us | 305.6641 |     - |     - | 1920192 B |
//|              Shadow_Equals |  1,123.63 us |  21.497 us |  20.108 us |  1,124.30 us | 406.2500 |     - |     - | 2560256 B |
//|       Shadow_ShallowEquals | 12,480.33 us | 322.406 us | 950.622 us | 12,251.09 us |        - |     - |     - |         - |
//|        HashSetEquals (NEW) |     23.27 us |   0.451 us |   0.501 us |            - |        - |     - |     - |         - |
//| HashSetEqualsVirtual (NEW) |     71.35 us |   1.367 us |   1.279 us |            - |        - |     - |     - |         - |
//|        Shadow_Equals (NEW) |    264.24 us |   5.235 us |  12.742 us |            - | 101.5625 |     - |     - |  640064 B |
//| Shadow_ShallowEquals (NEW) | 12,115.89 us | 270.650 us | 798.016 us |            - |        - |     - |     - |         - |

[<PlainExporter; MemoryDiagnoser>]
type EqualsBenchmark() =
    let empty1 = HashSet<int>.Empty
    let empty2 = HashSet<int>.Empty
    let obj1 = Object()
    let obj2 = Object()
    let empty1Obj = HashSet<int>.Empty :> Object
    let empty2Obj = HashSet<int>.Empty :> Object

    [<Benchmark>]
    member x.ReferenceEquals() =
        let mutable res = 0
        for i in 0..10000 do
            if Object.ReferenceEquals(obj1, obj2) then
                res <- res + 1
        res

    [<Benchmark>]
    member x.HashSetEquals() =
        let mutable res = 0
        for i in 0..10000 do
            if empty1.Equals(empty2) then
                res <- res + 1
        res

    [<Benchmark>]
    member x.HashSetEqualsVirtual() =
        let mutable res = 0
        for i in 0..10000 do
            if empty1Obj.Equals(empty2Obj) then
                res <- res + 1
        res

    [<Benchmark>]
    member x.Shadow_Equals() =
        let mutable res = 0
        for i in 0..10000 do
            if ShallowEqualityComparer<HashSet<int>>.Equals(empty1, empty2) then
                res <- res + 1
        res

    [<Benchmark>]
    member x.Shadow_ShallowEquals() =
        let mutable res = 0
        for i in 0..10000 do
            if ShallowEqualityComparer<HashSet<int>>.ShallowEquals(empty1, empty2) then
                res <- res + 1
        res