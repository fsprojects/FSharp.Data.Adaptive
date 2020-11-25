namespace Benchmarks

open FSharp.Data.Adaptive

open BenchmarkDotNet.Attributes

//BenchmarkDotNet=v0.12.0, OS=Windows 10.0.19041
//Intel Core i7-8700K CPU 3.70GHz (Coffee Lake), 1 CPU, 12 logical and 6 physical cores
//.NET Core SDK=5.0.100
//  [Host]     : .NET Core 3.1.9 (CoreCLR 4.700.20.47201, CoreFX 4.700.20.47203), X64 RyuJIT DEBUG
//  DefaultJob : .NET Core 3.1.9 (CoreCLR 4.700.20.47201, CoreFX 4.700.20.47203), X64 RyuJIT

//|                    Method |       Mean |     Error |    StdDev |  Gen 0 | Gen 1 | Gen 2 | Allocated |
//|-------------------------- |-----------:|----------:|----------:|-------:|------:|------:|----------:|
//|   CValGetValue (proposed) |   3.888 ns | 0.0520 ns | 0.0461 ns |      - |     - |     - |         - |
//|    CValGetValue (current) |  27.910 ns | 0.3370 ns | 0.2990 ns |      - |     - |     - |         - |
//| AValueGetValue (proposed) |   4.541 ns | 0.0727 ns | 0.0607 ns |      - |     - |     - |         - |
//|  AValueGetValue (current) |  29.520 ns | 0.3100 ns | 0.2750 ns |      - |     - |     - |         - |
//|     CValUpdate (proposed) | 991.300 ns | 7.7600 ns | 7.2600 ns | 0.1297 |     - |     - |     816 B |
//|      CValUpdate (current) | 987.500 ns | 7.8300 ns | 6.9400 ns | 0.1259 |     - |     - |     792 B |

[<PlainExporter; MemoryDiagnoser>]
type GetValueBenchmark() =
    let cval = ChangeableValue(10)
    let mapVal = cval |> AVal.map (fun x -> x * 2) |> AVal.map (fun x -> x * 3) |> AVal.map (fun x -> x * 4)
        
    [<Benchmark>]
    member x.CValGetValue() =
        cval |> AVal.force

    [<Benchmark>]
    member x.AValueGetValue() =
        mapVal |> AVal.force

    [<Benchmark>]
    member x.CValUpdate() =
        transact (fun () -> cval.Value <- cval.Value + 1)
        mapVal |> AVal.force
