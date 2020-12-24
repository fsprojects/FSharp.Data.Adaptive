namespace Benchmarks

open FSharp.Data.Adaptive

open BenchmarkDotNet.Attributes

//BenchmarkDotNet=v0.12.1, OS=Windows 10.0.19042
//Intel Core i7-8700K CPU 3.70GHz (Coffee Lake), 1 CPU, 12 logical and 6 physical cores
//.NET Core SDK=5.0.101
//  [Host]     : .NET Core 3.1.10 (CoreCLR 4.700.20.51601, CoreFX 4.700.20.51901), X64 RyuJIT DEBUG
//  DefaultJob : .NET Core 3.1.10 (CoreCLR 4.700.20.51601, CoreFX 4.700.20.51901), X64 RyuJIT

//|         Method |       Mean |   Error |  StdDev |  Gen 0 | Gen 1 | Gen 2 | Allocated |
//|--------------- |-----------:|--------:|--------:|-------:|------:|------:|----------:|
//|   CValGetValue |   261.7 ns | 2.42 ns | 2.26 ns |      - |     - |     - |         - |
//| AValueGetValue |   262.2 ns | 1.59 ns | 1.33 ns |      - |     - |     - |         - |
//|     CValUpdate | 1,155.7 ns | 8.26 ns | 7.32 ns | 0.1259 |     - |     - |     792 B |

// without CancellationToken
//|         Method |      Mean |    Error |   StdDev |  Gen 0 | Gen 1 | Gen 2 | Allocated |
//|--------------- |----------:|---------:|---------:|-------:|------:|------:|----------:|
//|   CValGetValue |  26.70 ns | 0.139 ns | 0.130 ns |      - |     - |     - |         - |
//| AValueGetValue |  28.01 ns | 0.187 ns | 0.175 ns |      - |     - |     - |         - |
//|     CValUpdate | 904.03 ns | 8.161 ns | 7.234 ns | 0.1259 |     - |     - |     792 B |

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
