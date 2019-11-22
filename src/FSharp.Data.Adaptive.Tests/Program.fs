module Program

open BenchmarkDotNet.Running

[<EntryPoint>]
let main _args =
    BenchmarkRunner.Run<Benchmarks.MapBenchmark>() |> ignore
    BenchmarkRunner.Run<Benchmarks.CollectBenchmark>() |> ignore
    0
