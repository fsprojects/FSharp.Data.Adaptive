module Program

open BenchmarkDotNet.Running
open FSharp.Data.Traceable
open FSharp.Data.Adaptive

[<EntryPoint>]
let main _args =

    //let a = cval [1;2;3;4]
    //let b = AVal.cast<seq<int>> a

    //AVal.force b |> printfn "%0A"

    
    //let c = AVal.cast<float> a
    //AVal.force c |> printfn "%0A"


    //exit 0

    //let b = HAMTBench()


    //while true do
    //    b.HAMT() |> ignore


    //Profile.run()
    //BenchmarkRunner.Run<Benchmarks.TransactBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.MapBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.CollectBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.EnumeratorBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.IndexListEnumeratorBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.HashSetEnumeratorBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.HashSetBenchmarks>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.HashMapEnumeratorBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.HashMapStructEnumeratorBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.CountingHashSetEnumeratorBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.IndexListDeltaEnumeratorBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.HashSetDeltaEnumeratorBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.HashMapDeltaEnumeratorBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.MapExtEnumeratorBenchmark>() |> ignore

    BenchmarkRunner.Run<Benchmarks.HashSetDeltaBench>() |> ignore

    0
