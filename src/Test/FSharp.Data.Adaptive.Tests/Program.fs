module Program

open BenchmarkDotNet.Running
open FSharp.Data.Traceable
open FSharp.Data.Adaptive


open NUnit.Framework
open FsCheck
open FSharp.Data.Adaptive
open FSharp.Data.Traceable
open FsUnit
open FsCheck.NUnit

[<Test;Ignore("unclear currently")>]
let ``[AList] sub``() =

    let l = clist [0 .. 10]

    let subbed = 
        l
        |> AList.indexed
        |> AList.sortBy snd
        |> AList.sub 0 3

    let thrd = subbed |> AList.force |> IndexList.toArray
    subbed |> AList.force |> printfn "%A"

    transact (fun _ -> 
        l.[fst thrd.[2]] <- 4
    )
    subbed |> AList.force |> Seq.toArray |> Array.map snd |> should equal [0;1;4]


    transact (fun _ -> 
        l.[fst thrd.[2]] <- 5
    )
    subbed |> AList.force |> should equal [1;2;5]


    transact (fun _ -> 
        l.[fst thrd.[2]] <- 6
    )
    subbed |> AList.force |> should equal [1;2;6]

[<EntryPoint>]
let main _args =

    //``[AList] sub``();


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
