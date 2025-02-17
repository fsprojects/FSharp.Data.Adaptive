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

let ``[AList] sub``() =

    let l = clist [0 .. 10]

    let subbed = 
        l
        //|> AList.indexed
        |> AList.sort
        |> AList.sub 0 3

    let r = subbed.GetReader()
    transact (fun _ -> 
        l.[2] <- 100
    )
    r.GetChanges AdaptiveToken.Top |> ignore
    r.State |> IndexList.toList |> should equal [0;1;3]


    transact (fun _ -> 
        l.[3] <- 200
    )
    r.GetChanges AdaptiveToken.Top |> ignore
    r.State |> IndexList.toList |> should equal [0;1;4]



    transact (fun _ -> 
        l.RemoveAt 1 |> ignore
    )
    r.GetChanges AdaptiveToken.Top |> ignore
    r.State |> IndexList.toList |> should equal [0;4;5]


[<EntryPoint>]
let main _args =

    //ASet.``[ASet] mapA/flattenA/chooseA async``()
    //ASet.``[ASet] union constant``()
    //AList.``[AList] toAset``()

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
    //BenchmarkRunner.Run<Benchmarks.HashSetDeltaBench>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.IndexListBenchmarks>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.IndexEqualsBenchmarks>() |> ignore
    BenchmarkRunner.Run<Benchmarks.IndexGarbageBenchmarks>() |> ignore

    //let x = Benchmarks.IndexGarbageBenchmarks()
    //x.ListCount <- 100
    //x.GarbageCount <- 1000000
    //for i in 0..1000 do
    //    x.GarbageRnd()
    //    printfn "iter %d" i

    //printfn "done"

    0
