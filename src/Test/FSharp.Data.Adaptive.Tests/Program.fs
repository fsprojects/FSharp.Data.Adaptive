module Program

open BenchmarkDotNet.Running

module Profile =
    open FSharp.Data.Adaptive
    let inputDiamond = AVal.custom (fun _ -> 0)
    let diamondSizes =
        [1;3;9;21;45;93;189;381;765;1533;3069;6141;12285; 24573;49149;98301;196605;393213;786429;1572861; 3145725;6291453;12582909;25165821;50331645; 100663293;201326589;402653181;805306365; 1610612733]


    let rec buildFan (inner : ref<int>) (n : int) =
        if n <= 0 then
            [inputDiamond]
        else
            buildFan inner (n - 1)
            |> List.collect (fun v ->
                inner := !inner + 2
                [AVal.map id v; AVal.map id v]
            )

    let rec reduce (inner : ref<int>) (l : list<aval<_>>) =
        match l with
        | [] -> inputDiamond
        | [v] -> v
        | l -> l |> List.chunkBySize 2 |> List.map (function [l;r] -> inner := !inner + 1; AVal.map2 (+) l r | _ -> failwith "bad") |> reduce inner

    let rec buildDiamond (depth : int) =
        let inner = ref 0
        let res = buildFan inner depth |> reduce inner
        printfn "DEPTH: %d / %d" depth !inner
        res


    let run() = 
        let d = buildDiamond 20
        for i in 1 .. 4 do
            AVal.force d |> ignore
            transact (fun () -> inputDiamond.MarkOutdated())
            
        AVal.force d |> ignore

        printfn "ready"
        System.Console.ReadLine() |> ignore
        transact (fun () -> inputDiamond.MarkOutdated())
        
        printfn "done"
        System.Console.ReadLine() |> ignore






[<EntryPoint>]
let main _args =
    //Profile.run()
    //BenchmarkRunner.Run<Benchmarks.TransactBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.MapBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.CollectBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.EnumeratorBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.IndexListEnumeratorBenchmark>() |> ignore
    BenchmarkRunner.Run<Benchmarks.HashSetEnumeratorBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.HashMapEnumeratorBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.CountingHashSetEnumeratorBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.IndexListDeltaEnumeratorBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.HashSetDeltaEnumeratorBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.HashMapDeltaEnumeratorBenchmark>() |> ignore

    0
