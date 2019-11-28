namespace Benchmarks

open FSharp.Data.Adaptive

open BenchmarkDotNet.Attributes

[<PlainExporter; MemoryDiagnoser>]
type TransactBenchmark() =
    let inputLinear = AVal.custom (fun _ -> 0)
    let inputDiamond = AVal.custom (fun _ -> 0)
    let inputWall = AVal.custom (fun _ -> 0)
    let mutable linear = inputLinear :> aval<_>
    let mutable diamond = inputDiamond :> aval<_>
    let mutable wall = inputWall

    let rec buildLinear (n : int) =
        let mutable c = inputLinear
        let mutable n = n
        while n > 0 do
            c <- AVal.map id c
            n <- n - 1
        c


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

    let rec buildWall (size : int) =
        if size > 1 then
            let d = List.init (size - 1) (fun _ -> AVal.map id inputWall)
            AVal.custom (fun t ->
                d |> List.iter (fun d -> d.GetValue t |> ignore)
                1
            )
        else
            AVal.map id inputWall

    [<DefaultValue; Params(1, 3, 9, 21, 189, 765, 1533)>]
    val mutable public Size : int

    [<GlobalSetup>]
    member x.Setup() =
        linear <- buildLinear x.Size
        wall <- buildWall x.Size
        if x.Size <= 1 then
            diamond <- inputLinear
        else
            let (d, s) = diamondSizes |> List.indexed |> List.minBy (fun (_,s) -> abs (s - x.Size))
            diamond <- buildDiamond d



    [<IterationSetup>]
    member x.Eval() =
        AVal.force linear |> ignore
        AVal.force diamond |> ignore
        AVal.force wall |> ignore

    [<Benchmark>]
    member x.Linear() =
        transact (fun () -> inputLinear.MarkOutdated())

    [<Benchmark>]
    member x.Diamond() =
        transact (fun () -> inputDiamond.MarkOutdated())
        
    [<Benchmark>]
    member x.Wall() =
        transact (fun () -> inputWall.MarkOutdated())


