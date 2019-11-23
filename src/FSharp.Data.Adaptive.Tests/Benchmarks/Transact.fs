namespace Benchmarks

open FSharp.Data.Adaptive

open BenchmarkDotNet.Attributes

[<PlainExporter; MemoryDiagnoser>]
type TransactBenchmark() =
    let input = AVal.custom (fun _ -> 0)
    let mutable dependent = input :> aval<_>

    let rec buildTree (n : int) =
        let mutable c = input
        let mutable n = n
        while n > 0 do
            c <- AVal.map id c
            n <- n - 1
        c

    [<DefaultValue; Params(0, 1, 5, 10, 100, 1000, 2000)>]
    val mutable public Size : int

    [<GlobalSetup>]
    member x.Setup() =
        dependent <- buildTree x.Size


    [<IterationSetup>]
    member x.Eval() =
        AVal.force dependent |> ignore

    [<Benchmark>]
    member x.Transact() =
        transact (fun () -> input.MarkOutdated())


