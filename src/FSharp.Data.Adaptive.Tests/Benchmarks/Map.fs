namespace Benchmarks

open FSharp.Data.Adaptive

open BenchmarkDotNet.Attributes

[<PlainExporter; MemoryDiagnoser>]
type MapBenchmark() =
    let mutable content : list<int> = Unchecked.defaultof<_>
    let mutable ilist : clist<int> = Unchecked.defaultof<_>
    let mutable olist : alist<int> = Unchecked.defaultof<_>
    let mutable list : list<int> = Unchecked.defaultof<_>
    let mutable sleeper = Unchecked.defaultof<_>

    let mutable change = []

    [<DefaultValue; Params(0.0001, 0.0005, 0.001)>]
    val mutable public SleepTime : float
    
    [<DefaultValue; Params(5000)>]
    val mutable public Size : int
    
    [<DefaultValue; Params(10, 100, 500, 1000, 2500, 5000)>]
    val mutable public ChangeSize : int

    member x.sleep (n : int) =
        let mutable sum = 0
        for i in 1 .. n do
            sum <- sum + i

    [<GlobalSetup>]
    member x.Setup() =
        sleeper <- Sleeper.Create x.SleepTime
        content <- List.init x.Size id 
        list <- content
        ilist <- clist list
        olist <- ilist |> AList.map (fun v -> sleeper.run(); v)
        AList.force olist |> ignore

        change <- List.init x.ChangeSize id
        
    [<IterationSetup>]
    member x.IterSetup() =  
        transact (fun () -> ilist.Value <- IndexList.ofList content)
        list <- content
        AList.force olist |> ignore
        transact (fun () -> 
            for c in change do
                ilist.Add c |> ignore
        )
        list <- list @ change

    [<Benchmark>]
    member x.AList() =
        AList.force olist 
        
    [<Benchmark>]
    member x.List() =
        list |> List.map (fun v -> sleeper.run(); v)

