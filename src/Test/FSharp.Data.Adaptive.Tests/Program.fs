module Program

open BenchmarkDotNet.Running
open FSharp.Data.Traceable
open FSharp.Data.Adaptive

[<EntryPoint>]
let main _args =
    
    let a = HAMTSet.ofList [1;2;3;4;5;6;7]
    let b = HAMTSet.ofList [2;4;6;8;10]
    let c = a |> HAMTSet.add 10
    printfn "a:              %0A" (HAMTSet.toList a)
    printfn "b:              %0A" (HAMTSet.toList b)
    printfn "c:              %0A" (HAMTSet.toList c)
    printfn "union a b:      %0A" (HAMTSet.toList (HAMTSet.union a b))
    printfn "intersect a b:  %0A" (HAMTSet.toList (HAMTSet.intersect a b))
    printfn "xor a b:        %0A" (HAMTSet.toList (HAMTSet.xor a b))
    printfn "difference a b: %0A" (HAMTSet.toList (HAMTSet.difference a b))
    printfn "difference b a: %0A" (HAMTSet.toList (HAMTSet.difference b a))

    printfn "a = a:          %0A" (a = a) 
    printfn "b = a+(b-a):    %0A" (b = fst (HAMTSet.applyDelta a (HAMTSet.computeDelta a b))) 
    printfn "a = b:          %0A" (a = b) 
    printfn "h a = h a:      %0A" (Unchecked.hash a = Unchecked.hash a)
    printfn "h a = h b:      %0A" (Unchecked.hash a = Unchecked.hash b)

    printfn "subset a a:     %0A" (a.IsSubsetOf a)
    printfn "subset' a a:    %0A" (a.IsProperSubsetOf a)
    printfn "subset a c:     %0A" (a.IsSubsetOf c)
    printfn "subset' a c:    %0A" (a.IsProperSubsetOf c)
    printfn "subset a b:     %0A" (a.IsSubsetOf b)

    exit 0
    let a = HAMT.ofList (List.init 10 (fun i -> i, string i))
    let b = HAMT.ofList (List.init 6 (fun i -> 2*i, string (2*i)))


    let ops = a.ComputeDeltaTo b
    printfn "%0A" (HAMT.toList a)
    printfn "%0A" (HAMT.toList b)
    printfn "%0A" (HAMT.toList ops)

    let tb, d = a.ApplyDelta(ops)
    printfn "%0A" (HAMT.toList tb)
    printfn "%0A" (HAMT.toList d)


    exit 0

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

    0
