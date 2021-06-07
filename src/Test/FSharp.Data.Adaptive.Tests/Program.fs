module Program

open BenchmarkDotNet.Running
open FSharp.Data.Traceable
open FSharp.Data.Adaptive
open System
open MBrace.FsPickler
open MBrace.FsPickler.Json
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open System.Runtime.CompilerServices

#nowarn "9"

module TLSHashTest =

    type SomeModel =
        {
            id : Guid
            name : string
            elements : list<string>
            entries : list<option<int>>
        }

    let run() =
        let item1 =
            {
                id = Guid.Parse "3e87677b-49fe-4f3d-b9a8-5734ce0cb378"
                name = "hans"
                elements = ["a"; "sepp"; "stonelicker"]
                entries = [Some 1; None; Some 412]
            }
        
        let item2 =
            {
                id = Guid.Parse "847f2bbe-b101-4919-b1d7-7e91f7a2e347"
                name = "Josef Maier"
                elements = ["franz"; "sepp"]
                entries = []
            }
        
        let item3 =
            {
                id = Guid.Parse "fef703c2-fda4-4024-9da6-a2e437744896"
                name = "Christina Oberleitmeier"
                elements = ["nix"]
                entries = [None; None; Some 123]
            }




        let h1 = item1 |> TLSHash.Compute
        let h2 = item2 |> TLSHash.Compute
        let h3 = item3 |> TLSHash.Compute
        let h1' = { item1 with entries = None :: item1.entries } |> TLSHash.Compute

        TLSHash.Distance(h1, h1) |> printfn "1/1:  %A"
        TLSHash.Distance(h2, h2) |> printfn "2/2:  %A"
        TLSHash.Distance(h3, h3) |> printfn "3/3:  %A"
        printfn ""
        TLSHash.Distance(h1, h2) |> printfn "1/2:  %A"
        TLSHash.Distance(h2, h3) |> printfn "2/3:  %A"
        TLSHash.Distance(h1, h3) |> printfn "1/3:  %A"
        printfn ""
    
        TLSHash.Distance(h1', h1) |> printfn "1'/1: %A"
        TLSHash.Distance(h1', h2) |> printfn "1'/2: %A"
        TLSHash.Distance(h1', h3) |> printfn "1'/3: %A"
    
        // prints:
        // 1/1:  0
        // 2/2:  0
        // 3/3:  0
        // 
        // 1/2:  216
        // 2/3:  212
        // 1/3:  247
        // 
        // 1'/1: 21
        // 1'/2: 217
        // 1'/3: 241
    

[<EntryPoint>]
let main _args =

    TLSHashTest.run()
    
    exit 0


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
