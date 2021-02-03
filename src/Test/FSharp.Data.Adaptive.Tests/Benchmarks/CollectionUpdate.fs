namespace Benchmarks

open FSharp.Data.Adaptive
open BenchmarkDotNet.Attributes
open System

//BenchmarkDotNet=v0.12.1, OS=Windows 10.0.19042
//Intel Core i7-8700K CPU 3.70GHz (Coffee Lake), 1 CPU, 12 logical and 6 physical cores
//.NET Core SDK=5.0.101
//  [Host]     : .NET Core 3.1.10 (CoreCLR 4.700.20.51601, CoreFX 4.700.20.51901), X64 RyuJIT DEBUG
//  Job-IIBYMR : .NET Core 3.1.10 (CoreCLR 4.700.20.51601, CoreFX 4.700.20.51901), X64 RyuJIT

//InvocationCount=1  UnrollFactor=1

//|             Method |  Count |           Mean |        Error |       StdDev |       Gen 0 |      Gen 1 |     Gen 2 |    Allocated |
//|------------------- |------- |---------------:|-------------:|-------------:|------------:|-----------:|----------:|-------------:|
//|  CSet_Map_GetValue |      0 |     1,214.4 us |     15.98 us |     14.95 us |           - |          - |         - |    752.27 KB |
//| CList_Map_GetValue |      0 |       987.1 us |     15.85 us |     14.82 us |           - |          - |         - |    519.59 KB |
//|  CSet_Map_GetValue |   1000 |     3,875.4 us |     64.44 us |     92.42 us |   1000.0000 |          - |         - |    8208.7 KB |
//| CList_Map_GetValue |   1000 |     1,360.2 us |     17.92 us |     15.89 us |           - |          - |         - |    768.28 KB |
//|  CSet_Map_GetValue |  10000 |    43,902.5 us |    865.72 us |    850.26 us |  11000.0000 |  1000.0000 |         - |  72241.13 KB |
//| CList_Map_GetValue |  10000 |     1,142.0 us |     22.71 us |     32.58 us |           - |          - |         - |    925.53 KB |
//|  CSet_Map_GetValue | 100000 | 1,572,069.7 us | 30,765.45 us | 40,003.77 us | 123000.0000 | 50000.0000 | 7000.0000 | 711116.59 KB |
//| CList_Map_GetValue | 100000 |     1,304.1 us |     19.63 us |     16.40 us |           - |          - |         - |    1091.7 KB |

[<PlainExporter; MemoryDiagnoser>]
type CollectionUpdate() =

    let mutable set = cset []
    let mutable list1 = clist []
    let mutable list2 = clist []

    let mutable mapSet : aset<int> = Unchecked.defaultof<_>
    let mutable mapList : alist<int> = Unchecked.defaultof<_>

    [<Params(0, 1000, 10000, 100000); DefaultValue>]
    val mutable public Count : int

    [<IterationSetup>]
    member x.Setup() =
        let rand = Random(123)
        let data = Array.init x.Count (fun i -> 1000 + rand.Next(99999999) ) // -> no hashset add conflicst below 1000
        set <- cset data
        list1 <- clist data
        list2 <- clist data

        mapSet <- set |> ASet.map (fun i -> i * 2)
        mapSet |> ASet.force |> ignore
        mapList <- list1 |> AList.map (fun i -> i * 2)
        mapList |> AList.force |> ignore

    //[<Benchmark>]
    //member x.CSet_Add() =
    //    for i in 0..100 do
    //        set.Add(i) |> ignore

    //[<Benchmark>]
    //member x.CSet_Add_GetValue() =
    //    for i in 0..100 do
    //        transact (fun () -> 
    //            set.Add(i) |> ignore
    //            set.Value |> ignore
    //            )

    [<Benchmark>]
    member x.CSet_Map_GetValue() =
        for i in 0..100 do
            transact (fun () -> 
                set.Add(i) |> ignore
                )
            mapSet |> ASet.force |> ignore

    //[<Benchmark>]
    //member x.CList_Append_GetValue() =
    //    for i in 0..100 do
    //        list1.Append(i) |> ignore
    //        list1.Value |> ignore

    [<Benchmark>]
    member x.CList_Map_GetValue() =
        for i in 0..100 do
            transact (fun () -> 
                list1.Append(i) |> ignore
                )
            mapList |> AList.force |> ignore