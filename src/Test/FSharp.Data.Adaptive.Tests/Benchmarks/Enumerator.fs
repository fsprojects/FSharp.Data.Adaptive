namespace Benchmarks

open FSharp.Data.Adaptive

open BenchmarkDotNet.Attributes
open System.Runtime.CompilerServices

[<Struct>]
type RangeEnumerator =
    struct
        val mutable private arr : int[] 
        val mutable private current : int 

        member x.MoveNext() = 
            x.current <- x.current + 1
            x.current < x.arr.Length

        
        member x.Current 
            with get() = x.arr.[x.current]

            
        member x.Dispose() = ()
        interface System.Collections.IEnumerator with
            member x.MoveNext() = 
                x.current <- x.current + 1
                x.current < x.arr.Length
            member x.Reset() =
                x.current <- -1
            member x.Current = x.arr.[x.current] :> obj

        interface System.Collections.Generic.IEnumerator<int> with
            member x.Current = x.arr.[x.current]
            member x.Dispose() = ()
    
        new(arr) = { arr = arr; current = -1 }
    end

type RangeEnumerable(arr : int[]) =
    
    member x.GetEnumerator() = new RangeEnumerator(arr)

    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = new RangeEnumerator(arr) :> _
        
    interface System.Collections.Generic.IEnumerable<int> with
        member x.GetEnumerator() = new RangeEnumerator(arr) :> _

(*
BenchmarkDotNet=v0.12.0, OS=Windows 10.0.19041
Intel Core i7-4790K CPU 4.00GHz (Haswell), 1 CPU, 8 logical and 4 physical cores
.NET Core SDK=3.1.402
  [Host]     : .NET Core 3.0.0 (CoreCLR 4.700.19.46205, CoreFX 4.700.19.46214), X64 RyuJIT DEBUG
  DefaultJob : .NET Core 3.0.0 (CoreCLR 4.700.19.46205, CoreFX 4.700.19.46214), X64 RyuJIT


|                     Method |       Mean |     Error |     StdDev |  Gen 0 | Gen 1 | Gen 2 | Allocated |
|--------------------------- |-----------:|----------:|-----------:|-------:|------:|------:|----------:|
|                   ArraySum |   5.668 ns | 0.1407 ns |  0.2573 ns |      - |     - |     - |         - |
|                    ListSum |   6.191 ns | 0.1310 ns |  0.1161 ns |      - |     - |     - |         - |
|            RangeEnumerator |   6.240 ns | 0.1279 ns |  0.1196 ns |      - |     - |     - |         - |
|      RangeEnumeratorStatic |   5.895 ns | 0.1466 ns |  0.1630 ns |      - |     - |     - |         - |
|             ListEnumerator |   4.986 ns | 0.1285 ns |  0.1962 ns |      - |     - |     - |         - |
|          ListSeqEnumerator |  53.358 ns | 1.0722 ns |  1.1917 ns | 0.0076 |     - |     - |      32 B |
|  IndexListEnumerator (old) | 447.900 ns |  10.73 ns | 11.4800 ns | 0.1779 |     - |     - |     744 B |
|  IndexListEnumerator (new) | 242.655 ns | 4.8702 ns |  4.7832 ns | 0.1626 |     - |     - |     680 B |
|              IndexListFold | 409.289 ns | 8.1908 ns | 12.0060 ns | 0.0114 |     - |     - |      48 B |
|              IndexListIter |  92.135 ns | 0.7728 ns |  0.6453 ns | 0.0172 |     - |     - |      72 B |
|                 MapExtIter |  87.903 ns | 2.7986 ns |  8.0298 ns | 0.0114 |     - |     - |      48 B |
|                 MapExtFold |  72.377 ns | 1.3931 ns |  1.2350 ns | 0.0057 |     - |     - |      24 B |
| IndexListToArrayEnumerator |  87.279 ns | 2.6711 ns |  7.7067 ns | 0.0210 |     - |     - |      88 B |
*)

[<PlainExporter; MemoryDiagnoser>]
type EnumeratorBenchmark() =

    [<Literal>]
    let COUNT = 10
    
    let arr = [| 1..COUNT |]
    let list = [1..COUNT]
    let seq = [| 1..COUNT |] :> seq<_>
    let indexlist = IndexList.ofList  [1..COUNT]
    let r = RangeEnumerable([| 1..COUNT |])
    
    /// Baseline with cache coherent collection
    [<Benchmark>]
    member x.ArraySum() =
        Array.sum arr

    /// Baseline with node based collection
    [<Benchmark>]
    member x.ListSum() =
        List.sum list
    
    /// Inlining of typed struct enumerator
    [<Benchmark>]
    member x.RangeEnumerator() =
        let mutable sum = 0
        for a in r do
            sum <- sum + a
        sum
     
    /// Inlining of typed struct enumerator
    [<Benchmark>]
    member x.RangeEnumeratorStatic() =
        use mutable r = new RangeEnumerator(arr)
        let mutable sum = 0
        while r.MoveNext() do
            sum <- sum + r.Current
        sum
            
    [<Benchmark>]
    member x.ListEnumerator() =
        let mutable sum = 0
        for a in list do
            sum <- sum + a
        sum
        
    [<Benchmark>]
    member x.ListSeqEnumerator() =
        let mutable sum = 0
        for a in seq do
            sum <- sum + a
        sum

    [<Benchmark>]
    member x.IndexListEnumerator() =
        let mutable sum = 0
        for a in indexlist do
            sum <- sum + a
        sum

    [<Benchmark>]
    member x.IndexListFold() =
        indexlist |> IndexList.fold (fun a b -> a + b) 0

    [<Benchmark>]
    member x.IndexListIter() =
        let mutable sum = 0
        indexlist |> IndexList.iter (fun v -> sum <- sum + v)
        sum

    [<Benchmark>]
    member x.MapExtIter() =
        let mutable sum = 0
        indexlist.Content |> MapExt.iter (fun k v -> sum <- sum + v)
        sum

    [<Benchmark>]
    member x.MapExtFold() =
        indexlist.Content |> MapExt.fold (fun a _ b -> a + b) 0 

    [<Benchmark>]
    member x.IndexListToArrayEnumerator() =
        let temp = indexlist.AsArray
        Array.sum temp

