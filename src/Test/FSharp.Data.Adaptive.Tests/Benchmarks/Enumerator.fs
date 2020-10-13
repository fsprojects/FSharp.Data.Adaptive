namespace Benchmarks

open FSharp.Data.Adaptive

open BenchmarkDotNet.Attributes
open System.Runtime.CompilerServices
open BenchmarkDotNet.Jobs

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


(*
|        Method |       Mean |     Error |    StdDev |  Gen 0 | Gen 1 | Gen 2 | Allocated |
|-------------- |-----------:|----------:|----------:|-------:|------:|------:|----------:|
|         Array |   2.880 ns | 0.0109 ns | 0.0102 ns |      - |     - |     - |         - |
|          List |   5.215 ns | 0.0137 ns | 0.0114 ns |      - |     - |     - |         - |
|       ListSeq |  90.615 ns | 1.1644 ns | 1.0892 ns | 0.0085 |     - |     - |      40 B |
|     IndexList | 147.915 ns | 2.1070 ns | 1.9709 ns | 0.0560 |     - |     - |     264 B |
|        MapExt | 203.700 ns | 4.0301 ns | 3.7698 ns | 0.0594 |     - |     - |     280 B |
|       HashSet | 197.202 ns | 0.5956 ns | 0.5280 ns | 0.1392 |     - |     - |     656 B |
|  IndexListSeq | 181.010 ns | 0.5090 ns | 0.4513 ns | 0.0560 |     - |     - |     264 B |
| IndexListIter |  74.670 ns | 0.2800 ns | 0.2482 ns | 0.0101 |     - |     - |      48 B |
| IndexListFold |  69.336 ns | 0.1507 ns | 0.1336 ns | 0.0050 |     - |     - |      24 B |
*)

type ArrayEnumerator2<'a> =
    struct
        val mutable public Index : int
        val mutable public Arr : 'a[]

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member x.MoveNext() =
            x.Index <- x.Index + 1
            x.Index < x.Arr.Length

        member x.Current 
            with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = x.Arr.[x.Index]
    
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member x.Reset() =
            x.Index <- -1

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member x.Dispose() =
            x.Index <- -1

        interface System.Collections.IEnumerator with
            member x.Current = x.Current :> obj
            member x.MoveNext() = x.MoveNext()
            member x.Reset() = x.Reset()
                    
        interface System.Collections.Generic.IEnumerator<'a> with
            member x.Current = x.Current
            member x.Dispose() = x.Dispose()

        new(arr : 'a[]) = { Arr = arr; Index = -1 }

    end
[<Sealed>]
type ArrayEnumerator<'a>(arr : 'a[]) =
    let mutable index = -1

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.MoveNext() =
        index <- index + 1
        index < arr.Length

    member x.Current 
        with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = arr.[index]
    
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Reset() =
        index <- -1

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Dispose() =
        index <- -1

    interface System.Collections.IEnumerator with
        member x.Current = x.Current :> obj
        member x.MoveNext() = x.MoveNext()
        member x.Reset() = x.Reset()
                    
    interface System.Collections.Generic.IEnumerator<'a> with
        member x.Current = x.Current
        member x.Dispose() = x.Dispose()

type ArrayEnumerable<'a>(arr : 'a[]) =
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.GetEnumerator() = new ArrayEnumerator<'a>(arr)

    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = x.GetEnumerator() :> _
        
    interface System.Collections.Generic.IEnumerable<'a> with
        member x.GetEnumerator() = x.GetEnumerator() :> _

type ArrayEnumerable2<'a>(arr : 'a[]) =
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.GetEnumerator() = new ArrayEnumerator2<'a>(arr)

    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = x.GetEnumerator() :> _
        
    interface System.Collections.Generic.IEnumerable<'a> with
        member x.GetEnumerator() = x.GetEnumerator() :> _

[<PlainExporter; MemoryDiagnoser; SimpleJob(RuntimeMoniker.NetCoreApp31)>]
type EnumeratorBenchmark() =

    let count = 10
    
    let mutable arr = [| 1..count |]
    let mutable list = Array.toList arr
    let mutable listseq = list :> seq<_>
    let mutable arrseq = arr :> seq<_>
    let mutable indexlist = IndexList.ofArray arr
    let mutable indexlistSeq = indexlist :> seq<_>
    let mutable hashset = HashSet.ofArray arr
    let mutable arrEnum = ArrayEnumerable(arr)
    let mutable arrEnumStruct = ArrayEnumerable2(arr)
    
    [<DefaultValue; Params(10)>] //, 20, 30, 50, 100, 500)>]
    val mutable public Size : int
    
    [<GlobalSetup>]
    member x.Setup() =
        arr <- [| 1 .. x.Size |]
        list <- Array.toList arr
        arrseq <- arr :> seq<_>
        listseq <- list :> seq<_>
        indexlist <- IndexList.ofArray arr
        indexlistSeq <- indexlist :> seq<_>
        hashset <- HashSet.ofArray arr
        arrEnum <- ArrayEnumerable(arr)
        arrEnumStruct <- ArrayEnumerable2(arr)


    /// Baseline with cache coherent collection
    [<Benchmark>]
    member x.Array() =   
        let mutable sum = 0
        for a in arr do
            sum <- sum + a
        sum
        
    [<Benchmark>]
    member x.ArraySeq() =   
        let mutable sum = 0
        for a in arrseq do
            sum <- sum + a
        sum

    [<Benchmark>]
    member x.ArrayEnumerable() =   
        let mutable sum = 0
        for a in arrEnum do
            sum <- sum + a
        sum
        
    [<Benchmark>]
    member x.ArrayEnumerableStruct() =   
        let mutable sum = 0
        for a in arrEnumStruct do
            sum <- sum + a
        sum

    /// Baseline with node based collection
    [<Benchmark>]
    member x.List() =
        let mutable sum = 0
        for a in list do
            sum <- sum + a
        sum
 
    [<Benchmark>]
    member x.ListSeq() =
        let mutable sum = 0
        for a in listseq do
            sum <- sum + a
        sum

    [<Benchmark>]
    member x.IndexList() =
        let mutable sum = 0
        for a in indexlist do
            sum <- sum + a
        sum
        
    [<Benchmark>]
    member x.MapExt() =
        let mutable sum = 0
        for a in indexlist.Content do
            sum <- sum + a.Value
        sum

    [<Benchmark>]
    member x.HashSet() =
        let mutable sum = 0
        for a in hashset do
            sum <- sum + a
        sum

    [<Benchmark>]
    member x.IndexListSeq() =
        let mutable sum = 0
        for a in indexlistSeq do
            sum <- sum + a
        sum

    [<Benchmark>]
    member x.IndexListIter() =
        let mutable sum = 0
        indexlist |> IndexList.iter (fun v -> sum <- sum + v)
        sum

    [<Benchmark>]
    member x.IndexListFold() =
        indexlist |> IndexList.fold (+) 0

