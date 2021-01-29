namespace Benchmarks

open System
open FSharp.Data.Adaptive
open FSharp.Data.Traceable
open BenchmarkDotNet.Attributes
open System.Runtime.CompilerServices
open BenchmarkDotNet.Configs


[<PlainExporter; MemoryDiagnoser; IterationTime(100.0); MaxIterationCount(20)>]
[<GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByCategory)>]
type HashSetBenchmarks() =

    let mutable hashSet = HashSet.empty<int>
    let mutable set = Set.empty<int>
   
    [<Params(1, 10, 50, 100, 1000, 10000); DefaultValue>]
    val mutable public Count : int

    [<GlobalSetup>]
    member x.Setup() =
        let rand = Random(123)
        hashSet <- HashSet.ofArray (Array.init x.Count (fun i -> rand.Next() ))
        set <- Set.ofSeq hashSet
        
    [<Benchmark; BenchmarkCategory("toArray")>]
    member x.HashSet_toArray() =
        hashSet.ToArray()
        
    [<Benchmark; BenchmarkCategory("toList")>]
    member x.HashSet_toList() =
        hashSet.ToList()
        
    [<Benchmark; BenchmarkCategory("toList")>]
    member x.HashSet_toListViaArray() =
        hashSet.ToArray() |> Array.toList