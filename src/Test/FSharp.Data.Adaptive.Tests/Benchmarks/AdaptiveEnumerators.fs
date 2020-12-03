namespace Benchmarks

open System
open FSharp.Data.Adaptive
open FSharp.Data.Traceable
open BenchmarkDotNet.Attributes

[<PlainExporter; MemoryDiagnoser>]
type AdaptiveEnumeratorBenchmark() =

    let mutable indexList = IndexList.empty<int>
    let mutable hashSet = HashSet.empty<int>
    let mutable hashMap = HashMap.empty<int, int>
    let mutable countingHashSet = CountingHashSet.empty<int>
    let mutable indexListDelta = IndexListDelta.empty<int>
    let mutable hashSetDelta = HashSetDelta.empty<int>
    let mutable hashMapDelta = HashMapDelta.empty<int, int>
   
    [<Params(0, 1, 10, 100, 1000); DefaultValue>]
    val mutable public Count : int

    [<GlobalSetup>]
    member x.Setup() =
        InlineStackBenchmark.Check()
        let rand = Random()
        let arr = 
            let mutable set = HashSet.empty
            while set.Count < x.Count do
                let v = rand.Next()
                set <- HashSet.add v set

            HashSet.toArray set

        indexList <- IndexList.ofArray arr
        hashSet <- HashSet.ofArray arr
        hashMap <- HashMap.ofArray (arr |> Array.map (fun a -> a,a))
        countingHashSet <- CountingHashSet.ofArray arr
        indexListDelta <- IndexList.computeDelta IndexList.empty indexList
        hashSetDelta <- HashSet.computeDelta HashSet.empty hashSet
        hashMapDelta <- HashMap.computeDelta HashMap.empty hashMap

    [<Benchmark>]
    member x.IndexList() =
        let mutable sum = 0
        for e in indexList do sum <- sum + e
        sum

    [<Benchmark>]
    member x.HashSet() =
        let mutable sum = 0
        for e in hashSet do sum <- sum + e
        sum

    [<Benchmark>]
    member x.HashMap() =
        let mutable sum = 0
        for (e,_) in hashMap do sum <- sum + e
        sum

    [<Benchmark>]
    member x.CountingHashSet() =
        let mutable sum = 0
        for e in countingHashSet do sum <- sum + e
        sum

    [<Benchmark>]
    member x.IndexListDelta() =
        let mutable sum = 0
        for (_,e) in indexListDelta do sum <- sum + 1
        sum
        
    [<Benchmark>]
    member x.HashSetDelta() =
        let mutable sum = 0
        for d in hashSetDelta do sum <- sum + d.Value
        sum
        
    [<Benchmark>]
    member x.HashMapDelta() =
        let mutable sum = 0
        for d in hashMapDelta do sum <- sum + 1
        sum