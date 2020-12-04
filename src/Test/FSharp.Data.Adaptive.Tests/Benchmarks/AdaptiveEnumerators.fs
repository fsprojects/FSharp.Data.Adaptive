namespace Benchmarks

open System
open FSharp.Data.Adaptive
open FSharp.Data.Traceable
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Jobs

type ImprovedHashSetEnumerator<'T> =
    struct
        val mutable private Root : HashSetNode<'T>
        val mutable private Head : HashSetNode<'T>
        val mutable private Tail : list<HashSetNode<'T>>
        val mutable private Values : 'T[]
        val mutable private ValueCount : int
        val mutable private Index : int

        member inline private x.Push(n : HashSetNode<'T>) =
            if isNull (x.Head :> obj) then
                x.Head <- n
            else
                x.Tail <- x.Head :: x.Tail
                x.Head <- n
                
        member inline private x.Pop() =
            match x.Tail with
            | [] -> 
                let v = x.Head
                x.Head <- Unchecked.defaultof<_>
                v
            | h :: t ->
                let v = x.Head
                x.Head <- h
                x.Tail <- t
                v

        member private x.MoveNext (current : HashSetNode<'T>) =
            let mutable current = current
            let mutable run = true
            while run do
                match current with
                | :? HashSetEmpty<'T> ->
                    failwith "bad"
                | :? HashSetNoCollisionLeaf<'T> as h ->
                    x.Values.[0] <- h.Value
                    x.ValueCount <- 1
                    run <- false
                | :? HashSetCollisionLeaf<'T> as h ->
                    if h.Count <= 16 then
                        x.Values.[0] <- h.Value
                        let mutable c = h.Next
                        let mutable i = 1
                        while not (isNull c) do
                            x.Values.[i] <- c.Value
                            c <- c.Next
                            i <- i + 1
                        x.ValueCount <- i    
                    else
                        // incredibly rare (more than 16 collisions)
                        let arr = Array.zeroCreate h.Count
                        arr.[0] <- h.Value
                        let mutable c = h.Next
                        let mutable i = 1
                        while not (isNull c) do
                            arr.[i] <- c.Value
                            c <- c.Next
                            i <- i + 1
                        x.Values <- arr
                        x.ValueCount <- i   
                    run <- false
                | :? HashSetInner<'T> as h ->
                    if h.Count <= 16 then
                        let index = ref 0
                        h.CopyTo(x.Values, index)
                        x.ValueCount <- !index
                        run <- false
                    else
                        match h.Left with
                        | :? HashSetEmpty<'T> ->
                            // right cannot be empty here
                            current <- h.Right
                        | _ ->
                            match h.Right with
                            | :? HashSetEmpty<'T> -> ()
                            | r -> x.Push r //x.Stack <- r :: x.Stack

                            current <- h.Left
                | _ ->
                    failwith "bad node"

            x.Index <- 0
            true

        member x.MoveNext() =
            x.Index <- x.Index + 1
            if x.Index >= x.ValueCount then
                if isNull (x.Head :> obj) then
                    false
                else
                    let h = x.Pop()
                    x.MoveNext h
            else
                true

        member x.Reset() =
            let cnt = x.Root.Count
            if cnt <= 0 then
                x.Values <- null
                x.ValueCount <- cnt
                x.Index <- -1
                x.Tail <- []
            elif cnt <= 16 then
                let array = Array.zeroCreate x.Root.Count
                let index = ref 0
                x.Root.CopyTo(array, index)
                x.Values <- array
                x.ValueCount <- array.Length
                x.Index <- -1
                x.Tail <- []
            else
                x.Values <- Array.zeroCreate 16
                x.ValueCount <- 0
                x.Index <- -1
                x.Tail <- [x.Root]

        member x.Dispose() =
            x.Values <- null
            x.ValueCount <- 0
            x.Index <- -1
            x.Tail <- []
            x.Root <- Unchecked.defaultof<_>

        member x.Current = 
            if isNull x.Values then
                match x.Root with
                | :? HashSetNoCollisionLeaf<'T> as r -> r.Value
                | _ -> failwith "bad"
            else
                x.Values.[x.Index]

        interface System.Collections.IEnumerator with
            member x.MoveNext() = x.MoveNext()
            member x.Reset() = x.Reset()
            member x.Current = x.Current :> obj
            
        interface System.Collections.Generic.IEnumerator<'T> with
            member x.Current = x.Current
            member x.Dispose() = x.Dispose()

        new (map : HashSet<'T>) =
            let cnt = map.Count
            if cnt <= 1 then
                {
                    Root = map.Root
                    Head = Unchecked.defaultof<_>
                    Tail = []
                    Values = null
                    ValueCount = cnt
                    Index = -1
                }
            elif cnt <= 16 then
                {
                    Root = map.Root
                    Head = Unchecked.defaultof<_>
                    Tail = []
                    Values = map.ToArray()
                    ValueCount = cnt
                    Index = -1
                }
            else
                {
                    Root = map.Root
                    Head = map.Root
                    Tail = []
                    Values = Array.zeroCreate 16
                    ValueCount = 0
                    Index = -1
                }
                

    end

[<PlainExporter; MemoryDiagnoser; SimpleJob(RuntimeMoniker.NetCoreApp31); SimpleJob(RuntimeMoniker.Net472)>]
type AdaptiveEnumeratorBenchmark() =

    let mutable indexList = IndexList.empty<int>
    let mutable hashSet = HashSet.empty<int>
    let mutable hashMap = HashMap.empty<int, int>
    let mutable countingHashSet = CountingHashSet.empty<int>
    let mutable indexListDelta = IndexListDelta.empty<int>
    let mutable hashSetDelta = HashSetDelta.empty<int>
    let mutable hashMapDelta = HashMapDelta.empty<int, int>

    static let check() =
        let set = HashSet.ofList [1 .. 1000]
        let res = System.Collections.Generic.List<int>()
        let mutable e = new ImprovedHashSetEnumerator<_>(set)
        while e.MoveNext() do res.Add e.Current
        if Seq.toList res <> HashSet.toList set then failwith "ImprovedHashSetEnumerator wrong"
   
        res.Clear()
        let mutable e = new HashSetEnumerator<_>(set)
        while e.MoveNext() do res.Add e.Current
        if Seq.toList res <> HashSet.toList set then failwith "HashSetEnumerator wrong"

    [<Params(1, 1000); DefaultValue>]
    val mutable public Count : int

    [<GlobalSetup>]
    member x.Setup() =
        check()
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
    member x.HashSet() =
        let mutable sum = 0
        for e in hashSet do sum <- sum + e
        sum

        
    [<Benchmark>]
    member x.HashSetImproved() =
        let mutable sum = 0
        let mutable e = new ImprovedHashSetEnumerator<int>(hashSet)
        try
            while e.MoveNext() do
                sum <- sum + e.Current
        finally
            (e :> IDisposable).Dispose()

        sum

    //[<Benchmark>]
    //member x.IndexList() =
    //    let mutable sum = 0
    //    for e in indexList do sum <- sum + e
    //    sum

    //[<Benchmark>]
    //member x.HashMap() =
    //    let mutable sum = 0
    //    for (e,_) in hashMap do sum <- sum + e
    //    sum

    //[<Benchmark>]
    //member x.CountingHashSet() =
    //    let mutable sum = 0
    //    for e in countingHashSet do sum <- sum + e
    //    sum

    //[<Benchmark>]
    //member x.IndexListDelta() =
    //    let mutable sum = 0
    //    for (_,e) in indexListDelta do sum <- sum + 1
    //    sum
        
    //[<Benchmark>]
    //member x.HashSetDelta() =
    //    let mutable sum = 0
    //    for d in hashSetDelta do sum <- sum + d.Value
    //    sum
        
    //[<Benchmark>]
    //member x.HashMapDelta() =
    //    let mutable sum = 0
    //    for d in hashMapDelta do sum <- sum + 1
    //    sum