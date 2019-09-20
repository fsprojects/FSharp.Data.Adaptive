namespace FSharp.Control.Incremental

open System.Collections.Generic

/// represents a simple priority queue using user-given compare function
type internal PriorityQueue<'a>(cmp : 'a -> 'a -> int) =

    // we simply use the Aardvark.Base implementation here.
    // to ensure that all compare-functions used are identical
    // we wrap the base implementation in PriorityQueue.
    let store = List<'a>()
    let cmpFun = OptimizedClosures.FSharpFunc<'a, 'a, int>.Adapt(cmp)

    /// enqueues a new element
    member x.Enqueue (v : 'a) =
        store.HeapEnqueue(cmpFun, v)

    /// dequeues the min element from the queue and
    /// fails if the queue is empty
    member x.Dequeue() =
        store.HeapDequeue(cmpFun)

    /// returns the number of elements currently contained in the queue
    member x.Count =
        store.Count

    /// returns the current minimal value (according to cmp) contained
    /// and fails if the queue is empty.
    member x.Min = store.[0]

/// implements a queue with "uncomparable" duplicates. 
/// This is helpful since regular heap implementation cannot
/// deal with a large number of duplicated keys efficiently.
/// Note: the duplicated values will be returned in the order they were enqueued
type internal DuplicatePriorityQueue<'a, 'k when 'k : comparison>(extract : 'a -> 'k) =
    let q = PriorityQueue<'k> compare
    let values = Dictionary<'k, Queue<'a>>()
    let mutable count = 0
    let mutable currentQueue = Unchecked.defaultof<_>

    /// enqueues a new element
    member x.Enqueue(v : 'a) =
        let k = extract v
        count <- count + 1

        if values.TryGetValue(k, &currentQueue) then
            currentQueue.Enqueue v
        else
            let inner = Queue<'a>()
            inner.Enqueue v
            values.[k] <- inner
            q.Enqueue k

             
    /// dequeues the current minimal value (and its key)
    /// </summary>   
    member x.Dequeue(key : byref<'k>) =
        let k = q.Min

        if values.TryGetValue(k, &currentQueue) then
            let res = currentQueue.Dequeue()
            count <- count - 1
            if currentQueue.Count = 0 then
                q.Dequeue() |> ignore
                values.Remove k |> ignore

            key <- k
            res
        else
            failwith "inconsistent state in DuplicatePriorityQueue"

    /// returns the number of elements currently contained in the queue
    member x.Count =
        count

