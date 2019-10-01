namespace FSharp.Data.Adaptive

open System.Collections.Generic

/// Implements a simple priority queue using user-given compare function
type internal PriorityQueue<'T>(cmp: 'T -> 'T -> int) =

    let store = List<'T>()
    let cmpFun = OptimizedClosures.FSharpFunc<'T, 'T, int>.Adapt(cmp)

    /// Enqueues a new element
    member x.Enqueue (v: 'T) =
        store.HeapEnqueue(cmpFun, v)

    /// Dequeues the min element from the queue and fails if the queue is empty
    member x.Dequeue() =
        store.HeapDequeue(cmpFun)

    /// Gets the number of elements currently contained in the queue
    member x.Count =
        store.Count

    /// Gets the current minimal value (according to cmp) contained
    /// and fails if the queue is empty.
    member x.Min = store.[0]

/// Implements a queue with "incomparable" duplicates. 
/// This is helpful since regular heap implementation cannot
/// deal with a large number of duplicated keys efficiently.
/// Note: the duplicated values will be returned in the order they were enqueued
type internal DuplicatePriorityQueue<'T, 'Key when 'Key: comparison>(extract: 'T -> 'Key) =
    let q = PriorityQueue<'Key> compare
    let values = Dictionary<'Key, Queue<'T>>()
    let mutable count = 0
    let mutable currentQueue = Unchecked.defaultof<_>

    /// Enqueues a new element
    member x.Enqueue(v: 'T) =
        let k = extract v
        count <- count + 1

        if values.TryGetValue(k, &currentQueue) then
            currentQueue.Enqueue v
        else
            let inner = Queue<'T>()
            inner.Enqueue v
            values.[k] <- inner
            q.Enqueue k
             
    /// Dequeues the current minimal value (and its key)
    member x.Dequeue(key: ref<'Key>) =
        let k = q.Min

        if values.TryGetValue(k, &currentQueue) then
            let res = currentQueue.Dequeue()
            count <- count - 1
            if currentQueue.Count = 0 then
                q.Dequeue() |> ignore
                values.Remove k |> ignore

            key := k
            res
        else
            failwith "inconsistent state in DuplicatePriorityQueue"

    /// Gets the number of elements currently contained in the queue
    member x.Count =
        count

