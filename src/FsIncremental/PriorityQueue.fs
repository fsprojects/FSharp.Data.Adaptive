namespace FsIncremental

open System.Collections.Generic

/// <summary>
/// represents a simple priority queue using user-given compare function
/// </summary>
type PriorityQueue<'a>(cmp : 'a -> 'a -> int) =

    // we simply use the Aardvark.Base implementation here.
    // to ensure that all compare-functions used are identical
    // we wrap the base implementation in PriorityQueue.
    let store = List<'a>()
    let cmpFun = System.Func<'a,'a, int>(cmp)

    /// <summary>
    /// enqueues a new element
    /// </summary>
    member x.Enqueue (v : 'a) =
        store.HeapEnqueue(cmpFun, v)

    /// <summary>
    /// dequeues the min element from the queue and
    /// fails if the queue is empty
    /// </summary>
    member x.Dequeue() =
        store.HeapDequeue(cmpFun)

    /// <summary>
    /// returns the number of elements currently contained in the queue
    /// </summary>
    member x.Count =
        store.Count

    /// <summary>
    /// returns the current minimal value (according to cmp) contained
    /// and fails if the queue is empty.
    /// </summary>
    member x.Min = store.[0]

/// <summary>
/// implements a queue with "uncomparable" duplicates. 
/// This is helpful since regular heap implementation cannot
/// deal with a large number of duplicated keys efficiently.
/// Note: the duplicated values will be returned in the order they were enqueued
/// </summary>
type DuplicatePriorityQueue<'a, 'k when 'k : comparison>(extract : 'a -> 'k) =
    let q = PriorityQueue<'k> compare
    let values = Dictionary<'k, Queue<'a>>()
    let mutable count = 0
    let mutable currentQueue = Unchecked.defaultof<_>

    /// <summary>
    /// enqueues a new element
    /// </summary>
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

             
    /// <summary>
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

    /// <summary>
    /// returns the number of elements currently contained in the queue
    /// </summary>
    member x.Count =
        count

