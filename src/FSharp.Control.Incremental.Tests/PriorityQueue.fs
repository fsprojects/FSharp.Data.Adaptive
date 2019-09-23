module PriorityQueue

open System.Collections.Generic
open FSharp.Control.Incremental
open FsUnit
open FsCheck
open FsCheck.NUnit


[<Property>]
let ``[Heap] sorting`` (values : array<int>) =
    let l = List<int>()
    for v in values do
        l.HeapEnqueue(compare, v)

    l.Count |> should equal values.Length

    let sorted = List.sort (Array.toList values)
    let heap = List.init l.Count (fun _ -> l.HeapDequeue(compare))

    heap |> should equal sorted

[<Property>]
let ``[Heap] enqueue`` (values : array<int>) =
    let l = List<int>()
    for v in values do
        l.HeapEnqueue(compare, v)
    
    l.Count |> should equal values.Length

    let rec validateHeapOrder (i : int) =
        let li = 2 * i + 1
        let ri = 2 * i + 2

        if li < l.Count then
            l.[i] |> should be (lessThanOrEqualTo l.[li])
            validateHeapOrder li
            
        if ri < l.Count then
            l.[i] |> should be (lessThanOrEqualTo l.[ri])
            validateHeapOrder ri

    if l.Count > 0 then
        validateHeapOrder 0
        

[<Property>]
let ``[PriorityQueue] enqueue`` (values : array<int>) =
    let queue = PriorityQueue(compare)
    values |> Array.iter queue.Enqueue
    queue.Count |> should equal values.Length
    
[<Property>]
let ``[PriorityQueue] sorting`` (values : array<int>) =
    let queue = PriorityQueue(compare)
    values |> Array.iter queue.Enqueue
    let sorted = List.sort (Array.toList values)
    let heap = List.init queue.Count (fun _ -> queue.Dequeue())
    heap |> should equal sorted


[<Property>]
let ``[DuplicatePriorityQueue] enqueue`` (values : array<int>) =
    let queue = DuplicatePriorityQueue(id)
    values |> Array.iter queue.Enqueue
    queue.Count |> should equal values.Length

        
[<Property>]
let ``[DuplicatePriorityQueue] sorting`` (values : array<int>) =
    let queue = DuplicatePriorityQueue(id)
    values |> Array.iter queue.Enqueue
    let sorted = List.sort (Array.toList values)
    let mutable foo = 0
    let heap = List.init queue.Count (fun _ -> queue.Dequeue(&foo))
    heap |> should equal sorted
