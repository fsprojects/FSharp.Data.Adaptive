module PriorityQueue

open System.Collections.Generic
open FSharp.Data.Adaptive
open FsUnit
open FsCheck
open FsCheck.NUnit


[<Property(EndSize = 10000)>]
let ``[Heap] sorting`` (values : array<int>) =
    let l = List<int>()
    for v in values do
        l.HeapEnqueue(compare, v)

    l.Count |> should equal values.Length

    let sorted = List.sort (Array.toList values)
    let heap = List.init l.Count (fun _ -> l.HeapDequeue(compare))

    heap |> should equal sorted

[<Property(EndSize = 10000)>]
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
