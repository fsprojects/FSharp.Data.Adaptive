module IndexList


open System
open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit
open FSharp.Data.Adaptive
open FSharp.Data.Traceable


[<Property>]
let ``[IndexList] creation`` (l : list<int>) =
    let test = l |> IndexList.ofList |> IndexList.toList
    test |> should equal l

[<Property>]
let ``[IndexList] count`` (l : list<int>) =
    let test = l |> IndexList.ofList
    test.Count |> should equal (List.length l)

[<Property>]
let ``[IndexList] append`` (l : list<int>) (r : list<int>) =
    let ll = IndexList.ofList l
    let rl = IndexList.ofList r

    IndexList.append ll rl 
    |> IndexList.toList
    |> should equal (List.append l r)

    
[<Property>]
let ``[IndexList] add/prepend`` (l : list<int>) = 
    let indexList = IndexList.ofList l
    indexList 
    |> IndexList.add 1
    |> IndexList.prepend 5
    |> IndexList.toList
    |> should equal ([5] @ l @ [1])

[<Property>]
let ``[IndexList] equality`` (l : list<int>) =
    let a = IndexList.ofList l

    a |> should equal a
    a |> should not' (equal (IndexList.add 1 a))
    a |> should not' (equal (IndexList.prepend 1 a))

[<Property>]
let ``[IndexList] computeDelta / applyDelta`` (l1 : list<int>) (l2 : list<int>) (l3 : list<int>) =
    let l1 = IndexList.ofList l1
    let l2 = IndexList.ofList l2
    let l3 = IndexList.ofList l3

    IndexList.computeDelta l1 l1 |> should equal IndexListDelta.empty<int>
    IndexList.computeDelta l2 l2 |> should equal IndexListDelta.empty<int>
    IndexList.computeDelta l3 l3 |> should equal IndexListDelta.empty<int>

    
    let d12 = IndexList.computeDelta l1 l2
    let d23 = IndexList.computeDelta l2 l3 
    let d31 = IndexList.computeDelta l3 l1 

    IndexList.applyDelta l1 d12 |> fst |> should equal l2
    IndexList.applyDelta l2 d23 |> fst |> should equal l3
    IndexList.applyDelta l3 d31 |> fst |> should equal l1
    
    let d123 = IndexListDelta.combine d12 d23
    let d231 = IndexListDelta.combine d23 d31
    let d312 = IndexListDelta.combine d31 d12
    IndexList.applyDelta l1 d123 |> fst |> should equal l3
    IndexList.applyDelta l2 d231 |> fst |> should equal l1
    IndexList.applyDelta l3 d312 |> fst |> should equal l2
    
    let d1231 = IndexListDelta.combine d123 d31
    let d2312 = IndexListDelta.combine d231 d12
    let d3123 = IndexListDelta.combine d312 d23
    IndexList.applyDelta l1 d1231 |> fst |> should equal l1
    IndexList.applyDelta l2 d2312 |> fst |> should equal l2
    IndexList.applyDelta l3 d3123 |> fst |> should equal l3