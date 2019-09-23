module ASet

open Xunit
open FsCheck
open FSharp.Control.Incremental
open FSharp.Control.Incremental.Validation
open FSharp.Control.Traceable
open FsUnit.Xunit
open FsCheck.Xunit

type Record<'a> = { value : 'a }

[<AutoOpen>]
module Helpers =
    let check (r : ISetReader<'a>) =
        let a = r.Adaptive.GetOperations FSharp.Control.Incremental.AdaptiveToken.Top
        let r = r.Reference.GetOperations FSharp.Control.Incremental.Reference.AdaptiveToken.Top
        a |> should setequal r
        r

let emptyDelta = DHashSet.empty<int>

[<Fact>]
let ``[CSet] reader add/remove/clear/union/except`` () =
    let set = cset<int>(HashSet.empty)

    let r = set.GetReader()
    r |> check |> should setequal emptyDelta

    // add 1
    transact (fun () -> set.Add 1) |> should be True
    set.Value |> should setequal [1]
    r |> check |> should setequal [Add 1]
    r.Adaptive.State |> should setequal [1]
    
    // add 1;2;3
    transact (fun () -> set.UnionWith [1;2;3])
    set.Value |> should setequal [1;2;3]
    r |> check |> should setequal [Add 2; Add 3]
    r.Adaptive.State |> should setequal [1;2;3]

    // remove 1;3
    transact (fun () -> set.ExceptWith [1;3])
    set.Value |> should setequal [2]
    r |> check |> should setequal [Rem 1; Rem 3]
    r.Adaptive.State |> should setequal [2]

    /// clear
    transact (fun () -> set.Clear())
    set.Value |> should setequal List.empty<int>
    r |> check |> should setequal [Rem 2]
    r.Adaptive.State |> should setequal List.empty<int>

[<Fact>]
let ``[CSet] contains/isEmpty/count`` () =
    let set = cset(HashSet.ofList [1;2])

    set.IsEmpty |> should be False
    set.Count |> should equal 2
    set.Contains 1 |> should be True
    set.Contains 2 |> should be True

    transact (fun () ->
        set.Remove 2 |> should be True
    )
    
    set.IsEmpty |> should be False
    set.Count |> should equal 1
    set.Contains 1 |> should be True
    set.Contains 2 |> should be False

    
    transact (fun () ->
        set.Remove 1 |> should be True
    )
    
    set.IsEmpty |> should be True
    set.Count |> should equal 0
    set.Contains 1 |> should be False
    set.Contains 2 |> should be False

[<Property>]
let ``[ASet] map``(values : list<Set<int>>) =
    let initial = cset(HashSet.empty)
    let derived = ASet.map (fun a -> a * 10) initial
    let reader = derived.GetReader()

    reader |> check |> should setequal emptyDelta

    let mutable i = 0
    for s in values do
        let s = 
            if i % 7 < 3 then Set.add i s
            else s

        transact (fun () -> initial.Value <- HashSet.ofSeq s)
        reader |> check |> ignore
        i <- i + 1





