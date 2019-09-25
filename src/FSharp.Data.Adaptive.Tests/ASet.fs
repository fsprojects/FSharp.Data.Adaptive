module ASet

open NUnit.Framework
open FsCheck
open FSharp.Data.Adaptive
open FSharp.Data.Adaptive.Validation
open FSharp.Data.Traceable
open FsUnit
open FsCheck.NUnit

type Record<'T> = { value : 'T }

[<AutoOpen>]
module Helpers =
    let check (r : IHashSetReader<'T>) =
        let a = r.Adaptive.GetChanges FSharp.Data.Adaptive.AdaptiveToken.Top
        let r = r.Reference.GetChanges FSharp.Data.Adaptive.Reference.AdaptiveToken.Top
        a |> should setequal r
        r

let emptyDelta = HashSetDelta.empty<int>

open FSharp.Data
open Generators
[<Property(Arbitrary = [| typeof<Generators.AdaptiveGenerators> |])>]
let ``[ASet] reference tests``() ({ sreal = real; sref = ref; sexpression = str; schanges = changes } : VSet<int>) =
    printfn "VALIDATE"
    printfn "%s" (Generators.Generators.indent (Generators.Generators.indent str))
     
    let r = real.GetReader()

    let check() = 
        r.GetChanges AdaptiveToken.Top |> ignore
        let vReal = real.Content.GetValue AdaptiveToken.Top // |> CountingHashSet.toHashSet
        let vRef = ref.Content.GetValue Reference.AdaptiveToken.Top
        vReal |> should setequal vRef
        vRef
        //printfn "    VALUE => %A" vRef
             
    let mutable lastValue = check()

    let run = 
        gen {
            let mutable effective = 0

            while effective < 20 do
                let all = changes() 
                match all with
                | [] -> 
                    effective <- System.Int32.MaxValue
                | _ -> 
                    let! some = 
                        all
                        |> List.map (fun g -> g.change) 
                        |> Gen.subListOf
                        |> Gen.filter (List.isEmpty >> not)

                    let! changeAll = Gen.collect id some
                    transact (fun () ->
                        changeAll |> List.map (fun c -> c()) |> ignore
                    )
                    let v = check()
                    if not (Unchecked.equals v lastValue) then
                        printfn "  change %d => %A" effective v
                        lastValue <- v

                    effective <- effective + 1
        }

    Gen.eval 15 (Random.newSeed()) run





[<Test>]
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

    /// Clear
    transact (fun () -> set.Clear())
    set.Value |> should setequal List.empty<int>
    r |> check |> should setequal [Rem 2]
    r.Adaptive.State |> should setequal List.empty<int>

[<Test>]
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

[<Property>]
let ``[ASet] choose``(values : list<Set<int>>) =
    let initial = cset(HashSet.empty)
    let derived = ASet.choose (fun a -> if a % 2 = 0 then Some (a * 10) else None) initial
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

[<Property>]
let ``[ASet] filter``(values : list<Set<int>>) =
    let initial = cset(HashSet.empty)
    let derived = ASet.filter (fun a -> a % 2 = 0) initial
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

[<Property>]
let ``[ASet] union``(all : list<list<Set<int>>>) =
    
    let initial = cset<aset<int>>(HashSet.empty)
    let innerSets = System.Collections.Generic.List<cset<int>>()

    let derived = ASet.union initial
    let reader = derived.GetReader()

    reader |> check |> should setequal emptyDelta

    for values in all do
        let arr = List.toArray values
        transact (fun () -> 
            for i in 0 .. arr.Length - 1 do
                if i < innerSets.Count then 
                    innerSets.[i].Value <- HashSet.ofSeq arr.[i]
                else 
                    let set = cset (HashSet.ofSeq arr.[i])
                    innerSets.Add set

            initial.Value <- HashSet.ofSeq (Seq.take arr.Length (Seq.cast innerSets))
        )
        reader |> check |> ignore

[<Property>]
let ``[ASet] collect``(all : list<list<Set<int>>>) =
    
    let initial = cset<cset<int>>(HashSet.empty)
    let innerSets = System.Collections.Generic.List<cset<int>>()

    let derived = ASet.collect (fun c -> c :> aset<_>) initial
    let reader = derived.GetReader()

    reader |> check |> should setequal emptyDelta

    for values in all do
        let arr = List.toArray values
        transact (fun () -> 
            for i in 0 .. arr.Length - 1 do
                if i < innerSets.Count then 
                    innerSets.[i].Value <- HashSet.ofSeq arr.[i]
                else 
                    let set = cset (HashSet.ofSeq arr.[i])
                    innerSets.Add set

            initial.Value <- HashSet.ofSeq (Seq.take arr.Length innerSets)
        )
        reader |> check |> ignore

[<Property>]
let ``[ASet] toAVal / ofAVal``(all : list<Set<int>>) =
    let all = all |> List.map HashSet.ofSeq

    let mutable last = HashSet.empty
    let input = cset<int>(HashSet.empty)
    let roundtrip = input |> ASet.toAVal |> ASet.ofAVal
    let reader = roundtrip.GetReader()

    for set in all do
        transact (fun () -> input.Value <- set)
        reader |> check |> should setequal (HashSet.differentiate last set)
        last <- set









