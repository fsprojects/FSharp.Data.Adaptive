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

    let str b =
        let m, str = str b
        
        String.concat "\r\n" [
            for (k,v) in Map.toSeq m do
                yield sprintf "let %s = %s" k v
            yield str
        ]


    printfn "%s" (Generators.Generators.indent (Generators.Generators.indent (str false)))
     
    let r = real.GetReader()

    let check (beforeChangeStr : string) (beforeChange : list<string>) (latestChanges : list<string>) = 
        r.GetChanges AdaptiveToken.Top |> ignore
        let vReal = real.Content.GetValue AdaptiveToken.Top // |> CountingHashSet.toHashSet
        let vRef = ref.Content.GetValue Reference.AdaptiveToken.Top

        let delta = HashSet.differentiate vReal vRef |> HashSetDelta.toList
        match delta with
        | [] ->
            vRef
        | delta ->
            let real = vReal |> Seq.sort |> Seq.map string |> String.concat "; " |> sprintf "[%s]"
            let ref = vRef |> Seq.sort |> Seq.map string |> String.concat "; " |> sprintf "[%s]"
            let delta = delta |> Seq.sortBy (fun v -> v.Value) |> Seq.map string |> String.concat "; "  |> sprintf "[%s]"
            
            let inputs = changes() |> List.map (fun i -> i.cell)

            let message =
                String.concat "\r\n" [
                    yield "ERROR"
                    yield "BEFORE"
                    yield! beforeChangeStr.Split("\r\n") |> Array.map Generators.indent
                    
                    yield "CURRENT"
                    yield! (str true).Split("\r\n") |> Array.map Generators.indent

                    yield sprintf "real:  %s" real
                    yield sprintf "ref:   %s" ref
                    yield sprintf "delta: %s" delta
                    
                    yield "before"
                    for i in beforeChange do
                        yield sprintf "   %A" i

                    yield "inputs"
                    for i in inputs do
                        yield sprintf "   %A" i

                    yield "latest changes"
                    for c in latestChanges do
                        yield "   " + c
                ]
            failwith message
        //printfn "    VALUE => %A" vRef
             
    let mutable lastValue = check "" [] []

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

                    let beforeChange =
                        all |> List.map (fun c -> c.cell |> string)

                    let beforeChangeStr = str true
                    let! changeAll = Gen.collect id some
                    let latestChange = 
                        transact (fun () ->
                            changeAll |> List.map (fun c -> c())
                        )
                    let v = check beforeChangeStr beforeChange latestChange
                    if not (Unchecked.equals v lastValue) then
                        printfn "  change %d => %A" effective v
                        lastValue <- v

                    effective <- effective + 1
        }

    Gen.eval 15 (Random.newSeed()) run

[<Property>]
let ``[CountingHashSet] ref counts`` (input : Set<int>) =
    let set = 
        input 
        |> Seq.map (fun v -> v, 2) 
        |> HashMap.ofSeq
        |> CountingHashSet.ofHashMap

    let direct =
        CountingHashSet.ofSeq input

    input 
    |> Set.fold (fun s v -> CountingHashSet.remove v s) set
    |> should setequal set

    input 
    |> Set.fold (fun s v -> CountingHashSet.remove v (CountingHashSet.remove v s)) set
    |> should setequal CountingHashSet.empty<int>

    let ops =
        set |> Seq.map Rem |> HashSetDelta.ofSeq

    let s, e = CountingHashSet.integrate set ops
    e |> should setequal HashSet.empty<SetOperation<int>>
    s |> should setequal set

    
    let s, e = CountingHashSet.integrate s ops
    e |> should setequal (set |> Seq.map Rem |> HashSet.ofSeq)
    s |> should setequal HashSet.empty<int>

    ()
[<Test>]
let ``[ASet] collect 2``() =
    let inline cset a = cset (HashSet.ofList a)

    let v1 = ASet.ofList [0; 10; -6; -4]
    let v2 = ASet.empty

    let c71 = cset [0; 2; 7; -2; -1] 
    let c30 = cset [false; true]
    let c55 = cset [0; 26]
    let c31 = cset [0; 1; 2; 6]
    let c23 = cset [false; true] 
    let c124 = cset [0; 1; 2; -4; -2; -1]
    let c69 = cset [0; 5]
    let c70 = cset [0; 1; -1]
    let c125 = cset [0; 2; 16]
    let c83 = cset [0; 17; -3]
    let tree =
        c71 |> ASet.collect (
            function
            | 0 -> 
                c30 |> ASet.collect (
                    function
                    | false -> v1
                    | true -> c55 :> aset<_>
                )
            | 2 ->
                c31 :> aset<_>
            | 7 ->
                c23 |> ASet.collect (
                    function
                    | false -> c124 :> aset<_>
                    | true -> ASet.collect (fun _ -> ASet.empty) v2
                )
            | -2 ->
                ASet.collect (fun _ -> ASet.empty) v2
            | -1 ->
                c69 |> ASet.collect (
                    function
                    | 0 -> c70 :> aset<_>
                    | 5 -> c125 :> aset<_>
                    | _ -> ASet.empty
                )
            | -14 -> 
                c83 :> aset<_>

            | _ ->
                ASet.empty
        )

    let reader = tree.GetReader()
    let ops = check reader
    let state = reader.Reference.State
    printfn "%A" state

    transact (fun () ->
        c124.Value <- HashSet.ofList [0; -2; -1]
        c69.Value <- HashSet.ofList [0; 5; -3; -2; -1]
        c71.Value <- HashSet.ofList [-14]
        c23.Value <- HashSet.ofList [false; true]
    )

    let ops = check reader
    let state = reader.Reference.State
    printfn "%A" state

[<Test>]
let ``[ASet] collect test``() =
    
    //let c4 = cset (HashSet.ofList [0; 1; -9])


    //let c1 = cset (HashSet.ofList [0; 1; -1])
    //let c4 = cset (HashSet.ofList [0; 7; -2])
    //let c26 = cset (HashSet.ofList [0; 1; -11; -5; -4])

    //// [0; 1; -9]

    //let v1 = ASet.ofList [0; 1; 2; -2; -1]
    //let mapping : int -> aset<int> =
    //    function
    //    | 0 -> v1
    //    | 7 -> c26 :> aset<_>
    //    | -2 -> c1 :> aset<_>
    //    | -9 -> 
    //    | _ -> ASet.empty<int>

    //let test = c4 |> ASet.collect mapping
    //let r = test.GetReader()
    //check r |> ignore


    let input = cset (HashSet.ofList [1;2;3])

    let cache = System.Collections.Generic.Dictionary<int, cset<int>>()
    let outCache = System.Collections.Generic.Dictionary<int, aset<int>>()

    let test =
        input |> ASet.collect (fun v ->
            match outCache.TryGetValue v with
            | (true, s) -> 
                s
            | _ ->
                let s = cset (HashSet.ofList [v])
                cache.[v] <- s
                let res = s |> ASet.map (fun v -> v % 3)
                outCache.[v] <- res
                res
        )

    let r = test.GetReader()
    check r |> ignore

    transact (fun () -> input.Add 4 |> ignore)
    check r |> ignore

    transact (fun () -> cache.[1].Value <- HashSet.ofList [6;8;1])
    check r |> ignore
    
    transact (fun () -> cache.[2].Value <- HashSet.ofList [6;8;1])
    check r |> ignore
    
    transact (fun () -> cache.[4].Value <- HashSet.ofList [])
    check r |> ignore
    
    transact (fun () -> cache.[2].Value <- HashSet.empty)
    check r |> ignore

    
    transact (fun () -> cache.[1].Value <- HashSet.empty)
    check r |> ignore
    
    transact (fun () -> input.Value <- HashSet.empty; cache.[1].Value <- HashSet.ofList [123])
    check r |> ignore

    cache.Clear()
    
    transact (fun () -> input.Value <- HashSet.ofList [1;2;3;4])
    check r |> ignore





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









