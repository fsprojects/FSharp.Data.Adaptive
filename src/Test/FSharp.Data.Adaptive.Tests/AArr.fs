module AArr

open NUnit.Framework
open FsCheck
open FSharp.Data.Adaptive
open FSharp.Data.Traceable
open FsUnit
open FsCheck.NUnit
open FSharp.Data
open Generators


[<Property(MaxTest = 500, Arbitrary = [| typeof<Generators.AdaptiveGenerators> |]); Timeout(60000)>]
let ``[AArr] reference impl``() ({ areal = real; aref = ref; aexpression = str; achanges = changes } : VArr<int>) =
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
        let vReal = real.Content.GetValue AdaptiveToken.Top
        let vRef = ref.Content.GetValue Reference.AdaptiveToken.Top

        let lReal = Arr.toArray vReal
        let lRef = Arr.toArray vRef

        let equal =
            lReal.Length = lRef.Length &&
            (lReal, lRef) ||> Array.forall2 (=)

        if equal then
            vRef
        else
            let real = vReal |> Seq.map string |> String.concat "; " |> sprintf "[%s]"
            let ref = vRef |> Seq.map string |> String.concat "; " |> sprintf "[%s]"
            
            let inputs = changes() |> List.map (fun i -> i.cell)

            let message =
                String.concat "\r\n" [
                    yield "ERROR"
                    yield "BEFORE"
                    //yield! beforeChangeStr.Split("\r\n") |> Array.map Generators.indent
                    
                    yield "CURRENT"
                    yield! (str true).Split([|"\r\n"|], System.StringSplitOptions.None) |> Array.map Generators.indent

                    yield sprintf "real:  %s" real
                    yield sprintf "ref:   %s" ref
                    
                    yield "before"
                    for i in beforeChange do
                        yield sprintf "   %A" i

                    //yield "inputs"
                    //for i in inputs do
                    //    yield sprintf "   %A" i

                    //yield "latest changes"
                    //for c in latestChanges do
                    //    yield "   " + c
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
                    if not (DefaultEquality.equals v lastValue) then
                        
                        printfn "  change %d => %A" effective v
                        lastValue <- v
                    

     
                    effective <- effective + 1
        }

    Gen.eval 15 (Random.newSeed()) run
  
[<Property(MaxTest = 1000, EndSize = 1000)>]
let ``[CArr] Value`` ((NonEmptyArray things) : NonEmptyArray<NonEmptyArray<int>>) =
    let (NonEmptyArray v0) = things.[0]
    
    let mutable value = Arr.ofArray v0
    let test = carr value
    test.Value |> should equal value
    
    for i in 1 .. things.Length - 1 do
        let (NonEmptyArray v0) = things.[i]
        value <- Arr.ofArray v0
        transact(fun () -> test.Value <- value)
        test.Value |> should equal value
    
[<Property(MaxTest = 1000, EndSize = 1000)>]
let ``[CArr] Add`` (NonEmptyArray (things : int[])) (NonEmptyArray (adds : int[]))=
    
    let reff = System.Collections.Generic.List things
    let res = carr things
    
    for a in adds do
        reff.Add a
        res.Add a
        reff |> Seq.toList |> should equal (Seq.toList res.Value)
    
[<Property(MaxTest = 1000, EndSize = 1000)>]
let ``[CArr] Insert`` (NonEmptyArray (things : int[])) (NonEmptyArray (inserts : (uint32 * int)[]))=
    
    let reff = System.Collections.Generic.List things
    let res = carr things
    
    for (i, a) in inserts do
        let i = int (i % (uint32 reff.Count + 1u))
        reff.Insert(i, a)
        res.Insert(i, a)
        reff |> Seq.toList |> should equal (Seq.toList res.Value)
    
    
[<Property(MaxTest = 1000, EndSize = 1000)>]
let ``[CArr] Set`` (NonEmptyArray (things : int[])) (NonEmptyArray (updates : (uint32 * int)[]))=
    
    let reff = System.Collections.Generic.List things
    let res = carr things
    
    for (i, a) in updates do
        let i = int (i % (uint32 reff.Count))
        reff.[i] <- a
        res.[i] <- a
        reff |> Seq.toList |> should equal (Seq.toList res.Value)
    
    
[<Property(MaxTest = 1000, EndSize = 1000)>]
let ``[CArr] RemoveAt`` (NonEmptyArray (things : int[])) (NonEmptyArray (removes : uint32[]))=
    
    let reff = System.Collections.Generic.List things
    let res = carr things
    
    for i in removes do
        if reff.Count = 0 then
            reff.Add (int i)
            res.Add (int i)
        else
            let i = int (i % (uint32 reff.Count))
            reff.RemoveAt i
            res.RemoveAt i
            reff |> Seq.toList |> should equal (Seq.toList res.Value)
    
    
[<Property(MaxTest = 1000, EndSize = 1000)>]
let ``[CArr] Clear`` (NonEmptyArray (things : int[])) =
    
    let reff = System.Collections.Generic.List things
    let res = carr things
    
    reff |> Seq.toList |> should equal (Seq.toList res.Value)
    res.Clear()
    reff.Clear()
    reff |> Seq.toList |> should equal (Seq.toList res.Value)
    
