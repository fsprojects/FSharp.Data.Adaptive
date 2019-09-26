module AList

open NUnit.Framework
open FsCheck
open FSharp.Data.Adaptive
open FSharp.Data.Traceable
open FsUnit
open FsCheck.NUnit
open FSharp.Data
open Generators

[<Property(Arbitrary = [| typeof<Generators.AdaptiveGenerators> |]); Timeout(60000)>]
let ``[AList] reference impl``() ({ lreal = real; lref = ref; lexpression = str; lchanges = changes } : VList<int>) =
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

        let lReal = IndexList.toArray vReal
        let lRef = IndexList.toArray vRef

        let equal =
            lReal.Length = lRef.Length &&
            (lReal, lRef) ||> Array.forall2 (=)

        if equal then
            vRef
        else
            let delta = IndexList.differentiate vReal vRef |> IndexListDelta.toList
            match delta with
            | [] ->
                vRef
            | delta ->
                let real = vReal |> Seq.map string |> String.concat "; " |> sprintf "[%s]"
                let ref = vRef |> Seq.map string |> String.concat "; " |> sprintf "[%s]"
                let delta = delta |> Seq.map string |> String.concat "; "  |> sprintf "[%s]"
            
                let inputs = changes() |> List.map (fun i -> i.cell)

                let message =
                    String.concat "\r\n" [
                        yield "ERROR"
                        yield "BEFORE"
                        //yield! beforeChangeStr.Split("\r\n") |> Array.map Generators.indent
                    
                        yield "CURRENT"
                        yield! (str true).Split("\r\n") |> Array.map Generators.indent

                        yield sprintf "real:  %s" real
                        yield sprintf "ref:   %s" ref
                        yield sprintf "delta: %s" delta
                    
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
                    if not (Unchecked.equals v lastValue) then
                        
                        printfn "  change %d => %A" effective v
                        lastValue <- v
                    

     
                    effective <- effective + 1
        }

    Gen.eval 15 (Random.newSeed()) run

[<Test>] 
let ``[IndexMapping] correct``() =
    let m = AdaptiveIndexListHelpers.IndexMapping<int*int>()

    let rand = System.Random()
    for i in 1 .. 100 do
        let data = List.init 50 (fun _ -> rand.Next(), rand.Next()) |> List.distinct
        printfn "%A" data
        let indices = data |> List.map m.Invoke
        let mutable m = Map.empty
        for (i, v) in List.zip indices data do
            m <- Map.add i v m

        let sorted = m |> Map.toSeq |> Seq.map snd |> Seq.toList
        data |> List.sort |> should equal sorted

 
[<Test>]
let ``[AList] duplicate inner``() =
    let a = clist [1;2]
    let b = clist [6;6;6]

    let i = clist []


    let test = 
        i |> AList.collecti (fun _ v ->
            if v % 2 = 1 then a :> alist<_>
            else b :> alist<_>
        )

    let r = test.GetReader()

    r.GetChanges AdaptiveToken.Top |> ignore
    r.State |> IndexList.toList |> should equal List.empty<int>

    transact (fun () ->
        i.Value <- IndexList.ofList [1;2]
    )
    
    r.GetChanges AdaptiveToken.Top |> ignore
    r.State |> IndexList.toList |> should equal [1;2;6;6;6]
    
    let mutable ai = Index.after Index.zero
    for i in 1 .. 10 do
        ai <- Index.between Index.zero ai

    transact (fun () ->
        i.Value <- IndexList.ofList [1;2;3]

        
        let mutable ai = Index.after Index.zero
        for i in 1 .. 10 do
            ai <- Index.between Index.zero ai


        a.Value <- IndexList.ofList [1;2;3]
    )
    
    r.GetChanges AdaptiveToken.Top |> ignore
    r.State |> IndexList.toList |> should equal [1;2;3;6;6;6;1;2;3]
