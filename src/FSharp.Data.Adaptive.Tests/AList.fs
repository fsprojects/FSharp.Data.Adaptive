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
            let real = vReal |> Seq.map string |> String.concat "; " |> sprintf "[%s]"
            let ref = vRef |> Seq.map string |> String.concat "; " |> sprintf "[%s]"
            
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
        let indices = data |> List.map (fun t -> m.Invoke(t))
        let mutable m = Map.empty
        for (i, v) in List.zip indices data do
            m <- Map.add i v m

        let sorted = m |> Map.toSeq |> Seq.map snd |> Seq.toList
        data |> List.sort |> should equal sorted

 
[<Test>] 
let ``[MapExt] neighbours``() =
    
    
    let rand = System.Random()
    let randomOrder (list : seq<'a>) =
        list 
        |> Seq.toArray
        |> Array.map (fun v -> rand.Next(), v) 
        |> Array.sortBy fst 
        |> Seq.map snd

    let l = List.init 1000 (fun v -> v * 2)
    let m = l |> Seq.map (fun i -> i,i) |> randomOrder |> MapExt.ofSeq

    for i in -1 .. 1999 do
        if i % 2 = 0 then
            let (l, s, r) = MapExt.neighbours i m
            let el = if i > 0 then Some (i-2, i-2) else None
            let er = if i < 1998 then Some (i+2, i+2) else None

            l |> should equal el
            r |> should equal er
            s |> should equal (Some (i,i))
        else
            let (l, s, r) = MapExt.neighbours i m
            let el = if i > 0 then Some (i-1, i-1) else None
            let er = if i < 1998 then Some (i+1, i+1) else None
            l |> should equal el
            r |> should equal er
            s |> should equal None



        



[<Test>]
let ``[AList] duplicate inner``() =
    let a = clist [1;2]
    let b = clist [6;6;6]
    let i = clist []

    let ra = Reference.clist [1;2]
    let rb = Reference.clist [6;6;6]
    let ri = Reference.clist []
    
    let ref = 
        ri |> Reference.AList.collecti (fun _ v ->
            if v % 2 = 1 then ra :> Reference.alist<_>
            else rb :> Reference.alist<_>
        )

    let test = 
        i |> AList.collecti (fun _ v ->
            if v % 2 = 1 then a :> alist<_>
            else b :> alist<_>
        )

    let r = test.GetReader()
    let rr = ref.GetReader()

    //i: IndexList [79; 98; 32] -> IndexList [26; 89]
    //a: IndexList [79; 7; 91] -> IndexList [80]
    //b: IndexList [68; 84; 104; 121] -> IndexList [68; 84; 104; 121]


    let check () =
        r.GetChanges AdaptiveToken.Top |> ignore
        rr.GetChanges Reference.AdaptiveToken.Top |> ignore
        let real = r.State |> IndexList.toList 
        let tr = rr.State |> IndexList.toList 
        if real <> tr then
            printfn "ERROR"
            printfn "ref:  %A" tr
            printfn "real: %A" real
            real |> should equal tr
        else
            printfn "ref:  %A" tr
            printfn "real: %A" real

    check()

    transact (fun () ->
        let vi = IndexList.ofList [1; 2; 2]
        let va = IndexList.ofList [79; 7; 91]
        let vb = IndexList.ofList [1; 2; 3; 4]

        i.Value <- vi; ri.Value <- vi
        a.Value <- va; ra.Value <- va
        b.Value <- vb; rb.Value <- vb
    )
    check()
    
    printfn "BAD"
    printfn ""
    transact (fun () ->
        let vi = IndexList.ofList [2; 1]
        let va = IndexList.ofList [80]
        i.Value <- vi; ri.Value <- vi
        a.Value <- va; ra.Value <- va
    )
    
    check()


    let rand = System.Random()
    let randomChange() =
        let oi = ri.Value
        let oa = ra.Value
        let ob = rb.Value
        transact (fun () ->
            if rand.NextDouble() > 0.5 then 
                let vi = IndexList.ofArray (Array.init (rand.Next 5) (fun _ -> rand.Next(128)))
                i.Value <- vi
                ri.Value <- vi
            if rand.NextDouble() > 0.5 then 
                let va = IndexList.ofArray (Array.init (rand.Next 5) (fun _ -> rand.Next(128)))
                a.Value <- va
                ra.Value <- va
            if rand.NextDouble() > 0.5 then 
                let vb = IndexList.ofArray (Array.init (rand.Next 5) (fun _ -> rand.Next(128)))
                b.Value <- vb
                rb.Value <- vb
        )

        printfn "i: %A -> %A" oi ri.Value
        printfn "a: %A -> %A" oa ra.Value
        printfn "b: %A -> %A" ob rb.Value

        check()

    for i in 1 .. 0 do
        randomChange()