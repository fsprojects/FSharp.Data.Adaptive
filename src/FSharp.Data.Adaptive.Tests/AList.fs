module AList

open NUnit.Framework
open FsCheck
open FSharp.Data.Adaptive
open FSharp.Data.Traceable
open FsUnit
open FsCheck.NUnit
open FSharp.Data
open Generators

[<Property(MaxTest = 500, Arbitrary = [| typeof<Generators.AdaptiveGenerators> |]); Timeout(60000)>]
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
let ``[AList] mapA``() =
    let l = clist [1;2;3]
    let even = cval 1
    let odd = cval 0

    let result = 
        l |> AList.mapA (fun v ->
            if v % 2 = 0 then even :> aval<_>
            else odd :> aval<_>
        )
    let reader = result.GetReader()

    let check (l : list<int>) =
        reader.GetChanges AdaptiveToken.Top |> ignore
        reader.State 
        |> IndexList.toList
        |> should equal l

    // (1,0) (2,1) (3,0) 
    check [0; 1; 0]

    // (1,2) (2,1) (3,2)
    transact (fun () -> odd.Value <- 2)
    check [2; 1; 2]
    
    // (1,2) (2,1) (3,2) (4,1)
    transact (fun () -> l.Add 4 |> ignore)
    check [2; 1; 2; 1]
    
    // (1,2) (2,5) (3,2) (4,5)
    transact (fun () -> even.Value <- 5)
    check [2; 5; 2; 5]
    
    // (2,5) (3,2) (4,5)
    transact (fun () -> l.RemoveAt 0 |> ignore)
    check [5; 2; 5]
    
    // (2,1) (3,0) (4,1)
    transact (fun () -> even.Value <- 1; odd.Value <- 0)
    check [1; 0; 1]
  
[<Test>]
let ``[AList] chooseA``() =
    let l = clist [1;2;3]
    let even = cval (Some 1)
    let odd = cval (Some 0)

    let result = 
        l |> AList.chooseA (fun v ->
            if v % 2 = 0 then even :> aval<_>
            else odd :> aval<_>
        )
    let reader = result.GetReader()

    let check (l : list<int>) =
        reader.GetChanges AdaptiveToken.Top |> ignore
        reader.State 
        |> IndexList.toList
        |> should equal l

    // (1,0) (2,1) (3,0) 
    check [0; 1; 0]

    // (1,2) (2,1) (3,2)
    transact (fun () -> odd.Value <- Some 2)
    check [2; 1; 2]
    
    // (1,2) (2,1) (3,2) (4,1)
    transact (fun () -> l.Add 4 |> ignore)
    check [2; 1; 2; 1]
    
    // (1,2) (2,5) (3,2) (4,5)
    transact (fun () -> even.Value <- Some 5)
    check [2; 5; 2; 5]
    
    // (2,5) (3,2) (4,5)
    transact (fun () -> l.RemoveAt 0 |> ignore)
    check [5; 2; 5]
    
    // (2,1) (3,0) (4,1)
    transact (fun () -> even.Value <- Some 1; odd.Value <- Some 0)
    check [1; 0; 1]

    // (2,_) (3,0) (4,_)
    transact (fun () -> even.Value <- None)
    check [0]

    // (2,2) (3,_) (4,2)
    transact (fun () -> even.Value <- Some 2; odd.Value <- None)
    check [2;2]
    
    // (2,1) (4,1)
    transact (fun () -> l.RemoveAt 1 |> ignore; even.Value <- Some 1; odd.Value <- Some 123)
    check [1;1]

[<Test>]
let ``[AList] reduce group``() =
    let list = clist [1;2;3]

    let reduction = AdaptiveReduction.sum()
    let res = AList.reduce reduction list

    res |> AVal.force |> should equal 6

    transact (fun () -> list.Add 4 |> ignore)
    res |> AVal.force |> should equal 10

    transact (fun () -> list.RemoveAt 0 |> ignore)
    res |> AVal.force |> should equal 9

    transact (fun () -> list.Clear())
    res |> AVal.force |> should equal 0
    
[<Test>]
let ``[AList] reduce half group``() =
    let list = clist [1;2;3]

    let reduction = AdaptiveReduction.product()
    let res = AList.reduce reduction list

    res |> AVal.force |> should equal 6

    transact (fun () -> list.Add 4 |> ignore)
    res |> AVal.force |> should equal 24

    transact (fun () -> list.RemoveAt 0 |> ignore)
    res |> AVal.force |> should equal 24

    transact (fun () -> list.Clear())
    res |> AVal.force |> should equal 1
    
    transact (fun () -> list.Add 0 |> ignore)
    res |> AVal.force |> should equal 0
    
    transact (fun () -> list.Add 10 |> ignore)
    res |> AVal.force |> should equal 0
    
    transact (fun () -> list.Add 2 |> ignore)
    res |> AVal.force |> should equal 0
    
    transact (fun () -> list.Add 2 |> ignore)
    res |> AVal.force |> should equal 0

    transact (fun () -> list.RemoveAt 0 |> ignore)
    res |> AVal.force |> should equal 40

[<Test>]
let ``[AList] reduce fold``() =
    let list = clist [1;2;3]

    let reduction = AdaptiveReduction.fold 0 (+)
    let res = AList.reduce reduction list

    res |> AVal.force |> should equal 6

    transact (fun () -> list.Add 4 |> ignore)
    res |> AVal.force |> should equal 10

    transact (fun () -> list.RemoveAt 0 |> ignore)
    res |> AVal.force |> should equal 9

    transact (fun () -> list.Clear())
    res |> AVal.force |> should equal 0


[<Test>]
let ``[AList] reduceBy group``() =
    let list = clist [1;2;3]

    let reduction = AdaptiveReduction.sum()
    let res = AList.reduceBy reduction (fun _ v -> float v) list

    res |> AVal.force |> should equal 6.0

    transact (fun () -> list.Add 4 |> ignore)
    res |> AVal.force |> should equal 10.0

    transact (fun () -> list.RemoveAt 0 |> ignore)
    res |> AVal.force |> should equal 9.0

    transact (fun () -> list.Clear())
    res |> AVal.force |> should equal 0.0
    
[<Test>]
let ``[AList] reduceBy half group``() =
    let list = clist [1;2;3]

    let reduction = AdaptiveReduction.product()
    let res = AList.reduceBy reduction (fun _ v -> float v) list

    res |> AVal.force |> should equal 6.0

    transact (fun () -> list.Add 4 |> ignore)
    res |> AVal.force |> should equal 24.0

    transact (fun () -> list.RemoveAt 0 |> ignore)
    res |> AVal.force |> should equal 24.0

    transact (fun () -> list.Clear())
    res |> AVal.force |> should equal 1.0
    
    transact (fun () -> list.Add 0 |> ignore)
    res |> AVal.force |> should equal 0.0
    
    transact (fun () -> list.Add 10 |> ignore)
    res |> AVal.force |> should equal 0.0
    
    transact (fun () -> list.Add 2 |> ignore)
    res |> AVal.force |> should equal 0.0
    
    transact (fun () -> list.Add 2 |> ignore)
    res |> AVal.force |> should equal 0.0

    transact (fun () -> list.RemoveAt 0 |> ignore)
    res |> AVal.force |> should equal 40.0

[<Test>]
let ``[AList] reduceBy fold``() =
    let list = clist [1;2;3]

    let reduction = AdaptiveReduction.fold 0.0 (+)
    let res = AList.reduceBy reduction (fun _ v -> float v) list

    res |> AVal.force |> should equal 6.0

    transact (fun () -> list.Add 4 |> ignore)
    res |> AVal.force |> should equal 10.0

    transact (fun () -> list.RemoveAt 0 |> ignore)
    res |> AVal.force |> should equal 9.0

    transact (fun () -> list.Clear())
    res |> AVal.force |> should equal 0.0

    
[<Test>]
let ``[AList] reduceByA group``() =
    let list = clist [1;2;3]
    
    let even = cval 1
    let odd = cval 0

    let mapping _ v =
        if v % 2 = 0 then even :> aval<_>
        else odd :> aval<_>

    let reduction = AdaptiveReduction.sum()
    let res = AList.reduceByA reduction mapping list
    
    // (1,0) (2,1) (3,0) = 1
    res |> AVal.force |> should equal 1

    // (1,0) (2,2) (3,0) = 2
    transact (fun () -> even.Value <- 2)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) = 1
    transact (fun () -> even.Value <- 1)
    res |> AVal.force |> should equal 1

    // (1,3) (2,1) (3,3) = 7
    transact (fun () -> odd.Value <- 3)
    res |> AVal.force |> should equal 7

    // (1,1) (2,0) (3,1) = 2
    transact (fun () -> odd.Value <- 1; even.Value <- 0)
    res |> AVal.force |> should equal 2
    
    // (1,1) (2,0) (3,1) (4,0) = 2
    transact (fun () -> list.Append 4 |> ignore)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) (4,1) = 2
    transact (fun () -> odd.Value <- 0; even.Value <- 1)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) (4,1) (5,0) = 2
    transact (fun () -> list.Append 5 |> ignore)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) (4,1) (5,0) (6,1) = 3
    transact (fun () -> list.Append 6 |> ignore)
    res |> AVal.force |> should equal 3

    
    // (2,1) (4,1) (6,1) = 3
    transact (fun () -> 
        list.RemoveAt 4 |> ignore
        list.RemoveAt 2 |> ignore
        list.RemoveAt 0 |> ignore
        odd.Value <- 1
    )
    res |> AVal.force |> should equal 3
    
    // (1,1) (3,1) (5,1) = 3
    transact (fun () -> list.Value <- IndexList.ofList [1;3;5])
    res |> AVal.force |> should equal 3

     
[<Test>]
let ``[AList] reduceByA half group``() =
    let list = clist [1;2;3]
    
    let even = cval 1
    let odd = cval 0

    let mapping _ v =
        if v % 2 = 0 then even :> aval<_>
        else odd :> aval<_>

    let mutable fails = 0
    let reduction = 
        { AdaptiveReduction.sum() with
            sub = fun s v -> 
                if s % 2 = 0 then ValueSome (s - v)
                else fails <- fails + 1; ValueNone
        }

    let res = AList.reduceByA reduction mapping list
    
    // (1,0) (2,1) (3,0) = 1
    res |> AVal.force |> should equal 1

    // (1,0) (2,2) (3,0) = 2
    transact (fun () -> even.Value <- 2)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) = 1
    transact (fun () -> even.Value <- 1)
    res |> AVal.force |> should equal 1

    // (1,3) (2,1) (3,3) = 7
    transact (fun () -> odd.Value <- 3)
    res |> AVal.force |> should equal 7

    // (1,1) (2,0) (3,1) = 2
    transact (fun () -> odd.Value <- 1; even.Value <- 0)
    res |> AVal.force |> should equal 2
    
    // (1,1) (2,0) (3,1) (4,0) = 2
    transact (fun () -> list.Append 4 |> ignore)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) (4,1) = 2
    transact (fun () -> odd.Value <- 0; even.Value <- 1)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) (4,1) (5,0) = 2
    transact (fun () -> list.Append 5 |> ignore)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) (4,1) (5,0) (6,1) = 3
    transact (fun () -> list.Append 6 |> ignore)
    res |> AVal.force |> should equal 3

    
    // (2,1) (4,1) (6,1) = 3
    transact (fun () -> 
        list.RemoveAt 4 |> ignore
        list.RemoveAt 2 |> ignore
        list.RemoveAt 0 |> ignore
        odd.Value <- 1
    )
    res |> AVal.force |> should equal 3
    
    // (1,1) (3,1) (5,1) = 3
    transact (fun () -> list.Value <- IndexList.ofList [1;3;5])
    res |> AVal.force |> should equal 3
   
    fails |> should be (greaterThan 0)
     
[<Test>]
let ``[AList] reduceByA fold``() =
    let list = clist [1;2;3]
    
    let even = cval 1
    let odd = cval 0

    let mapping _ v =
        if v % 2 = 0 then even :> aval<_>
        else odd :> aval<_>

    let reduction = AdaptiveReduction.fold 0 (+)

    let res = AList.reduceByA reduction mapping list
    
    // (1,0) (2,1) (3,0) = 1
    res |> AVal.force |> should equal 1

    // (1,0) (2,2) (3,0) = 2
    transact (fun () -> even.Value <- 2)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) = 1
    transact (fun () -> even.Value <- 1)
    res |> AVal.force |> should equal 1

    // (1,3) (2,1) (3,3) = 7
    transact (fun () -> odd.Value <- 3)
    res |> AVal.force |> should equal 7

    // (1,1) (2,0) (3,1) = 2
    transact (fun () -> odd.Value <- 1; even.Value <- 0)
    res |> AVal.force |> should equal 2
    
    // (1,1) (2,0) (3,1) (4,0) = 2
    transact (fun () -> list.Append 4 |> ignore)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) (4,1) = 2
    transact (fun () -> odd.Value <- 0; even.Value <- 1)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) (4,1) (5,0) = 2
    transact (fun () -> list.Append 5 |> ignore)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) (4,1) (5,0) (6,1) = 3
    transact (fun () -> list.Append 6 |> ignore)
    res |> AVal.force |> should equal 3

    
    // (2,1) (4,1) (6,1) = 3
    transact (fun () -> 
        list.RemoveAt 4 |> ignore
        list.RemoveAt 2 |> ignore
        list.RemoveAt 0 |> ignore
        odd.Value <- 1
    )
    res |> AVal.force |> should equal 3
    
    // (1,1) (3,1) (5,1) = 3
    transact (fun () -> list.Value <- IndexList.ofList [1;3;5])
    res |> AVal.force |> should equal 3


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