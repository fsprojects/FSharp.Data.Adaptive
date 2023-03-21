module AList

open NUnit.Framework
open FsCheck
open FSharp.Data.Adaptive
open FSharp.Data.Traceable
open FsUnit
open FsCheck.NUnit
open FSharp.Data
open Generators
open System

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
  
[<Test>]
let ``[AList] mapUse``() =
    let input = clist [1;2;3;4]
    let refCount = ref 0

    let newDisposable() =
        System.Threading.Interlocked.Increment(&refCount.contents) |> ignore
        { new System.IDisposable with
            member x.Dispose() =
                System.Threading.Interlocked.Decrement(&refCount.contents) |> ignore
                
        }

    let disp, set = input |> AList.mapUse (fun v -> newDisposable())

    refCount.Value |> should equal 0

    let r = set.GetReader()
    r.GetChanges(AdaptiveToken.Top) |> ignore
    r.State.Count |> should equal 4
    refCount.Value |> should equal 4

    transact (fun () -> input.RemoveAt 0 |> ignore)
    r.GetChanges(AdaptiveToken.Top) |> ignore
    r.State.Count |> should equal 3
    refCount.Value |> should equal 3
    
    transact (fun () -> input.Add 7 |> ignore)
    r.GetChanges(AdaptiveToken.Top) |> ignore
    r.State.Count |> should equal 4
    refCount.Value |> should equal 4

    disp.Dispose()
    r.GetChanges(AdaptiveToken.Top) |> ignore
    r.State.Count |> should equal 0
    refCount.Value |> should equal 0
    
    // double free resistance
    disp.Dispose()
    r.GetChanges(AdaptiveToken.Top) |> ignore
    r.State.Count |> should equal 0
    refCount.Value |> should equal 0

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
let ``[AList] reduce empty after lots of operations``() =
    let s = clist<float>()
    let r = AList.sum s
    let rand = System.Random()
    transact (fun () ->
        for i in 1 .. 10000 do
            s.Add(rand.NextDouble()) |> ignore
    )
    r |> AVal.force |> ignore
    
    transact s.Clear
    r |> AVal.force |> should equal 0.0

    transact (fun () ->
        for i in 1 .. 10000 do
            s.Add(rand.NextDouble()) |> ignore
    )

    let element = s.Value |> Seq.item (rand.Next(s.Count))
    transact (fun () -> s.Value <- IndexList.single element)
    
    r |> AVal.force |> should equal element



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
            s |> should equal (Some i)
        else
            let (l, s, r) = MapExt.neighbours i m
            let el = if i > 0 then Some (i-1, i-1) else None
            let er = if i < 1998 then Some (i+1, i+1) else None
            l |> should equal el
            r |> should equal er
            s |> should equal Option<int>.None



        



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


let checkReader (actual: alist<_>) expected = 
    let reader = actual.GetReader()
    fun () ->
        reader.GetChanges AdaptiveToken.Top |> ignore
        let actualValue = reader.State |> IndexList.toList

        let expectedValue = expected()
        if actualValue <> expectedValue then 
            printfn "actual = %A, exp = %A" actualValue expectedValue
            failwith "fail"
        actualValue |> should equal expectedValue

[<Test>]
let ``[AList] init``() =
    let len = cval 1
    
    let actual = AList.init len (fun i -> i + 1)
    let expected () = List.init len.Value (fun i -> i + 1)
    let check = checkReader actual expected

    check ()

    for i in [ 0 ; 4; 2; 1; 6; 6  ] do
        transact (fun () -> len.Value <- i)
        check()

[<Test>]
let ``[AList] range smoke``() =
    let lower = cval 1
    let upper = cval 1
    
    let actual =  AList.range lower upper
    let expected () = [ lower.Value .. upper.Value ]
    let check = checkReader actual expected

    check ()
    transact (fun () -> 
        lower.Value <- 0
        upper.Value <- 4
    )
    check()

let inline AListRangeSystematic(low, high) =
    let range = [ low .. high ]
    for pl in range do
        for pu in range do 
           for l in range do 
               for u in range do 
                printfn "checking change from (%A .. %A) to (%A .. %A)" pl pu l u
                let lower = cval pl
                let upper = cval pu
    
                let actual =  AList.range lower upper
                let expected () = [ lower.Value .. upper.Value ]
                let check = checkReader actual expected

                check ()
                transact (fun () -> 
                    lower.Value <- l
                    upper.Value <- u
                )
                check()
    printfn "checked %d cases" (range.Length * range.Length * range.Length * range.Length)

[<Test>]
let ``[AList] range systematic int32``() =
    AListRangeSystematic(0, 4)

[<Test>]
let ``[AList] range systematic int64``() =
    AListRangeSystematic(0L, 4L)

[<Test>]
let ``[AList] subA``() =
    let l = clist [1..100]

    let o = cval 0
    let c = cval 2


    let full = l |> AList.map ((+) 1)
    let part = full |> AList.subA o c

    let test(r : IIndexListReader<int>) =
        r.GetChanges(AdaptiveToken.Top) |> ignore
        let test = r.State |> IndexList.toList
        let ref = full |> AList.force |> IndexList.toList
        if test <> ref.[o.Value .. o.Value + c.Value - 1] then
            failwithf "is:\n%0A\nshould:\n%0A" test ref.[o.Value .. o.Value + c.Value - 1]


    let r = part.GetReader()
    test r
    
    // change offset
    transact (fun () -> o.Value <- 10)
    test r

    // change count
    transact (fun () -> c.Value <- 5)
    test r

    // remove in slice
    transact (fun () -> l.RemoveAt 11 |> ignore)
    test r
    
    // update in sclice
    transact (fun () -> l[11] <- 1111)
    test r

    // remove before slice
    transact (fun () -> l.RemoveAt 0 |> ignore)
    test r

    // insert before slice
    transact (fun () -> l.InsertAt(1, 4321) |> ignore)
    test r

    // update before slice
    transact (fun () -> l.[0] <- 1337)
    test r
    
    // remove after slice
    transact (fun () -> l.RemoveAt 70 |> ignore)
    test r
    
    // insert after slice
    transact (fun () -> l.InsertAt(60, 4321) |> ignore)
    test r

    // update after slice
    transact (fun () -> l.[61] <- 7331)
    test r
    
    // change offset
    transact (fun () -> o.Value <- 3)
    test r

    // insert in slice
    transact (fun () -> l.InsertAt(4, 1234) |> ignore)
    test r

    // clear list
    transact (fun () -> l.Clear())
    test r

    transact (fun () -> c.Value <- 3)
    test r

    // insert before
    transact (fun () -> l.AddRange [9;8;7])
    test r

    // insert less elements
    transact (fun () -> l.AddRange [6;5])
    test r

    // insert more elements
    transact (fun () -> l.AddRange [4;3])
    test r

[<Test>]
let ``[AList] skipA``() =
    let l = clist [1..100]


    let o = cval 0
    let full = l |> AList.map ((+) 1)
    let part = full |> AList.skipA o

    let test(r : IIndexListReader<int>) =
        r.GetChanges(AdaptiveToken.Top) |> ignore
        let test = r.State |> IndexList.toList
        let ref = full |> AList.force |> IndexList.toList
        if test <> ref.[o.Value ..] then
            failwithf "is:\n%0A\nshould:\n%0A" test ref.[o.Value ..]

    let r = part.GetReader()
    test r
 
    // change offset
    transact (fun () -> o.Value <- 10)
    test r

    // remove in slice
    transact (fun () -> l.RemoveAt 11 |> ignore)
    test r
    
    // remove before slice
    transact (fun () -> l.RemoveAt 0 |> ignore)
    test r
    
    // remove in slice
    transact (fun () -> l.RemoveAt 70 |> ignore)
    test r

    // insert before slice
    transact (fun () -> l.InsertAt(4, 1234) |> ignore)
    test r

    // insert in slice
    transact (fun () -> l.InsertAt(20, 4321) |> ignore)
    test r

    // update in slice
    transact (fun () -> l.[21] <- 1337)
    test r

    // update before slice
    transact (fun () -> l.[4] <- 1337)
    test r

    // change offset
    transact (fun () -> o.Value <- 3)
    test r

[<Test>]
let ``[AList] sub random``() =
    let input = clist ["a"; "b"; "c"; "d"; "e"]


    let range =
        input 
        |> AList.sortDescending
        |> AList.sub 1 3
    
    let reader = range.GetReader()

    let check() =

        let rec skip (n : int) (l : list<'a>) =
            if n <= 0 then l
            else 
                match l with
                | [] -> []
                | _ :: r -> skip (n - 1) r


        reader.GetChanges AdaptiveToken.Top |> ignore
        let real = reader.State |> IndexList.toList
        let test = input.Value |> IndexList.toList |> List.sortDescending |> skip 1 |> List.truncate 3

        if real <> test then
            failwithf "bad: %0A vs %0A" real test
        else
            printfn "OK: %0A" test
    check()

    transact (fun () ->
        input.Prepend "x" |> ignore
        input.Prepend "y" |> ignore
        input.Prepend "z" |> ignore
        input.Prepend "w" |> ignore
        input.Prepend "r" |> ignore
    )
    check()

    transact (fun () ->
        input.Clear()
    )
    check()

    transact (fun () ->
        input.UpdateTo ["hans"; "sepp"; "hugo"; "franz"] |> ignore
    )
    check()

    transact (fun () ->
        input.Append "zitha" |> ignore
    )
    check()

    transact (fun () ->
        input.RemoveAt 0 |> ignore
        input.RemoveAt 0 |> ignore
        input.RemoveAt 0 |> ignore
        input.RemoveAt (input.Count - 1) |> ignore
    )
    check()

    transact (fun () ->
        input.Clear()
    )
    check()

    let rand = System.Random()

    for i in 1 .. 10000 do
        transact (fun () ->
            let values =
                List.init (rand.Next 5) (fun _ -> rand.Next() |> string)

            match rand.Next 3 with
            | 0 when not (List.isEmpty values) ->
                match rand.Next 3 with
                | 0 ->
                    printf "append "
                    for v in values do input.Append v |> ignore
                | 1 -> 
                    printf "prepend "
                    for v in values do input.Prepend v |> ignore
                | _ ->
                    printf "insert "
                    for v in values do input.InsertAt (rand.Next (input.Count + 1), v) |> ignore
            | 1 when input.Count > 0 ->
                printf "remove "
                let c = rand.Next(input.Count) + 1
                for i in 1 .. c do
                    input.RemoveAt (rand.Next input.Count) |> ignore
            | _ ->
                printf "update "
                input.UpdateTo values |> ignore
        )
        check()





    
[<Test>]
let ``[AList] filterA``() =

    let a = Index.after Index.zero
    let b = Index.after a
    let c = Index.after b
    let d = Index.after c
    let e = Index.after d

    let map = clist (IndexList.ofSeqIndexed [a, 1; b, 2; c, 3; d, 4; e, 5])
    let keys = cset [a; c; e]

    let res =
        map |> AList.filterAi (fun k _ -> keys |> ASet.contains k)

    let r = res.GetReader()

    r.GetChanges AdaptiveToken.Top |> ignore
    r.State |> should equal (IndexList.ofSeqIndexed [a, 1; c, 3; e, 5])

    transact (fun () ->
        map.Value <- map.Value |> IndexList.map (fun v -> v * 2)
    )
    
    r.GetChanges AdaptiveToken.Top |> ignore
    r.State |> should equal (IndexList.ofSeqIndexed [a, 2; c, 6; e, 10])

    transact (fun () ->
        keys.Value <- HashSet.ofList [a; c; d; e]
    )
    
    r.GetChanges AdaptiveToken.Top |> ignore
    r.State |> should equal (IndexList.ofSeqIndexed [a, 2; c, 6; d, 8; e, 10])
    
    
[<Test>]
let ``[AList] mapA inner change``() =

    let a = Index.after Index.zero
    let b = Index.after a
    let c = Index.after b
    let d = Index.after c
    let e = Index.after d

    let map = clist (IndexList.ofSeqIndexed [a, 1; b, 2; c, 3; d, 4; e, 5])
    let keys = cset [a; c; e]

    let res =
        map |> AList.mapAi (fun k v -> keys |> ASet.contains k |> AVal.map (function true -> v | false -> -1))

    let r = res.GetReader()

    r.GetChanges AdaptiveToken.Top |> ignore
    r.State |> should equal (IndexList.ofSeqIndexed [a, 1; b, -1; c, 3; d, -1; e, 5])

    transact (fun () ->
        map.Value <- map.Value |> IndexList.map (fun v -> v * 2)
    )
    
    r.GetChanges AdaptiveToken.Top |> ignore
    r.State |> should equal (IndexList.ofSeqIndexed [a, 2; b, -1; c, 6; d, -1; e, 10])

    transact (fun () ->
        keys.Value <- HashSet.ofList [a; c; d; e]
    )
    
    r.GetChanges AdaptiveToken.Top |> ignore
    r.State |> should equal (IndexList.ofSeqIndexed [a, 2; b, -1; c, 6; d, 8; e, 10])

    
[<Test>]
let ``[AList] batchMap``() =

    let file1 = "File1.fs"
    let file1Cval = cval 1
    let file1DepCval = cval DateTime.UtcNow
    let file2 = "File2.fs"
    let file2Cval = cval 2
    let file2DepCval = cval DateTime.UtcNow
    let file3 = "File3.fs"
    let file3Cval = cval 3
    let file3DepCval = cval DateTime.UtcNow
    
    let file4 = "File4.fs"
    let file4Cval = cval 3
    let file4DepCval = cval DateTime.UtcNow

    let dependencies = Map [file1, file1DepCval; file2, file2DepCval; file3, file3DepCval; file4, file4DepCval]

    let filesCmap = 
        clist
            [
                file1,file1Cval
                file2,file2Cval
                file3,file3Cval
                // file4 added later
            ] 
    let files =
        filesCmap
        |> AList.mapA(fun (k, v) -> v |> AVal.map(fun v -> k,v))

    let mutable lastBatch = Unchecked.defaultof<_>
    let res =
        files
        |> AList.batchMap(fun d ->
            lastBatch <- d
            d
            |> IndexList.mapi(fun k (file,_) ->
                printfn "k -> %A" k
                (dependencies.[file] :> aval<_>
            )
        ))
    let firstResult = res |> AList.force
    lastBatch |> should haveCount 3

    transact(fun () -> file1Cval.Value <- file1Cval.Value + 1)

    let secondResult = res |> AList.force
    lastBatch |> should haveCount 1
    
    firstResult.[0] |> should equal secondResult.[0]
    firstResult.[1] |> should equal secondResult.[1]
    firstResult.[2] |> should equal secondResult.[2]

    transact(fun () -> file1DepCval.Value <- DateTime.UtcNow)

    let thirdResult = res |> AList.force
    lastBatch |> should haveCount 1
    
    secondResult.[0] |> should not' (equal thirdResult.[0])
    secondResult.[1] |> should equal thirdResult.[1]
    secondResult.[2] |> should equal thirdResult.[2]

    transact(fun () -> 
        file1DepCval.Value <- DateTime.UtcNow
        file3Cval.Value <- file3Cval.Value + 1
    )

    let fourthResult = res |> AList.force
    lastBatch |> should haveCount 2
    
    thirdResult.[0] |> should not' (equal fourthResult.[0])
    thirdResult.[1] |> should equal fourthResult.[1]
    thirdResult.[2] |> should equal fourthResult.[2]

    transact(fun () -> 
        file1Cval.Value <- file1Cval.Value + 1
        file2DepCval.Value <- DateTime.UtcNow
        filesCmap.Add(file4, file4Cval) |> ignore
    )

    let fifthResult = res |> AList.force
    lastBatch |> should haveCount 3

    fourthResult.[0] |> should equal fifthResult.[0]
    fourthResult.[1] |> should not' (equal fifthResult.[1])
    fourthResult.[2] |> should equal fifthResult.[2]
    fifthResult.[3] |> should equal file4DepCval.Value

