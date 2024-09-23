module ASet

open NUnit.Framework
open FsCheck
open FSharp.Data.Adaptive
open FSharp.Data.Traceable
open FsUnit
open FsCheck.NUnit

type Record<'T> = { value : 'T }

//[<AutoOpen>]
//module Helpers =
//    let check (r : IHashSetReader<'T>) =
//        let a = r.Adaptive.GetChanges FSharp.Data.Adaptive.AdaptiveToken.Top
//        let r = r.Reference.GetChanges FSharp.Data.Adaptive.Reference.AdaptiveToken.Top
//        a |> should setequal r
//        r

let emptyDelta = HashSetDelta.empty<int>

open FSharp.Data
open Generators
[<Property(MaxTest = 500, Arbitrary = [| typeof<Generators.AdaptiveGenerators> |]); Timeout(60000)>]
let ``[ASet] reference impl``() ({ sreal = real; sref = ref; sexpression = str; schanges = changes } : VSet<int>) =
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

        let delta = HashSet.computeDelta vReal vRef |> HashSetDelta.toList
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
                    yield! beforeChangeStr.Split([|"\r\n"|], System.StringSplitOptions.None) |> Array.map Generators.indent
                    
                    yield "CURRENT"
                    yield! (str true).Split([|"\r\n"|], System.StringSplitOptions.None) |> Array.map Generators.indent

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
                    if not (DefaultEquality.equals v lastValue) then
                        
                        printfn "  change %d => %A" effective v
                        lastValue <- v
                    

     
                    effective <- effective + 1
        }

    Gen.eval 15 (Random.newSeed()) run

[<Test>]
let ``[ASet] mapUse``() =
    let input = cset [1;2;3;4]
    let refCount = ref 0

    let newDisposable() =
        System.Threading.Interlocked.Increment(&refCount.contents) |> ignore
        { new System.IDisposable with
            member x.Dispose() =
                System.Threading.Interlocked.Decrement(&refCount.contents) |> ignore
                
        }

    let disp, set = input |> ASet.mapUse (fun v -> newDisposable())

    !refCount |> should equal 0

    let r = set.GetReader()
    r.GetChanges(AdaptiveToken.Top) |> ignore
    r.State.Count |> should equal 4
    !refCount |> should equal 4

    transact (fun () -> input.Remove 1 |> ignore)
    r.GetChanges(AdaptiveToken.Top) |> ignore
    r.State.Count |> should equal 3
    !refCount |> should equal 3
    
    transact (fun () -> input.Add 7 |> ignore)
    r.GetChanges(AdaptiveToken.Top) |> ignore
    r.State.Count |> should equal 4
    !refCount |> should equal 4

    disp.Dispose()
    r.GetChanges(AdaptiveToken.Top) |> ignore
    r.State.Count |> should equal 0
    !refCount |> should equal 0
    
    // double free resistance
    disp.Dispose()
    r.GetChanges(AdaptiveToken.Top) |> ignore
    r.State.Count |> should equal 0
    !refCount |> should equal 0

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

[<Test>]
let ``[CSet] intersectWith`` () =
    let s = cset [1;2;3;4]

    transact (fun () ->
        s.IntersectWith [2;3;5]
    )
    
    s.Value |> should equal (HashSet.OfList [2;3])


[<Test>]
let ``[ASet] reduce group``() =
    let set = cset [1;2;3]

    let reduction = AdaptiveReduction.sum()
    let res = ASet.reduce reduction set

    res |> AVal.force |> should equal 6

    transact (fun () -> set.Add 4 |> ignore)
    res |> AVal.force |> should equal 10

    transact (fun () -> set.Remove 1 |> ignore)
    res |> AVal.force |> should equal 9

    transact (fun () -> set.Clear())
    res |> AVal.force |> should equal 0
    
[<Test>]
let ``[ASet] reduce half group``() =
    let list = cset [1;2;3]

    let reduction = AdaptiveReduction.product()
    let res = ASet.reduce reduction list

    res |> AVal.force |> should equal 6

    transact (fun () -> list.Add 4 |> ignore)
    res |> AVal.force |> should equal 24

    transact (fun () -> list.Remove 1 |> ignore)
    res |> AVal.force |> should equal 24

    transact (fun () -> list.Clear())
    res |> AVal.force |> should equal 1
    
    transact (fun () -> list.Add 0 |> ignore)
    res |> AVal.force |> should equal 0
    
    transact (fun () -> list.Add 10 |> ignore)
    res |> AVal.force |> should equal 0
    
    transact (fun () -> list.Add 2 |> ignore)
    res |> AVal.force |> should equal 0
    
    transact (fun () -> list.Remove 0 |> ignore)
    res |> AVal.force |> should equal 20

    
[<Test>]
let ``[ASet] reduce empty after lots of operations``() =
    let s = cset<float>()
    let r = ASet.sum s
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
    transact (fun () -> s.Value <- HashSet.single element)
    
    r |> AVal.force |> should equal element



[<Test>]
let ``[ASet] reduce fold``() =
    let list = cset [1;2;3]

    let reduction = AdaptiveReduction.fold 0 (+)
    let res = ASet.reduce reduction list

    res |> AVal.force |> should equal 6

    transact (fun () -> list.Add 4 |> ignore)
    res |> AVal.force |> should equal 10

    transact (fun () -> list.Remove 1 |> ignore)
    res |> AVal.force |> should equal 9

    transact (fun () -> list.Clear())
    res |> AVal.force |> should equal 0


[<Test>]
let ``[ASet] reduceBy group``() =
    let list = cset [1;2;3]

    let reduction = AdaptiveReduction.sum()
    let res = ASet.reduceBy reduction float list

    res |> AVal.force |> should equal 6.0

    transact (fun () -> list.Add 4 |> ignore)
    res |> AVal.force |> should equal 10.0

    transact (fun () -> list.Remove 1 |> ignore)
    res |> AVal.force |> should equal 9.0

    transact (fun () -> list.Clear())
    res |> AVal.force |> should equal 0.0
    
[<Test>]
let ``[ASet] reduceBy half group``() =
    let list = cset [1;2;3]

    let reduction = AdaptiveReduction.product()
    let res = ASet.reduceBy reduction float list

    res |> AVal.force |> should equal 6.0

    transact (fun () -> list.Add 4 |> ignore)
    res |> AVal.force |> should equal 24.0

    transact (fun () -> list.Remove 1 |> ignore)
    res |> AVal.force |> should equal 24.0

    transact (fun () -> list.Clear())
    res |> AVal.force |> should equal 1.0
    
    transact (fun () -> list.Add 0 |> ignore)
    res |> AVal.force |> should equal 0.0
    
    transact (fun () -> list.Add 10 |> ignore)
    res |> AVal.force |> should equal 0.0
    
    transact (fun () -> list.Add 2 |> ignore)
    res |> AVal.force |> should equal 0.0
    
    transact (fun () -> list.Remove 0 |> ignore)
    res |> AVal.force |> should equal 20.0

[<Test>]
let ``[ASet] reduceBy fold``() =
    let list = cset [1;2;3]

    let reduction = AdaptiveReduction.fold 0.0 (+)
    let res = ASet.reduceBy reduction float list

    res |> AVal.force |> should equal 6.0

    transact (fun () -> list.Add 4 |> ignore)
    res |> AVal.force |> should equal 10.0

    transact (fun () -> list.Remove 1 |> ignore)
    res |> AVal.force |> should equal 9.0

    transact (fun () -> list.Clear())
    res |> AVal.force |> should equal 0.0

    
[<Test>]
let ``[ASet] reduceByA group``() =
    let list = cset [1;2;3]
    
    let even = cval 1
    let odd = cval 0

    let mapping v =
        if v % 2 = 0 then even :> aval<_>
        else odd :> aval<_>

    let reduction = AdaptiveReduction.sum()
    let res = ASet.reduceByA reduction mapping list
    
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
    transact (fun () -> list.Add 4 |> ignore)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) (4,1) = 2
    transact (fun () -> odd.Value <- 0; even.Value <- 1)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) (4,1) (5,0) = 2
    transact (fun () -> list.Add 5 |> ignore)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) (4,1) (5,0) (6,1) = 3
    transact (fun () -> list.Add 6 |> ignore)
    res |> AVal.force |> should equal 3

    
    // (2,1) (4,1) (6,1) = 3
    transact (fun () -> 
        list.Remove 5 |> ignore
        list.Remove 3 |> ignore
        list.Remove 1 |> ignore
        odd.Value <- 1
    )
    res |> AVal.force |> should equal 3
    
    // (1,1) (3,1) (5,1) = 3
    transact (fun () -> list.Value <- HashSet.ofList [1;3;5])
    res |> AVal.force |> should equal 3

     
[<Test>]
let ``[ASet] reduceByA half group``() =
    let list = cset [1;2;3]
    
    let even = cval 1
    let odd = cval 0

    let mapping v =
        if v % 2 = 0 then even :> aval<_>
        else odd :> aval<_>

    let mutable fails = 0
    let reduction = 
        { AdaptiveReduction.sum() with
            sub = fun s v -> 
                if s % 2 = 0 then ValueSome (s - v)
                else fails <- fails + 1; ValueNone
        }

    let res = ASet.reduceByA reduction mapping list
    
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
    transact (fun () -> list.Add 4 |> ignore)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) (4,1) = 2
    transact (fun () -> odd.Value <- 0; even.Value <- 1)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) (4,1) (5,0) = 2
    transact (fun () -> list.Add 5 |> ignore)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) (4,1) (5,0) (6,1) = 3
    transact (fun () -> list.Add 6 |> ignore)
    res |> AVal.force |> should equal 3

    
    // (2,1) (4,1) (6,1) = 3
    transact (fun () -> 
        list.Remove 1 |> ignore
        list.Remove 3 |> ignore
        list.Remove 5 |> ignore
        odd.Value <- 1
    )
    res |> AVal.force |> should equal 3
    
    // (1,1) (3,1) (5,1) = 3
    transact (fun () -> list.Value <- HashSet.ofList [1;3;5])
    res |> AVal.force |> should equal 3
   
    fails |> should be (greaterThan 0)
     
[<Test>]
let ``[ASet] reduceByA fold``() =
    let list = cset [1;2;3]
    
    let even = cval 1
    let odd = cval 0

    let mapping v =
        if v % 2 = 0 then even :> aval<_>
        else odd :> aval<_>

    let reduction = AdaptiveReduction.fold 0 (+)

    let res = ASet.reduceByA reduction mapping list
    
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
    transact (fun () -> list.Add 4 |> ignore)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) (4,1) = 2
    transact (fun () -> odd.Value <- 0; even.Value <- 1)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) (4,1) (5,0) = 2
    transact (fun () -> list.Add 5 |> ignore)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) (4,1) (5,0) (6,1) = 3
    transact (fun () -> list.Add 6 |> ignore)
    res |> AVal.force |> should equal 3

    
    // (2,1) (4,1) (6,1) = 3
    transact (fun () -> 
        list.Remove 1 |> ignore
        list.Remove 3 |> ignore
        list.Remove 5 |> ignore
        odd.Value <- 1
    )
    res |> AVal.force |> should equal 3
    
    // (1,1) (3,1) (5,1) = 3
    transact (fun () -> list.Value <- HashSet.ofList [1;3;5])
    res |> AVal.force |> should equal 3



let checkReader (actual: aset<_>) expected = 
    let reader = actual.GetReader()
    fun () ->
        reader.GetChanges AdaptiveToken.Top |> ignore
        let actualValue = reader.State |> CountingHashSet.toList

        let expectedValue = expected()
        if actualValue <> expectedValue then 
            printfn "actual = %A, exp = %A" actualValue expectedValue
            failwith "fail"
        actualValue |> should equal expectedValue

[<Test>]
let ``[ASet] range smoke``() =
    let lower = cval 1
    let upper = cval 1
    
    let actual =  ASet.range lower upper
    let expected () = [ lower.Value .. upper.Value ]
    let check = checkReader actual expected

    check ()
    transact (fun () -> 
        lower.Value <- 0
        upper.Value <- 4
    )
    check()

let inline ASetRangeSystematic(low, high) =
    let range = [ low .. high ]
    for pl in range do
        for pu in range do 
           for l in range do 
               for u in range do 
                printfn "checking change from (%A .. %A) to (%A .. %A)" pl pu l u
                let lower = cval pl
                let upper = cval pu
    
                let actual =  ASet.range lower upper
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
let ``[ASet] range systematic int32``() =
    ASetRangeSystematic(0, 4)

[<Test>]
let ``[ASet] range systematic int64``() =
    ASetRangeSystematic(0L, 4L)

[<Test>]
let ``[ASet] content bind``() =
    let set = cset<int>()
    let res = (set :> aset<_>).Content |> ASet.bind (fun x -> ASet.ofHashSet (x.Map(fun v -> v * 2)))

    for i in 1..100 do
        transact(fun () -> set.Add(i) |> ignore)
        let cnt = (res |> ASet.force).Count
        printfn "InputCount=%d OutputCount=%d" set.Count cnt
        cnt |> should equal set.Count
    

[<Test>]
let ``[ASet] mapA/flattenA/chooseA async``() =
    
    let set = cset<cval<int>>()
    
    let out1 = set |> ASet.mapA (fun x -> x)
    let out2 = set |> ASet.map(fun x -> x :> IAdaptiveValue<_>) |> ASet.flattenA
    let out3 = set |> ASet.chooseA (fun x -> x |> AVal.map (fun v -> if (v % 2) = 0 then Some v else None))

    let sw = System.Diagnostics.Stopwatch.StartNew()

    System.Threading.Thread(System.Threading.ThreadStart(fun x -> 
                                        while sw.ElapsedMilliseconds < 5000 do
                                            let res = out1 |> ASet.force
                                            //printfn "set count: %d   output count: %d" set.Count res.Count
                                            //System.Threading.Thread.Sleep(1)
                                            ()
        )).Start()

    System.Threading.Thread(System.Threading.ThreadStart(fun x -> 
                                        while sw.ElapsedMilliseconds < 5000 do
                                            let res = out2 |> ASet.force
                                            //printfn "set count: %d   output count: %d" set.Count res.Count
                                            //System.Threading.Thread.Sleep(1)
                                            ()
        )).Start()

    System.Threading.Thread(System.Threading.ThreadStart(fun x -> 
                                        while sw.ElapsedMilliseconds < 5000 do
                                            let res = out3 |> ASet.force
                                            //printfn "set count: %d   output count: %d" set.Count res.Count
                                            //System.Threading.Thread.Sleep(1)
                                            ()
        )).Start()

    let rnd = System.Random(2)
    while sw.ElapsedMilliseconds < 5000 do
        //System.Threading.Thread.Sleep(1)

        transact(fun () ->

            let rndAction = rnd.Next(10)
                        
            if rndAction = 0 then // add
                //printfn "add"
                let value = rnd.Next() % 100
                let addItem = cval<int>(value)
                set.Add(addItem) |> ignore

            elif rndAction = 1 then // rem
                if set.Count > 10 then
                    //printfn "rem"
                    let remIndex = rnd.Next(set.Count)
                    let remItem = set.Value.ToArray()[remIndex]
                    set.Remove(remItem) |> ignore

            elif rndAction = 2 then // rem + change
                if set.Count > 1 then
                    // !! potential crash !!
                    //printfn "change + rem"
                    let ind = rnd.Next(set.Count)
                    let item = set.Value.ToArray()[ind]
                    // NOTE: order of value change and remove does not matter, both variants cause the exception
                    item.Value <- rnd.Next() % 100 
                    set.Remove(item) |> ignore
                    
            elif set.Count > 0 then // change
                //printfn "change"
                let changeIndex = rnd.Next(set.Count)
                let changeItem = set.Value.ToArray()[changeIndex]
                changeItem.Value <- rnd.Next() % 100
            )

        //let res = out |> ASet.force

        ()

    ()


[<Test>]
let ``[ASet] union constant``() =
    let constSet = ASet.ofList [1; 2; 3]
    let changeSet = cset [4;5;6]

    let union1 = ASet.union constSet changeSet
    let union2 = ASet.union changeSet constSet

    let refSet = [1; 2; 3; 4; 5; 6]
    union1 |> ASet.force |> should setequal refSet
    union2 |> ASet.force |> should setequal refSet

    transact(fun () -> changeSet.Add(1) |> ignore)

    union1 |> ASet.force |> should setequal refSet
    union2 |> ASet.force |> should setequal refSet

    transact(fun () -> changeSet.Remove(1) |> ignore)

    union1 |> ASet.force |> should setequal refSet
    union2 |> ASet.force |> should setequal refSet

    transact(fun () -> changeSet.Remove(5) |> ignore)

    let refSet = [1; 2; 3; 4; 6]
    union1 |> ASet.force |> should setequal refSet
    union2 |> ASet.force |> should setequal refSet

    let constSet = ASet.ofList [1; 2; 3]
    let changeSet = cset [3;4;5]

    let union1 = ASet.union constSet changeSet
    let union2 = ASet.union changeSet constSet

    let refSet = [1; 2; 3; 4; 5]
    union1 |> ASet.force |> should setequal refSet
    union2 |> ASet.force |> should setequal refSet

    transact(fun () -> changeSet.Remove(5) |> ignore)

    let refSet = [1; 2; 3; 4]
    union1 |> ASet.force |> should setequal refSet
    union2 |> ASet.force |> should setequal refSet


[<Test>]
let ``[ASet] filterA``() =
    let takeEven = AVal.init true
    let takeOdd = AVal.init true
    let set = ASet.ofArray (Array.init 5 (fun i -> i))

    let filtered = set |> ASet.filterA (fun i -> if (i % 2) = 0 then takeEven else takeOdd)

    filtered |> ASet.force |> should setequal [0; 1; 2; 3; 4]

    transact(fun () -> takeEven.Value <- false)

    filtered |> ASet.force |> should setequal [1; 3]

    transact(fun () -> takeOdd.Value <- false)

    filtered |> ASet.force |> HashSet.count |> should equal 0

    transact(fun () -> 
        takeOdd.Value <- true
        takeEven.Value <- true
        )

    filtered |> ASet.force |> should setequal [0; 1; 2; 3; 4]