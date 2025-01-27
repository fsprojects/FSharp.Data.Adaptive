module AMap

open NUnit.Framework
open FsCheck
open FSharp.Data.Adaptive
open FSharp.Data.Traceable
open FsUnit
open FsCheck.NUnit
open FSharp.Data
open Generators
open System.IO
open System

[<Property(MaxTest = 500, Arbitrary = [| typeof<Generators.AdaptiveGenerators> |]); Timeout(60000)>]
let ``[AMap] reference impl``() ({ mreal = real; mref = ref; mexpression = str; mchanges = changes } : VMap<int, int>) =
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

        let delta = HashMap.computeDelta vReal vRef |> Seq.toList
        match delta with
        | [] ->
            if vReal <> vRef then failwith "wrong equals"
            if vReal.GetHashCode() <> vRef.GetHashCode() then failwith "wrong hash"
            vRef
        | delta ->
            let real = vReal |> Seq.sort |> Seq.map string |> String.concat "; " |> sprintf "[%s]"
            let ref = vRef |> Seq.sort |> Seq.map string |> String.concat "; " |> sprintf "[%s]"
            let delta = delta |> Seq.sortBy (fun (k, _) -> k) |> Seq.map string |> String.concat "; "  |> sprintf "[%s]"
            
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
let ``[AMap] mapUse``() =
    let input = cmap [1,0;2,0;3,0;4,0]
    let refCount = ref 0

    let newDisposable() =
        System.Threading.Interlocked.Increment(&refCount.contents) |> ignore
        { new System.IDisposable with
            member x.Dispose() =
                System.Threading.Interlocked.Decrement(&refCount.contents) |> ignore
                
        }

    let disp, set = input |> AMap.mapUse (fun _ _ -> newDisposable())

    !refCount |> should equal 0

    let r = set.GetReader()
    r.GetChanges(AdaptiveToken.Top) |> ignore
    r.State.Count |> should equal 4
    !refCount |> should equal 4

    transact (fun () -> input.Remove 1 |> ignore)
    r.GetChanges(AdaptiveToken.Top) |> ignore
    r.State.Count |> should equal 3
    !refCount |> should equal 3
    
    transact (fun () -> input.Add(7, 0) |> ignore)
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
let ``[AMap] toASet``() =
    let c = cmap (List.init 100 (fun i -> i,i))

    let test = c |> AMap.toASet |> ASet.sortBy snd

    let r = test.GetReader()

    let check (r : IIndexListReader<int * int>) =
        r.GetChanges AdaptiveToken.Top |> ignore
        let test = r.State |> IndexList.toList
        let ref = c.Value |> HashMap.toList |> List.sortBy snd
        test |> should equal ref

    check r

    transact (fun () -> c.[30] <- 1000)
    check r
    

    transact (fun () -> c.Remove 10 |> ignore)
    check r
    
    transact (fun () -> c.[14] <- 10)
    check r






[<Test>]
let ``[AMap] reduce group``() =
    let set = cmap [1,1;2,2;3,3]

    let reduction = AdaptiveReduction.sum()
    let res = AMap.reduce reduction set

    res |> AVal.force |> should equal 6

    transact (fun () -> set.Add(4, 4) |> ignore)
    res |> AVal.force |> should equal 10

    transact (fun () -> set.Remove 1 |> ignore)
    res |> AVal.force |> should equal 9
    
    transact (fun () -> set.[2] <- 3)
    res |> AVal.force |> should equal 10

    transact (fun () -> set.Clear())
    res |> AVal.force |> should equal 0
    
[<Test>]
let ``[AMap] reduce half group``() =
    let list = cmap [1,1;2,2;3,3]

    let reduction = AdaptiveReduction.product()
    let res = AMap.reduce reduction list

    res |> AVal.force |> should equal 6

    transact (fun () -> list.Add(4,4) |> ignore)
    res |> AVal.force |> should equal 24

    transact (fun () -> list.Remove 1 |> ignore)
    res |> AVal.force |> should equal 24

    transact (fun () -> list.Clear())
    res |> AVal.force |> should equal 1
    
    transact (fun () -> list.Add(0,0) |> ignore)
    res |> AVal.force |> should equal 0
    
    transact (fun () -> list.Add(10,10) |> ignore)
    res |> AVal.force |> should equal 0
    
    transact (fun () -> list.Add(2,2) |> ignore)
    res |> AVal.force |> should equal 0
    
    transact (fun () -> list.Remove 0 |> ignore)
    res |> AVal.force |> should equal 20

    transact (fun () -> list.[10] <- 20)
    res |> AVal.force |> should equal 40


    
[<Test>]
let ``[AMap] reduce empty after lots of operations``() =
    let s = cmap<float, float>()
    let r = AMap.reduce (AdaptiveReduction.sum()) s
    let rand = System.Random()
    transact (fun () ->
        for i in 1 .. 10000 do
            let v = rand.NextDouble()
            s.Add(v,v) |> ignore
    )
    r |> AVal.force |> ignore
    
    transact s.Clear
    r |> AVal.force |> should equal 0.0

    transact (fun () ->
        for i in 1 .. 10000 do
            let v = rand.NextDouble()
            s.Add(v,v) |> ignore
    )

    let (k,v) = s.Value |> Seq.item (rand.Next(s.Count))
    transact (fun () -> s.Value <- HashMap.single k v)
    
    r |> AVal.force |> should equal v



[<Test>]
let ``[AMap] reduce fold``() =
    let list = cmap [1,1;2,2;3,3]

    let reduction = AdaptiveReduction.fold 0 (+)
    let res = AMap.reduce reduction list

    res |> AVal.force |> should equal 6

    transact (fun () -> list.Add(4,4) |> ignore)
    res |> AVal.force |> should equal 10

    transact (fun () -> list.Remove 1 |> ignore)
    res |> AVal.force |> should equal 9
    
    transact (fun () -> list.[4] <- 5)
    res |> AVal.force |> should equal 10

    transact (fun () -> list.Clear())
    res |> AVal.force |> should equal 0


[<Test>]
let ``[AMap] reduceBy group``() =
    let list = cmap [1,1;2,2;3,3]

    let reduction = AdaptiveReduction.sum()
    let res = AMap.reduceBy reduction (fun _ -> float) list

    res |> AVal.force |> should equal 6.0

    transact (fun () -> list.Add(4, 4) |> ignore)
    res |> AVal.force |> should equal 10.0

    transact (fun () -> list.Remove 1 |> ignore)
    res |> AVal.force |> should equal 9.0

    transact (fun () -> list.[2] <- 3)
    res |> AVal.force |> should equal 10.0

    transact (fun () -> list.Clear())
    res |> AVal.force |> should equal 0.0
    
[<Test>]
let ``[AMap] reduceBy half group``() =
    let list = cmap [1,1;2,2;3,3]

    let reduction = AdaptiveReduction.product()
    let res = AMap.reduceBy reduction (fun _ -> float) list

    res |> AVal.force |> should equal 6.0

    transact (fun () -> list.Add(4,4) |> ignore)
    res |> AVal.force |> should equal 24.0

    transact (fun () -> list.Remove 1 |> ignore)
    res |> AVal.force |> should equal 24.0

    transact (fun () -> list.Clear())
    res |> AVal.force |> should equal 1.0
    
    transact (fun () -> list.Add(0,0) |> ignore)
    res |> AVal.force |> should equal 0.0
    
    transact (fun () -> list.Add(10,10) |> ignore)
    res |> AVal.force |> should equal 0.0
    
    transact (fun () -> list.Add(2,2) |> ignore)
    res |> AVal.force |> should equal 0.0
    
    transact (fun () -> list.Remove 0 |> ignore)
    res |> AVal.force |> should equal 20.0

    transact (fun () -> list.[2] <- 4)
    res |> AVal.force |> should equal 40.0

[<Test>]
let ``[AMap] reduceBy fold``() =
    let list = cmap [1,1;2,2;3,3]

    let reduction = AdaptiveReduction.fold 0.0 (+)
    let res = AMap.reduceBy reduction (fun _ -> float) list

    res |> AVal.force |> should equal 6.0

    transact (fun () -> list.Add(4,4) |> ignore)
    res |> AVal.force |> should equal 10.0

    transact (fun () -> list.Remove 1 |> ignore)
    res |> AVal.force |> should equal 9.0

    transact (fun () -> list.[4] <- 5)
    res |> AVal.force |> should equal 10.0

    transact (fun () -> list.Clear())
    res |> AVal.force |> should equal 0.0

    
[<Test>]
let ``[AMap] reduceByA group``() =
    let list = cmap [1,1;2,2;3,3]
    
    let even = cval 1
    let odd = cval 0

    let mapping _ v =
        if v % 2 = 0 then even :> aval<_>
        else odd :> aval<_>

    let reduction = AdaptiveReduction.sum()
    let res = AMap.reduceByA reduction mapping list
    
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
    transact (fun () -> list.Add(4,4) |> ignore)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) (4,1) = 2
    transact (fun () -> odd.Value <- 0; even.Value <- 1)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) (4,1) (5,0) = 2
    transact (fun () -> list.Add(5,5) |> ignore)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) (4,1) (5,0) (6,1) = 3
    transact (fun () -> list.Add(6,6) |> ignore)
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
    transact (fun () -> list.Value <- HashMap.ofList [1,1;3,3;5,5])
    res |> AVal.force |> should equal 3
    
    // (2,0) (3,1) (5,1) = 3
    transact (fun () -> even.Value <- 0; list.[1] <- 2)
    res |> AVal.force |> should equal 2
     
[<Test>]
let ``[AMap] reduceByA half group``() =
    let list = cmap [1,1;2,2;3,3]
    
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

    let res = AMap.reduceByA reduction mapping list
    
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
    transact (fun () -> list.Add(4,4) |> ignore)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) (4,1) = 2
    transact (fun () -> odd.Value <- 0; even.Value <- 1)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) (4,1) (5,0) = 2
    transact (fun () -> list.Add(5,5) |> ignore)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) (4,1) (5,0) (6,1) = 3
    transact (fun () -> list.Add(6, 6) |> ignore)
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
    transact (fun () -> list.Value <- HashMap.ofList [1,1;3,3;5,5])
    res |> AVal.force |> should equal 3
   
    // (2,0) (3,1) (5,1) = 3
    transact (fun () -> even.Value <- 0; list.[1] <- 2)
    res |> AVal.force |> should equal 2

    fails |> should be (greaterThan 0)
     
[<Test>]
let ``[AMap] reduceByA fold``() =
    let list = cmap [1,1;2,2;3,3]
    
    let even = cval 1
    let odd = cval 0

    let mapping _ v =
        if v % 2 = 0 then even :> aval<_>
        else odd :> aval<_>

    let reduction = AdaptiveReduction.fold 0 (+)

    let res = AMap.reduceByA reduction mapping list
    
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
    transact (fun () -> list.Add(4,4) |> ignore)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) (4,1) = 2
    transact (fun () -> odd.Value <- 0; even.Value <- 1)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) (4,1) (5,0) = 2
    transact (fun () -> list.Add(5,5) |> ignore)
    res |> AVal.force |> should equal 2
    
    // (1,0) (2,1) (3,0) (4,1) (5,0) (6,1) = 3
    transact (fun () -> list.Add(6,6) |> ignore)
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
    transact (fun () -> list.Value <- HashMap.ofList [1,1;3,3;5,5])
    res |> AVal.force |> should equal 3

    
    // (2,0) (3,1) (5,1) = 3
    transact (fun () -> even.Value <- 0; list.[1] <- 2)
    res |> AVal.force |> should equal 2


[<Test>]
let ``[AMap] filterA``() =
    let map = cmap ["A", 1; "B", 2; "C", 3; "D", 4; "E", 5]
    let keys = cset ["A"; "C"; "E"]

    let res =
        map |> AMap.filterA (fun k _ -> keys |> ASet.contains k)

    let r = res.GetReader()

    r.GetChanges AdaptiveToken.Top |> ignore
    r.State |> should equal (HashMap.ofList ["A", 1; "C", 3; "E", 5])

    transact (fun () ->
        map.Value <- map.Value |> HashMap.map (fun _ v -> v * 2)
    )
    
    r.GetChanges AdaptiveToken.Top |> ignore
    r.State |> should equal (HashMap.ofList ["A", 2; "C", 6; "E", 10])

    transact (fun () ->
        keys.Value <- HashSet.ofList ["A"; "C"; "D"; "E"]
    )
    
    r.GetChanges AdaptiveToken.Top |> ignore
    r.State |> should equal (HashMap.ofList ["A", 2; "C", 6; "D", 8; "E", 10])


[<Test>]
let ``[AMap] mapA``() =
    let map = cmap ["A", 1; "B", 2; "C", 3]
    let flag = cval true

    let res =
        map |> AMap.mapA (fun _ v ->
            flag |> AVal.map (function true -> v | false -> -1)
        )

    res |> AMap.force |> should equal (HashMap.ofList ["A", 1; "B", 2; "C", 3])

    transact (fun () ->
        flag.Value <- false
    )

    res |> AMap.force |> should equal (HashMap.ofList ["A", -1; "B", -1; "C", -1])

    transact (fun () ->
        map.Value <- map.Value |> HashMap.map (fun _ v -> v * 2)
    )

    res |> AMap.force |> should equal (HashMap.ofList ["A", -1; "B", -1; "C", -1])

    transact (fun () ->
        flag.Value <- true
    )

    res |> AMap.force |> should equal (HashMap.ofList ["A", 2; "B", 4; "C", 6])




[<Test>]
let ``[AMap] batchMap``() =

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
        cmap
            [
                file1, file1Cval
                file2, file2Cval
                file3, file3Cval
                // file4 added later
            ] 
    let files =
        filesCmap
        |> AMap.mapA(fun _ v -> v)

    let mutable lastBatch = Unchecked.defaultof<_>
    let res =
        files
        |> AMap.batchMap(fun d ->
            lastBatch <- d
            HashMap.ofList [
                for k,v in d do
                    k,  (dependencies.[k] :> aval<_>)
            ]
        )

    let firstResult = res |> AMap.force
    lastBatch |> should haveCount 3

    transact(fun () -> file1Cval.Value <- file1Cval.Value + 1)

    let secondResult = res |> AMap.force
    lastBatch |> should haveCount 1
    
    firstResult.[file1] |> should equal secondResult.[file1]
    firstResult.[file2] |> should equal secondResult.[file2]
    firstResult.[file3] |> should equal secondResult.[file3]

    transact(fun () -> file1DepCval.Value <- DateTime.UtcNow)

    let thirdResult = res |> AMap.force
    lastBatch |> should haveCount 1
    
    secondResult.[file1] |> should not' (equal thirdResult.[file1])
    secondResult.[file2] |> should equal thirdResult.[file2]
    secondResult.[file3] |> should equal thirdResult.[file3]

    transact(fun () -> 
        file1DepCval.Value <- DateTime.UtcNow
        file2Cval.Value <- file2Cval.Value + 1
    )

    let fourthResult = res |> AMap.force
    lastBatch |> should haveCount 2
    
    thirdResult.[file1] |> should not' (equal fourthResult.[file1])
    thirdResult.[file2] |> should equal fourthResult.[file2]
    thirdResult.[file3] |> should equal fourthResult.[file3]

    transact(fun () -> 
        file1Cval.Value <- file1Cval.Value + 1
        file2DepCval.Value <- DateTime.UtcNow
        filesCmap.Add(file4, file4Cval) |> ignore
    )

    let fifthResult = res |> AMap.force
    lastBatch |> should haveCount 3

    fourthResult.[file1] |> should equal fifthResult.[file1]
    fourthResult.[file2] |> should not' (equal fifthResult.[file2])
    fourthResult.[file3] |> should equal fifthResult.[file3]
    fifthResult.[file4] |> should equal file4DepCval.Value



[<Test>]
let ``[AMap] batchMapWithAdditionalDependencies``() =

    let file1 = "File1.fs"
    let file1Cval = cval 1
    let file1DepCval = cval 1
    let file2 = "File2.fs"
    let file2Cval = cval 2
    let file2DepCval = cval 1
    let file3 = "File3.fs"
    let file3Cval = cval 3
    let file3DepCval = cval 1

    let dependencies = Map [file1, file1DepCval; file2, file2DepCval; file3, file3DepCval]

    let files = 
        [
            file1, file1Cval
            file2, file2Cval
            file3, file3Cval
        ] 
        |> AMap.ofList
        |> AMap.mapA(fun _ v -> v)

    let mutable lastBatch = Unchecked.defaultof<_>
    let res =
        files
        |> AMap.batchMapWithAdditionalDependencies(fun d ->
            lastBatch <- d
            HashMap.ofList [
                for k,v in d do
                    k,  (Guid.NewGuid(), [dependencies.[k]])
            ]
        )
    let firstResult = res |> AMap.force
    lastBatch |> should haveCount 3

    transact(fun () -> file1Cval.Value <- file1Cval.Value + 1)

    let secondResult = res |> AMap.force
    lastBatch |> should haveCount 1
    
    firstResult.[file1] |> should not' (equal secondResult.[file1])
    firstResult.[file2] |> should equal secondResult.[file2]
    firstResult.[file3] |> should equal secondResult.[file3]


    transact(fun () -> 
        file1Cval.Value <- file1Cval.Value + 1
        file3Cval.Value <- file3Cval.Value + 1)
    
    let thirdResult = res |> AMap.force
    lastBatch |> should haveCount 2

    secondResult.[file1] |> should not' (equal thirdResult.[file1])
    secondResult.[file2] |> should equal thirdResult.[file2]
    secondResult.[file3] |> should not' (equal thirdResult.[file3])


    transact(fun () -> file1DepCval.Value <- file1DepCval.Value + 1)

    let fourthResult = res |> AMap.force
    lastBatch |> should haveCount 1
    
    thirdResult.[file1] |> should not' (equal fourthResult.[file1])
    thirdResult.[file2] |> should equal fourthResult.[file2]
    thirdResult.[file3] |> should equal fourthResult.[file3]

    transact(fun () -> 
        file1DepCval.Value <- file1DepCval.Value + 1
        file1Cval.Value <- file1Cval.Value + 1)

    let fifthResult = res |> AMap.force
    lastBatch |> should haveCount 1
    
    fourthResult.[file1] |> should not' (equal fifthResult.[file1])
    fourthResult.[file2] |> should equal fifthResult.[file2]
    fourthResult.[file3] |> should equal fifthResult.[file3]

    
    transact(fun () -> 
        file2DepCval.Value <- file2DepCval.Value + 1
        file3Cval.Value <- file3Cval.Value + 1)

    let sixthResult = res |> AMap.force
    lastBatch |> should haveCount 2
    
    fifthResult.[file1] |> should equal sixthResult.[file1]
    fifthResult.[file2] |> should not' (equal sixthResult.[file2])
    fifthResult.[file3] |> should not' (equal sixthResult.[file3])

    ()