module AVal

open FSharp.Data.Adaptive
open FSharp.Data.Traceable
open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit
open FSharp.Data
open Generators

[<AutoOpen>]
module Helpers =
    module List =
        let cross (a : list<'A>) (b : list<'B>) =
            a |> List.collect (fun va ->
                b |> List.map (fun vb -> (va, vb))
            )


type Record<'T> = { value : 'T }

[<Property(MaxTest = 1000, Arbitrary = [| typeof<AdaptiveGenerators> |])>]
let ``[AVal] reference impl``() ({ real = real; ref = ref; expression = str; changes = changes } : VVal<obj>) =
    printfn "VALIDATE"
    printfn "%s" (Generators.Generators.indent (Generators.Generators.indent str))
    let check() = 
        let vReal = Adaptive.AVal.force real
        let vRef = Reference.AVal.force ref
        vReal |> should equal vRef
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
                    if not (DefaultEquality.equals v lastValue) then
                        printfn "  change %d => %A" effective v
                        lastValue <- v

                    effective <- effective + 1
        }

    Gen.eval 50 (Random.newSeed()) run

[<Property(EndSize = 10000)>]
let ``[AVal] constant equality`` (value : obj) =
    let a = AVal.constant value
    let b = AVal.constant value
    a |> should equal b

    let a = AVal.constant { value = value }
    let b = AVal.constant { value = value }
    a |> should equal b

    let a = AVal.constant { value = 1 }
    let b = AVal.constant { value = 2 }
    a |> should not' (equal b)

    let a = AVal.constant null
    let b = AVal.constant null
    a |> should equal b

[<Test>]
let ``[AVal] map constant`` () =
    let a = AVal.constant 1
    let b = AVal.map id a
    b.IsConstant |> should be True
  
[<Test>]
let ``[AVal] map2 constant`` () =
    let a = AVal.constant 1
    let b = AVal.constant 2
    let test = AVal.map2 (fun a b -> (a,b)) a b
    test.IsConstant |> should be True

[<Test>]
let ``[AVal] map3 constant`` () =
    let a = AVal.constant 1
    let b = AVal.constant 2
    let c = AVal.constant 3
    let test = AVal.map3 (fun a b c -> (a,b,c)) a b c
    test.IsConstant |> should be True


[<Test>]
let ``[AVal] bind content`` () =
    let a = AVal.constant 10
    let b = AVal.init "b" |> AVal.map id
    let c = AVal.init "c" |> AVal.map id

    let test =
        a |> AVal.bind (fun va ->
            if va = 10 then b
            else c
        )

    test |> should equal b

[<Test>]
let ``[AVal] cast equality`` () =
    let a = cval [1;2;3]

    let b = a |> AVal.cast<seq<int>>
    let c = a |> AVal.cast<seq<int>>
    b |> should equal c

 
type EagerVal<'T>(input : aval<'T>) =
    inherit AdaptiveObject()

    let mutable last = None

    override x.MarkObject() = 
        let v = input.GetValue (AdaptiveToken(x))
        match last with
        | Some old when DefaultEquality.equals old v -> 
            printfn "shortcut %A" v
            false
        | _ ->
            printfn "changed %A" v
            true

    member x.GetValue(token : AdaptiveToken) =
        x.EvaluateAlways token (fun token ->
            let res = input.GetValue token
            last <- Some res
            res
        )
        
    interface IAdaptiveValue with
        member x.Accept (v : IAdaptiveValueVisitor<'R>) = v.Visit x
        member x.GetValueUntyped(t) = x.GetValue t :> obj
        member x.ContentType = typeof<'T>

    interface IAdaptiveValue<'T> with
        member x.GetValue(t) = x.GetValue t
        
[<Test>]
let ``[AVal] eager evaluation`` () =
    let a = AVal.init 0

    let short = AVal.init "a"
    let long = AVal.init "a" |> AVal.map id |> AVal.map id |> AVal.map id
    let different = AVal.init "b" |> AVal.map id |> AVal.map id |> AVal.map id |> AVal.map id |> AVal.map id

    let dynamic =   
        a |> AVal.bind (fun l ->
            if l = 0 then short :> aval<_>
            elif l = 1 then long
            else different
        )

    // eager level is initially small
    let eager = EagerVal(dynamic) :> aval<_>
    eager |> AVal.force |> should equal "a"
    eager.Level |> should equal 2

    // makes eager level larger (LevelChangedException)
    // but does not change content.
    transact (fun () -> a.Value <- 1)
    eager.OutOfDate |> should be False
    eager |> AVal.force |> should equal "a"
    eager.Level |> should be (greaterThan long.Level)

    // actually changes content.
    transact (fun () -> a.Value <- 2)
    eager.OutOfDate |> should be True
    eager |> AVal.force |> should equal "b"
    eager.Level |> should be (greaterThan different.Level)


[<Test>]
let ``[AVal] eager marking`` () =
    let a = AVal.init 0
    let mod2 = a |> AVal.map (fun v -> v % 2)
    let eager = EagerVal(mod2) :> aval<_>
    let output = eager |> AVal.map id

    output |> AVal.force |> should equal 0

    transact (fun () -> a.Value <- 2)
    output.OutOfDate |> should be False
    output |> AVal.force |> should equal 0
    
    transact (fun () -> a.Value <- 1)
    output.OutOfDate |> should be True
    output |> AVal.force |> should equal 1

    transact (fun () -> a.Value <- 3)
    output.OutOfDate |> should be False
    output |> AVal.force |> should equal 1

    transact (fun () -> a.Value <- 0)
    output.OutOfDate |> should be True
    output |> AVal.force |> should equal 0

[<Test>]
let ``[AVal] nop change evaluation`` () =
    
    let input = AVal.init 5
    let a = AVal.map id input
    let b = AVal.map (fun v -> -v) input
    let c = AVal.map2 (+) a b
    let mutable mapCounter = 0
    let d = c |> AVal.map (fun v -> mapCounter <- mapCounter + 1; v)
    
    // mapping should be evaluated
    d |> AVal.force |> should equal 0
    mapCounter |> should equal 1
    mapCounter <- 0

    // mapping should not be evaluated (input still 0)
    transact (fun () -> input.Value <- 10)
    d.OutOfDate |> should be True
    d |> AVal.force |> should equal 0
    mapCounter |> should equal 0

[<Test>]
let ``[AVal] ChangeableLazyVal working``() =
    let mutable computeCount = 0
    let compute (v : int) () =
        computeCount <- computeCount + 1
        v
    let sinceLast() =
        let v = computeCount
        computeCount <- 0
        v

        
    let v = ChangeableLazyVal(compute 1)
    let test = v |> AVal.map (fun v -> v + 2)

    test |> AVal.force |> should equal 3
    sinceLast() |> should equal 1

    transact (fun () -> v.Update(compute 0))
    sinceLast() |> should equal 1
    
    transact (fun () -> v.Update(compute 3))
    sinceLast() |> should equal 0
    
    test |> AVal.force |> should equal 5
    sinceLast() |> should equal 1

    
    transact (fun () -> v.Update(compute 3))
    sinceLast() |> should equal 1
    test.OutOfDate |> should be False
    ()

[<Test>]
let ``[AVal] map non-adaptive and bind``() =
    let v = AVal.init true
    let a = AVal.constant 0
    let b = AVal.constant 1

    let output = v |> AVal.map id |> AVal.mapNonAdaptive id |> AVal.bind (fun flag -> if flag then a else b)

    output |> AVal.force |> should equal 0

    transact (fun () -> v.Value <- false)
    output |> AVal.force |> should equal 1

[<Test>]
let ``[AVal] mapNonAdaptive GC correct``() =
    
    let v = cval 10

    let run() =
        v |> AVal.mapNonAdaptive ((+)1) |> AVal.map id

    let test = run()
    test |> AVal.force |> should equal 11

    System.GC.Collect(3)
    System.GC.WaitForFullGCComplete() |> ignore
    System.GC.WaitForPendingFinalizers()


    transact (fun () -> v.Value <- 100)
    test |> AVal.force |> should equal 101

[<Test>]
let ``[AVal] multi map non-adaptive and bind``() =
    let v = AVal.init true
    let a = AVal.constant 0
    let b = AVal.constant 1

    let output = v |> AVal.map id |> AVal.mapNonAdaptive id |> AVal.mapNonAdaptive id |> AVal.bind (fun flag -> if flag then a else b)

    output |> AVal.force |> should equal 0

    transact (fun () -> v.Value <- false)
    output |> AVal.force |> should equal 1



[<Test>]
let ``[AVal] mapWithAdditionalDependencies``() =
    let v = cval 1
    let incrDep (dep : cval<_>) =
        dep.Value <- dep.Value + 1 
    let mutable dependency1 = Unchecked.defaultof<_>
    let newDep1 () =  
        dependency1 <- cval 2
        dependency1
    let mutable dependency2 =  Unchecked.defaultof<_>
    let newDep2 () = 
        dependency2 <- cval 3
        dependency2
    let mutable mappingCalls = 0
    let incrMapping () =
        mappingCalls <- mappingCalls + 1

    let mapping (i : int) =
        incrMapping ()
        // dependencies aren't known until mapping time
        i * 2, [newDep1(); newDep2()]
    
    let output = v |> AVal.mapWithAdditionalDependencies mapping

    output |> AVal.force |> should equal 2
    mappingCalls |> should equal 1

    transact (fun () -> v.Value <- 2)
    output |> AVal.force |> should equal 4
    mappingCalls |> should equal 2

    transact (fun () -> incrDep dependency1)
    output |> AVal.force |> should equal 4
    mappingCalls |> should equal 3

    
    transact (fun () -> incrDep dependency1)
    output |> AVal.force |> should equal 4
    mappingCalls |> should equal 4

    transact (fun () -> v.Value <- 2)
    output |> AVal.force |> should equal 4
    mappingCalls |> should equal 4

    
    transact (fun () -> v.Value <- 1)
    output |> AVal.force |> should equal 2
    mappingCalls |> should equal 5

    transact (fun () -> 
        v.Value <- 1
        incrDep dependency1)
    output |> AVal.force |> should equal 2
    mappingCalls |> should equal 6

    transact (fun () -> 
        incrDep dependency2
        incrDep dependency1)
    output |> AVal.force |> should equal 2
    mappingCalls |> should equal 7