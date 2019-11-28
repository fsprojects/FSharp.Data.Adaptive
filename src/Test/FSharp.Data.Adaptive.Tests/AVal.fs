module AVal

open FSharp.Data.Adaptive
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
                    if not (Unchecked.equals v lastValue) then
                        printfn "  change %d => %A" effective v
                        lastValue <- v

                    effective <- effective + 1
        }

    Gen.eval 50 (Random.newSeed()) run

[<Property>]
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
let ``[AVal] bind constant`` () =
    let a = AVal.constant 10
    let b = AVal.init "b" |> AVal.map id
    let c = AVal.init "c" |> AVal.map id

    let test =
        a |> AVal.bind (fun va ->
            if va = 10 then b
            else c
        )

    test |> should equal b
 
 
type EagerVal<'T>(input : aval<'T>) =
    inherit AdaptiveObject()

    let mutable last = None

    override x.MarkObject() = 
        let v = input.GetValue AdaptiveToken.Top
        match last with
        | Some old when Unchecked.equals old v -> 
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




