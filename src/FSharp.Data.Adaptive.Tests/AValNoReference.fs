module AValNoReference

open FSharp.Data.Adaptive
open NUnit.Framework
open FsUnit
open FsCheck.NUnit

type EagerVal<'T>(input : aval<'T>) =
    inherit AdaptiveObject()

    let mutable last = None

    override x.Mark() = 
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

    interface AdaptiveValue<'T> with
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




