module ARefNoReference

open FSharp.Control.Incremental
open NUnit.Framework
open FsUnit
open FsCheck.NUnit

type EagerRef<'a>(input : aref<'a>) =
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

    interface aref<'a> with
        member x.GetValue(t) = x.GetValue t
        
[<Test>]
let ``[ARef] eager evaluation`` () =
    let a = ARef.init 0

    let short = ARef.init "a"
    let long = ARef.init "a" |> ARef.map id |> ARef.map id |> ARef.map id
    let different = ARef.init "b" |> ARef.map id |> ARef.map id |> ARef.map id |> ARef.map id |> ARef.map id

    let dynamic =   
        a |> ARef.bind (fun l ->
            if l = 0 then short :> aref<_>
            elif l = 1 then long
            else different
        )

    // eager level is initially small
    let eager = EagerRef(dynamic) :> aref<_>
    eager |> ARef.force |> should equal "a"
    eager.Level |> should equal 2

    // makes eager level larger (LevelChangedException)
    // but does not change content.
    transact (fun () -> a.Value <- 1)
    eager.OutOfDate |> should be False
    eager |> ARef.force |> should equal "a"
    eager.Level |> should be (greaterThan long.Level)

    // actually changes content.
    transact (fun () -> a.Value <- 2)
    eager.OutOfDate |> should be True
    eager |> ARef.force |> should equal "b"
    eager.Level |> should be (greaterThan different.Level)


[<Test>]
let ``[ARef] nop change evaluation`` () =
    let input = ARef.init 5

    let a = ARef.map id input
    let b = ARef.map (fun v -> -v) input

    let c = ARef.map2 (+) a b
    let mutable mapCounter = 0
    let d = c |> ARef.map (fun v -> mapCounter <- mapCounter + 1; printfn "eval"; v)

    d |> ARef.force |> should equal 0
    mapCounter |> should equal 1
    mapCounter <- 0

    transact (fun () -> input.Value <- 10)
    d.OutOfDate |> should be True
    d |> ARef.force |> should equal 0
    mapCounter |> should equal 0




