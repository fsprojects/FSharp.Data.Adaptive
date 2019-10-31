module Callbacks

open FsUnit
open FsCheck.NUnit
open FSharp.Data.Adaptive
open NUnit.Framework
open System.Threading

[<Test>]
let ``[MarkingCallback] fired`` () =
    let m = cval 10
    let d = m |> AVal.map (fun v -> v)

    let mutable fired = 0
    let callback() =
        Interlocked.Increment(&fired) |> ignore
        
    let wasFired() =
        Interlocked.Exchange(&fired, 0)


    use __ = d.AddMarkingCallback(callback)
    wasFired() |> should equal 0

    AVal.force d |> ignore
    wasFired() |> should equal 0

    transact (fun () -> m.Value <- 100)
    wasFired() |> should equal 1

    
    transact (fun () -> m.Value <- 20)
    wasFired() |> should equal 0

    AVal.force d |> ignore
    wasFired() |> should equal 0

    
    transact (fun () -> m.Value <- 15)
    wasFired() |> should equal 1


[<Test>]
let ``[OnNextMarking] fired`` () =
    
    let m = cval 10
    let d = m |> AVal.map (fun v -> v)

    let mutable fired = 0
    let callback() =
        Interlocked.Increment(&fired) |> ignore
        
    let wasFired() =
        Interlocked.Exchange(&fired, 0)


    use __ = d.OnNextMarking(callback)
    wasFired() |> should equal 0
    
    AVal.force d |> ignore
    wasFired() |> should equal 0

    transact (fun () -> m.Value <- 100)
    wasFired() |> should equal 1

    
    transact (fun () -> m.Value <- 20)
    wasFired() |> should equal 0

    AVal.force d |> ignore
    wasFired() |> should equal 0

    
    transact (fun () -> m.Value <- 15)
    wasFired() |> should equal 0