module History

open Xunit
open FSharp.Control.Incremental
open FSharp.Control.Traceable
open FsCheck
open FsUnit.Xunit
open Xunit.Sdk
open System.Diagnostics
open Xunit.Abstractions

type ExpensiveOp(delta : int) =
    static let size = 1 <<< 24
    let array : byte[] = 
        if delta <> 0 then Array.zeroCreate size
        else null

    static member Size = size
    member x.Array = array
    member x.Delta = delta

    static member Monoid =
        {
            mempty = ExpensiveOp(0)
            misEmpty = fun e -> e.Delta = 0
            mappend = fun l r -> ExpensiveOp(l.Delta + r.Delta)
        }

type Number(value : int) =
    member x.Value = value
    static member Trace =
        {
            tmonoid = ExpensiveOp.Monoid
            tempty = Number(0)
            tintegrate = fun n o -> Number(n.Value + o.Delta), o
            tdifferentiate = fun l r -> ExpensiveOp(r.Value - l.Value)
            tcollapse = fun _ _ -> false
        }

[<Fact>]
let ``[History] weak``() : unit =
    let h = History Number.Trace

    let createDeadReaders(n : int) =
        for i in 1 .. n do
            h.NewReader().GetOperations(AdaptiveToken.Top) |> ignore
        ensureGC()

    let mem0 = getRealMemory()
    createDeadReaders 5
    transact (fun () -> h.Perform (ExpensiveOp +4)) |> ignore
    createDeadReaders 5
    ensureGC()
    let mem1 = getRealMemory()
    printfn "leak: %A" (mem1 - mem0)
    (mem1 - mem0) |> int |> should be (lessThanOrEqualTo ExpensiveOp.Size)

    let iter() = 
        let r = h.NewReader()
        
        let mem1 = getRealMemory()
        let eval () = r.GetOperations(AdaptiveToken.Top) |> ignore
        createDeadReaders 5
        ensureGC()
        transact (fun () -> h.Perform (ExpensiveOp +4)) |> ignore
        eval()
        ensureGC()
        let mem2 = getRealMemory()
        printfn "leak: %A" (mem2 - mem1)
        (mem2 - mem1) |> int |> should be (lessThanOrEqualTo ExpensiveOp.Size)

    for i in 1 .. 10 do
        iter()





[<Fact>]
let ``[History] single reader``() =
    
    let h = History(CountingHashSet.trace)
    let r = h.NewReader()

    transact (fun () ->
        h.Perform (DHashSet.ofList [Add 1; Add 2]) |> ignore
    )
    ensureGC()

    r.GetOperations(AdaptiveToken.Top) 
    |> should setequal [Add 1; Add 2]

    r.State
    |> should setequal [1;2]
    
    transact (fun () ->
        h.Perform (DHashSet.ofList [Rem 1]) |> ignore
    )
    ensureGC()
    r.GetOperations(AdaptiveToken.Top) 
    |> should setequal [Rem 1]

    r.State
    |> should setequal [2]
    



    ()
   
[<Fact>]
let ``[History] multiple readers``() = 
    let h = History(CountingHashSet.trace)
    let r = h.NewReader()

    let secondReader() =
        h.NewReader() |> ignore
        ensureGC()
        

    transact (fun () ->
        h.Perform (DHashSet.ofList [Add 1; Add 2]) |> ignore
    )
    secondReader()

    r.GetOperations(AdaptiveToken.Top) 
    |> should setequal [Add 1; Add 2]

    r.State
    |> should setequal [1;2]
    
    secondReader()

    transact (fun () ->
        h.Perform (DHashSet.ofList [Rem 1]) |> ignore
    )
    secondReader()
    r.GetOperations(AdaptiveToken.Top) 
    |> should setequal [Rem 1]

    r.State
    |> should setequal [2]
    


