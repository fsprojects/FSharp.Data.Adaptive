module History

open NUnit.Framework
open FSharp.Control.Incremental
open FSharp.Control.Traceable
open FsCheck
open FsCheck.NUnit
open FsUnit

type HugeOp(delta : int) =
    static let size = 1 <<< 24
    let array : byte[] = 
        if delta <> 0 then Array.zeroCreate size
        else null

    static member Size = size
    member x.Array = array
    member x.Delta = delta

    static member Monoid =
        {
            mempty = HugeOp(0)
            misEmpty = fun e -> e.Delta = 0
            mappend = fun l r -> HugeOp(l.Delta + r.Delta)
        }

type Number(value : int) =
    member x.Value = value
    static member Trace =
        {
            tmonoid = HugeOp.Monoid
            tempty = Number(0)
            tintegrate = fun n o -> Number(n.Value + o.Delta), o
            tdifferentiate = fun l r -> HugeOp(r.Value - l.Value)
            tprune = None
            tsize = fun s -> 1
        }



[<Test>]
let ``[History] weak``() : unit =
    let mutable h = History Number.Trace

    let createDeadReaders(n : int) =
        for i in 1 .. n do
            h.NewReader().GetOperations(AdaptiveToken.Top) |> ignore
        ensureGC()

    let mem0 = getRealMemory()
    createDeadReaders 50
    transact (fun () -> h.Perform (HugeOp +4)) |> ignore
    createDeadReaders 50
    ensureGC()
    let mem1 = getRealMemory()
    printfn "leak: %A" (mem1 - mem0)
    (mem1 - mem0) |> int |> should be (lessThanOrEqualTo HugeOp.Size)
    

    let performChange =
        fun () -> h.Perform (HugeOp +4) |> ignore

    let createReaderAndEval() =
        let r = h.NewReader()
        let eval () = r.GetOperations(AdaptiveToken.Top) |> ignore
        eval()
        createDeadReaders 5
        transact performChange
        eval()

    let iter(warmup : bool) = 
        let mem1 = getRealMemory()
        createReaderAndEval()
        let mem2 = getRealMemory()

        if not warmup then printfn "leak: %A" (mem2 - mem1)
        (mem2 - mem1) |> int |> should be (lessThanOrEqualTo HugeOp.Size)

    for i in 1 .. 10 do
        iter true

    h <- History Number.Trace

    for i in 1 .. 20 do
        iter false



[<Test>]
let ``[History] different reader versions``() =
    let history = History CountingHashSet.trace

    let change (l : list<SetOperation<int>>) =
        transact (fun () ->
            history.Perform (DHashSet.ofList l) |> ignore
        )

    let eval (r : IOpReader<_,_>) = 
        r.GetOperations AdaptiveToken.Top

    // create three readers
    let r0 = history.NewReader()
    let r1 = history.NewReader()
    let r2 = history.NewReader()

    // add 1 and pull r1
    change [Add 1]
    r1 |> eval |> should setequal [Add 1]
    ensureGC()
    
    // add 3 and pull r2
    change [Add 2]
    r2 |> eval |> should setequal [Add 1; Add 2]
    ensureGC()

    // add 3 and pull all (all on different version now)
    change [Add 3]
    r0 |> eval |> should setequal [Add 1; Add 2; Add 3]
    r1 |> eval |> should setequal [Add 2; Add 3]
    r2 |> eval |> should setequal [Add 3]
    ensureGC()

    // remove 2 and pull all (all on the same version now)
    change [Rem 2]
    r0 |> eval |> should setequal [Rem 2]
    r1 |> eval |> should setequal [Rem 2]
    r2 |> eval |> should setequal [Rem 2]
    ensureGC()

    // create a new reader and evaluate  it
    let r3 = history.NewReader()
    r3 |> eval |> should setequal [Add 1; Add 3]
    ensureGC()

    // remove 3 and pull all (all on the same version now)
    change [Rem 3]
    r0 |> eval |> should setequal [Rem 3]
    r1 |> eval |> should setequal [Rem 3]
    r2 |> eval |> should setequal [Rem 3]
    r3 |> eval |> should setequal [Rem 3]

    




[<Test>]
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
   
[<Test>]
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
    


