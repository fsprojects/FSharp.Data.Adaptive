module Callbacks

open FsUnit
open FsCheck.NUnit
open FSharp.Data.Adaptive
open FSharp.Data.Traceable
open NUnit.Framework
open System.Threading
open System

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


[<Test>]
let ``[AddCallback] surviving GC``() =
    
    let subscribe (set : aset<int>) =
        let r = ref HashSet.empty
        let d =
            set.AddCallback(fun state delta ->
                let s, _ = CountingHashSet.applyDelta state delta
                r := CountingHashSet.toHashSet s
            )
        let w = WeakReference<_>(d)

        r, w

    let set = cset []

    let ref, disp = subscribe set
    set.Value |> should equal !ref
    GC.Collect()
    GC.WaitForFullGCComplete() |> ignore
    GC.WaitForPendingFinalizers()

    transact (fun () -> set.UnionWith [1;2;3])
    set.Value |> should equal !ref

    
    transact (fun () -> set.UnionWith [4;5])
    set.Value |> should equal !ref

    match disp.TryGetTarget() with
    | (true, d) -> d.Dispose()
    | _ -> ()
    
    
    transact (fun () -> set.Clear())
    set.Value |> should not' (equal !ref)






