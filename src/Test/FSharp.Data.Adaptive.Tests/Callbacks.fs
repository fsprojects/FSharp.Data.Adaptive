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

[<Test>]
let ``[AddWeakCallback] not surviving GC``() =
    
    let subscribe (set : aset<int>) =
        let r = ref HashSet.empty
        let d =
            set.AddWeakCallback(fun state delta ->
                let s, _ = CountingHashSet.applyDelta state delta
                r := CountingHashSet.toHashSet s
            )
        let w = WeakReference<_>(d)

        r, w

    let input = cset []
    let set = ASet.map id input

    use __ = set.AddWeakCallback(fun _ _ -> ())
    use __ = set.AddCallback(fun _ _ -> ())

    let ref, disp = subscribe set
    let initial = input.Value
    initial |> should equal !ref
    GC.Collect()
    GC.WaitForFullGCComplete() |> ignore
    GC.WaitForPendingFinalizers()

    transact (fun () -> input.UnionWith [1;2;3])
    initial |> should equal !ref

    
    transact (fun () -> input.UnionWith [4;5])
    initial |> should equal !ref

    match disp.TryGetTarget() with
    | (true, d) -> d.Dispose()
    | _ -> ()
    
    
    transact (fun () -> input.Clear())
    initial |> should equal !ref






// https://github.com/fsprojects/FSharp.Data.Adaptive/issues/120
[<Test>]
let ``[AddCallback] concurrent add/dispose vs transact does not deadlock`` () =
    let c = cval 1
    use __ = c.AddCallback(fun (_ : int) -> ())

    let running = ref true
    let writer =
        Thread((fun () ->
            while running.Value do
                transact (fun () ->
                    Thread.Yield() |> ignore
                    c.Value <- c.Value + 1
                )
        ), IsBackground = true)

    let worker =
        Thread((fun () ->
            for _ in 1 .. 200000 do
                let d = c.AddCallback(fun (_ : int) -> ())
                Thread.Yield() |> ignore
                d.Dispose()
        ), IsBackground = true)

    writer.Start()
    worker.Start()
    let finished = worker.Join(TimeSpan.FromSeconds 60.0)
    running.Value <- false
    finished |> should be True
    writer.Join(TimeSpan.FromSeconds 10.0) |> should be True

/// The private callback table (CallbackExtensions.callbackObjects) via reflection.
let private callbackTable () =
    let asm = typeof<IAdaptiveObject>.Assembly
    let modType = asm.GetType("FSharp.Data.Adaptive.CallbackExtensions", true)
    let flags = System.Reflection.BindingFlags.Static ||| System.Reflection.BindingFlags.NonPublic ||| System.Reflection.BindingFlags.Public
    let isTable (t : Type) = t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<System.Runtime.CompilerServices.ConditionalWeakTable<_,_>>
    let fromProp =
        modType.GetProperties(flags) |> Array.tryFind (fun p -> isTable p.PropertyType) |> Option.map (fun p -> p.GetValue(null))
    let fromField () =
        asm.GetTypes()
        |> Seq.collect (fun t -> t.GetFields(flags))
        |> Seq.tryFind (fun f -> isTable f.FieldType)
        |> Option.map (fun f -> f.GetValue(null))
    match fromProp with
    | Some t -> t
    | None ->
        match fromField() with
        | Some t -> t
        | None -> failwith "callback table not found"

/// When the table lock is contended during Dispose, the MultiCallbackObject cannot release
/// itself and lingers as an (empty) output of the cval. That must be harmless: a new
/// subscription reuses it correctly and the next marking releases it.
[<Test>]
let ``[AddCallback] lingering callback object after contended dispose is harmless`` () =
    let table = callbackTable ()
    let c = cval 1
    let outputs = (c :> IAdaptiveObject).Outputs

    let d = c.AddCallback(fun (_ : int) -> ())
    outputs.IsEmpty |> should be False

    // hold the table lock on another thread while disposing -> check's TryEnter fails
    use locked = new ManualResetEventSlim(false)
    use release = new ManualResetEventSlim(false)
    let holder = Thread((fun () -> lock table (fun () -> locked.Set(); release.Wait())), IsBackground = true)
    holder.Start()
    locked.Wait()
    d.Dispose()
    release.Set()
    holder.Join()

    // the empty callback object is still registered (linger)
    outputs.IsEmpty |> should be False

    // a new subscription reuses the lingering object and works
    let mutable fired = []
    let d2 = c.AddCallback(fun (v : int) -> fired <- v :: fired)
    fired |> should equal [1]
    transact (fun () -> c.Value <- 2)
    fired |> should equal [2; 1]
    outputs.IsEmpty |> should be False
    d2.Dispose()
    outputs.IsEmpty |> should be True

    // linger again, but this time nobody subscribes: the next marking releases it
    let d3 = c.AddCallback(fun (_ : int) -> ())
    use locked2 = new ManualResetEventSlim(false)
    use release2 = new ManualResetEventSlim(false)
    let holder2 = Thread((fun () -> lock table (fun () -> locked2.Set(); release2.Wait())), IsBackground = true)
    holder2.Start()
    locked2.Wait()
    d3.Dispose()
    release2.Set()
    holder2.Join()
    outputs.IsEmpty |> should be False
    transact (fun () -> c.Value <- 3)
    outputs.IsEmpty |> should be True
    fired |> should equal [2; 1]
