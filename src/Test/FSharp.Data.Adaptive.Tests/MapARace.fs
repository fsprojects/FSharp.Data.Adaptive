module MapARace

open System
open System.Threading
open System.Threading.Tasks
open NUnit.Framework
open FsUnit
open FSharp.Data.Adaptive

/// The aval returned by the mapping. While `armed` holds it stops the reading thread at its first hash
/// access after evaluation and runs `inWindow` there. On the unfixed readers that access was the
/// registration in `targets` after `GetValue`, so `inWindow` ran exactly in the race window.
/// After the fix the registration happens before evaluation and the hook never fires.
type private WindowVal<'a>(input : aval<int>, mapping : int -> 'a, readerThread : Thread, armed : unit -> bool, inWindow : IAdaptiveObject -> unit) =
    inherit AVal.AbstractVal<'a>()

    let mutable evaluated = false
    let mutable windowHit = false

    override x.Compute(t) =
        evaluated <- true
        mapping (input.GetValue t)

    override x.GetHashCode() =
        if armed () && evaluated && not windowHit && Thread.CurrentThread = readerThread && not (Monitor.IsEntered x) then
            windowHit <- true
            inWindow x
        LanguagePrimitives.PhysicalHash x

    override x.Equals(o) = obj.ReferenceEquals(x, o)

/// Runs `change` as a transaction on another thread and waits until it has marked `k`. Right after
/// marking, the transaction calls InputChanged on the reader (without its lock) and then blocks on the
/// reader's lock, which the reading thread holds until its Compute is done.
let private runTransactionInWindow (change : unit -> unit) (started : Task ref) (k : IAdaptiveObject) =
    started.Value <- Task.Run(fun () -> transact change)
    let sw = Diagnostics.Stopwatch.StartNew()
    while not k.OutOfDate && sw.Elapsed < TimeSpan.FromSeconds 5.0 do
        Thread.Sleep 1
    k.OutOfDate |> should equal true
    // give the transaction time to call InputChanged and wait for the reader's lock
    Thread.Sleep 200

/// Builds the structure under test with `build` (which gets a factory for the element aval), evaluates it
/// once on this thread while another thread sets the input to 1 in the race window, then sets the input
/// to 2 and returns the result the structure reports afterwards.
let private race (mapping : int -> 'a) (build : (unit -> aval<'a>) -> (unit -> 'r)) : 'r =
    let input = cval 0
    let readerThread = Thread.CurrentThread
    let transaction = ref Task.CompletedTask
    let firstRead = ref true
    let factory () =
        WindowVal<'a>(input, mapping, readerThread, (fun () -> firstRead.Value), runTransactionInWindow (fun () -> input.Value <- 1) transaction) :> aval<'a>

    let read = build factory
    read () |> ignore
    firstRead.Value <- false
    transaction.Value.Wait()

    // a later change of the same input must reach the reader
    transact (fun () -> input.Value <- 2)
    read ()

[<Test>]
let ``[AList] chooseA does not lose a change made during the first evaluation on another thread``() =
    race Some (fun mk ->
        let res = clist [ 1 ] |> AList.chooseA (fun _ -> mk ())
        fun () -> AList.force res |> IndexList.toList
    ) |> should equal [ 2 ]

[<Test>]
let ``[AList] mapA does not lose a change made during the first evaluation on another thread``() =
    race id (fun mk ->
        let res = clist [ 1 ] |> AList.mapA (fun _ -> mk ())
        fun () -> AList.force res |> IndexList.toList
    ) |> should equal [ 2 ]

[<Test>]
let ``[AList] sumByA does not lose a change made during the first evaluation on another thread``() =
    race id (fun mk ->
        // three elements: with at most two the reader rebuilds completely and re-pulls every element anyway
        let res = clist [ 1; 2; 3 ] |> AList.sumByA (fun i -> if i = 1 then mk () else AVal.constant i)
        fun () -> AVal.force res
    ) |> should equal 7

[<Test>]
let ``[AMap] chooseA does not lose a change made during the first evaluation on another thread``() =
    race Some (fun mk ->
        let res = cmap [ 1, 1 ] |> AMap.chooseA (fun _ _ -> mk ())
        fun () -> AMap.force res |> HashMap.toList
    ) |> should equal [ 1, 2 ]

[<Test>]
let ``[AMap] mapA does not lose a change made during the first evaluation on another thread``() =
    race id (fun mk ->
        let res = cmap [ 1, 1 ] |> AMap.mapA (fun _ _ -> mk ())
        fun () -> AMap.force res |> HashMap.toList
    ) |> should equal [ 1, 2 ]

[<Test>]
let ``[AMap] sumByA does not lose a change made during the first evaluation on another thread``() =
    race id (fun mk ->
        let res = cmap [ 1, 1; 2, 2; 3, 3 ] |> AMap.sumByA (fun k _ -> if k = 1 then mk () else AVal.constant k)
        fun () -> AVal.force res
    ) |> should equal 7

[<Test>]
let ``[ASet] sumByA does not lose a change made during the first evaluation on another thread``() =
    race id (fun mk ->
        let res = cset [ 1; 2; 3 ] |> ASet.sumByA (fun i -> if i = 1 then mk () else AVal.constant i)
        fun () -> AVal.force res
    ) |> should equal 7
