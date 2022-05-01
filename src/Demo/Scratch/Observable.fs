module Observable

open System
open System.Threading
open FSharp.Data.Adaptive
open FSharp.Data.Traceable

[<AutoOpen>]
module private ThreadingHelpers =
    open System.Threading.Tasks

    type AsyncSignal(isSet : bool) =
        static let finished = Task.FromResult()
        let mutable isSet = isSet
        let mutable tcs : option<TaskCompletionSource<unit>> = None

        member x.Pulse() =
            lock x (fun () ->
                if not isSet then
                    isSet <- true
                    match tcs with
                    | Some t -> 
                        t.SetResult()
                        tcs <- None
                    | None -> 
                        ()
                    tcs <- None
            )

        member x.Wait() =
            lock x (fun () ->
                if isSet then
                    isSet <- false
                    finished
                else
                    match tcs with
                    | Some tcs -> 
                        tcs.Task
                    | None -> 
                        let t = TaskCompletionSource<unit>()
                        tcs <- Some t
                        t.Task
            )

module AList =
    let observe (action : IndexList<'a> -> IndexListDelta<'a> -> IndexList<'a> -> unit) (list : alist<'a>) =
        let reader = list.GetReader()
        let action = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt action
        let mutable running = true
        let signal = AsyncSignal(true)
        let sub = reader.AddMarkingCallback signal.Pulse

        Async.Start <|
            async {
                while running do
                    do! Async.AwaitTask (signal.Wait())
                    if running then
                        do! Async.SwitchToThreadPool()

                        let mutable oldState = IndexList.empty
                        let mutable newState = IndexList.empty
                        let mutable ops = IndexListDelta.empty
                        lock reader (fun () ->
                            oldState <- reader.State
                            ops <- reader.GetChanges AdaptiveToken.Top
                            newState <- reader.State
                        )
                        if ops.Count > 0 then action.Invoke(oldState, ops, newState)

            }

        let shutdown() =
            sub.Dispose()
            running <- false
            signal.Pulse()

        { new IDisposable with
            member x.Dispose() = shutdown()
        }

    let toObservable (list : alist<'a>) =
        { new IObservable<IndexList<'a> * IndexListDelta<'a> * IndexList<'a>> with
            member x.Subscribe(obs : IObserver<IndexList<'a> * IndexListDelta<'a> * IndexList<'a>>) =
                list |> observe (fun o d n ->
                    obs.OnNext(o,d,n)
                )
        }

module ASet =
    let observe (action : HashSet<'a> -> HashSetDelta<'a> -> HashSet<'a> -> unit) (set : aset<'a>) =
        let reader = set.GetReader()
        let action = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt action
        let mutable running = true
        let signal = AsyncSignal(true)
        let sub = reader.AddMarkingCallback signal.Pulse

        Async.Start <|
            async {
                while running do
                    do! Async.AwaitTask (signal.Wait())
                    if running then
                        do! Async.SwitchToThreadPool()

                        let mutable oldState = HashSet.empty
                        let mutable newState = HashSet.empty
                        let mutable ops = HashSetDelta.empty
                        lock reader (fun () ->
                            oldState <- CountingHashSet.toHashSet reader.State
                            ops <- reader.GetChanges AdaptiveToken.Top
                            newState <- CountingHashSet.toHashSet reader.State
                        )
                        if ops.Count > 0 then action.Invoke(oldState, ops, newState)

            }

        let shutdown() =
            sub.Dispose()
            running <- false
            signal.Pulse()

        { new IDisposable with
            member x.Dispose() = shutdown()
        }

    let toObservable (set : aset<'a>) =
        { new IObservable<HashSet<'a> * HashSetDelta<'a> * HashSet<'a>> with
            member x.Subscribe(obs : IObserver<HashSet<'a> * HashSetDelta<'a> * HashSet<'a>>) =
                set |> observe (fun o d n ->
                    obs.OnNext(o,d,n)
                )
        }

module AMap =
    let observe (action : HashMap<'a, 'b> -> HashMapDelta<'a, 'b> -> HashMap<'a, 'b> -> unit) (map : amap<'a, 'b>) =
        let reader = map.GetReader()
        let action = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt action
        let mutable running = true
        let signal = AsyncSignal(true)
        let sub = reader.AddMarkingCallback signal.Pulse

        Async.Start <|
            async {
                while running do
                    do! Async.AwaitTask (signal.Wait())
                    if running then
                        do! Async.SwitchToThreadPool()

                        let mutable oldState = HashMap.empty
                        let mutable newState = HashMap.empty
                        let mutable ops = HashMapDelta.empty
                        lock reader (fun () ->
                            oldState <- reader.State
                            ops <- reader.GetChanges AdaptiveToken.Top
                            newState <- reader.State
                        )
                        if ops.Count > 0 then action.Invoke(oldState, ops, newState)

            }

        let shutdown() =
            sub.Dispose()
            running <- false
            signal.Pulse()

        { new IDisposable with
            member x.Dispose() = shutdown()
        }

    let toObservable (map : amap<'a, 'b>) =
        { new IObservable<HashMap<'a, 'b> * HashMapDelta<'a, 'b> * HashMap<'a, 'b>> with
            member x.Subscribe(obs : IObserver<HashMap<'a, 'b> * HashMapDelta<'a, 'b> * HashMap<'a, 'b>>) =
                map |> observe (fun o d n ->
                    obs.OnNext(o,d,n)
                )
        }

module AVal =
    let observe (action : option<'a> -> 'a -> unit) (value : aval<'a>) =
        let action = OptimizedClosures.FSharpFunc<_,_,_>.Adapt action
        let mutable running = true
        let signal = AsyncSignal(true)
        let sub = value.AddMarkingCallback signal.Pulse

        Async.Start <|
            async {
                let mutable old = None
                while running do
                    do! Async.AwaitTask (signal.Wait())
                    if running then
                        do! Async.SwitchToThreadPool()

                        let v = value.GetValue AdaptiveToken.Top
                        match old with
                        | Some o when DefaultEquality.equals o v -> ()
                        | _ ->
                            action.Invoke(old, v)
                            old <- Some v

            }

        let shutdown() =
            sub.Dispose()
            running <- false
            signal.Pulse()

        { new IDisposable with
            member x.Dispose() = shutdown()
        }

    let toObservable (value : aval<'a>) =
        { new IObservable<option<'a> * 'a> with
            member x.Subscribe(obs : IObserver<option<'a> * 'a>) =
                value |> observe (fun o n ->
                    obs.OnNext(o,n)
                )
        }



let run() =
    let a = clist []

    let sub = 
        a |> AList.observe (fun oldState ops newState ->
            printfn "thread: %d" Thread.CurrentThread.ManagedThreadId
            printfn "  old: %0A" (IndexList.toList oldState)
            printfn "  new: %0A" (IndexList.toList newState)
        )

    let rand = Random()
    for i in 1 .. 10 do
        let l = rand.Next(20)
        let list = List.init l (fun _ -> rand.Next(20))
        transact(fun () -> a.UpdateTo list |> ignore)
        printfn "changed"
        Thread.Sleep 1000

    sub.Dispose()

    let l = rand.Next(20)
    let list = List.init l (fun _ -> rand.Next(20))
    transact(fun () -> a.UpdateTo list |> ignore)
    printfn "changed"
    Thread.Sleep 1000