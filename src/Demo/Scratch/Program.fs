open System
open System.IO
open System.Threading
open FSharp.Data.Adaptive

module AList =
    open System.Threading.Tasks

    type private AsyncSignal(isSet : bool) =
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
                    Task.FromResult()
                else
                    match tcs with
                    | Some tcs -> 
                        tcs.Task
                    | None -> 
                        let t = TaskCompletionSource<unit>()
                        tcs <- Some t
                        t.Task
            )

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