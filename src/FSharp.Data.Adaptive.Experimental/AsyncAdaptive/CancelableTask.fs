namespace FSharp.Data.Adaptive

open System.Threading
open System.Threading.Tasks
   
type CancelableTask<'a>(cancel : unit -> unit, real : Task<'a>) =
    let cts = new CancellationTokenSource()
    
    let output =
        if real.IsCompleted then
            real
        else
            let tcs = new TaskCompletionSource<'a>()
            let s =
                cts.Token.Register(fun () ->
                    tcs.TrySetCanceled() |> ignore
                )
            real.ContinueWith(fun (t : Task<'a>) ->
                s.Dispose()
                if t.IsFaulted then
                    tcs.TrySetException(t.Exception)
                elif t.IsCanceled then
                    tcs.TrySetCanceled()
                else
                    tcs.TrySetResult(t.Result)
            ) |> ignore
            
            tcs.Task
        
    
    member x.Cancel() = 
        cancel()
        cts.Cancel()
        
    member x.Task =
        output
        
type internal RefCountingTaskCreator<'a>(create : CancellationToken -> Task<'a>) =
    
    let mutable refCount = 0
    let mutable cache : option<Task<'a>> = None
    let mutable cancel : CancellationTokenSource = null
    
    member private x.RemoveRef() =
        lock x (fun () ->
            if refCount = 1 then
                refCount <- 0
                cancel.Cancel()
                cancel.Dispose()
                cancel <- null
                cache <- None
            else
                refCount <- refCount - 1
        )

    member x.New() =
        lock x (fun () ->
            match cache with
            | Some cache ->
                refCount <- refCount + 1
                CancelableTask(x.RemoveRef, cache)
            | None ->
                cancel <- new CancellationTokenSource()
                let task = create cancel.Token
                cache <- Some task
                refCount <- refCount + 1
                CancelableTask(x.RemoveRef, task)
        )
 
    