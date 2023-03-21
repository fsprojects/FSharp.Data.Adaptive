namespace FSharp.Data.Adaptive

open System.Threading
open System.Threading.Tasks
    
type asyncaval<'a> =
    inherit IAdaptiveObject
    abstract GetTask : AdaptiveToken -> CancelableTask<'a>
    
module AsyncAVal =
    
    type ConstantVal<'a>(value : Task<'a>) =
        inherit ConstantObject()
        
        interface asyncaval<'a> with
            member x.GetTask _ = CancelableTask(id, value)
        
    [<AbstractClass>]
    type AbstractVal<'a>() =
        inherit AdaptiveObject()
        abstract Compute : AdaptiveToken -> CancelableTask<'a>
        
        member x.GetTask token =
            x.EvaluateAlways token x.Compute
        
        interface asyncaval<'a> with
            member x.GetTask t = x.GetTask t
    
    let constant (value : 'a) =
        ConstantVal(Task.FromResult value) :> asyncaval<_>
     
    let ofTask (value : Task<'a>) =
        ConstantVal(value) :> asyncaval<_>
     
    let ofCancelableTask (value : CancelableTask<'a>) =
        ConstantVal(value.Task) :> asyncaval<_>
     
    let force (v : asyncaval<'a>) =
        v.GetTask AdaptiveToken.Top
     
    let toAVal (defaultValue : 'a) (value : asyncaval<'a>) =
        let mutable valueDirty = 1
        let l = obj()
        let mutable latest = defaultValue
        let mutable lastTask : Task<unit> = null
        let mutable version = 0
        
        { new AVal.AbstractVal<'a>() with
            override x.InputChangedObject(_, _) =
                valueDirty <- 1
                
            override x.Compute(t) =
                if isNull lastTask || lastTask.IsCompleted then
                    if Interlocked.Exchange(&valueDirty, 0) = 1 then
                        let vv = Interlocked.Increment(&version)
                        let t = value.GetTask(t)
                        lastTask <-
                            task {
                                let! v = t.Task
                                if vv = version then
                                    lock l (fun () ->  latest <- v)
                                    transact x.MarkOutdated
                            }
                lock l (fun () -> latest)
        }
     
    let ofAVal (value : aval<'a>) =
        if value.IsConstant then
            ConstantVal(Task.FromResult(AVal.force value)) :> asyncaval<_>
        else
            { new AbstractVal<'a>() with
                member x.Compute t =
                    let real = Task.FromResult(value.GetValue t)
                    CancelableTask(id, real)
            } :> asyncaval<_>

    let map (mapping : 'a -> CancellationToken -> Task<'b>) (input : asyncaval<'a>) =
        let mutable cache : option<RefCountingTaskCreator<'b>> = None
        { new AbstractVal<'b>() with
            member x.Compute t =
                if x.OutOfDate || Option.isNone cache then
                    let ref =
                        RefCountingTaskCreator(fun ct ->
                            let it = input.GetTask t
                            let s = ct.Register(fun () -> it.Cancel())
                            task {
                                try
                                    let! i = it.Task
                                    return! mapping i ct
                                finally
                                    s.Dispose()
                            }    
                        )
                    cache <- Some ref
                    ref.New()
                else
                    cache.Value.New()
        } :> asyncaval<_>

    let map2 (mapping : 'a -> 'b -> CancellationToken -> Task<'c>) (ca : asyncaval<'a>) (cb : asyncaval<'b>) =
        let mutable cache : option<RefCountingTaskCreator<'c>> = None
        { new AbstractVal<'c>() with
            member x.Compute t =
                if x.OutOfDate || Option.isNone cache then
                    let ref =
                        RefCountingTaskCreator(fun ct ->
                            let ta = ca.GetTask t
                            let tb = cb.GetTask t
                            let s = ct.Register(fun () -> ta.Cancel(); tb.Cancel())
                            task {
                                try
                                    let! va = ta.Task
                                    let! vb = tb.Task
                                    return! mapping va vb ct
                                finally
                                    s.Dispose()
                            }    
                        )
                    cache <- Some ref
                    ref.New()
                else
                    cache.Value.New()
        } :> asyncaval<_>

    let bind (mapping : 'a -> CancellationToken -> asyncaval<'b>) (value : asyncaval<'a>) =
        let mutable cache : option<_> = None
        let mutable innerCache : option<_> = None
        let mutable inputChanged = 0
        let inners : ref<HashSet<asyncaval<'b>>> = ref HashSet.empty

        { new AbstractVal<'b>() with
            override x.InputChangedObject(_, o) =
                if System.Object.ReferenceEquals(o, value) then
                    inputChanged <- 1
                    lock inners (fun () ->
                        for i in inners.Value do i.Outputs.Remove x |> ignore
                        inners.Value <- HashSet.empty
                    )
            
            member x.Compute t =
                if x.OutOfDate then
                    if Interlocked.Exchange(&inputChanged, 0) = 1 || Option.isNone cache then
                        let outerTask =
                            RefCountingTaskCreator(fun ct ->
                                let it = value.GetTask t
                                let s = ct.Register(fun () -> it.Cancel())
                                task {
                                    try
                                        let! i = it.Task
                                        let inner = mapping i ct
                                        return inner
                                    finally
                                        s.Dispose()
                                }
                            )
                        cache <- Some outerTask
                        
                    let outerTask = cache.Value
                    let ref = 
                        RefCountingTaskCreator(fun ct ->
                            let innerCellTask = outerTask.New()
                            let s = ct.Register(fun () -> innerCellTask.Cancel())
                            task {
                                try
                                    let! inner = innerCellTask.Task
                                    let innerTask = inner.GetTask t
                                    lock inners (fun () -> inners.Value <- HashSet.add inner inners.Value)
                                    let s2 =
                                        ct.Register(fun () ->
                                            innerTask.Cancel()
                                            lock inners (fun () -> inners.Value <- HashSet.remove inner inners.Value)
                                            inner.Outputs.Remove x |> ignore
                                        )
                                    try
                                        let! innerValue = innerTask.Task
                                        return innerValue
                                    finally
                                        s2.Dispose()
                                finally
                                    s.Dispose()
                            }    
                        )
                        
                    innerCache <- Some ref
                        
                    ref.New()
                else
                    innerCache.Value.New()
                    
        } :> asyncaval<_>
        