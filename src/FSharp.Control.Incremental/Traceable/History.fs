namespace FSharp.Control.Traceable

open System
open FSharp.Control.Incremental

type IOpReader<'ops> =
    inherit IAdaptiveObject
    abstract member GetOperations : AdaptiveToken -> 'ops

type IOpReader<'s, 'ops> =
    inherit IOpReader<'ops>
    abstract member State : 's

[<AbstractClass>]
type AbstractReader<'ops>(t : Monoid<'ops>) =
    inherit AdaptiveObject()

    abstract member Release : unit -> unit
    abstract member Compute : AdaptiveToken -> 'ops

    abstract member Apply : 'ops -> 'ops
    default x.Apply o = o

    member x.GetOperations(token : AdaptiveToken) =
        x.EvaluateAlways token (fun token ->
            if x.OutOfDate then
                x.Compute token |> x.Apply
            else
                t.mempty
        )   

    member x.Dispose() =
        x.Release()
        x.Outputs.Consume() |> ignore

    interface IDisposable with
        member x.Dispose() = x.Dispose()

    interface IOpReader<'ops> with
        member x.GetOperations c = x.GetOperations c

[<AbstractClass>]
type AbstractReader<'s, 'ops>(t : Traceable<'s, 'ops>) =
    inherit AbstractReader<'ops>(t.tmonoid)

    let mutable state = t.tempty

    override x.Apply o =
        let (s, o) = t.tintegrate state o
        state <- s
        o
    member x.State = state

    interface IOpReader<'s, 'ops> with
        member x.State = state

[<AbstractClass>]
type AbstractDirtyReader<'t, 'ops when 't :> IAdaptiveObject>(t : Monoid<'ops>) =
    inherit AdaptiveObject()

    let dirty = ref <| System.Collections.Generic.HashSet<'t>()

    override x.InputChanged(_, o) =
        match o with
        | :? 't as o -> lock dirty (fun () -> dirty.Value.Add o |> ignore)
        | _ -> ()

    abstract member Release : unit -> unit
    abstract member Compute : AdaptiveToken * System.Collections.Generic.HashSet<'t> -> 'ops

    abstract member Apply : 'ops -> 'ops
    default x.Apply o = o

    member x.GetOperations(token : AdaptiveToken) =
        x.EvaluateAlways token (fun token ->
            if x.OutOfDate then
                let dirty = 
                    lock dirty (fun () ->
                        let d = !dirty
                        dirty := System.Collections.Generic.HashSet()
                        d
                    )
                x.Compute(token, dirty) |> x.Apply
            else
                t.mempty
        )   

    member x.Dispose() =
        x.Release()
        x.Outputs.Consume() |> ignore

    interface IDisposable with
        member x.Dispose() = x.Dispose()

    interface IOpReader<'ops> with
        member x.GetOperations c = x.GetOperations c

[<AllowNullLiteral>]
type internal RelevantNode<'s, 'a> =
    class
        val mutable public Prev : WeakReference<RelevantNode<'s, 'a>>
        val mutable public Next : RelevantNode<'s, 'a>
        val mutable public RefCount : int
        val mutable public BaseState : 's
        val mutable public Value : 'a
            
        new(p, s, v, n) = { Prev = p; Next = n; RefCount = 0; BaseState = s; Value = v }
    end

type History<'s, 'op> private(input : Option<Lazy<IOpReader<'op>>>, t : Traceable<'s, 'op>, finalize : 'op -> unit) =
    inherit AdaptiveObject()

    let mutable state   : 's = t.tempty
    let mutable last    : WeakReference<RelevantNode<'s, 'op>> = null

    // TODO: find a way to collapse things again
    // tracking Weak first does not do the trick since it may die
    // while intermediate nodes still live

    let append (op : 'op) =
        // only append non-empty ops
        if not (t.tmonoid.misEmpty op) then
            // apply the op to the state
            let s, op = t.tintegrate state op
            state <- s

            // if op got empty do not append it
            if not (t.tmonoid.misEmpty op) then
                let mutable lv = null
                // if last is null no reader is interested in ops.
                // therefore we simply discard them here
                if not (isNull last) && last.TryGetTarget(&lv) then
                    // last is non-null and alive and no one pulled it yet
                    // so we can append our op to it
                    lv.Value <- t.tmonoid.mappend lv.Value op
                else
                    last <- null
                    finalize op

                true

            else
                false
        else
            false

    let addRefToLast() =
        let mutable lv = null
        if isNull last || not (last.TryGetTarget(&lv)) then
            // if there is no last (the history is empty) we append
            // a new empty last with no ops and set its refcount to 1
            let n = RelevantNode(null, state, t.tmonoid.mempty, null)
            n.RefCount <- 1
            let wn = WeakReference<_> n
            last <- wn
            n
        else
            if t.tmonoid.misEmpty lv.Value then
                // if last has no ops we can reuse it here
                lv.RefCount <- lv.RefCount + 1
                lv
            else
                // if last contains ops we just consumed it and therefore
                // need a new empty last
                let n = RelevantNode(last, state, t.tmonoid.mempty, null)
                lv.Next <- n
                last <- WeakReference<_> n
                n.RefCount <- 1
                n
              
    let mergeIntoPrev (node : RelevantNode<'s, 'op>) =
        if node.RefCount = 1 then
            let res = node.Value
            let next = node.Next
            let prev = node.Prev
            
            // kill the node
            finalize node.Value
            node.Value <- Unchecked.defaultof<_>
            node.Prev <- null
            node.Next <- null
            node.RefCount <- -1

            // detach ourselves
            if isNull next then last <- prev
            else next.Prev <- prev
            let mutable prevValue = null
            if not (isNull prev) && prev.TryGetTarget(&prevValue) then
                // if prev is still relevant we merge our ops into it.
                // this is sound since the reader holding it would have seen the
                // operations anyway.
                prevValue.Next <- next
                prevValue.Value <- t.tmonoid.mappend prevValue.Value res

            res, next

        else
            node.RefCount <- node.RefCount - 1
            node.Value, node.Next      

    let isInvalid (node : RelevantNode<'s, 'op>) =
        isNull node || node.RefCount < 0

    member private x.Update (self : AdaptiveToken) =
        if x.OutOfDate then
            match input with
                | Some c -> 
                    let v = c.Value.GetOperations self
                    append v |> ignore
                | None ->
                    ()

    member x.State = state

    member x.Trace = t

    member x.Perform(op : 'op) =
        let changed = lock x (fun () -> append op)
        if changed then
            x.MarkOutdated()
            true
        else
            false

    member internal x.Read(token : AdaptiveToken, old : RelevantNode<'s, 'op>, oldState : 's) =
        x.EvaluateAlways token (fun token ->
            x.Update token

            if isInvalid old then
                let ops = t.tdifferentiate oldState state
                let node = addRefToLast()

                node, ops
            else
                let mutable res = t.tmonoid.mempty
                let mutable current = old

                while not (isNull current) do
                    let (o,c) = mergeIntoPrev current
                    res <- t.tmonoid.mappend res o
                    current <- c

                let node = addRefToLast()
                node, res
        )
        
    member x.GetValue(token : AdaptiveToken) =
        x.EvaluateAlways token (fun token ->
            x.Update token
            state
        )

    member x.NewReader() =
        new HistoryReader<'s, 'op>(x) :> IOpReader<'s, 'op>

    interface aref<'s> with
        member x.GetValue t = x.GetValue t

    new (t : Traceable<'s, 'op>, finalize : 'op -> unit) = History<'s, 'op>(None, t, finalize)
    new (input : unit -> IOpReader<'op>, t : Traceable<'s, 'op>, finalize : 'op -> unit) = History<'s, 'op>(Some (lazy (input())), t, finalize)
    new (t : Traceable<'s, 'op>) = History<'s, 'op>(None, t, ignore)
    new (input : unit -> IOpReader<'op>, t : Traceable<'s, 'op>) = History<'s, 'op>(Some (lazy (input())), t, ignore)

and internal HistoryReader<'s, 'op>(h : History<'s, 'op>) =
    inherit AdaptiveObject()
    let trace = h.Trace
    let mutable node : RelevantNode<'s, 'op> = null
    let mutable state = trace.tempty

    member x.GetOperations(token : AdaptiveToken) =
        x.EvaluateAlways token (fun token ->
            if x.OutOfDate then
                let nt, ops = h.Read(token, node, state)
                node <- nt
                state <- h.State
                ops
            else
                trace.tmonoid.mempty
        )

    interface IOpReader<'op> with
        member x.GetOperations c = x.GetOperations c

    interface IOpReader<'s, 'op> with
        member x.State = state

module History =

    module Readers =
        type EmptyReader<'s, 'ops>(t : Traceable<'s, 'ops>) =
            inherit ConstantObject()

            interface IOpReader<'ops> with
                member x.GetOperations(caller) = t.tmonoid.mempty
    
            interface IOpReader<'s, 'ops> with
                member x.State = t.tempty

        type ConstantReader<'s, 'ops>(t : Traceable<'s, 'ops>, ops : Lazy<'ops>, finalState : Lazy<'s>) =
            inherit ConstantObject()
            
            let mutable state = t.tempty
            let mutable initial = true

            interface IOpReader<'ops> with
                member x.GetOperations(caller) =
                    lock x (fun () ->
                        if initial then
                            initial <- false
                            state <- finalState.Value
                            ops.Value
                        else
                            t.tmonoid.mempty
                    )

            interface IOpReader<'s, 'ops> with
                member x.State = state
    
    let ofReader (t : Traceable<'s, 'ops>) (newReader : unit -> IOpReader<'ops>) =
        History<'s, 'ops>(newReader, t)

