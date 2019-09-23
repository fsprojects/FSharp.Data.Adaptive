namespace FSharp.Control.Traceable

open System
open FSharp.Control.Incremental

/// an adaptive reader that allows to get operations since the last evaluation
type IOpReader<'Delta> =
    inherit IAdaptiveObject

    /// dependency-aware evaluation of the reader
    abstract member GetChanges: AdaptiveToken -> 'Delta

/// an adaptive reader thath allows to get operations and also exposes its current state.
[<Interface>]
type IOpReader<'State, 'Delta> =
    inherit IOpReader<'Delta>

    /// the latest state of the Reader.
    /// note that the state gets updated after each evaluation (GetChanges)
    abstract member State: 'State

/// abstract base class for implementing IOpReader<_>
[<AbstractClass>]
type AbstractReader<'Delta>(t: Monoid<'Delta>) =
    inherit AdaptiveObject()

    abstract member Compute: AdaptiveToken -> 'Delta

    abstract member Apply: 'Delta -> 'Delta
    default x.Apply o = o

    member x.GetChanges(token: AdaptiveToken) =
        x.EvaluateAlways token (fun token ->
            if x.OutOfDate then
                x.Compute token |> x.Apply
            else
                t.mempty
        )   

    interface IOpReader<'Delta> with
        member x.GetChanges c = x.GetChanges c

/// abstract base class for implementing IOpReader<_,_>
[<AbstractClass>]
type AbstractReader<'State, 'Delta>(t: Traceable<'State, 'Delta>) =
    inherit AbstractReader<'Delta>(t.tmonoid)

    let mutable state = t.tempty

    override x.Apply o =
        let (s, o) = t.tintegrate state o
        state <- s
        o
    member x.State = state

    interface IOpReader<'State, 'Delta> with
        member x.State = state

/// abstract base class for implementing IOpReader<_> when dirty inputs are needed on evaluation.
[<AbstractClass>]
type AbstractDirtyReader<'T, 'Delta when 'T :> IAdaptiveObject>(t: Monoid<'Delta>) =
    inherit AdaptiveObject()

    let dirty = ref <| System.Collections.Generic.HashSet<'T>()

    override x.InputChanged(_, o) =
        match o with
        | :? 'T as o -> lock dirty (fun () -> dirty.Value.Add o |> ignore)
        | _ -> ()

    abstract member Compute: AdaptiveToken * System.Collections.Generic.HashSet<'T> -> 'Delta

    abstract member Apply: 'Delta -> 'Delta
    default x.Apply o = o

    member x.GetChanges(token: AdaptiveToken) =
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

    interface IOpReader<'Delta> with
        member x.GetChanges c = x.GetChanges c

/// linked list node used by the system to represent a 'version' in the History
[<AllowNullLiteral>]
type internal RelevantNode<'State, 'a> =
    class
        val mutable public Prev: WeakReference<RelevantNode<'State, 'a>>
        val mutable public Next: RelevantNode<'State, 'a>
        val mutable public RefCount: int
        val mutable public BaseState: 'State
        val mutable public Value: 'a
            
        new(p, s, v, n) = { Prev = p; Next = n; RefCount = 0; BaseState = s; Value = v }
    end

/// History and HistoryReader are the central implementation for traceable data-types.
/// The allow to construct a dependent History (by passing an input-reader) or imperatively
/// performing operations on the history while keeping track of all output-versions that may exist.
type History<'State, 'Delta> private(input: option<Lazy<IOpReader<'Delta>>>, t: Traceable<'State, 'Delta>, finalize: 'Delta -> unit) =
    inherit AdaptiveObject()

    /// the current state of the History
    let mutable state  : 'State = t.tempty

    /// the (weak) latest version known in the history
    let mutable last   : WeakReference<RelevantNode<'State, 'Delta>> = null
    
    let mutable appendCounter = 0

    /// gets the predecessor for a given node (null if none).
    let getPrev (node: RelevantNode<'State, 'Delta>) =
        if isNull node || isNull node.Prev then 
            null
        else
            match node.Prev.TryGetTarget() with
            | (true, prev) -> prev
            | _ -> null

    /// gets the first living node and the accumulated operation-size.
    let getFirstAndSize() =
        let mutable first = null
        if not (isNull last) && last.TryGetTarget(&first) then
            let mutable size = t.tsize first.Value
            let mutable prev = getPrev first
            while not (isNull prev) do
                size <- size + t.tsize prev.Value
                first <- prev
                prev <- getPrev first

            struct (first, size)
        else
            struct (null, 0)

    /// destroys nodes recursicely until shouldPrune returns false or the history is empty.
    let rec pruneNode (shouldPrune: 'State -> int -> bool) (totalDeltaSize: int) (first: RelevantNode<'State, 'Delta>) =
        if not (isNull first) && shouldPrune first.BaseState totalDeltaSize then
            let size = t.tsize first.Value
            let next = first.Next

            // destroy the node
            first.RefCount <- -1
            if isNull first.Next then last <- null
            else first.Next.Prev <- null
            first.Next <- null
            first.Prev <- null
            first.BaseState <- Unchecked.defaultof<_>
            first.Value <- Unchecked.defaultof<_>

            // continue
            pruneNode shouldPrune (totalDeltaSize - size) next

    /// prunes the history if needed
    let prune () =
        if appendCounter > 100 then
            appendCounter <- 0
            match t.tprune with
            | Some shouldPrune ->
                let struct (first, totalDeltaSize) = getFirstAndSize()
                pruneNode shouldPrune totalDeltaSize first
            | None ->
                ()
        else
            appendCounter <- appendCounter + 1

    /// appends operations to the history and updates the state
    /// returns whether or not the operation effectively changed the state
    let append (op: 'Delta) =
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

                prune()
                true

            else
                false
        else
            false

    /// adds a reference to the latest version or creates one.
    /// returns the RelevantNode representing the latest version
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
              
    /// merges the ops in node into its predecessor (if any) and deletes the node from the History.
    /// returns the next version and the operations from the (deleted) node
    let mergeIntoPrev (node: RelevantNode<'State, 'Delta>) =
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

    /// determines whether or not the node is invalid
    let isInvalid (node: RelevantNode<'State, 'Delta>) =
        isNull node || node.RefCount < 0

    /// used internally to pull the latest deltas from the input and append them to the history
    member private x.Update (self: AdaptiveToken) =
        if x.OutOfDate then
            match input with
                | Some c -> 
                    let v = c.Value.GetChanges self
                    append v |> ignore
                | None ->
                    ()

    /// the current state of the history
    member x.State = state

    /// the traceable instance used by the history
    member x.Trace = t

    /// imperatively performs operations on the history (similar to ModRef.Value <- ...).
    /// since the history may need to be marked a Transaction needs to be current.
    member x.Perform(op: 'Delta) =
        let changed = lock x (fun () -> append op)
        if changed then
            x.MarkOutdated()
            true
        else
            false

    /// used by HistoryReader to pull the operations since the old RelevantNode.
    /// additionaly the reader provides its latest state. 
    /// this way the history can differentiate the state in case it decided to drop the old version.
    member internal x.Read(token: AdaptiveToken, old: RelevantNode<'State, 'Delta>, oldState: 'State) =
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
        
    /// adaptively gets the history'State current state
    member x.GetValue(token: AdaptiveToken) =
        x.EvaluateAlways token (fun token ->
            x.Update token
            state
        )

    /// creates a new reader on the history
    member x.NewReader() =
        let reader = new HistoryReader<'State, 'Delta>(x) 
        reader :> IOpReader<'State, 'Delta>

    interface aref<'State> with
        member x.GetValue t = x.GetValue t

    new (t: Traceable<'State, 'Delta>, finalize: 'Delta -> unit) = History<'State, 'Delta>(None, t, finalize)
    new (input: unit -> IOpReader<'Delta>, t: Traceable<'State, 'Delta>, finalize: 'Delta -> unit) = History<'State, 'Delta>(Some (lazy (input())), t, finalize)
    new (t: Traceable<'State, 'Delta>) = History<'State, 'Delta>(None, t, ignore)
    new (input: unit -> IOpReader<'Delta>, t: Traceable<'State, 'Delta>) = History<'State, 'Delta>(Some (lazy (input())), t, ignore)

/// HistoryReader implements IOpReader<_,_> and takes care of managing versions correctly.
and internal HistoryReader<'State, 'Delta>(h: History<'State, 'Delta>) =
    inherit AdaptiveObject()
    let trace = h.Trace
    let mutable node: RelevantNode<'State, 'Delta> = null
    let mutable state = trace.tempty

    member x.RelevantNode = 
        node

    member x.DestroyRelevantNode() =
        node <- null

    member x.GetChanges(token: AdaptiveToken) =
        x.EvaluateAlways token (fun token ->
            if x.OutOfDate then
                let nt, ops = h.Read(token, node, state)
                node <- nt
                state <- h.State
                ops
            else
                trace.tmonoid.mempty
        )

    interface IOpReader<'Delta> with
        member x.GetChanges c = x.GetChanges c

    interface IOpReader<'State, 'Delta> with
        member x.State = state

/// Functional operators related to the History<_,_> type.
module History =

    module Readers =
        type EmptyReader<'State, 'Delta>(t: Traceable<'State, 'Delta>) =
            inherit ConstantObject()

            interface IOpReader<'Delta> with
                member x.GetChanges(_caller) = t.tmonoid.mempty
    
            interface IOpReader<'State, 'Delta> with
                member x.State = t.tempty

        type ConstantReader<'State, 'Delta>(t: Traceable<'State, 'Delta>, ops: Lazy<'Delta>, finalState: Lazy<'State>) =
            inherit ConstantObject()
            
            let mutable state = t.tempty
            let mutable initial = true

            interface IOpReader<'Delta> with
                member x.GetChanges(caller) =
                    lock x (fun () ->
                        if initial then
                            initial <- false
                            state <- finalState.Value
                            ops.Value
                        else
                            t.tmonoid.mempty
                    )

            interface IOpReader<'State, 'Delta> with
                member x.State = state
    
    /// creates a history depending on the given reader. 
    /// the history will internally use the given traceable instance.
    let ofReader (t: Traceable<'State, 'Delta>) (newReader: unit -> IOpReader<'Delta>) =
        History<'State, 'Delta>(newReader, t)

