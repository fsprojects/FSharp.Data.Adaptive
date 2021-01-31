namespace FSharp.Data.Traceable

open System
open FSharp.Data.Adaptive


/// An adaptive reader that allows to get operations since the last evaluation
type IOpReader<'Delta> =
    inherit IAdaptiveObject

    /// Dependency-aware evaluation of the reader
    abstract member GetChanges: AdaptiveToken -> 'Delta

/// An adaptive reader thath allows to get operations and also exposes its current state.
[<Interface>]
type IOpReader<'State, 'Delta> =
    inherit IOpReader<'Delta>

    /// The Traceable instance for the reader.
    abstract member Trace : Traceable<'State, 'Delta>

    /// The latest state of the Reader.
    /// Note that the state gets updated after each evaluation (GetChanges)
    abstract member State: 'State

/// Abstract base class for implementing IOpReader<_>
[<AbstractClass>]
type AbstractReader<'Delta>(empty: 'Delta) =
    inherit AdaptiveObject()
    
    /// Adaptively compute deltas.
    abstract member Compute: AdaptiveToken -> 'Delta

    /// Applies the delta to the current state and returns the 'effective' delta.
    abstract member Apply: 'Delta -> 'Delta
    default x.Apply o = o
    
    /// Adaptively get the latest deltas (or empty if up-to-date).
    member x.GetChanges(token: AdaptiveToken) =
        x.EvaluateAlways token (fun token ->
            if x.OutOfDate then
                x.Compute token |> x.Apply
            else
                empty
        )   

    interface IOpReader<'Delta> with
        member x.GetChanges c = x.GetChanges c

/// Abstract base class for implementing IOpReader<_,_>
[<AbstractClass>]
type AbstractReader<'State, 'Delta>(trace: Traceable<'State, 'Delta>) =
    inherit AbstractReader<'Delta>(trace.tmonoid.mempty)

    let mutable state = trace.tempty

    /// Applies the delta to the current state and returns the 'effective' delta.
    override x.Apply o =
        let (s, o) = trace.tapplyDelta state o
        state <- s
        o

    /// The reader's current content.
    member x.State = state

    interface IOpReader<'State, 'Delta> with
        member x.Trace = trace
        member x.State = state

/// Abstract base class for implementing IOpReader<_> when dirty inputs are needed on evaluation.
[<AbstractClass>]
type AbstractDirtyReader<'T, 'Delta when 'T :> IAdaptiveObject>(t: Monoid<'Delta>, take : obj -> bool) =
    inherit AdaptiveObject()

    let dirty = ref <| DefaultHashSet.create<'T>()

    override x.InputChangedObject(_, o) =
        #if FABLE_COMPILER
        if take o.Tag then dirty.Value.Add (unbox<'T> o) |> ignore
        #else
        match o with
        | :? 'T as o when take o.Tag -> lock dirty (fun () -> dirty.Value.Add o |> ignore)
        | _ -> ()
        #endif


    /// Adaptively compute deltas.
    abstract member Compute: AdaptiveToken * System.Collections.Generic.HashSet<'T> -> 'Delta

    /// Applies the delta to the current state and returns the 'effective' delta.
    abstract member Apply: 'Delta -> 'Delta
    default x.Apply o = o

    /// Adaptively get the latest deltas (or empty if up-to-date).
    member x.GetChanges(token: AdaptiveToken) =
        x.EvaluateAlways token (fun token ->
            if x.OutOfDate then
                let dirty = 
                    lock dirty (fun () ->
                        let d = !dirty
                        dirty := DefaultHashSet.create()
                        d
                    )
                x.Compute(token, dirty) |> x.Apply
            else
                t.mempty
        )   

    interface IOpReader<'Delta> with
        member x.GetChanges c = x.GetChanges c

/// Linked list node used by the system to represent a 'version' in the History
[<AllowNullLiteral>]
type internal RelevantNode<'State, 'T> =
    class
        val mutable public Prev: WeakReference<RelevantNode<'State, 'T>>
        val mutable public Next: RelevantNode<'State, 'T>
        val mutable public RefCount: int
        val mutable public BaseState: 'State
        val mutable public Value: 'T
            
        new(p, s, v, n) = { Prev = p; Next = n; RefCount = 0; BaseState = s; Value = v }
    end

/// History and HistoryReader are the central implementation for traceable data-types.
/// The allow to construct a dependent History (by passing an input-reader) or imperatively
/// performing operations on the history while keeping track of all output-versions that may exist.
type History<'State, 'Delta> private(input: option<Lazy<IOpReader<'Delta>>>, t: Traceable<'State, 'Delta>, finalize: 'Delta -> unit) =
    inherit AdaptiveObject()

    /// The current state of the History
    let mutable state  : 'State = t.tempty

    /// The (weak) latest version known in the history
    let mutable last   : WeakReference<RelevantNode<'State, 'Delta>> = null
    
    let mutable appendCounter = 0

    /// Gets the predecessor for a given node (null if none).
    let getPrev (node: RelevantNode<'State, 'Delta>) =
        if isNull node || isNull node.Prev then 
            null
        else
            match node.Prev.TryGetTarget() with
            | (true, prev) -> prev
            | _ -> null

    /// Gets the first living node and the accumulated operation-size.
    let getFirstAndSize() =
        let mutable first = null
        if not (isNull last) then
            match last.TryGetTarget() with
            | (true, first) ->
                let mutable first = first
                let mutable size = t.tsize first.Value
                let mutable prev = getPrev first
                while not (isNull prev) do
                    size <- size + t.tsize prev.Value
                    first <- prev
                    prev <- getPrev first

                struct (first, size)
            | _ ->
                struct (null, 0)
        else
            struct (null, 0)

    /// Destroys nodes recursicely until shouldPrune returns false or the history is empty.
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

    /// Prunes the history if needed
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

    /// Appends operations to the history and updates the state
    /// Returns whether or not the operation effectively changed the state
    let append (op: 'Delta) =
        // only append non-empty ops
        if not (t.tmonoid.misEmpty op) then
            // apply the op to the state
            let s, op = t.tapplyDelta state op
            state <- s

            // if op got empty do not append it
            if not (t.tmonoid.misEmpty op) then
                // if last is null no reader is interested in ops.
                // therefore we simply discard them here
                if not (isNull last) then
                    match last.TryGetTarget() with
                    | (true, lv) ->
                        // last is non-null and alive and no one pulled it yet
                        // so we can append our op to it
                        lv.Value <- t.tmonoid.mappend lv.Value op
                    | _ -> 
                        last <- null
                        finalize op
                else
                    last <- null
                    finalize op

                prune()
                true

            else
                false
        else
            false


    /// Appends operations to the history and updates the state
    /// Returns whether or not the operation effectively changed the state
    let appendUnsafe (s : 'State) (op: 'Delta) =
        // assume the delta is already sound!!!!
        state <- s

        // only append non-empty ops
        if not (t.tmonoid.misEmpty op) then
            // if last is null no reader is interested in ops.
            // therefore we simply discard them here
            if not (isNull last) then
                match last.TryGetTarget() with
                | (true, lv) ->
                    // last is non-null and alive and no one pulled it yet
                    // so we can append our op to it
                    lv.Value <- t.tmonoid.mappend lv.Value op
                | _ -> 
                    last <- null
                    finalize op
            else
                last <- null
                finalize op

            prune()
            true
        else
            false


    /// Adds a reference to the latest version or creates one.
    /// Returns the RelevantNode representing the latest version
    let addRefToLast() =
        if not (isNull last) then
            match last.TryGetTarget() with
            | (true, lv) ->
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
            | _ ->
                 // if there is no last (the history is empty) we append
                // a new empty last with no ops and set its refcount to 1
                let n = RelevantNode(null, state, t.tmonoid.mempty, null)
                n.RefCount <- 1
                let wn = WeakReference<_> n
                last <- wn
                n
        else
            // if there is no last (the history is empty) we append
            // a new empty last with no ops and set its refcount to 1
            let n = RelevantNode(null, state, t.tmonoid.mempty, null)
            n.RefCount <- 1
            let wn = WeakReference<_> n
            last <- wn
            n
              
    /// Merges the ops in node into its predecessor (if any) and deletes the node from the History.
    /// Returns the next version and the operations from the (deleted) node
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
            if not (isNull prev) then  
                match prev.TryGetTarget() with
                | (true, prevValue) -> 
                    // if prev is still relevant we merge our ops into it.
                    // this is sound since the reader holding it would have seen the
                    // operations anyway.
                    prevValue.Next <- next
                    prevValue.Value <- t.tmonoid.mappend prevValue.Value res
                | _ ->
                    ()
            res, next

        else
            node.RefCount <- node.RefCount - 1
            node.Value, node.Next      

    /// Determines whether or not the node is invalid
    let isInvalid (node: RelevantNode<'State, 'Delta>) =
        isNull node || node.RefCount < 0

    /// Used internally to pull the latest deltas from the input and append them to the history
    member private x.Update (self: AdaptiveToken) =
        if x.OutOfDate then
            match input with
                | Some c -> 
                    let v = c.Value.GetChanges self
                    append v |> ignore
                | None ->
                    ()

    /// The current state of the history
    member x.State = state

    /// The traceable instance used by the history
    member x.Trace = t

    /// Imperatively performs operations on the history (similar to ModRef.Value <- ...).
    /// Since the history may need to be marked a Transaction needs to be current.
    member x.Perform(op: 'Delta) =
        let changed = lock x (fun () -> append op)
        if changed then
            x.MarkOutdated()
            true
        else
            false

    /// Imperatively performs operations on the history (similar to ModRef.Value <- ...)
    /// and assumes that newState represents the current history-state with the given operations applied. (hence the Unsafe suffix)
    /// Since the history may need to be marked a Transaction needs to be current.
    member x.PerformUnsafe(newState : 'State, op: 'Delta) =
        let changed = lock x (fun () -> appendUnsafe newState op)
        if changed then
            x.MarkOutdated()
            true
        else
            false


    /// Used by HistoryReader to pull the operations since the old RelevantNode.
    /// Additionaly the reader provides its latest state. 
    /// This way the history can computeDelta the state in case it decided to drop the old version.
    member internal x.Read(token: AdaptiveToken, old: RelevantNode<'State, 'Delta>, oldState: 'State) =
        x.EvaluateAlways token (fun token ->
            x.Update token

            if isInvalid old then
                let ops = t.tcomputeDelta oldState state
                let node = addRefToLast()

                node, ops
            else
                let mutable res = t.tmonoid.mempty
                let mutable current = old

                while not (isNull current) do
                    let (o,c) = mergeIntoPrev current
                    res <- t.tmonoid.mappend res o
                    current <- c
                    if not (isNull c) then 
                        c.RefCount <- c.RefCount + 1

                let node = addRefToLast()
                node, res
        )
        
    /// Adaptively gets the history'State current state
    member x.GetValue(token: AdaptiveToken) =
        x.EvaluateAlways token (fun token ->
            x.Update token
            state
        )

    /// Creates a new reader on the history
    member x.NewReader() =
        let reader = new HistoryReader<'State, 'Delta>(x) 
        reader :> IOpReader<'State, 'Delta>
        
    /// Creates a new reader on the history
    member x.NewReader(trace : Traceable<'ViewState, 'ViewDelta>, mapping : 'State -> 'Delta -> 'ViewDelta) =
        let reader = new HistoryReader<'State, 'Delta, 'ViewState, 'ViewDelta>(x, mapping, trace) 
        reader :> IOpReader<'ViewState, 'ViewDelta>
         
    /// Creates a new reader on the history
    member x.NewReader(trace : Traceable<'ViewState, 'ViewDelta>, mapping : 'Delta -> 'ViewDelta) =
        let reader = new HistoryReader<'State, 'Delta, 'ViewState, 'ViewDelta>(x, (fun _ v -> mapping v), trace) 
        reader :> IOpReader<'ViewState, 'ViewDelta>
                   
    interface IAdaptiveValue with
        member x.Accept (v : IAdaptiveValueVisitor<'R>) = v.Visit x
        member x.GetValueUntyped t = x.GetValue t :> obj
        member x.ContentType = 
            #if FABLE_COMPILER
            typeof<obj>
            #else
            typeof<'State>
            #endif

    interface IAdaptiveValue<'State> with
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
        member x.Trace = trace
        member x.State = state

/// HistoryReader implements IOpReader<_,_> and takes care of managing versions correctly.
and internal HistoryReader<'State, 'Delta, 'ViewState, 'ViewDelta>(h: History<'State, 'Delta>, mapping : 'State -> 'Delta -> 'ViewDelta, trace : Traceable<'ViewState, 'ViewDelta>) =
    inherit AdaptiveObject()
    //let trace = h.Trace
    let mutable node: RelevantNode<'State, 'Delta> = null

    let mutable state = h.Trace.tempty
    let mutable viewState = trace.tempty
    let mapping = OptimizedClosures.FSharpFunc<_,_,_>.Adapt mapping
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
                let vops = mapping.Invoke(state, ops)
                let s, vops = trace.tapplyDelta viewState vops
                viewState <- s
                vops
            else
                trace.tmonoid.mempty
        )

    interface IOpReader<'ViewDelta> with
        member x.GetChanges c = x.GetChanges c

    interface IOpReader<'ViewState, 'ViewDelta> with
        member x.Trace = trace
        member x.State = viewState

/// Functional operators related to the History<_,_> type.
module History =
    
    /// Simple base-types for reader implementations.
    module Readers =
        /// The empty reader.
        type EmptyReader<'State, 'Delta>(t: Traceable<'State, 'Delta>) =
            inherit ConstantObject()

            interface IOpReader<'Delta> with
                member x.GetChanges(_caller) = t.tmonoid.mempty
    
            interface IOpReader<'State, 'Delta> with
                member x.Trace = t
                member x.State = t.tempty

        /// A constant reader.
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
                member x.Trace = t
                member x.State = state
    
    /// Creates a history depending on the given reader. 
    /// The history will internally use the given traceable instance.
    let ofReader (t: Traceable<'State, 'Delta>) (newReader: unit -> IOpReader<'Delta>) =
        History<'State, 'Delta>(newReader, t)

