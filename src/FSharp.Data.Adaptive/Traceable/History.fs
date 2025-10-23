namespace FSharp.Data.Traceable

open System
open FSharp.Data.Adaptive


/// An adaptive reader that provides incremental access to changes (deltas).
/// This is the foundation of the incremental update system in FSharp.Data.Adaptive.
///
/// Readers maintain a position in the history and return only the changes since their
/// last GetChanges call, enabling efficient O(k) updates where k = number of changes,
/// rather than O(n) full recomputations where n = total collection size.
type IOpReader<'Delta> =
    inherit IAdaptiveObject

    /// Adaptively gets the changes (delta) since the last evaluation.
    /// Returns an empty delta if the reader is up-to-date.
    /// The token tracks dependencies for automatic change propagation.
    abstract member GetChanges: AdaptiveToken -> 'Delta

/// An adaptive reader that provides both incremental changes and the current state.
/// This is used by alist, aset, and amap to provide efficient incremental updates.
///
/// The reader maintains:
/// - Current state after applying all deltas
/// - Position in the version history
/// - Only deltas since the last read
[<Interface>]
type IOpReader<'State, 'Delta> =
    inherit IOpReader<'Delta>

    /// The Traceable instance defining how states and deltas interact.
    /// Provides operations like applying deltas, combining deltas, computing diffs, etc.
    abstract member Trace : Traceable<'State, 'Delta>

    /// The current state of the reader after applying all deltas.
    /// This state is updated incrementally each time GetChanges is called.
    /// Time complexity: O(1) access, state maintained incrementally.
    abstract member State: 'State

/// Abstract base class for implementing custom incremental readers.
/// Provides the core logic for tracking changes and returning deltas.
///
/// Subclasses only need to implement Compute to define how to calculate changes.
/// The base class handles:
/// - Caching (returns empty delta when up-to-date)
/// - Dependency tracking through AdaptiveToken
/// - Optional delta transformation via Apply
[<AbstractClass>]
type AbstractReader<'Delta>(empty: 'Delta) =
    inherit AdaptiveObject()

    /// Computes the delta since the last evaluation.
    /// Called only when the reader is out-of-date.
    /// Subclasses implement this to define incremental update logic.
    abstract member Compute: AdaptiveToken -> 'Delta

    /// Optionally transforms the computed delta before returning it.
    /// Default implementation returns the delta unchanged.
    /// Override to implement delta filtering, normalization, or other transformations.
    abstract member Apply: 'Delta -> 'Delta
    default x.Apply o = o

    /// Adaptively gets the latest deltas (or empty if up-to-date).
    /// Returns empty delta when nothing changed, computed delta when out-of-date.
    /// Time complexity: O(1) when cached, O(compute cost) when invalidated.
    member x.GetChanges(token: AdaptiveToken) =
        x.EvaluateAlways token (fun token ->
            if x.OutOfDate then
                x.Compute token |> x.Apply
            else
                empty
        )

    interface IOpReader<'Delta> with
        member x.GetChanges c = x.GetChanges c

/// Abstract base class for implementing stateful incremental readers.
/// Maintains current state and automatically applies deltas to update it.
///
/// This is the typical base class for alist/aset/amap operations like map, filter, etc.
/// The base class handles:
/// - State management (maintained incrementally)
/// - Delta application via Traceable
/// - Returning effective deltas after state updates
[<AbstractClass>]
type AbstractReader<'State, 'Delta>(trace: Traceable<'State, 'Delta>) =
    inherit AbstractReader<'Delta>(trace.tmonoid.mempty)

    let mutable state = trace.tempty

    /// Applies the delta to the current state and returns the 'effective' delta.
    /// The effective delta reflects what actually changed (e.g., filtered results).
    /// Time complexity: O(k) where k = size of delta
    override x.Apply o =
        let (s, o) = trace.tapplyDelta state o
        state <- s
        o

    /// The reader's current state after applying all deltas.
    /// Maintained incrementally, always reflects the latest accumulated state.
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
                        let d = dirty.Value
                        dirty.Value <- DefaultHashSet.create()
                        d
                    )
                x.Compute(token, dirty) |> x.Apply
            else
                t.mempty
        )   

    interface IOpReader<'Delta> with
        member x.GetChanges c = x.GetChanges c

/// Linked list node representing a version in the History chain.
/// Each node contains:
/// - BaseState: The state at this version
/// - Value: The delta (changes) from the previous version
/// - Prev/Next: Weak references forming the version chain
/// - RefCount: Number of readers currently at this version
///
/// This structure enables O(k) access where k = changes since last read.
/// Nodes with RefCount = 0 can be garbage collected.
/// Weak references allow automatic cleanup of unreferenced versions.
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

/// History is THE central mechanism that makes incremental adaptive collections work.
///
/// It maintains a doubly-linked version chain where each node contains:
/// - A delta (the changes that occurred)
/// - The state after applying the delta
/// - Weak references to previous/next versions
/// - Reference count of readers at this version
///
/// Key features:
/// - Multiple readers can be at different versions simultaneously
/// - Each reader gets O(k) access to deltas where k = changes since last read
/// - Automatic pruning removes versions no readers need
/// - Weak references allow GC of unreferenced parts of the history
/// - Can be driven by an input reader (dependent) or imperatively (via Perform)
/// - Efficiently handles many readers on the same history
///
/// This is how alist/aset/amap provide efficient incremental updates.
/// When you call GetChanges on a reader, it walks from its last version
/// to the current version, accumulating only the deltas it needs.
///
/// Memory management:
/// - Nodes are pruned when no readers reference them (every 100 appends)
/// - Weak references allow GC of dead readers
/// - Single-reader case is optimized (deltas accumulated in current node)
type History<'State, 'Delta> private(input: voption<Lazy<IOpReader<'Delta>>>, t: Traceable<'State, 'Delta>, finalize: 'Delta -> unit) =
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
                | ValueSome c -> 
                    let v = c.Value.GetChanges self
                    append v |> ignore
                | ValueNone ->
                    ()

    /// The current state of the history after applying all operations.
    /// This is the "latest" version that new readers will start from.
    /// Time complexity: O(1)
    member x.State = state

    /// The Traceable instance defining how states and deltas interact.
    /// Provides operations like applying deltas, combining deltas, pruning, etc.
    member x.Trace = t

    /// Imperatively performs an operation on the history.
    /// This is how changeable collections (clist, cset, cmap) update their history.
    ///
    /// The operation is:
    /// 1. Applied to the current state
    /// 2. Appended to the version chain (if non-empty)
    /// 3. Made available to all readers via GetChanges
    ///
    /// Must be called within a transaction (like cval.Value <- ...).
    /// Returns true if the operation effectively changed the state.
    ///
    /// Example:
    ///     transact (fun () -> history.Perform(IndexListDelta.add index value))
    member x.Perform(op: 'Delta) =
        let changed = lock x (fun () -> append op)
        if changed then
            x.MarkOutdated()
            true
        else
            false

    /// Imperatively performs an operation and directly sets the new state.
    /// This is an optimization when you've already computed the new state.
    ///
    /// UNSAFE because it assumes newState is correct (state after applying op).
    /// Only use when you've computed the new state yourself for performance.
    /// Must be called within a transaction.
    /// Returns true if the operation effectively changed the state.
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
        
    /// Adaptively gets the history's current state.
    /// If the history has an input reader, pulls changes from it first.
    /// Returns the current state after all operations have been applied.
    /// Time complexity: O(1) if up-to-date, O(k) if pulling k changes from input
    member x.GetValue(token: AdaptiveToken) =
        x.EvaluateAlways token (fun token ->
            x.Update token
            state
        )

    /// Creates a new reader starting at the current version.
    /// The reader will track its position and return only incremental changes on GetChanges.
    /// Multiple readers can coexist, each maintaining their own position.
    ///
    /// This is how alist.GetReader(), aset.GetReader(), etc. work internally.
    /// Time complexity: O(1)
    member x.NewReader() =
        let reader = new HistoryReader<'State, 'Delta>(x)
        reader :> IOpReader<'State, 'Delta>

    /// Creates a new reader with a mapping to a different view.
    /// Useful for implementing derived operations that need different state/delta types.
    ///
    /// The mapping function transforms each delta before the reader returns it.
    /// The trace defines the view's state and delta algebra.
    ///
    /// Example: mapping IndexListDelta to HashSetDelta for AList.toASet
    member x.NewReader(trace : Traceable<'ViewState, 'ViewDelta>, mapping : 'State -> 'Delta -> 'ViewDelta) =
        let reader = new HistoryReader<'State, 'Delta, 'ViewState, 'ViewDelta>(x, mapping, trace)
        reader :> IOpReader<'ViewState, 'ViewDelta>

    /// Creates a new reader with a stateless delta mapping.
    /// Simpler overload when the mapping doesn't need the current state.
    ///
    /// Example: filtering deltas, transforming element types, etc.
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

    /// Creates an imperative history (no input reader).
    /// Used by changeable collections (clist, cset, cmap).
    /// Operations are added via Perform().
    /// The finalize callback is called on deltas when they're discarded.
    new (t: Traceable<'State, 'Delta>, finalize: 'Delta -> unit) = History<'State, 'Delta>(ValueNone, t, finalize)

    /// Creates a dependent history driven by an input reader.
    /// Used by derived operations (map, filter, etc.) that transform another collection.
    /// The input reader provides deltas that are automatically appended.
    /// The finalize callback is called on deltas when they're discarded.
    new (input: unit -> IOpReader<'Delta>, t: Traceable<'State, 'Delta>, finalize: 'Delta -> unit) = History<'State, 'Delta>(ValueSome (lazy (input())), t, finalize)

    /// Creates an imperative history with no finalization.
    /// Most common constructor for simple changeable collections.
    new (t: Traceable<'State, 'Delta>) = History<'State, 'Delta>(ValueNone, t, ignore)

    /// Creates a dependent history with no finalization.
    /// Most common constructor for derived operations.
    new (input: unit -> IOpReader<'Delta>, t: Traceable<'State, 'Delta>) = History<'State, 'Delta>(ValueSome (lazy (input())), t, ignore)

/// HistoryReader maintains a position in a History and provides incremental access to changes.
/// Each reader tracks its own version and walks forward through the version chain on GetChanges.
/// This is the implementation behind alist/aset/amap readers.
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

