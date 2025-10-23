namespace FSharp.Data.Adaptive

open System
open System.Threading
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Collections
open System.Collections.Generic

#nowarn "9"

/// Represents the core interface for all adaptive objects in the dependency graph.
///
/// IAdaptiveObject is the foundation of the adaptive system and provides:
/// - OutOfDate tracking: Knows when its cached value is stale
/// - Dependency management: Tracks inputs and outputs in the dependency graph
/// - Change propagation: Participates in efficient update cascades
/// - Lazy/eager evaluation: Can be evaluated on-demand or within transactions
///
/// Implementation notes:
/// - All adaptive values (aval, alist, aset, amap) implement this interface
/// - The interface supports both pull-based (lazy) and push-based (eager) evaluation
/// - Thread-safe: Uses locking to ensure consistency during concurrent updates
type IAdaptiveObject =

    /// General-purpose tag for storing metadata.
    /// Used by advanced scenarios like traversal algorithms or custom caching.
    abstract member Tag : obj with get, set

    /// Cached WeakReference to this object.
    /// The system internally needs WeakReferences to IAdaptiveObjects for garbage collection.
    /// Cached here to avoid creating multiple WeakReference instances for the same object.
    abstract member Weak: WeakReference<IAdaptiveObject>

    /// The maximal distance from an input cell in the dependency graph.
    /// Used to determine the ORDER in which objects are marked during change propagation.
    /// Objects are marked in order of increasing level (level 0 first, then 1, then 2, etc.).
    /// This ensures outputs are marked after their inputs, maintaining topological order.
    /// Input cells (cval, clist, etc.) have level 0.
    /// The level does NOT influence evaluation order - only marking order during transactions.
    abstract member Level: int with get, set

    ///// Used internally to ensure that AdaptiveObjects are not marked while their value
    ///// is still needed by an evaluation.
    //abstract member ReaderCount: int with get, set

    /// Called during change propagation to mark this object as out-of-date.
    /// This is where you can realize callbacks when the object becomes dirty.
    ///
    /// Return value controls propagation:
    /// - Return TRUE to continue propagating changes to outputs (normal behavior)
    /// - Return FALSE to stop propagation (prevents outputs from being marked)
    ///
    /// Common uses:
    /// - Constant objects return false (no outputs should be marked)
    /// - Objects with callbacks execute them here before returning true
    /// - Custom objects can conditionally stop propagation based on logic
    ///
    /// Called with the object locked, after AllInputsProcessed, before outputs are consumed.
    abstract member Mark: unit -> bool

    /// Indicates whether the object's cached value is out-of-date.
    /// True means the cached value is stale and needs recomputation.
    /// False means the cached value is current and can be used.
    /// Should only be accessed when holding a lock on the adaptive object.
    abstract member OutOfDate: bool with get, set

    /// The set of adaptive outputs that depend on this object.
    /// Uses weak references to allow unused parts of the graph to be garbage collected.
    /// When this object changes, all outputs in this set are notified.
    abstract member Outputs: IWeakOutputSet

    /// Called when an input dependency becomes out-of-date.
    /// The first argument is the transaction object causing the change.
    /// The second argument is the input object that changed.
    /// Implementations typically forward this notification to their own outputs.
    abstract member InputChanged: obj * IAdaptiveObject -> unit

    /// Called after all inputs have been processed during change propagation,
    /// directly before this object will be marked out-of-date.
    /// The argument is the transaction object.
    /// Useful for performing finalization or cleanup before marking.
    abstract member AllInputsProcessed: obj -> unit

    /// Indicates whether this adaptive object is constant (never changes).
    /// Constant objects have optimized implementations with no tracking overhead.
    /// Returns true for objects created with AVal.constant, ASet.single, etc.
    abstract member IsConstant : bool

/// Represents a set of weak references to output objects in the dependency graph.
///
/// This specialized data structure:
/// - Stores weak references to prevent memory leaks
/// - Allows adding/removing outputs dynamically
/// - Provides efficient Consume operation to get all living outputs
/// - Automatically cleans up garbage-collected objects
///
/// The weak references enable automatic cleanup of unused computation branches,
/// a key feature of the adaptive system's memory management.
and IWeakOutputSet =

    /// Indicates whether the set is (conservatively) known to be empty.
    /// May return false even if all weak references are dead.
    /// Used as a fast-path optimization to avoid unnecessary work.
    abstract member IsEmpty : bool

    /// Adds a weak reference to the given AdaptiveObject to the set.
    /// Returns true if the object was newly added.
    /// Returns false if the object was already in the set.
    abstract member Add : IAdaptiveObject -> bool

    /// Removes the reference to the given AdaptiveObject from the set.
    /// Returns true if the object was found and removed.
    /// Returns false if the object was not in the set.
    abstract member Remove : IAdaptiveObject -> bool

    /// Returns all currently living entries from the set and clears its content.
    /// This is the primary way to consume the output set during change propagation.
    /// Dead weak references are automatically filtered out.
    /// The int return value is the number of living entries returned.
    abstract member Consume : ref<IAdaptiveObject[]> -> int

    /// Clears all references from the set.
    /// Used when an adaptive object is being disposed or reset.
    abstract member Clear : unit -> unit

#if FABLE_COMPILER

/// Represents a set of outputs for an AdaptiveObject. The references to all
/// contained elements are weak and the datastructure allows to add/remove
/// entries. The only other functionality is Consume which returns all the
/// (currently live) entries and clears the set.
and internal WeakOutputSet() =
    static let arrayThreshold = 8
    let mutable cnt = 0
    let mutable data : obj = null

    member x.Add(obj: IAdaptiveObject) = 
        if cnt = 0 then 
            data <- obj
            cnt <- 1
            true
        elif cnt = 1 then
            if Object.ReferenceEquals(data, obj) then
                false
            else             
                let arr = Array.zeroCreate arrayThreshold
                arr.[0] <- unbox data
                arr.[1] <- obj     
                data <- arr 
                cnt <- 2 
                true  
        elif cnt <= arrayThreshold then
            let arr = unbox<IAdaptiveObject[]> data
            let mutable isNew = true
            let mutable i = 0
            while isNew && i < cnt do  
                if Object.ReferenceEquals(arr.[i], obj) then isNew <- false
                i <- i + 1

            if isNew then
                if cnt < arr.Length then 
                    arr.[cnt] <- obj
                    cnt <- cnt + 1
                    true
                else
                    let set = ReferenceHashSet.create<IAdaptiveObject>()     
                    for e in arr do set.Add e |> ignore
                    cnt <- cnt + 1
                    data <- set
                    set.Add obj       
            else
                false
        else
            let set = unbox<HashSet<IAdaptiveObject>> data
            set.Add obj

    member x.Remove(obj: IAdaptiveObject) =
        if cnt = 0 then 
            false
        elif cnt = 1 then
            if Object.ReferenceEquals(data, obj) then 
                cnt <- 0
                data <- null
                true
            else
                false
        elif cnt <= arrayThreshold then
            let arr = unbox<IAdaptiveObject[]> data
            let mutable found = false
            let mutable i = 0
            while not found && i < cnt do  
                if Object.ReferenceEquals(arr.[i], obj) then 
                    let newCnt = cnt - 1
                    if newCnt = 1 then
                        if i = 0 then data <- arr.[1]
                        else data <- arr.[0]
                    elif i = newCnt then 
                        arr.[i] <- Unchecked.defaultof<_>
                    else 
                        arr.[i] <- arr.[newCnt]
                        arr.[newCnt] <- Unchecked.defaultof<_>
                    cnt <- newCnt
                    found <- true
                i <- i + 1
            found
        else
            let set = unbox<HashSet<IAdaptiveObject>> data
            if set.Remove obj then
                cnt <- set.Count
                if cnt <= arrayThreshold then
                    data <- Seq.toArray set
                true
            else
                false 

    member x.Consume(output : ref<IAdaptiveObject[]>): int  = 
        if cnt = 0 then 
            0
        elif cnt = 1 then
            let d = data
            data <- null
            cnt <- 0
            output.Value.[0] <- unbox d
            1
        elif cnt <= arrayThreshold then
            let arr = unbox<IAdaptiveObject[]> data 
            let c = cnt
            data <- null
            cnt <- 0
            if c >= output.Value.Length then resizeArray output (arr.Length * 2)
            for i in 0 .. c - 1 do output.Value.[i] <- arr.[i]
            c
        else
            let set = unbox<HashSet<IAdaptiveObject>> data
            data <- null
            cnt <- 0
            let mutable oi = 0
            for e in set do 
                if oi >= output.Value.Length then resizeArray output (oi * 2)
                output.Value.[oi] <- e
                oi <- oi + 1
            oi

    member x.IsEmpty = cnt = 0

    interface IWeakOutputSet with
        member x.IsEmpty = x.IsEmpty
        member x.Add o = x.Add o
        member x.Remove o = x.Remove o
        member x.Consume(outputs) = x.Consume(outputs)
        member x.Clear() = data <- null; cnt <- 0
#else


/// Datastructure for zero-cost casts between different possible representations for WeakOutputSet.
/// We actually did experiments and for huge dependency graphs transactions were ~10% faster 
/// than they were when using unbox.
and [<Struct; StructLayout(LayoutKind.Explicit)>] private VolatileSetData =

    [<FieldOffset(0)>]
    val mutable public Single: WeakReference<IAdaptiveObject>

    [<FieldOffset(0)>]
    val mutable public Array: WeakReference<IAdaptiveObject>[]

    [<FieldOffset(0)>]
    val mutable public Set: HashSet<WeakReference<IAdaptiveObject>>

    [<FieldOffset(8)>]
    val mutable public Tag: int


/// Represents a set of outputs for an AdaptiveObject. The references to all
/// contained elements are weak and the datastructure allows to add/remove
/// entries. The only other functionality is Consume which returns all the
/// (currently live) entries and clears the set.
and internal WeakOutputSet() =

    let [<Literal>] ArrayCapacity = 8
    let [<Literal>] HashSetCapacity = 16

    let mutable data = Unchecked.defaultof<VolatileSetData>
    let mutable setOps = 0
    let mutable valueReader = ref Unchecked.defaultof<IAdaptiveObject>

    let add (obj: IAdaptiveObject) =
        let weakObj = obj.Weak
        match data.Tag with
        | 0 ->  
            if isNull data.Single then 
                data.Single <- weakObj
                true
            elif data.Single = weakObj then
                false
            elif data.Single.TryGetTarget(valueReader) then
                let found = Object.ReferenceEquals(valueReader.Value, obj)
                valueReader.Value <- Unchecked.defaultof<IAdaptiveObject>
                if found then
                    false
                else
                    let arr = Array.zeroCreate ArrayCapacity
                    arr.[0] <- data.Single
                    arr.[1] <- weakObj
                    data.Tag <- 1
                    data.Array <- arr
                    true
            else
                data.Single <- weakObj
                true
        | 1 ->
            let mutable freeIndex = -1
            let mutable i = 0
            let len = data.Array.Length
            while i < len do
                if isNull data.Array.[i] then
                    if freeIndex < 0 then freeIndex <- i
                elif data.Array.[i] = weakObj then
                    freeIndex <- -2
                    i <- len
                else
                    if data.Array.[i].TryGetTarget(valueReader) then
                        if Object.ReferenceEquals(valueReader.Value, obj) then
                            freeIndex <- -2
                            i <- len
                    else
                        if freeIndex < 0 then freeIndex <- i
                i <- i + 1

            let res = 
                if freeIndex = -2 then
                    false
                elif freeIndex >= 0 then
                    data.Array.[freeIndex] <- weakObj
                    true
                else
                    // r cannot be null here (empty index would have been found)
                    let set = HashSet<WeakReference<IAdaptiveObject>>()
                    #if NET8_0_OR_GREATER
                    set.EnsureCapacity(HashSetCapacity) |> ignore
                    #endif
                    for r in data.Array do
                        if r.TryGetTarget(valueReader) then
                            set.Add r |> ignore
                    let res = set.Add weakObj
                    data.Tag <- 2
                    data.Set <- set
                    res

            valueReader.Value <- Unchecked.defaultof<IAdaptiveObject>
            res
        | _ ->
            data.Set.Add weakObj

    /// Used interally to get rid of leaking WeakReferences
    member x.Cleanup() =
        lock x (fun () ->
            // TODO: better heuristic?
            if setOps > 100 then
                setOps <- 0
                let arr = ref (Array.zeroCreate 100)
                let cnt = x.Consume arr
                for i in 0 .. cnt - 1 do
                    add arr.Value.[i] |> ignore
        )

    /// Adds a weak reference to the given AdaptiveObject to the set
    /// And returns a boolean indicating whether the obj was new.
    member x.Add(obj: IAdaptiveObject) =
        if not obj.IsConstant then
            lock x (fun () ->
                if add obj then
                    setOps <- setOps + 1
                    x.Cleanup()
                    true
                else
                    false
            )
        else
            false
        
    /// Removes the reference to the given AdaptiveObject from the set
    /// And returns a boolean indicating whether the obj was removed.
    member x.Remove(obj: IAdaptiveObject) =
        if not obj.IsConstant then
            lock x (fun () ->
                //let obj = obj.WeakSelf
                let mutable old = Unchecked.defaultof<IAdaptiveObject>

                match data.Tag with
                | 0 ->  
                    if isNull data.Single then
                        false
                    else
                        if data.Single.TryGetTarget(&old) then
                            if Object.ReferenceEquals(old, obj) then
                                data.Single <- null
                                true
                            else
                                false
                        else
                            data.Single <- null
                            false
                | 1 ->
                    let mutable found = false
                    let mutable i = 0
                    let len = data.Array.Length
                    let mutable count = 0
                    let mutable living = null
                    while i < len do
                        if not (isNull data.Array.[i]) then
                            let ref = data.Array.[i]
                            if ref.TryGetTarget(&old) then
                                if Object.ReferenceEquals(old, obj) then
                                    data.Array.[i] <- null
                                    found <- true
                                else
                                    count <- count + 1
                                    living <- ref
                            else
                                data.Array.[i] <- null
                        i <- i + 1

                    if count = 0 then
                        data.Tag <- 0
                        data.Single <- null
                    elif count = 1 then
                        data.Tag <- 0
                        data.Single <- living

                    found
     
                | _ ->  
                    if data.Set.Remove obj.Weak then
                        setOps <- setOps + 1
                        x.Cleanup()
                        true
                    else
                        false
            )
        else
            false

    /// Returns all currenty living entries from the set
    /// And clears its content.
    member x.Consume(output : ref<IAdaptiveObject[]>): int =
        lock x (fun () ->
            let cnt = 
                match data.Tag with
                | 0 ->  
                    if isNull data.Single then 
                        0
                    else 
                        match data.Single.TryGetTarget() with
                        | (true, v) -> 
                            output.Value.[0] <- v
                            1
                        | _ ->
                            0
                | 1 ->  
                    let mutable oi = 0
                    let arr = data.Array
                    for i in 0 ..arr.Length - 1 do
                        let r = arr.[i]
                        if not (isNull r) then
                            match r.TryGetTarget() with
                            | (true, v) -> 
                                if oi >= output.Value.Length then resizeArray output (oi <<< 2)
                                output.Value.[oi] <- v
                                oi <- oi + 1
                            | _ -> ()
                    oi
                | _ ->
                    let mutable oi = 0
                    let mutable o = Unchecked.defaultof<_>
                    for r in data.Set do
                        if r.TryGetTarget(&o) then
                            if oi >= output.Value.Length then resizeArray output (oi <<< 2)
                            output.Value.[oi] <- o
                            oi <- oi + 1
                    oi
            data.Single <- null
            data.Tag <- 0
            setOps <- 0
            cnt
        )

    member x.Clear() =
        lock x (fun () ->
            data.Single <- null
            data.Tag <- 0
            setOps <- 0
        )

    /// Indicates whether the set is (conservatively) known to be empty.
    /// Note that we don't dereference any WeakReferences here.
    member x.IsEmpty =
        lock x (fun () ->
            match data.Tag with
            | 0 -> isNull data.Single
            | _ -> false
        )

    interface IWeakOutputSet with
        member x.IsEmpty = x.IsEmpty
        member x.Add o = x.Add o
        member x.Remove o = x.Remove o
        member x.Consume(output) = x.Consume(output)
        member x.Clear() = x.Clear()

#endif

and internal EmptyOutputSet() =
    interface IWeakOutputSet with
        member x.IsEmpty = true
        member x.Add _ = false
        member x.Remove _ = false
        member x.Consume(_) = 0
        member x.Clear() = ()
    

/// Supporting operations for the WeakOutputSet type.
module internal WeakOutputSet =
    /// Creates a new empty WeakOutputSet
    let inline create () = WeakOutputSet()

    /// Adds a weak reference to the given AdaptiveObject to the set
    /// and returns a boolean indicating whether the obj was new.
    let inline add (o: IAdaptiveObject) (set: WeakOutputSet) =
        set.Add o

    /// Removes the reference to the given AdaptiveObject from the set
    /// and returns a boolean indicating whether the obj was removed.
    let inline remove (o: IAdaptiveObject) (set: WeakOutputSet) =
        set.Remove o

