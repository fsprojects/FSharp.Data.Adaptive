namespace FSharp.Control.Incremental

open System
open System.Threading
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Collections
open System.Collections.Generic

#nowarn "9"

/// Represents the core interface for all adaptive objects.
/// Contains support for tracking OutOfDate flags, managing in-/outputs 
/// and lazy/eager evaluation in the dependency tree.
[<AllowNullLiteral>]
type IAdaptiveObject =

    /// Each object can cache a WeakReference pointing to itself.
    /// This is because the system internally needs WeakReferences to IAdaptiveObjects
    abstract member Weak: WeakReference<IAdaptiveObject>

    /// Used internally to represent the maximal distance from an input
    /// cell in the dependency graph when evaluating inside a transaction.
    abstract member Level: int with get, set

    ///// Used internally to ensure that AdaptiveObjects are not marked while their value
    ///// is still needed by an evaluation.
    //abstract member ReaderCount: int with get, set

    /// Allows a specific implementation to evaluate the cell during the change propagation process.
    abstract member Mark: unit -> bool

    /// Indicates whether the object has been marked. This flag should only be accessed when holding
    /// a lock on the adaptive object.
    abstract member OutOfDate: bool with get, set

    /// The adaptive outputs for the object. Represented by Weak references to allow for
    /// unused parts of the graph to be garbage collected.
    abstract member Outputs: IWeakOutputSet

    /// Gets called whenever a current input of the object gets marked
    /// out of date. The first argument represents the Transaction that
    /// causes the object to be marked
    abstract member InputChanged: obj * IAdaptiveObject -> unit

    /// Gets called after all inputs of the object have been processed
    /// and directly before the object will be marked
    abstract member AllInputsProcessed: obj -> unit

    /// Indicates whether the IAdaptiveObject is constant
    abstract member IsConstant : bool

/// Datastructure for zero-cost casts between different possible representations for WeakOutputSet.
/// We actually did experiments and for huge
/// dependency graphs transactions were ~10% faster 
/// than they were when using unbox
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
/// contained elements are weak and the datastructure allows to add/remove entries.
/// The only other functionality is Consume which returns all the (currently alive)
/// entries and clears the set.
and IWeakOutputSet =

    /// Indicates whether the set is (conservatively) known to be empty.
    abstract member IsEmpty : bool

    /// Adds a weak reference to the given AdaptiveObject to the set
    /// and returns a boolean indicating whether the obj was new.
    abstract member Add : IAdaptiveObject -> bool

    /// Removes the reference to the given AdaptiveObject from the set
    /// and returns a boolean indicating whether the obj was removed.
    abstract member Remove : IAdaptiveObject -> bool

    /// Returns all currenty living entries from the set
    /// and clears its content.
    abstract member Consume : unit -> IAdaptiveObject[]

/// Represents a set of outputs for an AdaptiveObject. The references to all
/// contained elements are weak and the datastructure allows to add/remove
/// entries. The only other functionality is Consume which returns all the
/// (currently live) entries and clears the set.
and internal WeakOutputSet() =
    let mutable data = Unchecked.defaultof<VolatileSetData>
    let mutable setOps = 0

    let add (obj: IAdaptiveObject) =
        let mutable value = Unchecked.defaultof<IAdaptiveObject>
        let weakObj = obj.Weak
        match data.Tag with
        | 0 ->  
            if isNull data.Single then 
                data.Single <- weakObj
                true
            elif data.Single = weakObj then
                false
            elif data.Single.TryGetTarget(&value) then
                if Object.ReferenceEquals(value, obj) then
                    false
                else
                    let arr = Array.zeroCreate 8
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
                    if data.Array.[i].TryGetTarget(&value) then
                        if Object.ReferenceEquals(value, obj) then
                            freeIndex <- -2
                            i <- len
                    else
                        if freeIndex < 0 then freeIndex <- i
                i <- i + 1

            if freeIndex = -2 then
                false
            elif freeIndex >= 0 then
                data.Array.[freeIndex] <- weakObj
                true
            else
                // r cannot be null here (empty index would have been found)
                let all = data.Array |> Array.choose (fun r -> if r.TryGetTarget(&value) then Some r else None)
                let set = HashSet all
                let res = set.Add weakObj
                data.Tag <- 2
                data.Set <- set
                res
        | _ ->
            data.Set.Add weakObj

    /// Used interally to get rid of leaking WeakReferences
    member x.Cleanup() =
        lock x (fun () ->
            // TODO: better heuristic?
            if setOps > 100 then
                setOps <- 0
                let all = x.Consume()
                for a in all do add a |> ignore
        )

    /// Adds a weak reference to the given AdaptiveObject to the set
    /// and returns a boolean indicating whether the obj was new.
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
    /// and returns a boolean indicating whether the obj was removed.
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
    /// and clears its content.
    member x.Consume(): IAdaptiveObject[] =
        lock x (fun () ->
            let n = data
            data <- Unchecked.defaultof<_>
            setOps <- 0
            match n.Tag with
            | 0 ->  
                if isNull n.Single then 
                    [||]
                else 
                    match n.Single.TryGetTarget() with
                    | (true, v) -> [| v |]
                    | _ -> [||]
            | 1 ->  
                n.Array |> Array.choose (fun r ->
                    if isNull r then None
                    else 
                        match r.TryGetTarget() with
                        | (true, v) -> Some v
                        | _ -> None
                )
            | _ ->
                let mutable cnt = 0
                let mutable arr = Array.zeroCreate n.Set.Count
                let mutable o = Unchecked.defaultof<_>
                for r in n.Set do
                    if r.TryGetTarget(&o) then
                        arr.[cnt] <- o
                        cnt <- cnt + 1
                if cnt < arr.Length then Array.Resize(&arr, cnt)
                arr
        )

    /// Indicates whether the set is (conservatively) known to be empty.
    /// note that we don't dereference any WeakReferences here.
    member x.IsEmpty =
        match data.Tag with
        | 0 -> isNull data.Single
        | _ -> false

    interface IWeakOutputSet with
        member x.IsEmpty = x.IsEmpty
        member x.Add o = x.Add o
        member x.Remove o = x.Remove o
        member x.Consume() = x.Consume()

and internal EmptyOutputSet() =
    static let emptyArray : IAdaptiveObject[] = Array.zeroCreate 0
    interface IWeakOutputSet with
        member x.IsEmpty = true
        member x.Add _ = false
        member x.Remove _ = false
        member x.Consume() = emptyArray
    

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

