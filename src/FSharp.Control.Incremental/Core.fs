namespace FSharp.Control.Incremental

open System
open System.Threading
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Collections
open System.Collections.Generic

#nowarn "9"


/// IAdaptiveObject represents the core interface for all
/// adaptive objects and contains everything necessary for
/// tracking OutOfDate flags and managing in-/outputs in the
/// dependency tree.
///
/// Since eager evalutation might be desirable in some scenarios
/// the interface also contains a Level representing the execution
/// order when evaluating inside a transaction and a function called
/// Mark allowing implementations to actually perform the evaluation.
/// Mark returns a bool since eager evaluation might cause the change
/// propagation process to exit early (if the actual value was unchanged)
[<AllowNullLiteral>]
type IAdaptiveObject =
    /// since the system internally needs WeakReferences to IAdaptiveObjects
    /// every object can cache a WeakReference pointing to itself.
    abstract member Weak : WeakReference<IAdaptiveObject>

    /// the level for an adaptive object represents the
    /// maximal distance from an input cell in the depdency graph
    /// Note that this level is entirely managed by the system 
    /// and shall not be accessed directly by users of the system.
    abstract member Level : int with get, set

    /// the ReaderCount is used by the system internally to 
    /// ensure that AdaptiveObjects are not marked while their value
    /// is still needed by an evaluation.
    /// it should not be accessed directly by users of the system.
    abstract member ReaderCount : int with get, set

    /// Mark allows a specific implementation to
    /// evaluate the cell during the change propagation process.
    abstract member Mark : unit -> bool

    /// the outOfDate flag for the object is true
    /// whenever the object has been marked and shall
    /// be set to false by specific implementations.
    /// Note that this flag shall only be accessed when holding
    /// a lock on the adaptive object (allowing for concurrency)
    abstract member OutOfDate : bool with get, set

    /// the adaptive outputs for the object which are recommended
    /// to be represented by Weak references in order to allow for
    /// unused parts of the graph to be garbage collected.
    abstract member Outputs : WeakOutputSet

    /// gets called whenever a current input of the object gets marked
    /// out of date. The first argument represents the Transaction that
    /// causes the object to be marked
    abstract member InputChanged : obj * IAdaptiveObject -> unit


    /// gets called after all inputs of the object have been processed
    /// and directly before the object will be marked
    abstract member AllInputsProcessed : obj -> unit




/// datastructure for zero-cost casts. 
/// we actually did experiments and for huge
/// dependency graphs transactions were ~10% faster 
/// than they were when using unbox
and [<StructLayout(LayoutKind.Explicit)>] private VolatileSetData =
    struct
        [<FieldOffset(0)>]
        val mutable public Single : WeakReference<IAdaptiveObject>
        [<FieldOffset(0)>]
        val mutable public Array : WeakReference<IAdaptiveObject>[]
        [<FieldOffset(0)>]
        val mutable public Set : HashSet<WeakReference<IAdaptiveObject>>
        [<FieldOffset(8)>]
        val mutable public Tag : int
    end

/// Represents a set ouf Outputs for an AdaptiveObject.
/// The references to all contained elements are weak and the 
/// datastructure allows to add/remove entries.
/// The only other functionality is Consume which returns all the
/// (currenlty living) entries and clears the set.
and WeakOutputSet() =
    let mutable data = Unchecked.defaultof<VolatileSetData>
    let mutable setOps = 0

    let add (obj : IAdaptiveObject) =
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
            if data.Set.Add weakObj then
                true
            else
                false

    /// used interally to get rid of leaking WeakReferences
    member x.Cleanup() =
        lock x (fun () ->
            // TODO: better heuristic?
            if setOps > 100 then
                setOps <- 0
                let all = x.Consume()
                for a in all do add a |> ignore
        )

    /// adds a weak reference to the given AdaptiveObject to the set
    /// and returns a boolean indicating whether the obj was new.
    member x.Add(obj : IAdaptiveObject) =
        lock x (fun () ->
            if add obj then
                setOps <- setOps + 1
                x.Cleanup()
                true
            else
                false
        )
        
    /// removes the reference to the given AdaptiveObject from the set
    /// and returns a boolean indicating whether the obj was removed.
    member x.Remove(obj : IAdaptiveObject) =
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

    /// returns all currenty living entries from the set
    /// and clears its content.
    member x.Consume() : IAdaptiveObject[] =
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

/// Represents a set ouf Outputs for an AdaptiveObject.
/// The references to all contained elements are weak and the 
/// datastructure allows to add/remove entries.
/// The only other functionality is Consume which returns all the
/// (currenlty living) entries and clears the set.
module WeakOutputSet =
    /// creates a new empty WeakOutputSet
    let inline create () = WeakOutputSet()

    /// adds a weak reference to the given AdaptiveObject to the set
    /// and returns a boolean indicating whether the obj was new.
    let inline add (o : IAdaptiveObject) (set : WeakOutputSet) =
        set.Add o

    /// removes the reference to the given AdaptiveObject from the set
    /// and returns a boolean indicating whether the obj was removed.
    let inline remove (o : IAdaptiveObject) (set : WeakOutputSet) =
        set.Remove o

