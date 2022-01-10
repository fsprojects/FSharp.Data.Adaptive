namespace FSharp.Data.Adaptive

open System
open System.Threading
open System.Collections.Generic
open FSharp.Data.Adaptive

[<AutoOpen>]
module internal HeapExtensions =

    /// Swaps the given elements inside the list.
    let inline private swap (heap: List<'T>) (l: int) (r: int) =
        let t = heap.[l]
        heap.[l] <- heap.[r]
        heap.[r] <- t

    /// Moves an element in the list 'up' in heap-order.
    /// Assumes that the list is in heap-order except for the given element.
    let rec private bubbleUp (heap: List<'T>) (compare: OptimizedClosures.FSharpFunc<'T, 'T, int>) (i: int) (v: 'T) =
        if i > 0 then
            let pi = (i - 1) >>> 1
            let pe = heap.[pi]

            if compare.Invoke(pe, v) > 0 then
                swap heap pi i
                bubbleUp heap compare pi v
                
    /// Moves an element in the list 'down' in heap-order.
    /// Assumes that the list is in heap-order except for the given element.
    let rec private pushDown (heap: List<'T>) (compare: OptimizedClosures.FSharpFunc<'T, 'T, int>) (i: int) (v: 'T) =
        let li = (i <<< 1) + 1
        let ri = li + 1

        let cl = if li < heap.Count then compare.Invoke(v, heap.[li]) <= 0 else true
        let cr = if ri < heap.Count then compare.Invoke(v, heap.[ri]) <= 0 else true

        if cl && not cr then
            swap heap ri i
            pushDown heap compare ri v

        elif not cl && cr then
            swap heap li i
            pushDown heap compare li v

        elif not cl && not cr then
            let c = compare.Invoke(heap.[li], heap.[ri])
            if c < 0 then
                swap heap li i
                pushDown heap compare li v
            else
                swap heap ri i
                pushDown heap compare ri v
         
    type List<'T> with
        /// Enqueues an element  to the list in heap-order.
        member x.HeapEnqueue(compare: OptimizedClosures.FSharpFunc<'T, 'T, int>, value: 'T): unit =
            let index = x.Count
            x.Add value
            bubbleUp x compare index value
            
        /// Enqueues an element to the list in heap-order.
        member x.HeapEnqueue(compare: 'T -> 'T -> int, value: 'T): unit =
            let compare = OptimizedClosures.FSharpFunc<'T, 'T, int>.Adapt(compare)
            x.HeapEnqueue(compare, value)
            
        /// Dequeues the smallest element from the heap-order list.
        member x.HeapDequeue(compare: OptimizedClosures.FSharpFunc<'T, 'T, int>): 'T =
            if x.Count = 0 then raise <| ArgumentException("heap empty")
            let result = x.[0]
            let li = x.Count - 1
            let l = x.[li]
            x.[0] <- l
            x.RemoveAt li
            pushDown x compare 0 l
            result

        /// Dequeues the smallest element from the heap-order list.
        member x.HeapDequeue(compare: 'T -> 'T -> int) =
            let compare = OptimizedClosures.FSharpFunc<'T, 'T, int>.Adapt(compare)
            x.HeapDequeue(compare)


[<AutoOpen>]
module internal ReferenceEqualityOperators =

    /// Gets a reference-hashcode
    let inline refhash<'T when 'T: not struct> (obj: 'T) =
        System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode (obj :> obj)

    /// Determines whether the given objects are reference equal
    let inline refequal<'T when 'T: not struct> (l: 'T) (r: 'T) =
        Object.ReferenceEquals(l :> obj, r :> obj)

    /// Determines whether the given objects are reference equal
    let inline (==) (l: 'T) (r: 'T) = refequal l r

    /// Determines whether the given objects are not reference equal
    let inline (!=) (l: 'T) (r: 'T) = not (refequal l r)

#if !FABLE_COMPILER
[<AutoOpen>]
module internal InterlockedExtensions =

    type System.Threading.Interlocked with
        /// Changes the byref by applying the given function in a thread-safe way. 
        /// NOTE that the function might be evaluated multiple times.
        static member Change(location: byref<'T>, f: 'T -> 'T) =
            let mutable initial = location
            let mutable computed = f initial

            while Interlocked.CompareExchange(&location, computed, initial) != initial do
                initial <- location
                computed <- f initial

            computed
#endif


[<AutoOpen>]
module internal CheapEquality =
    open FSharp.Reflection
    open System.Collections.Generic
    open System.Runtime.CompilerServices

    #if FABLE_COMPILER
    let cheapHash (a : 'T) = ShallowEqualityComparer<'T>.Instance.GetHashCode a
    let cheapEqual (a : 'T) (b : 'T) = ShallowEqualityComparer<'T>.Instance.Equals(a, b)
    #else
    let cheapHash (a : 'T) = ShallowEqualityComparer<'T>.Instance.GetHashCode a
    let cheapEqual (a : 'T) (b : 'T) = ShallowEqualityComparer<'T>.Instance.Equals(a, b)
    #endif 

module internal Unchecked =
    let inline isNull<'T when 'T : not struct> (value : 'T) =
        isNull (value :> obj)

[<AutoOpen>]
module internal Failures =
    let inline unexpected() = failwith "[Adaptive] encountered an invalid state"



[<AutoOpen>]
module internal AdaptiveIndexListHelpers = 

    let inline combineHash (a: int) (b: int) =
        uint32 a ^^^ uint32 b + 0x9e3779b9u + ((uint32 a) <<< 6) + ((uint32 a) >>> 2) |> int

    [<Struct; CustomEquality; CustomComparison>]
    type UCmp<'a>(compare : OptimizedClosures.FSharpFunc<'a, 'a, int>, value : 'a) =

        member x.Value = value

        override x.GetHashCode() = DefaultEquality.hash value
        override x.Equals o =
            match o with
            | :? UCmp<'a> as o -> DefaultEquality.equals value o.Value
            | _ -> false
            
        member x.CompareTo(o : UCmp<'a>) = compare.Invoke(value, o.Value)

        #if !FABLE_COMPILER
        interface IComparable<UCmp<'a>> with
            member x.CompareTo(o) = compare.Invoke(value, o.Value)
        #endif


        interface IComparable with
            member x.CompareTo(o) =
                match o with
                | :? UCmp<'a> as o -> compare.Invoke(value, o.Value)
                | _ -> 0

    type IndexMapping<'k when 'k : comparison>() =
        let mutable store = MapExt.empty<'k, Index>

        member x.Invoke(k : 'k) =
            let mutable index = Index.zero
            let inline ret i = index <- i; Some i
            let newStore =
                store |> MapExt.changeWithNeighbours k (fun left self right -> 
                    match self with
                    | Some i -> ret i
                    | None -> 
                        match left, right with
                        | None, None                -> Index.after Index.zero |> ret
                        | Some(_,l), None           -> Index.after l          |> ret
                        | None, Some(_,r)           -> Index.before r         |> ret
                        | Some (_,l), Some(_,r)     -> Index.between l r      |> ret
                ) 
            store <- newStore
            index

        member x.Revoke(k : 'k) =
            match MapExt.tryRemove k store with
            | Some(i, rest) ->
                store <- rest
                Some i
            | None -> 
                None

        member x.Clear() =
            store <- MapExt.empty
            
    type CustomIndexMapping<'k>(cmp : OptimizedClosures.FSharpFunc<'k, 'k, int>) =
        let mutable store = MapExt.empty<UCmp<'k>, Index>

        member x.Invoke(k : 'k) =
            let k = UCmp(cmp, k)
            let (left, self, right) = MapExt.neighbours k store
            match self with
                | Some i -> 
                    i 
                | None ->
                    let result = 
                        match left, right with
                        | None, None                -> Index.after Index.zero
                        | Some(_,l), None           -> Index.after l
                        | None, Some(_,r)           -> Index.before r
                        | Some (_,l), Some(_,r)     -> Index.between l r

                    store <- MapExt.add k result store
                    result

        member x.Revoke(k : 'k) =
            let k = UCmp(cmp, k)
            match MapExt.tryRemove k store with
            | Some(i, rest) ->
                store <- rest
                Some i
            | None -> 
                None

        member x.Clear() =
            store <- MapExt.empty

        new(compare : 'k -> 'k -> int) =
            CustomIndexMapping(OptimizedClosures.FSharpFunc<_,_,_>.Adapt compare)

    type IndexCache<'a, 'b>(f : Index -> 'a -> 'b, release : 'b -> unit) =
        let store = DefaultDictionary.create<Index, 'a * 'b>()

        member x.InvokeAndGetOld(i : Index, a : 'a) =
            match store.TryGetValue(i) with
                | (true, (oa, old)) ->
                    if DefaultEquality.equals oa a then
                        None, old
                    else
                        let res = f i a
                        store.[i] <- (a, res)
                        Some old, res
                | _ ->
                    let res = f i a
                    store.[i] <- (a, res)
                    None, res       
                                        
        member x.Revoke(i : Index) =
            match store.TryGetValue i with
                | (true, (oa,ob)) -> 
                    store.Remove i |> ignore
                    release ob
                    Some ob
                | _ -> 
                    None 

        member x.Clear() =
            store.Values |> Seq.iter (snd >> release)
            store.Clear()

        new(f : Index -> 'a -> 'b) = IndexCache(f, ignore)

    type Unique<'b when 'b : comparison>(value : 'b) =
        static let mutable currentId = 0
        static let newId() = 
            #if FABLE_COMPILER
            let v = currentId in currentId <- v + 1; v
            #else 
            System.Threading.Interlocked.Increment(&currentId)
            #endif

        let id = newId()

        member x.Value = value
        member private x.Id = id

        override x.ToString() = value.ToString()

        override x.GetHashCode() = combineHash(DefaultEquality.hash value) id
        override x.Equals o =
            match o with
            | :? Unique<'b> as o -> DefaultEquality.equals value o.Value && id = o.Id
            | _ -> false

        interface IComparable with
            member x.CompareTo o =
                match o with
                | :? Unique<'b> as o ->
                    let c = compare value o.Value
                    if c = 0 then compare id o.Id
                    else c
                | _ ->
                    failwith "uncomparable"


module internal RangeDelta =

    /// Determine the changes in a range of integers
    let inline rangeChange (lastMin: ^T, lastMax, newMin, newMax) =
        let one = LanguagePrimitives.GenericOne< ^T >
        let maxIncrease = 
            // start the add at newMin if necessary
            let low = max newMin (lastMax + one) 
            (low, newMax)

        let maxDecrease = 
            // limit the removal to lastMin if necessary
            let high = max (newMax + one) lastMin  
            (lastMax, high)

        let minDecrease = 
            // start the addition at newMax if necessary
            let low = min newMax (lastMin - one) 
            // prevent double insertion after max increase
            let low = if newMax > lastMax then min low (fst maxIncrease - one) else low
            (low, newMin)

        let minIncrease =
            // limit the removal to lastMax if necessary
            let high = min (newMin - one) lastMax  
            // prevent double removal after max decrease
            let high = if newMax < lastMax then min high (snd maxDecrease - one) else high
            (lastMin, high)

        maxIncrease, maxDecrease, minDecrease, minIncrease
