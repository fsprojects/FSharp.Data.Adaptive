namespace FSharp.Data.Adaptive

open System
open System.Threading
open System.Collections.Generic

[<AutoOpen>]
module internal HeapExtensions =
    /// swaps the given elements inside the list.
    let inline private swap (heap: List<'T>) (l: int) (r: int) =
        let t = heap.[l]
        heap.[l] <- heap.[r]
        heap.[r] <- t

    /// moves an element in the list 'up' in heap-order.
    /// assumes that the list is in heap-order except for the given element.
    let rec private bubbleUp (heap: List<'T>) (compare: OptimizedClosures.FSharpFunc<'T, 'T, int>) (i: int) (v: 'T) =
        if i > 0 then
            let pi = (i - 1) >>> 1
            let pe = heap.[pi]

            if compare.Invoke(pe, v) > 0 then
                swap heap pi i
                bubbleUp heap compare pi v
                
    /// moves an element in the list 'down' in heap-order.
    /// assumes that the list is in heap-order except for the given element.
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
        /// enqueues an element  to the list in heap-order.
        member x.HeapEnqueue(compare: OptimizedClosures.FSharpFunc<'T, 'T, int>, value: 'T): unit =
            let index = x.Count
            x.Add value
            bubbleUp x compare index value
            
        /// enqueues an element to the list in heap-order.
        member x.HeapEnqueue(compare: 'T -> 'T -> int, value: 'T): unit =
            let compare = OptimizedClosures.FSharpFunc<'T, 'T, int>.Adapt(compare)
            x.HeapEnqueue(compare, value)
            
        /// dequeues the smallest element from the heap-order list.
        member x.HeapDequeue(compare: OptimizedClosures.FSharpFunc<'T, 'T, int>): 'T =
            if x.Count = 0 then raise <| ArgumentException("heap empty")
            let result = x.[0]
            let li = x.Count - 1
            let l = x.[li]
            x.[0] <- l
            x.RemoveAt li
            pushDown x compare 0 l
            result

        /// dequeues the smallest element from the heap-order list.
        member x.HeapDequeue(compare: 'T -> 'T -> int) =
            let compare = OptimizedClosures.FSharpFunc<'T, 'T, int>.Adapt(compare)
            x.HeapDequeue(compare)


[<AutoOpen>]
module internal ReferenceEqualityOperators =

    /// gets a reference-hashcode
    let inline refhash<'T when 'T: not struct> (obj: 'T) =
        System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode (obj :> obj)

    /// determines whether the given objects are reference equal
    let inline refequal<'T when 'T: not struct> (l: 'T) (r: 'T) =
        Object.ReferenceEquals(l :> obj, r :> obj)

    /// determines whether the given objects are reference equal
    let inline (==) (l: 'T) (r: 'T) = refequal l r

    /// determines whether the given objects are not reference equal
    let inline (!=) (l: 'T) (r: 'T) = not (refequal l r)

[<AutoOpen>]
module internal InterlockedExtensions =

    type System.Threading.Interlocked with
        /// changes the byref by applying the given function in a thread-safe way. 
        /// NOTE that the function might be evaluated multiple times.
        static member Change(location: byref<'T>, f: 'T -> 'T) =
            let mutable initial = location
            let mutable computed = f initial

            while Interlocked.CompareExchange(&location, computed, initial) != initial do
                initial <- location
                computed <- f initial

            computed
            
        /// changes the byref by applying the given function in a thread-safe way. 
        /// NOTE that the function might be evaluated multiple times.
        static member Change(location: byref<'T>, f: 'T -> 'T * 'U) =
            let mutable initial = location
            let (n, r) = f initial
            let mutable computed = n
            let mutable result = r

            while Interlocked.CompareExchange(&location, computed, initial) != initial do
                initial <- location
                let (n, r) = f initial
                computed <- n
                result <- r

            result

            
        /// changes the byref by applying the given function in a thread-safe way. 
        /// NOTE that the function might be evaluated multiple times.
        static member Change(location: byref<int>, f: int -> int) =
            let mutable initial = location
            let mutable computed = f initial

            while Interlocked.CompareExchange(&location, computed, initial) <> initial do
                initial <- location
                computed <- f initial

            computed
            
        /// changes the byref by applying the given function in a thread-safe way. 
        /// NOTE that the function might be evaluated multiple times.
        static member Change(location: byref<int>, f: int -> int * 'U) =
            let mutable initial = location
            let (n, r) = f initial
            let mutable computed = n
            let mutable result = r

            while Interlocked.CompareExchange(&location, computed, initial) <> initial do
                initial <- location
                let (n, r) = f initial
                computed <- n
                result <- r

            result
            
        /// changes the byref by applying the given function in a thread-safe way. 
        /// NOTE that the function might be evaluated multiple times.
        static member Change(location: byref<int64>, f: int64 -> int64) =
            let mutable initial = location
            let mutable computed = f initial

            while Interlocked.CompareExchange(&location, computed, initial) <> initial do
                initial <- location
                computed <- f initial

            computed
            
        /// changes the byref by applying the given function in a thread-safe way. 
        /// NOTE that the function might be evaluated multiple times.
        static member Change(location: byref<int64>, f: int64 -> int64 * 'U) =
            let mutable initial = location
            let (n, r) = f initial
            let mutable computed = n
            let mutable result = r

            while Interlocked.CompareExchange(&location, computed, initial) <> initial do
                initial <- location
                let (n, r) = f initial
                computed <- n
                result <- r

            result

[<AutoOpen>]
module internal CheapEquality =
    open FSharp.Reflection
    open System.Collections.Generic
    open System.Runtime.CompilerServices

    type private CheapEquality<'a> private() =

        static let comparer =
            let typ = typeof<'a>

            // TODO: any reasonable ideas?
            if FSharpType.IsRecord typ || FSharpType.IsUnion typ || FSharpType.IsTuple typ then
                EqualityComparer<'a>.Default

            elif not typ.IsValueType then
                { new EqualityComparer<'a>() with 
                    member x.GetHashCode(o : 'a) = RuntimeHelpers.GetHashCode o
                    member x.Equals(a : 'a, b : 'a) = Object.ReferenceEquals(a, b)
                }

            else 
                EqualityComparer<'a>.Default


        static member Comparer = comparer

    let cheapComparer<'a> : EqualityComparer<'a> = CheapEquality<'a>.Comparer

    let cheapHash (a : 'a) = CheapEquality<'a>.Comparer.GetHashCode a
    let cheapEqual (a : 'a) (b : 'a) = CheapEquality<'a>.Comparer.Equals(a, b)

module Unchecked =
    let inline isNull<'a when 'a : not struct> (value : 'a) =
        isNull (value :> obj)

[<AutoOpen>]
module Failures =
    let inline unexpected() = failwith "[Adaptive] encountered an invalid state"



