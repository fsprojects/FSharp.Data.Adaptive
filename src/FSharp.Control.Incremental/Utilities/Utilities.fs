namespace FSharp.Control.Incremental

open System
open System.Threading
open System.Collections.Generic

[<AutoOpen>]
module internal HeapExtensions =
    let inline private swap (heap: List<'T>) (l: int) (r: int) =
        let t = heap.[l]
        heap.[l] <- heap.[r]
        heap.[r] <- t

    let rec private bubbleUp (heap: List<'T>) (compare: OptimizedClosures.FSharpFunc<'T, 'T, int>) (i: int) (v: 'T) =
        if i > 0 then
            let pi = (i - 1) >>> 1
            let pe = heap.[pi]

            if compare.Invoke(pe, v) > 0 then
                swap heap pi i
                bubbleUp heap compare pi v

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

        member x.HeapEnqueue(compare: OptimizedClosures.FSharpFunc<'T, 'T, int>, value: 'T): unit =
            let index = x.Count
            x.Add value
            bubbleUp x compare index value
            
        member x.HeapEnqueue(compare: 'T -> 'T -> int, value: 'T): unit =
            let compare = OptimizedClosures.FSharpFunc<'T, 'T, int>.Adapt(compare)
            x.HeapEnqueue(compare, value)

        member x.HeapDequeue(compare: OptimizedClosures.FSharpFunc<'T, 'T, int>): 'T =
            if x.Count = 0 then raise <| ArgumentException("heap empty")
            let result = x.[0]
            let li = x.Count - 1
            let l = x.[li]
            x.[0] <- l
            x.RemoveAt li
            pushDown x compare 0 l
            result

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
