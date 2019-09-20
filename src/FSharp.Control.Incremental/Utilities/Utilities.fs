namespace FSharp.Control.Incremental

open System
open System.Threading
open System.Collections.Generic

[<AutoOpen>]
module internal HeapExtensions =
    let inline private swap (heap : List<'a>) (l : int) (r : int) =
        let t = heap.[l]
        heap.[l] <- heap.[r]
        heap.[r] <- t

    let rec private bubbleUp (heap : List<'a>) (compare : OptimizedClosures.FSharpFunc<'a, 'a, int>) (i : int) (v : 'a) =
        if i > 0 then
            let pi = (i - 1) >>> 1
            let pe = heap.[pi]

            if compare.Invoke(pe, v) > 0 then
                swap heap pi i
                bubbleUp heap compare pi v

    let rec private pushDown (heap : List<'a>) (compare : OptimizedClosures.FSharpFunc<'a, 'a, int>) (i : int) (v : 'a) =
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
         
    type List<'a> with
        member x.HeapEnqueue(compare : OptimizedClosures.FSharpFunc<'a, 'a, int>, value : 'a) : unit =
            let index = x.Count
            x.Add value
            bubbleUp x compare index value
            
        member x.HeapEnqueue(compare : 'a -> 'a -> int, value : 'a) : unit =
            let compare = OptimizedClosures.FSharpFunc<'a, 'a, int>.Adapt(compare)
            x.HeapEnqueue(compare, value)

        member x.HeapDequeue(compare : OptimizedClosures.FSharpFunc<'a, 'a, int>) : 'a =
            if x.Count = 0 then raise <| ArgumentException("heap empty")
            let result = x.[0]
            let li = x.Count - 1
            let l = x.[li]
            x.[0] <- l
            x.RemoveAt li
            pushDown x compare 0 l
            result

        member x.HeapDequeue(compare : 'a -> 'a -> int) =
            let compare = OptimizedClosures.FSharpFunc<'a, 'a, int>.Adapt(compare)
            x.HeapDequeue(compare)


[<AutoOpen>]
module internal ReferenceEqualityOperators =

    /// gets a reference-hashcode
    let inline refhash<'a when 'a : not struct> (obj : 'a) =
        System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode (obj :> obj)

    /// determines whether the given objects are reference equal
    let inline refequal<'a when 'a : not struct> (l : 'a) (r : 'a) =
        Object.ReferenceEquals(l :> obj, r :> obj)

    /// determines whether the given objects are reference equal
    let inline (==) (l : 'a) (r : 'a) = refequal l r

    /// determines whether the given objects are not reference equal
    let inline (!=) (l : 'a) (r : 'a) = not (refequal l r)

[<AutoOpen>]
module internal InterlockedExtensions =

    type System.Threading.Interlocked with
        /// changes the byref by applying the given function in a thread-safe way. 
        /// NOTE that the function might be evaluated multiple times.
        static member Change(location : byref<'a>, f : 'a -> 'a) =
            let mutable initial = location
            let mutable computed = f initial

            while Interlocked.CompareExchange(&location, computed, initial) != initial do
                initial <- location
                computed <- f initial

            computed
            
        /// changes the byref by applying the given function in a thread-safe way. 
        /// NOTE that the function might be evaluated multiple times.
        static member Change(location : byref<'a>, f : 'a -> 'a * 'b) =
            let mutable initial = location
            let (n,r) = f initial
            let mutable computed = n
            let mutable result = r

            while Interlocked.CompareExchange(&location, computed, initial) != initial do
                initial <- location
                let (n,r) = f initial
                computed <- n
                result <- r

            result

            
        /// changes the byref by applying the given function in a thread-safe way. 
        /// NOTE that the function might be evaluated multiple times.
        static member Change(location : byref<int>, f : int -> int) =
            let mutable initial = location
            let mutable computed = f initial

            while Interlocked.CompareExchange(&location, computed, initial) <> initial do
                initial <- location
                computed <- f initial

            computed
            
        /// changes the byref by applying the given function in a thread-safe way. 
        /// NOTE that the function might be evaluated multiple times.
        static member Change(location : byref<int>, f : int -> int * 'b) =
            let mutable initial = location
            let (n,r) = f initial
            let mutable computed = n
            let mutable result = r

            while Interlocked.CompareExchange(&location, computed, initial) <> initial do
                initial <- location
                let (n,r) = f initial
                computed <- n
                result <- r

            result
            
        /// changes the byref by applying the given function in a thread-safe way. 
        /// NOTE that the function might be evaluated multiple times.
        static member Change(location : byref<int64>, f : int64 -> int64) =
            let mutable initial = location
            let mutable computed = f initial

            while Interlocked.CompareExchange(&location, computed, initial) <> initial do
                initial <- location
                computed <- f initial

            computed
            
        /// changes the byref by applying the given function in a thread-safe way. 
        /// NOTE that the function might be evaluated multiple times.
        static member Change(location : byref<int64>, f : int64 -> int64 * 'b) =
            let mutable initial = location
            let (n,r) = f initial
            let mutable computed = n
            let mutable result = r

            while Interlocked.CompareExchange(&location, computed, initial) <> initial do
                initial <- location
                let (n,r) = f initial
                computed <- n
                result <- r

            result
