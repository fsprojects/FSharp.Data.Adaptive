namespace FSharp.Data.Adaptive

open System
open System.Threading
open System.Collections.Generic
open System.Runtime.CompilerServices

module internal PrimeSizes =
    let primeSizes =
        [|
            (*    prime no.           prime *)
            (*           2                3       +  1 = 2^2 *)
            (*           4 *) 7                // +  1 = 2^3, minimal size
            (*           6 *) 13               // +  3 = 2^4
            (*          11 *) 31               // +  1 = 2^5
            (*          18 *) 61               // +  3 = 2^6
            (*          31 *) 127              // +  1 = 2^7
            (*          54 *) 251              // +  5 = 2^8
            (*          97 *) 509              // +  3 = 2^9
            (*         172 *) 1021             // +  3 = 2^10
            (*         309 *) 2039             // +  9 = 2^11
            (*         564 *) 4093             // +  3 = 2^12
            (*        1028 *) 8191             // +  1 = 2^13
            (*        1900 *) 16381            // +  3 = 2^14
            (*        3512 *) 32749            // + 19 = 2^15
            (*        6542 *) 65521            // + 15 = 2^16
            (*       12251 *) 131071           // +  1 = 2^17
            (*       23000 *) 262139           // +  5 = 2^18
            (*       43390 *) 524287           // +  1 = 2^19
            (*       82025 *) 1048573          // +  3 = 2^20
            (*      155611 *) 2097143          // +  9 = 2^21
            (*      295947 *) 4194301          // +  3 = 2^22
            (*      564163 *) 8388593          // + 15 = 2^23
            (*     1077871 *) 16777213         // +  3 = 2^24
            (*     2063689 *) 33554393         // + 39 = 2^25
            (*     3957809 *) 67108859         // +  5 = 2^26
            (*     7603553 *) 134217689        // + 39 = 2^27
            (*    14630843 *) 268435399        // + 57 = 2^28
            (*    28192750 *) 536870909        // +  3 = 2^29
            (*    54400028 *) 1073741789       // + 35 = 2^30
            (*   105097565 *) 2147483647       // +  1 = 2^31
        |]

[<AllowNullLiteral; Sealed>]
type internal TransactQueueEntry<'V>() =
    [<DefaultValue>]
    val mutable public Hash : uint32
    [<DefaultValue>]
    val mutable public Slot : int
    [<DefaultValue>]
    val mutable public Key : int
    [<DefaultValue>]
    val mutable public Value : 'V
    [<DefaultValue>]
    val mutable public Prev : TransactQueueEntry<'V>
    [<DefaultValue>]
    val mutable public Next : TransactQueueEntry<'V>
    
    static member inline New(h, s, k, v, p, n) = 
        new TransactQueueEntry<_>(
            Hash = h, 
            Slot = s, 
            Key = k, 
            Value = v, 
            Prev = p, 
            Next = n
        )

module internal TransactQueueEntryHeap =

    /// Swaps the given elements inside the list.
    let inline swap (heap: List<TransactQueueEntry<'V>>) (l: int) (r: int) =
        let t = heap.[l]
        heap.[l] <- heap.[r]
        heap.[r] <- t

    /// Moves an element in the list 'up' in heap-order.
    /// Assumes that the list is in heap-order except for the given element.
    let rec bubbleUp (heap: List<TransactQueueEntry<'V>>) (i: int) (v: TransactQueueEntry<'V>) =
        if i > 0 then
            let pi = (i - 1) >>> 1
            let pe = heap.[pi]

            if pe.Key > v.Key then
                swap heap pi i
                bubbleUp heap pi v
                
    /// Moves an element in the list 'down' in heap-order.
    /// Assumes that the list is in heap-order except for the given element.
    let rec pushDown (heap: List<TransactQueueEntry<'V>>) (i: int) (v: TransactQueueEntry<'V>) =
        let li = (i <<< 1) + 1
        let ri = li + 1

        let cl = if li < heap.Count then v.Key <= heap.[li].Key else true
        let cr = if ri < heap.Count then v.Key <= heap.[ri].Key else true

        if cl && not cr then
            swap heap ri i
            pushDown heap ri v

        elif not cl && cr then
            swap heap li i
            pushDown heap li v

        elif not cl && not cr then
            if heap.[li].Key < heap.[ri].Key then
                swap heap li i
                pushDown heap li v
            else
                swap heap ri i
                pushDown heap ri v
         

    let inline enqueue (queue : List<TransactQueueEntry<'V>>) (value: TransactQueueEntry<'V>): unit =
        let index = queue.Count
        queue.Add value
        bubbleUp queue index value

    let inline dequeue (x : List<TransactQueueEntry<'V>>): TransactQueueEntry<'V> =
        let result = x.[0]
        let li = x.Count - 1
        let l = x.[li]
        x.[0] <- l
        x.RemoveAt li
        pushDown x 0 l
        result

/// Implements a priority queue (with int as priority) where each
/// 'V (by reference) can only be enqueued once.
/// Note that the order for 'colliding' keys is undefined.
[<Sealed>]
type internal TransactQueue<'V when 'V : not struct>() =
    let mutable capacityIndex = 0

    let mutable count = 0
    let mutable store : TransactQueueEntry<'V>[] = Array.zeroCreate PrimeSizes.primeSizes.[capacityIndex]
    let mutable heap = List<TransactQueueEntry<'V>>() 
    let mutable reuse : TransactQueueEntry<'V> = null

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let newEntry () =
        if isNull reuse then 
            TransactQueueEntry()
        else
            let e = reuse
            reuse <- e.Next
            e
            
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let free (e : TransactQueueEntry<'V>) =
        e.Next <- reuse
        reuse <- e
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let resize (newCapIndex : int) =
        if newCapIndex <> capacityIndex then
            capacityIndex <- newCapIndex
            let newStore : TransactQueueEntry<_>[] = Array.zeroCreate PrimeSizes.primeSizes.[newCapIndex]

            for i in 0 .. heap.Count - 1 do
                let e = heap.[i]
                let slot = e.Hash % uint32 newStore.Length |> int

                let o = newStore.[slot]
                if not (isNull o) then o.Prev <- e
                e.Slot <- slot
                e.Prev <- null
                e.Next <- o
                newStore.[slot] <- e

            store <- newStore

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let enqueue (key : int) (value : 'V) =
        let hash = RuntimeHelpers.GetHashCode value |> uint32
        let mutable slot = 
            hash % uint32 store.Length |> int

        let mutable found = false
        let mutable e = store.[slot]
        while not found && not (isNull e) do
            if Object.ReferenceEquals(e.Value, value) then
                found <- true
            else            
                e <- e.Next

        if not found then
            if count >= store.Length then 
                resize (2 + capacityIndex)
                slot <- hash % uint32 store.Length |> int

            let o = store.[slot]
            let e = newEntry()

            e.Hash <- hash
            e.Slot <- slot
            e.Key <- key
            e.Value <- value
            e.Next <- store.[slot]
            e.Prev <- null

            if not (isNull o) then o.Prev <- e
            store.[slot] <- e
            count <- count + 1
            TransactQueueEntryHeap.enqueue heap e
           
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]     
    let dequeue() =
        let e = TransactQueueEntryHeap.dequeue heap
        
        let p = e.Prev
        let n = e.Next
        if isNull p then
            let slot = e.Slot
            if not (isNull n) then n.Prev <- null
            store.[slot] <- n
        else
            p.Next <- n
            if not (isNull n) then n.Prev <- p

        free e

        count <- count - 1
        struct(e.Key, e.Value)
        
    /// Is the queue emoty?
    member x.IsEmpty = count = 0
    
    /// Does the queue contain the given value?
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Contains (value : 'V) =
        let hash = RuntimeHelpers.GetHashCode value |> uint32
        let slot = hash % uint32 store.Length |> int

        let mutable found = false
        let mutable e = store.[slot]
        while not found && not (isNull e) do
            if Object.ReferenceEquals(e.Value, value) then
                found <- true
            else            
                e <- e.Next
        found        
        
    /// Enqueue a key/value pair to the queue.
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Enqueue(k : int, v : 'V) =
        enqueue k v
        
    /// Dequeues the minimal element from the queue and returns the key/value pair
    /// as a struct tuple.
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Dequeue() =
        dequeue()    




//[<Struct>]
//type HashQueueEntry2<'K, 'V> =
//    val mutable public Hash : uint32
//    val mutable public Slot : int
//    val mutable public Key : 'K
//    val mutable public Value : 'V
//    val mutable public Next : int

//type HashQueue2<'K, 'V when 'V : not struct>() =
//    static let cmp  = Comparer<'K>.Default

    
//    let mutable capacityIndex = 0
//    let mutable hashTable   = Array.zeroCreate<int> PrimeSizes.primeSizes.[capacityIndex]
//    let mutable free = -1

//    let store   = List<HashQueueEntry2<'K, 'V>>()
//    let heap    = List<int>()
//    let cmp     = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(fun l r -> cmp.Compare(store.[l].Key, store.[r].Key))

//    let resize (newCapIndex : int) =
//        if newCapIndex <> capacityIndex then
//            capacityIndex <- newCapIndex
//            let newTable : int[] = Array.zeroCreate PrimeSizes.primeSizes.[newCapIndex]

//            for eid in heap do
//                let mutable e = store.[eid]
//                let slot = e.Hash % uint32 newTable.Length |> int
//                e.Next <- newTable.[slot]
//                e.Slot <- slot
//                store.[eid] <- e
//                newTable.[slot] <- eid + 1

//            hashTable <- newTable

//    let remove (slot : int) (eid : int) =
//        let mutable prev = -1
//        let mutable id = hashTable.[slot] - 1
//        let mutable found = false
//        while not found && id >= 0 do
//            let e = store.[id]
//            if id = eid then
//                if prev < 0 then 
//                    hashTable.[slot] <- e.Next 
//                else 
//                    let mutable pp = store.[prev]
//                    pp.Next <- e.Next
//                    store.[prev] <- pp

//                found <- true
//            else
//                prev <- id
//                id <- e.Next - 1
                
//        found

//    let contains (v : 'V) =
//        let hash = RuntimeHelpers.GetHashCode v
//        let slot = uint32 hash % uint32 hashTable.Length |> int
//        let mutable id = hashTable.[slot] - 1
//        let mutable found = false
//        while not found && id >= 0 do
//            let e = store.[id]
//            if Object.ReferenceEquals(e.Value, v) then
//                found <- true
//            else
//                id <- e.Next - 1

//        found

//    let add (key : 'K) (value : 'V) =
//        let hash = RuntimeHelpers.GetHashCode value |> uint32
//        let mutable slot = hash % uint32 hashTable.Length |> int
//        let mutable id = hashTable.[slot] - 1
//        let mutable found = false
//        while not found && id >= 0 do
//            let e = store.[id]
//            if Object.ReferenceEquals(e.Value, value) then
//                found <- true
//            else
//                id <- e.Next - 1

//        if not found then
//            if heap.Count >= hashTable.Length then 
//                resize (capacityIndex + 1)
//                slot <- hash % uint32 hashTable.Length |> int
//            let mutable n = Unchecked.defaultof<HashQueueEntry2<_,_>>
//            n.Hash <- hash
//            n.Slot <- slot
//            n.Key <- key
//            n.Value <- value
//            n.Next <- hashTable.[slot]

//            let nid = 
//                if free >= 0 then
//                    let id = free
//                    free <- store.[id].Next - 1
//                    store.[id] <- n
//                    id
//                else
//                    let nid = store.Count
//                    store.Add n
//                    nid

//            hashTable.[slot] <- nid + 1
//            ValueSome nid
//        else
//            ValueNone

//    member x.Count = heap.Count

//    member x.Contains(value : 'V) =
//        contains value

//    member x.Enqueue(key : 'K, value : 'V) =
//        match add key value with
//        | ValueSome id ->
//            heap.HeapEnqueue(cmp, id)
//        | ValueNone ->
//            ()

//    member x.Dequeue() =
//        let id = heap.HeapDequeue(cmp)
//        let mutable e = store.[id]
//        remove e.Slot id |> ignore
//        let k = e.Key
//        let v = e.Value

//        e.Key <- Unchecked.defaultof<_>
//        e.Value <- Unchecked.defaultof<_>
//        e.Hash <- 0u
//        e.Slot <- -1
//        e.Next <- free + 1
//        store.[id] <- e
//        free <- id
//        struct(k, v)

//type HashQueue3<'K, 'V when 'K : comparison>() =
    
//    let mutable store = SortedDictionary<'K, HashSet<'V>>()
    
//    member x.IsEmpty = store.Count = 0
    
//    member x.Enqueue(key : 'K, value : 'V) =
//        match store.TryGetValue key with
//        | (true, set) ->
//            set.Add value |> ignore
//        | _ ->
//            let set = HashSet()
//            set.Add value |> ignore
//            store.[key] <- set

//    member x.Dequeue() =
//        let key = Seq.head store.Keys
//        let set = store.[key]
//        let e = Seq.head set
//        set.Remove e |> ignore

//        if set.Count = 0 then store.Remove key |> ignore
//        struct(key, e)

//type HashQueue4<'K, 'V when 'K : comparison and 'V : not struct>() =
//    let mutable store : MapExt<'K, HashSet<'V>> = MapExt.empty
//    let set = ref Unchecked.defaultof<_>
//    let element = ref Unchecked.defaultof<_>
    
//    member x.IsEmpty = store.Count = 0
    
//    member x.Enqueue(key : 'K, value : 'V) =
//        store <- 
//            store.AlterV(key, function 
//                | ValueSome o -> 
//                    set := o
//                    ValueSome o 
//                | ValueNone ->
//                    let s = ReferenceHashSet.create()
//                    set := s
//                    ValueSome s
//            )

//        set.Value.Add value |> ignore

//    member x.Dequeue() =
//        store <- 
//            store.UpdateMinV(fun k s ->
//                let e = Seq.head s
//                element := struct(k, e)
//                s.Remove e |> ignore
//                if s.Count > 0 then ValueSome s
//                else ValueNone
//            )
//        !element