namespace FSharp.Data.Adaptive

#nowarn "7331"

/// internal list-diffing implementation.
[<CompilerMessage("internal", 7331, IsHidden = true)>]
module ComputeListDeltaHelpers = 
    open System
    open System.Collections.Generic

    type DeltaOperation =
        | Remove = 0
        | Add = 1
        | Equal = 2

    [<Struct>]
    type DeltaOperationList64 private(store : uint64, next : ref<DeltaOperationList64>) =

        // 6 bit count
        // 29 entries * 2bit = 58 bit

        static let extractCount (v : uint64) =
            v >>> 58 |> int

        static let extractItem (index : int) (v : uint64) =
            (v >>> (index <<< 1)) &&& 3UL |> int

        static let create (cnt : int) (value : uint64) =
            let mask = (1UL <<< (cnt <<< 1)) - 1UL
            (uint64 cnt <<< 58) ||| (value &&& mask)

        static let add (v : int) (oldCnt : int) (value : uint64) =
            let newCnt = oldCnt + 1
            let mask = (1UL <<< (oldCnt <<< 1)) - 1UL
            (uint64 newCnt <<< 58) ||| 
            ((uint64 v &&& 3UL) <<< (oldCnt <<< 1)) |||
            (value &&& mask)

        static let empty = DeltaOperationList64(0UL, Unchecked.defaultof<_>)

        member x.IsNil = isNull (next :> obj)
            
        member x.Head =
            let c = extractCount store
            extractItem (c - 1) store |> unbox<DeltaOperation>

        static member Empty = empty

        member x.Prepend(value : DeltaOperation) =
            let v = int value &&& 3
            if isNull (next :> obj) then
                DeltaOperationList64(create 1 (uint64 v), ref x)
            else
                let cnt = extractCount store
                if cnt >= 29 then
                    DeltaOperationList64(create 1 (uint64 v), ref x)
                else
                    DeltaOperationList64(add v cnt store, next)

        member x.UnsafeUnconsV() =
            let cnt = extractCount store
            let v = extractItem (cnt - 1) store
            if cnt = 1 then
                struct(unbox<DeltaOperation> v, next.Value)
            else
                struct(unbox<DeltaOperation> v, DeltaOperationList64(create (cnt - 1) store, next))

    module DeltaOperationList = 

        type DeltaOperationList = DeltaOperationList64
        
        /// inspired by this [paper](https://neil.fraser.name/writing/diff/myers.pdf)
        let ofArrayMyers (equal : 'a -> 'b -> bool) (src : 'a[]) (dst : 'b[]) : DeltaOperationList =
            let equal = OptimizedClosures.FSharpFunc<_,_,_>.Adapt equal
            
            let max = src.Length + dst.Length
            let vs = Array.zeroCreate<int> (2 * max + 1)
            let ps = Array.zeroCreate<DeltaOperationList> vs.Length

            let inline setv (k : int) value =
                vs.[k + max] <- value

            let inline getv (k : int) =
                vs.[k + max]

            let inline setp (k : int) value =
                ps.[k + max] <- value
                
            let inline getp (k : int) =
                ps.[k + max]
                
            let mutable x = 0
            let mutable y = 0
            let mutable d = 0
            let mutable result = Unchecked.defaultof<_>

            let eSrc = src.Length - 1
            let eDst = dst.Length - 1

            let inline equal x y =
                // reversed arrays
                equal.Invoke(src.[eSrc - x], dst.[eDst - y])

            while d <= max do
                let mutable k = -d
                while k <= d do
                    let down = k = -d || (k <> d && getv (k-1) < getv (k+1))
                    
                    let mutable p = Unchecked.defaultof<_>
                    if down then
                        x <- getv (k+1)
                        p <- getp (k+1)
                    else    
                        x <- getv(k-1) + 1
                        p <- getp(k-1)

                    y <- x - k
                    let kPrev = if down then k + 1 else k - 1

                    let xStart = getv kPrev
                    let yStart = xStart - kPrev

                    let xMid = if down then xStart else xStart + 1
                    let yMid = xMid - k

                    if xStart >= 0 && xMid <> xStart then p <- p.Prepend DeltaOperation.Remove 
                    elif yStart >= 0 && yMid <> yStart then p <- p.Prepend DeltaOperation.Add

                    while x < src.Length && y < dst.Length && equal x y do
                        x <- x + 1
                        y <- y + 1
                        p <- p.Prepend DeltaOperation.Equal

                    setv k x
                    setp k p
                    if x >= src.Length && y >= dst.Length then
                        // terminate
                        result <- p
                        d <- max
                        k <- max

                    k <- k + 2
                d <- d + 1

            result
       
        /// inspired by this [paper](https://neil.fraser.name/writing/diff/myers.pdf)
        let ofArrayMyersComparer (cmp : System.Collections.Generic.IEqualityComparer<'a>) (src : 'a[]) (dst : 'a[]) : DeltaOperationList =
            let max = src.Length + dst.Length
            let vs = Array.zeroCreate<int> (2 * max + 1)
            let ps = Array.zeroCreate<DeltaOperationList> vs.Length

            let inline setv (k : int) value =
                vs.[k + max] <- value

            let inline getv (k : int) =
                vs.[k + max]

            let inline setp (k : int) value =
                ps.[k + max] <- value
                
            let inline getp (k : int) =
                ps.[k + max]
                
            let mutable x = 0
            let mutable y = 0
            let mutable d = 0
            let mutable result = Unchecked.defaultof<_>

            let eSrc = src.Length - 1
            let eDst = dst.Length - 1

            let inline equal x y =
                // reversed arrays
                cmp.Equals(src.[eSrc - x], dst.[eDst - y])

            while d <= max do
                let mutable k = -d
                while k <= d do
                    let down = k = -d || (k <> d && getv (k-1) < getv (k+1))
                    
                    let mutable p = Unchecked.defaultof<_>
                    if down then
                        x <- getv (k+1)
                        p <- getp (k+1)
                    else    
                        x <- getv(k-1) + 1
                        p <- getp(k-1)

                    y <- x - k
                    let kPrev = if down then k + 1 else k - 1

                    let xStart = getv kPrev
                    let yStart = xStart - kPrev

                    let xMid = if down then xStart else xStart + 1
                    let yMid = xMid - k

                    if xStart >= 0 && xMid <> xStart then p <- p.Prepend DeltaOperation.Remove 
                    elif yStart >= 0 && yMid <> yStart then p <- p.Prepend DeltaOperation.Add

                    while x < src.Length && y < dst.Length && equal x y do
                        x <- x + 1
                        y <- y + 1
                        p <- p.Prepend DeltaOperation.Equal

                    setv k x
                    setp k p
                    if x >= src.Length && y >= dst.Length then
                        // terminate
                        result <- p
                        d <- max
                        k <- max

                    k <- k + 2
                d <- d + 1

            result
       



/// Differentiation extensions for several immutable datastructures.
[<AutoOpen>]
module DifferentiationExtensions =

    /// Functional programming operators related to the HashSet<_> type.
    module HashSet =

        /// Determines the operations needed to transform l into r, using custom element operation functions.
        /// Returns a HashSetDelta containing these operations.
        let computeDeltaCustom (l: HashSet<'T>) (r: HashSet<'T>) (add : 'T -> bool) (remove : 'T -> bool) =
            l.ComputeDeltaAsHashMap(r, remove, add) |> HashSetDelta

        /// Determines the operations needed to transform l into r.
        /// Returns a HashSetDelta containing these operations.
        let computeDelta (l: HashSet<'T>) (r: HashSet<'T>) =
            l.ComputeDeltaAsHashMap(r) |> HashSetDelta

        /// Same as computeDelta set empty
        let removeAll (set: HashSet<'T>) =
            set.MapToMap(fun _ -> -1) |> HashSetDelta
            
        /// Same as computeDelta empty set
        let addAll (set: HashSet<'T>) =
            set.MapToMap(fun _ -> 1) |> HashSetDelta

        /// Applies the given operations to the set. 
        /// Returns the new set and the 'effective' operations.
        let applyDelta (value: HashSet<'T>) (delta: HashSetDelta<'T>) =
            let inline apply _ (o : bool) (n : int) =
                if n < 0 then
                    if o then struct (false, ValueSome -1)
                    else struct(false, ValueNone)
                elif n > 0 then
                    if o then struct (true, ValueNone)
                    else struct (true, ValueSome 1)
                else
                    struct(o, ValueNone)
                    
            let cmp = value.Comparer
            let value = value.Root
            let struct(effective, value) = HashImplementation.SetNode.applyDelta cmp (OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt apply) value delta.Store.Root
            let set = HashSet<'T>(cmp, value)
            let delta = HashMap<'T, int>(cmp, effective)
            set, HashSetDelta delta

    /// Functional programming operators related to the HashMap<_,_> type.
    module HashMap =
    
        /// Determines the operations needed to transform l into r, using custom element operation functions.
        /// Returns a HashMapDelta containing all the needed operations.
        let computeDeltaCustom 
            (add: 'A -> 'B -> ValueOption<ElementOperation<'B>>) 
            (remove: 'A -> 'B -> ValueOption<ElementOperation<'B>>) 
            (update: 'A -> 'B -> 'B -> ValueOption<ElementOperation<'B>>)
            (l: HashMap<'A, 'B>) (r: HashMap<'A, 'B>): HashMapDelta<'A, 'B> =
            let delta = 
                HashImplementation.MapNode.computeDelta 
                    l.Comparer
                    (OptimizedClosures.FSharpFunc<_,_,_>.Adapt remove)
                    (OptimizedClosures.FSharpFunc<_,_,_>.Adapt add)
                    (OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt update)
                    l.Root
                    r.Root

            HashMap<'A, ElementOperation<'B>>(l.Comparer, delta) |> HashMapDelta

        /// Determines the operations needed to transform l into r.
        /// Returns a HashMapDelta containing all the needed operations.
        let computeDelta (l: HashMap<'A, 'B>) (r: HashMap<'A, 'B>): HashMapDelta<'A, 'B> =
            let inline add (_k : 'A) (v : 'B) = ValueSome (Set v)
            let inline remove (_k : 'A) (v : 'B) = ValueSome Remove
            let inline update (_k : 'A) (o : 'B) (n : 'B) =
                if DefaultEquality.equals o n then ValueNone
                else ValueSome (Set n)
            computeDeltaCustom add remove update l r

        let applyDelta (l : HashMap<'K, 'V>) (r : HashMapDelta<'K, 'V>) =
            let inline apply (_ : 'K) (o : voption<'V>) (n : ElementOperation<'V>) =
                match n with
                | Remove ->
                    match o with
                    | ValueSome _ -> struct (ValueNone, ValueSome Remove)
                    | ValueNone -> struct (ValueNone, ValueNone)
                | Set v ->
                    match o with
                    | ValueSome o ->
                        if DefaultEquality.equals o v then struct (ValueSome v, ValueNone)
                        else struct(ValueSome v, ValueSome (Set v))
                    | ValueNone ->
                        struct(ValueSome v, ValueSome (Set v))


            let state = l.Root
            let struct(delta, state) = HashImplementation.MapNode.applyDelta l.Comparer (OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt apply) state r.Store.Root

            let state = HashMap<'K, 'V>(l.Comparer, state)
            let delta = HashMap<'K, ElementOperation<'V>>(l.Comparer, delta)

            state, HashMapDelta delta

    /// Functional programming operators related to the IndexList<_> type.
    module IndexList =
        
        /// Applies the given operations to the list. 
        /// Returns the new list and the 'effective' operations.
        let applyDelta (x : IndexList<'T>) (deltas : IndexListDelta<'T>) =
            let inline apply _ o n =
                match n with
                | Remove ->
                    match o with
                    | ValueSome _ -> struct (ValueNone, ValueSome Remove)
                    | ValueNone -> struct (ValueNone, ValueNone)
                | Set v ->
                    match o with
                    | ValueSome o ->
                        if DefaultEquality.equals o v then struct (ValueSome v, ValueNone)
                        else struct(ValueSome v, ValueSome (Set v))
                    | ValueNone ->
                        struct(ValueSome v, ValueSome (Set v))
            let s, d = x.Content.ApplyDeltaAndGetEffective(deltas.Content, apply)
            IndexList.ofMap s, IndexListDelta.ofMap d
            
        /// Applies the given operations to the list, using custom element operation functions. 
        /// Returns the new list and the 'effective' operations.
        let computeDeltaCustom 
            (add: Index -> 'T -> ElementOperation<'T>) 
            (remove: Index -> 'T -> ElementOperation<'T>) 
            (update: Index -> 'T -> 'T -> ValueOption<ElementOperation<'T>>)
            (l : IndexList<'T>) (r : IndexList<'T>) : IndexListDelta<'T> =
            let res = l.Content.ComputeDeltaTo(r.Content, add, update, remove)
            IndexListDelta res

        /// Determines the operations needed to transform l into r.
        /// Returns a IndexListDelta containing these operations.
        let computeDelta (l : IndexList<'T>) (r : IndexList<'T>) : IndexListDelta<'T> =
            let inline add _ v = Set v
            let inline rem _ _ = Remove
            let inline update _ o n =
                if DefaultEquality.equals o n then ValueNone
                else ValueSome (Set n)
            computeDeltaCustom add rem update l r


    
        /// Determines the operations needed to transform src into dst.
        /// Returns a IndexListDelta containing these operations.
        let computeDeltaToArray (cmp : System.Collections.Generic.IEqualityComparer<'a>) (src : IndexList<'a>) (dst : 'a[]) =
            if dst.Length = 0 then
                computeDelta src IndexList.empty
            else
                if src.Count = 0 then
                    computeDelta src (IndexList.ofArray dst)
                else
                    // both are non-empty
                    let src = IndexList.toArrayIndexed src

                    let mutable steps = ComputeListDeltaHelpers.DeltaOperationList.ofArrayMyers (fun (_,a) b -> cmp.Equals(a, b)) src dst
                    let mutable si = 0
                    let mutable di = 0
                    let mutable delta = IndexListDelta.empty
                    let mutable lastIndex = Index.zero

                    while not steps.IsNil do
                        let struct(h, t) = steps.UnsafeUnconsV()

                        match h with
                        | ComputeListDeltaHelpers.DeltaOperation.Equal ->
                            steps <- t
                            // step both => no delta
                            lastIndex <- fst src.[si]
                            si <- si + 1
                            di <- di + 1
                        | _ ->
                       
                            let mutable remCnt = 0
                            let mutable addCnt = 0
                            let mutable struct(h, t) = steps.UnsafeUnconsV()
                            steps <- t

                            while h <> ComputeListDeltaHelpers.DeltaOperation.Equal do
                                if h = ComputeListDeltaHelpers.DeltaOperation.Remove then remCnt <- remCnt + 1
                                else addCnt <- addCnt + 1

                                if steps.IsNil then 
                                    h <- ComputeListDeltaHelpers.DeltaOperation.Equal
                                else 
                                    let struct(hh, tt) = steps.UnsafeUnconsV()
                                    if hh <> ComputeListDeltaHelpers.DeltaOperation.Equal then 
                                        h <- hh
                                        steps <- tt
                                    else
                                        h <- ComputeListDeltaHelpers.DeltaOperation.Equal

                        
                            let replace = min remCnt addCnt
                            for _ in 0 .. replace - 1 do
                                let (idx,_) = src.[si]
                                delta <- delta |> IndexListDelta.add idx (Set dst.[di])
                                si <- si + 1
                                di <- di + 1
                                lastIndex <- idx

                            for _ in replace .. remCnt - 1 do
                                let (idx, _) = src.[si]
                                delta <- delta |> IndexListDelta.add idx Remove
                                si <- si + 1

                            if replace < addCnt then
                                let newIndex =
                                    if si < src.Length then 
                                        let (ni,_) = src.[si]
                                        fun l -> Index.between l ni
                                    else 
                                        Index.after

                                for _ in replace .. addCnt - 1 do
                                    let idx = newIndex lastIndex
                                    delta <- delta |> IndexListDelta.add idx (Set dst.[di])
                                    lastIndex <- idx
                                    di <- di + 1

                    delta
              
        /// Determines the operations needed to transform src into dst.
        /// Returns a IndexListDelta containing these operations and the
        /// resulting IndexList.
        let computeDeltaToArrayAndGetResult (cmp : System.Collections.Generic.IEqualityComparer<'a>) (src : IndexList<'a>) (dst : 'a[]) =
            if dst.Length = 0 then
                computeDelta src IndexList.empty, IndexList.empty
            else
                if src.Count = 0 then
                    let dst = IndexList.ofArray dst
                    computeDelta src dst, dst
                else
                    // both are non-empty
                    let mutable result = src
                    let src = IndexList.toArrayIndexed src

                    let mutable steps = ComputeListDeltaHelpers.DeltaOperationList.ofArrayMyers (fun (_,a) b -> cmp.Equals(a, b)) src dst
                    let mutable si = 0
                    let mutable di = 0
                    let mutable delta = IndexListDelta.empty
                    let mutable lastIndex = Index.zero
                    while not steps.IsNil do
                        let struct(h, t) = steps.UnsafeUnconsV()

                        match h with
                        | ComputeListDeltaHelpers.DeltaOperation.Equal ->
                            steps <- t
                            // step both => no delta
                            lastIndex <- fst src.[si]
                            si <- si + 1
                            di <- di + 1
                        | _ ->
                       
                            let mutable remCnt = 0
                            let mutable addCnt = 0
                            let mutable struct(h, t) = steps.UnsafeUnconsV()
                            steps <- t

                            while h <> ComputeListDeltaHelpers.DeltaOperation.Equal do
                                if h = ComputeListDeltaHelpers.DeltaOperation.Remove then remCnt <- remCnt + 1
                                else addCnt <- addCnt + 1

                                if steps.IsNil then 
                                    h <- ComputeListDeltaHelpers.DeltaOperation.Equal
                                else 
                                    let struct(hh, tt) = steps.UnsafeUnconsV()
                                    if hh <> ComputeListDeltaHelpers.DeltaOperation.Equal then 
                                        h <- hh
                                        steps <- tt
                                    else
                                        h <- ComputeListDeltaHelpers.DeltaOperation.Equal

                        
                            let replace = min remCnt addCnt
                            for _ in 0 .. replace - 1 do
                                let (idx,_) = src.[si]
                                let nv = dst.[di]
                                delta <- delta |> IndexListDelta.add idx (Set nv)
                                result <- result |> IndexList.set idx nv
                                si <- si + 1
                                di <- di + 1
                                lastIndex <- idx

                            for _ in replace .. remCnt - 1 do
                                let (idx, _) = src.[si]
                                delta <- delta |> IndexListDelta.add idx Remove
                                result <- result |> IndexList.remove idx
                                si <- si + 1

                            if replace < addCnt then
                                let newIndex =
                                    if si < src.Length then 
                                        let (ni,_) = src.[si]
                                        fun l -> Index.between l ni
                                    else 
                                        Index.after

                                for _ in replace .. addCnt - 1 do
                                    let idx = newIndex lastIndex
                                    let nv = dst.[di]
                                    delta <- delta |> IndexListDelta.add idx (Set nv)
                                    result <- result |> IndexList.set idx nv
                                    lastIndex <- idx
                                    di <- di + 1

                    delta, result
                    
        /// Determines the operations needed to transform src into dst.
        /// Returns a IndexListDelta containing these operations.
        let computeDeltaToList (cmp : System.Collections.Generic.IEqualityComparer<'a>) (src : IndexList<'a>) (dst : list<'a>) =
            computeDeltaToArray cmp  src (List.toArray dst)
            
        /// Determines the operations needed to transform src into dst.
        /// Returns a IndexListDelta containing these operations.
        let computeDeltaToSeq (cmp : System.Collections.Generic.IEqualityComparer<'a>) (src : IndexList<'a>) (dst : seq<'a>) =
            match dst with
            | :? IndexList<'a> as dst -> computeDelta src dst
            | :? array<'a> as dst -> computeDeltaToArray cmp src dst
            | :? list<'a> as dst -> computeDeltaToList cmp src dst
            | _ -> computeDeltaToArray cmp src (Seq.toArray dst)


 


