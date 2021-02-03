namespace FSharp.Data.Adaptive

/// Differentiation extensions for several immutable datastructures.
[<AutoOpen>]
module DifferentiationExtensions =

    /// Functional programming operators related to the HashSet<_> type.
    module HashSet =

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
            let mutable value = value.Root
            let effective = HashImplementation.SetNode.applyDelta cmp (OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt apply) &value delta.Store.Root
            let set = HashSet<'T>(cmp, value)
            let delta = HashMap<'T, int>(cmp, effective)
            set, HashSetDelta delta

    /// Functional programming operators related to the HashMap<_,_> type.
    module HashMap =
    
        /// Determines the operations needed to transform l into r.
        /// Returns a HashMapDelta containing all the needed operations.
        let computeDelta (l: HashMap<'A, 'B>) (r: HashMap<'A, 'B>): HashMapDelta<'A, 'B> =
            let inline add (_k : 'A) (v : 'B) = ValueSome (Set v)
            let inline remove (_k : 'A) (v : 'B) = ValueSome Remove
            let inline update (_k : 'A) (o : 'B) (n : 'B) =
                if DefaultEquality.equals o n then ValueNone
                else ValueSome (Set n)

            let delta = 
                HashImplementation.MapNode.computeDelta 
                    l.Comparer
                    (OptimizedClosures.FSharpFunc<_,_,_>.Adapt remove)
                    (OptimizedClosures.FSharpFunc<_,_,_>.Adapt add)
                    (OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt update)
                    l.Root
                    r.Root

            HashMap<'A, ElementOperation<'B>>(l.Comparer, delta) |> HashMapDelta

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


            let mutable state = l.Root
            let delta = HashImplementation.MapNode.applyDelta l.Comparer (OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt apply) &state r.Store.Root

            let state = HashMap<'K, 'V>(l.Comparer, state)
            let delta = HashMap<'K, ElementOperation<'V>>(l.Comparer, delta)

            state, HashMapDelta delta

    /// Functional programming operators related to the IndexList<_> type.
    module IndexList =
        
        /// Determines the operations needed to transform l into r.
        /// Returns a IndexListDelta containing these operations.
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

        /// Applies the given operations to the list. 
        /// Returns the new list and the 'effective' operations.
        let computeDelta (l : IndexList<'T>) (r : IndexList<'T>) : IndexListDelta<'T> =
            let inline add _ v = Set v
            let inline rem _ _ = Remove
            let inline update _ o n =
                if DefaultEquality.equals o n then ValueNone
                else ValueSome (Set n)
            let res = l.Content.ComputeDeltaTo(r.Content, add, update, rem)
            IndexListDelta res
