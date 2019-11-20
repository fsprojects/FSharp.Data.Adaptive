namespace FSharp.Data.Adaptive

/// Differentiation extensions for several immutable datastructures.
[<AutoOpen>]
module DifferentiationExtensions =

    /// Functional programming operators related to the HashSet<_> type.
    module HashSet =

        /// Determines the operations needed to transform l into r.
        /// Returns a HashSetDelta containing these operations.
        let computeDelta (l: HashSet<'T>) (r: HashSet<'T>) =
            HashSet<'T>.ComputeDelta(l, r, (fun _ -> 1), (fun _ -> -1)) |> HashSetDelta

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
                    
            let set, delta = HashSet<'T>.ApplyDelta(value, delta.Store, apply)
            set, HashSetDelta delta

    /// Functional programming operators related to the HashMap<_,_> type.
    module HashMap =
    
        /// Determines the operations needed to transform l into r.
        /// Returns a HashMapDelta containing all the needed operations.
        let computeDelta (l: HashMap<'A, 'B>) (r: HashMap<'A, 'B>): HashMapDelta<'A, 'B> =
            let inline add _k v = Set v
            let inline remove _k _v = Remove
            let inline update _l o n =
                if Unchecked.equals o n then ValueNone
                else ValueSome (Set n)

            HashMap<'A, 'B>.ComputeDelta(l, r, add, update, remove) |> HashMapDelta

        let applyDelta (l : HashMap<'K, 'V>) (r : HashMapDelta<'K, 'V>) =
            let inline apply _ o n =
                match n with
                | Remove ->
                    match o with
                    | ValueSome _ -> struct (ValueNone, ValueSome Remove)
                    | ValueNone -> struct (ValueNone, ValueNone)
                | Set v ->
                    match o with
                    | ValueSome o ->
                        if Unchecked.equals o v then struct (ValueSome v, ValueNone)
                        else struct(ValueSome v, ValueSome (Set v))
                    | ValueNone ->
                        struct(ValueSome v, ValueSome (Set v))

            let state, delta = HashMap<'K, 'V>.ApplyDelta(l, r.Store, apply)
            state, HashMapDelta delta

    /// Functional programming operators related to the IndexList<_> type.
    module IndexList =
        
        /// Determines the operations needed to transform l into r.
        /// Returns a IndexListDelta containing these operations.
        let applyDelta (x : IndexList<'T>) (deltas : IndexListDelta<'T>) =
            if deltas.Count = 0 then
                x, deltas
            else
                let mutable res = x
                let finalDeltas =
                    deltas |> IndexListDelta.filter (fun i op ->
                        match op with
                        | Remove -> 
                            res <- res.Remove i
                            true
                        | Set v -> 
                            match res.TryGet i with
                            | Some o when Unchecked.equals o v -> 
                                false
                            | _ -> 
                                res <- res.Set(i,v)
                                true
                    )

                res, finalDeltas

        /// Applies the given operations to the list. 
        /// Returns the new list and the 'effective' operations.
        let computeDelta (l : IndexList<'T>) (r : IndexList<'T>) : IndexListDelta<'T> =
            if l.Count = 0 && r.Count = 0 then
                IndexListDelta.empty

            elif l.Count = 0 then
                r.Content |> MapExt.map (fun i v -> Set v) |> IndexListDelta.ofMap
                
            elif r.Count = 0 then
                l.Content |> MapExt.map (fun i v -> Remove) |> IndexListDelta.ofMap

            elif System.Object.ReferenceEquals (l.Content, r.Content) then
                IndexListDelta.empty

            else
                // TODO: one small???
                let merge (k : Index) (l : option<'T>) (r : option<'T>) =
                    match l, r with
                    | Some l, Some r when Unchecked.equals l r -> 
                        None
                    | _, Some r -> 
                        Some (Set r)
                    | Some _l, None -> 
                        Some Remove
                    | None, None ->
                        None

                MapExt.choose2 merge l.Content r.Content |> IndexListDelta.ofMap

