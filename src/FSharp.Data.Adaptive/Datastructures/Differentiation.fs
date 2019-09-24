namespace FSharp.Data.Adaptive

/// Differentiation extensions for several immutable datastructures.
[<AutoOpen>]
module DifferentiationExtensions =

    /// Functional programming operators related to the HashSet<_> type.
    module HashSet =

        /// Determines the operations needed to transform l into r.
        /// Returns a HashSetDelta containing these operations.
        let differentiate (l: HashSet<'T>) (r: HashSet<'T>) =
            // O(1)
            if System.Object.ReferenceEquals(l.Store, r.Store) then
                HashSetDelta.empty

            // O(|r|)
            elif l.IsEmpty then 
                let delta = r.Store |> IntMap.map (List.map (fun v -> struct (v, 1)))
                HashMap(r.Count, delta) |> HashSetDelta

            // O(|l|)
            elif r.IsEmpty then
                let delta = l.Store |> IntMap.map (List.map (fun v -> struct (v, -1)))
                HashMap(l.Count, delta) |> HashSetDelta
        
            // O(|l|*log|l|)
            elif r.Count * 5 < l.Count then
                // r is small
                let mutable lStore = l.Store
                let mutable cnt = 0

                // O(|r|*log|l|)
                let deltaR = 
                    r.Store |> IntMap.mapOptionWithKey (fun hash rValues ->
                        match IntMap.tryRemove hash lStore with
                        | Some (lValues, rest) ->
                            lStore <- rest
                            (lValues, rValues) ||> HashSetList.mergeWithOption (fun _value l r ->
                                if l && not r then cnt <- cnt + 1; Some -1
                                elif r && not l then cnt <- cnt + 1; Some 1
                                else None
                            )

                        | None ->
                            rValues
                            |> List.map (fun v -> cnt <- cnt + 1; struct(v,1))
                            |> Some
                    )
                
                // O(|l|)
                let deltaL =
                    lStore 
                    |> IntMap.map (List.map (fun v -> cnt <- cnt + 1; struct(v,-1)))

                let deltas = IntMap.append deltaL deltaR

                HashSetDelta(HashMap(cnt, deltas))

            // O(|r|*log|r|)
            elif l.Count * 5 < r.Count then
                // l is small
                let mutable rStore = r.Store
                let mutable cnt = 0
                
                // O(|l|*log|r|)
                let deltaL = 
                    l.Store |> IntMap.mapOptionWithKey (fun hash lValues ->
                        match IntMap.tryRemove hash rStore with
                        | Some (rValues, rest) ->
                            rStore <- rest
                            (lValues, rValues) ||> HashSetList.mergeWithOption (fun _value l r ->
                                if l && not r then cnt <- cnt + 1; Some -1
                                elif r && not l then cnt <- cnt + 1; Some 1
                                else None
                            )

                        | None ->
                            lValues
                            |> List.map (fun v -> cnt <- cnt + 1; struct(v,-1))
                            |> Some
                    )
                
                // O(|r|)
                let deltaR =
                    rStore 
                    |> IntMap.map (List.map (fun v -> cnt <- cnt + 1; struct(v,1)))

                let deltas = IntMap.append deltaL deltaR

                HashSetDelta(HashMap(cnt, deltas))

            // O(|l|+|r|)
            else
                let mutable cnt = 0

                let del (l: list<'T>) =
                    l |> List.map (fun v -> cnt <- cnt + 1; struct (v, -1))
            
                let add (l: list<'T>) =
                    l |> List.map (fun v -> cnt <- cnt + 1; struct (v, 1))

                let both (_hash: int) (l: list<'T>) (r: list<'T>) =
                    HashSetList.mergeWithOption (fun v l r ->
                        if l && not r then cnt <- cnt + 1; Some -1
                        elif r && not l then cnt <- cnt + 1; Some 1
                        else None
                    ) l r

                let store = IntMap.computeDelta both (IntMap.map del) (IntMap.map add) l.Store r.Store
                HashSetDelta(HashMap(cnt, store))
            
        /// Same as differentiate set empty
        let removeAll (set: HashSet<'T>) =
            let store = set.Store |> IntMap.map (List.map (fun v -> struct (v, -1)))
            HashMap(set.Count, store) |> HashSetDelta
            
        /// Same as differentiate empty set
        let addAll (set: HashSet<'T>) =
            let store = set.Store |> IntMap.map (List.map (fun v -> struct (v, 1)))
            HashMap(set.Count, store) |> HashSetDelta

        /// Applies the given operations to the set. 
        /// Returns the new set and the 'effective' operations.
        let integrate (value: HashSet<'T>) (delta: HashSetDelta<'T>) =
            // O(1)
            if delta.IsEmpty then
                value, delta

            // O(delta)
            elif value.IsEmpty then
                let mutable maxDelta = 0
                let mutable hasRemove = false
                let state = 
                    delta.Store.Store |> IntMap.mapOption (fun l ->
                        let result = 
                            l |> List.choose (fun struct (k, delta) ->
                                if delta > 0 then 
                                    if delta > maxDelta then maxDelta <- delta
                                    Some k
                                else 
                                    hasRemove <- true
                                    None
                            )
                        if List.isEmpty result then None
                        else Some result
                    )

                let delta = 
                    if maxDelta > 1 || hasRemove then 
                        delta.Store |> HashMap.choose (fun _ d -> 
                            if d > 0 then Some 1
                            else None
                        )
                    else 
                        delta.Store

                HashSet(delta.Count, state), HashSetDelta delta

            // O(delta * log N)
            elif delta.Count * 5 < value.Count then
                // delta small
                let mutable result = value
                let effective =
                    delta |> HashSetDelta.choose (fun op ->
                        match op with
                        | Add(_, v) -> 
                            match HashSet.tryAdd v result with
                            | Some newSet ->
                                result <- newSet
                                Some (Add v)
                            | None ->
                                None
                        | Rem(_, v) ->
                            match HashSet.tryRemove v result with
                            | Some newSet ->
                                result <- newSet
                                Some (Rem v)
                            | None ->
                                None
                    )

                result, effective
                
            // TODO: implementation possible?
            //elif value.Count * 5 < delta.Count then

            else
                let mutable effective = HashSetDelta.empty
                let newValue = 
                    delta.Store.Choose2SetSet(value, fun k d o ->
                        match d with
                        | Some d ->
                            if not o && d > 0 then
                                effective <- HashSetDelta.add (Add k) effective
                                true
                            elif o && d < 0 then
                                effective <- HashSetDelta.add (Rem k) effective
                                false
                            else
                                o
                        | None ->
                            o
                    )

                newValue, effective

    /// Functional programming operators related to the HashMap<_,_> type.
    module HashMap =
    
        /// Determines the operations needed to transform l into r.
        /// Returns a HashMapDelta containing all the needed operations.
        let differentiate (l: HashMap<'A, 'B>) (r: HashMap<'A, 'B>): HashMapDelta<'A, 'B> =
            if System.Object.ReferenceEquals(l.Store, r.Store) then
                HashMapDelta.empty
            elif l.Count = 0 && r.Count = 0 then
                HashMapDelta.empty
            elif l.Count = 0 then
                r |> HashMap.map (fun _ v -> Set v) |> HashMapDelta
            elif r.Count = 0 then
                l |> HashMap.map (fun _ _ -> Remove) |> HashMapDelta
            else
                // TODO: one small???
                let merge (_key: 'A) (l: option<'B>) (r: option<'B>) =
                    match l, r with
                        | None, None -> None
                        | Some l, None -> Some Remove
                        | None, Some r -> Some (Set r)
                        | Some l, Some r ->
                            if Unchecked.equals l r then None
                            else Some (Set r)
                HashMap.choose2 merge l r |> HashMapDelta
                
        /// Applies the given operations to the map. 
        /// Returns the new map and the 'effective' operations.
        let integrate (m: HashMap<'A, 'B>) (delta: HashMapDelta<'A, 'B>) =
            if delta.Store.Count = 0 then
                m, delta
            elif m.Count = 0 then
                let state, delta = 
                    delta.Store.ChooseTup(fun _ op ->
                        match op with
                        | Set v -> Some (v, Set v)
                        | _ -> None
                    )
                state, HashMapDelta delta
            else
                let mutable effective = HashMap.empty
                let mutable m = m
                for (k,v) in delta do
                    m <- m.Alter(k, fun o ->
                        match o, v with
                            | Some o, Remove ->
                                effective <- HashMap.add k Remove effective
                                None
                            | None, Remove ->
                                None

                            | None, Set n ->
                                effective <- HashMap.add k (Set n) effective
                                Some n

                            | Some o, Set n ->
                                if not (Unchecked.equals o n) then
                                    effective <- HashMap.add k (Set n) effective

                                Some n
                    )

                m, HashMapDelta effective

    /// Functional programming operators related to the IndexList<_> type.
    module IndexList =
        
        /// Determines the operations needed to transform l into r.
        /// Returns a IndexListDelta containing these operations.
        let integrate (x : IndexList<'T>) (deltas : IndexListDelta<'T>) =
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
        let differentiate (l : IndexList<'T>) (r : IndexList<'T>) : IndexListDelta<'T> =
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
                let merge (k : Index) (l : Option<'T>) (r : Option<'T>) =
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

