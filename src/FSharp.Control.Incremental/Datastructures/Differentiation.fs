namespace FSharp.Control.Incremental

/// differentiation extensions for several immutable datastructures.
[<AutoOpen>]
module DifferentiationExtensions =

    /// Functional programming operators related to the HashSet<_> type.
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module HashSet =
        /// determines the operations needed to transform l into r.
        /// returns a DHashSet containing all the needed operations.
        let differentiate (l : HashSet<'a>) (r : HashSet<'a>) =
            // O(1)
            if System.Object.ReferenceEquals(l.Store, r.Store) then
                DHashSet.empty

            // O(|r|)
            elif l.IsEmpty then 
                let delta = r.Store |> IntMap.map (List.map (fun v -> struct (v, 1)))
                HashMap(r.Count, delta) |> DHashSet

            // O(|l|)
            elif r.IsEmpty then
                let delta = l.Store |> IntMap.map (List.map (fun v -> struct (v, -1)))
                HashMap(l.Count, delta) |> DHashSet
        
            // TODO: |l|*log|r| and |r|*log|l| implementations should exists
            // which will most likely be faster than |l|+|r| when one of the sets is small.

            // O(max |l| |r|*log|l|)
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

                DHashSet(HashMap(cnt, deltas))

            // O(|l| + |r|)
            else
                let mutable cnt = 0

                let del (l : list<'a>) =
                    l |> List.map (fun v -> cnt <- cnt + 1; struct (v, -1))
            
                let add (l : list<'a>) =
                    l |> List.map (fun v -> cnt <- cnt + 1; struct (v, 1))

                let both (_hash : int) (l : list<'a>) (r : list<'a>) =
                    HashSetList.mergeWithOption (fun v l r ->
                        if l && not r then cnt <- cnt + 1; Some -1
                        elif r && not l then cnt <- cnt + 1; Some 1
                        else None
                    ) l r

                let store = IntMap.computeDelta both (IntMap.map del) (IntMap.map add) l.Store r.Store
                DHashSet(HashMap(cnt, store))
            
        /// applies the given operations to the set. 
        /// returns the new set and the 'effective' operations.
        let integrate (value : HashSet<'a>) (delta : DHashSet<'a>) =
            // O(1)
            if delta.IsEmpty then
                value, delta

            // O(Delta)
            elif value.IsEmpty then
                let mutable maxDelta = 0

                let state = 
                    delta.Store.Store |> IntMap.map (
                        List.map (fun struct (k, delta) ->
                            if delta > maxDelta then maxDelta <- delta
                            k
                        )
                    )

                let delta = 
                    if maxDelta > 1 then delta.Store |> HashMap.map (fun _ _ -> 1)
                    else delta.Store

                HashSet(delta.Count, state), DHashSet delta

            // O(Delta * log N)
            else
                let mutable res = value
                let effective =
                    delta |> DHashSet.choose (fun d ->
                        let value = d.Value
                        let contained = HashSet.contains value res
                        if contained && d.Count < 0 then
                            res <- HashSet.remove value res
                            Some (Rem value)
                        elif not contained && d.Count > 0 then
                            res <- HashSet.add value res
                            Some (Add value)
                        else
                            None
                    )

                res, effective

    /// Functional programming operators related to the HashMap<_,_> type.
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module HashMap =
    
        /// determines the operations needed to transform l into r.
        /// returns a DHashMap containing all the needed operations.
        let differentiate (l : HashMap<'k, 'v>) (r : HashMap<'k, 'v>) : DHashMap<'k, 'v> =
            if System.Object.ReferenceEquals(l.Store, r.Store) then
                DHashMap.empty
            elif l.Count = 0 && r.Count = 0 then
                DHashMap.empty
            elif l.Count = 0 then
                r |> HashMap.map (fun _ v -> Set v) |> DHashMap
            elif r.Count = 0 then
                l |> HashMap.map (fun _ _ -> Remove) |> DHashMap
            else
                // TODO: one small???
                let merge (_key : 'k) (l : Option<'v>) (r : Option<'v>) =
                    match l, r with
                        | None, None -> None
                        | Some l, None -> Some Remove
                        | None, Some r -> Some (Set r)
                        | Some l, Some r ->
                            if Unchecked.equals l r then None
                            else Some (Set r)
                HashMap.choose2 merge l r |> DHashMap
                
        /// applies the given operations to the map. 
        /// returns the new map and the 'effective' operations.
        let integrate (m : HashMap<'k, 'v>) (delta : DHashMap<'k, 'v>) =
            if delta.Count = 0 then
                m, delta
            elif m.Count = 0 then
                let state, delta = 
                    delta.Store.ChooseTup(fun _ op ->
                        match op with
                        | Set v -> Some (v, Set v)
                        | _ -> None
                    )
                state, DHashMap delta
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

                m, DHashMap effective

