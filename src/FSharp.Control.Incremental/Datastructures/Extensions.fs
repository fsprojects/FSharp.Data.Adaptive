namespace FSharp.Control.Incremental

open System.Runtime.CompilerServices

[<AbstractClass; Sealed; Extension>]
type CollectionExtensions private() =
    /// creates a HashSet holding all keys from the map.
    /// `O(N)`
    [<Extension>]
    static member GetKeys(map : HashMap<'k, 'v>) =
        let setStore =
            map.Store |> IntMap.map (
                List.map (fun struct(k,_v) -> k)
            )
        HashSet(map.Count, setStore)


[<AutoOpen>]
module CollectionExtensions =

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


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module HashMap =
        /// creates a HashSet holding all keys from the map.
        /// `O(N)`
        let inline keys (map : HashMap<'k, 'v>) = map.GetKeys()

