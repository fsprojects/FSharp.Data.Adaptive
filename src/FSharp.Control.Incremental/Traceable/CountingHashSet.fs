namespace FSharp.Control.Traceable

open System
open System.Collections
open System.Collections.Generic
open FSharp.Control.Incremental

type internal SetCmp =
    | Distinct          = 0
    | ProperSubset      = 1
    | ProperSuperset    = 2
    | Overlap           = 3
    | Equal             = 4

[<Struct; StructuralEquality; NoComparison>]
[<StructuredFormatDisplay("{AsString}")>]
type CountingHashSet<'a>(store : HashMap<'a, int>) =
    
    static let trace =
        {
            tmonoid = DHashSet<'a>.Monoid
            tempty = CountingHashSet<'a>(HashMap.empty)
            tintegrate = fun s d -> s.ApplyDelta d
            tdifferentiate = fun l r -> l.ComputeDelta r
            tcollapse = fun _ _ -> false
        }

    static let traceNoRefCount =
        {
            tmonoid = DHashSet<'a>.Monoid
            tempty = CountingHashSet<'a>(HashMap.empty)
            tintegrate = fun s d -> s.ApplyDeltaNoRefCount d
            tdifferentiate = fun l r -> l.ComputeDelta r
            tcollapse = fun _ _ -> false
        }

    static member Empty = CountingHashSet<'a>(HashMap.empty)

    static member Trace = trace

    static member TraceNoRefCount = traceNoRefCount

    member x.IsEmpty = store.IsEmpty

    member x.Count = store.Count

    member private x.Store = store

    member x.Contains (value : 'a) =
        HashMap.containsKey value store

    member x.GetRefCount (value : 'a) =
        HashMap.tryFind value store |> Option.defaultValue 0

    member x.Add(value : 'a) =
        store
        |> HashMap.update value (fun o -> 
            match o with
                | Some o -> o + 1
                | None -> 1
        )
        |> CountingHashSet

    member x.Remove(value : 'a) =
        store
        |> HashMap.alter value (fun o ->
            match o with
                | Some 1 -> None
                | Some c -> Some (c - 1)
                | None -> None
        )
        |> CountingHashSet

    member x.Alter(value : 'a, f : int -> int) =
        store
        |> HashMap.alter value (fun o ->
            let o = defaultArg o 0
            let n = f o
            if n > 0 then
                Some n
            else
                None
        )
        |> CountingHashSet

    member x.Union(other : CountingHashSet<'a>) =
        HashMap.map2 (fun k l r ->
            match l, r with 
                | Some l, Some r -> l + r
                | Some l, None -> l
                | None, Some r -> r
                | None, None -> 0
        ) store other.Store
        |> CountingHashSet

    member x.Difference(other : CountingHashSet<'a>) =
        HashMap.choose2 (fun k l r ->
            let newRefCount = 
                match l, r with 
                    | Some l, Some r -> l - r
                    | Some l, None -> l
                    | None, Some r -> 0
                    | None, None -> 0

            if newRefCount > 0 then Some newRefCount
            else None
        ) store other.Store
        |> CountingHashSet

    member x.Intersect(other : CountingHashSet<'a>) =
        HashMap.choose2 (fun k l r ->
            match l, r with 
                | Some l, Some r -> Some (min l r)
                | _ -> None
        ) store other.Store
        |> CountingHashSet

    member x.UnionWith(other : CountingHashSet<'a>, f : int -> int -> int) =
        HashMap.map2 (fun k l r ->
            match l, r with 
                | Some l, Some r -> f l r
                | Some l, None -> f l 0
                | None, Some r -> f 0 r
                | None, None -> f 0 0
        ) store other.Store
        |> CountingHashSet

    member x.ToHashMap() =
        store

    member x.ToSeq() =
        store.ToSeq() |> Seq.map fst

    member x.ToList() =
        store.ToList() |> List.map fst

    member x.ToArray() =
        let result = Array.zeroCreate store.Count
        let mutable i = 0
        for (key, _value) in store do
            result.[i] <- key
            i <- i + 1
        result


    member x.Map(mapping : 'a -> 'b) =
        let mutable res = HashMap.empty
        for (k,v) in HashMap.toSeq store do
            let k = mapping k
            res <- res |> HashMap.update k (fun o -> defaultArg o 0 + v) 

        CountingHashSet res

    member x.Choose(mapping : 'a -> Option<'b>) =
        let mutable res = HashMap.empty
        for (k,v) in HashMap.toSeq store do
            match mapping k with
                | Some k ->
                    res <- res |> HashMap.update k (fun o -> defaultArg o 0 + v) 
                | None ->
                    ()

        CountingHashSet res

    member x.Filter(predicate : 'a -> bool) =
        store |> HashMap.filter (fun k _ -> predicate k) |> CountingHashSet

    member x.Collect(mapping : 'a -> CountingHashSet<'b>) =
        let mutable res = CountingHashSet<'b>.Empty
        for (k,ro) in store.ToSeq() do
            let r = mapping k
            if ro = 1 then
                res <- res.Union r
            else
                res <- res.UnionWith(r, fun li ri -> li + ro * ri)
        res

    member x.Iter (iterator : 'a -> unit) =
        store |> HashMap.iter (fun k _ -> iterator k)

    member x.Exists (predicate : 'a -> bool) =
        store |> HashMap.exists (fun k _ -> predicate k)

    member x.Forall (predicate : 'a -> bool) =
        store |> HashMap.forall (fun k _ -> predicate k)

    member x.Fold (seed : 's, folder : 's -> 'a -> 's) =
        store |> HashMap.fold (fun s k _ -> folder s k) seed 


    static member OfSeq (seq : seq<'a>) =
        let mutable res = CountingHashSet<'a>.Empty
        for e in seq do
            res <- res.Add e
        res

    static member OfList (list : list<'a>) =
        CountingHashSet<'a>.OfSeq list

    static member OfArray (arr : 'a[]) =
        CountingHashSet<'a>.OfSeq arr

    static member OfHashMap (map : HashMap<'a, int>) =
        CountingHashSet map

    member x.ComputeDelta(other : CountingHashSet<'a>) =
        // O(1)
        if Object.ReferenceEquals(store.Store, other.Store.Store) then
            DHashSet.empty

        // O(other)
        elif store.IsEmpty then 
            other.Store |> HashMap.map (fun _ _ -> 1) |> DHashSet.ofHashMap

        // O(N)
        elif other.IsEmpty then
            store |> HashMap.map (fun _ _ -> -1) |> DHashSet.ofHashMap
        
        // O(N + other)
        else
            let mutable cnt = 0

            let del (l : list<struct ('a * int)>) =
                l |> List.map (fun struct(v,_) -> cnt <- cnt + 1; struct (v, -1))
            
            let add (l : list<struct('a * int)>) =
                l |> List.map (fun struct(v,_) -> cnt <- cnt + 1; struct (v, 1))

            let both (_hash : int) (l : list<struct('a * int)>) (r : list<struct('a * int)>) =
                HashMapList.mergeWithOption' (fun v l r ->
                    match l, r with
                        | Some l, None ->  cnt <- cnt + 1; Some -1
                        | None, Some r ->  cnt <- cnt + 1; Some 1
                        | _ -> None
                ) l r

            let store = IntMap.computeDelta both (IntMap.map del) (IntMap.map add) store.Store other.Store.Store
            DHashSet (HashMap(cnt, store))

    member x.ApplyDelta (deltas : DHashSet<'a>) =
        // O(1)
        if deltas.IsEmpty then
            x, deltas

        // O(Delta)
        elif store.IsEmpty then
            let mutable maxDelta = 0
            let state = deltas |> DHashSet.toHashMap |> HashMap.filter (fun _ d -> maxDelta <- max maxDelta d; d > 0)
            let delta = 
                if maxDelta > 1 then state |> HashMap.map (fun _ _ -> 1)
                else state

            CountingHashSet state, DHashSet delta

        // O(Delta * log N)
        elif deltas.Count * 5 < store.Count then
            let mutable res = store

            let effective =
                deltas |> DHashSet.choose (fun d ->
                    let mutable delta = Unchecked.defaultof<SetOperation<'a>>
                    let value = d.Value
                    res <- res |> HashMap.alter value (fun cnt ->
                        let o = defaultArg cnt 0
                        let n = o + d.Count
                        if n > 0 && o = 0 then 
                            delta <- Add(value)
                        elif n = 0 && o > 0 then
                            delta <- Rem(value)

                        if n <= 0 then None
                        else Some n
                    )

                    if delta.Count <> 0 then Some delta
                    else None
                )

            CountingHashSet res, effective

        // O(Delta + N)
        else
            let mutable effective = DHashSet.empty
            let deltas = DHashSet.toHashMap deltas
            let newStore = 
                HashMap.choose2 (fun k s d ->
                    match d with
                        | Some d ->
                            let o = Option.defaultValue 0 s 
                            let n = d + o
                            if o = 0 && n > 0 then
                                effective <- DHashSet.add (Add k) effective
                            elif o > 0 && n = 0 then
                                effective <- DHashSet.add (Rem k) effective
                            
                            if n <= 0 then None
                            else Some n
                        | None ->
                            s
                ) store deltas

            CountingHashSet newStore, effective

    member x.ApplyDeltaNoRefCount (deltas : DHashSet<'a>) =
        // O(1)
        if deltas.IsEmpty then
            x, deltas

        // O(Delta)
        elif store.IsEmpty then
            let state = deltas |> DHashSet.toHashMap |> HashMap.choose (fun _ d -> if d > 0 then Some 1 else None)
            CountingHashSet state, DHashSet state

        // O(Delta * log N)
        elif deltas.Count * 5 < store.Count then
            let mutable res = store

            let effective =
                deltas |> DHashSet.choose (fun d ->
                    let mutable delta = Unchecked.defaultof<SetOperation<'a>>
                    let value = d.Value
                    res <- res |> HashMap.alter value (fun cnt ->
                        let o = defaultArg cnt 0
                        let n = 
                            if d.Count > 0 then 1 
                            elif d.Count < 0 then 0
                            else o

                        if n > 0 && o = 0 then 
                            delta <- Add(value)
                        elif n = 0 && o > 0 then
                            delta <- Rem(value)

                        if n <= 0 then None
                        else Some n
                    )

                    if delta.Count <> 0 then Some delta
                    else None
                )

            CountingHashSet res, effective
        
        // O(Delta + N)
        else
            let mutable effective = DHashSet.empty
            let deltas = DHashSet.toHashMap deltas
            let newStore = 
                HashMap.choose2 (fun k s d ->
                    match d with
                        | Some d ->
                            let o = Option.defaultValue 0 s 
                            let n = if d > 0 then 1 elif d < 0 then 0 else o

                            if o = 0 && n > 0 then
                                effective <- DHashSet.add (Add k) effective
                            elif o > 0 && n = 0 then
                                effective <- DHashSet.add (Rem k) effective
                            
                            if n <= 0 then None
                            else Some n
                        | None ->
                            s
                ) store deltas

            CountingHashSet newStore, effective

    static member internal Compare(l : CountingHashSet<'a>, r : CountingHashSet<'a>) =
        let i = l.Intersect r
        let b = i.Count
        let lo = l.Count - b
        let ro = r.Count - b

        match lo, b, ro with
            | 0, _, 0 -> SetCmp.Equal
            | _, 0, _ -> SetCmp.Distinct
            | a, _, 0 -> SetCmp.ProperSuperset
            | 0, _, a -> SetCmp.ProperSubset
            | _, _, _ -> SetCmp.Overlap


    override x.ToString() =
        let suffix =
            if x.Count > 5 then "; ..."
            else ""

        let content =
            x.ToSeq() |> Seq.truncate 5 |> Seq.map (sprintf "%A") |> String.concat "; "

        "CountingHashSet [" + content + suffix + "]"

    member private x.AsString = x.ToString()

    interface IEnumerable with
        member x.GetEnumerator() = new CountingHashSetEnumerator<_>(store) :> _

    interface IEnumerable<'a> with
        member x.GetEnumerator() = new CountingHashSetEnumerator<_>(store) :> _

and private CountingHashSetEnumerator<'a>(store : HashMap<'a, int>) =
    let e = (store :> seq<_>).GetEnumerator()

    member x.Current = 
        let (v,_) = e.Current
        v

    interface IEnumerator with
        member x.MoveNext() = e.MoveNext()
        member x.Current = x.Current :> obj
        member x.Reset() = e.Reset()

    interface IEnumerator<'a> with
        member x.Dispose() = e.Dispose()
        member x.Current = x.Current

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CountingHashSet =
    let inline empty<'a> = CountingHashSet<'a>.Empty


    let single v = CountingHashSet (HashMap.single v 1)

    // O(1)
    let inline toHashMap (set : CountingHashSet<'a>) = set.ToHashMap()

    // O(n)
    let inline toSeq (set : CountingHashSet<'a>) = set.ToSeq()
    
    // O(n)
    let inline toList (set : CountingHashSet<'a>) = set.ToList()

    // O(n)
    let inline toArray (set : CountingHashSet<'a>) = set.ToArray()
    

    // O(1)
    let inline ofHashMap (map : HashMap<'a, int>) = CountingHashSet.OfHashMap map

    // O(n)
    let inline ofSeq (seq : seq<'a>) = CountingHashSet.OfSeq seq

    // O(n)
    let inline ofList (list : list<'a>) = CountingHashSet.OfList list

    // O(n)
    let inline ofArray (arr : 'a[]) = CountingHashSet.OfArray arr




    // O(1)
    let inline isEmpty (set : CountingHashSet<'a>) =
        set.IsEmpty

    // O(1)
    let inline count (set : CountingHashSet<'a>) =
        set.Count

    let inline refcount (value : 'a) (set : CountingHashSet<'a>) =
        set.GetRefCount value

    let inline contains (value : 'a) (set : CountingHashSet<'a>) =
        set.Contains value

    // O(min(n,32))
    let inline add (value : 'a) (set : CountingHashSet<'a>) =
        set.Add value
        
    // O(min(n,32))
    let inline remove (value : 'a) (set : CountingHashSet<'a>) =
        set.Remove value

    // O(n + m)
    let inline union (l : CountingHashSet<'a>) (r : CountingHashSet<'a>) =
        l.Union r

    // O(n + m)
    let inline difference (l : CountingHashSet<'a>) (r : CountingHashSet<'a>) =
        l.Difference r

    // O(n + m)
    let inline intersect (l : CountingHashSet<'a>) (r : CountingHashSet<'a>) =
        l.Intersect r

    let inline alter (value : 'a) (f : int -> int) (set : CountingHashSet<'a>) =
        set.Alter(value, f)

    // O(n)
    let inline map (mapping : 'a -> 'b) (set : CountingHashSet<'a>) =
        set.Map mapping

    // O(n)
    let inline choose (mapping : 'a -> Option<'b>) (set : CountingHashSet<'a>) =
        set.Choose mapping

    // O(n)
    let inline filter (predicate : 'a -> bool) (set : CountingHashSet<'a>) =
        set.Filter predicate

    // O(sum ni)
    let inline collect (mapping : 'a -> CountingHashSet<'b>) (set : CountingHashSet<'a>) =
        set.Collect mapping

    // O(n)
    let inline iter (iterator : 'a -> unit) (set : CountingHashSet<'a>) =
        set.Iter iterator

    // O(n)
    let inline exists (predicate : 'a -> bool) (set : CountingHashSet<'a>) =
        set.Exists predicate

    // O(n)
    let inline forall (predicate : 'a -> bool) (set : CountingHashSet<'a>) =
        set.Forall predicate

    // O(n)
    let inline fold (folder : 's -> 'a -> 's) (seed : 's) (set : CountingHashSet<'a>) =
        set.Fold(seed, folder)



    
    let inline trace<'a> = CountingHashSet<'a>.Trace
    let inline traceNoRefCount<'a> = CountingHashSet<'a>.TraceNoRefCount

    // O(n + m)
    let inline computeDelta (src : CountingHashSet<'a>) (dst : CountingHashSet<'a>) =
        src.ComputeDelta dst

    // O(|delta| * min(32, n))
    let inline applyDelta (set : CountingHashSet<'a>) (delta : DHashSet<'a>) =
        set.ApplyDelta delta

    // O(|delta| * min(32, n))
    let inline applyDeltaNoRefCount (set : CountingHashSet<'a>) (delta : DHashSet<'a>) =
        set.ApplyDeltaNoRefCount delta
        
    // O(n + m)
    let internal compare (l : CountingHashSet<'a>) (r : CountingHashSet<'a>) =
        CountingHashSet.Compare(l,r)

