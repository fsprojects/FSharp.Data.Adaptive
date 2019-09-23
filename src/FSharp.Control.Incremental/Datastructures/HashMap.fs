namespace FSharp.Control.Incremental


open System.Collections
open System.Collections.Generic
open FSharp.Control.Incremental

/// helper functions for hash-collision lists.
/// most members have bad runtime, but the lists should be quite small when using appropriate hashCodes.
module internal HashMapList =

    let inline combineHash (a : int) (b : int) =
        uint32 a ^^^ uint32 b + 0x9e3779b9u + ((uint32 a) <<< 6) + ((uint32 a) >>> 2) |> int

    let rec alter (k : 'k) (f : option<'v> -> option<'v>) (l : list<struct ('k * 'v)>) =
        match l with
            | [] ->
                match f None with
                    | None -> []
                    | Some v -> [struct (k,v)]

            | struct (k1, v1) :: rest ->
                if Unchecked.equals k k1 then
                    match f (Some v1) with
                        | None -> rest
                        | Some v2 -> struct (k1, v2) :: rest
                else
                    struct(k1, v1) :: alter k f rest

    let rec alter' (cnt : byref<int>) (k : 'k) (f : option<'v> -> option<'v>) (l : list<struct ('k * 'v)>) =
        match l with
            | [] ->
                match f None with
                    | None -> []
                    | Some v -> 
                        cnt <- cnt + 1
                        [struct (k,v)]

            | struct(k1, v1) :: rest ->
                if Unchecked.equals k k1 then
                    match f (Some v1) with
                        | None -> 
                            cnt <- cnt - 1
                            rest
                        | Some v2 -> 
                            struct (k1, v2) :: rest
                else
                    struct (k1, v1) :: alter' &cnt k f rest

    let rec update (k : 'k) (f : option<'v> -> 'v) (l : list<struct ('k * 'v)>) =
        match l with
            | [] -> 
                let v = f None
                [struct (k, v)]

            | (k1, v1) :: rest ->
                if Unchecked.equals k k1 then
                    let v2 = f (Some v1)
                    struct (k1, v2) :: rest
                else
                    struct (k1, v1) :: update k f rest

    let rec add (cnt : byref<int>) (k : 'k) (v : 'v) (l : list<struct ('k * 'v)>) =
        match l with
            | [] ->     
                cnt <- cnt + 1
                [struct (k,v)]
            | struct(k1, v1) :: rest ->
                if Unchecked.equals k k1 then
                    struct(k1, v) :: rest
                else
                    struct(k1, v1) :: add &cnt k v rest

    let rec remove (cnt : byref<int>) (k : 'k) (l : list<struct('k * 'v)>) =
        match l with
            | [] -> []
            | struct(k1, v1) :: rest ->
                if Unchecked.equals k k1 then
                    cnt <- cnt - 1
                    rest
                else
                    struct(k1, v1) :: remove &cnt k rest

    let rec tryRemove (k : 'k) (l : list<struct('k * 'v)>) =
        match l with
            | [] -> None
            | struct(k1,v1) :: rest ->
                if Unchecked.equals k k1 then
                    Some (v1, rest)
                else
                    match tryRemove k rest with
                        | None -> None
                        | Some(v,rest) -> Some(v, struct(k1,v1)::rest)

    let rec unionWith (f : 'k -> 'v -> 'v -> 'v) (l : list<struct('k * 'v)>) (r : list<struct('k * 'v)>) =
        let newL = 
            l |> List.map (fun struct(lk, lv) ->
                let other = r |> List.tryFind (fun struct(rk, rv) -> Unchecked.equals rk lk)
                match other with
                    | Some (_,rv) -> struct(lk, f lk lv rv)
                    | None -> struct(lk, lv)
            )
        let newR =
            r |> List.filter (fun struct (rk,_) ->
                l |> List.forall (fun struct (lk,_) -> not (Unchecked.equals lk rk))
            )

        newL @ newR

    let rec mergeWith (f : 'k -> option<'a> -> option<'b> -> 'c) (l : list<struct ('k * 'a)>) (r : list<struct ('k * 'b)>) =
        let newL = 
            l |> List.choose (fun struct(lk, lv) ->
                let other = r |> List.tryFind (fun struct(rk, rv) -> Unchecked.equals rk lk)
                match other with
                    | Some (_,rv) -> 
                        Some (struct (lk, f lk (Some lv) (Some rv)))
                    | None -> 
                        Some (struct (lk, f lk (Some lv) None))
            )
        let newR =
            r |> List.choose (fun struct(rk,rv) ->
                if l |> List.forall (fun struct(lk,_) -> not (Unchecked.equals lk rk)) then
                    Some (struct (rk, f rk None (Some rv)))
                else 
                    None
            )

        newL @ newR

    let rec mergeWithOption (f : 'k -> option<'a> -> option<'b> -> option<'c>) (l : list<struct('k * 'a)>) (r : list<struct('k * 'b)>) =
        let newL = 
            l |> List.choose (fun struct(lk, lv) ->
                let other = r |> List.tryFind (fun struct(rk, rv) -> Unchecked.equals rk lk)
                match other with
                    | Some (_,rv) -> 
                        match f lk (Some lv) (Some rv) with
                            | Some r -> Some (struct (lk, r))
                            | None -> None
                    | None -> 
                        match f lk (Some lv) None with
                            | Some r -> Some (struct (lk, r))
                            | None -> None
            )
        let newR =
            r |> List.choose (fun struct(rk,rv) ->
                if l |> List.forall (fun struct(lk,_) -> not (Unchecked.equals lk rk)) then
                    match f rk None (Some rv) with
                        | Some r -> Some (struct (rk, r))
                        | None -> None
                else 
                    None
            )

        newL @ newR

    let rec mergeWithOptionSetMap (f : 'k -> option<'a> -> bool -> option<'c>) (l : list<struct('k * 'a)>) (r : list<'k>) =
        let newL = 
            l |> List.choose (fun struct(lk, lv) ->
                let other = r |> List.tryFind (fun rk -> Unchecked.equals rk lk)
                match other with
                    | Some rv -> 
                        match f lk (Some lv) true with
                            | Some r -> Some (struct (lk, r))
                            | None -> None
                    | None -> 
                        match f lk (Some lv) false with
                            | Some r -> Some (struct (lk, r))
                            | None -> None
            )
        let newR =
            r |> List.choose (fun rk ->
                if l |> List.forall (fun struct(lk,_) -> not (Unchecked.equals lk rk)) then
                    match f rk None true with
                        | Some r -> Some (struct (rk, r))
                        | None -> None
                else 
                    None
            )

        newL @ newR
        
    let rec mergeWithOptionSetSet (f : 'k -> option<'a> -> bool -> bool) (l : list<struct('k * 'a)>) (r : list<'k>) =
        let newL = 
            l |> List.choose (fun struct(lk, lv) ->
                let other = r |> List.tryFind (fun rk -> Unchecked.equals rk lk)
                match other with
                    | Some rv -> 
                        match f lk (Some lv) true with
                            | true -> Some lk
                            | false -> None
                    | None -> 
                        match f lk (Some lv) false with
                            | true -> Some lk
                            | false -> None
            )
        let newR =
            r |> List.choose (fun rk ->
                if l |> List.forall (fun struct(lk,_) -> not (Unchecked.equals lk rk)) then
                    match f rk None true with
                        | true -> Some rk
                        | false -> None
                else 
                    None
            )

        newL @ newR

    let rec mergeWithOption' (f : 'k -> option<'a> -> option<'b> -> option<'c>) (l : list<struct('k * 'a)>) (r : list<struct('k * 'b)>) =
        let newL = 
            l |> List.choose (fun struct(lk,lv) ->
                let other = r |> List.tryFind (fun struct(rk,_) -> Unchecked.equals rk lk) |> Option.map (fun struct(_,a) -> a)
                let other =
                    match other with
                    | Some v -> Some v
                    | None -> None
                
                match f lk (Some lv) other with
                    | Some r -> Some (struct (lk, r))
                    | None -> None
            )
        let newR =
            r |> List.choose (fun struct (rk, rv) ->
                if l |> List.forall (fun struct(lk,_) -> not (Unchecked.equals lk rk)) then
                    match f rk None (Some rv) with
                        | Some r -> Some (struct (rk, r))
                        | None -> None
                else 
                    None
            )
        match newL with
            | [] -> 
                match newR with
                    | [] -> None
                    | _ -> Some newR
            | _ ->
                match newR with
                    | [] -> Some newL
                    | _ -> Some (newL @ newR)

    let rec equals (l : list<struct('k * 'a)>) (r : list<struct('k * 'a)>) =
        let mutable r = r
        let mutable eq = true
        
        use e = (l :> seq<_>).GetEnumerator()
        while eq && e.MoveNext() do
            let struct (lk, lv) = e.Current
            match tryRemove lk r with
                | Some(rv, nr) ->
                    r <- nr
                    eq <- Unchecked.equals lv rv
                | _ ->
                    eq <- false

        eq && List.isEmpty r
            

/// Immutable hash-based map datastructure.
/// hash/equality are determined using the Unchecked module
[<Struct; CustomEquality; NoComparison>]
[<StructuredFormatDisplay("{AsString}")>]
type HashMap<'k, [<EqualityConditionalOn>] 'v> internal(cnt : int, store : intmap<list<struct ('k * 'v)>>) =
    static let empty = HashMap<'k, 'v>(0, IntMap.empty)

    /// internal for getting the IntMap store
    member internal x.Store = store

    /// the empty HashMap.
    static member Empty = empty
    
    /// is the map empty? `O(1)`
    member x.IsEmpty = cnt = 0

    /// the number of elements in the map `O(1)`
    member x.Count = cnt

    /// adds, deletes or updates the entry for the given key.
    /// the update functions gets the optional old value and may optionally return
    /// a new value (or None for deleting the entry).
    /// `O(log N)`
    member x.Alter (key : 'k, update : option<'v> -> option<'v>) =
        let hash = Unchecked.hash key

        let mutable changed = false
        let mutable deltaCnt = 0
        let update (old : option<list<struct('k * 'v)>>) =
            match old with
            | None -> 
                match update None with
                | Some v -> 
                    deltaCnt <- 1
                    changed <- true
                    Some [struct(key, v)]
                | None -> 
                    None
            | Some oldList ->
                match HashMapList.tryRemove key oldList with
                | Some (oldValue, rest) ->
                    match update (Some oldValue) with
                    | None ->
                        deltaCnt <- -1
                        changed <- true
                        match rest with
                        | [] -> None
                        | _ -> Some rest
                    | Some newValue ->
                        if Unchecked.equals oldValue newValue then
                            Some oldList
                        else
                            changed <- true
                            Some (struct(key, newValue)::rest)
                | None ->
                    match update None with
                    | Some v ->
                        changed <- true
                        deltaCnt <- 1
                        Some (struct(key, v) :: oldList)
                    | None ->
                        Some oldList

        let newStore = store |> IntMap.alter update hash
        if changed then HashMap(cnt + deltaCnt, newStore)
        else x

    /// adds or updates the entry for the given key based on the optional current value. 
    /// `O(log N)`
    member x.Update (key : 'k, f : option<'v> -> 'v) =
        x.Alter(key, f >> Some)
            
    /// adds or updates the entry for the given key. `O(log N)`
    member x.Add (key : 'k, value : 'v) =
        let hash = Unchecked.hash key
        let mutable cnt = cnt
        let newMap = 
            store |> IntMap.alter (fun l ->
                match l with
                    | None -> 
                        cnt <- cnt + 1
                        Some [key,value]
                    | Some l -> 
                        Some (HashMapList.add &cnt key value l)
            ) hash
        HashMap(cnt, newMap)

    /// removes the entry for the given key. `O(log N)`
    member x.Remove (key : 'k) =
        let mutable cnt = cnt
        let hash = Unchecked.hash key
        let newMap = 
            store |> IntMap.update (fun l ->
                match HashMapList.remove &cnt key l with
                    | [] -> None
                    | l -> Some l
            ) hash
        HashMap(cnt, newMap)

    /// tests if an entry for the given key exists. `O(log N)`
    member x.ContainsKey (key : 'k) =
        let hash = Unchecked.hash key
        match IntMap.tryFind hash store with
            | Some l -> l |> List.exists (fun struct (k,_) -> Unchecked.equals k key)
            | None -> false
        
    /// creates a HashSet holding all keys from the map.
    /// `O(N)`
    member x.GetKeys() =
        let setStore =
            store |> IntMap.map (
                List.map (fun struct(k,_v) -> k)
            )
        HashSet(cnt, setStore)


    /// creates a new map (with the same keys) by applying the given function to all entries.
    /// `O(N)`
    member x.Map(mapping : 'k -> 'v -> 'b) =
        let newStore = 
            store 
                |> IntMap.map (fun l -> l |> List.map (fun struct (k,v) -> struct (k, mapping k v)))
        HashMap(cnt, newStore)

    /// creates two maps (with the same keys) by applying the given function to all entries.
    /// `O(N)`
    member x.ChooseTup(mapping : 'k -> 'v -> option<'b * 'c>) =
        let mutable cnt = 0
        let mapping (struct (k : 'k, v : 'v)) =
            match mapping k v with
                | Some b -> 
                    cnt <- cnt + 1
                    Some (struct(k,b))
                | None -> 
                    None

        let a, b = 
            store
                |> IntMap.mapOptionWithKey2 (fun _ l ->
                    match List.choose mapping l with
                        | [] -> None
                        | l ->  
                            let ll = l |> List.map (fun struct(k,(l,_)) -> struct (k, l))
                            let rl = l |> List.map (fun struct(k,(_,r)) -> struct (k, r))
                            Some (ll, rl)
                   )
        HashMap(cnt, a), HashMap(cnt, b)
        
    /// creates a new map (with the same keys) by applying the given function to all entries.
    /// `O(N)`
    member x.Choose(mapping : 'k -> 'v -> option<'b>) =
        let mutable cnt = 0
        let mapping (struct (k : 'k, v : 'v)) =
            match mapping k v with
                | Some b -> 
                    cnt <- cnt + 1
                    Some (struct (k,b))
                | None -> 
                    None

        let newStore = 
            store
                |> IntMap.mapOption (fun l ->
                    match List.choose mapping l with
                        | [] -> None
                        | l -> Some l
                   )
        HashMap(cnt, newStore)

    /// creates a new map (with the same keys) that contains all entries for which predicate was true.
    /// `O(N)`
    member x.Filter(predicate : 'k -> 'v -> bool) =
        let mutable cnt = 0
        let predicate (struct (k, v)) =
            if predicate k v then
                cnt <- cnt + 1
                true
            else
                false

        let newStore = 
            store |> IntMap.mapOption (fun l ->
                match l |> List.filter predicate with
                    | [] -> None
                    | l -> Some l
            )
        HashMap(cnt, newStore)

    /// applies the iter function to all entries of the map.
    /// `O(N)`
    member x.Iter(iter : 'k -> 'v -> unit) =
        store |> IntMap.toSeq |> Seq.iter (fun (_,l) ->
            l |> List.iter (fun struct(k,v) -> iter k v)
        )
        
    /// tests whether an entry making the predicate true exists.
    /// `O(N)`
    member x.Exists(predicate : 'k -> 'v -> bool) =
        store |> IntMap.toSeq |> Seq.exists (fun (_,v) ->
            v |> List.exists (fun struct(k,v) -> predicate k v)
        )

    /// tests whether all entries fulfil the given predicate.
    /// `O(N)`
    member x.Forall(predicate : 'k -> 'v -> bool) =
        store |> IntMap.toSeq |> Seq.forall (fun (_,v) ->
            v |> List.forall (fun struct (k,v) -> predicate k v)
        )

    /// folds over all entries of the map.
    /// note that the order for elements is undefined.
    /// `O(N)`
    member x.Fold(seed : 's, folder : 's -> 'k -> 'v -> 's) =
        store |> IntMap.fold (fun s l ->
            l |> List.fold (fun s struct (k,v) -> folder s k v) s
        ) seed
        
    /// creates a new map containing all elements from this and other.
    /// the collide functions is used to resolve conflicts.
    /// `O(N + M)`
    member x.UnionWith(other : HashMap<'k, 'v>, collide : 'k -> 'v -> 'v -> 'v) =
        let mutable cnt = cnt + other.Count
        let f k l r =
            cnt <- cnt - 1
            collide k l r
            
        let newStore = 
            IntMap.appendWith (HashMapList.unionWith f) store other.Store
        
        HashMap(cnt, newStore)
        
    /// creates a new map by applying the mapping function to all entries.
    /// the respective option-arguments are some whenever the left/right map has an entry for the current key.
    /// note that one of the options will always be some.
    /// `O(N + M)`
    member x.Choose2(other : HashMap<'k, 'a>, mapping : 'k -> option<'v> -> option<'a> -> option<'c>) =
        let mutable cnt = 0
        let f k l r =
            match mapping k l r with
                | Some r -> 
                    cnt <- cnt + 1
                    Some r
                | None -> 
                    None

        let both (_hash : int) (l : list<struct ('k * 'v)>) (r : list<struct ('k * 'a)>) =
            match HashMapList.mergeWithOption f l r with
                | [] -> None
                | l -> Some l

        let onlyLeft (l : intmap<list<struct ('k * 'v)>>) =
            l |> IntMap.mapOption (fun l -> 
                match l |> List.choose (fun struct (lk, lv) -> match f lk (Some lv) None with | Some r -> Some (struct (lk,r)) | None -> None) with
                    | [] -> None
                    | l -> Some l
            )
            
        let onlyRight (r : intmap<list<struct ('k * 'a)>>) =
            r |> IntMap.mapOption (fun r -> 
                match r |> List.choose (fun struct (rk, rv) -> match f rk None (Some rv) with | Some r -> Some (struct (rk,r)) | None -> None) with
                    | [] -> None
                    | r -> Some r
            )

        let newStore =
            IntMap.mergeWithKey both onlyLeft onlyRight store other.Store

        HashMap(cnt, newStore)
    
    /// creates a new map by applying the mapping function to all entries.
    /// the respective option-arguments are some whenever the left/right map has an entry for the current key.
    /// note that one of the options will always be some.
    /// `O(N + M)`
    member x.Choose2SetMap(other : HashSet<'k>, mapping : 'k -> option<'v> -> bool -> option<'c>) =
        let mutable cnt = 0
        let f k l r =
            match mapping k l r with
                | Some r -> 
                    cnt <- cnt + 1
                    Some r
                | None -> 
                    None

        let both (_hash : int) (l : list<struct ('k * 'v)>) (r : list<'k>) =
            match HashMapList.mergeWithOptionSetMap f l r with
                | [] -> None
                | l -> Some l

        let onlyLeft (l : intmap<list<struct ('k * 'v)>>) =
            l |> IntMap.mapOption (fun l -> 
                match l |> List.choose (fun struct (lk, lv) -> match f lk (Some lv) false with | Some r -> Some (struct (lk,r)) | None -> None) with
                    | [] -> None
                    | l -> Some l
            )
            
        let onlyRight (r : intmap<list<'k>>) =
            r |> IntMap.mapOption (fun r -> 
                match r |> List.choose (fun rk -> match f rk None true with | Some r -> Some (struct (rk,r)) | None -> None) with
                    | [] -> None
                    | r -> Some r
            )

        let newStore =
            IntMap.mergeWithKey both onlyLeft onlyRight store other.Store

        HashMap(cnt, newStore)
    
    /// creates a new set by applying the mapping function to all entries.
    /// the respective option-arguments are some whenever the left/right map has an entry for the current key.
    /// note that one of the options will always be some.
    /// `O(N + M)`
    member x.Choose2SetSet(other : HashSet<'k>, mapping : 'k -> option<'v> -> bool -> bool) =
        let mutable cnt = 0
        let f k l r =
            match mapping k l r with
            | true -> 
                cnt <- cnt + 1
                true
            | false -> 
                false

        let both (_hash : int) (l : list<struct ('k * 'v)>) (r : list<'k>) =
            match HashMapList.mergeWithOptionSetSet f l r with
                | [] -> None
                | l -> Some l

        let onlyLeft (l : intmap<list<struct ('k * 'v)>>) =
            l |> IntMap.mapOption (fun l -> 
                match l |> List.choose (fun struct (lk, lv) -> match f lk (Some lv) false with | true -> Some lk | false -> None) with
                    | [] -> None
                    | l -> Some l
            )
            
        let onlyRight (r : intmap<list<'k>>) =
            r |> IntMap.mapOption (fun r -> 
                match r |> List.choose (fun rk -> match f rk None true with | true -> Some rk | false -> None) with
                    | [] -> None
                    | r -> Some r
            )

        let newStore =
            IntMap.mergeWithKey both onlyLeft onlyRight store other.Store

        HashSet(cnt, newStore)

    /// creates a new map by applying the mapping function to all entries.
    /// the respective option-arguments are some whenever the left/right map has an entry for the current key.
    /// note that one of the options will always be some.
    /// `O(N + M)`
    member x.Map2(other : HashMap<'k, 'a>, f : 'k -> option<'v> -> option<'a> -> 'c) =
        let mutable cnt = 0
        let f k l r =
            cnt <- cnt + 1
            f k l r

        let both (hash : int) (l : list<struct ('k * 'v)>) (r : list<struct ('k * 'a)>) =
            match HashMapList.mergeWith f l r with
                | [] -> None
                | l -> Some l

        let onlyLeft (l : intmap<list<struct ('k * 'v)>>) =
            l |> IntMap.mapOption (fun l -> 
                match l |> List.map (fun struct (lk, lv) -> struct (lk, f lk (Some lv) None)) with
                    | [] -> None
                    | l -> Some l
            )
            
        let onlyRight (r : intmap<list<struct ('k * 'a)>>) =
            r |> IntMap.mapOption (fun r -> 
                match r |> List.map (fun struct (rk, rv) -> struct (rk, f rk None (Some rv))) with
                    | [] -> None
                    | r -> Some r
            )

        let newStore =
            IntMap.mergeWithKey both onlyLeft onlyRight store other.Store

        HashMap(cnt, newStore)
    
    /// creates a new map containing all elements from this and other.
    /// colliding entries are taken from other.
    /// `O(N + M)`
    member x.Union(other : HashMap<'k, 'v>) =
        x.UnionWith(other, fun _ _ r -> r)

    /// tries to remove the entry for the given key from the map and returns its value and the rest of the map.
    /// `O(log N)`
    member x.TryRemove(key : 'k) =
        let hash = Unchecked.hash key
        let mutable removed = None
        let newStore =
            store |> IntMap.update (fun o ->
                match HashMapList.tryRemove key o with
                | Some(v,l) ->
                    removed <- Some v
                    match l with
                    | [] -> None
                    | l -> Some l
                | None -> 
                    Some o
            ) hash

        match removed with
            | Some rem -> Some(rem, HashMap(cnt - 1, newStore))
            | None -> None
       
    /// tries to find the value for the given key.
    /// `O(log N)`
    member x.TryFind(key : 'k) =
        let hash = Unchecked.hash key
        match IntMap.tryFind hash store with
            | Some l ->
                l |> List.tryPick (fun struct (k,v) ->
                    if Unchecked.equals k key then
                        Some v
                    else
                        None
                )
            | None ->
                None

    /// finds the value for the given key and raises KeyNotFoundException on failure.
    /// `O(log N)`
    member x.Find(key : 'k) =
        match x.TryFind key with
            | Some v -> v
            | None -> raise <| System.Collections.Generic.KeyNotFoundException()
            
    /// finds the value for the given key and raises KeyNotFoundException on failure.
    /// `O(log N)`
    member x.Item
        with get (key : 'k) = x.Find key

    /// creates a seq holding all tuples contained in the map.
    /// `O(N)`
    member x.ToSeq() =
        store |> IntMap.toSeq |> Seq.collect (fun (_,l) -> l |> Seq.map (fun struct(k,v) -> (k,v)))
        
    /// creates a list holding all tuples contained in the map.
    /// `O(N)`
    member x.ToList() =
        store |> IntMap.toList |> List.collect (fun (_,l) -> l |> List.map (fun struct(k,v) -> (k,v)))
        
    /// creates a list holding all tuples contained in the map.
    /// `O(N)`
    member x.ToArray() =
        let result = Array.zeroCreate x.Count
        let mutable i = 0
        for (_hash, kvps) in IntMap.toSeq store do
            for struct(k,v) in kvps do
                result.[i] <- (k, v)
                i <- i + 1
        result

    /// creates a map with a single entry.
    /// `O(1)`
    static member Single (k : 'k) (v : 'v) =
        let hash = Unchecked.hash k
        HashMap(1, IntMap.single hash [(k, v)])
        
    /// creates a map with all entries from the seq.
    /// `O(N * log N)`
    static member OfSeq (seq : seq<'k * 'v>) =
        let mutable res = empty
        for (k,v) in seq do
            res <- res.Add(k,v)
        res
        
    /// creates a map with all entries from the list.
    /// `O(N * log N)`
    static member OfList (list : list<'k * 'v>) =
        HashMap.OfSeq list
        
    /// creates a map with all entries from the array.
    /// `O(N * log N)`
    static member OfArray (list : array<'k * 'v>) =
        HashMap.OfSeq list

    override x.ToString() =
        let suffix =
            if x.Count > 5 then "; ..."
            else ""

        let content =
            x.ToSeq() |> Seq.truncate 5 |> Seq.map (sprintf "%A") |> String.concat "; "

        "HashMap [" + content + suffix + "]"

    override x.GetHashCode() =
        match store with
            | Nil -> 0
            | _ -> 
                (0, store) ||> Seq.fold (fun s (h,vs) ->
                    let listHash = 
                        (0, vs) ||> List.fold (fun s struct (_,v) -> 
                            /// need unordered hash combine here
                            s ^^^ (Unchecked.hash v)
                        )
                    HashMapList.combineHash h listHash
                )

    override x.Equals o =
        match o with
            | :? HashMap<'k, 'v> as o ->
                IntMap.equals HashMapList.equals store o.Store
            | _ ->
                false

    member private x.AsString = x.ToString()

    interface IEnumerable with
        member x.GetEnumerator() = 
            new HashMapEnumerator<_,_>(store) :> _

    interface IEnumerable<'k * 'v> with
        member x.GetEnumerator() = 
            new HashMapEnumerator<_,_>(store) :> _

and private HashMapEnumerator<'k, 'v>(m : intmap<list<struct ('k * 'v)>>) =
    
    let mutable stack = [m]
    let mutable inner = []
    let mutable current = Unchecked.defaultof<_>

    let rec moveNext() =
        match inner with
            | [] ->
                match stack with
                    | [] -> false
                    | h :: s ->
                        stack <- s
                        match h with
                            | Tip(k,v) -> 
                                match v with
                                    | [] -> failwith "asdasdsadasd"
                                    | v :: rest ->
                                        current <- v
                                        inner <- rest
                                        true

                            | Nil ->
                                moveNext()

                            | Bin(_,_,l,r) ->
                                stack <- l :: r :: stack
                                moveNext()

            | h :: rest ->
                current <- h
                inner <- rest
                true

    member x.MoveNext() =
        moveNext()

    member x.Current = 
        let struct (k,v) = current
        (k,v)

    member x.Reset() =
        stack <- [m]
        inner <- []
        current <- Unchecked.defaultof<_>

    member x.Dispose() =
        stack <- []
        inner <- []
        current <- Unchecked.defaultof<_>


    interface IEnumerator with
        member x.MoveNext() = x.MoveNext()
        member x.Current = x.Current :> obj
        member x.Reset() = x.Reset()

    interface IEnumerator<'k * 'v> with
        member x.Current = x.Current
        member x.Dispose() = x.Dispose()



/// Functional programming operators related to the HashMap<_,_> type.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HashMap =

    /// The empty map.
    [<GeneralizableValue>]
    let empty<'k, 'v> = HashMap<'k, 'v>.Empty
    
    /// creates a map with a single entry.
    /// `O(1)`
    let inline single (k : 'k) (v : 'v) = 
        HashMap.Single k v

    /// creates a map with all entries from the seq.
    /// `O(N * log N)`
    let inline ofSeq (seq : seq<'k * 'v>) = 
        HashMap.OfSeq seq

    /// creates a map with all entries from the map.
    /// `O(N * log N)`
    let inline ofMap (map : Map<'k, 'v>) = 
        map |> Map.toSeq |> ofSeq

    /// creates a map with all entries from the list.
    /// `O(N * log N)`
    let inline ofList (list : list<'k * 'v>) = 
        HashMap.OfList list
        
    /// creates a map with all entries from the array.
    /// `O(N * log N)`
    let inline ofArray (arr : array<'k * 'v>) = 
        HashMap.OfArray arr

    
    /// creates a seq holding all tuples contained in the map.
    /// `O(N)`
    let inline toSeq (map : HashMap<'k, 'v>) = 
        map.ToSeq()

    /// creates a list holding all tuples contained in the map.
    /// `O(N)`
    let inline toList (map : HashMap<'k, 'v>) = 
        map.ToList()

    /// creates a list holding all tuples contained in the map.
    /// `O(N)`
    let inline toArray (map : HashMap<'k, 'v>) = 
        map.ToArray()

    /// creates a Map holding all entries contained in the HashMap.
    /// `O(N)`
    let inline toMap (map : HashMap<'k, 'v>) = 
        let mutable res = Map.empty
        for (k,v) in map do
            res <- Map.add k v res
        res


    /// adds or updates the entry for the given key. `O(log N)`
    let inline add (key : 'k) (value : 'v) (map : HashMap<'k, 'v>) =
        map.Add(key, value)

    /// removes the entry for the given key. `O(log N)`
    let inline remove (key : 'k) (map : HashMap<'k, 'v>) =
        map.Remove(key)

    /// adds, deletes or updates the entry for the given key.
    /// the update functions gets the optional old value and may optionally return
    /// a new value (or None for deleting the entry).
    /// `O(log N)`
    let inline alter (key : 'k) (mapping : option<'v> -> option<'v>) (map : HashMap<'k, 'v>) =
        map.Alter(key, mapping)
        
    /// adds or updates the entry for the given key based on the optional current value. 
    /// `O(log N)`
    let inline update (key : 'k) (mapping : option<'v> -> 'v) (map : HashMap<'k, 'v>) =
        map.Update(key, mapping)

    /// creates a new map containing all elements from l and r.
    /// the resolve functions is used to resolve conflicts.
    /// `O(N + M)`
    let inline unionWith (resolve : 'k -> 'v -> 'v -> 'v) (l : HashMap<'k, 'v>) (r : HashMap<'k, 'v>) =
        l.UnionWith(r, resolve)
        
    /// creates a new map containing all elements from l and r.
    /// colliding entries are taken from r.
    /// `O(N + M)`
    let inline union (l : HashMap<'k, 'v>) (r : HashMap<'k, 'v>) =
        l.Union r

    /// tries to remove the entry for the given key from the map and returns its value and the rest of the map.
    /// `O(log N)`
    let inline tryRemove (key : 'k) (map : HashMap<'k, 'v>) =
        map.TryRemove key


    /// creates a new map (with the same keys) by applying the given function to all entries.
    /// `O(N)`
    let inline map (mapping : 'k -> 'a -> 'b) (map : HashMap<'k, 'a>) =
        map.Map(mapping)
        
    /// creates a new map (with the same keys) by applying the given function to all entries.
    /// `O(N)`
    let inline choose (mapping : 'k -> 'a -> option<'b>) (map : HashMap<'k, 'a>) =
        map.Choose mapping

    /// creates a new map (with the same keys) that contains all entries for which predicate was true.
    /// `O(N)`
    let inline filter (predicate : 'k -> 'v -> bool) (map : HashMap<'k, 'v>) =
        map.Filter predicate

    /// applies the iter function to all entries of the map.
    /// `O(N)`
    let inline iter (iter : 'k -> 'v -> unit) (map : HashMap<'k, 'v>) =
        map.Iter iter

    /// folds over all entries of the map.
    /// note that the order for elements is undefined.
    /// `O(N)`
    let inline fold (folder : 's -> 'k -> 'v -> 's) (seed : 's) (map : HashMap<'k, 'v>) =
        map.Fold(seed, folder)
        
    /// tests whether an entry making the predicate true exists.
    /// `O(N)`
    let inline exists (predicate : 'k -> 'v -> bool) (map : HashMap<'k, 'v>) =
        map.Exists(predicate)

    /// tests whether all entries fulfil the given predicate.
    /// `O(N)`
    let inline forall (predicate : 'k -> 'v -> bool) (map : HashMap<'k, 'v>) =
        map.Forall(predicate)

    /// creates a new map by applying the mapping function to all entries.
    /// the respective option-arguments are some whenever the left/right map has an entry for the current key.
    /// note that one of the options will always be some.
    /// `O(N + M)`
    let inline map2 (mapping : 'k -> option<'a> -> option<'b> -> 'c) (l : HashMap<'k, 'a>) (r : HashMap<'k, 'b>) =
        l.Map2(r, mapping)

    /// creates a new map by applying the mapping function to all entries.
    /// the respective option-arguments are some whenever the left/right map has an entry for the current key.
    /// note that one of the options will always be some.
    /// `O(N + M)`
    let inline choose2 (mapping : 'k -> option<'a> -> option<'b> -> option<'c>) (l : HashMap<'k, 'a>) (r : HashMap<'k, 'b>) =
        l.Choose2(r, mapping)

    /// tries to find the value for the given key.
    /// `O(log N)`
    let inline tryFind (key : 'k) (map : HashMap<'k, 'v>) =
        map.TryFind key
        
    /// finds the value for the given key and raises KeyNotFoundException on failure.
    /// `O(log N)`
    let inline find (key : 'k) (map : HashMap<'k, 'v>) =
        map.Find key
        
    /// tests if an entry for the given key exists. `O(log N)`
    let inline containsKey (key : 'k) (map : HashMap<'k, 'v>) =
        map.ContainsKey key

    /// creates a HashSet holding all keys from the map.
    /// `O(N)`
    let inline keys (map : HashMap<'k, 'v>) = map.GetKeys()

    /// the number of elements in the map `O(1)`
    let inline count (map : HashMap<'k, 'v>) = map.Count
    
    /// is the map empty? `O(1)`
    let inline isEmpty (map : HashMap<'k, 'v>) = map.IsEmpty

