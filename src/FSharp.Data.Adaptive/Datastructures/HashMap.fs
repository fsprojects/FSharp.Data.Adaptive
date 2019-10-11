namespace FSharp.Data.Adaptive


open System.Collections
open System.Collections.Generic
open FSharp.Data.Adaptive

/// Helper functions for hash-collision lists.
/// Most members have bad runtime, but the lists should be quite small when using appropriate hashCodes.
module internal HashMapList =

    let inline combineHash (a: int) (b: int) =
        uint32 a ^^^ uint32 b + 0x9e3779b9u + ((uint32 a) <<< 6) + ((uint32 a) >>> 2) |> int

    let rec alter (k: 'K) (f: option<'V> -> option<'V>) (l: list<struct ('K * 'V)>) =
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

    let rec alter' (cnt: ref<int>) (k: 'K) (f: option<'V> -> option<'V>) (l: list<struct ('K * 'V)>) =
        match l with
        | [] ->
            match f None with
                | None -> []
                | Some v -> 
                    cnt := !cnt + 1
                    [struct (k,v)]

        | struct(k1, v1) :: rest ->
            if Unchecked.equals k k1 then
                match f (Some v1) with
                | None -> 
                    cnt := !cnt - 1
                    rest
                | Some v2 -> 
                    struct (k1, v2) :: rest
            else
                struct (k1, v1) :: alter' cnt k f rest

    let rec update (k: 'K) (f: option<'V> -> 'V) (l: list<struct ('K * 'V)>) =
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

    let rec add (cnt: ref<int>) (k: 'K) (v: 'V) (l: list<struct ('K * 'V)>) =
        match l with
        | [] ->     
            cnt := !cnt + 1
            [struct (k,v)]
        | struct(k1, v1) :: rest ->
            if Unchecked.equals k k1 then
                struct(k1, v) :: rest
            else
                struct(k1, v1) :: add cnt k v rest

    let rec remove (cnt: ref<int>) (k: 'K) (l: list<struct('K * 'V)>) =
        match l with
        | [] -> []
        | struct(k1, v1) :: rest ->
            if Unchecked.equals k k1 then
                cnt := !cnt - 1
                rest
            else
                struct(k1, v1) :: remove cnt k rest

    let rec tryRemove (k: 'K) (l: list<struct('K * 'V)>) =
        match l with
        | [] -> None
        | struct(k1,v1) :: rest ->
            if Unchecked.equals k k1 then
                Some (v1, rest)
            else
                match tryRemove k rest with
                | None -> None
                | Some(v,rest) -> Some(v, struct(k1,v1)::rest)

    let rec unionWith (f: 'K -> 'V -> 'V -> 'V) (l: list<struct('K * 'V)>) (r: list<struct('K * 'V)>) =
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

    let rec mergeWith (f: 'K -> option<'V> -> option<'V2> -> 'V3) (l: list<struct ('K * 'V)>) (r: list<struct ('K * 'V2)>) =
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

    let rec mergeWithOption (f: 'K -> option<'V> -> option<'V2> -> option<'V3>) (l: list<struct('K * 'V)>) (r: list<struct('K * 'V2)>) =
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

    let rec mergeWithOptionSetMap (f: 'K -> option<'V> -> bool -> option<'V3>) (l: list<struct('K * 'V)>) (r: list<'K>) =
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
        
    let rec mergeWithOptionSetSet (f: 'K -> option<'V> -> bool -> bool) (l: list<struct('K * 'V)>) (r: list<'K>) =
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

    let rec mergeWithOption' (f: 'K -> option<'V> -> option<'V2> -> option<'V3>) (l: list<struct('K * 'V)>) (r: list<struct('K * 'V2)>) =
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

    let rec equals (l: list<struct('K * 'V)>) (r: list<struct('K * 'V)>) =
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
/// Hash/equality are determined using the Unchecked module
[<Struct; CustomEquality; NoComparison>]
[<StructuredFormatDisplay("{AsString}")>]
type HashMap<'K, [<EqualityConditionalOn>] 'V> internal(cnt: int, store: intmap<list<struct ('K * 'V)>>) =
    static let empty = HashMap<'K, 'V>(0, IntMap.empty)

    /// Internal for getting the IntMap store
    member internal x.Store = store

    /// The empty HashMap.
    static member Empty = empty
    
    /// Is the map empty? `O(1)`
    member x.IsEmpty = cnt = 0

    /// The number of elements in the map `O(1)`
    member x.Count = cnt

    /// Adds, deletes or updates the entry for the given key.
    /// The update functions gets the optional old value and may optionally return
    /// A new value (or None for deleting the entry).
    /// `O(log N)`
    member x.Alter (key: 'K, update: option<'V> -> option<'V>) =
        let hash = Unchecked.hash key

        let mutable changed = false
        let mutable deltaCnt = 0
        let update (old: option<list<struct('K * 'V)>>) =
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

    /// Adds or updates the entry for the given key based on the optional current value. 
    /// `O(log N)`
    member x.Update (key: 'K, f: option<'V> -> 'V) =
        x.Alter(key, f >> Some)
            
    /// Adds or updates the entry for the given key. `O(log N)`
    member x.Add (key: 'K, value: 'V) =
        let hash = Unchecked.hash key
        let cnt = ref cnt
        let newMap = 
            store |> IntMap.alter (function 
                | None -> 
                    cnt := !cnt + 1
                    Some [key,value]
                | Some l -> 
                    Some (HashMapList.add cnt key value l)
            ) hash
        HashMap(!cnt, newMap)

    /// Removes the entry for the given key. `O(log N)`
    member x.Remove (key: 'K) =
        let cnt = ref cnt
        let hash = Unchecked.hash key
        let newMap = 
            store |> IntMap.update (fun l ->
                match HashMapList.remove cnt key l with
                | [] -> None
                | l -> Some l
            ) hash
        HashMap(!cnt, newMap)

    /// Tests if an entry for the given key exists. `O(log N)`
    member x.ContainsKey (key: 'K) =
        let hash = Unchecked.hash key
        match IntMap.tryFind hash store with
        | Some l -> l |> List.exists (fun struct (k,_) -> Unchecked.equals k key)
        | None -> false
        
    /// Creates a HashSet holding all keys from the map.
    /// `O(N)`
    member x.GetKeys() =
        let setStore =
            store |> IntMap.map (
                List.map (fun struct(k,_v) -> k)
            )
        HashSet(cnt, setStore)


    /// Creates a new map (with the same keys) by applying the given function to all entries.
    /// `O(N)`
    member x.Map(mapping: 'K -> 'V -> 'V2) =
        let newStore = 
            store 
                |> IntMap.map (fun l -> l |> List.map (fun struct (k,v) -> struct (k, mapping k v)))
        HashMap(cnt, newStore)

    /// Creates two maps (with the same keys) by applying the given function to all entries.
    /// `O(N)`
    member x.ChooseTup(mapping: 'K -> 'V -> option<'V2 * 'V3>) =
        let mutable cnt = 0
        let mapping (struct (k: 'K, v: 'V)) =
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
        
    /// Creates a new map (with the same keys) by applying the given function to all entries.
    /// `O(N)`
    member x.Choose(mapping: 'K -> 'V -> option<'V2>) =
        let mutable cnt = 0
        let mapping (struct (k: 'K, v: 'V)) =
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

    /// Creates a new map (with the same keys) that contains all entries for which predicate was true.
    /// `O(N)`
    member x.Filter(predicate: 'K -> 'V -> bool) =
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

    /// Applies the iter function to all entries of the map.
    /// `O(N)`
    member x.Iter(iter: 'K -> 'V -> unit) =
        store |> IntMap.toSeq |> Seq.iter (fun (_,l) ->
            l |> List.iter (fun struct(k,v) -> iter k v)
        )
        
    /// Tests whether an entry making the predicate true exists.
    /// `O(N)`
    member x.Exists(predicate: 'K -> 'V -> bool) =
        store |> IntMap.toSeq |> Seq.exists (fun (_,v) ->
            v |> List.exists (fun struct(k,v) -> predicate k v)
        )

    /// Tests whether all entries fulfil the given predicate.
    /// `O(N)`
    member x.Forall(predicate: 'K -> 'V -> bool) =
        store |> IntMap.toSeq |> Seq.forall (fun (_,v) ->
            v |> List.forall (fun struct (k,v) -> predicate k v)
        )

    /// Folds over all entries of the map.
    /// Note that the order for elements is undefined.
    /// `O(N)`
    member x.Fold(seed: 'State, folder: 'State -> 'K -> 'V -> 'State) =
        store |> IntMap.fold (fun s l ->
            l |> List.fold (fun s struct (k,v) -> folder s k v) s
        ) seed
        
    /// Creates a new map containing all elements from this and other.
    /// The collide functions is used to resolve conflicts.
    /// `O(N + M)`
    member x.UnionWith(other: HashMap<'K, 'V>, collide: 'K -> 'V -> 'V -> 'V) =
        let mutable cnt = cnt + other.Count
        let f k l r =
            cnt <- cnt - 1
            collide k l r
            
        let newStore = 
            IntMap.appendWith (HashMapList.unionWith f) store other.Store
        
        HashMap(cnt, newStore)
        
    /// Like choose2 but with existing right values.
    member x.UpdateTo(other : HashMap<'K, 'V2>, mapping : 'K -> option<'V> -> 'V2 -> 'V3) =
        if other.Count * 5 < cnt then
            let tryFind = x.TryFind
            other.Map (fun k v2 ->
                let v1 = tryFind k
                mapping k v1 v2
            )
        else
            x.Choose2(other, fun k v1 v2 ->
                match v2 with
                | Some v2 -> mapping k v1 v2 |> Some
                | None -> None
            )

    /// Creates a new map by applying the mapping function to all entries.
    /// The respective option-arguments are some whenever the left/right map has an entry for the current key.
    /// Note that one of the options will always be some.
    /// `O(N + M)`
    member x.Choose2(other: HashMap<'K, 'V2>, mapping: 'K -> option<'V> -> option<'V2> -> option<'V3>) =
        let mutable cnt = 0
        let f k l r =
            match mapping k l r with
            | Some r -> 
                cnt <- cnt + 1
                Some r
            | None -> 
                None

        let both (_hash: int) (l: list<struct ('K * 'V)>) (r: list<struct ('K * 'V2)>) =
            match HashMapList.mergeWithOption f l r with
            | [] -> None
            | l -> Some l

        let onlyLeft (l: intmap<list<struct ('K * 'V)>>) =
            l |> IntMap.mapOption (fun l -> 
                match l |> List.choose (fun struct (lk, lv) -> match f lk (Some lv) None with | Some r -> Some (struct (lk,r)) | None -> None) with
                | [] -> None
                | l -> Some l
            )
            
        let onlyRight (r: intmap<list<struct ('K * 'V2)>>) =
            r |> IntMap.mapOption (fun r -> 
                match r |> List.choose (fun struct (rk, rv) -> match f rk None (Some rv) with | Some r -> Some (struct (rk,r)) | None -> None) with
                | [] -> None
                | r -> Some r
            )

        let newStore =
            IntMap.mergeWithKey both onlyLeft onlyRight store other.Store

        HashMap(cnt, newStore)
    
    /// Creates a new map by applying the mapping function to all entries.
    /// The respective option-arguments are some whenever the left/right map has an entry for the current key.
    /// Note that one of the options will always be some.
    /// `O(N + M)`
    member x.Choose2SetMap(other: HashSet<'K>, mapping: 'K -> option<'V> -> bool -> option<'V3>) =
        let mutable cnt = 0
        let f k l r =
            match mapping k l r with
            | Some r -> 
                cnt <- cnt + 1
                Some r
            | None -> 
                None

        let both (_hash: int) (l: list<struct ('K * 'V)>) (r: list<'K>) =
            match HashMapList.mergeWithOptionSetMap f l r with
            | [] -> None
            | l -> Some l

        let onlyLeft (l: intmap<list<struct ('K * 'V)>>) =
            l |> IntMap.mapOption (fun l -> 
                match l |> List.choose (fun struct (lk, lv) -> match f lk (Some lv) false with | Some r -> Some (struct (lk,r)) | None -> None) with
                | [] -> None
                | l -> Some l
            )
            
        let onlyRight (r: intmap<list<'K>>) =
            r |> IntMap.mapOption (fun r -> 
                match r |> List.choose (fun rk -> match f rk None true with | Some r -> Some (struct (rk,r)) | None -> None) with
                | [] -> None
                | r -> Some r
            )

        let newStore =
            IntMap.mergeWithKey both onlyLeft onlyRight store other.Store

        HashMap(cnt, newStore)
    
    /// Creates a new set by applying the mapping function to all entries.
    /// The respective option-arguments are some whenever the left/right map has an entry for the current key.
    /// Note that one of the options will always be some.
    /// `O(N + M)`
    member x.Choose2SetSet(other: HashSet<'K>, mapping: 'K -> option<'V> -> bool -> bool) =
        let mutable cnt = 0
        let f k l r =
            match mapping k l r with
            | true -> 
                cnt <- cnt + 1
                true
            | false -> 
                false

        let both (_hash: int) (l: list<struct ('K * 'V)>) (r: list<'K>) =
            match HashMapList.mergeWithOptionSetSet f l r with
            | [] -> None
            | l -> Some l

        let onlyLeft (l: intmap<list<struct ('K * 'V)>>) =
            l |> IntMap.mapOption (fun l -> 
                match l |> List.choose (fun struct (lk, lv) -> match f lk (Some lv) false with | true -> Some lk | false -> None) with
                | [] -> None
                | l -> Some l
            )
            
        let onlyRight (r: intmap<list<'K>>) =
            r |> IntMap.mapOption (fun r -> 
                match r |> List.choose (fun rk -> match f rk None true with | true -> Some rk | false -> None) with
                | [] -> None
                | r -> Some r
            )

        let newStore =
            IntMap.mergeWithKey both onlyLeft onlyRight store other.Store

        HashSet(cnt, newStore)

    /// Creates a new map by applying the mapping function to all entries.
    /// The respective option-arguments are some whenever the left/right map has an entry for the current key.
    /// Note that one of the options will always be some.
    /// `O(N + M)`
    member x.Map2(other: HashMap<'K, 'V2>, f: 'K -> option<'V> -> option<'V2> -> 'V3) =
        let mutable cnt = 0
        let f k l r =
            cnt <- cnt + 1
            f k l r

        let both (hash: int) (l: list<struct ('K * 'V)>) (r: list<struct ('K * 'V2)>) =
            match HashMapList.mergeWith f l r with
            | [] -> None
            | l -> Some l

        let onlyLeft (l: intmap<list<struct ('K * 'V)>>) =
            l |> IntMap.mapOption (fun l -> 
                match l |> List.map (fun struct (lk, lv) -> struct (lk, f lk (Some lv) None)) with
                | [] -> None
                | l -> Some l
            )
            
        let onlyRight (r: intmap<list<struct ('K * 'V2)>>) =
            r |> IntMap.mapOption (fun r -> 
                match r |> List.map (fun struct (rk, rv) -> struct (rk, f rk None (Some rv))) with
                | [] -> None
                | r -> Some r
            )

        let newStore =
            IntMap.mergeWithKey both onlyLeft onlyRight store other.Store

        HashMap(cnt, newStore)
    
    /// Creates a new map containing all elements from this and other.
    /// Colliding entries are taken from other.
    /// `O(N + M)`
    member x.Union(other: HashMap<'K, 'V>) =
        x.UnionWith(other, fun _ _ r -> r)

    /// Tries to remove the entry for the given key from the map and returns its value and the rest of the map.
    /// `O(log N)`
    member x.TryRemove(key: 'K) =
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
       
    /// Tries to find the value for the given key.
    /// `O(log N)`
    member x.TryFind(key: 'K) =
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

    /// Finds the value for the given key and raises KeyNotFoundException on failure.
    /// `O(log N)`
    member x.Find(key: 'K) =
        match x.TryFind key with
        | Some v -> v
        | None -> raise <| System.Collections.Generic.KeyNotFoundException()
            
    /// Finds the value for the given key and raises KeyNotFoundException on failure.
    /// `O(log N)`
    member x.Item
        with get (key: 'K) = x.Find key

    /// Creates a seq holding all tuples contained in the map.
    /// `O(N)`
    member x.ToSeq() =
        store |> IntMap.toSeq |> Seq.collect (fun (_,l) -> l |> Seq.map (fun struct(k,v) -> (k,v)))
        
    /// Creates a list holding all tuples contained in the map.
    /// `O(N)`
    member x.ToList() =
        store |> IntMap.toList |> List.collect (fun (_,l) -> l |> List.map (fun struct(k,v) -> (k,v)))
        
    /// Creates a list holding all tuples contained in the map.
    /// `O(N)`
    member x.ToArray() =
        let result = Array.zeroCreate x.Count
        let mutable i = 0
        for (_hash, kvps) in IntMap.toSeq store do
            for struct(k,v) in kvps do
                result.[i] <- (k, v)
                i <- i + 1
        result

    /// Creates a map with a single entry.
    /// `O(1)`
    static member Single (k: 'K) (v: 'V) =
        let hash = Unchecked.hash k
        HashMap(1, IntMap.single hash [(k, v)])
        
    /// Creates a map with all entries from the seq.
    /// `O(N * log N)`
    static member OfSeq (seq: seq<'K * 'V>) =
        #if !ADAPTIVE_NO_TYPE_TESTS
        match seq with
        | :? HashMap<'K, 'V> as o ->
            o
        | _ -> 
        #endif
            let mutable res = empty
            for (k,v) in seq do
                res <- res.Add(k,v)
            res
        
    /// Creates a map with all entries from the list.
    /// `O(N * log N)`
    static member OfList (list: list<'K * 'V>) =
        HashMap.OfSeq list
        
    /// Creates a map with all entries from the array.
    /// `O(N * log N)`
    static member OfArray (list: array<'K * 'V>) =
        HashMap.OfSeq list

    override x.ToString() =
        let suffix =
            if x.Count > 5 then "; ..."
            else ""

        let content =
            x.ToSeq() |> Seq.truncate 5 |> Seq.map (sprintf "%A") |> String.concat "; "

        "HashMap [" + content + suffix + "]"

    
    /// Conservatively determines whether the two HashMaps are equal.
    /// `O(1)`
    member x.ConservativeEquals(other : HashMap<'K, 'V>) =
        System.Object.ReferenceEquals(store, other.Store)


    override x.GetHashCode() =
        match store with
        | Nil -> 0
        | _ -> 
            (0, store) ||> Seq.fold (fun s (h,vs) ->
                let listHash = 
                    (0, vs) ||> List.fold (fun s struct (_,v) -> 
                        /// Need unordered hash combine here
                        s ^^^ (Unchecked.hash v)
                    )
                HashMapList.combineHash h listHash
            )

    override x.Equals o =
        #if ADAPTIVE_NO_TYPE_TESTS
        let o = unbox<HashMap<'K, 'V>> o
        IntMap.equals HashMapList.equals store o.Store
        #else
        match o with
        | :? HashMap<'K, 'V> as o ->
            IntMap.equals HashMapList.equals store o.Store
        | _ ->
            false
        #endif
    member private x.AsString = x.ToString()

    interface IEnumerable with
        member x.GetEnumerator() = 
            new HashMapEnumerator<_,_>(store) :> _

    interface IEnumerable<'K * 'V> with
        member x.GetEnumerator() = 
            new HashMapEnumerator<_,_>(store) :> _

and private HashMapEnumerator<'K, 'V>(m: intmap<list<struct ('K * 'V)>>) =
    
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

    interface IEnumerator<'K * 'V> with
        member x.Current = x.Current
        member x.Dispose() = x.Dispose()



/// Functional programming operators related to the HashMap<_,_> type.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HashMap =

    /// The empty map.
    [<GeneralizableValue>]
    let empty<'K, 'V> = HashMap<'K, 'V>.Empty
    
    /// Creates a map with a single entry.
    /// `O(1)`
    let inline single (k: 'K) (v: 'V) = 
        HashMap.Single k v

    /// Creates a map with all entries from the seq.
    /// `O(N * log N)`
    let inline ofSeq (seq: seq<'K * 'V>) = 
        HashMap.OfSeq seq

    /// Creates a map with all entries from the map.
    /// `O(N * log N)`
    let inline ofMap (map: Map<'K, 'V>) = 
        map |> Map.toSeq |> ofSeq

    /// Creates a map with all entries from the list.
    /// `O(N * log N)`
    let inline ofList (list: list<'K * 'V>) = 
        HashMap.OfList list
        
    /// Creates a map with all entries from the array.
    /// `O(N * log N)`
    let inline ofArray (arr: array<'K * 'V>) = 
        HashMap.OfArray arr

    
    /// Creates a seq holding all tuples contained in the map.
    /// `O(N)`
    let inline toSeq (map: HashMap<'K, 'V>) = 
        map.ToSeq()

    /// Creates a list holding all tuples contained in the map.
    /// `O(N)`
    let inline toList (map: HashMap<'K, 'V>) = 
        map.ToList()

    /// Creates a list holding all tuples contained in the map.
    /// `O(N)`
    let inline toArray (map: HashMap<'K, 'V>) = 
        map.ToArray()

    /// Creates a Map holding all entries contained in the HashMap.
    /// `O(N)`
    let inline toMap (map: HashMap<'K, 'V>) = 
        let mutable res = Map.empty
        for (k,v) in map do
            res <- Map.add k v res
        res


    /// Adds or updates the entry for the given key. `O(log N)`
    let inline add (key: 'K) (value: 'V) (map: HashMap<'K, 'V>) =
        map.Add(key, value)

    /// Removes the entry for the given key. `O(log N)`
    let inline remove (key: 'K) (map: HashMap<'K, 'V>) =
        map.Remove(key)

    /// Adds, deletes or updates the entry for the given key.
    /// The update functions gets the optional old value and may optionally return
    /// A new value (or None for deleting the entry).
    /// `O(log N)`
    let inline alter (key: 'K) (mapping: option<'V> -> option<'V>) (map: HashMap<'K, 'V>) =
        map.Alter(key, mapping)
        
    /// Adds or updates the entry for the given key based on the optional current value. 
    /// `O(log N)`
    let inline update (key: 'K) (mapping: option<'V> -> 'V) (map: HashMap<'K, 'V>) =
        map.Update(key, mapping)

    /// Creates a new map containing all elements from l and r.
    /// The resolve functions is used to resolve conflicts.
    /// `O(N + M)`
    let inline unionWith (resolve: 'K -> 'V -> 'V -> 'V) (l: HashMap<'K, 'V>) (r: HashMap<'K, 'V>) =
        l.UnionWith(r, resolve)
        
    /// Creates a new map containing all elements from l and r.
    /// Colliding entries are taken from r.
    /// `O(N + M)`
    let inline union (l: HashMap<'K, 'V>) (r: HashMap<'K, 'V>) =
        l.Union r

    /// Tries to remove the entry for the given key from the map and returns its value and the rest of the map.
    /// `O(log N)`
    let inline tryRemove (key: 'K) (map: HashMap<'K, 'V>) =
        map.TryRemove key


    /// Creates a new map (with the same keys) by applying the given function to all entries.
    /// `O(N)`
    let inline map (mapping: 'K -> 'V -> 'V2) (map: HashMap<'K, 'V>) =
        map.Map(mapping)
        
    /// Creates a new map (with the same keys) by applying the given function to all entries.
    /// `O(N)`
    let inline choose (mapping: 'K -> 'V -> option<'V2>) (map: HashMap<'K, 'V>) =
        map.Choose mapping

    /// Creates a new map (with the same keys) that contains all entries for which predicate was true.
    /// `O(N)`
    let inline filter (predicate: 'K -> 'V -> bool) (map: HashMap<'K, 'V>) =
        map.Filter predicate

    /// Applies the iter function to all entries of the map.
    /// `O(N)`
    let inline iter (iter: 'K -> 'V -> unit) (map: HashMap<'K, 'V>) =
        map.Iter iter

    /// Folds over all entries of the map.
    /// Note that the order for elements is undefined.
    /// `O(N)`
    let inline fold (folder: 'State -> 'K -> 'V -> 'State) (seed: 'State) (map: HashMap<'K, 'V>) =
        map.Fold(seed, folder)
        
    /// Tests whether an entry making the predicate true exists.
    /// `O(N)`
    let inline exists (predicate: 'K -> 'V -> bool) (map: HashMap<'K, 'V>) =
        map.Exists(predicate)

    /// Tests whether all entries fulfil the given predicate.
    /// `O(N)`
    let inline forall (predicate: 'K -> 'V -> bool) (map: HashMap<'K, 'V>) =
        map.Forall(predicate)

    /// Creates a new map by applying the mapping function to all entries.
    /// The respective option-arguments are some whenever the left/right map has an entry for the current key.
    /// Note that one of the options will always be some.
    /// `O(N + M)`
    let inline map2 (mapping: 'K -> option<'V> -> option<'V2> -> 'V3) (l: HashMap<'K, 'V>) (r: HashMap<'K, 'V2>) =
        l.Map2(r, mapping)

    /// Creates a new map by applying the mapping function to all entries.
    /// The respective option-arguments are some whenever the left/right map has an entry for the current key.
    /// Note that one of the options will always be some.
    /// `O(N + M)`
    let inline choose2 (mapping: 'K -> option<'V1> -> option<'V2> -> option<'V3>) (l: HashMap<'K, 'V1>) (r: HashMap<'K, 'V2>) =
        l.Choose2(r, mapping)

    /// Tries to find the value for the given key.
    /// `O(log N)`
    let inline tryFind (key: 'K) (map: HashMap<'K, 'V>) =
        map.TryFind key
        
    /// Finds the value for the given key and raises KeyNotFoundException on failure.
    /// `O(log N)`
    let inline find (key: 'K) (map: HashMap<'K, 'V>) =
        map.Find key
        
    /// Tests if an entry for the given key exists. `O(log N)`
    let inline containsKey (key: 'K) (map: HashMap<'K, 'V>) =
        map.ContainsKey key

    /// Creates a HashSet holding all keys from the map.
    /// `O(N)`
    let inline keys (map: HashMap<'K, 'V>) = map.GetKeys()

    /// The number of elements in the map `O(1)`
    let inline count (map: HashMap<'K, 'V>) = map.Count
    
    /// Is the map empty? `O(1)`
    let inline isEmpty (map: HashMap<'K, 'V>) = map.IsEmpty

