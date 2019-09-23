namespace FSharp.Control.Incremental

open System.Collections
open System.Collections.Generic

/// helper functions for hash-collision lists.
/// most members have bad runtime, but the lists should be quite small when using appropriate hashCodes.
module internal HashSetList =

    let inline combineHash (a : int) (b : int) =
        uint32 a ^^^ uint32 b + 0x9e3779b9u + ((uint32 a) <<< 6) + ((uint32 a) >>> 2) |> int

    let rec add (cnt : byref<int>) (value : 'a) (list : list<'a>) =
        match list with
            | [] -> 
                cnt <- cnt + 1
                [value]
            | h :: tail ->
                if Unchecked.equals h value then
                    list
                else
                    h :: add &cnt value tail

    let rec remove (cnt : byref<int>) (value : 'a) (list : list<'a>) =
        match list with
            | [] ->
                None
            | h :: tail ->
                if Unchecked.equals h value then
                    cnt <- cnt - 1
                    match tail with
                        | [] -> None
                        | _ -> Some tail
                else
                    match remove &cnt value tail with
                        | Some t -> Some (h :: t)
                        | None -> Some [h]

    let rec union (dupl : byref<int>) (l : list<'a>) (r : list<'a>) =
        let mutable d = dupl
        let newR = 
            r |> List.filter (fun r ->
                if l |> List.exists (Unchecked.equals r) then
                    d <- d + 1
                    false
                else
                    true
            )

        dupl <- d
        l @ newR

    let rec difference (cnt : byref<int>) (l : list<'a>) (r : list<'a>) =
        match l with
            | [] -> 
                None
            | h :: tail ->
                if List.exists (Unchecked.equals h) r then
                    difference &cnt tail r
                else
                    cnt <- cnt + 1
                    match difference &cnt tail r with
                        | Some t -> Some (h :: t)
                        | None -> Some [h]
                    

    let rec intersect (cnt : byref<int>) (l : list<'a>) (r : list<'a>) =
        match l with
            | [] ->
                None
            | h :: tail ->
                if List.exists (Unchecked.equals h) r then
                    cnt <- cnt + 1
                    match intersect &cnt tail r with
                        | Some t -> Some (h :: t)
                        | None -> Some [h]
                else
                    intersect &cnt tail r


    let rec mergeWithOption (f : 'a -> bool -> bool -> option<'c>) (l : list<'a>) (r : list<'a>) =
        let newL = 
            l |> List.choose (fun lk ->
                let other = r |> List.exists (fun rk -> Unchecked.equals rk lk)
 
                match f lk true other with
                    | Some r -> Some (struct (lk, r))
                    | None -> None
            )
        let newR =
            r |> List.choose (fun rk ->
                if l |> List.forall (fun lk -> not (Unchecked.equals lk rk)) then
                    match f rk false true with
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
          
    let rec equals (l : list<'a>) (r : list<'a>) =
        let mutable r = r
        let mutable c = 0
        
        use e = (l :> seq<_>).GetEnumerator()
        while c = 0 && e.MoveNext() do
            let l = e.Current
            c <- 1
            r <- remove &c l r |> Option.defaultValue []

        c = 0 && List.isEmpty r

/// Immutable hash-based set datastructure.
/// hash/equality are determined using the Unchecked module
[<Struct; NoComparison; CustomEquality>]
[<StructuredFormatDisplay("{AsString}")>]
type HashSet<'a> internal(cnt : int, store : intmap<list<'a>>) =
    static let empty = HashSet(0, IntMap.empty)

    /// internal for getting the IntMap store
    member internal x.Store = store

    /// the empty HashSet
    static member Empty : HashSet<'a> = empty
    
    /// is the set empty? `O(1)`
    member x.IsEmpty = cnt = 0

    /// the number of elements in the set `O(1)`    
    member x.Count = cnt

    /// adds the given entry. `O(log N)`
    member x.Add (value : 'a) =
        let hash = Unchecked.hash value
        let mutable cnt = cnt

        let newStore = 
            store |> IntMap.alter (fun o ->
                match o with
                    | None -> 
                        cnt <- cnt + 1 
                        Some [value]
                    | Some old -> 
                        HashSetList.add &cnt value old |> Some
            ) hash

        HashSet(cnt, newStore)
  
    /// adds the given entry and returns none if it was already existing. `O(log N)`
    member x.TryAdd (value : 'a) =
        let res = x.Add value
        if res.Count <> cnt then Some res
        else None
      
    /// removes the given entry. `O(log N)`
    member x.Remove (value : 'a) =
        let hash = Unchecked.hash value
        let mutable cnt = cnt
        
        let newStore = 
            store |> IntMap.alter (fun o ->
                match o with
                    | None -> None
                    | Some old -> HashSetList.remove &cnt value old
            ) hash

        HashSet(cnt, newStore)
        
    /// removes the given entry and returns none if it was not existing. `O(log N)`
    member x.TryRemove (value : 'a) =
        let res = x.Remove value
        if res.Count <> cnt then Some res
        else None

    /// tests if the given key exists. `O(log N)`
    member x.Contains (value : 'a) =
        let hash = Unchecked.hash value
        match IntMap.tryFind hash store with
            | Some l -> l |> List.exists (Unchecked.equals value)
            | None -> false
    
    /// adds or deletes the given key.
    /// the update functions gets a boolean indicating whether the key was contained and
    /// can return a new "contained-value".
    /// `O(log N)`     
    member x.Alter(key : 'a, f : bool -> bool) =
        let hash = Unchecked.hash key
        let mutable cnt = cnt

        let newStore =  
            store |> IntMap.alter (fun ol ->
                match ol with
                    | None ->
                        if f false then
                            cnt <- cnt + 1
                            Some [key]
                        else
                            None
                    | Some ol ->
                        let mutable was = List.exists (Unchecked.equals key) ol
                        let should = f was
                        if should && not was then 
                            cnt <- cnt + 1
                            Some (key :: ol)
                        elif not should && was then
                            cnt <- cnt - 1
                            match List.filter (Unchecked.equals key >> not) ol with
                                | [] -> None
                                | l -> Some l
                        else
                            Some ol
            ) hash

        HashSet(cnt, newStore)

    /// creates a new set by applying the given function to all entries.
    /// `O(N * log N)`
    member x.Map (mapping : 'a -> 'b) =
        let mutable res = HashSet.Empty
        for e in x.ToSeq() do
            res <- res.Add(mapping e)
        res

    /// creates a new set by applying the given function to all entries.
    /// `O(N * log N)`
    member x.Choose (mapping : 'a -> option<'b>) =
        let mutable res = HashSet.Empty
        for e in x.ToSeq() do
            match mapping e with
                | Some e ->
                    res <- res.Add(e)
                | None ->
                    ()
        res

    /// creates a new set that contains all entries for which predicate was true.
    /// `O(N * log N)`
    member x.Filter (predicate : 'a -> bool) =
        let mutable cnt = 0
        let predicate v =
            if predicate v then
                cnt <- cnt + 1
                true
            else
                false

        let newStore =
            store |> IntMap.mapOption (fun l ->
                match List.filter predicate l with
                    | [] -> None
                    | l -> Some l
            )

        HashSet(cnt, newStore)
    
    /// creates a new set by applying the mapping function to each entry and unioning the results.
    /// `O(N * log N)`
    member x.Collect (mapping : 'a -> HashSet<'b>) =
        let mutable res = HashSet<'b>.Empty
        for (_,l) in IntMap.toSeq store do
            for e in l do
                res <- res.Union (mapping e)
        res

    /// applies the iter function to all entries of the set.
    /// `O(N)`
    member x.Iter (iter : 'a -> unit) =
        store |> IntMap.toSeq |> Seq.iter (fun (_,l) -> l |> List.iter iter)
    
    /// tests whether an entry making the predicate true exists.
    /// `O(N)`
    member x.Exists (predicate : 'a -> bool) =
        store |> IntMap.toSeq |> Seq.exists (fun (_,l) -> l |> List.exists predicate)

    /// tests whether all entries fulfil the given predicate.
    /// `O(N)`
    member x.Forall (predicate : 'a -> bool) =
        store |> IntMap.toSeq |> Seq.forall (fun (_,l) -> l |> List.forall predicate)

    /// folds over all entries of the set.
    /// note that the order for elements is undefined.
    /// `O(N)`
    member x.Fold (seed : 's, folder : 's -> 'a -> 's) =
        store |> IntMap.toSeq |> Seq.fold (fun s (_,l) ->
            l |> List.fold folder s
        ) seed

    /// creates a new set containing all elements from this and other.
    /// `O(N + M)`
    member x.Union (other : HashSet<'a>) : HashSet<'a> =
        let mutable dupl = 0
        let newStore = IntMap.appendWith (fun l r -> HashSetList.union &dupl l r) store other.Store
        HashSet(cnt + other.Count - dupl, newStore)

    /// creates a new set containing all elements from this that are not in other.
    /// `O(N + M)`
    member x.Difference (other : HashSet<'a>) : HashSet<'a> =
        let mutable cnt = 0
        let newStore =
            IntMap.mergeWithKey 
                (fun k ll rl -> HashSetList.difference &cnt ll rl) 
                (fun l -> cnt <- l |> IntMap.fold (fun s l -> s + List.length l) cnt; l)
                (fun r -> IntMap.empty) 
                store 
                other.Store

        HashSet(cnt, newStore)
    
    /// creates a new set containing all elements that are present in both sets.
    /// `O(N + M)`
    member x.Intersect (other : HashSet<'a>) : HashSet<'a> =
        let mutable cnt = 0
        let newStore =
            IntMap.mergeWithKey 
                (fun k ll rl -> HashSetList.intersect &cnt ll rl) 
                (fun l -> IntMap.empty)
                (fun r -> IntMap.empty) 
                store 
                other.Store

        HashSet(cnt, newStore)
       
    /// creates a seq holding all values contained in the set.
    /// `O(N)`
    member x.ToSeq() =
        store |> IntMap.toSeq |> Seq.collect snd
       
    /// creates a list holding all values contained in the set.
    /// `O(N)`
    member x.ToList() =
        store |> IntMap.toList |> List.collect snd
               
    /// creates an array holding all values contained in the set.
    /// `O(N)`
    member x.ToArray() =
        let result = Array.zeroCreate cnt
        let mutable i = 0
        for (_, list) in IntMap.toSeq store do
            for value in list do
                result.[i] <- value
                i <- i + 1
        result

    /// creates a set with a single entry
    /// `O(1)`
    static member Single (value : 'a) =
        empty.Add value

    /// creates a set with all entries from the seq.
    /// `O(N * log N)`
    static member OfSeq (seq : seq<'a>) =
        match seq with
        | :? HashSet<'a> as set -> set
        | _ -> 
            let mutable res = empty
            for e in seq do
                res <- res.Add e
            res
        
    /// creates a set with all entries from the list.
    /// `O(N * log N)`
    static member OfList (list : list<'a>) =
        HashSet.OfSeq list
        
    /// creates a set with all entries from the array.
    /// `O(N * log N)`
    static member OfArray (arr : array<'a>) =
        HashSet.OfSeq arr

    override x.GetHashCode() =
        match store with
            | Nil -> 0
            | _ -> store |> Seq.fold (fun s (h,_l) -> HashSetList.combineHash s h) 0

    override x.Equals(o) =
        match o with
            | :? HashSet<'a> as o -> 
                IntMap.equals HashSetList.equals store o.Store
            | _ ->
                false

    override x.ToString() =
        let suffix =
            if x.Count > 5 then "; ..."
            else ""

        let content =
            x.ToSeq() |> Seq.truncate 5 |> Seq.map (sprintf "%A") |> String.concat "; "

        "HashSet [" + content + suffix + "]"

    member private x.AsString = x.ToString()

    interface IEnumerable with
        member x.GetEnumerator() = new HashSetEnumerator<_>(store) :> _

    interface IEnumerable<'a> with
        member x.GetEnumerator() = new HashSetEnumerator<_>(store) :> _

and private HashSetEnumerator<'a>(store : intmap<list<'a>>) =
    let mutable stack = [store]
    let mutable inner = []
    let mutable current = Unchecked.defaultof<'a>

    let rec moveNext() =
        match inner with
            | [] -> 
                match stack with
                    | [] -> false
                    | h :: rest ->
                        stack <- rest
                        match h with
                            | Nil -> 
                                moveNext()

                            | Tip(_,vs) ->
                                match vs with
                                    | v :: rest ->
                                        current <- v
                                        inner <- rest
                                        true
                                    | [] ->
                                        moveNext()

                            | Bin(_,_,l,r) ->
                                stack <- l :: r :: stack
                                moveNext()
            | h :: rest ->
                current <- h
                inner <- rest
                true

    interface IEnumerator with
        member x.MoveNext() = moveNext()
        member x.Current = current :> obj
        member x.Reset() =
            stack <- [store]
            inner <- []
            current <- Unchecked.defaultof<_>

    interface IEnumerator<'a> with
        member x.Current = current
        member x.Dispose() =
            stack <- []
            inner <- []
            current <- Unchecked.defaultof<_>
            
/// Functional programming operators related to the HashSet<_> type.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HashSet =

    /// the empty set.
    [<GeneralizableValue>]
    let empty<'a> = HashSet<'a>.Empty

    /// creates a set with a single entry
    /// `O(1)`
    let inline single (value : 'a) =
        HashSet.Single value

    /// creates a set with all entries from the seq.
    /// `O(N * log N)`
    let inline ofSeq (seq : seq<'a>) =
        HashSet.OfSeq seq

    /// creates a set with all entries from the list.
    /// `O(N * log N)`
    let inline ofList (list : list<'a>) =
        HashSet.OfList list

    /// creates a set with all entries from the array.
    /// `O(N * log N)`
    let inline ofArray (arr : 'a[]) =
        HashSet.OfArray arr

    /// creates a seq holding all values contained in the set.
    /// `O(N)`
    let inline toSeq (set : HashSet<'a>) =
        set.ToSeq()

    /// creates a list holding all values contained in the set.
    /// `O(N)`
    let inline toList (set : HashSet<'a>) =
        set.ToList()

    /// creates an array holding all values contained in the set.
    /// `O(N)`
    let inline toArray (set : HashSet<'a>) =
        set.ToArray()

    /// adds the given entry. `O(log N)`
    let inline add (value : 'a) (set : HashSet<'a>) =
        set.Add value
        
    /// adds the given entry and returns none if it was already existing. `O(log N)`
    let inline tryAdd (value : 'a) (set : HashSet<'a>) =
        set.TryAdd value

    /// removes the given entry. `O(log N)`
    let inline remove (value : 'a) (set : HashSet<'a>) =
        set.Remove value
        
    /// removes the given entry and returns none if it was not existing. `O(log N)`
    let inline tryRemove (value : 'a) (set : HashSet<'a>) =
        set.TryRemove value

    /// adds or deletes the given key.
    /// the update functions gets a boolean indicating whether the key was contained and
    /// can return a new "contained-value".
    /// `O(log N)`   
    let inline alter (value : 'a) (mapping : bool -> bool) (set : HashSet<'a>) =
        set.Alter(value, mapping)

    /// creates a new set containing all elements from l and r.
    /// `O(N + M)`
    let inline union (l : HashSet<'a>) (r : HashSet<'a>) =
        l.Union r

    /// creates a new set containing all elements from the given sets.
    /// `O(N + M)`
    let inline unionMany (sets : seq<HashSet<'a>>) =
        sets |> Seq.fold union empty

    /// creates a new set containing all elements from l that are not in r.
    /// `O(N + M)`
    let inline difference (l : HashSet<'a>) (r : HashSet<'a>) =
        l.Difference r

    /// creates a new set containing all elements that are present in both sets.
    /// `O(N + M)`
    let inline intersect (l : HashSet<'a>) (r : HashSet<'a>) =
        l.Intersect r

    /// creates a new set by applying the given function to all entries.
    /// `O(N * log N)`
    let inline map (mapping : 'a -> 'b) (set : HashSet<'a>) =
        set.Map mapping

    /// creates a new set by applying the given function to all entries.
    /// `O(N * log N)`
    let inline choose (mapping : 'a -> option<'b>) (set : HashSet<'a>) =
        set.Choose mapping

    /// creates a new set that contains all entries for which predicate was true.
    /// `O(N * log N)`
    let inline filter (predicate : 'a -> bool) (set : HashSet<'a>) =
        set.Filter predicate

    /// creates a new set by applying the mapping function to each entry and unioning the results.
    /// `O(N * log N)`
    let inline collect (mapping : 'a -> HashSet<'b>) (set : HashSet<'a>) =
        set.Collect mapping

    /// applies the iter function to all entries of the set.
    /// `O(N)`
    let inline iter (mapping : 'a -> unit) (set : HashSet<'a>) =
        set.Iter mapping

    /// tests whether an entry making the predicate true exists.
    /// `O(N)`
    let inline exists (predicate : 'a -> bool) (set : HashSet<'a>) =
        set.Exists predicate

    /// tests whether all entries fulfil the given predicate.
    /// `O(N)`
    let inline forall (predicate : 'a -> bool) (set : HashSet<'a>) =
        set.Forall predicate

    /// folds over all entries of the set.
    /// note that the order for elements is undefined.
    /// `O(N)`
    let inline fold (folder : 's -> 'a -> 's) (seed : 's) (set : HashSet<'a>) =
        set.Fold(seed, folder)

    /// is the set empty? `O(1)`
    let inline isEmpty (set : HashSet<'a>) =
        set.IsEmpty

    /// the number of elements in the set `O(1)` 
    let inline count (set : HashSet<'a>) =
        set.Count

    /// tests if the given key exists. `O(log N)`
    let inline contains (value : 'a) (set : HashSet<'a>) =
        set.Contains value
