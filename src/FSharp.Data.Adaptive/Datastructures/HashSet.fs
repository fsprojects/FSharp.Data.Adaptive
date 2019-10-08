namespace FSharp.Data.Adaptive

open System.Collections
open System.Collections.Generic

/// Helper functions for hash-collision lists.
/// Most members have bad runtime, but the lists should be quite small when using appropriate hashCodes.
module internal HashSetList =

    let inline combineHash (a : int) (b : int) =
        uint32 a ^^^ uint32 b + 0x9e3779b9u + ((uint32 a) <<< 6) + ((uint32 a) >>> 2) |> int

    let rec add (cnt : ref<int>) (value : 'T) (list : list<'T>) =
        match list with
            | [] -> 
                cnt := !cnt + 1
                [value]
            | h :: tail ->
                if Unchecked.equals h value then
                    list
                else
                    h :: add cnt value tail

    let rec remove (cnt : ref<int>) (value : 'T) (list : list<'T>) =
        match list with
            | [] ->
                None
            | h :: tail ->
                if Unchecked.equals h value then
                    cnt := !cnt - 1
                    match tail with
                        | [] -> None
                        | _ -> Some tail
                else
                    match remove cnt value tail with
                        | Some t -> Some (h :: t)
                        | None -> Some [h]

    let rec union (dupl : ref<int>) (l : list<'T>) (r : list<'T>) =
        let newR = 
            r |> List.filter (fun r ->
                if l |> List.exists (Unchecked.equals r) then
                    dupl := !dupl + 1
                    false
                else
                    true
            )

        l @ newR

    let rec difference (cnt : ref<int>) (l : list<'T>) (r : list<'T>) =
        match l with
            | [] -> 
                None
            | h :: tail ->
                if List.exists (Unchecked.equals h) r then
                    difference cnt tail r
                else
                    cnt := !cnt + 1
                    match difference cnt tail r with
                        | Some t -> Some (h :: t)
                        | None -> Some [h]
                    

    let rec intersect (cnt : ref<int>) (l : list<'T>) (r : list<'T>) =
        match l with
            | [] ->
                None
            | h :: tail ->
                if List.exists (Unchecked.equals h) r then
                    cnt := !cnt + 1
                    match intersect cnt tail r with
                        | Some t -> Some (h :: t)
                        | None -> Some [h]
                else
                    intersect cnt tail r


    let rec mergeWithOption (f : 'A -> bool -> bool -> option<'B>) (l : list<'A>) (r : list<'A>) =
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
          
    let rec equals (l : list<'T>) (r : list<'T>) =
        let mutable r = r
        let c = ref 0
        
        use e = (l :> seq<_>).GetEnumerator()
        while !c = 0 && e.MoveNext() do
            let l = e.Current
            c := 1
            r <- remove c l r |> Option.defaultValue []

        !c = 0 && List.isEmpty r

/// Immutable hash-based set datastructure.
/// Hash/equality are determined using the Unchecked module
[<Struct; NoComparison; CustomEquality>]
[<StructuredFormatDisplay("{AsString}")>]
type HashSet<'T> internal(cnt : int, store : intmap<list<'T>>) =
    static let empty = HashSet(0, IntMap.empty)

    /// Internal for getting the IntMap store
    member internal x.Store = store

    /// The empty HashSet
    static member Empty : HashSet<'T> = empty
    
    /// Is the set empty? `O(1)`
    member x.IsEmpty = cnt = 0

    /// The number of elements in the set `O(1)`    
    member x.Count = cnt

    /// Adds the given entry. `O(log N)`
    member x.Add (value : 'T) =
        let hash = Unchecked.hash value
        let cnt = ref cnt

        let newStore = 
            store |> IntMap.alter (fun o ->
                match o with
                    | None -> 
                        cnt := !cnt + 1 
                        Some [value]
                    | Some old -> 
                        HashSetList.add cnt value old |> Some
            ) hash

        HashSet(!cnt, newStore)
  
    /// Adds the given entry and returns none if it was already existing. `O(log N)`
    member x.TryAdd (value : 'T) =
        let res = x.Add value
        if res.Count <> cnt then Some res
        else None
      
    /// Removes the given entry. `O(log N)`
    member x.Remove (value : 'T) =
        let hash = Unchecked.hash value
        let cnt = ref cnt
        
        let newStore = 
            store |> IntMap.alter (fun o ->
                match o with
                    | None -> None
                    | Some old -> HashSetList.remove cnt value old
            ) hash

        HashSet(!cnt, newStore)
        
    /// Removes the given entry and returns none if it was not existing. `O(log N)`
    member x.TryRemove (value : 'T) =
        let res = x.Remove value
        if res.Count <> cnt then Some res
        else None

    /// Tests if the given key exists. `O(log N)`
    member x.Contains (value : 'T) =
        let hash = Unchecked.hash value
        match IntMap.tryFind hash store with
            | Some l -> l |> List.exists (Unchecked.equals value)
            | None -> false
    
    /// Adds or deletes the given key.
    /// The update functions gets a boolean indicating whether the key was contained and
    /// Can return a new "contained-value".
    /// `O(log N)`     
    member x.Alter(key : 'T, f : bool -> bool) =
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

    /// Creates a new set by applying the given function to all entries.
    /// `O(N * log N)`
    member x.Map (mapping : 'T -> 'B) =
        let mutable res = HashSet.Empty
        for e in x.ToSeq() do
            res <- res.Add(mapping e)
        res

    /// Creates a new set by applying the given function to all entries.
    /// `O(N * log N)`
    member x.Choose (mapping : 'T -> option<'B>) =
        let mutable res = HashSet.Empty
        for e in x.ToSeq() do
            match mapping e with
                | Some e ->
                    res <- res.Add(e)
                | None ->
                    ()
        res

    /// Creates a new set that contains all entries for which predicate was true.
    /// `O(N * log N)`
    member x.Filter (predicate : 'T -> bool) =
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
    
    /// Creates a new set by applying the mapping function to each entry and unioning the results.
    /// `O(N * log N)`
    member x.Collect (mapping : 'T -> HashSet<'B>) =
        let mutable res = HashSet<'B>.Empty
        for (_,l) in IntMap.toSeq store do
            for e in l do
                res <- res.Union (mapping e)
        res

    /// Applies the iter function to all entries of the set.
    /// `O(N)`
    member x.Iter (iter : 'T -> unit) =
        store |> IntMap.toSeq |> Seq.iter (fun (_,l) -> l |> List.iter iter)
    
    /// Tests whether an entry making the predicate true exists.
    /// `O(N)`
    member x.Exists (predicate : 'T -> bool) =
        store |> IntMap.toSeq |> Seq.exists (fun (_,l) -> l |> List.exists predicate)

    /// Tests whether all entries fulfil the given predicate.
    /// `O(N)`
    member x.Forall (predicate : 'T -> bool) =
        store |> IntMap.toSeq |> Seq.forall (fun (_,l) -> l |> List.forall predicate)

    /// Folds over all entries of the set.
    /// Note that the order for elements is undefined.
    /// `O(N)`
    member x.Fold (seed : 'S, folder : 'S -> 'T -> 'S) =
        store |> IntMap.toSeq |> Seq.fold (fun s (_,l) ->
            l |> List.fold folder s
        ) seed

    /// Creates a new set containing all elements from this and other.
    /// `O(N + M)`
    member x.Union (other : HashSet<'T>) : HashSet<'T> =
        let dupl = ref 0
        let newStore = IntMap.appendWith (fun l r -> HashSetList.union dupl l r) store other.Store
        HashSet(cnt + other.Count - !dupl, newStore)

    /// Creates a new set containing all elements from this that are not in other.
    /// `O(N + M)`
    member x.Difference (other : HashSet<'T>) : HashSet<'T> =
        let cnt = ref 0
        let newStore =
            IntMap.mergeWithKey 
                (fun k ll rl -> HashSetList.difference cnt ll rl) 
                (fun l -> cnt := l |> IntMap.fold (fun s l -> s + List.length l) !cnt; l)
                (fun r -> IntMap.empty) 
                store 
                other.Store

        HashSet(!cnt, newStore)
    
    /// Creates a new set containing all elements that are present in both sets.
    /// `O(N + M)`
    member x.Intersect (other : HashSet<'T>) : HashSet<'T> =
        let cnt = ref 0
        let newStore =
            IntMap.mergeWithKey 
                (fun k ll rl -> HashSetList.intersect cnt ll rl) 
                (fun l -> IntMap.empty)
                (fun r -> IntMap.empty) 
                store 
                other.Store

        HashSet(!cnt, newStore)
       
    /// Creates a seq holding all values contained in the set.
    /// `O(N)`
    member x.ToSeq() =
        store |> IntMap.toSeq |> Seq.collect snd
       
    /// Creates a list holding all values contained in the set.
    /// `O(N)`
    member x.ToList() =
        store |> IntMap.toList |> List.collect snd
               
    /// Creates an array holding all values contained in the set.
    /// `O(N)`
    member x.ToArray() =
        let result = Array.zeroCreate cnt
        let mutable i = 0
        for (_, list) in IntMap.toSeq store do
            for value in list do
                result.[i] <- value
                i <- i + 1
        result

    /// Creates a set with a single entry
    /// `O(1)`
    static member Single (value : 'T) =
        empty.Add value

    /// Creates a set with all entries from the seq.
    /// `O(N * log N)`
    static member OfSeq (seq : seq<'T>) =
        #if !ADAPTIVE_NO_TYPE_TESTS
        match seq with
        | :? HashSet<'T> as set -> set
        | _ -> 
        #endif
            let mutable res = empty
            for e in seq do
                res <- res.Add e
            res
        
    /// Creates a set with all entries from the list.
    /// `O(N * log N)`
    static member OfList (list : list<'T>) =
        HashSet.OfSeq list
        
    /// Creates a set with all entries from the array.
    /// `O(N * log N)`
    static member OfArray (arr : array<'T>) =
        HashSet.OfSeq arr

    /// Conservatively determines whether the two HashSets are equal.
    /// `O(1)`
    member x.ConservativeEquals(other : HashSet<'T>) =
        System.Object.ReferenceEquals(store, other.Store)

    override x.GetHashCode() =
        match store with
            | Nil -> 0
            | _ -> store |> Seq.fold (fun s (h,_l) -> HashSetList.combineHash s h) 0

    override x.Equals(o) =
        #if ADAPTIVE_NO_TYPE_TESTS 
        IntMap.equals HashSetList.equals store (unbox<HashSet<'T>> o).Store
        #else
        match o with
        | :? HashSet<'T> as o -> 
            IntMap.equals HashSetList.equals store o.Store
        | _ ->
            false
        #endif

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

    interface IEnumerable<'T> with
        member x.GetEnumerator() = new HashSetEnumerator<_>(store) :> _

and private HashSetEnumerator<'T>(store : intmap<list<'T>>) =
    let mutable stack = [store]
    let mutable inner = []
    let mutable current = Unchecked.defaultof<'T>

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

    interface IEnumerator<'T> with
        member x.Current = current
        member x.Dispose() =
            stack <- []
            inner <- []
            current <- Unchecked.defaultof<_>
            
/// Functional programming operators related to the HashSet<_> type.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HashSet =

    /// The empty set.
    [<GeneralizableValue>]
    let empty<'T> = HashSet<'T>.Empty

    /// Creates a set with a single entry
    /// `O(1)`
    let inline single (value : 'T) =
        HashSet.Single value

    /// Creates a set with all entries from the seq.
    /// `O(N * log N)`
    let inline ofSeq (seq : seq<'T>) =
        HashSet.OfSeq seq

    /// Creates a set with all entries from the list.
    /// `O(N * log N)`
    let inline ofList (list : list<'T>) =
        HashSet.OfList list

    /// Creates a set with all entries from the array.
    /// `O(N * log N)`
    let inline ofArray (arr : 'T[]) =
        HashSet.OfArray arr

    /// Creates a seq holding all values contained in the set.
    /// `O(N)`
    let inline toSeq (set : HashSet<'T>) =
        set.ToSeq()

    /// Creates a list holding all values contained in the set.
    /// `O(N)`
    let inline toList (set : HashSet<'T>) =
        set.ToList()

    /// Creates an array holding all values contained in the set.
    /// `O(N)`
    let inline toArray (set : HashSet<'T>) =
        set.ToArray()

    /// Adds the given entry. `O(log N)`
    let inline add (value : 'T) (set : HashSet<'T>) =
        set.Add value
        
    /// Adds the given entry and returns none if it was already existing. `O(log N)`
    let inline tryAdd (value : 'T) (set : HashSet<'T>) =
        set.TryAdd value

    /// Removes the given entry. `O(log N)`
    let inline remove (value : 'T) (set : HashSet<'T>) =
        set.Remove value
        
    /// Removes the given entry and returns none if it was not existing. `O(log N)`
    let inline tryRemove (value : 'T) (set : HashSet<'T>) =
        set.TryRemove value

    /// Adds or deletes the given key.
    /// The update functions gets a boolean indicating whether the key was contained and
    /// Can return a new "contained-value".
    /// `O(log N)`   
    let inline alter (value : 'T) (mapping : bool -> bool) (set : HashSet<'T>) =
        set.Alter(value, mapping)

    /// Creates a new set containing all elements from l and r.
    /// `O(N + M)`
    let inline union (l : HashSet<'T>) (r : HashSet<'T>) =
        l.Union r

    /// Creates a new set containing all elements from the given sets.
    /// `O(N + M)`
    let inline unionMany (sets : seq<HashSet<'T>>) =
        sets |> Seq.fold union empty

    /// Creates a new set containing all elements from l that are not in r.
    /// `O(N + M)`
    let inline difference (l : HashSet<'T>) (r : HashSet<'T>) =
        l.Difference r

    /// Creates a new set containing all elements that are present in both sets.
    /// `O(N + M)`
    let inline intersect (l : HashSet<'T>) (r : HashSet<'T>) =
        l.Intersect r

    /// Creates a new set by applying the given function to all entries.
    /// `O(N * log N)`
    let inline map (mapping : 'A -> 'B) (set : HashSet<'A>) =
        set.Map mapping

    /// Creates a new set by applying the given function to all entries.
    /// `O(N * log N)`
    let inline choose (mapping : 'A -> option<'B>) (set : HashSet<'A>) =
        set.Choose mapping

    /// Creates a new set that contains all entries for which predicate was true.
    /// `O(N * log N)`
    let inline filter (predicate : 'T -> bool) (set : HashSet<'T>) =
        set.Filter predicate

    /// Creates a new set by applying the mapping function to each entry and unioning the results.
    /// `O(N * log N)`
    let inline collect (mapping : 'A -> HashSet<'B>) (set : HashSet<'A>) =
        set.Collect mapping

    /// Applies the iter function to all entries of the set.
    /// `O(N)`
    let inline iter (mapping : 'T -> unit) (set : HashSet<'T>) =
        set.Iter mapping

    /// Tests whether an entry making the predicate true exists.
    /// `O(N)`
    let inline exists (predicate : 'T -> bool) (set : HashSet<'T>) =
        set.Exists predicate

    /// Tests whether all entries fulfil the given predicate.
    /// `O(N)`
    let inline forall (predicate : 'T -> bool) (set : HashSet<'T>) =
        set.Forall predicate

    /// Folds over all entries of the set.
    /// Note that the order for elements is undefined.
    /// `O(N)`
    let inline fold (folder : 'S -> 'T -> 'S) (seed : 'S) (set : HashSet<'T>) =
        set.Fold(seed, folder)

    /// Is the set empty? `O(1)`
    let inline isEmpty (set : HashSet<'T>) =
        set.IsEmpty

    /// The number of elements in the set `O(1)` 
    let inline count (set : HashSet<'T>) =
        set.Count

    /// Tests if the given key exists. `O(log N)`
    let inline contains (value : 'T) (set : HashSet<'T>) =
        set.Contains value
