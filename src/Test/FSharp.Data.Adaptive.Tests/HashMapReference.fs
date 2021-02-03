module HashMapReference

open System
open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit
open FSharp.Data.Adaptive
open FSharp.Data.Traceable

[<CustomEquality; CustomComparison>]
type StupidHash = { value : int } with
    
    static member Zero = { value = 0 }
    static member (+) (l : StupidHash, r : StupidHash) = { value = l.value + r.value }

    interface IComparable with
        member x.CompareTo o =
            match o with
                | :? StupidHash as o -> compare x.value o.value
                | _ -> failwith "cannot compare"

    override x.GetHashCode() = -(x.value % 32)
    override x.Equals o =   
        match o with
        | :? StupidHash as o -> x.value = o.value
        | _ -> false
    
module Set =

    let check (set : Set<'A>) (hashSet : HashSet<'A>) =
        if Set.count set <> hashSet.Count then 
            failwithf "inconsistent count %A vs %A" (Set.count set) hashSet.Count

        let lh = HashSet.toList hashSet |> List.sort
        let ah = HashSet.toArray hashSet |> Array.toList |> List.sort
        let sh = HashSet.toSeq hashSet |> Seq.toList |> List.sort
        let ref = Set.toList set

        lh |> should equal ref
        ah |> should equal ref
        sh |> should equal ref


module Map =

    let single (key : 'A) (value : 'B) = Map.ofList [key, value]

    let ofSeqV (elements : seq<struct('A * 'B)>) =
        let mutable res = Map.empty
        for struct(k, v) in elements do res <- Map.add k v res
        res
        
    let ofListV (elements : list<struct('A * 'B)>) =
        let mutable res = Map.empty
        for struct(k, v) in elements do res <- Map.add k v res
        res
        
    let ofArrayV (elements : array<struct('A * 'B)>) =
        let mutable res = Map.empty
        for struct(k, v) in elements do res <- Map.add k v res
        res

    let tryRemove (key : 'A) (map : Map<'A, 'B>) =
        match Map.tryFind key map with
        | Some v ->
            Some (v, Map.remove key map)
        | None ->
            None
            
    let tryRemoveV (key : 'A) (map : Map<'A, 'B>) =
        match Map.tryFind key map with
        | Some v ->
            ValueSome struct(v, Map.remove key map)
        | None ->
            ValueNone

    let alter (key : 'A) (update : option<'B> -> option<'B>) (map : Map<'A, 'B>) =
        let o = Map.tryFind key map
        let n = update o
        match o, n with
        | _, Some v -> Map.add key v map
        | Some _, None -> Map.remove key map
        | _ -> map
        
    let alterV (key : 'A) (update : voption<'B> -> voption<'B>) (map : Map<'A, 'B>) =
        let o = Map.tryFind key map
        let n = update (match o with | Some o -> ValueSome o | None -> ValueNone)
        match o, n with
        | _, ValueSome v -> Map.add key v map
        | Some _, ValueNone -> Map.remove key map
        | _ -> map

    let tryFindV (key : 'A)(map : Map<'A, 'B>) =
        match Map.tryFind key map with
        | Some v -> ValueSome v
        | None -> ValueNone

    let choose (mapping : 'A -> 'B -> option<'C>) (map : Map<'A, 'B>) =
        map 
        |> Map.toSeq 
        |> Seq.choose (fun (a,b) -> 
            match mapping a b with
            | Some c -> Some (a,c)
            | None -> None
        )
        |> Map.ofSeq
        
    let chooseV (mapping : 'A -> 'B -> voption<'C>) (map : Map<'A, 'B>) =
        map 
        |> Map.toSeq 
        |> Seq.choose (fun (a,b) -> 
            match mapping a b with
            | ValueSome c -> Some (a,c)
            | ValueNone -> None
        )
        |> Map.ofSeq

    let toSeqV (m : Map<'A, 'B>) =
        m |> Map.toSeq |> Seq.map (fun (a,b) -> struct(a,b))
        
    let toListV (m : Map<'A, 'B>) =
        m |> Map.toList |> List.map (fun (a,b) -> struct(a,b))
        
    let toArrayV (m : Map<'A, 'B>) =
        m |> Map.toArray |> Array.map (fun (a,b) -> struct(a,b))
        
    let toKeySeq (m : Map<'A, 'B>) =
        m |> Map.toSeq |> Seq.map (fun (a,b) -> a)
        
    let toKeyList (m : Map<'A, 'B>) =
        m |> Map.toList |> List.map (fun (a,b) -> a)
        
    let toKeyArray (m : Map<'A, 'B>) =
        m |> Map.toArray |> Array.map (fun (a,b) -> a)
        
    let toValueSey (m : Map<'A, 'B>) =
        m |> Map.toSeq |> Seq.map (fun (a,b) -> b)
        
    let toValueList (m : Map<'A, 'B>) =
        m |> Map.toList |> List.map (fun (a,b) -> b)
        
    let toValueArray (m : Map<'A, 'B>) =
        m |> Map.toArray |> Array.map (fun (a,b) -> b)

    let choose2 (mapping : 'K -> option<'A> -> option<'B> -> option<'C>) (a : Map<'K, 'A>) (b : Map<'K, 'B>) =
        let mutable b = b
        let mutable res = Map.empty
        for (KeyValue(k, va)) in a do
            match tryRemove k b with
            | Some (vb, nb) ->
                match mapping k (Some va) (Some vb) with
                | Some c -> res <- Map.add k c res
                | None -> ()
                b <- nb
            | None ->
                match mapping k (Some va) None with
                | Some c -> res <- Map.add k c res
                | None -> ()
                
        for (KeyValue(k, vb)) in b do
            match mapping k None (Some vb) with
            | Some c -> res <- Map.add k c res
            | None -> ()

        res

    let choose2V (mapping : 'K -> voption<'A> -> voption<'B> -> voption<'C>) (a : Map<'K, 'A>) (b : Map<'K, 'B>) =
        (a,b) ||> choose2 (fun k a b ->
            let a = match a with | Some a -> ValueSome a | None -> ValueNone
            let b = match b with | Some a -> ValueSome a | None -> ValueNone
            match mapping k a b with
            | ValueSome c -> Some c
            | ValueNone -> None
        )

    let map2 (mapping : 'K -> option<'A> -> option<'B> -> 'C) (a : Map<'K, 'A>) (b : Map<'K, 'B>) =
        (a,b) ||> choose2 (fun k a b -> mapping k a b |> Some)
        
    let map2V (mapping : 'K -> voption<'A> -> voption<'B> -> 'C) (a : Map<'K, 'A>) (b : Map<'K, 'B>) =
        (a,b) ||> choose2V (fun k a b -> mapping k a b |> ValueSome)

    let union (a : Map<'K, 'A>) (b : Map<'K, 'A>) =
        let mutable res = a
        for (KeyValue(k, v)) in b do
            res <- Map.add k v res
        res
        
    let unionWith (resolve : 'K -> 'A -> 'A -> 'A) (a : Map<'K, 'A>) (b : Map<'K, 'A>) =
        let mutable res = a
        for (KeyValue(k, v)) in b do
            let newValue = 
                match Map.tryFind k res with
                | Some o -> resolve k o v
                | None -> v
            res <- Map.add k newValue res
        res
        
    let unionWithV (resolve : 'K -> 'A -> 'A -> voption<'A>) (a : Map<'K, 'A>) (b : Map<'K, 'A>) =
        let mutable res = a
        for (KeyValue(k, v)) in b do
            let newValue = 
                match Map.tryFind k res with
                | Some o -> resolve k o v
                | None -> ValueSome v
            match newValue with
            | ValueSome newValue -> 
                res <- Map.add k newValue res
            | ValueNone ->
                res <- Map.remove k res
        res
    let check (m : Map<'A, 'B>) (h : HashMap<'A, 'B>) =
        let lh = h |> HashMap.toList |> List.sortBy fst
        let ah = h |> HashMap.toArray |> Array.toList |> List.sortBy fst
        let sh = h |> HashMap.toSeq |> Seq.toList |> List.sortBy fst
        let s = m |> Map.toList
        h.Count |> should equal m.Count 
        lh |> should equal s
        sh |> should equal s
        ah |> should equal s




type TestType = StupidHash
type ValueType = string
type ValueType2 = string

[<Test>]
let empty() =
    let m = Map.empty<TestType, ValueType>
    let h = HashMap.empty<TestType, ValueType>
    Map.check m h

[<Property(EndSize = 10000)>]
let count (s : Map<TestType, ValueType>) =
    let h = HashMap.ofMap s
    Map.check s h

    HashMap.count h |> should equal (Map.count s)
    h.Count |> should equal (Map.count s)
    
[<Property(EndSize = 10000)>]
let isEmpty (s : Map<TestType, ValueType>) =
    let h = HashMap.ofMap s
    Map.check s h

    HashMap.isEmpty h |> should equal (Map.isEmpty s)
    h.IsEmpty |> should equal (Map.isEmpty s)
      
[<Property(EndSize = 10000)>]
let equals (m1 : Map<TestType, ValueType>) (m2 : Map<TestType, ValueType>)=
    let h1 = HashMap.ofMap m1
    let h2 = HashMap.ofMap m2
    Map.check m1 h1
    Map.check m2 h2
    
    (h1 = h1) |> should be True
    (h2 = h2) |> should be True
    (h1 = h2) |> should equal (m1 = m2)
      
[<Property(EndSize = 10000)>]
let single (key : TestType) (value : ValueType) =
    let h = HashMap.single key value
    let m = Map.single key value
    Map.check m h
    
[<Property(EndSize = 10000)>]
let ofSeq (elements : array<TestType * ValueType>) =
    let elements = Seq.init elements.Length (fun i -> elements.[i])
    let h = HashMap.ofSeq elements
    let m = Map.ofSeq elements
    Map.check m h
    
[<Property(EndSize = 10000)>]
let ofSeqV (elements : array<struct(TestType * ValueType)>) =
    let elements = Seq.init elements.Length (fun i -> elements.[i])
    let h = HashMap.ofSeqV elements
    let m = Map.ofSeqV elements
    Map.check m h

[<Property(EndSize = 10000)>]
let ofList (elements : list<TestType * ValueType>) =
    let h = HashMap.ofList elements
    let m = Map.ofList elements
    Map.check m h

[<Property(EndSize = 10000)>]
let ofListV (elements : list<struct(TestType * ValueType)>) =
    let h = HashMap.ofListV elements
    let m = Map.ofListV elements
    Map.check m h

[<Property(EndSize = 10000)>]
let ofArray (elements : array<TestType * ValueType>) =
    let h = HashMap.ofArray elements
    let m = Map.ofArray elements
    Map.check m h

[<Property(EndSize = 10000)>]
let ofArrayV (elements : array<struct(TestType * ValueType)>) =
    let h = HashMap.ofArrayV elements
    let m = Map.ofArrayV elements
    Map.check m h

    
[<Property(EndSize = 10000)>]
let add (m : Map<TestType, ValueType>) (key : TestType) (value : ValueType) =
    let h = HashMap.ofMap m
    Map.check m h

    let m1 = m |> Map.add key value
    let h1 = h |> HashMap.add key value
    Map.check m1 h1
    
    
[<Property(EndSize = 10000)>]
let remove (m : Map<TestType, ValueType>) (key : TestType) =
    let h = HashMap.ofMap m
    Map.check m h

    let m1 = m |> Map.remove key
    let h1 = h |> HashMap.remove key
    Map.check m1 h1
    
    
[<Property(EndSize = 10000)>]
let tryRemove (m : Map<TestType, ValueType>) (key : TestType) =
    let h = HashMap.ofMap m
    Map.check m h
    let m1 = m |> Map.tryRemove key
    let h1 = h |> HashMap.tryRemove key
    match m1, h1 with
    | Some(mv, m1), Some(hv, h1) ->
        hv |> should equal mv
        Map.check m1 h1
    | None, None ->
        ()
    | _ -> 
        failwithf "inconsistent tryRemove %0A %0A: %A vs %A" key m m1 h1
        
[<Property(EndSize = 10000)>]
let tryRemoveV (m : Map<TestType, ValueType>) (key : TestType) =
    let h = HashMap.ofMap m
    Map.check m h
    let m1 = m |> Map.tryRemoveV key
    let h1 = h |> HashMap.tryRemoveV key
    match m1, h1 with
    | ValueSome(mv, m1), ValueSome(hv, h1) ->
        hv |> should equal mv
        Map.check m1 h1
    | ValueNone, ValueNone ->
        ()
    | _ -> 
        failwithf "inconsistent tryRemove %0A %0A: %A vs %A" key m m1 h1


[<Property(EndSize = 10000)>]
let alter (m : Map<TestType, ValueType>) (key : TestType) (update : option<ValueType> -> option<ValueType>) =
    let h = HashMap.ofMap m
    Map.check m h
    let m1 = m |> Map.alter key update
    let h1 = h |> HashMap.alter key update
    Map.check m1 h1

[<Property(EndSize = 10000)>]
let alterV (m : Map<TestType, ValueType>) (key : TestType) (update : voption<ValueType> -> voption<ValueType>) =
    let h = HashMap.ofMap m
    Map.check m h
    let m1 = m |> Map.alterV key update
    let h1 = h |> HashMap.alterV key update
    Map.check m1 h1

[<Property(EndSize = 10000)>]
let containsKey (m : Map<TestType, ValueType>) (key : TestType) =
    let h = HashMap.ofMap m
    Map.check m h
    let m1 = m |> Map.containsKey key
    let h1 = h |> HashMap.containsKey key
    h1 |> should equal m1

[<Property(EndSize = 10000)>]
let tryFind (m : Map<TestType, ValueType>) (key : TestType) =
    let h = HashMap.ofMap m
    Map.check m h
    let m1 = m |> Map.tryFind key
    let h1 = h |> HashMap.tryFind key
    h1 |> should equal m1

[<Property(EndSize = 10000)>]
let tryFindV (m : Map<TestType, ValueType>) (key : TestType) =
    let h = HashMap.ofMap m
    Map.check m h
    let m1 = m |> Map.tryFindV key
    let h1 = h |> HashMap.tryFindV key
    h1 |> should equal m1
    
[<Property(EndSize = 10000)>]
let find (m : Map<TestType, ValueType>) (key : TestType) =
    let h = HashMap.ofMap m
    Map.check m h
    let m1 = try m |> Map.find key |> Some with :? System.Collections.Generic.KeyNotFoundException -> None
    let h1 = try h |> HashMap.find key |> Some with :? System.Collections.Generic.KeyNotFoundException -> None
    h1 |> should equal m1

[<Property(EndSize = 10000)>]
let forall (m : Map<TestType, ValueType>) (predicate : TestType -> ValueType -> bool) =
    let h = HashMap.ofMap m
    Map.check m h

    let mr = m |> Map.forall predicate
    let hr = h |> HashMap.forall predicate
    hr |> should equal mr

[<Property(EndSize = 10000)>]
let exists (m : Map<TestType, ValueType>) (predicate : TestType -> ValueType -> bool) =
    let h = HashMap.ofMap m
    Map.check m h

    let mr = m |> Map.exists predicate
    let hr = h |> HashMap.exists predicate
    hr |> should equal mr

[<Property(EndSize = 10000)>]
let map (m : Map<TestType, ValueType>) (mapping : TestType -> ValueType -> ValueType2) =
    let h = HashMap.ofMap m
    Map.check m h

    let mr = m |> Map.map mapping
    let hr = h |> HashMap.map mapping
    Map.check mr hr

[<Property(EndSize = 10000)>]
let choose (m : Map<TestType, ValueType>) (mapping : TestType -> ValueType -> option<ValueType2>) =
    let h = HashMap.ofMap m
    Map.check m h

    let mr = m |> Map.choose mapping
    let hr = h |> HashMap.choose mapping
    Map.check mr hr

[<Property(EndSize = 10000)>]
let chooseV (m : Map<TestType, ValueType>) (mapping : TestType -> ValueType -> voption<ValueType2>) =
    let h = HashMap.ofMap m
    Map.check m h

    let mr = m |> Map.chooseV mapping
    let hr = h |> HashMap.chooseV mapping
    Map.check mr hr
    

[<Property(EndSize = 10000)>]
let filter (m : Map<TestType, ValueType>) (predicate : TestType -> ValueType -> bool) =
    let h = HashMap.ofMap m
    Map.check m h

    let mr = m |> Map.filter predicate
    let hr = h |> HashMap.filter predicate
    Map.check mr hr

    
[<Property(EndSize = 10000)>]
let iter (m : Map<TestType, ValueType>)  =
    let h = HashMap.ofMap m
    Map.check m h

    let mutable ml = []
    let mutable hl = []
    m |> Map.iter (fun k v -> ml <- (k,v) :: ml)
    h |> HashMap.iter (fun k v -> hl <- (k,v) :: hl)
    let ml1 = List.sortBy fst ml
    let hl1 = List.sortBy fst hl

    hl1 |> should equal ml1

    
[<Property(EndSize = 10000)>]
let fold (m : Map<TestType, ValueType>)  =
    let h = HashMap.ofMap m
    Map.check m h

    let mr = (0, m) ||> Map.fold (fun s k v -> s + (Unchecked.hash k) * (Unchecked.hash v))
    let hr = (0, h) ||> HashMap.fold (fun s k v -> s + (Unchecked.hash k) * (Unchecked.hash v))
    hr |> should equal mr
    
[<Property(EndSize = 10000)>]
let toList (m : Map<TestType, ValueType>)  =
    let h = HashMap.ofMap m
    Map.check m h
    let mr = m |> Map.toList
    let hr = h |> HashMap.toList |> List.sort
    hr |> should equal mr
    
[<Property(EndSize = 10000)>]
let toListV (m : Map<TestType, ValueType>)  =
    let h = HashMap.ofMap m
    Map.check m h
    let mr = m |> Map.toListV
    let hr = h |> HashMap.toListV |> List.sort
    hr |> should equal mr
     
[<Property(EndSize = 10000)>]
let toKeyList (m : Map<TestType, ValueType>)  =
    let h = HashMap.ofMap m
    Map.check m h
    let mr = m |> Map.toKeyList
    let hr = h |> HashMap.toKeyList |> List.sort
    hr |> should equal mr

[<Property(EndSize = 10000)>]
let toValueList (m : Map<TestType, ValueType>)  =
    let h = HashMap.ofMap m
    Map.check m h
    let mr = m |> Map.toValueList |> List.sort
    let hr = h |> HashMap.toValueList |> List.sort
    hr |> should equal mr
    
[<Property(EndSize = 10000)>]
let toArray (m : Map<TestType, ValueType>)  =
    let h = HashMap.ofMap m
    Map.check m h
    let mr = m |> Map.toList
    let hr = h |> HashMap.toArray |> Array.toList |> List.sort
    hr |> should equal mr
    
[<Property(EndSize = 10000)>]
let toArrayV (m : Map<TestType, ValueType>)  =
    let h = HashMap.ofMap m
    Map.check m h
    let mr = m |> Map.toListV
    let hr = h |> HashMap.toArrayV |> Array.toList |> List.sort
    hr |> should equal mr
         
[<Property(EndSize = 10000)>]
let toKeyArray (m : Map<TestType, ValueType>)  =
    let h = HashMap.ofMap m
    Map.check m h
    let mr = m |> Map.toKeyList
    let hr = h |> HashMap.toKeyArray |> Array.toList |> List.sort
    hr |> should equal mr

[<Property(EndSize = 10000)>]
let toValueArray (m : Map<TestType, ValueType>)  =
    let h = HashMap.ofMap m
    Map.check m h
    let mr = m |> Map.toValueList |> List.sort
    let hr = h |> HashMap.toValueArray |> Array.toList |> List.sort
    hr |> should equal mr
    
[<Property(EndSize = 10000)>]
let toSeq (m : Map<TestType, ValueType>)  =
    let h = HashMap.ofMap m
    Map.check m h
    let mr = m |> Map.toList
    let hr = h |> HashMap.toSeq |> Seq.toList |> List.sort
    hr |> should equal mr
    
[<Property(EndSize = 10000)>]
let toSeqV (m : Map<TestType, ValueType>)  =
    let h = HashMap.ofMap m
    Map.check m h
    let mr = m |> Map.toListV
    let hr = h |> HashMap.toSeqV |> Seq.toList |> List.sort
    hr |> should equal mr

    
[<Property(EndSize = 10000)>]
let toKeySeq (m : Map<TestType, ValueType>)  =
    let h = HashMap.ofMap m
    Map.check m h
    let mr = m |> Map.toKeyList
    let hr = h |> HashMap.toKeySeq |> Seq.toList |> List.sort
    hr |> should equal mr

[<Property(EndSize = 10000)>]
let toValueSeq (m : Map<TestType, ValueType>)  =
    let h = HashMap.ofMap m
    Map.check m h
    let mr = m |> Map.toValueList |> List.sort
    let hr = h |> HashMap.toValueSeq |> Seq.toList |> List.sort
    hr |> should equal mr
    
[<Property(EndSize = 10000)>]
let keys (m : Map<TestType, ValueType>)  =
    let h = HashMap.ofMap m

    let mr = m |> Map.toKeyList |> Set.ofList
    let hr = h |> HashMap.keys
    Set.check mr hr

[<Property(EndSize = 10000)>]
let choose2 (m1 : Map<TestType, ValueType>) (m2 : Map<TestType, ValueType>) (mapping : TestType -> option<ValueType> -> option<ValueType> -> option<ValueType2>)  =
    let h1 = HashMap.ofMap m1
    let h2 = HashMap.ofMap m2

    let mr = (m1, m2) ||> Map.choose2 mapping
    let hr = (h1, h2) ||> HashMap.choose2 mapping

    Map.check mr hr

[<Property(EndSize = 10000)>]
let choose2V (m1 : Map<TestType, ValueType>) (m2 : Map<TestType, ValueType>) (mapping : TestType -> voption<ValueType> -> voption<ValueType> -> voption<ValueType2>)  =
    let h1 = HashMap.ofMap m1
    let h2 = HashMap.ofMap m2

    let mr = (m1, m2) ||> Map.choose2V mapping
    let hr = (h1, h2) ||> HashMap.choose2V mapping

    Map.check mr hr

[<Property(EndSize = 10000)>]
let map2 (m1 : Map<TestType, ValueType>) (m2 : Map<TestType, ValueType>) (mapping : TestType -> option<ValueType> -> option<ValueType> -> ValueType2)  =
    let h1 = HashMap.ofMap m1
    let h2 = HashMap.ofMap m2

    let mr = (m1, m2) ||> Map.map2 mapping
    let hr = (h1, h2) ||> HashMap.map2 mapping

    Map.check mr hr

[<Property(EndSize = 10000)>]
let map2V (m1 : Map<TestType, ValueType>) (m2 : Map<TestType, ValueType>) (mapping : TestType -> voption<ValueType> -> voption<ValueType> -> ValueType2)  =
    let h1 = HashMap.ofMap m1
    let h2 = HashMap.ofMap m2

    let mr = (m1, m2) ||> Map.map2V mapping
    let hr = (h1, h2) ||> HashMap.map2V mapping

    Map.check mr hr

[<Property(EndSize = 10000)>]
let union (m1 : Map<TestType, ValueType>) (m2 : Map<TestType, ValueType>) =
    let h1 = HashMap.ofMap m1
    let h2 = HashMap.ofMap m2

    let mr = (m1, m2) ||> Map.union
    let hr = (h1, h2) ||> HashMap.union

    Map.check mr hr

[<Property(EndSize = 10000)>]
let unionWith (m1 : Map<TestType, ValueType>) (m2 : Map<TestType, ValueType>) =
    let h1 = HashMap.ofMap m1
    let h2 = HashMap.ofMap m2

    let mr = (m1, m2) ||> Map.unionWith (fun _ a b -> a + b)
    let hr = (h1, h2) ||> HashMap.unionWith (fun _ a b -> a + b)

    Map.check mr hr

[<Property(EndSize = 10000)>]
let unionWithV (m1 : Map<TestType, ValueType>) (m2 : Map<TestType, ValueType>) (resolve : TestType -> ValueType -> ValueType -> voption<ValueType>) =
    let h1 = HashMap.ofMap m1
    let h2 = HashMap.ofMap m2

    let mr = (m1, m2) ||> Map.unionWithV resolve
    let hr = h1.UnionWithV(h2, resolve)

    Map.check mr hr