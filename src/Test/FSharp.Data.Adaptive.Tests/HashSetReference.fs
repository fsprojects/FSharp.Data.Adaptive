module HashSetReference

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
    let choose (mapping : 'A -> option<'B>) (set : Set<'A>) =
        set |> Seq.choose mapping |> Set.ofSeq
        
    let chooseV (mapping : 'A -> voption<'B>) (set : Set<'A>) =
        set |> Seq.choose (fun v -> match mapping v with | ValueSome a -> Some a | ValueNone -> None) |> Set.ofSeq


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

    let tryRemove (value : 'A) (set : Set<'A>) =
        if Set.contains value set then
            Some (Set.remove value set)
        else
            None
            
    let tryRemoveV (value : 'A) (set : Set<'A>) =
        if Set.contains value set then
            ValueSome (Set.remove value set)
        else
            ValueNone

    let alter (value : 'A) (update : bool -> bool) (set : Set<'A>) =
        let o = Set.contains value set
        let n = update o
        if n && not o then Set.add value set
        elif o && not n then Set.remove value set
        else set

    let collect (mapping : 'A -> Set<'B>) (set : Set<'A>) =
        let mutable res = Set.empty
        for e in set do
            res <- Set.union res (mapping e)
        res

    let xor (a : Set<'A>) (b : Set<'A>) =
        let r = Set.union a b
        let i = Set.intersect a b
        Set.difference r i

    let overlaps (a : Set<'A>) (b : Set<'A>) =
        a |> Set.exists (fun v -> Set.contains v b)
        
    let isSubset (a : Set<'A>) (b : Set<'A>) =
        a |> Set.forall (fun v -> Set.contains v b)
        
    let isSuperset (a : Set<'A>) (b : Set<'A>) =
        isSubset b a
        
    let isProperSubset (a : Set<'A>) (b : Set<'A>) =
        Set.count a < Set.count b &&
        isSubset a b
        
    let isProperSuperset (a : Set<'A>) (b : Set<'A>) =
        Set.count a > Set.count b &&
        isSuperset a b

    let computeDelta (m1 : Set<'K>) (m2 : Set<'K>) =
        let mutable result : Map<'K, int> = Map.empty
        for kb in m2 do
            if not (Set.contains kb m1) then
                result <- Map.add kb 1 result
        for ka in m1 do
            if not (Set.contains ka m2) then
                result <- Map.add ka -1 result
        result

    let applyDelta (state : Set<'K>) (delta : Map<'K, int>) =
        let mutable state = state
        let mutable res = Map.empty
        for (KeyValue(k, d)) in delta do
            if Set.contains k state then
                if d < 0 then 
                    state <- Set.remove k state
                    res <- Map.add k -1 res
            else
                if d > 0 then
                    state <- Set.add k state
                    res <- Map.add k 1 res
        state, res

    let mapToMap (mapping : 'A -> 'T) (set : Set<'A>) =
        set |> Seq.map (fun v -> v, mapping v) |> Map.ofSeq

module Map =
    let checkDelta (m : Map<'A, int>) (delta : HashSetDelta<'A>) =
        let h = delta |> HashSetDelta.toHashMap |> HashMap.toList |> List.sortBy fst
        let s = m |> Map.toList

        h |> should equal s
        
    let check (m : Map<'A, 'B>) (h : HashMap<'A, 'B>) =
        let h = h |> HashMap.toList |> List.sortBy fst
        let s = m |> Map.toList

        h |> should equal s


type TestType = StupidHash
type TestType2 = NormalFloat

[<Test>]
let empty() =
    let s = Set.empty<TestType>
    let h = HashSet.empty<TestType>
    Set.check s h
    
[<Property(EndSize = 10000)>]
let count (s : Set<TestType>) =
    let h = HashSet.ofSeq s
    Set.check s h

    HashSet.count h |> should equal (Set.count s)
    h.Count |> should equal (Set.count s)

[<Property(EndSize = 10000)>]
let isEmpty (s : Set<TestType>) =
    let h = HashSet.ofSeq s
    Set.check s h

    HashSet.isEmpty h |> should equal (Set.isEmpty s)
    h.IsEmpty |> should equal (Set.isEmpty s)


[<Property(EndSize = 10000)>]
let single (v : TestType) =
    let s = Set.singleton v
    let h = HashSet.single v
    Set.check s h


[<Property(EndSize = 10000)>]
let ofSeq (v : array<TestType>) =
    let v = Seq.init v.Length (fun i -> v.[i])
    let s = Set.ofSeq v
    let h = HashSet.ofSeq v
    Set.check s h
    
[<Property(EndSize = 10000)>]
let ofList (v : list<TestType>) =
    let s = Set.ofList v
    let h = HashSet.ofList v
    Set.check s h
    
[<Property(EndSize = 10000)>]
let ofArray (v : array<TestType>) =
    let s = Set.ofArray v
    let h = HashSet.ofArray v
    Set.check s h
      
[<Property(EndSize = 10000)>]
let add (s : Set<TestType>) (v : TestType) =
    let h = HashSet.ofSet s

    let s1 = Set.add v s
    let h1 = HashSet.add v h

    Set.check s1 h1
    
[<Property(EndSize = 10000)>]
let remove (s : Set<TestType>) (v : TestType) =
    let h = HashSet.ofSet s

    let s1 = Set.remove v s
    let h1 = HashSet.remove v h

    Set.check s1 h1
        
[<Property(EndSize = 10000)>]
let tryRemove (s : Set<TestType>) (v : TestType) =
    let h = HashSet.ofSet s

    let s1 = Set.tryRemove v s
    let h1 = HashSet.tryRemove v h
    match s1, h1 with
    | Some s1, Some h1 -> Set.check s1 h1
    | None, None -> ()
    | s1, h1 -> failwithf "inconsistent tryRemove %0A %0A: %A vs %A" v s s1 h1

[<Property(EndSize = 10000)>]
let tryRemoveV (s : Set<TestType>) (v : TestType) =
    let h = HashSet.ofSet s

    let s1 = Set.tryRemoveV v s
    let h1 = HashSet.tryRemoveV v h
    match s1, h1 with
    | ValueSome s1, ValueSome h1 -> Set.check s1 h1
    | ValueNone, ValueNone -> ()
    | s1, h1 -> failwithf "inconsistent tryRemove %0A %0A: %A vs %A" v s s1 h1

[<Property(EndSize = 10000)>]
let alter (s : Set<TestType>) (v : TestType) (update : bool -> bool) =
    let h = HashSet.ofSet s

    let s1 = Set.alter v update s
    let h1 = HashSet.alter v update h
    Set.check s1 h1


[<Property(EndSize = 10000)>]
let iter (s : Set<TestType>)  =
    let h = HashSet.ofSet s

    let mutable sr = []
    let mutable hr = []
    s |> Set.iter (fun v -> sr <- v::sr)
    h |> HashSet.iter (fun v -> hr <- v :: hr)

    let sr = sr |> List.sort
    let hr = hr |> List.sort

    hr |> should equal sr


[<Property(EndSize = 10000)>]
let fold (s : Set<TestType>)  =
    let h = HashSet.ofSet s

    let sr = (LanguagePrimitives.GenericZero, s) ||> Set.fold (+)
    let hr = (LanguagePrimitives.GenericZero, h) ||> HashSet.fold (+)
    hr |> should equal sr


[<Property(EndSize = 10000)>]
let exists (s : Set<TestType>) (predicate : TestType -> bool) =
    let h = HashSet.ofSet s
    let sr = s |> Set.exists predicate
    let hr = h |> HashSet.exists predicate
    hr |> should equal sr

[<Property(EndSize = 10000)>]
let forall (s : Set<TestType>) (predicate : TestType -> bool) =
    let h = HashSet.ofSet s
    let sr = s |> Set.forall predicate
    let hr = h |> HashSet.forall predicate
    hr |> should equal sr

[<Property(EndSize = 10000)>]
let map (s : Set<TestType>) (mapping : TestType -> TestType2) =
    let h = HashSet.ofSet s
    let sr = s |> Set.map mapping
    let hr = h |> HashSet.map mapping
    Set.check sr hr
    
[<Property(EndSize = 10000)>]
let choose (s : Set<TestType>) (mapping : TestType -> option<TestType2>) =
    let h = HashSet.ofSet s
    let sr = s |> Set.choose mapping
    let hr = h |> HashSet.choose mapping
    Set.check sr hr
    
[<Property(EndSize = 10000)>]
let chooseV (s : Set<TestType>) (mapping : TestType -> voption<TestType2>) =
    let h = HashSet.ofSet s
    let sr = s |> Set.chooseV mapping
    let hr = h |> HashSet.chooseV mapping
    Set.check sr hr
    
[<Property(EndSize = 10000)>]
let filter (s : Set<TestType>) (predicate : TestType -> bool) =
    let h = HashSet.ofSet s
    let sr = s |> Set.filter predicate
    let hr = h |> HashSet.filter predicate
    Set.check sr hr

[<Property(EndSize = 1000)>]
let collect (s : Set<TestType>) (setMapping : TestType -> Set<TestType2>) =
    let h = HashSet.ofSet s
    let hashMapping v = setMapping v |> HashSet.ofSet
    let sr = s |> Set.collect setMapping
    let hr = h |> HashSet.collect hashMapping
    Set.check sr hr
    
[<Property(EndSize = 10000)>]
let contains (s : Set<TestType>) (v : TestType) =
    let h = HashSet.ofSet s

    let sr = s |> Set.contains v
    let hr = h |> HashSet.contains v
    hr |> should equal sr

[<Property(EndSize = 10000)>]
let toSeq (s : Set<TestType>) (v : TestType) =
    let h = HashSet.ofSet s

    let sr = s |> Set.toList
    let hr = h |> HashSet.toSeq |> Seq.toList |> List.sort
    hr |> should equal sr

[<Property(EndSize = 10000)>]
let toList (s : Set<TestType>) (v : TestType) =
    let h = HashSet.ofSet s

    let sr = s |> Set.toList
    let hr = h |> HashSet.toList |> List.sort
    hr |> should equal sr

[<Property(EndSize = 10000)>]
let toArray (s : Set<TestType>) (v : TestType) =
    let h = HashSet.ofSet s

    let sr = s |> Set.toList
    let hr = h |> HashSet.toArray |> Array.toList |> List.sort
    hr |> should equal sr
    
[<Property(EndSize = 10000)>]
let toSet (s : Set<TestType>) (v : TestType) =
    let hr = HashSet.ofSet s |> HashSet.toSet
    hr |> should equal s
    
[<Property(EndSize = 10000)>]
let union (s1 : Set<TestType>) (s2 : Set<TestType>) =
    let h1 = HashSet.ofSet s1
    let h2 = HashSet.ofSet s2

    let sr = Set.union s1 s2
    let hr = HashSet.union h1 h2
    Set.check sr hr
    
[<Property(EndSize = 10000)>]
let intersect (s1 : Set<TestType>) (s2 : Set<TestType>) =
    let h1 = HashSet.ofSet s1
    let h2 = HashSet.ofSet s2

    let sr = Set.intersect s1 s2
    let hr = HashSet.intersect h1 h2
    Set.check sr hr
    
[<Property(EndSize = 10000)>]
let difference (s1 : Set<TestType>) (s2 : Set<TestType>) =
    let h1 = HashSet.ofSet s1
    let h2 = HashSet.ofSet s2

    let sr = Set.difference s1 s2
    let hr = HashSet.difference h1 h2
    Set.check sr hr
    
    let sr2 = Set.difference s2 s1
    let hr2 = HashSet.difference h2 h1
    Set.check sr2 hr2
    
[<Property(EndSize = 10000)>]
let xor (s1 : Set<TestType>) (s2 : Set<TestType>) =
    let h1 = HashSet.ofSet s1
    let h2 = HashSet.ofSet s2

    let sr = Set.xor s1 s2
    let hr = HashSet.xor h1 h2
    Set.check sr hr
    
[<Property(EndSize = 10000)>]
let equals (s1 : Set<TestType>) (s2 : Set<TestType>) =
    let h1 = HashSet.ofSet s1
    let h2 = HashSet.ofSet s2
    
    (h1 = h1) |> should be True
    (h2 = h2) |> should be True
    (h1 = h2) |> should equal (s1 = s2)
    
[<Property(EndSize = 10000)>]
let overlaps (s1 : Set<TestType>) (s2 : Set<TestType>) =
    let h1 = HashSet.ofSet s1
    let h2 = HashSet.ofSet s2
    HashSet.overlaps h1 h2 |> should equal (Set.overlaps s1 s2)
    
[<Property(EndSize = 10000)>]
let isSubset (s1 : Set<TestType>) (s2 : Set<TestType>) =
    let h1 = HashSet.ofSet s1
    let h2 = HashSet.ofSet s2
    let hr = HashSet.isSubset h1 h2
    let sr = Set.isSubset s1 s2
    HashSet.isSubset h1 h1 |> should be True
    hr |> should equal sr
    
[<Property(EndSize = 10000)>]
let isProperSubset (s1 : Set<TestType>) (s2 : Set<TestType>) =
    let h1 = HashSet.ofSet s1
    let h2 = HashSet.ofSet s2
    HashSet.isProperSubset h1 h1 |> should be False
    HashSet.isProperSubset h1 h2 |> should equal (Set.isProperSubset s1 s2)
    
[<Property(EndSize = 10000)>]
let isSuperset (s1 : Set<TestType>) (s2 : Set<TestType>) =
    let h1 = HashSet.ofSet s1
    let h2 = HashSet.ofSet s2
    HashSet.isSuperset h1 h1 |> should be True
    HashSet.isSuperset h1 h2 |> should equal (Set.isSuperset s1 s2)
    
[<Property(EndSize = 10000)>]
let isProperSuperset (s1 : Set<TestType>) (s2 : Set<TestType>) =
    let h1 = HashSet.ofSet s1
    let h2 = HashSet.ofSet s2
    HashSet.isProperSuperset h1 h1 |> should be False
    HashSet.isProperSuperset h1 h2 |> should equal (Set.isProperSuperset s1 s2)
    
[<Property(EndSize = 10000)>]
let computeDelta (s1 : Set<TestType>) (s2 : Set<TestType>) =
    let h1 = HashSet.ofSet s1
    let h2 = HashSet.ofSet s2
    let hd = HashSet.computeDelta h1 h2
    let sd = Set.computeDelta s1 s2
    Map.checkDelta sd hd
    
[<Property(EndSize = 10000)>]
let applyDelta (s : Set<TestType>) (d : Map<TestType, int>) =
    let d = d |> Map.filter (fun _ v -> v <> 0)
    let h = HashSet.ofSet s
    let hd = d |> Map.toList |> HashMap.ofList |> HashSetDelta.ofHashMap

    let h1, he = HashSet.applyDelta h hd
    let s1, se = Set.applyDelta s d
    Map.checkDelta se he
    Set.check s1 h1
    
[<Property(EndSize = 10000)>]
let mapToMap (s : Set<TestType>) (mapping : TestType -> TestType2) =
    let h = HashSet.ofSet s

    let h1 = h.MapToMap(mapping)
    let s1 = s |> Set.mapToMap mapping

    Map.check s1 h1