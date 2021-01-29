module HashSet

open System
open FsUnit
open FsCheck
open FsCheck.NUnit
open FSharp.Data.Adaptive
open FSharp.Data.Traceable

module List =
    let all (l : list<bool>) =
        l |> List.fold (&&) true

[<CustomEquality; CustomComparison>]
type StupidHash = { value : int } with
    
    interface IComparable with
        member x.CompareTo o =
            match o with
                | :? StupidHash as o -> compare x.value o.value
                | _ -> failwith "cannot compare"

    override x.GetHashCode() = x.value % 2
    override x.Equals o =   
        match o with
            | :? StupidHash as o -> x.value = o.value
            | _ -> false

/// Avoid obj defaulting
let emptyDelta : HashSetDelta<int> = HashSetDelta.empty


[<Property(EndSize = 10000)>]
let ``[CountingHashSet] ref counts`` (input : Set<int>) =
    let set = 
        input 
        |> Seq.map (fun v -> v, 2) 
        |> HashMap.ofSeq
        |> CountingHashSet.ofHashMap

    let direct =
        CountingHashSet.ofSeq input

    input 
    |> Set.fold (fun s v -> CountingHashSet.remove v s) set
    |> should setequal set

    input 
    |> Set.fold (fun s v -> CountingHashSet.remove v (CountingHashSet.remove v s)) set
    |> should setequal CountingHashSet.empty<int>

    let ops =
        set |> Seq.map Rem |> HashSetDelta.ofSeq

    let s, e = CountingHashSet.applyDelta set ops
    e |> should setequal HashSet.empty<SetOperation<int>>
    s |> should setequal set

    
    let s, e = CountingHashSet.applyDelta s ops
    e |> should setequal (set |> Seq.map Rem |> HashSet.ofSeq)
    s |> should setequal HashSet.empty<int>

    ()


[<Property(EndSize = 10000)>]
let ``[HashSet] applyDelta drops useless removes``() =

    // value empty
    // applyDelta({}, {Rem 1}) = ({}, {})
    let set = HashSet.empty<int>
    let delta = HashSetDelta.ofList [Rem 1]
    let res, eff = HashSet.applyDelta set delta
    res |> should setequal set
    eff |> should setequal emptyDelta
  
    // delta empty
    // applyDelta({1}, {}) = ({1}, {})
    let set = HashSet.ofList [1]
    let delta = HashSetDelta.empty
    let res, eff = HashSet.applyDelta set delta
    res |> should setequal set
    eff |> should setequal emptyDelta  

    // delta small
    // applyDelta({2..20}, {Rem 1}) = ({2..20}, {})
    let set = HashSet.ofList [2..20]
    let delta = HashSetDelta.ofList [Rem 1]
    let res, eff = HashSet.applyDelta set delta
    res |> should setequal set
    eff |> should setequal emptyDelta
    
    // value small
    // applyDelta({21}, {Rem 1..20}) = ({21}, {})
    let set = HashSet.single 21
    let delta = HashSetDelta.ofList ([1..20] |> List.map Rem)
    let res, eff = HashSet.applyDelta set delta
    res |> should setequal set
    eff |> should setequal emptyDelta
    
    // similar size
    // applyDelta({1..20}, {Rem 21..40}) = ({1..20}, {})
    let set = HashSet.ofList [1..20]
    let delta = HashSetDelta.ofList ([21..40] |> List.map Rem)
    let res, eff = HashSet.applyDelta set delta
    res |> should setequal set
    eff |> should setequal emptyDelta
    

[<Property(EndSize = 10000)>]
let ``[HashSet] applyDelta drops useless adds``() =
    // applyDelta({1}, {Add 1}) = ({1}, {})
    let set = HashSet.single 1
    let delta = HashSetDelta.ofList [Add 1]
    let res, eff = HashSet.applyDelta set delta
    res |> should setequal set
    eff |> should setequal emptyDelta

    // applyDelta({1..20}, {Add 1}) = ({1..20}, {})
    let set = HashSet.ofList [1..20]
    let delta = HashSetDelta.ofList [Add 1]
    let res, eff = HashSet.applyDelta set delta
    res |> should setequal set
    eff |> should setequal emptyDelta
    
    // applyDelta({1..20}, {Add 1..20}) = ({1..20}, {})
    let set = HashSet.ofList [1..20]
    let delta = HashSetDelta.ofList ([1..20] |> List.map Add)
    let res, eff = HashSet.applyDelta set delta
    res |> should setequal set
    eff |> should setequal emptyDelta

[<Property(EndSize = 10000)>]
let ``[HashSet] applyDelta basic``() =  
    // applyDelta({1..19}, {Add 20}) = ({1..20}, {Add 20})
    let delta = HashSetDelta.ofList [Add 20]
    let set, eff = HashSet.applyDelta (HashSet.ofList [1..19]) delta
    set |> should setequal (HashSet.ofList [1..20])
    eff |> should setequal delta
    
    // applyDelta({1}, {Add 1..20}) = ({1..20}, {Add 2..20})
    let delta = HashSetDelta.ofList ([1..20] |> List.map Add)
    let set, eff = HashSet.applyDelta (HashSet.ofList [1]) delta
    set |> should setequal (HashSet.ofList [1..20])
    eff |> should setequal (HashSetDelta.ofList ([2..20] |> List.map Add))

[<Property(EndSize = 10000)>]
let ``[HashSet] computeDelta/applyDelta`` (set1 : Set<int>) (set2 : Set<int>) (set3 : Set<int>) =
    let set1 = HashSet.ofSeq set1
    let set2 = HashSet.ofSeq set2
    let set3 = HashSet.ofSeq set3



    // diff(A, A) = 0
    HashSet.computeDelta set1 set1 |> should setequal emptyDelta
    HashSet.computeDelta set2 set2 |> should setequal emptyDelta

    // applyDelta(A, 0) = (A, _)
    HashSet.applyDelta set1 emptyDelta |> fst |> should setequal set1
    HashSet.applyDelta set2 emptyDelta |> fst |> should setequal set2
    
    // applyDelta(A, 0) = (_, 0)
    HashSet.applyDelta set1 emptyDelta |> snd |> should setequal emptyDelta
    HashSet.applyDelta set2 emptyDelta |> snd |> should setequal emptyDelta

    // applyDelta(A, diff(A, B)) = (A, _)
    // applyDelta(A, diff(A, B)) = (_, diff(A, B))
    let fw = HashSet.computeDelta set1 set2
    let t2, d1 = HashSet.applyDelta set1 fw
    t2 |> should setequal set2
    d1 |> should setequal fw

    // diff(A, B) = -diff(B, A)
    let bw = HashSet.computeDelta set2 set1
    bw.Inverse |> should setequal fw

    // diff(A, B) + diff(B, A) = 0
    HashSetDelta.combine fw bw |> should setequal emptyDelta

    let d12 = HashSet.computeDelta set1 set2
    let d23 = HashSet.computeDelta set2 set3
    let d31 = HashSet.computeDelta set3 set1

    // diff(A, B) + diff(B, C) + diff(C, A) = 0
    HashSetDelta.combine (HashSetDelta.combine d12 d23) d31 |> should setequal emptyDelta

    // diff(A, B) + diff(B, C) = diff(A, C)
    HashSetDelta.combine d12 d23 |> should setequal d31.Inverse

[<Property(EndSize = 10000)>]
let ``[CountingHashSet] computeDelta/applyDelta`` (set1 : Set<int>) (set2 : Set<int>) (set3 : Set<int>) =
    let set1 = CountingHashSet.ofSeq set1
    let set2 = CountingHashSet.ofSeq set2
    let set3 = CountingHashSet.ofSeq set3

    // applyDelta({}, {Rem 1}) = ({}, {})
    let delta = HashSetDelta.ofList [Rem 1]
    let set, eff = CountingHashSet.applyDelta CountingHashSet.empty delta
    set |> should setequal CountingHashSet.empty<int>
    eff |> should setequal emptyDelta
    
    // applyDelta({1}, {Add 1}) = ({1}, {})
    let delta = HashSetDelta.ofList [Add 1]
    let set, eff = CountingHashSet.applyDelta (CountingHashSet.single 1) delta
    set |> should setequal (CountingHashSet.single 1)
    eff |> should setequal emptyDelta

    // diff(A, A) = 0
    CountingHashSet.computeDelta set1 set1 |> should setequal emptyDelta
    CountingHashSet.computeDelta set2 set2 |> should setequal emptyDelta

    // applyDelta(A, 0) = (A, _)
    CountingHashSet.applyDelta set1 emptyDelta |> fst |> should setequal set1
    CountingHashSet.applyDelta set2 emptyDelta |> fst |> should setequal set2
    
    // applyDelta(A, 0) = (_, 0)
    CountingHashSet.applyDelta set1 emptyDelta |> snd |> should setequal emptyDelta
    CountingHashSet.applyDelta set2 emptyDelta |> snd |> should setequal emptyDelta

    // applyDelta(A, diff(A, B)) = (A, _)
    // applyDelta(A, diff(A, B)) = (_, diff(A, B))
    let fw = CountingHashSet.computeDelta set1 set2
    let t2, d1 = CountingHashSet.applyDelta set1 fw
    t2 |> should setequal set2
    d1 |> should setequal fw

    // diff(A, B) = -diff(B, A)
    let bw = CountingHashSet.computeDelta set2 set1
    bw.Inverse |> should setequal fw

    // diff(A, B) + diff(B, A) = 0
    HashSetDelta.combine fw bw |> should setequal emptyDelta

    let d12 = CountingHashSet.computeDelta set1 set2
    let d23 = CountingHashSet.computeDelta set2 set3
    let d31 = CountingHashSet.computeDelta set3 set1

    // diff(A, B) + diff(B, C) + diff(C, A) = 0
    HashSetDelta.combine (HashSetDelta.combine d12 d23) d31 |> should setequal emptyDelta

    // diff(A, B) + diff(B, C) = diff(A, C)
    HashSetDelta.combine d12 d23 |> should setequal d31.Inverse


[<Property(EndSize = 10000)>]
let ``[CountingHashSet] applyDelta drops useless removes``() =

    // value empty
    // applyDelta({}, {Rem 1}) = ({}, {})
    let set = CountingHashSet.empty<int>
    let delta = HashSetDelta.ofList [Rem 1]
    let res, eff = CountingHashSet.applyDelta set delta
    res |> should setequal set
    eff |> should setequal emptyDelta
  
    // delta empty
    // applyDelta({1}, {}) = ({1}, {})
    let set = CountingHashSet.ofList [1]
    let delta = HashSetDelta.empty
    let res, eff = CountingHashSet.applyDelta set delta
    res |> should setequal set
    eff |> should setequal emptyDelta  

    // delta small
    // applyDelta({2..20}, {Rem 1}) = ({2..20}, {})
    let set = CountingHashSet.ofList [2..20]
    let delta = HashSetDelta.ofList [Rem 1]
    let res, eff = CountingHashSet.applyDelta set delta
    res |> should setequal set
    eff |> should setequal emptyDelta
    
    // value small
    // applyDelta({21}, {Rem 1..20}) = ({21}, {})
    let set = CountingHashSet.single 21
    let delta = HashSetDelta.ofList ([1..20] |> List.map Rem)
    let res, eff = CountingHashSet.applyDelta set delta
    res |> should setequal set
    eff |> should setequal emptyDelta
    
    // similar size
    // applyDelta({1..20}, {Rem 21..40}) = ({1..20}, {})
    let set = CountingHashSet.ofList [1..20]
    let delta = HashSetDelta.ofList ([21..40] |> List.map Rem)
    let res, eff = CountingHashSet.applyDelta set delta
    res |> should setequal set
    eff |> should setequal emptyDelta

[<Property(EndSize = 10000)>]
let ``[CountingHashSet] applyDelta drops useless adds``() =
    // applyDelta({1}, {Add 1}) = ({1}, {})
    let set = CountingHashSet.single 1
    let delta = HashSetDelta.ofList [Add 1]
    let res, eff = CountingHashSet.applyDelta set delta
    res |> should setequal set
    eff |> should setequal emptyDelta

    // applyDelta({1..20}, {Add 1}) = ({1..20}, {})
    let set = CountingHashSet.ofList [1..20]
    let delta = HashSetDelta.ofList [Add 1]
    let res, eff = CountingHashSet.applyDelta set delta
    res |> should setequal set
    eff |> should setequal emptyDelta
    
    // applyDelta({1..20}, {Add 1..20}) = ({1..20}, {})
    let set = CountingHashSet.ofList [1..20]
    let delta = HashSetDelta.ofList ([1..20] |> List.map Add)
    let res, eff = CountingHashSet.applyDelta set delta
    res |> should setequal set
    eff |> should setequal emptyDelta

[<Property(EndSize = 10000)>]
let ``[CountingHashSet] applyDelta basic``() =  
    // applyDelta({1..19}, {Add 20}) = ({1..20}, {Add 20})
    let delta = HashSetDelta.ofList [Add 20]
    let set, eff = CountingHashSet.applyDelta (CountingHashSet.ofList [1..19]) delta
    set |> should setequal (CountingHashSet.ofList [1..20])
    eff |> should setequal delta
    
    // applyDelta({1}, {Add 1..20}) = ({1..20}, {Add 2..20})
    let delta = HashSetDelta.ofList ([1..20] |> List.map Add)
    let set, eff = CountingHashSet.applyDelta (CountingHashSet.ofList [1]) delta
    set |> should setequal (CountingHashSet.ofList [1..20])
    eff |> should setequal (HashSetDelta.ofList ([2..20] |> List.map Add))


    
[<Property(EndSize = 10000)>]
let ``[CountingHashSet] basic properties`` (fset1 : Set<int>) (fset2 : Set<int>) =
    let empty : CountingHashSet<int> = CountingHashSet.empty
    let set1 = CountingHashSet.ofSeq fset1
    let set2 = CountingHashSet.ofSeq fset2
    
    let rand = System.Random()
    let notContained = 
        Seq.initInfinite (fun _ -> rand.Next()) 
        |> Seq.find (fun v -> not (Set.contains v fset1) && not (Set.contains v fset2))

    // union works
    CountingHashSet.union set1 set2 |> should setequal (Set.union fset1 fset2)

    // intersect works
    CountingHashSet.intersect set1 set2  |> should setequal (Set.intersect fset1 fset2)

    // difference works
    CountingHashSet.difference set1 set2 |> should setequal (Set.difference fset1 fset2)

    // add works
    CountingHashSet.add notContained set1 
    |> CountingHashSet.contains notContained 
    |> should be True
    
    // add maintains count
    CountingHashSet.add notContained set1 
    |> CountingHashSet.count
    |> should equal (set1.Count + 1)

    // remove works
    CountingHashSet.add notContained set1
    |> CountingHashSet.remove  notContained
    |> CountingHashSet.contains notContained 
    |> should be False

    
    // remove maintains count
    CountingHashSet.add notContained set1
    |> CountingHashSet.remove  notContained
    |> CountingHashSet.count
    |> should equal set1.Count

    // set semantics
    // (A + A) = A
    let test = CountingHashSet.union set1 set1
    test |> should setequal set1

    // (A + A) - A = A
    CountingHashSet.difference test set1 
    |> should setequal set1
    
    // (A + A) - A - A = 0
    CountingHashSet.difference (CountingHashSet.difference test set1) set1 
    |> should setequal empty

    // no negative refcounts!!!
    // (0 - A) = 0
    CountingHashSet.difference empty set1 |> should setequal empty
   
   
[<Property(EndSize = 10000)>]
let ``[HashSet] union`` (fset1 : Set<int>) (fset2 : Set<int>) =
    let empty : HashSet<int> = HashSet.empty
    let set1 = HashSet.ofSeq fset1
    let set2 = HashSet.ofSeq fset2
    HashSet.union set1 set2 |> should setequal (Set.union fset1 fset2)

    // A + A = A
    HashSet.union set1 set1 |> should setequal set1
    
    // 0 + A = A
    HashSet.union empty set1 |> should setequal set1
    
    // A + 0 = A
    HashSet.union set1 empty |> should setequal set1
    
    // A + B = B + A
    HashSet.union set1 set2 |> should setequal (Set.union fset2 fset1)

[<Property(EndSize = 10000)>]
let ``[HashSet] difference`` (fset1 : Set<int>) (fset2 : Set<int>) =
    let empty : HashSet<int> = HashSet.empty
    let set1 = HashSet.ofSeq fset1
    let set2 = HashSet.ofSeq fset2
    let a = HashSet.difference set1 set2 
    let b = Set.difference fset1 fset2
    a |> should setequal b

    // A - A = 0
    HashSet.difference set1 set1 |> should setequal empty
    
    // 0 - A = 0
    HashSet.difference empty set1 |> should setequal empty
    
    // A - 0 = A
    HashSet.difference set1 empty |> should setequal set1
    
[<Property(EndSize = 10000)>]
let ``[HashSet] intersect`` (fset1 : Set<int>) (fset2 : Set<int>) =
    let empty : HashSet<int> = HashSet.empty
    let set1 = HashSet.ofSeq fset1
    let set2 = HashSet.ofSeq fset2
    HashSet.intersect set1 set2 |> should setequal (Set.intersect fset1 fset2)

    // A ^ A = A
    HashSet.intersect set1 set1 |> should setequal set1
    
    // 0 ^ A = 0
    HashSet.intersect empty set1 |> should setequal empty
    
    // A ^ 0 = 0
    HashSet.intersect set1 empty |> should setequal empty

[<Property(EndSize = 10000)>]
let ``[HashSet] xor`` (fset1 : Set<int>) (fset2 : Set<int>) =
    let empty : HashSet<int> = HashSet.empty
    let set1 = HashSet.ofSeq fset1
    let set2 = HashSet.ofSeq fset2

    let inline xor (a : Set<int>) (b : Set<int>) =
        Set.difference (Set.union a b) (Set.intersect a b)

    HashSet.xor set1 set2 |> should setequal (xor fset1 fset2)

    // A x A = 0
    HashSet.xor set1 set1 |> should setequal empty
    
    // 0 x A = A
    HashSet.xor empty set1 |> should setequal set1
    
    // A x 0 = A
    HashSet.xor set1 empty |> should setequal set1
    
    // A x B = B x A
    HashSet.xor set1 set2 |> should setequal (xor fset2 fset1)



[<Property(EndSize = 10000)>]
let ``[HashSet] count`` (l : Set<int>) (a : int)  =
    not (Set.contains a l) ==> lazy (
        let set = l |> Set.toList |> HashSet.ofList
        let setWithA = HashSet.add a set

        List.all [
            HashSet.count HashSet.empty = 0
            HashSet.count setWithA = HashSet.count set + 1
            HashSet.count (HashSet.remove a setWithA) = HashSet.count set
            HashSet.count set = l.Count
            HashSet.count (HashSet.union set set) = HashSet.count set
            HashSet.count (HashSet.union set setWithA) = HashSet.count setWithA
            HashSet.count (HashSet.difference setWithA set) = 1
            HashSet.count (HashSet.intersect setWithA set) = HashSet.count set
            HashSet.count (HashSet.map id set) = HashSet.count set
            HashSet.count (HashSet.filter (fun _ -> true) set) = HashSet.count set
            HashSet.count (HashSet.filter (fun _ -> false) set) = 0
            HashSet.count (HashSet.choose Some set) = HashSet.count set
            HashSet.count (HashSet.choose (fun _ -> None) set) = 0
            HashSet.count (HashSet.choose (fun _ -> Some 1) setWithA) = 1
            HashSet.count (HashSet.alter a (fun _ -> false) setWithA) = HashSet.count set
            HashSet.count (HashSet.alter a (fun _ -> true) setWithA) = HashSet.count setWithA
        ]
    )

[<Property(EndSize = 10000)>]
let ``[HashSet] contains`` (l : Set<int>) (a : int)  =
    not (Set.contains a l) ==> lazy (
        let set = l |> Set.toList |> HashSet.ofList
        let setWithA = HashSet.add a set
        
        List.all [
            HashSet.contains a setWithA
            HashSet.contains a set |> not
            HashSet.contains a (HashSet.add a setWithA) 
            HashSet.contains a (HashSet.add a set)
            HashSet.contains a (HashSet.remove a setWithA) |> not
            HashSet.contains a (HashSet.union set setWithA)
            HashSet.contains a (HashSet.difference setWithA set)
            HashSet.contains a (HashSet.intersect setWithA set) |> not
            HashSet.contains a (HashSet.alter a (fun o -> true) setWithA)
            HashSet.contains a (HashSet.alter a (fun o -> false) setWithA) |> not
            HashSet.contains a (HashSet.choose Some setWithA)
            HashSet.contains a (HashSet.choose (fun v -> None) setWithA) |> not
            HashSet.contains 7 (HashSet.choose (fun v -> Some 7) setWithA)
            HashSet.contains a (HashSet.filter (fun v -> true) setWithA)
            HashSet.contains a (HashSet.filter (fun v -> false) setWithA) |> not

        ]

    )

[<Property(EndSize = 10000)>]
let ``[HashSet] ofList`` (l : list<int>) =
    HashSet.toList (HashSet.ofList l) |> List.sort = Set.toList (Set.ofList l)
    
[<Property(EndSize = 10000)>]
let ``[HashSet] enumerator correct`` (m : Set<int>) =
    let h = HashSet.ofSeq m

    h |> Seq.toList |> should equal (HashSet.toList h)
    h |> Seq.toList |> Seq.sort |> should equal (Set.toList m)

    
[<Property(EndSize = 10000)>]
let ``[HashSet] equality`` (h0 : StupidHash) =
    let h1 = { value = h0.value + 1 }
    let h2 = { value = h0.value + 2 }
    let h3 = { value = h0.value + 3 }

    let a = HashSet.empty |> HashSet.add h0 |> HashSet.add h1 |> HashSet.add h2 |> HashSet.add h3
    let b = HashSet.empty |> HashSet.add h1 |> HashSet.add h2 |> HashSet.add h3 |> HashSet.add h0
    let c = HashSet.empty |> HashSet.add h2 |> HashSet.add h3 |> HashSet.add h0 |> HashSet.add h1
    let d = HashSet.empty |> HashSet.add h3 |> HashSet.add h0 |> HashSet.add h1 |> HashSet.add h2
    let e = d |> HashSet.add h3
    
    let x = d |> HashSet.add { value = h0.value + 4 }

    let ah = a.GetHashCode()
    let bh = b.GetHashCode()
    let ch = c.GetHashCode()
    let dh = d.GetHashCode()
    let eh = e.GetHashCode()

    a = a && b = b && c = c && d = d && x = x && e = e &&

    a = b && a = c && a = d && a = e && b = c && b = d && b = e && c = d && c = e && d = e &&
    b = a && c = a && d = a && e = a && c = b && d = b && e = b && d = c && e = c && e = d &&

    ah = bh && bh = ch && ch = dh && dh = eh &&

    x <> a && x <> b && x <> c && x <> d && x <> e &&

    a.Count = 4 && b.Count = 4 && c.Count = 4 && d.Count = 4 && e.Count = 4 &&
    x.Count = 5
    