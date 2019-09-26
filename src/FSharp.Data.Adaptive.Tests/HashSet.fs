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


[<Property>]
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

    let s, e = CountingHashSet.integrate set ops
    e |> should setequal HashSet.empty<SetOperation<int>>
    s |> should setequal set

    
    let s, e = CountingHashSet.integrate s ops
    e |> should setequal (set |> Seq.map Rem |> HashSet.ofSeq)
    s |> should setequal HashSet.empty<int>

    ()


[<Property>]
let ``[HashSet] integrate drops useless removes``() =

    // value empty
    // integrate({}, {Rem 1}) = ({}, {})
    let set = HashSet.empty<int>
    let delta = HashSetDelta.ofList [Rem 1]
    let res, eff = HashSet.integrate set delta
    res |> should setequal set
    eff |> should setequal emptyDelta
  
    // delta empty
    // integrate({1}, {}) = ({1}, {})
    let set = HashSet.ofList [1]
    let delta = HashSetDelta.empty
    let res, eff = HashSet.integrate set delta
    res |> should setequal set
    eff |> should setequal emptyDelta  

    // delta small
    // integrate({2..20}, {Rem 1}) = ({2..20}, {})
    let set = HashSet.ofList [2..20]
    let delta = HashSetDelta.ofList [Rem 1]
    let res, eff = HashSet.integrate set delta
    res |> should setequal set
    eff |> should setequal emptyDelta
    
    // value small
    // integrate({21}, {Rem 1..20}) = ({21}, {})
    let set = HashSet.single 21
    let delta = HashSetDelta.ofList ([1..20] |> List.map Rem)
    let res, eff = HashSet.integrate set delta
    res |> should setequal set
    eff |> should setequal emptyDelta
    
    // similar size
    // integrate({1..20}, {Rem 21..40}) = ({1..20}, {})
    let set = HashSet.ofList [1..20]
    let delta = HashSetDelta.ofList ([21..40] |> List.map Rem)
    let res, eff = HashSet.integrate set delta
    res |> should setequal set
    eff |> should setequal emptyDelta
    

[<Property>]
let ``[HashSet] integrate drops useless adds``() =
    // integrate({1}, {Add 1}) = ({1}, {})
    let set = HashSet.single 1
    let delta = HashSetDelta.ofList [Add 1]
    let res, eff = HashSet.integrate set delta
    res |> should setequal set
    eff |> should setequal emptyDelta

    // integrate({1..20}, {Add 1}) = ({1..20}, {})
    let set = HashSet.ofList [1..20]
    let delta = HashSetDelta.ofList [Add 1]
    let res, eff = HashSet.integrate set delta
    res |> should setequal set
    eff |> should setequal emptyDelta
    
    // integrate({1..20}, {Add 1..20}) = ({1..20}, {})
    let set = HashSet.ofList [1..20]
    let delta = HashSetDelta.ofList ([1..20] |> List.map Add)
    let res, eff = HashSet.integrate set delta
    res |> should setequal set
    eff |> should setequal emptyDelta

[<Property>]
let ``[HashSet] integrate basic``() =  
    // integrate({1..19}, {Add 20}) = ({1..20}, {Add 20})
    let delta = HashSetDelta.ofList [Add 20]
    let set, eff = HashSet.integrate (HashSet.ofList [1..19]) delta
    set |> should setequal (HashSet.ofList [1..20])
    eff |> should setequal delta
    
    // integrate({1}, {Add 1..20}) = ({1..20}, {Add 2..20})
    let delta = HashSetDelta.ofList ([1..20] |> List.map Add)
    let set, eff = HashSet.integrate (HashSet.ofList [1]) delta
    set |> should setequal (HashSet.ofList [1..20])
    eff |> should setequal (HashSetDelta.ofList ([2..20] |> List.map Add))

[<Property>]
let ``[HashSet] differentiate/integrate`` (set1 : Set<int>) (set2 : Set<int>) (set3 : Set<int>) =
    let set1 = HashSet.ofSeq set1
    let set2 = HashSet.ofSeq set2
    let set3 = HashSet.ofSeq set3



    // diff(A, A) = 0
    HashSet.differentiate set1 set1 |> should setequal emptyDelta
    HashSet.differentiate set2 set2 |> should setequal emptyDelta

    // integrate(A, 0) = (A, _)
    HashSet.integrate set1 emptyDelta |> fst |> should setequal set1
    HashSet.integrate set2 emptyDelta |> fst |> should setequal set2
    
    // integrate(A, 0) = (_, 0)
    HashSet.integrate set1 emptyDelta |> snd |> should setequal emptyDelta
    HashSet.integrate set2 emptyDelta |> snd |> should setequal emptyDelta

    // integrate(A, diff(A, B)) = (A, _)
    // integrate(A, diff(A, B)) = (_, diff(A, B))
    let fw = HashSet.differentiate set1 set2
    let t2, d1 = HashSet.integrate set1 fw
    t2 |> should setequal set2
    d1 |> should setequal fw

    // diff(A, B) = -diff(B, A)
    let bw = HashSet.differentiate set2 set1
    bw.Inverse |> should setequal fw

    // diff(A, B) + diff(B, A) = 0
    HashSetDelta.combine fw bw |> should setequal emptyDelta

    let d12 = HashSet.differentiate set1 set2
    let d23 = HashSet.differentiate set2 set3
    let d31 = HashSet.differentiate set3 set1

    // diff(A, B) + diff(B, C) + diff(C, A) = 0
    HashSetDelta.combine (HashSetDelta.combine d12 d23) d31 |> should setequal emptyDelta

    // diff(A, B) + diff(B, C) = diff(A, C)
    HashSetDelta.combine d12 d23 |> should setequal d31.Inverse

[<Property>]
let ``[CountingHashSet] differentiate/integrate`` (set1 : Set<int>) (set2 : Set<int>) (set3 : Set<int>) =
    let set1 = CountingHashSet.ofSeq set1
    let set2 = CountingHashSet.ofSeq set2
    let set3 = CountingHashSet.ofSeq set3

    // integrate({}, {Rem 1}) = ({}, {})
    let delta = HashSetDelta.ofList [Rem 1]
    let set, eff = CountingHashSet.integrate CountingHashSet.empty delta
    set |> should setequal CountingHashSet.empty<int>
    eff |> should setequal emptyDelta
    
    // integrate({1}, {Add 1}) = ({1}, {})
    let delta = HashSetDelta.ofList [Add 1]
    let set, eff = CountingHashSet.integrate (CountingHashSet.single 1) delta
    set |> should setequal (CountingHashSet.single 1)
    eff |> should setequal emptyDelta

    // diff(A, A) = 0
    CountingHashSet.differentiate set1 set1 |> should setequal emptyDelta
    CountingHashSet.differentiate set2 set2 |> should setequal emptyDelta

    // integrate(A, 0) = (A, _)
    CountingHashSet.integrate set1 emptyDelta |> fst |> should setequal set1
    CountingHashSet.integrate set2 emptyDelta |> fst |> should setequal set2
    
    // integrate(A, 0) = (_, 0)
    CountingHashSet.integrate set1 emptyDelta |> snd |> should setequal emptyDelta
    CountingHashSet.integrate set2 emptyDelta |> snd |> should setequal emptyDelta

    // integrate(A, diff(A, B)) = (A, _)
    // integrate(A, diff(A, B)) = (_, diff(A, B))
    let fw = CountingHashSet.differentiate set1 set2
    let t2, d1 = CountingHashSet.integrate set1 fw
    t2 |> should setequal set2
    d1 |> should setequal fw

    // diff(A, B) = -diff(B, A)
    let bw = CountingHashSet.differentiate set2 set1
    bw.Inverse |> should setequal fw

    // diff(A, B) + diff(B, A) = 0
    HashSetDelta.combine fw bw |> should setequal emptyDelta

    let d12 = CountingHashSet.differentiate set1 set2
    let d23 = CountingHashSet.differentiate set2 set3
    let d31 = CountingHashSet.differentiate set3 set1

    // diff(A, B) + diff(B, C) + diff(C, A) = 0
    HashSetDelta.combine (HashSetDelta.combine d12 d23) d31 |> should setequal emptyDelta

    // diff(A, B) + diff(B, C) = diff(A, C)
    HashSetDelta.combine d12 d23 |> should setequal d31.Inverse


[<Property>]
let ``[CountingHashSet] integrate drops useless removes``() =

    // value empty
    // integrate({}, {Rem 1}) = ({}, {})
    let set = CountingHashSet.empty<int>
    let delta = HashSetDelta.ofList [Rem 1]
    let res, eff = CountingHashSet.integrate set delta
    res |> should setequal set
    eff |> should setequal emptyDelta
  
    // delta empty
    // integrate({1}, {}) = ({1}, {})
    let set = CountingHashSet.ofList [1]
    let delta = HashSetDelta.empty
    let res, eff = CountingHashSet.integrate set delta
    res |> should setequal set
    eff |> should setequal emptyDelta  

    // delta small
    // integrate({2..20}, {Rem 1}) = ({2..20}, {})
    let set = CountingHashSet.ofList [2..20]
    let delta = HashSetDelta.ofList [Rem 1]
    let res, eff = CountingHashSet.integrate set delta
    res |> should setequal set
    eff |> should setequal emptyDelta
    
    // value small
    // integrate({21}, {Rem 1..20}) = ({21}, {})
    let set = CountingHashSet.single 21
    let delta = HashSetDelta.ofList ([1..20] |> List.map Rem)
    let res, eff = CountingHashSet.integrate set delta
    res |> should setequal set
    eff |> should setequal emptyDelta
    
    // similar size
    // integrate({1..20}, {Rem 21..40}) = ({1..20}, {})
    let set = CountingHashSet.ofList [1..20]
    let delta = HashSetDelta.ofList ([21..40] |> List.map Rem)
    let res, eff = CountingHashSet.integrate set delta
    res |> should setequal set
    eff |> should setequal emptyDelta

[<Property>]
let ``[CountingHashSet] integrate drops useless adds``() =
    // integrate({1}, {Add 1}) = ({1}, {})
    let set = CountingHashSet.single 1
    let delta = HashSetDelta.ofList [Add 1]
    let res, eff = CountingHashSet.integrate set delta
    res |> should setequal set
    eff |> should setequal emptyDelta

    // integrate({1..20}, {Add 1}) = ({1..20}, {})
    let set = CountingHashSet.ofList [1..20]
    let delta = HashSetDelta.ofList [Add 1]
    let res, eff = CountingHashSet.integrate set delta
    res |> should setequal set
    eff |> should setequal emptyDelta
    
    // integrate({1..20}, {Add 1..20}) = ({1..20}, {})
    let set = CountingHashSet.ofList [1..20]
    let delta = HashSetDelta.ofList ([1..20] |> List.map Add)
    let res, eff = CountingHashSet.integrate set delta
    res |> should setequal set
    eff |> should setequal emptyDelta

[<Property>]
let ``[CountingHashSet] integrate basic``() =  
    // integrate({1..19}, {Add 20}) = ({1..20}, {Add 20})
    let delta = HashSetDelta.ofList [Add 20]
    let set, eff = CountingHashSet.integrate (CountingHashSet.ofList [1..19]) delta
    set |> should setequal (CountingHashSet.ofList [1..20])
    eff |> should setequal delta
    
    // integrate({1}, {Add 1..20}) = ({1..20}, {Add 2..20})
    let delta = HashSetDelta.ofList ([1..20] |> List.map Add)
    let set, eff = CountingHashSet.integrate (CountingHashSet.ofList [1]) delta
    set |> should setequal (CountingHashSet.ofList [1..20])
    eff |> should setequal (HashSetDelta.ofList ([2..20] |> List.map Add))


    
[<Property>]
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

    // no negatice refcounts!!!
    // (0 - A) = 0
    CountingHashSet.difference empty set1 |> should setequal empty


[<Property>]
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

[<Property>]
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

[<Property>]
let ``[HashSet] ofList`` (l : list<int>) =
    HashSet.toList (HashSet.ofList l) |> List.sort = Set.toList (Set.ofList l)

    
[<Property>]
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
    