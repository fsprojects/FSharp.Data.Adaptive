module IndexList


open System
open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit
open FSharp.Data.Adaptive
open FSharp.Data.Traceable


[<Property(EndSize = 1024, Verbose = true)>]
let ``[Index] maintaining order``(lr : list<bool>) =
    let min = Index.zero
    let max = Index.after min
    let mutable l = min
    let mutable r = max

    let all =
        lr |> List.map (fun left ->
            if left then 
                r <- Index.between l r
                r
            else 
                l <- Index.between l r
                l
        )
        
    let rec check (ii : list<bool * Index>) (l : Index) (r : Index) =
        match ii with
        | [] -> ()
        | (left, i) :: rest ->
            i |> should be (greaterThan l)
            i |> should be (lessThan r)
            if left then check rest l i
            else check rest i r

    check (List.zip lr all) min max

[<Property(EndSize = 10000)>]
let ``[IndexList] creation`` (l : list<int>) =
    let test = l |> IndexList.ofList |> IndexList.toList
    test |> should equal l

[<Property(EndSize = 10000)>]
let ``[IndexList] count`` (l : list<int>) =
    let test = l |> IndexList.ofList
    test.Count |> should equal (List.length l)
    
[<Property(EndSize = 10000)>]
let ``[IndexList] GetSlice`` (a : int) (b : int) (c : int) (l : list<int>) =
    let l = a :: b :: c :: l
    let test = l |> IndexList.ofList

    test.[1..2] |> IndexList.toList |> should equal l.[1..2]
    test.[1..] |> IndexList.toList |> should equal l.[1..]
    test.[..2] |> IndexList.toList |> should equal l.[..2]

    let idx = test.Count / 2
    test.[idx..idx] |> IndexList.toList |> should equal [l.[idx]]
        
[<Property(EndSize = 10000)>]
let ``[IndexList] GetSliceIndex`` (a : int) (b : int) (c : int) (l : list<int>) =
    let l = a :: b :: c :: l
    let test = l |> IndexList.ofList

    let indices = test |> IndexList.toSeqIndexed |> Seq.map fst |> Seq.toArray

    let a = 1
    let b = 2
    test.[indices.[a] .. indices.[b]] |> IndexList.toList |> should equal l.[a..b]
    test.[indices.[a] .. ] |> IndexList.toList |> should equal l.[a..]
    test.[ .. indices.[b]] |> IndexList.toList |> should equal l.[..b]
    
    let idx = test.Count / 2
    test.[indices.[idx] .. indices.[idx]] |> IndexList.toList |> should equal [l.[idx]]

    
[<Property(EndSize = 10000)>]
let ``[IndexList] skip`` (a : int) (b : int) (c : int) (l : list<int>) =
    let l = a :: b :: c :: l
    let test = l |> IndexList.ofList

    IndexList.skip 2 test |> IndexList.toList |> should equal (List.skip 2 l)
    IndexList.skip 0 test |> IndexList.toList |> should equal l
    IndexList.skip 1 test |> IndexList.toList |> should equal (List.skip 1 l)
    
        
[<Property(EndSize = 10000)>]
let ``[IndexList] take`` (a : int) (b : int) (c : int) (l : list<int>) =
    let l = a :: b :: c :: l
    let test = l |> IndexList.ofList

    let cnt = List.length l / 2

    IndexList.take cnt test |> IndexList.toList |> should equal (List.take cnt l)
    IndexList.take 0 test |> IndexList.toList |> should equal List.empty<int>
    IndexList.take 2 test |> IndexList.toList |> should equal (List.take 2 l)
    




[<Property(EndSize = 10000)>]
let ``[IndexList] append`` (l : list<int>) (r : list<int>) =
    let ll = IndexList.ofList l
    let rl = IndexList.ofList r

    IndexList.append ll rl 
    |> IndexList.toList
    |> should equal (List.append l r)

[<Property(EndSize = 1000)>]
let ``[IndexList] take/skip`` (l : list<int>) =
    let ll = IndexList.ofList l

    let c = List.length l

    let s1 = c / 2
    let s2 = c / 3
    IndexList.skip c ll |> IndexList.toList |> should equal List.empty<int>
    IndexList.skip 0 ll |> IndexList.toList |> should equal l
    IndexList.skip s1 ll |> IndexList.toList |> should equal (List.skip s1 l)
    IndexList.skip s2 ll |> IndexList.toList |> should equal (List.skip s2 l)
    
    IndexList.take c ll |> IndexList.toList |> should equal l
    IndexList.take 0 ll |> IndexList.toList |> should equal List.empty<int>
    IndexList.take s1 ll |> IndexList.toList |> should equal (List.take s1 l)
    IndexList.take s2 ll |> IndexList.toList |> should equal (List.take s2 l)

[<Property(EndSize = 1000)>]
let ``[IndexList] sort`` (l : list<int>) =
    let ll = IndexList.ofList l

    let cmp (a : int) (b : int) =
        b - a

    ll |> IndexList.sortBy id |> IndexList.toList |> should equal (List.sortBy id l)
    ll |> IndexList.sortByDescending id |> IndexList.toList |> should equal (List.sortByDescending id l)
    ll |> IndexList.sortWith cmp |> IndexList.toList |> should equal (List.sortWith cmp l)
    ll |> IndexList.sort |> IndexList.toList |> should equal (List.sort l)
    ll |> IndexList.sortDescending |> IndexList.toList |> should equal (List.sortDescending l)
    
[<Property(EndSize = 1000)>]
let ``[IndexList] sum/average`` (h : NormalFloat) (l : list<NormalFloat>) =
    let l = h :: l |> List.map float
    let ll = IndexList.ofList l
    let mapping (v : float) = v + 1.0
    ll |> IndexList.sum |> should equal (List.sum l)
    ll |> IndexList.average |> should equal (List.average l)
    ll |> IndexList.sumBy mapping |> should equal (List.sumBy mapping l)
    ll |> IndexList.averageBy mapping |> should equal (List.averageBy mapping l)
    
      
[<Property(EndSize = 1000)>]
let ``[IndexList] unzip`` (l : list<int * float>) =
    let a, b = List.unzip l
    let la, lb = IndexList.unzip (IndexList.ofList l)
      
    la |> IndexList.toList |> should equal a
    lb |> IndexList.toList |> should equal b
    
      
[<Property(EndSize = 1000)>]
let ``[IndexList] unzip3`` (l : list<int * float * string>) =
    let a, b, c = List.unzip3 l
    let la, lb, lc = IndexList.unzip3 (IndexList.ofList l)
      
    la |> IndexList.toList |> should equal a
    lb |> IndexList.toList |> should equal b
    lc |> IndexList.toList |> should equal c

[<Property(EndSize = 1000)>]
let ``[IndexList] rev`` (l : list<float>) =
    let ll = IndexList.ofList l

    let rl = IndexList.rev ll
    ll.MinIndex |> should equal rl.MinIndex
    ll.MaxIndex |> should equal rl.MaxIndex

    ll |> IndexList.rev |> IndexList.toList |> should equal (List.rev l)
    
[<Property(EndSize = 10000)>]
let ``[IndexList] enumerator correct`` (m : list<int>) =
    let h = IndexList.ofList m

    h |> Seq.toList |> should equal (IndexList.toList h)
    h |> Seq.toList |> should equal m

    
[<Property(EndSize = 10000)>]
let ``[IndexList] collect`` (l : list<int>) =
    let ref = l |> List.collect (fun v -> [v; 2*v; 3*v])
    let test = l |> IndexList.ofList |> IndexList.collect (fun v -> IndexList.ofList [v; 2*v; 3*v]) |> IndexList.toList
    test |> should equal ref
    

[<Property(EndSize = 10000)>]
let ``[IndexList] map`` (l : list<int>) =
    let ref = l |> List.map (fun v -> v / 3)
    let test = l |> IndexList.ofList |> IndexList.map (fun v -> v / 3) |> IndexList.toList
    test |> should equal ref

[<Property(EndSize = 10000)>]
let ``[IndexList] add/prepend`` (l : list<int>) = 
    let indexList = IndexList.ofList l
    indexList 
    |> IndexList.add 1
    |> IndexList.prepend 5
    |> IndexList.toList
    |> should equal ([5] @ l @ [1])

[<Property(EndSize = 10000)>]
let ``[IndexList] equality`` (l : list<int>) =
    let a = IndexList.ofList l

    a |> should equal a
    a |> should not' (equal (IndexList.add 1 a))
    a |> should not' (equal (IndexList.prepend 1 a))

[<Property(EndSize = 10000)>]
let ``[IndexList] range`` (lowerBound: int)  (upperBound: int)=
    let a = IndexList.range lowerBound upperBound

    a |> IndexList.toList |> should equal [ lowerBound .. upperBound ]

[<Property(EndSize = 10000)>]
let ``[IndexList] init`` (length: int)  =
    if length >= 0 then 
        let a = IndexList.init length id 

        a |> IndexList.toList |> should equal [ 0 .. length - 1 ]

[<Property(EndSize = 1000)>]
let ``[IndexList] tryGetPosition`` (l : list<int>) =
    let l = IndexList.ofList l
    let mutable i = 0
    for (idx, _) in IndexList.toSeqIndexed l do
        match IndexList.tryGetPosition idx l with
        | Some p -> p |> should equal i
        | None -> failwithf "%A should be contained" idx
        i <- i + 1




[<Property(MaxTest = 4000)>]
let ``[IndexList] computeDelta / applyDelta`` (l1 : list<int>) (l2 : list<int>) (l3 : list<int>) =
    let l1 = IndexList.ofList l1
    let l2 = IndexList.ofList l2
    let l3 = IndexList.ofList l3

    IndexList.computeDelta l1 l1 |> should equal IndexListDelta.empty<int>
    IndexList.computeDelta l2 l2 |> should equal IndexListDelta.empty<int>
    IndexList.computeDelta l3 l3 |> should equal IndexListDelta.empty<int>

    
    let d12 = IndexList.computeDelta l1 l2
    let d23 = IndexList.computeDelta l2 l3 
    let d31 = IndexList.computeDelta l3 l1 

    IndexList.applyDelta l1 d12 |> fst |> should equal l2
    IndexList.applyDelta l2 d23 |> fst |> should equal l3
    IndexList.applyDelta l3 d31 |> fst |> should equal l1
    
    let d123 = IndexListDelta.combine d12 d23
    let d231 = IndexListDelta.combine d23 d31
    let d312 = IndexListDelta.combine d31 d12
    IndexList.applyDelta l1 d123 |> fst |> should equal l3
    IndexList.applyDelta l2 d231 |> fst |> should equal l1
    IndexList.applyDelta l3 d312 |> fst |> should equal l2
    
    let d1231 = IndexListDelta.combine d123 d31
    let d2312 = IndexListDelta.combine d231 d12
    let d3123 = IndexListDelta.combine d312 d23
    IndexList.applyDelta l1 d1231 |> fst |> should equal l1
    IndexList.applyDelta l2 d2312 |> fst |> should equal l2
    IndexList.applyDelta l3 d3123 |> fst |> should equal l3

    
[<Property(MaxTest = 4000)>]
let ``[IndexList] computeDeltaToList`` (l1 : list<int>) (l2 : list<int>) =
    let a1 = IndexList.ofList l1
    let a2 = IndexList.ofList l2

    let cmp = DefaultEqualityComparer.Instance

    IndexList.computeDeltaToList cmp a1 l1 |> should equal IndexListDelta.empty<int>
    IndexList.computeDeltaToList cmp a2 l2 |> should equal IndexListDelta.empty<int>

    let d12 = IndexList.computeDeltaToList cmp a1 l2
    let d21 = IndexList.computeDeltaToList cmp a2 l1

    IndexList.applyDelta a1 d12 |> fst |> IndexList.toList |> should equal l2
    IndexList.applyDelta a2 d21 |> fst |> IndexList.toList |> should equal l1
