module HashMap

open System
open NUnit
open FsCheck
open FsCheck.NUnit
open FSharp.Data.Adaptive

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

    override x.GetHashCode() = abs x.value % 2
    override x.Equals o =   
        match o with
            | :? StupidHash as o -> x.value = o.value
            | _ -> false


[<Property>]
let ``[HashMap] count`` (l : Map<int, int>) (a : int)  =
    not (Map.containsKey a l) ==> lazy (
        let map = l |> Map.toList |> HashMap.ofList
        let mapWithA = HashMap.add a a map

        List.all [
            HashMap.count HashMap.empty = 0
            HashMap.count mapWithA = HashMap.count map + 1
            HashMap.count (HashMap.remove a mapWithA) = HashMap.count map
            HashMap.count map = l.Count
            HashMap.count (HashMap.union map map) = HashMap.count map
            HashMap.count (HashMap.map (fun _ v -> v) map) = HashMap.count map
            HashMap.count (HashMap.filter (fun _ _ -> true) map) = HashMap.count map
            HashMap.count (HashMap.filter (fun _ _ -> false) map) = 0
            HashMap.count (HashMap.choose (fun _ v -> Some v) map) = HashMap.count map
            HashMap.count (HashMap.choose (fun _ _ -> None) map) = 0
            HashMap.count (HashMap.alter a (fun _ -> None) mapWithA) = HashMap.count map
            HashMap.count (HashMap.alter a (fun _ -> Some 5) mapWithA) = HashMap.count mapWithA
            HashMap.count (HashMap.update a (fun _ -> 5) mapWithA) = HashMap.count mapWithA
        ]
    )

[<Property>]
let ``[HashMap] tryFind`` (l : Map<int, int>) (a : int)  =
    not (Map.containsKey a l) ==> lazy (
        let map = l |> Map.toList |> HashMap.ofList
        let mapWithA = HashMap.add a a map
        
        List.all [
            HashMap.tryFind a mapWithA = Some a
            HashMap.tryFind a map = None
            HashMap.tryFind a (HashMap.add a 7 mapWithA) = Some 7
            HashMap.tryFind a (HashMap.add a 7 map) = Some 7
            HashMap.tryFind a (HashMap.remove a mapWithA) = None
            HashMap.tryFind a (HashMap.union map mapWithA) = Some a
            HashMap.tryFind a (HashMap.alter a (fun o -> Some 100) mapWithA) = Some 100
            HashMap.tryFind a (HashMap.alter a (fun o -> None) mapWithA) = None
            HashMap.tryFind a (HashMap.update a (fun o -> 123) mapWithA) = Some 123
            HashMap.tryFind a (HashMap.update a (fun o -> 123) map) = Some 123
            HashMap.tryFind a (HashMap.choose (fun _ v -> Some v) mapWithA) = Some a
            HashMap.tryFind a (HashMap.choose (fun _ v -> None) mapWithA) = None
            HashMap.tryFind a (HashMap.choose (fun _ v -> Some 7) mapWithA) = Some 7
            HashMap.tryFind a (HashMap.filter (fun _ v -> true) mapWithA) = Some a
            HashMap.tryFind a (HashMap.filter (fun _ v -> false) mapWithA) = None

        ]

    )

[<Property>]
let ``[HashMap] containsKey`` (l : Map<int, int>) (a : int)  =
    let map = l |> Map.toList |> HashMap.ofList
    HashMap.containsKey a map = Option.isSome (HashMap.tryFind a map)

[<Property>]
let ``[HashMap] find`` (l : Map<int, int>) (a : int)  =
    let map = l |> Map.toList |> HashMap.ofList
    let map = map |> HashMap.add a a
    HashMap.find a map = a

[<Property>]
let ``[HashMap] ofList`` (l : list<int * int>) =
    List.sortBy fst (HashMap.toList (HashMap.ofList l)) = Map.toList (Map.ofList l)

[<Property>]
let ``[HashMap] map2/choose2`` (lm : Map<int, int>) (rm : Map<int, int>) =
    let l = lm |> Map.toList |> HashMap.ofList
    let r = rm |> Map.toList |> HashMap.ofList

    let map2 (f : 'K -> option<'A> -> option<'B> -> 'C) (l : Map<'K, 'A>) (r : Map<'K, 'B>) =
        let mutable res = Map.empty

        for (lk,lv) in Map.toSeq l do
            match Map.tryFind lk r with
                | Some rv -> res <- Map.add lk (f lk (Some lv) (Some rv)) res
                | None -> res <- Map.add lk (f lk (Some lv) None) res

        for (rk,rv) in Map.toSeq r do
            match Map.tryFind rk l with
                | Some _ -> ()
                | None -> res <- Map.add rk (f rk None (Some rv)) res

        res

    let choose2 (f : 'K -> option<'A> -> option<'B> -> option<'C>) (l : Map<'K, 'A>) (r : Map<'K, 'B>) =
        let mutable res = Map.empty

        for (lk,lv) in Map.toSeq l do
            match Map.tryFind lk r with
                | Some rv -> 
                    match f lk (Some lv) (Some rv) with
                        | Some r ->
                            res <- Map.add lk r res
                        | None ->
                            ()
                | None -> 
                    match f lk (Some lv) None with
                        | Some r -> res <- Map.add lk r res
                        | None -> ()

        for (rk,rv) in Map.toSeq r do
            match Map.tryFind rk l with
                | Some _ -> ()
                | None -> 
                    match f rk None (Some rv) with
                        | Some r -> res <- Map.add rk r res
                        | None -> ()

        res

    let equal (l : HashMap<'K, 'V>) (r : Map<'K, 'V>) =
        let l = l |> HashMap.toList |> List.sortBy fst
        let r = r |> Map.toList
        l = r

    let add (k : int) (l : option<int>) (r : option<int>) =
        match l, r with
            | Some l, Some r -> l + r
            | None, Some r -> r
            | Some l, None -> l
            | None, None -> failwith "that's bad (Map invented a key)"

    let add2 (k : int) (l : option<int>) (r : option<int>) =
        match l, r with
            | Some l, Some r -> if l > r then Some r else None
            | None, Some r -> Some r
            | Some l, None -> Some l
            | None, None -> failwith "that's bad (Map invented a key)"

    List.all [
        equal (HashMap.map2 add l r) (map2 add lm rm)
        equal (HashMap.choose2 (fun k l r -> add k l r |> Some) l r) (map2 add lm rm)
        equal (HashMap.choose2 add2 l r) (choose2 add2 lm rm)
    ]

[<Property>]
let ``[HashMap] choose`` (m : Map<int, int>) (f : int -> int -> option<int>) =
    let h = HashMap.ofSeq (Map.toSeq m)

    let tm =
        let mutable res = Map.empty
        for (KeyValue(k,v)) in m do
            match f k v with
                | Some v -> res <- Map.add k v res
                | _ -> ()
        res

    let th = HashMap.choose f h |> Map.ofSeq

    tm = th



[<Property>]
let ``[HashMap] equality`` (h0 : StupidHash) =
    let h1 = { value = h0.value + 1 }
    let h2 = { value = h0.value + 2 }
    let h3 = { value = h0.value + 3 }
    let a = HashMap.empty |> HashMap.add h0 0 |> HashMap.add h1 1 |> HashMap.add h2 2 |> HashMap.add h3 3
    let b = HashMap.empty |> HashMap.add h1 1 |> HashMap.add h2 2 |> HashMap.add h3 3 |> HashMap.add h0 0
    let c = HashMap.empty |> HashMap.add h2 2 |> HashMap.add h3 3 |> HashMap.add h0 0 |> HashMap.add h1 1
    let d = HashMap.empty |> HashMap.add h3 3 |> HashMap.add h0 0 |> HashMap.add h1 1 |> HashMap.add h2 2
    
    let x = d |> HashMap.add h3 4
    let y = d |> HashMap.add { value = h0.value + 4 } 4

    let ah = a.GetHashCode()
    let bh = b.GetHashCode()
    let ch = c.GetHashCode()
    let dh = d.GetHashCode()

    a = a && b = b && c = c && d = d && x = x && y = y &&

    a = b && a = c && a = d && b = c && b = d && c = d && 
    b = a && c = a && d = a && c = b && d = b && d = c && 

    ah = bh && bh = ch && ch = dh &&

    x <> a && x <> b && x <> c && x <> d &&
    y <> a && y <> b && y <> c && y <> d &&
    x <> y &&

    a.Count = 4 && b.Count = 4 && c.Count = 4 && d.Count = 4 && 
    x.Count = 4 && y.Count = 5


    