module IntMap

open FsUnit.Xunit
open FsCheck.Xunit
open FSharp.Control.Incremental

[<Property>]
let ``[IntMap] basics`` (data : list<int>) (value : int) =
    let data = data |> List.map (fun i -> i, i)
    let map = IntMap.ofList data

    // exists after insert
    map 
    |> IntMap.insert value value 
    |> IntMap.tryFind value 
    |> should equal (Some value)

    // delete deletes
    map 
    |> IntMap.delete value 
    |> IntMap.tryFind value 
    |> should equal None

    // alter inserts
    map 
    |> IntMap.alter (fun _ -> Some value) value 
    |> IntMap.tryFind value 
    |> should equal (Some value)

    // alter deletes
    map 
    |> IntMap.alter (fun _ -> None) value 
    |> IntMap.tryFind value 
    |> should equal None

    // TODO: many more

[<Property>]
let ``[IntMap] computeDelta`` (data : list<int>) (value : int) =
    let data = data |> List.map (fun i -> i, i)
    let sorted = data |> Map.ofList |> Map.toList
    let map = IntMap.ofList data
    let map2 = IntMap.ofList (List.map (fun (k, v) -> k, v + 1) data)

    let computeDelta l r =
        IntMap.computeDelta
            (fun _k l r -> if l <> r then Some (Some r) else None)
            (fun l -> l |> IntMap.map (fun _ -> None))
            (fun r -> r |> IntMap.map Some)
            l 
            r

    // delta for equal is empty
    computeDelta map map 
    |> IntMap.isEmpty 
    |> should be True

    // delta from empty to map
    computeDelta IntMap.empty map 
    |> IntMap.toList |> List.sortBy fst
    |> should equal (List.map (fun (k,v) -> k, Some v) sorted)
 
    // delta from map to empty
    computeDelta map IntMap.empty 
    |> IntMap.toList |> List.sortBy fst
    |> should equal (List.map (fun (k,_v) -> k, Option<int>.None) sorted)

    // delta for all elements
    computeDelta map map2
    |> IntMap.toList |> List.sortBy fst
    |> should equal (List.map (fun (k,v) -> k, Some(v + 1)) sorted)


