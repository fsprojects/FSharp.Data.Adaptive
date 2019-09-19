module Weak

open System
open FsUnit
open Xunit
open FsCheck
open FsCheck.Xunit
open FsIncremental

/// a dummy type failing on GetHashCode/Equals to ensure that
/// WeakRef/WeakSet only operate on reference-hashes/equality
type BrokenEquality() =    
    override x.GetHashCode() = failwith "BrokenEquality.GetHashCode should not be called"
    override x.Equals _o = failwith "BrokenEquality.Equals should not be called"

/// force garbage collection
let ensureGC() =
    GC.Collect(3, GCCollectionMode.Forced, true, true)
    GC.WaitForFullGCComplete() |> ignore

[<Fact>]
let ``[WeakRef] equality``() =

    let deadWeaks() =
        let a = BrokenEquality()
        WeakRef a, WeakRef a

    let a = BrokenEquality()
    let b = BrokenEquality()

    let wa  = WeakRef a
    let wa' = WeakRef a
    let wb  = WeakRef b
    let wb' = WeakRef b
    let wd, wd' = deadWeaks()
    ensureGC()
    
    // WeakRefs maintain equality
    wa |> should equal wa'
    wb |> should equal wb'
    wd |> should equal wd'
    wa |> should not' (equal wb)
    wa |> should not' (equal wd)
    wb |> should not' (equal wd)

    // WeakRefs are actually weak
    wd.Object |> should equal None
    wd'.Object |> should equal None

    // WeakRefs hold correct values
    wa.Object.Value |> should refequal a
    wb.Object.Value |> should refequal b
    wa'.Object.Value |> should refequal a
    wb'.Object.Value |> should refequal b

[<Fact>]
let ``[WeakSet] add``() =
    let set = WeakSet<BrokenEquality>()

    let a = BrokenEquality()
    set.Add a |> should be True
    set.Add a |> should be False

    let addDead() =
        let a = BrokenEquality()
        set.Add a |> should be True

    for _i in 1 .. 10 do addDead()
    ensureGC()
    match Seq.toList set with
    | [v] -> v |> should refequal a
    | _ -> failwith "inconsistent WeakSet length"

[<Fact>]
let ``[WeakSet] remove``() =
    let set = WeakSet<BrokenEquality>()
    let a = BrokenEquality()
    set.Add a |> ignore
    set.Remove a |> should be True
    set |> Seq.isEmpty |> should be True






