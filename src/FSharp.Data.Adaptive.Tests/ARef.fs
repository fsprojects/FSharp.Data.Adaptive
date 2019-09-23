module ARef

open FSharp.Data.Adaptive
open FSharp.Data.Adaptive.Validation
open NUnit.Framework
open FsUnit
open FsCheck.NUnit

[<AutoOpen>]
module Helpers =
    module List =
        let cross (a : list<'a>) (b : list<'b>) =
            a |> List.collect (fun va ->
                b |> List.map (fun vb -> (va, vb))
            )

    let inline check (r : aref<'a>) =
        let (a, r) = ARef.force r
        a |> should equal r


type Record<'a> = { value : 'a }

[<Property>]
let ``[ARef] constant equality`` (value : obj) =
    let a = ARef.constant value
    let b = ARef.constant value
    a.Adaptive |> should equal b.Adaptive

    let a = ARef.constant { value = value }
    let b = ARef.constant { value = value }
    a.Adaptive |> should equal b.Adaptive

    
    let a = ARef.constant { value = 1 }
    let b = ARef.constant { value = 2 }
    a.Adaptive |> should not' (equal b.Adaptive)

    let a = ARef.constant null
    let b = ARef.constant null
    a.Adaptive |> should equal b.Adaptive

[<Property>]
let ``[CRef] can change`` (values : list<obj>) =
    
    let ref = ARef.init (obj())
    check ref

    for v in values do
        transact (fun () -> ref.Value <- v)
        check ref

[<Property>]
let ``[ARef] map validation`` (values : list<obj>) =
    let values = obj() :: values
    let input = ARef.init (obj())
    let test = input |> ARef.map (fun v -> v, Unchecked.hash v)

    check test

    for v in values do
        transact (fun () -> input.Value <- v)
        check test
      
[<Test>]
let ``[ARef] map constant`` () =
    let a = ARef.constant 1
    let b = ARef.map id a
    check b
    b.Adaptive.IsConstant |> should be True
  
[<Property>]
let ``[ARef] map2 validation`` (va : list<obj>) (vb : list<obj>)  =
    let va = obj() :: va
    let vb = obj() :: vb
    let ra = ARef.init (obj())
    let rb = ARef.init (obj())
    let test = ARef.map2 (fun a b -> Unchecked.hash a - Unchecked.hash b) ra rb

    check test

    for va, vb in List.cross va vb do
        transact (fun () -> 
            ra.Value <- va
            rb.Value <- vb
        )
        check test

[<Test>]
let ``[ARef] map2 constant`` () =
    let a = ARef.constant 1
    let b = ARef.constant 2
    let test = ARef.map2 (fun a b -> (a,b)) a b
    check test
    test.Adaptive.IsConstant |> should be True

[<Property>]
let ``[ARef] map3 validation`` (va : list<obj>) (vb : list<obj>) (vc : list<obj>)  =
    let va = obj() :: va
    let vb = obj() :: vb
    let vc = obj() :: vc

    let ra = ARef.init (obj())
    let rb = ARef.init (obj())
    let rc = ARef.init (obj())
    let test = ARef.map3 (fun a b c -> Unchecked.hash a - Unchecked.hash b - Unchecked.hash c) ra rb rc

    check test

    for (va, vb), vc in List.cross (List.cross va vb) vc do
        transact (fun () -> 
            ra.Value <- va
            rb.Value <- vb
            rc.Value <- vc
        )
        check test

[<Test>]
let ``[ARef] map3 constant`` () =
    let a = ARef.constant 1
    let b = ARef.constant 2
    let c = ARef.constant 3
    let test = ARef.map3 (fun a b c -> (a,b,c)) a b c
    check test
    test.Adaptive.IsConstant |> should be True

[<Property>]
let ``[ARef] bind validation`` (values : list<obj>) =
    let input = ARef.init (obj())
    let a = ARef.init (obj())
    let b = ARef.init (obj())

    let test = 
        input |> ARef.bind (fun v -> 
            let hash = Unchecked.hash v
            if hash % 2 = 0 then a :> aref<_>
            else b :> aref<_>
        )

    check test

    for v in values do
        transact (fun () -> 
            let hash = Unchecked.hash v |> abs

            if hash % 3 <= 1 then input.Value <- v
            if hash % 5 <= 3 then a.Value <- v
            if hash % 7 <= 4 then b.Value <- v

        )
        check test

[<Test>]
let ``[ARef] bind constant`` () =
    let a = ARef.constant 10
    let b = ARef.init "b" |> ARef.map id
    let c = ARef.init "c" |> ARef.map id

    let test =
        a |> ARef.bind (fun va ->
            if va = 10 then b
            else c
        )

    check test
    test.Adaptive |> should equal b.Adaptive
 
[<Property>]
let ``[ARef] bind2 validation`` (values : list<obj>) =
    let ref1 = ARef.init (obj())
    let ref2 = ARef.init (obj())
    let a = ARef.init (obj())
    let b = ARef.init (obj())

    let test = 
        ARef.bind2 (fun v1 v2 -> 
            let hash = Unchecked.hash v1 - Unchecked.hash v2
            if hash % 2 = 0 then a :> aref<_>
            else b :> aref<_>
        ) ref1 ref2

    check test

    for v in values do
        transact (fun () -> 
            let hash = Unchecked.hash v |> abs

            if hash % 3 <= 1 then ref1.Value <- v
            if hash % 11 <= 6 then ref2.Value <- v
            if hash % 5 <= 3 then a.Value <- v
            if hash % 7 <= 4 then b.Value <- v

        )
        check test



