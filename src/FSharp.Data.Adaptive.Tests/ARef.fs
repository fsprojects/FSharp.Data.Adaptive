module AVal

open FSharp.Data.Adaptive
open FSharp.Data.Adaptive.Validation
open NUnit.Framework
open FsUnit
open FsCheck.NUnit

[<AutoOpen>]
module Helpers =
    module List =
        let cross (a : list<'A>) (b : list<'B>) =
            a |> List.collect (fun va ->
                b |> List.map (fun vb -> (va, vb))
            )

    let inline check (r : aval<'T>) =
        let (a, r) = AVal.force r
        a |> should equal r


type Record<'T> = { value : 'T }

[<Property>]
let ``[AVal] constant equality`` (value : obj) =
    let a = AVal.constant value
    let b = AVal.constant value
    a.Adaptive |> should equal b.Adaptive

    let a = AVal.constant { value = value }
    let b = AVal.constant { value = value }
    a.Adaptive |> should equal b.Adaptive

    
    let a = AVal.constant { value = 1 }
    let b = AVal.constant { value = 2 }
    a.Adaptive |> should not' (equal b.Adaptive)

    let a = AVal.constant null
    let b = AVal.constant null
    a.Adaptive |> should equal b.Adaptive

[<Property>]
let ``[CVal] can change`` (values : list<obj>) =
    
    let ref = AVal.init (obj())
    check ref

    for v in values do
        transact (fun () -> ref.Value <- v)
        check ref

[<Property>]
let ``[AVal] map validation`` (values : list<obj>) =
    let values = obj() :: values
    let input = AVal.init (obj())
    let test = input |> AVal.map (fun v -> v, Unchecked.hash v)

    check test

    for v in values do
        transact (fun () -> input.Value <- v)
        check test
      
[<Test>]
let ``[AVal] map constant`` () =
    let a = AVal.constant 1
    let b = AVal.map id a
    check b
    b.Adaptive.IsConstant |> should be True
  
[<Property>]
let ``[AVal] map2 validation`` (va : list<obj>) (vb : list<obj>)  =
    let va = obj() :: va
    let vb = obj() :: vb
    let ra = AVal.init (obj())
    let rb = AVal.init (obj())
    let test = AVal.map2 (fun a b -> Unchecked.hash a - Unchecked.hash b) ra rb

    check test

    for va, vb in List.cross va vb do
        transact (fun () -> 
            ra.Value <- va
            rb.Value <- vb
        )
        check test

[<Test>]
let ``[AVal] map2 constant`` () =
    let a = AVal.constant 1
    let b = AVal.constant 2
    let test = AVal.map2 (fun a b -> (a,b)) a b
    check test
    test.Adaptive.IsConstant |> should be True

[<Property>]
let ``[AVal] map3 validation`` (va : list<obj>) (vb : list<obj>) (vc : list<obj>)  =
    let va = obj() :: va
    let vb = obj() :: vb
    let vc = obj() :: vc

    let ra = AVal.init (obj())
    let rb = AVal.init (obj())
    let rc = AVal.init (obj())
    let test = AVal.map3 (fun a b c -> Unchecked.hash a - Unchecked.hash b - Unchecked.hash c) ra rb rc

    check test

    for (va, vb), vc in List.cross (List.cross va vb) vc do
        transact (fun () -> 
            ra.Value <- va
            rb.Value <- vb
            rc.Value <- vc
        )
        check test

[<Test>]
let ``[AVal] map3 constant`` () =
    let a = AVal.constant 1
    let b = AVal.constant 2
    let c = AVal.constant 3
    let test = AVal.map3 (fun a b c -> (a,b,c)) a b c
    check test
    test.Adaptive.IsConstant |> should be True

[<Property>]
let ``[AVal] bind validation`` (values : list<obj>) =
    let input = AVal.init (obj())
    let a = AVal.init (obj())
    let b = AVal.init (obj())

    let test = 
        input |> AVal.bind (fun v -> 
            let hash = Unchecked.hash v
            if hash % 2 = 0 then a :> aval<_>
            else b :> aval<_>
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
let ``[AVal] bind constant`` () =
    let a = AVal.constant 10
    let b = AVal.init "b" |> AVal.map id
    let c = AVal.init "c" |> AVal.map id

    let test =
        a |> AVal.bind (fun va ->
            if va = 10 then b
            else c
        )

    check test
    test.Adaptive |> should equal b.Adaptive
 
[<Property>]
let ``[AVal] bind2 validation`` (values : list<obj>) =
    let ref1 = AVal.init (obj())
    let ref2 = AVal.init (obj())
    let a = AVal.init (obj())
    let b = AVal.init (obj())

    let test = 
        AVal.bind2 (fun v1 v2 -> 
            let hash = Unchecked.hash v1 - Unchecked.hash v2
            if hash % 2 = 0 then a :> aval<_>
            else b :> aval<_>
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



