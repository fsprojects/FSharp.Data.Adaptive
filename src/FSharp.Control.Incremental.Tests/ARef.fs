module ARef

open FSharp.Control
open FSharp.Control.Incremental
open FsUnit.Xunit
open FsCheck.Xunit
open Xunit

type aref<'a> =
    abstract member Adaptive : Incremental.aref<'a>
    abstract member Reference : Reference.aref<'a>

type cref<'a>(value : 'a) = 
    let adaptive = Incremental.cref value
    let reference = Reference.cref value

    member x.Value 
        with get() = reference.Value
        and set v =
            reference.Value <- v
            adaptive.Value <- v

    interface aref<'a> with
        member x.Adaptive = adaptive :> Incremental.aref<_>
        member x.Reference = reference :> Reference.aref<_>

module ARef =

    let create (adaptive : Incremental.aref<'a>) (reference : Reference.aref<'a>) =
        { new aref<'a> with
            member x.Adaptive = adaptive
            member x.Reference = reference
        }

    let force (ref : aref<'a>) =
        let adaptive = Incremental.ARef.force ref.Adaptive
        let reference = Reference.ARef.force ref.Reference
        (adaptive, reference)

    let init (value : 'a) =
        cref<'a>(value)

    let constant (value : 'a) =
        create 
            (Incremental.ARef.constant value)
            (Reference.ARef.constant value)

    let map (mapping : 'a -> 'b) (r : aref<'a>) =
        create
            (Incremental.ARef.map mapping r.Adaptive)
            (Reference.ARef.map mapping r.Reference)
 
    let map2 (mapping : 'a -> 'b -> 'c) (ref1 : aref<'a>) (ref2 : aref<'b>) =
        create
            (Incremental.ARef.map2 mapping ref1.Adaptive ref2.Adaptive)
            (Reference.ARef.map2 mapping ref1.Reference ref2.Reference)
 
    let map3 (mapping : 'a -> 'b -> 'c -> 'd) (ref1 : aref<'a>) (ref2 : aref<'b>) (ref3 : aref<'c>) =
        create
            (Incremental.ARef.map3 mapping ref1.Adaptive ref2.Adaptive ref3.Adaptive)
            (Reference.ARef.map3 mapping ref1.Reference ref2.Reference ref3.Reference)
 
    let bind (mapping : 'a -> aref<'b>) (r : aref<'a>) =
        create
            (Incremental.ARef.bind (fun v -> mapping(v).Adaptive) r.Adaptive)
            (Reference.ARef.bind (fun v -> mapping(v).Reference) r.Reference)
    
  
module List =
    let cross (a : list<'a>) (b : list<'b>) =
        a |> List.collect (fun va ->
            b |> List.map (fun vb -> (va, vb))
        )

let inline check (r : aref<'a>) =
    let (a, r) = ARef.force r
    a |> should equal r


[<Property>]
let ``[CRef] can change`` (values : list<obj>) =
    
    let ref = ARef.init (obj())
    check ref

    for v in values do
        transact (fun () -> ref.Value <- v)
        check ref

[<Property>]
let ``[ARef] map working`` (values : list<obj>) =
    let values = obj() :: values
    let input = ARef.init (obj())
    let test = input |> ARef.map (fun v -> v, Unchecked.hash v)

    check test

    for v in values do
        transact (fun () -> input.Value <- v)
        check test
      
[<Fact>]
let ``[ARef] map constant`` () =
    let a = ARef.constant 1
    let b = ARef.map id a
    check b
    b.Adaptive.IsConstant |> should be True
  
[<Property>]
let ``[ARef] map2 working`` (va : list<obj>) (vb : list<obj>)  =
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

[<Fact>]
let ``[ARef] map2 constant`` () =
    let a = ARef.constant 1
    let b = ARef.constant 2
    let test = ARef.map2 (fun a b -> (a,b)) a b
    check test
    test.Adaptive.IsConstant |> should be True

[<Property>]
let ``[ARef] map3 working`` (va : list<obj>) (vb : list<obj>) (vc : list<obj>)  =
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

[<Fact>]
let ``[ARef] map3 constant`` () =
    let a = ARef.constant 1
    let b = ARef.constant 2
    let c = ARef.constant 3
    let test = ARef.map3 (fun a b c -> (a,b,c)) a b c
    check test
    test.Adaptive.IsConstant |> should be True

[<Property>]
let ``[ARef] bind working`` (values : list<obj>) =
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






