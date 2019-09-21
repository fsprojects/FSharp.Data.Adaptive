module ARef

open FSharp.Control
open FSharp.Control.Incremental
open FsUnit.Xunit
open FsCheck.Xunit

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
 
    let bind (mapping : 'a -> aref<'b>) (r : aref<'a>) =
        create
            (Incremental.ARef.bind (fun v -> mapping(v).Adaptive) r.Adaptive)
            (Reference.ARef.bind (fun v -> mapping(v).Reference) r.Reference)
    
    
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
    let input = ARef.init (obj())
    let test = input |> ARef.map (fun v -> v, Unchecked.hash v)

    check test

    for v in values do
        transact (fun () -> input.Value <- v)
        check test

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






