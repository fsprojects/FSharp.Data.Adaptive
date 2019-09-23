namespace FSharp.Control.Incremental.Validation

open FSharp.Control.Incremental
open FSharp.Control
open FsUnit

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

    let bind2 (mapping : 'a -> 'b -> aref<'c>) (r1 : aref<'a>) (r2 : aref<'b>) =
        create
            (Incremental.ARef.bind2 (fun va vb -> (mapping va vb).Adaptive) r1.Adaptive r2.Adaptive)
            (Reference.ARef.bind2 (fun va vb -> (mapping va vb).Reference) r1.Reference r2.Reference)


type aset<'a> =
    abstract member Adaptive : Incremental.aset<'a>
    abstract member Reference : Reference.aset<'a>

type ISetReader<'a> =
    abstract member Adaptive : Incremental.ISetReader<'a>
    abstract member Reference : Reference.ISetReader<'a>

[<AutoOpen>]
module ASetReaders = 
    type aset<'a> with
        member x.GetReader() =
            let adaptive = x.Adaptive.GetReader()
            let reference = x.Reference.GetReader()
            { new ISetReader<'a> with
                member __.Adaptive = adaptive
                member __.Reference = reference
            }

type cset<'a>(value : HashSet<'a>) = 
    let adaptive = Incremental.cset value
    let reference = Reference.cset value

    member x.Contains value =
        let cr = reference.Contains value
        let ca = adaptive.Contains value
        ca |> should equal cr
        cr

    member x.IsEmpty =
        let er = reference.IsEmpty
        let ea = adaptive.IsEmpty
        ea |> should equal er
        er

    member x.Count = 
        let cr = reference.Count
        let ca = adaptive.Count
        ca |> should equal cr
        cr

    member x.Value
        with get() = 
            let vr = reference.Value
            let va = adaptive.Value
            va |> should equal vr
            vr

        and set v = 
            adaptive.Value <- v
            reference.Value <- v

    member x.Add (value : 'a) =
        let wa = adaptive.Add value
        let wr = reference.Add value
        wa |> should equal wr
        wr

    member x.Remove(value : 'a) =
        let wa = adaptive.Remove value
        let wr = reference.Remove value
        wa |> should equal wr
        wr

    member x.Clear() =
        adaptive.Clear()
        reference.Clear()

    member x.UnionWith other =
        adaptive.UnionWith other
        reference.UnionWith other

    member x.ExceptWith other =
        adaptive.ExceptWith other
        reference.ExceptWith other

    interface aset<'a> with
        member x.Adaptive = adaptive :> _
        member x.Reference = reference :> _

module ASet =
    let create (a : Incremental.aset<'a>) (r : Reference.aset<'a>) =
        { new aset<'a> with
            member x.Adaptive = a
            member x.Reference = r
        }

    let empty<'a> = 
        create (Incremental.ASet.empty) (Reference.ASet.empty)
            
    let single (value : 'a) = 
        create (Incremental.ASet.single value) (Reference.ASet.single value)

    let ofSeq (value : seq<'a>) = 
        create (Incremental.ASet.ofSeq value) (Reference.ASet.ofSeq value)

    let ofList (value : list<'a>) = 
        create (Incremental.ASet.ofList value) (Reference.ASet.ofList value)

    let ofArray (value : array<'a>) = 
        create (Incremental.ASet.ofArray value) (Reference.ASet.ofArray value)

    let ofHashSet (value : HashSet<'a>) = 
        create (Incremental.ASet.ofHashSet value) (Reference.ASet.ofHashSet value)

    let toARef (set : aset<'a>) =
        ARef.create
            (Incremental.ASet.toARef set.Adaptive)
            (Reference.ASet.toARef set.Reference)

    let map (mapping : 'a -> 'b) (set : aset<'a>) =
        create
            (Incremental.ASet.map mapping set.Adaptive)
            (Reference.ASet.map mapping set.Reference)

    let choose (mapping : 'a -> Option<'b>) (set : aset<'a>) =
        create
            (Incremental.ASet.choose mapping set.Adaptive)
            (Reference.ASet.choose mapping set.Reference)

    let filter (predicate : 'a -> bool) (set : aset<'a>) =
        create
            (Incremental.ASet.filter predicate set.Adaptive)
            (Reference.ASet.filter predicate set.Reference)
            
    let union (sets : aset<aset<'a>>) =
        create
            (Incremental.ASet.union (sets.Adaptive |> Incremental.ASet.map (fun s -> s.Adaptive)))
            (Reference.ASet.union (sets.Reference |> Reference.ASet.map (fun s -> s.Reference)))

    let collect (mapping : 'a -> aset<'b>) (set : aset<'a>) =
        create
            (Incremental.ASet.collect (fun v -> (mapping v).Adaptive) set.Adaptive)
            (Reference.ASet.collect (fun v -> (mapping v).Reference) set.Reference)

    let ofARef (ref : aref<#seq<'a>>) =
        create
            (ref.Adaptive |> Incremental.ASet.ofARef)
            (ref.Reference |> Reference.ASet.ofARef)
         
