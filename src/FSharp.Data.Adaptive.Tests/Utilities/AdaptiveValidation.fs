namespace FSharp.Data.Adaptive.Validation

open FSharp.Data.Adaptive
open FSharp.Data
open FsUnit

type aref<'a> =
    abstract member Adaptive : Adaptive.aref<'a>
    abstract member Reference : Reference.aref<'a>

type cref<'a>(value : 'a) = 
    let adaptive = Adaptive.cref value
    let reference = Reference.cref value

    member x.Value 
        with get() = reference.Value
        and set v =
            reference.Value <- v
            adaptive.Value <- v

    interface aref<'a> with
        member x.Adaptive = adaptive :> Adaptive.aref<_>
        member x.Reference = reference :> Reference.aref<_>

module ARef =

    let create (adaptive : Adaptive.aref<'a>) (reference : Reference.aref<'a>) =
        { new aref<'a> with
            member x.Adaptive = adaptive
            member x.Reference = reference
        }

    let force (ref : aref<'a>) =
        let adaptive = Adaptive.ARef.force ref.Adaptive
        let reference = Reference.ARef.force ref.Reference
        (adaptive, reference)

    let init (value : 'a) =
        cref<'a>(value)

    let constant (value : 'a) =
        create 
            (Adaptive.ARef.constant value)
            (Reference.ARef.constant value)

    let map (mapping : 'a -> 'b) (r : aref<'a>) =
        create
            (Adaptive.ARef.map mapping r.Adaptive)
            (Reference.ARef.map mapping r.Reference)
 
    let map2 (mapping : 'a -> 'b -> 'c) (ref1 : aref<'a>) (ref2 : aref<'b>) =
        create
            (Adaptive.ARef.map2 mapping ref1.Adaptive ref2.Adaptive)
            (Reference.ARef.map2 mapping ref1.Reference ref2.Reference)
 
    let map3 (mapping : 'a -> 'b -> 'c -> 'd) (ref1 : aref<'a>) (ref2 : aref<'b>) (ref3 : aref<'c>) =
        create
            (Adaptive.ARef.map3 mapping ref1.Adaptive ref2.Adaptive ref3.Adaptive)
            (Reference.ARef.map3 mapping ref1.Reference ref2.Reference ref3.Reference)
 
    let bind (mapping : 'a -> aref<'b>) (r : aref<'a>) =
        create
            (Adaptive.ARef.bind (fun v -> mapping(v).Adaptive) r.Adaptive)
            (Reference.ARef.bind (fun v -> mapping(v).Reference) r.Reference)

    let bind2 (mapping : 'a -> 'b -> aref<'c>) (r1 : aref<'a>) (r2 : aref<'b>) =
        create
            (Adaptive.ARef.bind2 (fun va vb -> (mapping va vb).Adaptive) r1.Adaptive r2.Adaptive)
            (Reference.ARef.bind2 (fun va vb -> (mapping va vb).Reference) r1.Reference r2.Reference)


type aset<'a> =
    abstract member Adaptive : Adaptive.aset<'a>
    abstract member Reference : Reference.aset<'a>

type IHashSetReader<'a> =
    abstract member Adaptive : Adaptive.IHashSetReader<'a>
    abstract member Reference : Reference.IHashSetReader<'a>

[<AutoOpen>]
module ASetReaders = 
    type aset<'a> with
        member x.GetReader() =
            let adaptive = x.Adaptive.GetReader()
            let reference = x.Reference.GetReader()
            { new IHashSetReader<'a> with
                member __.Adaptive = adaptive
                member __.Reference = reference
            }

type cset<'a>(value : HashSet<'a>) = 
    let adaptive = Adaptive.cset value
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
    let create (a : Adaptive.aset<'a>) (r : Reference.aset<'a>) =
        { new aset<'a> with
            member x.Adaptive = a
            member x.Reference = r
        }

    let empty<'a> = 
        create (Adaptive.ASet.empty) (Reference.ASet.empty)
            
    let single (value : 'a) = 
        create (Adaptive.ASet.single value) (Reference.ASet.single value)

    let ofSeq (value : seq<'a>) = 
        create (Adaptive.ASet.ofSeq value) (Reference.ASet.ofSeq value)

    let ofList (value : list<'a>) = 
        create (Adaptive.ASet.ofList value) (Reference.ASet.ofList value)

    let ofArray (value : array<'a>) = 
        create (Adaptive.ASet.ofArray value) (Reference.ASet.ofArray value)

    let ofHashSet (value : HashSet<'a>) = 
        create (Adaptive.ASet.ofHashSet value) (Reference.ASet.ofHashSet value)

    let toARef (set : aset<'a>) =
        ARef.create
            (Adaptive.ASet.toARef set.Adaptive)
            (Reference.ASet.toARef set.Reference)

    let map (mapping : 'a -> 'b) (set : aset<'a>) =
        create
            (Adaptive.ASet.map mapping set.Adaptive)
            (Reference.ASet.map mapping set.Reference)

    let choose (mapping : 'a -> option<'b>) (set : aset<'a>) =
        create
            (Adaptive.ASet.choose mapping set.Adaptive)
            (Reference.ASet.choose mapping set.Reference)

    let filter (predicate : 'a -> bool) (set : aset<'a>) =
        create
            (Adaptive.ASet.filter predicate set.Adaptive)
            (Reference.ASet.filter predicate set.Reference)
            
    let union (sets : aset<aset<'a>>) =
        create
            (Adaptive.ASet.union (sets.Adaptive |> Adaptive.ASet.map (fun s -> s.Adaptive)))
            (Reference.ASet.union (sets.Reference |> Reference.ASet.map (fun s -> s.Reference)))

    let collect (mapping : 'a -> aset<'b>) (set : aset<'a>) =
        create
            (Adaptive.ASet.collect (fun v -> (mapping v).Adaptive) set.Adaptive)
            (Reference.ASet.collect (fun v -> (mapping v).Reference) set.Reference)

    let ofARef (ref : aref<#seq<'a>>) =
        create
            (ref.Adaptive |> Adaptive.ASet.ofARef)
            (ref.Reference |> Reference.ASet.ofARef)

    let bind (mapping : 'a -> aset<'b>) (ref : aref<'a>) =
        create
            (ref.Adaptive |> Adaptive.ASet.bind (fun v -> (mapping v).Adaptive))
            (ref.Reference |> Reference.ASet.bind (fun v -> (mapping v).Reference))
         

    let mapA (mapping : 'a -> aref<'b>) (set : aset<'a>) =
        create
            (set.Adaptive |> Adaptive.ASet.mapA (fun v -> (mapping v).Adaptive))
            (set.Reference |> Reference.ASet.mapA (fun v -> (mapping v).Reference))
       

    let chooseA (mapping : 'a -> aref<option<'b>>) (set : aset<'a>) =
        create
            (set.Adaptive |> Adaptive.ASet.chooseA (fun v -> (mapping v).Adaptive))
            (set.Reference |> Reference.ASet.chooseA (fun v -> (mapping v).Reference))
           

    let filterA (predicate : 'a -> aref<bool>) (set : aset<'a>) =
        create
            (set.Adaptive |> Adaptive.ASet.filterA (fun v -> (predicate v).Adaptive))
            (set.Reference |> Reference.ASet.filterA (fun v -> (predicate v).Reference))
                   

    let foldHalfGroup (add : 's -> 'a -> 's) (trySub : 's -> 'a -> Option<'s>) (zero : 's) (set : aset<'a>) =
        ARef.create
            (set.Adaptive |> Adaptive.ASet.foldHalfGroup add trySub zero)
            (set.Reference |> Reference.ASet.foldHalfGroup add trySub zero)
        
    let foldGroup (add : 's -> 'a -> 's) (sub : 's -> 'a -> 's) (zero : 's) (set : aset<'a>) =
        ARef.create
            (set.Adaptive |> Adaptive.ASet.foldGroup add sub zero)
            (set.Reference |> Reference.ASet.foldGroup add sub zero)
                
    let fold (add : 's -> 'a -> 's) (zero : 's) (set : aset<'a>) =
        ARef.create
            (set.Adaptive |> Adaptive.ASet.fold add zero)
            (set.Reference |> Reference.ASet.fold add zero)
                