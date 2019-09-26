namespace FSharp.Data.Adaptive.Validation

open FSharp.Data.Adaptive
open FSharp.Data
open FsUnit

type aval<'T> =
    abstract member Adaptive : Adaptive.aval<'T>
    abstract member Reference : Reference.aval<'T>

type cval<'T>(value : 'T) = 
    let adaptive = Adaptive.cval value
    let reference = Reference.cval value

    member x.Value 
        with get() = reference.Value
        and set v =
            reference.Value <- v
            adaptive.Value <- v

    interface aval<'T> with
        member x.Adaptive = adaptive :> Adaptive.aval<_>
        member x.Reference = reference :> Reference.aval<_>

module AVal =

    let create (adaptive : Adaptive.aval<'T>) (reference : Reference.aval<'T>) =
        { new aval<'T> with
            member x.Adaptive = adaptive
            member x.Reference = reference
        }

    let force (value : aval<'T>) =
        let adaptive = Adaptive.AVal.force value.Adaptive
        let reference = Reference.AVal.force value.Reference
        (adaptive, reference)

    let init (value : 'T) =
        cval<'T>(value)

    let constant (value : 'T) =
        create 
            (Adaptive.AVal.constant value)
            (Reference.AVal.constant value)

    let map (mapping : 'T1 -> 'T2) (r : aval<'T1>) =
        create
            (Adaptive.AVal.map mapping r.Adaptive)
            (Reference.AVal.map mapping r.Reference)
 
    let map2 (mapping : 'T1 -> 'T2 -> 'T3) (value1 : aval<'T1>) (value2 : aval<'T2>) =
        create
            (Adaptive.AVal.map2 mapping value1.Adaptive value2.Adaptive)
            (Reference.AVal.map2 mapping value1.Reference value2.Reference)
 
    let map3 (mapping : 'T1 -> 'T2 -> 'T3 -> 'T4) (value1 : aval<'T1>) (value2 : aval<'T2>) (value3 : aval<'T3>) =
        create
            (Adaptive.AVal.map3 mapping value1.Adaptive value2.Adaptive value3.Adaptive)
            (Reference.AVal.map3 mapping value1.Reference value2.Reference value3.Reference)
 
    let bind (mapping : 'T1 -> aval<'T2>) (r : aval<'T1>) =
        create
            (Adaptive.AVal.bind (fun v -> mapping(v).Adaptive) r.Adaptive)
            (Reference.AVal.bind (fun v -> mapping(v).Reference) r.Reference)

    let bind2 (mapping : 'T1 -> 'T2 -> aval<'T3>) (r1 : aval<'T1>) (r2 : aval<'T2>) =
        create
            (Adaptive.AVal.bind2 (fun va vb -> (mapping va vb).Adaptive) r1.Adaptive r2.Adaptive)
            (Reference.AVal.bind2 (fun va vb -> (mapping va vb).Reference) r1.Reference r2.Reference)


type aset<'T> =
    abstract member Adaptive : Adaptive.aset<'T>
    abstract member Reference : Reference.aset<'T>

type IHashSetReader<'T> =
    abstract member Adaptive : Adaptive.IHashSetReader<'T>
    abstract member Reference : Reference.IHashSetReader<'T>

[<AutoOpen>]
module ASetReaders = 
    type aset<'T> with
        member x.GetReader() =
            let adaptive = x.Adaptive.GetReader()
            let reference = x.Reference.GetReader()
            { new IHashSetReader<'T> with
                member __.Adaptive = adaptive
                member __.Reference = reference
            }

type cset<'T>(value : HashSet<'T>) = 
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

    member x.Add (value : 'T) =
        let wa = adaptive.Add value
        let wr = reference.Add value
        wa |> should equal wr
        wr

    member x.Remove(value : 'T) =
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

    interface aset<'T> with
        member x.Adaptive = adaptive :> _
        member x.Reference = reference :> _

module ASet =
    let create (a : Adaptive.aset<'T>) (r : Reference.aset<'T>) =
        { new aset<'T> with
            member x.Adaptive = a
            member x.Reference = r
        }

    [<GeneralizableValue>]
    let empty<'T> : aset<'T> = 
        create (Adaptive.ASet.empty) (Reference.ASet.empty)
            
    let single (value : 'T) = 
        create (Adaptive.ASet.single value) (Reference.ASet.single value)

    let ofSeq (value : seq<'T>) = 
        create (Adaptive.ASet.ofSeq value) (Reference.ASet.ofSeq value)

    let ofList (value : list<'T>) = 
        create (Adaptive.ASet.ofList value) (Reference.ASet.ofList value)

    let ofArray (value : array<'T>) = 
        create (Adaptive.ASet.ofArray value) (Reference.ASet.ofArray value)

    let ofHashSet (value : HashSet<'T>) = 
        create (Adaptive.ASet.ofHashSet value) (Reference.ASet.ofHashSet value)

    let toAVal (set : aset<'T>) =
        AVal.create
            (Adaptive.ASet.toAVal set.Adaptive)
            (Reference.ASet.toAVal set.Reference)

    let map (mapping : 'T1 -> 'T2) (set : aset<'T1>) =
        create
            (Adaptive.ASet.map mapping set.Adaptive)
            (Reference.ASet.map mapping set.Reference)

    let choose (mapping : 'T1 -> option<'T2>) (set : aset<'T1>) =
        create
            (Adaptive.ASet.choose mapping set.Adaptive)
            (Reference.ASet.choose mapping set.Reference)

    let filter (predicate : 'T1 -> bool) (set : aset<'T1>) =
        create
            (Adaptive.ASet.filter predicate set.Adaptive)
            (Reference.ASet.filter predicate set.Reference)
            
    let union (sets : aset<aset<'T1>>) =
        create
            (Adaptive.ASet.union (sets.Adaptive |> Adaptive.ASet.map (fun s -> s.Adaptive)))
            (Reference.ASet.union (sets.Reference |> Reference.ASet.map (fun s -> s.Reference)))

    let collect (mapping : 'T1 -> aset<'T2>) (set : aset<'T1>) =
        create
            (Adaptive.ASet.collect (fun v -> (mapping v).Adaptive) set.Adaptive)
            (Reference.ASet.collect (fun v -> (mapping v).Reference) set.Reference)

    let ofAVal (value : aval<#seq<'T1>>) =
        create
            (value.Adaptive |> Adaptive.ASet.ofAVal)
            (value.Reference |> Reference.ASet.ofAVal)

    let bind (mapping : 'T1 -> aset<'T2>) (value : aval<'T1>) =
        create
            (value.Adaptive |> Adaptive.ASet.bind (fun v -> (mapping v).Adaptive))
            (value.Reference |> Reference.ASet.bind (fun v -> (mapping v).Reference))
         

    let mapA (mapping : 'T1 -> aval<'T2>) (set : aset<'T1>) =
        create
            (set.Adaptive |> Adaptive.ASet.mapA (fun v -> (mapping v).Adaptive))
            (set.Reference |> Reference.ASet.mapA (fun v -> (mapping v).Reference))
       

    let chooseA (mapping : 'T1 -> aval<option<'T2>>) (set : aset<'T1>) =
        create
            (set.Adaptive |> Adaptive.ASet.chooseA (fun v -> (mapping v).Adaptive))
            (set.Reference |> Reference.ASet.chooseA (fun v -> (mapping v).Reference))
           

    let filterA (predicate : 'T -> aval<bool>) (set : aset<'T>) =
        create
            (set.Adaptive |> Adaptive.ASet.filterA (fun v -> (predicate v).Adaptive))
            (set.Reference |> Reference.ASet.filterA (fun v -> (predicate v).Reference))
                   

    let foldHalfGroup (add : 'S -> 'T1 -> 'S) (trySub : 'S -> 'T1 -> option<'S>) (zero : 'S) (set : aset<'T1>) =
        AVal.create
            (set.Adaptive |> Adaptive.ASet.foldHalfGroup add trySub zero)
            (set.Reference |> Reference.ASet.foldHalfGroup add trySub zero)
        
    let foldGroup (add : 'S -> 'T1 -> 'S) (sub : 'S -> 'T1 -> 'S) (zero : 'S) (set : aset<'T1>) =
        AVal.create
            (set.Adaptive |> Adaptive.ASet.foldGroup add sub zero)
            (set.Reference |> Reference.ASet.foldGroup add sub zero)
                
    let fold (add : 'S -> 'T -> 'S) (zero : 'S) (set : aset<'T>) =
        AVal.create
            (set.Adaptive |> Adaptive.ASet.fold add zero)
            (set.Reference |> Reference.ASet.fold add zero)
                