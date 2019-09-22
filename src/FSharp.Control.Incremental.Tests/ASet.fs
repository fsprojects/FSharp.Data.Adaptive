module ASet

open Xunit
open FsCheck
open FSharp.Control
open FSharp.Control.Incremental
open FSharp.Control.Traceable
open FsUnit.Xunit
open FsCheck.Xunit

type Record<'a> = { value : 'a }

[<AutoOpen>]
module ASetTestImplementation =
    open FSharp.Control.Traceable
    
    type aset<'a> =
        abstract member Adaptive : Incremental.aset<'a>
        abstract member Reference : Reference.aset<'a>

    type ISetReader<'a> =
        abstract member Adaptive : Incremental.ISetReader<'a>
        abstract member Reference : (unit -> DHashSet<'a>)

    [<AutoOpen>]
    module ASetReaders = 
        type aset<'a> with
            member x.GetReader() =
                let adaptive = x.Adaptive.GetReader()
                let reference =
                    let mutable state = HashSet.empty
                    fun () ->
                        let newState = x.Reference.GetContent()
                        let ops = HashSet.differentiate state newState
                        state <- newState
                        ops
                    
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
            ARef.ARefTestImplementation.ARef.create
                (Incremental.ASet.toARef set.Adaptive)
                (Reference.ASet.toARef set.Reference)

        let map (mapping : 'a -> 'b) (set : aset<'a>) =
            create
                (Incremental.ASet.map mapping set.Adaptive)
                (Reference.ASet.map mapping set.Reference)

    let check (r : ISetReader<'a>) =
        let a = r.Adaptive.GetOperations AdaptiveToken.Top
        let r = r.Reference ()
        a |> should setequal r
        r

let emptyDelta = FSharp.Control.Traceable.DHashSet.empty<int>

[<Fact>]
let ``[CSet] reader add/remove/clear/union/except`` () =
    let set = cset<int>(HashSet.empty)

    let r = set.GetReader()
    r |> check |> should setequal emptyDelta

    // add 1
    transact (fun () -> set.Add 1) |> should be True
    set.Value |> should setequal [1]
    r |> check |> should setequal [Add 1]
    r.Adaptive.State |> should setequal [1]
    
    // add 1;2;3
    transact (fun () -> set.UnionWith [1;2;3])
    set.Value |> should setequal [1;2;3]
    r |> check |> should setequal [Add 2; Add 3]
    r.Adaptive.State |> should setequal [1;2;3]

    // remove 1;3
    transact (fun () -> set.ExceptWith [1;3])
    set.Value |> should setequal [2]
    r |> check |> should setequal [Rem 1; Rem 3]
    r.Adaptive.State |> should setequal [2]

    /// clear
    transact (fun () -> set.Clear())
    set.Value |> should setequal List.empty<int>
    r |> check |> should setequal [Rem 2]
    r.Adaptive.State |> should setequal List.empty<int>

[<Fact>]
let ``[CSet] contains/isEmpty/count`` () =
    let set = cset(HashSet.ofList [1;2])

    set.IsEmpty |> should be False
    set.Count |> should equal 2
    set.Contains 1 |> should be True
    set.Contains 2 |> should be True

    transact (fun () ->
        set.Remove 2 |> should be True
    )
    
    set.IsEmpty |> should be False
    set.Count |> should equal 1
    set.Contains 1 |> should be True
    set.Contains 2 |> should be False

    
    transact (fun () ->
        set.Remove 1 |> should be True
    )
    
    set.IsEmpty |> should be True
    set.Count |> should equal 0
    set.Contains 1 |> should be False
    set.Contains 2 |> should be False

[<Property>]
let ``[ASet] map``(values : list<Set<int>>) =
    let initial = cset(HashSet.empty)
    let derived = ASet.map (fun a -> a * 10) initial
    let reader = derived.GetReader()

    reader |> check |> should setequal emptyDelta

    let mutable i = 0
    for s in values do
        let s = 
            if i % 7 < 3 then Set.add i s
            else s

        transact (fun () -> initial.Value <- HashSet.ofSeq s)
        reader |> check |> ignore
        i <- i + 1





