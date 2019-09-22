namespace FSharp.Control.Incremental

// TODO: documentation

open FSharp.Control.Traceable

type ISetReader<'a> = IOpReader<CountingHashSet<'a>, DHashSet<'a>>

type aset<'a> =
    abstract member IsConstant : bool
    abstract member Content : aref<HashSet<'a>>
    abstract member GetReader : unit -> ISetReader<'a>

module ASetImplementation =
    type AdaptiveSet<'a>(createReader : unit -> IOpReader<DHashSet<'a>>) =
        let history = History(createReader, CountingHashSet.trace)

        let content = history |> ARef.map CountingHashSet.toHashSet

        member x.GetReader() : ISetReader<'a> =
            history.NewReader()

        member x.Content =
            content

        interface aset<'a> with
            member x.IsConstant = false
            member x.GetReader() = x.GetReader()
            member x.Content = x.Content

    type EmptySet<'a> private() =   
        static let instance = EmptySet<'a>() :> aset<_>
        let content = ARef.constant HashSet.empty
        let reader = new History.Readers.EmptyReader<CountingHashSet<'a>, DHashSet<'a>>(CountingHashSet.trace) :> ISetReader<'a>
        static member Instance = instance
        
        member x.Content = content
        member x.GetReader() = reader
        
        interface aset<'a> with
            member x.IsConstant = true
            member x.GetReader() = x.GetReader()
            member x.Content = x.Content

    type ConstantSet<'a>(content : Lazy<HashSet<'a>>) =
        let ref = ARef.delay (fun () -> content.Value)

        member x.Content = ref

        member x.GetReader() =
            new History.Readers.ConstantReader<_,_>(
                CountingHashSet.trace,
                lazy (HashSet.differentiate HashSet.empty content.Value),
                lazy (CountingHashSet.ofHashSet content.Value)
            ) :> ISetReader<_>

        interface aset<'a> with
            member x.IsConstant = true
            member x.GetReader() = x.GetReader()
            member x.Content = x.Content

    type MapReader<'a, 'b>(input : aset<'a>, f : 'a -> 'b) =
        inherit AbstractReader<DHashSet<'b>>(DHashSet.monoid)
            
        let cache = Cache f
        let r = input.GetReader()

        override x.Compute(token) =
            r.GetOperations token |> DHashSet.map (fun d ->
                match d with
                    | Add(1, v) -> Add(cache.Invoke v)
                    | Rem(1, v) -> Rem(cache.Revoke v)
                    | _ -> unexpected()
            )
          

    let inline force (set : aset<'a>) = 
        ARef.force set.Content

    let inline constant (content : unit -> HashSet<'a>) = 
        ConstantSet(lazy(content())) :> aset<_> 

    let inline create (reader : unit -> #IOpReader<DHashSet<'a>>) =
        AdaptiveSet(fun () -> reader() :> IOpReader<_>) :> aset<_>

module ASet =
    open ASetImplementation

    [<GeneralizableValue>]
    let empty<'a> : aset<'a> = 
        EmptySet<'a>.Instance

    let single (value : 'a) =
        lazy (HashSet.single value) |> ConstantSet :> aset<_>
        
    let ofSeq (s : seq<'a>) =
        lazy (HashSet.ofSeq s) |> ConstantSet :> aset<_>

    let ofList (s : list<'a>) =
        lazy (HashSet.ofList s) |> ConstantSet :> aset<_>
        
    let ofArray (s : 'a[]) =
        lazy (HashSet.ofArray s) |> ConstantSet :> aset<_>

    let ofHashSet (elements : HashSet<'a>) =
        ConstantSet(lazy elements) :> aset<_>

    let toARef (set : aset<'a>) =
        set.Content

    let map (mapping : 'a -> 'b) (set : aset<'a>) =
        if set.IsConstant then
            constant (fun () -> set |> force |> HashSet.map mapping)
        else
            create (fun () -> MapReader(set, mapping))
            


