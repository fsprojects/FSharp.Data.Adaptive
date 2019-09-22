namespace FSharp.Control.Incremental

open FSharp.Control.Traceable

/// an incremental reader for aset that allows to pull operations and exposes its current state.
type ISetReader<'a> = IOpReader<CountingHashSet<'a>, DHashSet<'a>>

/// incremental set datastructure.
type aset<'a> =
    /// is the set constant?
    abstract member IsConstant : bool

    /// the current content of the set as aref.
    abstract member Content : aref<HashSet<'a>>

    /// gets a new reader to the set.
    abstract member GetReader : unit -> ISetReader<'a>

/// internal implementations for aset operations.
module ASetImplementation =
    /// core implementation for a dependent set.
    type AdaptiveSet<'a>(createReader : unit -> IOpReader<DHashSet<'a>>) =
        let history = History(createReader, CountingHashSet.trace)
        let content = history |> ARef.map CountingHashSet.toHashSet

        /// gets a new reader to the set.
        member x.GetReader() : ISetReader<'a> =
            history.NewReader()

        /// current content of the set as aref.
        member x.Content =
            content

        interface aset<'a> with
            member x.IsConstant = false
            member x.GetReader() = x.GetReader()
            member x.Content = x.Content

    /// efficient implementation for an empty incremental set.
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

    /// efficient implementation for a constant incremental set.
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

    /// reader for map operations.
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
          
    /// gets the current content of the aset as HashSet.
    let inline force (set : aset<'a>) = 
        ARef.force set.Content

    /// creates a constant set using the creation function.
    let inline constant (content : unit -> HashSet<'a>) = 
        ConstantSet(lazy(content())) :> aset<_> 

    /// creates an incremental set using the reader.
    let inline create (reader : unit -> #IOpReader<DHashSet<'a>>) =
        AdaptiveSet(fun () -> reader() :> IOpReader<_>) :> aset<_>

/// functional operators for aset<_>
module ASet =
    open ASetImplementation

    /// the empty aset.
    [<GeneralizableValue>]
    let empty<'a> : aset<'a> = 
        EmptySet<'a>.Instance

    /// a constant aset holding a single value.
    let single (value : 'a) =
        lazy (HashSet.single value) |> ConstantSet :> aset<_>
        
    /// creates an aset holding the given values.
    let ofSeq (s : seq<'a>) =
        lazy (HashSet.ofSeq s) |> ConstantSet :> aset<_>
        
    /// creates an aset holding the given values.
    let ofList (s : list<'a>) =
        lazy (HashSet.ofList s) |> ConstantSet :> aset<_>
        
    /// creates an aset holding the given values.
    let ofArray (s : 'a[]) =
        lazy (HashSet.ofArray s) |> ConstantSet :> aset<_>
        
    /// creates an aset holding the given values. `O(1)`
    let ofHashSet (elements : HashSet<'a>) =
        ConstantSet(lazy elements) :> aset<_>

    /// creates an aref providing access to the current content of the set.
    let toARef (set : aset<'a>) =
        set.Content

    /// incrementally maps over the given set.
    let map (mapping : 'a -> 'b) (set : aset<'a>) =
        if set.IsConstant then
            constant (fun () -> set |> force |> HashSet.map mapping)
        else
            create (fun () -> MapReader(set, mapping))
            


