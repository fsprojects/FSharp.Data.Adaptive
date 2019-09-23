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
                lazy (HashSet.addAll content.Value),
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
          
    /// reader for choose operations.
    type ChooseReader<'a, 'b>(input : aset<'a>, mapping : 'a -> Option<'b>) =
        inherit AbstractReader<DHashSet<'b>>(DHashSet.monoid)
            
        let cache = Cache mapping
        let r = input.GetReader()

        override x.Compute(token) =
            r.GetOperations token |> DHashSet.choose (fun d ->
                match d with
                | Add(1, v) -> 
                    match cache.Invoke v with
                    | Some v -> Some (Add v)
                    | None -> None

                | Rem(1, v) ->
                    match cache.Revoke v with
                    | Some v -> Some (Rem v)
                    | None -> None

                | _ -> 
                    unexpected()
            )

    /// reader for filter operations.
    type FilterReader<'a>(input : aset<'a>, predicate : 'a -> bool) =
        inherit AbstractReader<DHashSet<'a>>(DHashSet.monoid)
            
        let cache = Cache predicate
        let r = input.GetReader()

        override x.Compute(token) =
            r.GetOperations token |> DHashSet.filter (fun d ->
                match d with
                | Add(1, v) -> cache.Invoke v
                | Rem(1, v) -> cache.Revoke v
                | _ -> unexpected()
            )

    /// reader for fully dynamic uinon operations.
    type UnionReader<'a>(input : aset<aset<'a>>) =
        inherit AbstractDirtyReader<ISetReader<'a>, DHashSet<'a>>(DHashSet.monoid)

        let reader = input.GetReader()
        let cache = Cache(fun (inner : aset<'a>) -> inner.GetReader())

        override x.Compute(token, dirty) =
            let mutable deltas = 
                reader.GetOperations token |> DHashSet.collect (fun d ->
                    match d with
                    | Add(1, v) ->
                        // r is no longer dirty since we pull it here.
                        let r = cache.Invoke v
                        dirty.Remove r |> ignore
                        r.GetOperations token

                    | Rem(1, v) -> 
                        // r is no longer dirty since we either pull or destroy it here.
                        let deleted, r = cache.RevokeAndGetDeleted v
                        dirty.Remove r |> ignore
                        if deleted then 
                            // in case r was not dirty we need to explicitly remove ourselves
                            // from its output-set. if it was dirty it can't hurt to do so.
                            r.Outputs.Remove x |> ignore
                            CountingHashSet.removeAll r.State
                        else
                            r.GetOperations token
                                
                    | _ -> unexpected()
                )

            // finally pull all the dirty readers and accumulate the deltas.
            for d in dirty do
                deltas <- DHashSet.combine deltas (d.GetOperations token)

            deltas

    /// reader for unioning a constant set of asets.
    type UnionConstantReader<'a>(input : HashSet<aset<'a>>) =
        inherit AbstractDirtyReader<ISetReader<'a>, DHashSet<'a>>(DHashSet.monoid)

        let mutable isInitial = true
        let input = input |> HashSet.map (fun s -> s.GetReader())

        override x.Compute(token, dirty) = 
            if isInitial then
                isInitial <- false
                // initially we need to pull all inner readers.
                (DHashSet.empty, input) ||> HashSet.fold (fun deltas r ->   
                    DHashSet.combine deltas (r.GetOperations token)
                )
            else
                // once evaluated only dirty readers need to be pulled.
                (DHashSet.empty, dirty) ||> Seq.fold (fun deltas r -> 
                    DHashSet.combine deltas (r.GetOperations token)
                )

    /// reader for unioning a dynamic aset of immutable sets.
    type UnionHashSetReader<'a>(input : aset<HashSet<'a>>) =
        inherit AbstractReader<DHashSet<'a>>(DHashSet.monoid)

        let reader = input.GetReader()

        override x.Compute(token) =
            reader.GetOperations token |> DHashSet.collect (fun d ->
                match d with
                | Add(1, v) -> HashSet.addAll v
                | Rem(1, v) -> HashSet.removeAll v   
                | _ -> unexpected()
            )

    /// reader for collect operations.
    type CollectReader<'a, 'b>(input : aset<'a>, mapping : 'a -> aset<'b>) =
        inherit AbstractDirtyReader<ISetReader<'b>, DHashSet<'b>>(DHashSet.monoid)

        let reader = input.GetReader()
        let cache = Cache(fun value -> (mapping value).GetReader())

        override x.Compute(token,dirty) =
            let mutable deltas = 
                reader.GetOperations token |> DHashSet.collect (fun d ->
                    match d with
                    | Add(1, value) ->
                        // r is no longer dirty since we pull it here.
                        let r = cache.Invoke value
                        dirty.Remove r |> ignore
                        r.GetOperations token

                    | Rem(1, value) -> 
                        match cache.RevokeAndGetDeletedTotal value with
                        | Some (deleted, r) -> 
                            // r is no longer dirty since we either pull or destroy it here.
                            dirty.Remove r |> ignore
                            if deleted then 
                                // in case r was not dirty we need to explicitly remove ourselves
                                // from its output-set. if it was dirty it can't hurt to do so.
                                r.Outputs.Remove x |> ignore
                                CountingHashSet.removeAll r.State
                            else
                                r.GetOperations token
                        | None -> 
                            // weird
                            DHashSet.empty
                                
                    | _ -> unexpected()
                )
                
            // finally pull all the dirty readers and accumulate the deltas.
            for d in dirty do
                deltas <- DHashSet.combine deltas (d.GetOperations token)

            deltas


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
          
    /// incrementally chooses all elements returned by mapping.  
    let choose (mapping : 'a -> Option<'b>) (set : aset<'a>) =
        if set.IsConstant then
            constant (fun () -> set |> force |> HashSet.choose mapping)
        else
            create (fun () -> ChooseReader(set, mapping))
            
    /// incrementally filters the set using the given predicate.
    let filter (predicate : 'a -> bool) (set : aset<'a>) =
        if set.IsConstant then
            constant (fun () -> set |> force |> HashSet.filter predicate)
        else
            create (fun () -> FilterReader(set, predicate))

    /// incrementally unions all the given sets
    let union (sets : aset<aset<'a>>) = 
        if sets.IsConstant then
            // outer set is constant
            let all = force sets
            if all |> HashSet.forall (fun s -> s.IsConstant) then
                // all inner sets are constant
                constant (fun () -> all |> HashSet.collect force)
            else
                // inner sets non-constant
                create (fun () -> UnionConstantReader all)
        else
            // sadly no way to know if inner sets will always be constants.
            create (fun () -> UnionReader sets)

    /// incrementally maps over the given set and unions all resulting sets.
    let collect (mapping : 'a -> aset<'b>) (set : aset<'a>) =   
        if set.IsConstant then
            // outer set is constant
            let all = set |> force |> HashSet.map mapping
            if all |> HashSet.forall (fun s -> s.IsConstant) then
                // all inner sets are constant
                constant (fun () -> all |> HashSet.collect force)
            else
                // inner sets non-constant
                create (fun () -> UnionConstantReader all)
        else
            create (fun () -> CollectReader(set, mapping))
