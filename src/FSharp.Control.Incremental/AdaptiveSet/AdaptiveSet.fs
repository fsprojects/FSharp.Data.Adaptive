namespace FSharp.Control.Incremental

open FSharp.Control.Traceable

/// an adaptive reader for aset that allows to pull operations and exposes its current state.
type IHashSetReader<'a> = IOpReader<CountingHashSet<'a>, HashSetDelta<'a>>

/// adaptive set datastructure.
type AdaptiveHashSet<'a> =
    /// is the set constant?
    abstract member IsConstant : bool

    /// the current content of the set as aref.
    abstract member Content : aref<HashSet<'a>>

    /// gets a new reader to the set.
    abstract member GetReader : unit -> IHashSetReader<'a>

and aset<'T> = AdaptiveHashSet<'T>

/// internal implementations for aset operations.
module AdaptiveHashSetImplementation =

    /// core implementation for a dependent set.
    type AdaptiveHashSet<'a>(createReader : unit -> IOpReader<HashSetDelta<'a>>) =
        let history = History(createReader, CountingHashSet.trace)
        let content = history |> ARef.map CountingHashSet.toHashSet

        /// gets a new reader to the set.
        member x.GetReader() : IHashSetReader<'a> =
            history.NewReader()

        /// current content of the set as aref.
        member x.Content =
            content

        interface aset<'a> with
            member x.IsConstant = false
            member x.GetReader() = x.GetReader()
            member x.Content = x.Content

    /// efficient implementation for an empty adaptive set.
    type EmptySet<'a> private() =   
        static let instance = EmptySet<'a>() :> aset<_>
        let content = ARef.constant HashSet.empty
        let reader = new History.Readers.EmptyReader<CountingHashSet<'a>, HashSetDelta<'a>>(CountingHashSet.trace) :> IHashSetReader<'a>
        static member Instance = instance
        
        member x.Content = content
        member x.GetReader() = reader
        
        interface aset<'a> with
            member x.IsConstant = true
            member x.GetReader() = x.GetReader()
            member x.Content = x.Content

    /// efficient implementation for a constant adaptive set.
    type ConstantSet<'a>(content : Lazy<HashSet<'a>>) =
        let ref = ARef.delay (fun () -> content.Value)

        member x.Content = ref

        member x.GetReader() =
            new History.Readers.ConstantReader<_,_>(
                CountingHashSet.trace,
                lazy (HashSet.addAll content.Value),
                lazy (CountingHashSet.ofHashSet content.Value)
            ) :> IHashSetReader<_>

        interface aset<'a> with
            member x.IsConstant = true
            member x.GetReader() = x.GetReader()
            member x.Content = x.Content


    /// reader for map operations.
    type MapReader<'a, 'b>(input : aset<'a>, f : 'a -> 'b) =
        inherit AbstractReader<HashSetDelta<'b>>(HashSetDelta.monoid)
            
        let cache = Cache f
        let r = input.GetReader()

        override x.Compute(token) =
            r.GetOperations token |> HashSetDelta.map (fun d ->
                match d with
                | Add(1, v) -> Add(cache.Invoke v)
                | Rem(1, v) -> Rem(cache.Revoke v)
                | _ -> unexpected()
            )
          
    /// reader for choose operations.
    type ChooseReader<'a, 'b>(input : aset<'a>, mapping : 'a -> option<'b>) =
        inherit AbstractReader<HashSetDelta<'b>>(HashSetDelta.monoid)
            
        let cache = Cache mapping
        let r = input.GetReader()

        override x.Compute(token) =
            r.GetOperations token |> HashSetDelta.choose (fun d ->
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
        inherit AbstractReader<HashSetDelta<'a>>(HashSetDelta.monoid)
            
        let cache = Cache predicate
        let r = input.GetReader()

        override x.Compute(token) =
            r.GetOperations token |> HashSetDelta.filter (fun d ->
                match d with
                | Add(1, v) -> cache.Invoke v
                | Rem(1, v) -> cache.Revoke v
                | _ -> unexpected()
            )

    /// reader for fully dynamic uinon operations.
    type UnionReader<'a>(input : aset<aset<'a>>) =
        inherit AbstractDirtyReader<IHashSetReader<'a>, HashSetDelta<'a>>(HashSetDelta.monoid)

        let reader = input.GetReader()
        let cache = Cache(fun (inner : aset<'a>) -> inner.GetReader())

        override x.Compute(token, dirty) =
            let mutable deltas = 
                reader.GetOperations token |> HashSetDelta.collect (fun d ->
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
                deltas <- HashSetDelta.combine deltas (d.GetOperations token)

            deltas

    /// reader for unioning a constant set of asets.
    type UnionConstantReader<'a>(input : HashSet<aset<'a>>) =
        inherit AbstractDirtyReader<IHashSetReader<'a>, HashSetDelta<'a>>(HashSetDelta.monoid)

        let mutable isInitial = true
        let input = input |> HashSet.map (fun s -> s.GetReader())

        override x.Compute(token, dirty) = 
            if isInitial then
                isInitial <- false
                // initially we need to pull all inner readers.
                (HashSetDelta.empty, input) ||> HashSet.fold (fun deltas r ->   
                    HashSetDelta.combine deltas (r.GetOperations token)
                )
            else
                // once evaluated only dirty readers need to be pulled.
                (HashSetDelta.empty, dirty) ||> Seq.fold (fun deltas r -> 
                    HashSetDelta.combine deltas (r.GetOperations token)
                )

    /// reader for unioning a dynamic aset of immutable sets.
    type UnionHashSetReader<'a>(input : aset<HashSet<'a>>) =
        inherit AbstractReader<HashSetDelta<'a>>(HashSetDelta.monoid)

        let reader = input.GetReader()

        override x.Compute(token) =
            reader.GetOperations token |> HashSetDelta.collect (fun d ->
                match d with
                | Add(1, v) -> HashSet.addAll v
                | Rem(1, v) -> HashSet.removeAll v   
                | _ -> unexpected()
            )

    /// reader for collect operations.
    type CollectReader<'a, 'b>(input : aset<'a>, mapping : 'a -> aset<'b>) =
        inherit AbstractDirtyReader<IHashSetReader<'b>, HashSetDelta<'b>>(HashSetDelta.monoid)

        let reader = input.GetReader()
        let cache = Cache(fun value -> (mapping value).GetReader())

        override x.Compute(token,dirty) =
            let mutable deltas = 
                reader.GetOperations token |> HashSetDelta.collect (fun d ->
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
                            HashSetDelta.empty
                                
                    | _ -> unexpected()
                )
                
            // finally pull all the dirty readers and accumulate the deltas.
            for d in dirty do
                deltas <- HashSetDelta.combine deltas (d.GetOperations token)

            deltas

    /// reader for aref<HashSet<_>>
    type ARefReader<'s, 'a when 's :> seq<'a>>(input : aref<'s>) =
        inherit AbstractReader<HashSetDelta<'a>>(HashSetDelta.monoid)

        let mutable oldSet = HashSet.empty

        override x.Compute(token) =
            let newSet = input.GetValue token :> seq<_> |> HashSet.ofSeq
            let deltas = HashSet.differentiate oldSet newSet
            oldSet <- newSet
            deltas

    /// reader for bind operations.
    type BindReader<'a, 'b>(input : aref<'a>, mapping : 'a -> aset<'b>) =
        inherit AbstractReader<HashSetDelta<'b>>(HashSetDelta.monoid)
            
        let mutable refChanged = 0
        let mutable cache : option<'a * IHashSetReader<'b>> = None
            
        override x.InputChanged(_, i) =
            if System.Object.ReferenceEquals(i, input) then
                refChanged <- 1

        override x.Compute(token) =
            let newValue = input.GetValue token
            let refChanged = System.Threading.Interlocked.Exchange(&refChanged, 0) = 1

            match cache with
            | Some(oldValue, oldReader) when refChanged && not (cheapEqual oldValue newValue) ->
                // input changed
                let rem = CountingHashSet.removeAll oldReader.State
                oldReader.Outputs.Remove x |> ignore
                let newReader = (mapping newValue).GetReader()
                let add = newReader.GetOperations token
                cache <- Some(newValue, newReader)
                HashSetDelta.combine rem add

            | Some(_, ro) ->    
                // input unchanged
                ro.GetOperations token

            | None ->
                // initial
                let r = (mapping newValue).GetReader()
                cache <- Some(newValue, r)
                r.GetOperations token


    /// gets the current content of the aset as HashSet.
    let inline force (set : aset<'a>) = 
        ARef.force set.Content

    /// creates a constant set using the creation function.
    let inline constant (content : unit -> HashSet<'a>) = 
        ConstantSet(lazy(content())) :> aset<_> 

    /// creates an adaptive set using the reader.
    let inline create (reader : unit -> #IOpReader<HashSetDelta<'a>>) =
        AdaptiveHashSet(fun () -> reader() :> IOpReader<_>) :> aset<_>

/// functional operators for aset<_>
module ASet =
    open AdaptiveHashSetImplementation

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

    /// adaptively maps over the given set.
    let map (mapping : 'a -> 'b) (set : aset<'a>) =
        if set.IsConstant then
            constant (fun () -> set |> force |> HashSet.map mapping)
        else
            create (fun () -> MapReader(set, mapping))
          
    /// adaptively chooses all elements returned by mapping.  
    let choose (mapping : 'a -> option<'b>) (set : aset<'a>) =
        if set.IsConstant then
            constant (fun () -> set |> force |> HashSet.choose mapping)
        else
            create (fun () -> ChooseReader(set, mapping))
            
    /// adaptively filters the set using the given predicate.
    let filter (predicate : 'a -> bool) (set : aset<'a>) =
        if set.IsConstant then
            constant (fun () -> set |> force |> HashSet.filter predicate)
        else
            create (fun () -> FilterReader(set, predicate))

    /// adaptively unions all the given sets
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

    /// adaptively maps over the given set and unions all resulting sets.
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

    /// creates an aset for the given aref.
    let ofARef (ref : aref<#seq<'a>>) =
        if ref.IsConstant then
            constant (fun () -> ARef.force ref :> seq<'a> |> HashSet.ofSeq)
        else
            create (fun () -> ARefReader(ref))

    let bind (mapping : 'a -> aset<'b>) (ref : aref<'a>) =
        if ref.IsConstant then
            ref |> ARef.force |> mapping
        else
            create (fun () -> BindReader(ref, mapping))