namespace FSharp.Data.Adaptive

open FSharp.Data.Traceable

/// An adaptive reader for aset that allows to pull operations and exposes its current state.
type IHashSetReader<'T> = IOpReader<CountingHashSet<'T>, HashSetDelta<'T>>

/// Adaptive set datastructure.
type IAdaptiveHashSet<'T> =
    /// Is the set constant?
    abstract member IsConstant : bool

    /// The current content of the set as aval.
    abstract member Content : aval<HashSet<'T>>

    /// Gets a new reader to the set.
    abstract member GetReader : unit -> IHashSetReader<'T>

    /// Gets the underlying History instance for the aset (if any)
    abstract member History : option<History<CountingHashSet<'T>, HashSetDelta<'T>>>

and aset<'T> = IAdaptiveHashSet<'T>


/// Internal implementations for aset reductions.
module SetReductions =
    
    /// aval for reduce operations.
    type ReduceValue<'a, 's, 'v>(reduction : AdaptiveReduction<'a, 's, 'v>, input : aset<'a>) =
        inherit AdaptiveObject()

        let reader = input.GetReader()
        let mutable sum = reduction.seed

        let mutable result = Unchecked.defaultof<'v>

        member x.GetValue(token : AdaptiveToken) =
            x.EvaluateAlways token (fun token ->
                if x.OutOfDate then
                    let ops = reader.GetChanges token
                    let mutable working = true
                    use e = (ops :> seq<_>).GetEnumerator()
                    while working && e.MoveNext() do
                        let op = e.Current
                        match op with
                        | Add(_, a) ->
                            sum <- reduction.add sum a

                        | Rem(_, old) ->
                            match reduction.sub sum old with
                            | ValueSome s -> sum <- s
                            | ValueNone -> working <- false

                    if not working then
                        sum <- reader.State |> Seq.fold reduction.add reduction.seed

                    result <- reduction.view sum

                result
            )

        interface IAdaptiveValue with
            member x.GetValueUntyped t = 
                x.GetValue t :> obj
            member x.ContentType =
                #if FABLE_COMPILER
                typeof<obj>
                #else
                typeof<'v>
                #endif

        interface IAdaptiveValue<'v> with
            member x.GetValue t = x.GetValue t
            
    /// aval for reduceBy operations.
    type ReduceByValue<'a, 'b, 's, 'v>(reduction : AdaptiveReduction<'b, 's, 'v>, mapping : 'a -> 'b, input : aset<'a>) =
        inherit AdaptiveObject()

        let reader = input.GetReader()
        let mutable sum = ValueSome reduction.seed

        let mutable result = Unchecked.defaultof<'v>
        let mutable state = HashMap.empty<'a, 'b>

        let add (s : ValueOption<'s>) (v : 'b) =
            match s with
            | ValueSome s -> ValueSome (reduction.add s v)
            | ValueNone -> ValueNone
            
        let sub (s : ValueOption<'s>) (v : 'b) =
            match s with
            | ValueSome s -> reduction.sub s v
            | ValueNone -> ValueNone

        member x.GetValue(token : AdaptiveToken) =
            x.EvaluateAlways token (fun token ->
                if x.OutOfDate then
                    let ops = reader.GetChanges token
                    for op in ops do
                        match op with
                        | Add(_, a) ->
                            match HashMap.tryFind a state with
                            | Some old -> sum <- sub sum old
                            | None -> ()

                            let b = mapping a

                            sum <- add sum b
                            state <- HashMap.add a b state

                        | Rem(_, a) ->
                            match HashMap.tryRemove a state with
                            | Some(old, rest) ->
                                state <- rest
                                sum <- sub sum old
                            | None ->
                                ()

                    match sum with
                    | ValueSome s ->
                        result <- reduction.view s
                    | ValueNone ->
                        let s = state |> HashMap.fold (fun s _ v -> reduction.add s v) reduction.seed
                        sum <- ValueSome s
                        result <- reduction.view s

                result
            )

        interface IAdaptiveValue with
            member x.GetValueUntyped t = 
                x.GetValue t :> obj
            member x.ContentType =
                #if FABLE_COMPILER
                typeof<obj>
                #else
                typeof<'v>
                #endif

        interface IAdaptiveValue<'v> with
            member x.GetValue t = x.GetValue t

    /// aval for reduceByA operations.
    type AdaptiveReduceByValue<'a, 'b, 's, 'v>(reduction : AdaptiveReduction<'b, 's, 'v>, mapping : 'a -> aval<'b>, l : aset<'a>) =
        inherit AdaptiveObject()

        let reader = l.GetReader()
        do reader.Tag <- "FoldReader"

        #if !FABLE_COMPILER
        let dirtyLock = obj()
        #endif


        let mutable targets = MultiSetMap.empty<aval<'b>, 'a>
        let mutable state = HashMap.empty<'a, aval<'b> * 'b>

        let mutable dirty : HashMap<'a, aval<'b>> = HashMap.empty
        let mutable sum = ValueSome reduction.seed
        let mutable res = Unchecked.defaultof<'v>

        let sub (s : ValueOption<'s>) (v : 'b) =
            match s with
            | ValueSome s -> reduction.sub s v
            | ValueNone -> ValueNone
            
        let add (s : ValueOption<'s>) (v : 'b) =
            match s with
            | ValueSome s -> ValueSome (reduction.add s v)
            | ValueNone -> ValueNone


        let consumeDirty() =
            #if FABLE_COMPILER
            let d = dirty
            dirty <- HashMap.empty
            d
            #else
            lock dirtyLock (fun () ->
                let d = dirty
                dirty <- HashMap.empty
                d
            )
            #endif

        let removeIndex (x : AdaptiveReduceByValue<_,_,_,_>) (i : 'a) =
            match HashMap.tryRemove i state with
            | Some ((ov, o), newState) ->
                state <- newState
                sum <- sub sum o    
                let rem, newTargets = MultiSetMap.remove ov i targets
                targets <- newTargets
                if rem then ov.Outputs.Remove x |> ignore
                
            | None ->
                ()

        override x.InputChangedObject(t, o) =
            if isNull o.Tag then
                #if FABLE_COMPILER
                let o = unbox<aval<'b>> o
                for i in MultiSetMap.find o targets do
                    dirty <- HashMap.add i o dirty
                #else
                match o with
                | :? aval<'b> as o ->
                    lock dirtyLock (fun () -> 
                        for i in MultiSetMap.find o targets do
                            dirty <- HashMap.add i o dirty
                    )
                | _ -> 
                    ()
                #endif
 
        member x.GetValue (t : AdaptiveToken) =
            x.EvaluateAlways t (fun t ->
                if x.OutOfDate then
                    let ops = reader.GetChanges t
                    let mutable dirty = consumeDirty()
                    for op in ops do
                        dirty <- HashMap.remove op.Value dirty
                        match op with
                        | Add(_, v) ->
                            removeIndex x v

                            let r = mapping v
                            let n = r.GetValue(t)
                            targets <- MultiSetMap.add r v targets
                            state <- HashMap.add v (r, n) state
                            sum <- add sum n

                        | Rem(_, v) ->
                            removeIndex x v


                    for (i, r) in dirty do
                        let n = r.GetValue(t)
                        state <-
                            state |> HashMap.alter i (fun old ->
                                match old with
                                | Some (ro, o) -> 
                                    assert(ro = r)
                                    sum <- add (sub sum o) n
                                | None -> 
                                    sum <- add sum n
                                Some (r, n)
                            )

                    match sum with
                    | ValueNone ->
                        let s = state |> HashMap.fold (fun s _ (_,v) -> reduction.add s v) reduction.seed
                        sum <- ValueSome s
                        res <- reduction.view s
                    | ValueSome s ->
                        res <- reduction.view s

                res
            )

        interface IAdaptiveValue with
            member x.GetValueUntyped t = x.GetValue t :> obj
            member x.ContentType =
                #if FABLE_COMPILER
                typeof<obj>
                #else
                typeof<'v>
                #endif

        interface IAdaptiveValue<'v> with
            member x.GetValue t = x.GetValue t  
  

/// Internal implementations for aset operations.
module AdaptiveHashSetImplementation =

    let inline checkTag (value : 'a) (real : obj) = DefaultEquality.equals (value :> obj) real
    
    /// Core implementation for a dependent set.
    type AdaptiveHashSetImpl<'T>(createReader : unit -> IOpReader<HashSetDelta<'T>>) =
        let history = History(createReader, CountingHashSet.trace)
        let content = history |> AVal.map CountingHashSet.toHashSet

        /// Gets a new reader to the set.
        member x.GetReader() : IHashSetReader<'T> =
            history.NewReader()

        /// Current content of the set as aval.
        member x.Content =
            content

        interface IAdaptiveHashSet<'T> with
            member x.IsConstant = false
            member x.GetReader() = x.GetReader()
            member x.Content = x.Content
            member x.History = Some history

    /// Efficient implementation for an empty adaptive set.
    type EmptySet<'T> private() =   
        static let instance = EmptySet<'T>() :> aset<_>
        let content = AVal.constant HashSet.empty
        let reader = new History.Readers.EmptyReader<CountingHashSet<'T>, HashSetDelta<'T>>(CountingHashSet.trace) :> IHashSetReader<'T>
        static member Instance = instance
        
        member x.Content = content
        member x.GetReader() = reader
        
        interface IAdaptiveHashSet<'T> with
            member x.IsConstant = true
            member x.GetReader() = x.GetReader()
            member x.Content = x.Content
            member x.History = None

    /// Efficient implementation for a constant adaptive set.
    type ConstantSet<'T>(content : Lazy<HashSet<'T>>) =
        let value = AVal.delay (fun () -> content.Value)

        member x.Content = value

        member x.GetReader() =
            new History.Readers.ConstantReader<_,_>(
                CountingHashSet.trace,
                lazy (HashSet.addAll content.Value),
                lazy (CountingHashSet.ofHashSet content.Value)
            ) :> IHashSetReader<_>

        interface IAdaptiveHashSet<'T> with
            member x.IsConstant = true
            member x.GetReader() = x.GetReader()
            member x.Content = x.Content
            member x.History = None


    /// Reader for map operations.
    type MapReader<'A, 'B>(input : aset<'A>, mapping : 'A -> 'B) =
        inherit AbstractReader<HashSetDelta<'B>>(HashSetDelta.empty)
            
        let cache = Cache mapping
        let reader = input.GetReader()
        
        static member DeltaMapping (mapping : 'A -> 'B) =    
            let cache = Cache mapping
            HashSetDelta.map (fun d ->
                match d with
                | Add(1, v) -> Add(cache.Invoke v)
                | Rem(1, v) -> Rem(cache.Revoke v)
                | _ -> unexpected()
            )

        override x.Compute(token) =
            reader.GetChanges token |> HashSetDelta.map (fun d ->
                match d with
                | Add(1, v) -> Add(cache.Invoke v)
                | Rem(1, v) -> Rem(cache.Revoke v)
                | _ -> unexpected()
            )
          
    /// Reader for choose operations.
    type ChooseReader<'A, 'B>(input : aset<'A>, mapping : 'A -> option<'B>) =
        inherit AbstractReader<HashSetDelta<'B>>(HashSetDelta.empty)
            
        let cache = Cache mapping
        let r = input.GetReader()
  
        static member DeltaMapping (mapping : 'A -> option<'B>) =    
            let cache = Cache mapping
            HashSetDelta.choose (fun d ->
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
      
        override x.Compute(token) =
            r.GetChanges token |> HashSetDelta.choose (fun d ->
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

    /// Reader for filter operations.
    type FilterReader<'T>(input : aset<'T>, predicate : 'T -> bool) =
        inherit AbstractReader<HashSetDelta<'T>>(HashSetDelta.empty)
            
        let cache = Cache predicate
        let r = input.GetReader()

        static member DeltaMapping (predicate : 'T -> bool) =    
            let cache = Cache predicate
            HashSetDelta.filter (fun d ->
                match d with
                | Add(1, v) -> cache.Invoke v
                | Rem(1, v) -> cache.Revoke v
                | _ -> unexpected()
            )
      
        override x.Compute(token) =
            r.GetChanges token |> HashSetDelta.filter (fun d ->
                match d with
                | Add(1, v) -> cache.Invoke v
                | Rem(1, v) -> cache.Revoke v
                | _ -> unexpected()
            )

    /// Reader for fully dynamic uinon operations.
    type UnionReader<'T>(input : aset<aset<'T>>) =
        inherit AbstractDirtyReader<IHashSetReader<'T>, HashSetDelta<'T>>(HashSetDelta.monoid, checkTag "InnerReader")

        let reader = input.GetReader()
        let cache = 
            Cache(fun (inner : aset<'T>) -> 
                let r = inner.GetReader()
                r.Tag <- "InnerReader"
                r
            )
        
        override x.Compute(token, dirty) =
            let mutable deltas = 
                reader.GetChanges token |> HashSetDelta.collect (fun d ->
                    match d with
                    | Add(1, v) ->
                        // r is no longer dirty since we pull it here.
                        let r = cache.Invoke v
                        dirty.Remove r |> ignore
                        r.GetChanges token

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
                            r.GetChanges token
                                
                    | _ -> unexpected()
                )

            // finally pull all the dirty readers and accumulate the deltas.
            for d in dirty do
                deltas <- HashSetDelta.combine deltas (d.GetChanges token)

            deltas

    /// Reader for unioning a constant set of asets.
    type UnionConstantReader<'T>(input : HashSet<aset<'T>>) =
        inherit AbstractDirtyReader<IHashSetReader<'T>, HashSetDelta<'T>>(HashSetDelta.monoid, checkTag "InnerReader")

        let mutable isInitial = true
        let input = 
            input |> HashSet.map (fun s -> 
                let r = s.GetReader()
                r.Tag <- "InnerReader"
                r
            )
        
        override x.Compute(token, dirty) = 
            if isInitial then
                isInitial <- false
                // initially we need to pull all inner readers.
                (HashSetDelta.empty, input) ||> HashSet.fold (fun deltas r ->   
                    HashSetDelta.combine deltas (r.GetChanges token)
                )
            else
                // once evaluated only dirty readers need to be pulled.
                (HashSetDelta.empty, dirty) ||> Seq.fold (fun deltas r -> 
                    HashSetDelta.combine deltas (r.GetChanges token)
                )

    /// Reader for unioning a dynamic aset of immutable sets.
    type UnionHashSetReader<'T>(input : aset<HashSet<'T>>) =
        inherit AbstractReader<HashSetDelta<'T>>(HashSetDelta.empty)

        let reader = input.GetReader()

        override x.Compute(token) =
            reader.GetChanges token |> HashSetDelta.collect (fun d ->
                match d with
                | Add(1, v) -> HashSet.addAll v
                | Rem(1, v) -> HashSet.removeAll v   
                | _ -> unexpected()
            )

    /// Reader for collect operations.
    type CollectReader<'A, 'B>(input : aset<'A>, mapping : 'A -> aset<'B>) =
        inherit AbstractDirtyReader<IHashSetReader<'B>, HashSetDelta<'B>>(HashSetDelta.monoid, checkTag "InnerReader")

        let reader = input.GetReader()
        let cache = 
            Cache(fun value -> 
                let reader = (mapping value).GetReader()
                reader.Tag <- "InnerReader"
                reader
            )

        override x.Compute(token,dirty) =
            let mutable deltas = 
                reader.GetChanges token |> HashSetDelta.collect (fun d ->
                    match d with
                    | Add(1, value) ->
                        // r is no longer dirty since we pull it here.
                        let r = cache.Invoke value
                        dirty.Remove r |> ignore
                        r.GetChanges token

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
                                r.GetChanges token
                        | None -> 
                            // weird
                            HashSetDelta.empty
                                
                    | _ -> unexpected()
                )
                
            // finally pull all the dirty readers and accumulate the deltas.
            for d in dirty do
                deltas <- HashSetDelta.combine deltas (d.GetChanges token)

            deltas

    /// Reader for aval<HashSet<_>>
    type AValReader<'S, 'A when 'S :> seq<'A>>(input : aval<'S>) =
        inherit AbstractReader<HashSetDelta<'A>>(HashSetDelta.empty)

        let mutable oldSet = HashSet.empty

        override x.Compute(token) =
            let newSet = input.GetValue token :> seq<_> |> HashSet.ofSeq
            let deltas = HashSet.computeDelta oldSet newSet
            oldSet <- newSet
            deltas

    /// Reader for bind operations.
    type BindReader<'A, 'B>(input : aval<'A>, mapping : 'A -> aset<'B>) =
        inherit AbstractReader<HashSetDelta<'B>>(HashSetDelta.empty)
            
        let mutable valChanged = 0
        let mutable cache : option<'A * IHashSetReader<'B>> = None
            
        override x.InputChangedObject(_, i) =
            if System.Object.ReferenceEquals(i, input) then
                valChanged <- 1

        override x.Compute(token) =
            let newValue = input.GetValue token
            #if FABLE_COMPILER
            let valChanged = let v = valChanged in valChanged <- 0; v = 1
            #else
            let valChanged = System.Threading.Interlocked.Exchange(&valChanged, 0) = 1
            #endif 

            match cache with
            | Some(oldValue, oldReader) when valChanged && not (cheapEqual oldValue newValue) ->
                // input changed
                let rem = CountingHashSet.removeAll oldReader.State
                oldReader.Outputs.Remove x |> ignore
                let newReader = (mapping newValue).GetReader()
                let add = newReader.GetChanges token
                cache <- Some(newValue, newReader)
                HashSetDelta.combine rem add

            | Some(_, ro) ->    
                // input unchanged
                ro.GetChanges token

            | None ->
                // initial
                let r = (mapping newValue).GetReader()
                cache <- Some(newValue, r)
                r.GetChanges token

    /// Reader for flattenA
    type FlattenAReader<'T>(input : aset<aval<'T>>) =
        inherit AbstractDirtyReader<aval<'T>, HashSetDelta<'T>>(HashSetDelta.monoid, isNull)
            
        let r = input.GetReader()
        do r.Tag <- "Input"

        let mutable initial = true
        let cache = DefaultDictionary.create<aval<'T>, 'T>()

        member x.Invoke(token : AdaptiveToken, m : aval<'T>) =
            let v = m.GetValue token
            cache.[m] <- v
            v

        member x.Invoke2(token : AdaptiveToken, m : aval<'T>) =
            let o = cache.[m]
            let v = m.GetValue token
            cache.[m] <- v
            o, v

        member x.Revoke(m : aval<'T>, dirty : System.Collections.Generic.HashSet<_>) =
            match cache.TryGetValue m with
            | (true, v) -> 
                cache.Remove m |> ignore
                m.Outputs.Remove x |> ignore
                dirty.Remove m |> ignore
                v
            | _ -> 
                failwith "[ASet] cannot remove unknown object"

        override x.Compute(token, dirty) =
            let mutable deltas = 
                r.GetChanges token |> HashSetDelta.map (fun d ->
                    match d with
                    | Add(1,m) -> Add(x.Invoke(token, m))
                    | Rem(1,m) -> Rem(x.Revoke(m, dirty))
                    | _ -> unexpected()
                )

            for d in dirty do
                let o, n = x.Invoke2(token, d)
                if not (DefaultEquality.equals o n) then
                    deltas <- HashSetDelta.combine deltas (HashSetDelta.ofList [Add n; Rem o])

            deltas
            

    /// Reader for mapA
    type MapAReader<'A, 'B>(input : aset<'A>, mapping : 'A -> aval<'B>) =
        inherit AbstractDirtyReader<aval<'B>, HashSetDelta<'B>>(HashSetDelta.monoid, isNull)
            
        let reader = input.GetReader()
        do reader.Tag <- "Reader"
        let mapping = Cache mapping
        let cache = DefaultDictionary.create<aval<'B>, ref<int * 'B>>()

        member x.Invoke(token : AdaptiveToken, v : 'A) =
            let m = mapping.Invoke v
            let v = m.GetValue token
            match cache.TryGetValue m with
            | (true, r) ->
                r := (fst !r + 1, v)
            | _ ->
                let r = ref (1, v)
                cache.[m] <- r

            v

        member x.Invoke2(token : AdaptiveToken, m : aval<'B>) =
            let r = cache.[m]
            let v = m.GetValue token
            let (rc, o) = !r
            r := (rc, v)
            o, v

        member x.Revoke(v : 'A, dirty : System.Collections.Generic.HashSet<_>) =
            let m = mapping.Revoke v
                
            match cache.TryGetValue m with
            | (true, r) -> 
                let (cnt, v) = !r
                if cnt = 1 then
                    cache.Remove m |> ignore
                    dirty.Remove m |> ignore
                    lock m (fun () -> m.Outputs.Remove x |> ignore )
                    v
                else
                    r := (cnt - 1, v)
                    v
            | _ -> 
                failwith "[ASet] cannot remove unknown object"

        override x.Compute(token, dirty) =
            let mutable deltas = 
                reader.GetChanges token |> HashSetDelta.map (fun d ->
                    match d with
                    | Add(1,m) -> Add(x.Invoke(token,m))
                    | Rem(1,m) -> Rem(x.Revoke(m, dirty))
                    | _ -> unexpected()
                )

            for d in dirty do
                let o, n = x.Invoke2(token, d)
                if not (DefaultEquality.equals o n) then
                    deltas <- HashSetDelta.combine deltas (HashSetDelta.ofList [Add n; Rem o])

            deltas
            
    /// Reader for chooseA
    type ChooseAReader<'A, 'B>(input : aset<'A>, f : 'A -> aval<option<'B>>) =
        inherit AbstractDirtyReader<aval<option<'B>>, HashSetDelta<'B>>(HashSetDelta.monoid, isNull)
            
        let r = input.GetReader()
        do r.Tag <- "Reader"

        let f = Cache f
        let mutable initial = true
        let cache = DefaultDictionary.create<aval<option<'B>>, ref<int * option<'B>>>()

        member x.Invoke(token : AdaptiveToken, v : 'A) =
            let m = f.Invoke v
            let v = m.GetValue token

            match cache.TryGetValue m with
            | (true, r) ->
                r := (fst !r + 1, v)
            | _ ->
                let r = ref (1, v)
                cache.[m] <- r

            v


        member x.Invoke2(token : AdaptiveToken, m : aval<option<'B>>) =
            match cache.TryGetValue m with
            | (true, r) ->
                let (rc, o) = !r
                let v = m.GetValue token
                r := (rc, v)
                o, v
            | _ ->
                None, None  

        member x.Revoke(v : 'A) =
            let m = f.Revoke v
            match cache.TryGetValue m with
            | (true, r) -> 
                let (rc, v) = !r
                if rc = 1 then
                    cache.Remove m |> ignore
                    lock m (fun () -> m.Outputs.Remove x |> ignore )
                else
                    r := (rc - 1, v)
                v
            | _ -> 
                failwith "[ASet] cannot remove unknown object"


        override x.Compute(token, dirty) =
            let mutable deltas = 
                r.GetChanges token |> HashSetDelta.choose (fun d ->
                    match d with
                    | Add(1,m) -> 
                        match x.Invoke(token,m) with
                        | Some v -> Some (Add v)
                        | None -> None

                    | Rem(1,m) ->
                        match x.Revoke m with
                        | Some v -> Some (Rem v)
                        | None -> None

                    | _ -> 
                        unexpected()
                )

            for d in dirty do
                match x.Invoke2(token, d) with
                | None, Some n ->
                    deltas <- 
                        deltas
                        |> HashSetDelta.add (Add n)

                | Some o, None ->
                    deltas <- 
                        deltas
                        |> HashSetDelta.add (Rem o)

                | Some o, Some n when not (DefaultEquality.equals o n) ->
                    deltas <- 
                        deltas
                        |> HashSetDelta.add (Rem o)
                        |> HashSetDelta.add (Add n)

                | _ ->
                    ()

            deltas
 
    /// Gets the current content of the aset as HashSet.
    let inline force (set : aset<'T>) = 
        AVal.force set.Content

/// Functional operators for aset<_>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ASet =
    open AdaptiveHashSetImplementation

    /// Creates a constant set using the creation function.
    let constant (content : unit -> HashSet<'T>) = 
        ConstantSet(lazy(content())) :> aset<_> 

    /// Creates an aset using the given reader-creator.
    let ofReader (reader : unit -> #IOpReader<HashSetDelta<'T>>) =
        AdaptiveHashSetImpl(fun () -> reader() :> IOpReader<_>) :> aset<_>

    /// The empty aset.
    [<GeneralizableValue>]
    let empty<'T> : aset<'T> = 
        EmptySet<'T>.Instance

    /// A constant aset holding a single value.
    let single (value : 'T) =
        constant (fun () -> HashSet.single value)
        
    /// Creates an aset holding the given values.
    let ofSeq (s : seq<'T>) =
        constant (fun () -> HashSet.ofSeq s)
        
    /// Creates an aset holding the given values.
    let ofList (s : list<'T>) =
        constant (fun () -> HashSet.ofList s)
        
    /// Creates an aset holding the given values.
    let ofArray (s : 'T[]) =
        constant (fun () -> HashSet.ofArray s)
        
    /// Creates an aset holding the given values. `O(1)`
    let ofHashSet (elements : HashSet<'T>) =
        constant (fun () -> elements)

    /// Creates an aval providing access to the current content of the set.
    let toAVal (set : aset<'T>) =
        set.Content

    /// Adaptively maps over the given set.
    let map (mapping : 'A -> 'B) (set : aset<'A>) =
        if set.IsConstant then
            constant (fun () -> set |> force |> HashSet.map mapping)
        else
            match set.History with
            | Some history ->
                ofReader (fun () -> 
                    history.NewReader(CountingHashSet.trace, MapReader.DeltaMapping mapping)
                )
            | _ ->
                ofReader (fun () -> MapReader(set, mapping))
          
    /// Adaptively chooses all elements returned by mapping.  
    let choose (mapping : 'A -> option<'B>) (set : aset<'A>) =
        if set.IsConstant then
            constant (fun () -> set |> force |> HashSet.choose mapping)
        else
            match set.History with
            | Some history ->
                ofReader (fun () -> 
                    history.NewReader(CountingHashSet.trace, ChooseReader.DeltaMapping mapping)
                )
            | _ ->
                ofReader (fun () -> ChooseReader(set, mapping))
            
    /// Adaptively filters the set using the given predicate.
    let filter (predicate : 'A -> bool) (set : aset<'A>) =
        if set.IsConstant then
            constant (fun () -> set |> force |> HashSet.filter predicate)
        else
            match set.History with
            | Some history ->
                ofReader (fun () -> 
                    history.NewReader(CountingHashSet.trace, FilterReader.DeltaMapping predicate)
                )
            | _ ->
                ofReader (fun () -> FilterReader(set, predicate))
            
    /// Adaptively unions the given sets
    let union (a : aset<'A>) (b : aset<'A>) =
        if a = b then
            a
        elif a.IsConstant && b.IsConstant then
            let va = force a
            let vb = force b
            if va.IsEmpty && vb.IsEmpty then empty
            else constant (fun () -> HashSet.union va vb)
        else
            // TODO: can be optimized in case one of the two sets is constant.
            ofReader (fun () -> UnionConstantReader (HashSet.ofList [a;b]))

    /// Adaptively unions all the given sets
    let unionMany (sets : aset<aset<'A>>) = 
        if sets.IsConstant then
            // outer set is constant
            let all = force sets
            if all |> HashSet.forall (fun s -> s.IsConstant) then
                // all inner sets are constant
                constant (fun () -> all |> HashSet.collect force)
            else
                // inner sets non-constant
                ofReader (fun () -> UnionConstantReader all)
        else
            // sadly no way to know if inner sets will always be constants.
            ofReader (fun () -> UnionReader sets)

    /// Adaptively maps over the given set and unions all resulting sets.
    let collect (mapping : 'A -> aset<'B>) (set : aset<'A>) =   
        if set.IsConstant then
            // outer set is constant
            let all = set |> force |> HashSet.map mapping
            if all |> HashSet.forall (fun s -> s.IsConstant) then
                // all inner sets are constant
                constant (fun () -> all |> HashSet.collect force)
            else
                // inner sets non-constant
                ofReader (fun () -> UnionConstantReader all)
        else
            ofReader (fun () -> CollectReader(set, mapping))

    /// Creates an aset for the given aval.
    let ofAVal (value : aval<#seq<'T>>) =
        if value.IsConstant then
            constant (fun () -> AVal.force value :> seq<'T> |> HashSet.ofSeq)
        else
            ofReader (fun () -> AValReader(value))

    /// Adaptively maps over the given aval and returns the resulting set.
    let bind (mapping : 'A -> aset<'B>) (value : aval<'A>) =
        if value.IsConstant then
            value |> AVal.force |> mapping
        else
            ofReader (fun () -> BindReader(value, mapping))

    /// Adaptively flattens the set of adaptive avals.
    let flattenA (set : aset<aval<'A>>) =
        if set.IsConstant then
            let all = set |> force
            if all |> HashSet.forall (fun r -> r.IsConstant) then
                constant (fun () -> all |> HashSet.map AVal.force)
            else
                // TODO: better implementation possible
                ofReader (fun () -> FlattenAReader(set))
        else
            ofReader (fun () -> FlattenAReader(set))
            
    /// Adaptively maps over the set and also respects inner changes.
    let mapA (mapping : 'A -> aval<'B>) (set : aset<'A>) =
        // TODO: constants
        ofReader (fun () -> MapAReader(set, mapping))

    /// Adaptively maps over the set and also respects inner changes.
    let chooseA (mapping : 'A -> aval<option<'B>>) (set : aset<'A>) =
        // TODO: constants
        ofReader (fun () -> ChooseAReader(set, mapping))

    /// Adaptively filters the set and also respects inner changes.
    let filterA (predicate : 'A -> aval<bool>) (set : aset<'A>) =
        // TODO: direct implementation
        ofReader (fun () -> 
            let mapping (a : 'A) =  
                predicate a 
                |> AVal.map (function true -> Some a | false -> None)

            ChooseAReader(set, mapping)
        )

    /// Evaluates the given adaptive set and returns its current content.
    /// This should not be used inside the adaptive evaluation
    /// of other AdaptiveObjects since it does not track dependencies.
    let force (set : aset<'T>) = AVal.force set.Content

    /// Reduces the set using the given `AdaptiveReduction` and returns
    /// the resulting adaptive value.
    let reduce (r : AdaptiveReduction<'a, 's, 'v>) (list: aset<'a>) =
        SetReductions.ReduceValue(r, list) :> aval<'v>
        
    /// Applies the mapping function to all elements of the set and reduces the results
    /// using the given `AdaptiveReduction`.
    /// Returns the resulting adaptive value.
    let reduceBy (r : AdaptiveReduction<'b, 's, 'v>) (mapping: 'a -> 'b) (list: aset<'a>) =
        SetReductions.ReduceByValue(r, mapping, list) :> aval<'v>
        
    /// Applies the mapping function to all elements of the set and reduces the results
    /// using the given `AdaptiveReduction`.
    /// Returns the resulting adaptive value.
    let reduceByA (r : AdaptiveReduction<'b, 's, 'v>) (mapping: 'a -> aval<'b>) (list: aset<'a>) =
        SetReductions.AdaptiveReduceByValue(r, mapping, list) :> aval<'v>

    /// Adaptively folds over the set using add for additions and trySubtract for removals.
    /// Note the trySubtract may return None indicating that the result needs to be recomputed.
    /// Also note that the order of elements given to add/trySubtract is undefined.
    let foldHalfGroup (add : 'S -> 'A -> 'S) (trySub : 'S -> 'A -> option<'S>) (zero : 'S) (s : aset<'A>) =
        let inline trySub s v =
            match trySub s v with
            | Some v -> ValueSome v
            | None -> ValueNone
        let reduction = AdaptiveReduction.halfGroup zero add trySub
        reduce reduction s

    /// Adaptively folds over the set using add for additions and recomputes the value on every removal.
    /// Note that the order of elements given to add is undefined.
    let fold (f : 'S -> 'A -> 'S) (seed : 'S) (s : aset<'A>) =
        let reduction = AdaptiveReduction.fold seed f
        reduce reduction s

    /// Adaptively folds over the set using add for additions and subtract for removals.
    /// Note that the order of elements given to add/subtract is undefined.
    let foldGroup (add : 'S -> 'A -> 'S) (sub : 'S -> 'A -> 'S) (zero : 'S) (s : aset<'A>) =
        let reduction = AdaptiveReduction.group zero add sub
        reduce reduction s

    /// Creates a constant aset lazy content.
    let delay (creator : unit -> HashSet<'T>) =
        constant creator

    /// Adaptively tests if the list is empty.
    let isEmpty (l: aset<'a>) =
        l.Content |> AVal.map HashSet.isEmpty
        
    /// Adaptively gets the number of elements in the list.
    let count (l: aset<'a>) =
        l.Content |> AVal.map HashSet.count

    let forall (predicate: 'T -> bool) (list: aset<'T>) =
        let r = AdaptiveReduction.countNegative |> AdaptiveReduction.mapOut (fun v -> v = 0)
        reduceBy r predicate list
        
    let exists (predicate: 'T -> bool) (list: aset<'T>) =
        let r = AdaptiveReduction.countPositive |> AdaptiveReduction.mapOut (fun v -> v <> 0)
        reduceBy r predicate list
        
    let forallA (predicate: 'T -> aval<bool>) (list: aset<'T>) =
        let r = AdaptiveReduction.countNegative |> AdaptiveReduction.mapOut (fun v -> v = 0)
        reduceByA r predicate list
        
    let existsA (predicate: 'T -> aval<bool>) (list: aset<'T>) =
        let r = AdaptiveReduction.countPositive |> AdaptiveReduction.mapOut (fun v -> v <> 0)
        reduceByA r predicate list

        
    /// Adaptively counts all elements fulfilling the predicate
    let countBy (predicate: 'a -> bool) (list: aset<'a>) =
        reduceBy AdaptiveReduction.countPositive predicate list 

    /// Adaptively counts all elements fulfilling the predicate
    let countByA (predicate: 'a -> aval<bool>) (list: aset<'a>) =
        reduceByA AdaptiveReduction.countPositive predicate list 

    let inline tryMin (l : aset<'a>) =
        let reduction = 
            AdaptiveReduction.tryMin
            |> AdaptiveReduction.mapOut (function ValueSome v -> Some v | ValueNone -> None)
        reduce reduction l

    let inline tryMax (l : aset<'a>) =
        let reduction = 
            AdaptiveReduction.tryMax
            |> AdaptiveReduction.mapOut (function ValueSome v -> Some v | ValueNone -> None)
        reduce reduction l

    /// Adaptively computes the sum all entries in the list.
    let inline sum (s : aset<'a>) = 
        reduce (AdaptiveReduction.sum()) s
    
    let inline sumBy (mapping : 'T1 -> 'T2) (list : aset<'T1>) =
        reduceBy (AdaptiveReduction.sum()) mapping list

    let inline sumByA (mapping : 'T1 -> aval<'T2>) (list : aset<'T1>) =
        reduceByA (AdaptiveReduction.sum()) mapping list

    let inline average (s : aset<'a>) =
        reduce (AdaptiveReduction.average()) s
        
    let inline averageBy (mapping : 'T1 -> 'T2) (list : aset<'T1>) =
        reduceBy (AdaptiveReduction.average()) mapping list

    let inline averageByA (mapping : 'T1 -> aval<'T2>) (list : aset<'T1>) =
        reduceByA (AdaptiveReduction.average()) mapping list

    let inline range (lowerBound: aval< ^T >) (upperBound: aval< ^T >) =
        if lowerBound.IsConstant && upperBound.IsConstant then
            constant (fun () -> HashSet.ofSeq (seq { AVal.force lowerBound .. AVal.force upperBound }))
        else
            ofReader (fun () -> 
                let zero = LanguagePrimitives.GenericZero< ^T >
                let one = LanguagePrimitives.GenericOne< ^T >
                let minusOne = -one
                let mutable lastMax = minusOne
                let mutable lastMin = zero
                { new AbstractReader<HashSetDelta< ^T >>(HashSetDelta.empty) with 
                    override x.Compute(token) =
                        let newMin = lowerBound.GetValue(token)
                        let newMax = upperBound.GetValue(token)

                        let (maxIncreaseLow, maxIncreaseHigh), 
                            (maxDecreaseLow, maxDecreaseHigh),
                            (minDecreaseLow, minDecreaseHigh),
                            (minIncreaseLow, minIncreaseHigh) = 
                              RangeDelta.rangeChange (lastMin, lastMax, newMin, newMax) 

                        let mutable delta = HashSetDelta.empty

                        // Count up through additions caused by increasing maximum 
                        for i in maxIncreaseLow .. maxIncreaseHigh do  
                            delta <- delta.Add(SetOperation.Add i)

                        // Count down through removals caused by decreasing maximum
                        for i in maxDecreaseLow .. minusOne .. maxDecreaseHigh do  
                            delta <- delta.Add(SetOperation.Rem i)

                        // Count down through additions caused by decreasing minimum
                        for i in minDecreaseLow .. minusOne .. minDecreaseHigh do  
                            delta <- delta.Add(SetOperation.Add i)

                        // Count up through removals caused by increasing minimum
                        for i in minIncreaseLow .. minIncreaseHigh do  
                            delta <- delta.Add(SetOperation.Rem i)

                        lastMax <- newMax
                        lastMin <- newMin
                        delta })
