namespace FSharp.Data.Adaptive

open FSharp.Data.Traceable
open System

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
    
    /// aval for contains queries.
    [<Sealed>]
    type ContainsValue<'T>(input : aset<'T>, value : 'T) =
        inherit AbstractVal<bool>()

        let mutable refCount = 0
        let reader = input.GetReader()

        override x.Compute (token : AdaptiveToken) =
            let ops = reader.GetChanges token |> HashSetDelta.toHashMap
            match HashMap.tryFindV value ops with
            | ValueSome delta ->
                refCount <- refCount + delta
            | ValueNone ->
                ()
            refCount > 0

    /// aval for reduce operations.
    [<Sealed>]
    type ReduceValue<'a, 's, 'v>(reduction : AdaptiveReduction<'a, 's, 'v>, input : aset<'a>) =
        inherit AdaptiveObject()

        let reader = input.GetReader()
        let mutable sum = reduction.seed

        let mutable result = Unchecked.defaultof<'v>

        member x.GetValue(token : AdaptiveToken) =
            x.EvaluateAlways token (fun token ->
                if x.OutOfDate then
                    let ops = reader.GetChanges token

                    if reader.State.Count <= 2 || reader.State.Count <= ops.Count then
                        sum <- reader.State |> CountingHashSet.fold reduction.add reduction.seed
                    else
                        let mutable working = true
                        let mutable e = ops.GetEnumerator()
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
                            sum <- reader.State |> CountingHashSet.fold reduction.add reduction.seed

                    result <- reduction.view sum

                result
            )

        interface IAdaptiveValue with
            member x.Accept (v : IAdaptiveValueVisitor<'R>) = v.Visit x
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
    [<Sealed>]
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

                    if reader.State.Count <= 2 || reader.State.Count <= ops.Count then
                        let newState =
                            reader.State.Store |> HashMap.map (fun a _ ->
                                match state.TryFindV a with
                                | ValueSome b -> b
                                | ValueNone -> mapping a
                            )

                        let s = newState |> HashMap.fold (fun s _ v -> reduction.add s v) reduction.seed
                        state <- newState
                        sum <- ValueSome s
                        result <- reduction.view s

                    else
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
            member x.Accept (v : IAdaptiveValueVisitor<'R>) = v.Visit x
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
    [<Sealed>]
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

                    if state.Count <= 2 || state.Count <= ops.Count then
                        dirty <- HashMap.empty
                        targets |> HashMap.iter (fun m _ ->
                            m.Outputs.Remove x |> ignore
                        )
                        targets <- HashMap.empty

                        let newState =  
                            reader.State.Store |> HashMap.map (fun k _ ->
                                match HashMap.tryFindV k state with
                                | ValueSome(m,_) ->
                                    let v = m.GetValue t
                                    targets <- MultiSetMap.add m k targets
                                    (m, v)
                                | _ ->
                                    let m = mapping k
                                    let v = m.GetValue t
                                    targets <- MultiSetMap.add m k targets
                                    (m, v)
                            )
                        state <- newState
                        let s = state |> HashMap.fold (fun s _ (_,v) -> reduction.add s v) reduction.seed
                        sum <- ValueSome s
                        res <- reduction.view s
                    else

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
            member x.Accept (v : IAdaptiveValueVisitor<'R>) = v.Visit x
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
    [<Sealed>]
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
    [<Sealed>]
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
    [<Sealed>]
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
    [<Sealed>]
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
        

    /// Reader for mapUse operations.
    [<Sealed>]
    type MapUseReader<'A, 'B when 'B :> IDisposable>(input : aset<'A>, mapping : 'A -> 'B) =
        inherit AbstractReader<HashSetDelta<'B>>(HashSetDelta.empty)
            
        let cache = Cache(mapping)
        let mutable disposeDelta = HashSetDelta.empty
        let mutable reader = input.GetReader()

        member x.Dispose() =
            lock x (fun () ->
                cache.Clear(fun d -> 
                    disposeDelta <- HashSetDelta.add (Rem d) disposeDelta
                    (d :> IDisposable).Dispose()
                )
                reader <- Unchecked.defaultof<_>
            )
            match Transaction.Current with
            | ValueSome t -> t.Enqueue x
            | ValueNone -> transact x.MarkOutdated

        interface System.IDisposable with
            member x.Dispose() = x.Dispose()

        override x.Compute(token) =
            if Unchecked.isNull reader then 
                let d = disposeDelta
                disposeDelta <- HashSetDelta.empty
                d
            else
                reader.GetChanges token |> HashSetDelta.choose (fun d ->
                    match d with
                    | Add(1, v) -> 
                        Add(cache.Invoke v) |> Some
                    | Rem(1, v) -> 
                        match cache.RevokeAndGetDeletedTotal v with
                        | Some (del, v) -> 
                            if del then (v :> IDisposable).Dispose()
                            Rem v |> Some
                        | None ->
                            None
                    | _ -> 
                        unexpected()
                )
            

    /// Reader for collect operations with inner constants.
    [<Sealed>]
    type CollectSeqReader<'A, 'B>(input : aset<'A>, mapping : 'A -> seq<'B>) =
        inherit AbstractReader<HashSetDelta<'B>>(HashSetDelta.empty)
            
        let cache = Cache (mapping >> HashSet.ofSeq)
        let reader = input.GetReader()
        
        static member DeltaMapping (mapping : 'A -> seq<'B>) =    
            let cache = Cache (mapping >> HashSet.ofSeq)
            HashSetDelta.collect (fun d ->
                match d with
                | Add(1, v) -> HashSet.computeDelta HashSet.empty (cache.Invoke v)
                | Rem(1, v) -> HashSet.computeDelta (cache.Revoke v) HashSet.empty
                | _ -> unexpected()
            )

        override x.Compute(token) =
            reader.GetChanges token |> HashSetDelta.collect (fun d ->
                match d with
                | Add(1, v) -> HashSet.computeDelta HashSet.empty (cache.Invoke v)
                | Rem(1, v) -> HashSet.computeDelta (cache.Revoke v) HashSet.empty
                | _ -> unexpected()
            )
          
    /// Reader for choose operations.
    [<Sealed>]
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
    [<Sealed>]
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

    /// Reader for fully dynamic union operations.
    [<Sealed>]
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
            
    /// Reader for binary difference operations.
    [<Sealed>]
    type DifferenceReader<'T>(set1 : aset<'T>, set2 : aset<'T>) =
        inherit AbstractReader<HashSetDelta<'T>>(HashSetDelta.empty)
        
        let mutable state = HashMap.empty
        let r1 = set1.GetReader()
        let r2 = set2.GetReader()

        override x.Compute(token : AdaptiveToken) =
            let changes1 = r1.GetChanges token |> HashSetDelta.toHashMap
            let changes2 = r2.GetChanges token |> HashSetDelta.toHashMap
            let changes = (changes1, changes2) ||> HashMap.map2V (fun _k l r -> struct(l,r))

            let inline apply (_key : 'T) (value : voption<struct(int * int)>) (struct(delta1 : voption<int>, delta2 : voption<int>)) : struct(voption<struct(int * int)> * voption<int>) = 
                let struct(oldRef1, oldRef2) =
                    match value with
                    | ValueSome s -> s
                    | ValueNone -> struct(0, 0)

                let newRef1 = 
                    match delta1 with
                    | ValueSome d1 -> oldRef1 + d1
                    | ValueNone -> oldRef1

                let newRef2 = 
                    match delta2 with
                    | ValueSome d2 -> oldRef2 + d2
                    | ValueNone -> oldRef2

                let oldRef = oldRef1 - oldRef2
                let newRef = newRef1 - newRef2

                let outDelta =
                    if newRef > 0 && oldRef <= 0 then ValueSome 1
                    elif newRef <= 0 && oldRef > 0 then ValueSome -1
                    else ValueNone

                let outRef =
                    if newRef1 >= 0 || newRef2 >= 0 then ValueSome(struct (newRef1, newRef2))
                    else ValueNone

                struct (outRef, outDelta)
                
            let newState, delta = HashMap.ApplyDelta(state, changes, apply)
            state <- newState
            HashSetDelta.ofHashMap delta

    /// Reader for binary intersect operations.
    [<Sealed>]
    type IntersectReader<'T>(set1 : aset<'T>, set2 : aset<'T>) =
        inherit AbstractReader<HashSetDelta<'T>>(HashSetDelta.empty)

        let mutable state : HashMap<'T, struct(int * int)> = HashMap.empty
        let r1 = set1.GetReader()
        let r2 = set2.GetReader()

        override x.Compute (token : AdaptiveToken) = 
            let changes1 = r1.GetChanges token |> HashSetDelta.toHashMap
            let changes2 = r2.GetChanges token |> HashSetDelta.toHashMap
            let changes = (changes1, changes2) ||> HashMap.map2V (fun _k l r -> struct(l,r))

            let inline apply (_key : 'T) (value : voption<struct(int * int)>) (struct(delta1 : voption<int>, delta2 : voption<int>)) : struct(voption<struct(int * int)> * voption<int>) = 
                let struct(oldRef1, oldRef2) =
                    match value with
                    | ValueSome s -> s
                    | ValueNone -> struct(0, 0)

                let newRef1 = 
                    match delta1 with
                    | ValueSome d1 -> oldRef1 + d1
                    | ValueNone -> oldRef1

                let newRef2 = 
                    match delta2 with
                    | ValueSome d2 -> oldRef2 + d2
                    | ValueNone -> oldRef2

                let oldRef = min oldRef1 oldRef2
                let newRef = min newRef1 newRef2

                let outDelta =
                    if newRef > 0 && oldRef <= 0 then ValueSome 1
                    elif newRef <= 0 && oldRef > 0 then ValueSome -1
                    else ValueNone

                let outRef =
                    if newRef1 >= 0 || newRef2 >= 0 then ValueSome(struct (newRef1, newRef2))
                    else ValueNone

                struct (outRef, outDelta)
                
            let newState, delta = HashMap.ApplyDelta(state, changes, apply)
            state <- newState
            HashSetDelta.ofHashMap delta
                  
    /// Reader for binary difference operations.
    [<Sealed>]
    type XorReader<'T>(set1 : aset<'T>, set2 : aset<'T>) =
        inherit AbstractReader<HashSetDelta<'T>>(HashSetDelta.empty)
        
        let mutable state = HashMap.empty
        let r1 = set1.GetReader()
        let r2 = set2.GetReader()

        override x.Compute(token : AdaptiveToken) =
            let changes1 = r1.GetChanges token |> HashSetDelta.toHashMap
            let changes2 = r2.GetChanges token |> HashSetDelta.toHashMap
            let changes = (changes1, changes2) ||> HashMap.map2V (fun _k l r -> struct(l,r))

            let inline apply (_key : 'T) (value : voption<struct(int * int)>) (struct(delta1 : voption<int>, delta2 : voption<int>)) : struct(voption<struct(int * int)> * voption<int>) = 
                let struct(oldRef1, oldRef2) =
                    match value with
                    | ValueSome s -> s
                    | ValueNone -> struct(0, 0)

                let newRef1 = 
                    match delta1 with
                    | ValueSome d1 -> oldRef1 + d1
                    | ValueNone -> oldRef1

                let newRef2 = 
                    match delta2 with
                    | ValueSome d2 -> oldRef2 + d2
                    | ValueNone -> oldRef2

                let oldRef = (oldRef1 + oldRef2) % 2
                let newRef = (newRef1 + newRef2) % 2

                let outDelta =
                    if newRef > 0 && oldRef <= 0 then ValueSome 1
                    elif newRef <= 0 && oldRef > 0 then ValueSome -1
                    else ValueNone

                let outRef =
                    if newRef1 >= 0 || newRef2 >= 0 then ValueSome(struct (newRef1, newRef2))
                    else ValueNone

                struct (outRef, outDelta)
                
            let newState, delta = HashMap.ApplyDelta(state, changes, apply)
            state <- newState
            HashSetDelta.ofHashMap delta

    /// Reader for unioning a constant set of asets.
    [<Sealed>]
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
    [<Sealed>]
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
    [<Sealed>]
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
    [<Sealed>]
    type AValReader<'S, 'A when 'S :> seq<'A>>(input : aval<'S>) =
        inherit AbstractReader<HashSetDelta<'A>>(HashSetDelta.empty)

        let mutable oldSet = HashSet.empty

        override x.Compute(token) =
            let newSet = input.GetValue token :> seq<_> |> HashSet.ofSeq
            let deltas = HashSet.computeDelta oldSet newSet
            oldSet <- newSet
            deltas

    /// Reader for bind operations.
    [<Sealed>]
    type BindReader<'A, 'B>(input : aval<'A>, mapping : 'A -> aset<'B>) =
        inherit AbstractReader<HashSetDelta<'B>>(HashSetDelta.empty)
            
        let mutable valChanged = 0
        let mutable cache : option<'A * IHashSetReader<'B>> = None
            
        override x.InputChangedObject(_, i) =
            // check if input is different object
            // NOTE: in case of 'input' being a MapFastVal, that forwrads the Outputs set of its input, 
            // this callback will actually be invoked by the input of the MapFastVal
            //  -> test Outputs set for equality instead of AdaptiveObjects to dected 'valChanged' in all cases
            if System.Object.ReferenceEquals(i.Outputs, input.Outputs) then
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
    [<Sealed>]
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
    [<Sealed>]
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
    [<Sealed>]
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
    let constant (value : unit -> HashSet<'T>) = 
        ConstantSet(lazy(value())) :> aset<_> 

    /// Creates an aset using the given reader-creator.
    let ofReader (create : unit -> #IOpReader<HashSetDelta<'T>>) =
        AdaptiveHashSetImpl(fun () -> create() :> IOpReader<_>) :> aset<_>
        
    /// Creates an aset using the given compute function
    let custom (compute : AdaptiveToken -> CountingHashSet<'a> -> HashSetDelta<'a>) : aset<'a> = 
        ofReader (fun () -> 
            { new AbstractReader<CountingHashSet<'a>,HashSetDelta<'a>>(CountingHashSet.trace) with
                override x.Compute(t) = 
                    compute t x.State
            }
        )

    /// The empty aset.
    [<GeneralizableValue>]
    let empty<'T> : aset<'T> = 
        EmptySet<'T>.Instance

    /// A constant aset holding a single value.
    let single (value : 'T) =
        constant (fun () -> HashSet.single value)
        
    /// Creates an aset holding the given values.
    let ofSeq (elements : seq<'T>) =
        constant (fun () -> HashSet.ofSeq elements)
        
    /// Creates an aset holding the given values.
    let ofList (elements : list<'T>) =
        constant (fun () -> HashSet.ofList elements)
        
    /// Creates an aset holding the given values.
    let ofArray (elements : 'T[]) =
        constant (fun () -> HashSet.ofArray elements)
        
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
            
    /// Adaptively subtracts the given sets.
    let difference (a : aset<'A>) (b : aset<'A>) =
        if a = b then empty
        elif a.IsConstant && b.IsConstant then
            let va = force a
            let vb = force b
            if va.IsEmpty then empty
            elif vb.IsEmpty then a
            else constant (fun () -> HashSet.difference va vb)
        else
            ofReader (fun () -> DifferenceReader(a, b))
            

    /// Adaptively intersects the given sets
    let intersect (a : aset<'T>) (b : aset<'T>) =
        if a = b then
            a
        elif a.IsConstant && b.IsConstant then
            let va = force a
            let vb = force b
            if va.IsEmpty && vb.IsEmpty then empty
            else constant (fun () -> HashSet.intersect va vb)
        else
            ofReader (fun () -> IntersectReader(a, b))
            
    /// Adaptively xors the given sets
    let xor (a : aset<'T>) (b : aset<'T>) =
        if a = b then
            empty
        elif a.IsConstant && b.IsConstant then
            let va = force a
            let vb = force b
            if va.IsEmpty && vb.IsEmpty then empty
            else constant (fun () -> HashSet.xor va vb)
        else
            ofReader (fun () -> XorReader(a, b))
            

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

    /// Adaptively maps over the given set and unions all resulting seqs.
    let collect' (mapping : 'A -> seq<'B>) (set : aset<'A>) =   
        if set.IsConstant then
            constant (fun () -> set |> force |> HashSet.collect (mapping >> HashSet.ofSeq))
        else
            ofReader (fun () -> CollectSeqReader(set, mapping))

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

    /// Adaptively maps over the given avals and returns the resulting set.
    let bind2 (mapping : 'A -> 'B -> aset<'C>) (valueA : aval<'A>) (valueB : aval<'B>) =
        // TODO: better implementation?
        (valueA, valueB) 
        ||> AVal.map2 (fun a b -> a,b)
        |> bind (fun (a,b) -> mapping a b)

    /// Adaptively maps over the given avals and returns the resulting set.
    let bind3 (mapping : 'A -> 'B -> 'C -> aset<'D>) (valueA : aval<'A>) (valueB : aval<'B>) (valueC : aval<'C>) =
        // TODO: better implementation?
        (valueA, valueB, valueC) 
        |||> AVal.map3 (fun a b c -> a,b,c)
        |> bind (fun (a,b,c) -> mapping a b c)

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

    
    /// Adaptively maps over the given set and disposes all removed values while active.
    /// Additionally the returned Disposable disposes all currently existing values and clears the resulting set.
    let mapUse<'A, 'B when 'B :> IDisposable> (mapping : 'A -> 'B) (set : aset<'A>) : IDisposable * aset<'B> =
        // NOTE that the resulting set can never be constant (due to disposal).
        let reader = ref None
        let set = 
            ofReader (fun () ->
                let r = new MapUseReader<'A, 'B>(set, mapping)
                reader := Some r
                r
            )
        let disp =
            { new System.IDisposable with 
                member x.Dispose() =
                    match !reader with
                    | Some r -> 
                        r.Dispose()
                        reader := None
                    | None -> 
                        ()
            }
        disp, set

    /// Evaluates the given adaptive set and returns its current content.
    /// This should not be used inside the adaptive evaluation
    /// of other AdaptiveObjects since it does not track dependencies.
    let force (set : aset<'T>) = AVal.force set.Content

    /// Reduces the set using the given `AdaptiveReduction` and returns
    /// the resulting adaptive value.
    let reduce (reduction : AdaptiveReduction<'a, 's, 'v>) (set: aset<'a>) =
        SetReductions.ReduceValue(reduction, set) :> aval<'v>
        
    /// Applies the mapping function to all elements of the set and reduces the results
    /// using the given `AdaptiveReduction`.
    /// Returns the resulting adaptive value.
    let reduceBy (reduction : AdaptiveReduction<'b, 's, 'v>) (mapping: 'a -> 'b) (set: aset<'a>) =
        SetReductions.ReduceByValue(reduction, mapping, set) :> aval<'v>
        
    /// Applies the mapping function to all elements of the set and reduces the results
    /// using the given `AdaptiveReduction`.
    /// Returns the resulting adaptive value.
    let reduceByA (reduction : AdaptiveReduction<'b, 's, 'v>) (mapping: 'a -> aval<'b>) (set: aset<'a>) =
        SetReductions.AdaptiveReduceByValue(reduction, mapping, set) :> aval<'v>

    /// Adaptively folds over the set using add for additions and trySubtract for removals.
    /// Note the trySubtract may return None indicating that the result needs to be recomputed.
    /// Also note that the order of elements given to add/trySubtract is undefined.
    let foldHalfGroup (add : 'S -> 'A -> 'S) (trySubtract : 'S -> 'A -> option<'S>) (zero : 'S) (set : aset<'A>) =
        let inline trySub s v =
            match trySubtract s v with
            | Some v -> ValueSome v
            | None -> ValueNone
        let reduction = AdaptiveReduction.halfGroup zero add trySub
        reduce reduction set

    /// Adaptively folds over the set using add for additions and recomputes the value on every removal.
    /// Note that the order of elements given to add is undefined.
    let fold (add : 'S -> 'A -> 'S) (zero : 'S) (set : aset<'A>) =
        let reduction = AdaptiveReduction.fold zero add
        reduce reduction set
        
    /// Adaptively folds over the set using add for additions and subtract for removals.
    /// Note that the order of elements given to add/subtract is undefined.
    let foldGroup (add : 'S -> 'A -> 'S) (subtract : 'S -> 'A -> 'S) (zero : 'S) (set : aset<'A>) =
        let reduction = AdaptiveReduction.group zero add subtract
        reduce reduction set

    /// Creates a constant aset lazy content.
    let delay (create : unit -> HashSet<'T>) =
        constant create

    /// Adaptively tests if the list is empty.
    let isEmpty (l: aset<'a>) =
        l.Content |> AVal.map HashSet.isEmpty
        
    /// Adaptively gets the number of elements in the list.
    let count (l: aset<'a>) =
        l.Content |> AVal.map HashSet.count

    let forall (predicate: 'T -> bool) (set: aset<'T>) =
        let r = AdaptiveReduction.countNegative |> AdaptiveReduction.mapOut (fun v -> v = 0)
        reduceBy r predicate set
        
    let exists (predicate: 'T -> bool) (set: aset<'T>) =
        let r = AdaptiveReduction.countPositive |> AdaptiveReduction.mapOut (fun v -> v <> 0)
        reduceBy r predicate set
        
    let contains (value: 'T) (set: aset<'T>) =
        SetReductions.ContainsValue(set, value) :> aval<_>

    let forallA (predicate: 'T -> aval<bool>) (set: aset<'T>) =
        let r = AdaptiveReduction.countNegative |> AdaptiveReduction.mapOut (fun v -> v = 0)
        reduceByA r predicate set
        
    let existsA (predicate: 'T -> aval<bool>) (set: aset<'T>) =
        let r = AdaptiveReduction.countPositive |> AdaptiveReduction.mapOut (fun v -> v <> 0)
        reduceByA r predicate set

        
    /// Adaptively counts all elements fulfilling the predicate
    let countBy (predicate: 'a -> bool) (set: aset<'a>) =
        reduceBy AdaptiveReduction.countPositive predicate set 

    /// Adaptively counts all elements fulfilling the predicate
    let countByA (predicate: 'a -> aval<bool>) (set: aset<'a>) =
        reduceByA AdaptiveReduction.countPositive predicate set 

    let inline tryMin (set : aset<'a>) =
        let reduction = 
            AdaptiveReduction.tryMin
            |> AdaptiveReduction.mapOut (function ValueSome v -> Some v | ValueNone -> None)
        reduce reduction set

    let inline tryMax (set : aset<'a>) =
        let reduction = 
            AdaptiveReduction.tryMax
            |> AdaptiveReduction.mapOut (function ValueSome v -> Some v | ValueNone -> None)
        reduce reduction set

    /// Adaptively computes the sum all entries in the list.
    let inline sum (set : aset<'a>) = 
        reduce (AdaptiveReduction.sum()) set
    
    let inline sumBy (mapping : 'T1 -> 'T2) (set : aset<'T1>) =
        reduceBy (AdaptiveReduction.sum()) mapping set

    let inline sumByA (mapping : 'T1 -> aval<'T2>) (set : aset<'T1>) =
        reduceByA (AdaptiveReduction.sum()) mapping set

    let inline average (set : aset<'a>) =
        reduce (AdaptiveReduction.average()) set
        
    let inline averageBy (mapping : 'T1 -> 'T2) (set : aset<'T1>) =
        reduceBy (AdaptiveReduction.average()) mapping set

    let inline averageByA (mapping : 'T1 -> aval<'T2>) (set : aset<'T1>) =
        reduceByA (AdaptiveReduction.average()) mapping set

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
