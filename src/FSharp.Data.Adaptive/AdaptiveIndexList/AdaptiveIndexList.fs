namespace FSharp.Data.Adaptive

open System
open FSharp.Data.Traceable
open FSharp.Data.Adaptive

/// An adaptive reader for alist that allows to pull operations and exposes its current state.
type IIndexListReader<'T> = 
    IOpReader<IndexList<'T>, IndexListDelta<'T>>

/// Adaptive list datastructure.
[<Interface>]
type IAdaptiveIndexList<'T> =
    /// Is the list constant?
    abstract member IsConstant : bool

    /// The current content of the list as aval.
    abstract member Content : aval<IndexList<'T>>
    
    /// Gets a new reader to the list.
    abstract member GetReader : unit -> IIndexListReader<'T>
    
    /// Gets the underlying History instance for the alist (if any)
    abstract member History : option<History<IndexList<'T>, IndexListDelta<'T>>>

/// Adaptive list datastructure.
and alist<'T> = IAdaptiveIndexList<'T>

/// Internal implementations for alist reductions.
module internal Reductions =

    /// aval for reduce operations.
    [<Sealed>]
    type ReduceValue<'a, 's, 'v>(reduction : AdaptiveReduction<'a, 's, 'v>, input : alist<'a>) =
        inherit AdaptiveObject()

        let reader = input.GetReader()
        let mutable sum = reduction.seed

        let mutable result = Unchecked.defaultof<'v>
        let mutable state = IndexList.empty<'a>

        member x.GetValue(token : AdaptiveToken) =
            x.EvaluateAlways token (fun token ->
                if x.OutOfDate then
                    let ops = reader.GetChanges token
                    if reader.State.Count <= 2 || reader.State.Count <= ops.Count then
                        state <- reader.State
                        sum <- state |> Seq.fold reduction.add reduction.seed
                        result <- reduction.view sum
                    else
                        let mutable working = true
                        let mutable e = ops.GetEnumerator()
                        while working && e.MoveNext() do
                            let index, op = e.Current
                            match op with
                            | Set a ->
                                match IndexList.tryGet index state with
                                | Some old ->
                                    match reduction.sub sum old with
                                    | ValueSome s -> sum <- s
                                    | ValueNone -> working <- false
                                | None ->
                                    ()

                                sum <- reduction.add sum a
                                state <- IndexList.set index a state

                            | Remove ->
                                match IndexList.tryRemove index state with
                                | Some(old, rest) ->
                                    state <- rest
                                    match reduction.sub sum old with
                                    | ValueSome s -> sum <- s
                                    | ValueNone -> working <- false
                                | None ->
                                    ()

                        if not working then
                            state <- reader.State
                            sum <- state |> Seq.fold reduction.add reduction.seed

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
    type ReduceByValue<'a, 'b, 's, 'v>(reduction : AdaptiveReduction<'b, 's, 'v>, mapping : Index -> 'a -> 'b, input : alist<'a>) =
        inherit AdaptiveObject()

        let reader = input.GetReader()
        let mutable sum = ValueSome reduction.seed

        let mutable result = Unchecked.defaultof<'v>
        let mutable state = IndexList.empty<'a * 'b>

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
                            reader.State |> IndexList.mapi (fun k a ->
                                match IndexList.tryGet k state with
                                | Some (oa, b) -> 
                                    if DefaultEquality.equals a oa then a, b
                                    else a, mapping k a
                                | None ->
                                    let b = mapping k a
                                    a, b
                            )
                        state <- newState
                        let s = state |> Seq.fold (fun s (_,v) -> reduction.add s v) reduction.seed
                        sum <- ValueSome s
                        result <- reduction.view s
                    else
                        for (index, op) in ops do
                            match op with
                            | Set a ->
                                match IndexList.tryGet index state with
                                | Some (oa, _) when DefaultEquality.equals oa a -> 
                                    ()
                                | _ -> 
                                    match IndexList.tryGet index state with
                                    | Some(_,old) -> sum <- sub sum old
                                    | None -> ()

                                    let b = mapping index a

                                    sum <- add sum b
                                    state <- IndexList.set index (a,b) state

                            | Remove ->
                                match IndexList.tryRemove index state with
                                | Some((_,old), rest) ->
                                    state <- rest
                                    sum <- sub sum old
                                | None ->
                                    ()

                        match sum with
                        | ValueSome s ->
                            result <- reduction.view s
                        | ValueNone ->
                            let s = state |> Seq.fold (fun s (_,b) -> reduction.add s b) reduction.seed
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
    type AdaptiveReduceByValue<'a, 'b, 's, 'v>(reduction : AdaptiveReduction<'b, 's, 'v>, mapping : Index -> 'a -> aval<'b>, l : alist<'a>) =
        inherit AdaptiveObject()

        let reader = l.GetReader()
        do reader.Tag <- "FoldReader"

        #if !FABLE_COMPILER
        let dirtyLock = obj()
        #endif


        let mutable targets = MultiSetMap.empty<aval<'b>, Index>
        let mutable state = IndexList.empty<'a * aval<'b> * 'b>

        let mutable dirty : IndexList<aval<'b>> = IndexList.empty
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
            dirty <- IndexList.empty
            d
            #else
            lock dirtyLock (fun () ->
                let d = dirty
                dirty <- IndexList.empty
                d
            )
            #endif

        let removeIndex (x : AdaptiveReduceByValue<_,_,_,_>) (i : Index) =
            match IndexList.tryRemove i state with
            | Some ((_oa, ov, o), newState) ->
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
                    dirty <- IndexList.set i o dirty
                #else
                match o with
                | :? aval<'b> as o ->
                    lock dirtyLock (fun () -> 
                        for i in MultiSetMap.find o targets do
                            dirty <- IndexList.set i o dirty
                    )
                | _ -> 
                    ()
                #endif
 
        member x.GetValue (t : AdaptiveToken) =
            x.EvaluateAlways t (fun t ->
                if x.OutOfDate then
                    let ops = reader.GetChanges t
                    if state.Count <= 2 || state.Count <= ops.Count then
                        dirty <- IndexList.empty
                        targets |> HashMap.iter (fun m _ ->
                            m.Outputs.Remove x |> ignore
                        )
                        targets <- HashMap.empty

                        let newState =  
                            reader.State |> IndexList.mapi (fun k a ->
                                match IndexList.tryGet k state with
                                | Some(oa, m,_) when DefaultEquality.equals oa a ->
                                    let v = m.GetValue t
                                    targets <- MultiSetMap.add m k targets
                                    (a, m, v)
                                | _ ->
                                    let m = mapping k a
                                    let v = m.GetValue t
                                    targets <- MultiSetMap.add m k targets
                                    (a, m, v)
                            )
                        state <- newState
                        let s = state |> Seq.fold (fun s (_,_,v) -> reduction.add s v) reduction.seed
                        sum <- ValueSome s
                        res <- reduction.view s
                    else
                        let mutable dirty = consumeDirty()
                        for (i, op) in ops do
                            dirty <- IndexList.remove i dirty
                            match op with
                            | Set v ->
                                removeIndex x i

                                let r = mapping i v
                                let n = r.GetValue(t)
                                targets <- MultiSetMap.add r i targets
                                state <- IndexList.set i (v, r, n) state
                                sum <- add sum n

                            | Remove ->
                                removeIndex x i

                        dirty.Content |> MapExt.iter (fun i r ->
                                let n = r.GetValue(t)
                                state <-
                                    state |> IndexList.alter i (fun old ->
                                        match old with
                                        | Some (oa, ro, o) -> 
                                            assert(ro = r)
                                            sum <- add (sub sum o) n
                                            Some (oa, r, n)
                                        | None -> 
                                            sum <- add sum n
                                            None
                                    )
                            )

                        match sum with
                        | ValueNone ->
                            let s = state |> Seq.fold (fun s (_,_,v) -> reduction.add s v) reduction.seed
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


/// Internal implementations for alist operations.
module internal AdaptiveIndexListImplementation =

    let inline checkTag (value : 'a) (real : obj) = DefaultEquality.equals (value :> obj) real
    
    /// Core implementation for a dependent list.
    [<Sealed>]
    type AdaptiveIndexListImpl<'T>(ofReaderReader : unit -> IOpReader<IndexListDelta<'T>>) =
        let history = History(ofReaderReader, IndexList.trace)

        /// Gets a new reader to the list.
        member x.GetReader() : IIndexListReader<'T> =
            history.NewReader()

        /// Current content of the list as aval.
        member x.Content =
            history :> aval<_>

        interface IAdaptiveIndexList<'T> with
            member x.IsConstant = false
            member x.GetReader() = x.GetReader()
            member x.Content = x.Content
            member x.History = Some history

    /// Efficient implementation for an empty adaptive list.
    [<Sealed>]
    type EmptyList<'T> private() =   
        static let instance = EmptyList<'T>() :> alist<_>
        let content = AVal.constant IndexList.empty
        let reader = new History.Readers.EmptyReader<IndexList<'T>, IndexListDelta<'T>>(IndexList.trace) :> IIndexListReader<'T>
        static member Instance = instance
        
        member x.Content = content
        member x.GetReader() = reader
        
        interface IAdaptiveIndexList<'T> with
            member x.IsConstant = true
            member x.GetReader() = x.GetReader()
            member x.Content = x.Content
            member x.History = None

    /// Efficient implementation for a constant adaptive list.
    [<Sealed>]
    type ConstantList<'T>(content : Lazy<IndexList<'T>>) =
        let value = AVal.delay (fun () -> content.Value)

        member x.Content = value

        member x.GetReader() =
            new History.Readers.ConstantReader<_,_>(
                IndexList.trace,
                lazy (IndexList.computeDelta IndexList.empty content.Value),
                content
            ) :> IIndexListReader<_>

        interface IAdaptiveIndexList<'T> with
            member x.IsConstant = true
            member x.GetReader() = x.GetReader()
            member x.Content = x.Content
            member x.History = None


    /// Reader for init operation
    [<Sealed>]
    type InitReader<'T>(input : aval<int>, mapping : int -> 'T) =
        inherit AbstractReader<IndexListDelta<'T>>(IndexListDelta.empty)
        let mutable lastLength = 0
        let mutable idxs = IndexList.empty
        override x.Compute(token) =
            let newLength = input.GetValue(token)
            let mutable delta = IndexListDelta.empty
            // length increase
            for i in lastLength .. newLength - 1 do  
                idxs <- idxs.Add ()
                let nextIdx = IndexList.lastIndex idxs
                delta <- delta.Add(nextIdx, Set (mapping i))
            // length decrease
            for _ in lastLength - 1 .. -1 .. newLength do  
                let lastIdx = IndexList.lastIndex idxs
                delta <- delta.Add(lastIdx, Remove)
                idxs <- IndexList.remove lastIdx idxs 
            lastLength <- newLength
            delta

    /// Reader for map operations.
    [<Sealed>]
    type MapReader<'a, 'b>(input : alist<'a>, mapping : Index -> 'a -> 'b) =
        inherit AbstractReader<IndexListDelta<'b>>(IndexListDelta.empty)

        let reader = input.GetReader()

        static member DeltaMapping (mapping : Index -> 'a -> 'b) =
            IndexListDelta.map (fun i op ->
                match op with
                | Remove -> Remove
                | Set v -> Set (mapping i v)
            )

        override x.Compute(token) =
            reader.GetChanges token |> IndexListDelta.map (fun i op ->
                match op with
                | Remove -> Remove
                | Set v -> Set (mapping i v)
            )

    /// Reader for mapUse operations.
    [<Sealed>]
    type MapUseReader<'a, 'b when 'b :> System.IDisposable>(input : alist<'a>, mapping : Index -> 'a -> 'b) =
        inherit AbstractReader<IndexListDelta<'b>>(IndexListDelta.empty)

        let mutable state : MapExt<Index, 'b> = MapExt.empty
        let mutable disposeDelta = IndexListDelta.empty
        let mutable reader = input.GetReader()

        member x.Dispose() =
            lock x (fun () ->
                for v in MapExt.toValueList state do (v :> System.IDisposable).Dispose()
                disposeDelta <- state |> MapExt.map (fun _ _ -> Remove) |> IndexListDelta.ofMap
                state <- MapExt.empty
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
                disposeDelta <- IndexListDelta.empty
                d
            else
                let ops = reader.GetChanges token
                ops |> IndexListDelta.map (fun k op ->
                    match op with
                    | Set v ->
                        let r = mapping k v
                        state <-
                            state.Change(k, fun (o : option<'b>) ->
                                match o with
                                | Some o -> o.Dispose(); Some r
                                | None -> Some r
                            )
                        Set r
                    | Remove -> 
                        state <-
                            state.Change(k, fun (o : option<'b>) ->
                                match o with
                                | Some o -> o.Dispose(); None
                                | None -> None
                            )
                        Remove

                )

    /// Reader for choose operations.
    [<Sealed>]
    type ChooseReader<'a, 'b>(input : alist<'a>, mapping : Index -> 'a -> option<'b>) =
        inherit AbstractReader<IndexListDelta<'b>>(IndexListDelta.empty)

        let r = input.GetReader()
        let mapping = IndexCache mapping

        static member DeltaMapping (mapping : Index -> 'a -> option<'b>) =
            let mapping = IndexCache mapping
            IndexListDelta.choose (fun i op ->
                match op with
                | Remove -> 
                    match mapping.Revoke(i) with
                    | Some _ -> Some Remove
                    | _ -> None
                | Set v -> 
                    let o, n = mapping.InvokeAndGetOld(i, v)
                    match n with
                    | Some res -> Some (Set res)
                    | None -> 
                        match o with
                        | Some (Some _o) -> Some Remove
                        | _ -> None
            )


        override x.Compute(token) =
            r.GetChanges token |> IndexListDelta.choose (fun i op ->
                match op with
                | Remove -> 
                    match mapping.Revoke(i) with
                    | Some _ -> Some Remove
                    | _ -> None
                | Set v -> 
                    let o, n = mapping.InvokeAndGetOld(i, v)
                    match n with
                    | Some res -> Some (Set res)
                    | None -> 
                        match o with
                        | Some (Some _o) -> Some Remove
                        | _ -> None
        )

    /// Reader for filter operations.
    [<Sealed>]
    type FilterReader<'a>(input : alist<'a>, predicate : Index -> 'a -> bool) =
        inherit AbstractReader<IndexListDelta<'a>>(IndexListDelta.empty)

        let reader = input.GetReader()
        let mapping = IndexCache predicate

        static member DeltaMapping (predicate : Index -> 'a -> bool) =
            let mapping = IndexCache predicate
            IndexListDelta.choose (fun i op ->
                match op with
                | Remove -> 
                    match mapping.Revoke(i) with
                    | Some true -> Some Remove
                    | _ -> None
                | Set v -> 
                    let o, n = mapping.InvokeAndGetOld(i, v)
                    match n with
                    | true -> 
                        Some (Set v)
                    | false -> 
                        match o with
                            | Some true -> Some Remove
                            | _ -> None
            )

        override x.Compute(token) =
            reader.GetChanges token |> IndexListDelta.choose (fun i op ->
                match op with
                | Remove -> 
                    match mapping.Revoke(i) with
                    | Some true -> Some Remove
                    | _ -> None
                | Set v -> 
                    let o, n = mapping.InvokeAndGetOld(i, v)
                    match n with
                    | true -> 
                        Some (Set v)
                    | false -> 
                        match o with
                            | Some true -> Some Remove
                            | _ -> None
            )
            
    /// Reader for mapA operations.
    [<Sealed>]
    type MapAReader<'a, 'b>(input : alist<'a>, mapping : Index -> 'a -> aval<'b>) =
        inherit AbstractReader<IndexListDelta<'b>>(IndexListDelta.empty)

        let mapping = OptimizedClosures.FSharpFunc<Index, 'a, aval<'b>>.Adapt mapping
        let reader = input.GetReader()
        do reader.Tag <- "input"
        let cache = Cache (fun (a,b) -> mapping.Invoke(a,b))
        let mutable targets = MultiSetMap.empty<aval<'b>, Index>
        let mutable dirty = IndexList.empty<aval<'b>>

        let consumeDirty() =
            lock cache (fun () ->
                let d = dirty
                dirty <- IndexList.empty
                d
            )

        override x.InputChangedObject(t, o) =
            #if FABLE_COMPILER
            if isNull o.Tag then
                let o = unbox<aval<'b>> o
                for i in MultiSetMap.find o targets do
                    dirty <- IndexList.set i o dirty
            #else
            match o with
            | :? aval<'b> as o ->
                lock cache (fun () ->
                    for i in MultiSetMap.find o targets do
                        dirty <- IndexList.set i o dirty
                )
            | _ ->
                ()
            #endif

        override x.Compute t =
            let mutable dirty = consumeDirty()
            let old = reader.State
            let ops = reader.GetChanges t

            let mutable changes =
                ops |> IndexListDelta.choose (fun i op ->
                    dirty <- IndexList.remove i dirty
                    match op with
                    | Set v ->
                        let k = cache.Invoke(i,v)
                        let v = k.GetValue t
                        targets <- MultiSetMap.add k i targets
                        Some (Set v)
                    | Remove ->
                        match IndexList.tryGet i old with
                        | Some v ->                  
                            let o = cache.Revoke(i, v)
                            let rem, r = MultiSetMap.remove o i targets
                            targets <- r
                            if rem then o.Outputs.Remove x |> ignore
                            Some Remove
                        | None ->
                            None
                )

            dirty.Content |> MapExt.iter (fun i d ->
                    let v = d.GetValue t
                    changes <- IndexListDelta.add i (Set v) changes
                )

            changes
     
    /// Reader for chooseA operations.
    [<Sealed>]
    type ChooseAReader<'a, 'b>(input : alist<'a>, mapping : Index -> 'a -> aval<Option<'b>>) =
        inherit AbstractReader<IndexListDelta<'b>>(IndexListDelta.empty)

        let reader = input.GetReader()
        do reader.Tag <- "input"
        let mapping = OptimizedClosures.FSharpFunc<Index, 'a, aval<option<'b>>>.Adapt mapping
        let keys = DefaultHashSet.create<Index>()
        let cache = Cache (fun (a,b) ->  mapping.Invoke(a,b))
        let mutable targets = MultiSetMap.empty<aval<option<'b>>, Index>
        let mutable dirty = IndexList.empty<aval<option<'b>>>

        let consumeDirty() =
            lock cache (fun () ->
                let d = dirty
                dirty <- IndexList.empty
                d
            )

        override x.InputChangedObject(t, o) =
            #if FABLE_COMPILER
            if isNull o.Tag then
                let o = unbox<aval<option<'b>>> o
                for i in MultiSetMap.find o targets do
                    dirty <- IndexList.set i o dirty
            #else
            match o with
            | :? aval<option<'b>> as o ->
                lock cache (fun () ->
                    for i in MultiSetMap.find o targets do
                        dirty <- IndexList.set i o dirty
                )
            | _ ->
                ()
            #endif


        override x.Compute(t) =
            let mutable dirty = consumeDirty()
            let old = reader.State
            let ops = reader.GetChanges t
            let mutable changes =
                ops |> IndexListDelta.choose (fun i op ->
                    dirty <- IndexList.remove i dirty
                    match op with
                    | Set v ->
                        let k = cache.Invoke(i,v)
                        let v = k.GetValue t
                        targets <- MultiSetMap.add k i targets
                        match v with
                        | Some v -> 
                            keys.Add i |> ignore
                            Some (Set v)
                        | None ->
                            if keys.Remove i then Some Remove
                            else None
                    | Remove ->
                        match IndexList.tryGet i old with
                        | Some v ->                  
                            let o = cache.Revoke(i, v)
                            let rem, rest = MultiSetMap.remove o i targets
                            targets <- rest
                            if rem then o.Outputs.Remove x |> ignore

                            if keys.Remove i then Some Remove
                            else None
                        | None ->
                            None
                )

            dirty.Content |> MapExt.iter (fun i d ->
                    let v = d.GetValue t
                    match v with
                    | Some v -> 
                        keys.Add i |> ignore
                        changes <- IndexListDelta.add i (Set v) changes
                    | None ->
                        if keys.Remove i then
                            changes <- IndexListDelta.add i Remove changes
                )

            changes
  
    /// Ulitity used by CollectReader.
    [<Sealed>]
    type MultiReader<'a>(mapping : IndexMapping<Index * Index>, list : alist<'a>, release : alist<'a> -> unit) =
        inherit AbstractReader<IndexListDelta<'a>>(IndexListDelta.empty)
            
        let targets = DefaultHashSet.create<Index>()

        let mutable reader = None

        let getReader() =
            match reader with
            | Some r -> r
            | None ->
                let r = list.GetReader()
                reader <- Some r
                r

        member x.AddTarget(oi : Index) =
            if targets.Add oi then
                getReader().State.Content.MapMonotonic (fun ii v -> mapping.Invoke(oi, ii), Set v)
                |> IndexListDelta.ofMap
            else
                IndexListDelta.empty

        member x.RemoveTarget(dirty : System.Collections.Generic.HashSet<MultiReader<'a>>, oi : Index) =
            if targets.Remove oi then
                match reader with
                | Some r ->
                    let result = 
                        r.State.Content 
                        |> MapExt.toSeq
                        |> Seq.choose (fun (ii, v) -> 
                            match mapping.Revoke(oi,ii) with
                            | Some v -> Some (v, Remove)
                            | None -> None
                        )
                        |> IndexListDelta.ofSeq

                    if targets.Count = 0 then 
                        dirty.Remove x |> ignore
                        x.Release()

                    result

                | None ->
                    IndexListDelta.empty
            else
                IndexListDelta.empty

        member x.Release() =
            match reader with
            | Some r ->
                release(list)
                r.Outputs.Remove x |> ignore
                x.Outputs.Clear()
                reader <- None
            | None ->   
                ()

        override x.Compute(token) =
            match reader with
            | Some r -> 
                let ops = r.GetChanges token

                ops |> IndexListDelta.collect (fun ii op ->
                    match op with
                    | Remove -> 
                        targets
                        |> Seq.choose (fun oi -> 
                            match mapping.Revoke(oi, ii) with   
                            | Some i -> Some(i, Remove)
                            | None -> None
                        )
                        |> IndexListDelta.ofSeq

                    | Set v ->
                        targets
                        |> Seq.map (fun oi -> mapping.Invoke(oi, ii), Set v)
                        |> IndexListDelta.ofSeq

                )

            | None ->
                IndexListDelta.empty

    /// Reader for collect operations.
    [<Sealed>]
    type CollectReader<'a, 'b>(input : alist<'a>, f : Index -> 'a -> alist<'b>) =
        inherit AbstractDirtyReader<MultiReader<'b>, IndexListDelta<'b>>(IndexListDelta.monoid, checkTag "MultiReader")
            
        let mapping = IndexMapping<Index * Index>()
        let cache = DefaultDictionary.create<Index, 'a * alist<'b>>()
        let readers = DefaultDictionary.create<alist<'b>, MultiReader<'b>>()
        let input = input.GetReader()

        let removeReader (l : alist<'b>) =
            readers.Remove l |> ignore

        let getReader (l : alist<'b>) =
            match readers.TryGetValue l with
            | (true, r) -> r
            | _ ->
                let r = new MultiReader<'b>(mapping, l, removeReader)
                r.Tag <- "MultiReader"
                readers.Add(l, r) 
                r

        member x.Invoke (dirty : System.Collections.Generic.HashSet<MultiReader<'b>>, i : Index, v : 'a) =
            match cache.TryGetValue(i) with
            | (true, (oldValue, oldList)) ->
                if DefaultEquality.equals oldValue v then
                    let r = getReader(oldList)
                    dirty.Add r |> ignore
                    IndexListDelta.empty
                else
                    let newList = f i v
                    cache.[i] <- (v, newList)
                    if newList <> oldList then
                        let newReader = getReader(newList)
                        let rem = 
                            match readers.TryGetValue oldList with
                            | (true, r) -> r.RemoveTarget(dirty, i)
                            | _ -> IndexListDelta.empty
                        let add = newReader.AddTarget i

                        dirty.Add newReader |> ignore
                        IndexListDelta.combine rem add 
                    else
                        let r = getReader(oldList)
                        dirty.Add r |> ignore
                        IndexListDelta.empty
                        

            | _ ->
                let newList = f i v
                cache.[i] <- (v, newList)
                let newReader = getReader(newList)
                let add = newReader.AddTarget i
                dirty.Add newReader |> ignore
                add

        member x.Revoke (dirty : System.Collections.Generic.HashSet<MultiReader<'b>>, i : Index) =
            match cache.TryGetValue i with
            | (true, (v,l)) ->
                let r = getReader l
                cache.Remove i |> ignore
                r.RemoveTarget(dirty, i)
            | _ ->
                IndexListDelta.empty

        override x.Compute(token, dirty) =
            let mutable result = 
                input.GetChanges token |> IndexListDelta.collect (fun i op ->
                    match op with
                    | Remove -> x.Revoke(dirty, i)
                    | Set v -> x.Invoke(dirty, i, v)
                )

            for d in dirty do
                result <- IndexListDelta.combine result (d.GetChanges token)

            result

    /// Helper for ConcatReader.
    [<Sealed>]
    type IndexedReader<'a>(mapping : IndexMapping<Index * Index>, index : Index, input : alist<'a>) =
        inherit AbstractReader<IndexListDelta<'a>>(IndexListDelta.empty) 

        let reader = input.GetReader()

        override x.Compute(token : AdaptiveToken) =
            reader.GetChanges(token)
            |> IndexListDelta.chooseIndexed (fun i op ->
                match op with
                | Set v ->
                    let outIndex = mapping.Invoke(index, i)
                    Some (outIndex, Set v)
                | Remove ->
                    let outIndex = mapping.Revoke(index, i)
                    match outIndex with
                    | Some outIndex ->
                        Some (outIndex, Remove)
                    | None ->
                        None
            )

    /// Reader for concat operations.
    [<Sealed>]
    type ConcatReader<'a>(inputs : IndexList<alist<'a>>) =
        inherit AbstractDirtyReader<IndexedReader<'a>, IndexListDelta<'a>>(IndexListDelta.monoid, checkTag "InnerReader")

        let mapping = IndexMapping<Index * Index>()
        let readers = 
            inputs |> IndexList.mapi (fun i l -> 
                let r = IndexedReader(mapping, i, l)
                r.Tag <- "InnerReader"
                r
            )

        let mutable initial = true

        override x.Compute(token : AdaptiveToken, dirty : System.Collections.Generic.HashSet<_>) =
            if initial then
                initial <- false
                let mutable result = IndexListDelta.empty
                for r in readers do
                    result <- IndexListDelta.combine result (r.GetChanges token)
                result
            else
                let mutable result = IndexListDelta.empty
                for r in dirty do
                    result <- IndexListDelta.combine result (r.GetChanges token)
                result

    /// Reader for bind operations.
    [<Sealed>]
    type BindReader<'a, 'b>(input : aval<'a>, mapping : 'a -> alist<'b>) =
        inherit AbstractReader<IndexListDelta<'b>>(IndexListDelta.empty)

        let mutable inputChanged = 1
        let mutable reader : Option<'a * IIndexListReader<'b>> = None

        override x.InputChangedObject(t : obj, o : IAdaptiveObject) =
            if System.Object.ReferenceEquals(input, o) then
                inputChanged <- 1

        override x.Compute(token) =
            let v = input.GetValue token
            #if FABLE_COMPILER
            let inputChanged = let v = inputChanged in inputChanged <- 0; v
            #else
            let inputChanged = System.Threading.Interlocked.Exchange(&inputChanged, 0)
            #endif
            match reader with
            | Some (oldA, oldReader) when inputChanged = 0 || cheapEqual v oldA ->
                oldReader.GetChanges token
            | _ -> 
                let newReader = mapping(v).GetReader()
                let deltas = 
                    let addNew = newReader.GetChanges token
                    match reader with
                        | Some(_,old) ->
                            let remOld = IndexList.computeDelta old.State IndexList.empty
                            old.Outputs.Clear()
                            IndexListDelta.combine remOld addNew
                        | None ->
                            addNew
                reader <- Some (v,newReader)
                deltas

    /// Reader for sortBy operations
    [<Sealed>]
    type SortByReader<'a, 'b when 'b : comparison>(input : alist<'a>, mapping : Index -> 'a -> 'b) =
        inherit AbstractReader<IndexListDelta<'a>>(IndexListDelta.empty)

        let reader = input.GetReader()
        let idx = IndexMapping<('b * Index)>()
        let cache = DefaultDictionary.create<Index, 'b>()

        override x.Compute(token : AdaptiveToken) =
            reader.GetChanges(token).Content
            |> Seq.collect (fun (KeyValue(i, op)) ->
                match op with
                | Set v -> 
                    let rem =
                        match cache.TryGetValue i with
                        | (true, b) ->
                            match idx.Revoke((b, i)) with
                            | Some oi -> Some (oi, Remove)
                            | None -> None
                        | _ ->
                            None
                    let b = mapping i v
                    cache.[i] <- b
                    let oi = idx.Invoke((b, i))
                    match rem with
                    | Some op -> [op; (oi, Set v)]
                    | None -> [(oi, Set v)]
                | Remove ->
                    match cache.TryGetValue i with
                    | (true, b) ->
                        cache.Remove i |> ignore
                        match idx.Revoke((b, i)) with
                        | Some oi -> [(oi, Remove)]
                        | None -> []
                    | _ ->
                        []
            )
            |> IndexListDelta.ofSeq

    /// Reader for sortWith operations
    [<Sealed>]
    type SortWithReader<'a>(input : alist<'a>, compare : 'a -> 'a -> int) =
        inherit AbstractReader<IndexListDelta<'a>>(IndexListDelta.empty)

        let reader = input.GetReader()
        let idx = IndexMapping<UCmp<struct ('a * Index)>>()

        let cmp = 
            let icmp = LanguagePrimitives.FastGenericComparer<Index>
            let vcmp = OptimizedClosures.FSharpFunc<_,_,_>.Adapt compare
            let cmp (struct (lv: 'a, li: Index)) (struct (rv: 'a, ri : Index)) =
                let c = vcmp.Invoke(lv, rv)
                if c = 0 then icmp.Compare(li, ri)
                else c
            OptimizedClosures.FSharpFunc<_,_,_>.Adapt cmp

        override x.Compute(token : AdaptiveToken) =
            let old = reader.State.Content
            reader.GetChanges(token).Content
            |> Seq.collect (fun (KeyValue(i, op)) ->
                match op with
                | Set v -> 
                    let rem =
                        match MapExt.tryFind i old with
                        | Some ov ->
                            match idx.Revoke(UCmp(cmp, struct(ov, i))) with
                            | Some oi -> Some (oi, Remove)
                            | None -> None
                        | _ ->
                            None
                    let oi = idx.Invoke(UCmp(cmp, struct(v, i)))
                    match rem with
                    | Some op -> [op; (oi, Set v)]
                    | None -> [(oi, Set v)]
                | Remove ->
                    match MapExt.tryFind i old with
                    | Some ov ->
                        match idx.Revoke(UCmp(cmp, struct(ov, i))) with
                        | Some oi -> [(oi, Remove)]
                        | None -> []
                    | _ ->
                        []
            )
            |> IndexListDelta.ofSeq

    /// Reader for ofAVal operations
    [<Sealed>]
    type AValReader<'s, 'a when 's :> seq<'a>>(input: aval<'s>) =
        inherit AbstractReader<IndexListDelta<'a>>(IndexListDelta.empty)

        let mutable last = IndexList.empty

        override x.Compute(token: AdaptiveToken) =
            let v = input.GetValue token |> IndexList.ofSeq
            let ops = IndexList.computeDelta last v
            last <- v
            ops
    
    /// Reader for pairwise operations
    [<Sealed>]
    type PairwiseReader<'a>(input : alist<'a>, cyclic : bool) =
        inherit AbstractReader<IndexList<'a * 'a>, IndexListDelta<'a * 'a>>(IndexList.trace)
        
        let reader = input.GetReader()

        override x.Compute(token: AdaptiveToken) =
            let o = reader.State
            let ops = reader.GetChanges token
            let s = reader.State

            let inline neighbours (i : Index) =
                let (l, _, r) = IndexList.neighbours i s

                let r =
                    match r with
                    | None when cyclic -> s.Content.TryMin()
                    | _ -> r
                    
                let l =
                    match l with
                    | None when cyclic -> s.Content.TryMax()
                    | _ -> l

                l, r

            let mutable delta = IndexListDelta.empty
            for i, op in ops do
                match op with
                | Remove ->
                    match IndexList.tryGet i o with
                    | Some _ov ->
                        let (l, r) = neighbours i
                        delta <- IndexListDelta.add i Remove delta
                        match l with
                        | Some (li, lv) ->
                            match r with
                            | Some (_ri, rv) ->
                                delta <- IndexListDelta.add li (Set(lv, rv)) delta
                            | None ->
                                delta <- IndexListDelta.add li Remove delta
                        | None ->
                            ()
                    | None ->
                        ()
                | Set v ->
                    let (l, r) = neighbours i
                    match IndexList.tryGet i o with
                    | Some ov when CheapEquality.cheapEqual ov v ->
                        ()
                    | _ ->
                        match r with
                        | Some (_ri, rv) ->
                            delta <- IndexListDelta.add i (Set(v, rv)) delta
                        | None ->
                            ()
                    
                        match l with
                        | Some(li, lv) ->
                            delta <- IndexListDelta.add li (Set(lv, v)) delta
                        | None ->
                            ()
               
            //let ref = 
            //    if cyclic then IndexList.pairwiseCyclic s
            //    else IndexList.pairwise s
            //let t, _ = IndexList.applyDelta x.State delta

            //if not (DefaultEquality.equals ref t) then
            //    printfn "%A %A" ref t
            
            
            delta


    /// Gets the current content of the alist as IndexList.
    let inline force (list : alist<'T>) = 
        AVal.force list.Content


/// Functional operators for the alist<_> type.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AList =
    open AdaptiveIndexListImplementation

    /// Creates an alist using the given reader-creator.
    let ofReader (create: unit -> #IOpReader<IndexListDelta<'T>>) =
        AdaptiveIndexListImpl(fun () -> create() :> IOpReader<_>) :> alist<_>

        
    /// Creates an alist using the given compute function
    let custom (compute : AdaptiveToken -> IndexList<'a> -> IndexListDelta<'a>) : alist<'a> = 
        ofReader (fun () -> 
            { new AbstractReader<IndexList<'a>,IndexListDelta<'a>>(IndexList.trace) with
                override x.Compute(t) = 
                    compute t x.State
            }
        )


    /// The empty alist.
    [<GeneralizableValue>]
    let empty<'T> : alist<'T> = 
        EmptyList<'T>.Instance

    /// Creates an alist holding the given values.
    let constant (value : unit -> IndexList<'T>) =
        lazy value() |> ConstantList :> alist<_>
        
    /// A constant alist holding a single value.
    let single (value : 'T) =
        constant (fun () -> IndexList.single value)
        
    /// Creates an alist holding the given values.
    let ofSeq (elements : seq<'T>) = 
        constant (fun () -> IndexList.ofSeq elements)
        
    /// Creates an alist holding the given values.
    let ofList (elements : list<'T>) =
        constant (fun () -> IndexList.ofList elements)
        
    /// Creates an alist holding the given values.
    let ofArray (elements : 'T[]) =
        constant (fun () -> IndexList.ofArray elements)
        
    /// Creates an alist holding the given values. `O(1)`
    let ofIndexList (elements : IndexList<'T>) =
        constant (fun () -> elements)

    /// Creates an alist from the given adaptive content
    let ofAVal (value: aval<#seq<'T>>) =
        if value.IsConstant then
            constant (fun () -> IndexList.ofSeq (AVal.force value))
        else
            ofReader (fun () -> AValReader(value))

    /// Adaptively applies the given mapping function to all elements and returns a new alist containing the results.
    let mapi (mapping: Index -> 'T1 -> 'T2) (list : alist<'T1>) =
        if list.IsConstant then
            constant (fun () -> list |> force |> IndexList.mapi mapping)
        else
            match list.History with
            | Some history ->
                ofReader (fun () -> history.NewReader(IndexList.trace, MapReader.DeltaMapping mapping))
            | None -> 
                ofReader (fun () -> MapReader(list, mapping))

    /// Adaptively applies the given mapping function to all elements and returns a new alist containing the results.
    let map (mapping: 'T1 -> 'T2) (list : alist<'T1>) =
        if list.IsConstant then
            constant (fun () -> list |> force |> IndexList.map mapping)
        else
            match list.History with
            | Some history ->
                ofReader (fun () -> history.NewReader(IndexList.trace, MapReader.DeltaMapping (fun _ v -> mapping v)))
            | None -> 
                // TODO: better implementation (caching possible since no Index needed)
                ofReader (fun () -> MapReader(list, fun _ -> mapping))
  
    /// Adaptively chooses all elements returned by mapping.  
    let choosei (mapping: Index -> 'T1 -> option<'T2>) (list: alist<'T1>) =
        if list.IsConstant then
            constant (fun () -> list |> force |> IndexList.choosei mapping)
        else
            match list.History with
            | Some history ->
                ofReader (fun () -> history.NewReader(IndexList.trace, ChooseReader.DeltaMapping mapping))
            | None -> 
                ofReader (fun () -> ChooseReader(list, mapping))
  
    /// Adaptively chooses all elements returned by mapping.  
    let choose (mapping: 'T1 -> option<'T2>) (list: alist<'T1>) =
        if list.IsConstant then
            constant (fun () -> list |> force |> IndexList.choose mapping)
        else
            match list.History with
            | Some history ->
                ofReader (fun () -> history.NewReader(IndexList.trace, ChooseReader.DeltaMapping (fun _ v -> mapping v)))
            | None -> 
                ofReader (fun () -> ChooseReader(list, fun _ v -> mapping v))

    /// Adaptively filters the list using the given predicate.
    let filteri (predicate : Index -> 'T -> bool) (list: alist<'T>) =
        if list.IsConstant then
            constant (fun () -> list |> force |> IndexList.filteri predicate)
        else
            match list.History with
            | Some history ->
                ofReader (fun () -> history.NewReader(IndexList.trace, FilterReader.DeltaMapping predicate))
            | None -> 
                ofReader (fun () -> FilterReader(list, predicate))
        
    /// Adaptively filters the list using the given predicate.
    let filter (predicate : 'T -> bool) (list: alist<'T>) =
        if list.IsConstant then
            constant (fun () -> list |> force |> IndexList.filter predicate)
        else
            match list.History with
            | Some history ->
                ofReader (fun () -> history.NewReader(IndexList.trace, FilterReader.DeltaMapping (fun _ v -> predicate v)))
            | None -> 
                ofReader (fun () -> FilterReader(list, fun _ v -> predicate v))

    /// Adaptively applies the given mapping function to all elements and returns a new alist containing the results.  
    let mapAi (mapping: Index -> 'T1 -> aval<'T2>) (list: alist<'T1>) =
        if list.IsConstant then
            let list = force list |> IndexList.mapi mapping
            if list |> Seq.forall (fun v -> v.IsConstant) then
                constant (fun () -> list |> IndexList.map AVal.force)
            else
                // TODO better impl possible
                ofReader (fun () -> MapAReader(ofIndexList list, fun _ v -> v))
        else
            ofReader (fun () -> MapAReader(list, mapping))

    /// Adaptively applies the given mapping function to all elements and returns a new alist containing the results.  
    let mapA (mapping: 'T1 -> aval<'T2>) (list: alist<'T1>) =
        mapAi (fun _ v -> mapping v) list

    /// Adaptively chooses all elements returned by mapping.  
    let chooseAi (mapping: Index ->'T1 -> aval<Option<'T2>>) (list: alist<'T1>) =
        if list.IsConstant then
            let list = force list |> IndexList.mapi mapping
            if list |> Seq.forall (fun v -> v.IsConstant) then
                constant (fun () -> list |> IndexList.choose AVal.force)
            else
                // TODO better impl possible
                ofReader (fun () -> ChooseAReader(ofIndexList list, fun _ v -> v))
        else
            ofReader (fun () -> ChooseAReader(list, mapping))

    /// Adaptively chooses all elements returned by mapping.  
    let chooseA (mapping: 'T1 -> aval<option<'T2>>) (list: alist<'T1>) =
        chooseAi (fun _ v -> mapping v) list

    /// Adaptively filters the list using the given predicate.
    let filterAi (predicate: Index -> 'T -> aval<bool>) (list: alist<'T>) =
        list |> chooseAi (fun i v ->
            predicate i v |> AVal.map (function true -> Some v | false -> None)
        )

    /// Adaptively filters the list using the given predicate.
    let filterA (predicate: 'T -> aval<bool>) (list: alist<'T>) =
        list |> chooseAi (fun i v ->
            predicate v |> AVal.map (function true -> Some v | false -> None)
        )

    /// Adaptively applies the given mapping function to all elements and returns a new alist holding the concatenated results.
    let collecti (mapping: Index -> 'T1 -> alist<'T2>) (list : alist<'T1>) =
        if list.IsConstant then
            let content = force list |> IndexList.mapi mapping
            if content |> Seq.forall (fun l -> l.IsConstant) then
                constant (fun () -> content |> IndexList.collect force)
            else
                ofReader (fun () -> ConcatReader(content))
        else
            ofReader (fun () -> CollectReader(list, mapping))
                     
    /// Adaptively applies the given mapping function to all elements and returns a new alist holding the concatenated results.
    let collect (mapping: 'T1 -> alist<'T2>) (list : alist<'T1>) =
        // TODO: better implementation possible (caching?)
        collecti (fun _ v -> mapping v) list
        
    /// Adaptively applies the given mapping function to all elements and returns a new alist holding the concatenated results.
    let collect' (mapping: 'T1 -> seq<'T2>) (list : alist<'T1>) =
        // TODO: better implementation possible (caching?)
        collecti (fun _ v -> mapping v |> ofSeq) list

    /// Adaptively ofReaders an alist with the source-indices.
    let indexed (list : alist<'T>) =
        list |> mapi (fun i v -> (i, v))

    /// Adaptively concatenates the given lists.
    let concat (lists : #seq<alist<'T>>) =
        let lists = IndexList.ofSeq lists
        if lists.IsEmpty then 
            empty
        elif lists |> Seq.forall (fun l -> l.IsConstant) then
            constant (fun () -> lists |> IndexList.collect force)
        else
            ofReader (fun () -> ConcatReader(lists))

    /// Adaptively concatenates the given lists.
    let append (l: alist<'T>) (r: alist<'T>) =
        concat [l; r]

    /// Creates an aval providing access to the current content of the list.
    let toAVal (list : alist<'T>) =
        list.Content

    /// Adaptively maps over the given aval and returns the resulting list.
    let bind (mapping: 'T1 -> alist<'T2>) (value: aval<'T1>) =
        if value.IsConstant then
            value |> AVal.force |> mapping
        else
            ofReader (fun () -> BindReader(value, mapping))
            
    /// Adaptively maps over the given avals and returns the resulting list.
    let bind2 (mapping : 'A -> 'B -> alist<'C>) (valueA : aval<'A>) (valueB : aval<'B>) =
        // TODO: better implementation?
        (valueA, valueB) 
        ||> AVal.map2 (fun a b -> a,b)
        |> bind (fun (a,b) -> mapping a b)

    /// Adaptively maps over the given avals and returns the resulting list.
    let bind3 (mapping : 'A -> 'B -> 'C -> alist<'D>) (valueA : aval<'A>) (valueB : aval<'B>) (valueC : aval<'C>) =
        // TODO: better implementation?
        (valueA, valueB, valueC) 
        |||> AVal.map3 (fun a b c -> a,b,c)
        |> bind (fun (a,b,c) -> mapping a b c)
        
    
    /// Adaptively maps over the given list and disposes all removed values while active.
    /// Additionally the returned Disposable disposes all currently existing values and clears the resulting list.
    let mapUsei<'A, 'B when 'B :> IDisposable> (mapping : Index -> 'A -> 'B) (list : alist<'A>) : IDisposable * alist<'B> =
        // NOTE that the resulting list can never be constant (due to disposal).
        let reader = ref None
        let set = 
            ofReader (fun () ->
                let r = new MapUseReader<'A, 'B>(list, mapping)
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
        
    /// Adaptively maps over the given list and disposes all removed values while active.
    /// Additionally the returned Disposable disposes all currently existing values and clears the resulting list.
    let mapUse mapping list = 
        mapUsei (fun _ v -> mapping v) list

    /// Sorts the list using the keys given by projection.
    /// Note that the sorting is stable.
    let sortByi (mapping : Index -> 'T1 -> 'T2) (list : alist<'T1>) =
        if list.IsConstant then
            constant (fun () -> force list |> IndexList.sortByi mapping)
        else
            ofReader (fun () -> SortByReader(list, mapping))
            
    /// Sorts the list using the keys given by projection.
    /// Note that the sorting is stable.
    let sortBy (mapping : 'T1 -> 'T2) (list : alist<'T1>) =
        if list.IsConstant then
            constant (fun () -> force list |> IndexList.sortBy mapping)
        else
            ofReader (fun () -> SortByReader(list, fun _ v -> mapping v))

    /// Sorts the list using the keys given by projection in descending order.
    /// Note that the sorting is stable.
    let sortByDescendingi (mapping : Index -> 'T1 -> 'T2) (list : alist<'T1>) =
        if list.IsConstant then
            constant (fun () -> force list |> IndexList.sortByDescendingi mapping)
        else
            let inline projection i v = ReversedCompare(mapping i v)
            ofReader (fun () -> SortByReader(list, projection))
            
    /// Sorts the list using the keys given by projection in descending order.
    /// Note that the sorting is stable.
    let sortByDescending (mapping : 'T1 -> 'T2) (list : alist<'T1>) =
        if list.IsConstant then
            constant (fun () -> force list |> IndexList.sortByDescending mapping)
        else
            let inline projection _ v = ReversedCompare(mapping v)
            ofReader (fun () -> SortByReader(list, projection))

    /// Sorts the list using the given compare function.
    /// Note that the sorting is stable.
    let sortWith (compare : 'T -> 'T -> int) (list : alist<'T>) =
        if list.IsConstant then
            constant (fun () -> force list |> IndexList.sortWith compare)
        else
            ofReader (fun () -> SortWithReader(list, compare))

    /// Sorts the list.
    let inline sort (list: alist<'T>) = sortWith compare list
    
    /// Sorts the list in descending order.
    let inline sortDescending (list: alist<'T>) = 
        let inline cmp a b = compare b a
        sortWith cmp list
        
    /// Returns a list containing all elements tupled with their successor.
    let pairwise (list: alist<'T>) =
        if list.IsConstant then
            constant (fun () -> force list |> IndexList.pairwise)
        else
            ofReader (fun () -> PairwiseReader(list, false))
        
    /// Returns a list of each element tupled with its successor and the last element tupled with the first.
    let pairwiseCyclic (list: alist<'T>) =
        if list.IsConstant then
            constant (fun () -> force list |> IndexList.pairwiseCyclic)
        else
            ofReader (fun () -> PairwiseReader(list, true))
        


    /// Adaptively reverses the list
    let rev (list: alist<'T>) =
        // TODO: more efficient implementation possible?
        sortByDescendingi (fun i _ -> i) list

    /// Tries to get the element associated to a specific Index from the list.
    /// Note that this operation should not be used extensively since its resulting
    /// aval will be re-evaluated upon every change of the list.
    let tryGet (index: Index) (list: alist<'T>) =
        list.Content |> AVal.map (IndexList.tryGet index)
    
    /// Tries to get the element at a specific position from the list.
    /// Note that this operation should not be used extensively since its resulting
    /// aval will be re-evaluated upon every change of the list.
    let tryAt (index: int) (list: alist<'T>) =
        list.Content |> AVal.map (IndexList.tryAt index)
        
    /// Tries to get the first element from the list.
    let tryFirst (list: alist<'T>) =
        list.Content |> AVal.map IndexList.tryFirst

    /// Tries to get the first element from the list.
    let tryLast (list: alist<'T>) =
        list.Content |> AVal.map IndexList.tryLast

    /// Evaluates the given adaptive list and returns its current content.
    /// This should not be used inside the adaptive evaluation
    /// of other AdaptiveObjects since it does not track dependencies.
    let force (set : alist<'T>) = AVal.force set.Content

    /// Reduces the list using the given `AdaptiveReduction` and returns
    /// the resulting adaptive value.
    let reduce (reduction : AdaptiveReduction<'a, 's, 'v>) (list: alist<'a>) =
        Reductions.ReduceValue(reduction, list) :> aval<'v>
        
    /// Applies the mapping function to all elements of the list and reduces the results
    /// using the given `AdaptiveReduction`.
    /// Returns the resulting adaptive value.
    let reduceBy (reduction : AdaptiveReduction<'b, 's, 'v>) (mapping: Index -> 'a -> 'b) (list: alist<'a>) =
        Reductions.ReduceByValue(reduction, mapping, list) :> aval<'v>
        
    /// Applies the mapping function to all elements of the list and reduces the results
    /// using the given `AdaptiveReduction`.
    /// Returns the resulting adaptive value.
    let reduceByA (reduction : AdaptiveReduction<'b, 's, 'v>) (mapping: Index -> 'a -> aval<'b>) (list: alist<'a>) =
        Reductions.AdaptiveReduceByValue(reduction, mapping, list) :> aval<'v>

    /// Adaptively folds over the list using add for additions and trySubtract for removals.
    /// Note the trySubtract may return None indicating that the result needs to be recomputed.
    /// Also note that the order of elements given to add/trySubtract is undefined.
    let foldHalfGroup (add : 's -> 'a -> 's) (trySubtract : 's -> 'a -> Option<'s>) (zero : 's) (list : alist<'a>) =
        let trySub s a =
            match trySubtract s a with
            | Some v -> ValueSome v
            | None -> ValueNone
        reduce (AdaptiveReduction.halfGroup zero add trySub) list

    /// Adaptively folds over the list using add for additions and subtract for removals.
    /// Note that the order of elements given to add/subtract is undefined.
    let foldGroup (add : 's -> 'a -> 's) (subtract : 's -> 'a -> 's) (zero : 's) (list : alist<'a>) =
        reduce (AdaptiveReduction.group zero add subtract) list

    /// Adaptively folds over the list using add for additions and recomputes the value on every removal.
    /// Note that the order of elements given to add is undefined.
    let fold (add : 's -> 'a -> 's) (zero : 's) (list : alist<'a>) = 
        reduce (AdaptiveReduction.fold zero add) list
        
    /// Adaptively tests if the list is empty.
    let isEmpty (l: alist<'a>) =
        l.Content |> AVal.map IndexList.isEmpty
        
    /// Adaptively gets the number of elements in the list.
    let count (l: alist<'a>) =
        l.Content |> AVal.map IndexList.count

    let forall (predicate: 'T -> bool) (list: alist<'T>) =
        let r = AdaptiveReduction.countNegative |> AdaptiveReduction.mapOut (fun v -> v = 0)
        reduceBy r (fun _ v -> predicate v) list
        
    let exists (predicate: 'T -> bool) (list: alist<'T>) =
        let r = AdaptiveReduction.countPositive |> AdaptiveReduction.mapOut (fun v -> v <> 0)
        reduceBy r (fun _ v -> predicate v) list
        
    let forallA (predicate: 'T -> aval<bool>) (list: alist<'T>) =
        let r = AdaptiveReduction.countNegative |> AdaptiveReduction.mapOut (fun v -> v = 0)
        reduceByA r (fun _ v -> predicate v) list
        
    let existsA (predicate: 'T -> aval<bool>) (list: alist<'T>) =
        let r = AdaptiveReduction.countPositive |> AdaptiveReduction.mapOut (fun v -> v <> 0)
        reduceByA r (fun _ v -> predicate v) list

    let init (length: aval<int>) (initializer: int -> 'T) =
        if length.IsConstant then
            constant (fun () -> IndexList.init (AVal.force length) initializer)
        else
            ofReader (fun () -> InitReader(length, initializer))
        
    let inline range (lowerBound: aval< ^T >) (upperBound: aval< ^T >) =
        if lowerBound.IsConstant && upperBound.IsConstant then
            ofSeq (seq { AVal.force lowerBound .. AVal.force upperBound })
        else
            // Reader for range operation
            // Must be inline via object expression since this is generic
            ofReader (fun () -> 
                let zero = LanguagePrimitives.GenericZero< ^T >
                let one = LanguagePrimitives.GenericOne< ^T >
                let minusOne = -one
                let mutable lastMax = minusOne
                let mutable lastMin = zero
                let mutable idxs = IndexList.empty
                { new AbstractReader<IndexListDelta< ^T >>(IndexListDelta.empty) with 
                    override x.Compute(token) =
                        let newMin = lowerBound.GetValue(token)
                        let newMax = upperBound.GetValue(token)

                        let (maxIncreaseLow, maxIncreaseHigh), 
                            (maxDecreaseLow, maxDecreaseHigh),
                            (minDecreaseLow, minDecreaseHigh),
                            (minIncreaseLow, minIncreaseHigh) = 
                              RangeDelta.rangeChange (lastMin, lastMax, newMin, newMax) 

                        let mutable delta = IndexListDelta.empty
                        
                        // Count up through additions caused by increasing maximum
                        for i in maxIncreaseLow .. maxIncreaseHigh do  
                            idxs <- idxs.Add i
                            delta <- delta.Add(IndexList.lastIndex idxs, Set i)

                        // Count down through removals caused by decreasing maximum
                        for i in maxDecreaseLow .. minusOne .. maxDecreaseHigh do  
                            let lastMaxIdx = IndexList.lastIndex idxs
                            delta <- delta.Add(lastMaxIdx, Remove)
                            idxs <- IndexList.remove lastMaxIdx idxs 

                        // Count down through additions caused by decreasing minimum
                        for i in minDecreaseLow .. minusOne .. minDecreaseHigh do  
                            idxs <- idxs.Prepend i
                            delta <- delta.Add(IndexList.firstIndex idxs, Set i)

                        // Count up through removals caused by increasing minimum
                        for i in minIncreaseLow .. minIncreaseHigh do  
                            let lastMinIdx = IndexList.firstIndex idxs
                            delta <- delta.Add(lastMinIdx, Remove)
                            idxs <- IndexList.remove lastMinIdx idxs 

                        lastMax <- newMax
                        lastMin <- newMin
                        delta })
        
    /// Adaptively counts all elements fulfilling the predicate
    let countBy (predicate: 'a -> bool) (list: alist<'a>) =
        reduceBy AdaptiveReduction.countPositive (fun _ v -> predicate v) list 

    /// Adaptively counts all elements fulfilling the predicate
    let countByA (predicate: 'a -> aval<bool>) (list: alist<'a>) =
        reduceByA AdaptiveReduction.countPositive (fun _ v -> predicate v) list 

    let inline tryMin (list : alist<'a>) =
        let reduction = 
            AdaptiveReduction.tryMin
            |> AdaptiveReduction.mapOut (function ValueSome v -> Some v | ValueNone -> None)
        reduce reduction list

    let inline tryMax (list : alist<'a>) =
        let reduction = 
            AdaptiveReduction.tryMax
            |> AdaptiveReduction.mapOut (function ValueSome v -> Some v | ValueNone -> None)
        reduce reduction list

    /// Adaptively computes the sum all entries in the list.
    let inline sum (list : alist<'a>) = 
        reduce (AdaptiveReduction.sum()) list
    
    let inline sumBy (mapping : 'T1 -> 'T2) (list : alist<'T1>) =
        reduceBy (AdaptiveReduction.sum()) (fun _ v -> mapping v) list

    let inline sumByA (mapping : 'T1 -> aval<'T2>) (list : alist<'T1>) =
        reduceByA (AdaptiveReduction.sum()) (fun _ v -> mapping v) list

    let inline average (list : alist<'a>) =
        reduce (AdaptiveReduction.average()) list
        
    let inline averageBy (mapping : 'T1 -> 'T2) (list : alist<'T1>) =
        reduceBy (AdaptiveReduction.average()) (fun _ v -> mapping v) list

    let inline averageByA (mapping : 'T1 -> aval<'T2>) (list : alist<'T1>) =
        reduceByA (AdaptiveReduction.average()) (fun _ v -> mapping v) list


    /// Adaptively computes the product of all entries in the list.
    let inline product (s : alist<'a>) = 
        reduce (AdaptiveReduction.product()) s
