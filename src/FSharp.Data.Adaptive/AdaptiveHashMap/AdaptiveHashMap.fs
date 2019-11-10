namespace FSharp.Data.Adaptive

open FSharp.Data.Traceable

/// An adaptive reader for amap that allows to pull operations and exposes its current state.
type IHashMapReader<'Key, 'Value> = IOpReader<HashMap<'Key, 'Value>, HashMapDelta<'Key, 'Value>>

/// Adaptive map datastructure.
type AdaptiveHashMap<'Key, 'Value> =
    /// Is the map constant?
    abstract member IsConstant : bool

    /// The current content of the map as aval.
    abstract member Content : aval<HashMap<'Key, 'Value>>

    /// Gets a new reader to the map.
    abstract member GetReader : unit -> IHashMapReader<'Key, 'Value>

/// Adaptive map datastructure.
and amap<'Key, 'Value> = AdaptiveHashMap<'Key, 'Value>



/// Internal implementations for alist reductions.
module internal MapReductions =

    /// aval for reduce operations.
    type ReduceValue<'k, 'a, 's, 'v>(reduction : AdaptiveReduction<'a, 's, 'v>, input : amap<'k, 'a>) =
        inherit AdaptiveObject()

        let reader = input.GetReader()
        let mutable sum = reduction.seed

        let mutable result = Unchecked.defaultof<'v>
        let mutable state = HashMap.empty<'k, 'a>

        member x.GetValue(token : AdaptiveToken) =
            x.EvaluateAlways token (fun token ->
                if x.OutOfDate then
                    let ops = reader.GetChanges token
                    let mutable working = true
                    use e = (ops :> seq<_>).GetEnumerator()
                    while working && e.MoveNext() do
                        let index, op = e.Current
                        match op with
                        | Set a ->
                            match HashMap.tryFind index state with
                            | Some old ->
                                match reduction.sub sum old with
                                | ValueSome s -> sum <- s
                                | ValueNone -> working <- false
                            | None ->
                                ()

                            sum <- reduction.add sum a
                            state <- HashMap.add index a state

                        | Remove ->
                            match HashMap.tryRemove index state with
                            | Some(old, rest) ->
                                state <- rest
                                match reduction.sub sum old with
                                | ValueSome s -> sum <- s
                                | ValueNone -> working <- false
                            | None ->
                                ()

                    if not working then
                        state <- reader.State
                        sum <- state |> HashMap.fold (fun s _ v -> reduction.add s v) reduction.seed

                    result <- reduction.view sum

                result
            )

        interface AdaptiveValue with
            member x.GetValueUntyped t = 
                x.GetValue t :> obj
            member x.ContentType =
                #if FABLE_COMPILER
                typeof<obj>
                #else
                typeof<'v>
                #endif

        interface AdaptiveValue<'v> with
            member x.GetValue t = x.GetValue t
            
    /// aval for reduceBy operations.
    type ReduceByValue<'k, 'a, 'b, 's, 'v>(reduction : AdaptiveReduction<'b, 's, 'v>, mapping : 'k -> 'a -> 'b, input : amap<'k, 'a>) =
        inherit AdaptiveObject()

        let reader = input.GetReader()
        let mutable sum = ValueSome reduction.seed

        let mutable result = Unchecked.defaultof<'v>
        let mutable state = HashMap.empty<'k, 'b>

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
                    for (index, op) in ops do
                        match op with
                        | Set a ->
                            match HashMap.tryFind index state with
                            | Some old -> sum <- sub sum old
                            | None -> ()

                            let b = mapping index a

                            sum <- add sum b
                            state <- HashMap.add index b state

                        | Remove ->
                            match HashMap.tryRemove index state with
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

        interface AdaptiveValue with
            member x.GetValueUntyped t = 
                x.GetValue t :> obj
            member x.ContentType =
                #if FABLE_COMPILER
                typeof<obj>
                #else
                typeof<'v>
                #endif

        interface AdaptiveValue<'v> with
            member x.GetValue t = x.GetValue t

    /// aval for reduceByA operations.
    type AdaptiveReduceByValue<'k, 'a, 'b, 's, 'v>(reduction : AdaptiveReduction<'b, 's, 'v>, mapping : 'k -> 'a -> aval<'b>, l : amap<'k, 'a>) =
        inherit AdaptiveObject()

        let reader = l.GetReader()
        do reader.Tag <- "FoldReader"

        #if !FABLE_COMPILER
        let dirtyLock = obj()
        #endif


        let mutable targets = MultiSetMap.empty<aval<'b>, 'k>
        let mutable state = HashMap.empty<'k, aval<'b> * 'b>

        let mutable dirty : HashMap<'k, aval<'b>> = HashMap.empty
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

        let removeIndex (x : AdaptiveReduceByValue<_,_,_,_,_>) (i : 'k) =
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
                    for (i, op) in ops do
                        dirty <- HashMap.remove i dirty
                        match op with
                        | Set v ->
                            removeIndex x i

                            let r = mapping i v
                            let n = r.GetValue(t)
                            targets <- MultiSetMap.add r i targets
                            state <- HashMap.add i (r, n) state
                            sum <- add sum n

                        | Remove ->
                            removeIndex x i


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

        interface AdaptiveValue with
            member x.GetValueUntyped t = x.GetValue t :> obj
            member x.ContentType =
                #if FABLE_COMPILER
                typeof<obj>
                #else
                typeof<'v>
                #endif

        interface AdaptiveValue<'v> with
            member x.GetValue t = x.GetValue t  


/// Internal implementations for amap operations.
module AdaptiveHashMapImplementation =

    /// Core implementation for a dependent map.
    type AdaptiveHashMapImpl<'Key, 'Value>(createReader : unit -> IOpReader<HashMapDelta<'Key, 'Value>>) =
        let history = History(createReader, HashMap.trace)
        /// Gets a new reader to the set.
        member x.GetReader() : IHashMapReader<'Key, 'Value> =
            history.NewReader()

        /// Current content of the set as aval.
        member x.Content =
            history :> aval<_>

        interface AdaptiveHashMap<'Key, 'Value> with
            member x.IsConstant = false
            member x.GetReader() = x.GetReader()
            member x.Content = x.Content

    /// Efficient implementation for an empty adaptive map.
    type EmptyMap<'Key, 'Value> private() =   
        static let instance = EmptyMap<'Key, 'Value>() :> amap<_,_>
        let content = AVal.constant HashMap.empty
        let reader = new History.Readers.EmptyReader<HashMap<'Key, 'Value>, HashMapDelta<'Key, 'Value>>(HashMap.trace) :> IHashMapReader<'Key, 'Value>
        static member Instance = instance
        
        member x.Content = content
        member x.GetReader() = reader
        
        interface AdaptiveHashMap<'Key, 'Value> with
            member x.IsConstant = true
            member x.GetReader() = x.GetReader()
            member x.Content = x.Content

    /// Efficient implementation for a constant adaptive map.
    type ConstantMap<'Key, 'Value>(content : Lazy<HashMap<'Key, 'Value>>) =
        let value = AVal.delay (fun () -> content.Value)

        member x.Content = value

        member x.GetReader() =
            new History.Readers.ConstantReader<_,_>(
                HashMap.trace,
                lazy (HashMap.computeDelta HashMap.empty content.Value),
                content
            ) :> IHashMapReader<_,_>

        interface AdaptiveHashMap<'Key, 'Value> with
            member x.IsConstant = true
            member x.GetReader() = x.GetReader()
            member x.Content = x.Content

    /// Reader for map operations.
    type MapWithKeyReader<'Key, 'Value1, 'Value2>(input : amap<'Key, 'Value1>, mapping : 'Key -> 'Value1 -> 'Value2) =
        inherit AbstractReader<HashMapDelta<'Key, 'Value2>>(HashMapDelta.empty)
        
        let reader = input.GetReader()

        override x.Compute(token) =
            let ops = reader.GetChanges token
            ops.Store |> HashMap.map (fun k op ->
                match op with
                    | Set v -> Set (mapping k v)
                    | Remove -> Remove
            ) |> HashMapDelta
            
    /// Reader for map operations without keys.
    type MapReader<'Key, 'Value1, 'Value2>(input : amap<'Key, 'Value1>, mapping : 'Value1 -> 'Value2) =
        inherit AbstractReader<HashMapDelta<'Key, 'Value2>>(HashMapDelta.empty)

        let cache = Cache<'Value1, 'Value2>(mapping)
        let reader = input.GetReader()

        override x.Compute(token) =
            let oldState = reader.State
            let ops = reader.GetChanges token
            ops.Store |> HashMap.map (fun k op ->
                match op with
                    | Set v -> 
                        let res = cache.Invoke(v)
                        Set res
                    | Remove -> 
                        match HashMap.tryFind k oldState with
                        | Some value -> cache.RevokeAndGetDeleted value |> ignore
                        | None -> () // strange
                        Remove
            ) |> HashMapDelta

    /// Reader for choose operations.
    type ChooseWithKeyReader<'Key, 'Value1, 'Value2>(input : amap<'Key, 'Value1>, mapping : 'Key -> 'Value1 -> option<'Value2>) =
        inherit AbstractReader<HashMapDelta<'Key, 'Value2>>(HashMapDelta.empty)

        let reader = input.GetReader()
        let livingKeys = UncheckedHashSet.create<'Key>()

        override x.Compute(token) =
            let ops = reader.GetChanges token
            ops.Store |> HashMap.choose (fun k op ->
                match op with
                | Set v -> 
                    match mapping k v with
                    | Some n ->
                        livingKeys.Add k |> ignore
                        Some (Set n)
                    | None ->
                        if livingKeys.Remove k then
                            Some Remove
                        else
                            None
                | Remove -> 
                    if livingKeys.Remove k then Some Remove
                    else None
            ) |> HashMapDelta
            
    /// Reader for choose operations without keys.
    type ChooseReader<'Key, 'Value1, 'Value2>(input : amap<'Key, 'Value1>, f : 'Value1 -> option<'Value2>) =
        inherit AbstractReader<HashMapDelta<'Key, 'Value2>>(HashMapDelta.empty)

        let reader = input.GetReader()
        let cache = Cache f
        let livingKeys = UncheckedHashSet.create<'Key>()

        override x.Compute(token) =
            let oldState = reader.State
            let ops = reader.GetChanges token
            ops.Store |> HashMap.choose (fun k op ->
                match op with
                | Set v -> 
                    match cache.Invoke v with
                    | Some r -> 
                        livingKeys.Add k |> ignore
                        Some (Set r)
                    | None -> 
                        match HashMap.tryFind k oldState with
                        | Some ov -> 
                            livingKeys.Remove k |> ignore
                            cache.Revoke ov |> ignore
                            Some Remove
                        | _ -> 
                            None
                | Remove -> 
                    if livingKeys.Remove k then 
                        Some Remove
                    else
                        None
            ) |> HashMapDelta

    /// Reader for union/unionWith operations.
    type UnionWithReader<'Key, 'Value>(l : amap<'Key, 'Value>, r : amap<'Key, 'Value>, resolve : 'Key -> 'Value -> 'Value -> 'Value) =
        inherit AbstractReader<HashMapDelta<'Key, 'Value>>(HashMapDelta.empty)

        let lReader = l.GetReader()
        let rReader = r.GetReader()

        override x.Compute(token) =
            let lops = lReader.GetChanges token
            let rops = rReader.GetChanges token

            let merge (key : 'Key) (lop : option<ElementOperation<'Value>>) (rop : option<ElementOperation<'Value>>) : ElementOperation<'Value> =
                let lv =
                    match lop with
                    | Some (Set lv) -> Some lv
                    | Some (Remove) -> None
                    | None -> HashMap.tryFind key lReader.State
                            
                let rv =
                    match rop with
                    | Some (Set rv) -> Some rv
                    | Some (Remove) -> None
                    | None -> HashMap.tryFind key rReader.State


                match lv, rv with
                | None, None -> Remove
                | Some l, None -> Set l
                | None, Some r -> Set r
                | Some l, Some r -> Set (resolve key l r)

            HashMap.map2 merge lops.Store rops.Store |> HashMapDelta

    /// Reader for ofAVal.
    type AValReader<'Seq, 'Key, 'Value when 'Seq :> seq<'Key * 'Value>>(input : aval<'Seq>) =
        inherit AbstractReader<HashMap<'Key, 'Value>, HashMapDelta<'Key, 'Value>>(HashMap.trace)

        override x.Compute(token) =
            input.GetValue token
            :> seq<_>
            |> HashMap.ofSeq
            |> HashMap.computeDelta x.State

    /// Reader for bind.
    type BindReader<'T, 'Key, 'Value>(value : aval<'T>, mapping : 'T -> amap<'Key, 'Value>) =
        inherit AbstractReader<HashMapDelta<'Key, 'Value>>(HashMapDelta.empty)

        let mutable oldValue : option<'T * IHashMapReader<'Key, 'Value>> = None

        override x.Compute(token) =
            let v = value.GetValue token
            
            match oldValue with
            | Some (ov, r) when Unchecked.equals ov v ->
                r.GetChanges(token)
            | _ ->
                let rem =
                    match oldValue with
                    | Some (_, oldReader) ->
                        let res = HashMap.computeDelta oldReader.State HashMap.empty
                        oldReader.Outputs.Remove x |> ignore
                        res.Store
                    | _ ->
                        HashMap.empty
                                
                let newMap = mapping v
                let newReader = newMap.GetReader()
                oldValue <- Some(v, newReader)
                let add = newReader.GetChanges token
                HashMapDelta.combine (HashMapDelta rem) add

    /// Reader for toASet.
    type ToASetReader<'Key, 'Value>(input : amap<'Key, 'Value>) =
        inherit AbstractReader<HashSetDelta<'Key * 'Value>>(HashSetDelta.empty)

        let reader = input.GetReader()

        override x.Compute(token) =
            let oldState = reader.State
            let ops = reader.GetChanges token
            let mutable deltas = HashSetDelta.empty

            for (k,op) in ops do
                match op with
                | Set v ->
                    match HashMap.tryFind k oldState with
                    | None -> ()
                    | Some oldValue ->
                        deltas <- HashSetDelta.add (Rem(k, oldValue)) deltas
                    deltas <- HashSetDelta.add (Add(k, v)) deltas

                | Remove ->
                    // NOTE: As it is not clear at what point the toASet computation has been evaluated last, it is 
                    //       a valid case that something is removed that is not present in the current local state.
                    match HashMap.tryFind k oldState with
                    | None -> ()
                    | Some ov ->
                        deltas <- HashSetDelta.add (Rem (k, ov)) deltas


            deltas

    /// Reader for mapSet.
    type MapSetReader<'Key, 'Value>(set : aset<'Key>, mapping : 'Key -> 'Value) =
        inherit AbstractReader<HashMapDelta<'Key, 'Value>>(HashMapDelta.empty)

        let reader = set.GetReader()

        override x.Compute(token) =
            reader.GetChanges(token).Store
            |> HashMap.choose (fun key v ->
                if v > 0 then Some (Set (mapping key))
                elif v < 0 then Some Remove
                else None
            )
            |> HashMapDelta


    /// Base class for standard avals
    [<AbstractClass; StructuredFormatDisplay("{AsString}")>]
    type internal AbstractVal<'T>() =
        inherit AdaptiveObject()

        let mutable cache = Unchecked.defaultof<'T>

        abstract member Compute: AdaptiveToken -> 'T

        member x.GetValue(token: AdaptiveToken) =
            x.EvaluateAlways token (fun token ->
                if x.OutOfDate then
                    let v = x.Compute token
                    cache <- v
                    v
                else
                    cache                
            )

        member private x.AsString =
            if x.OutOfDate then sprintf "aval*(%A)" cache
            else sprintf "aval(%A)" cache

        override x.ToString() =
            if x.OutOfDate then System.String.Format("aval*({0})", cache)
            else System.String.Format("aval({0})", cache)
            
        interface AdaptiveValue with
            member x.GetValueUntyped t = x.GetValue t :> obj
            member x.ContentType = 
                #if FABLE_COMPILER
                typeof<obj>
                #else
                typeof<'T>
                #endif
    
        interface AdaptiveValue<'T> with
            member x.GetValue t = x.GetValue t  
    
    /// Reader used for ofASet operations.
    /// It's safe to assume that the view function will only be called with non-empty HashSets.
    /// Internally assumes that the view function is cheap.
    type SetReader<'Key, 'Value, 'View>(input : aset<'Key * 'Value>, view : HashSet<'Value> -> 'View) =
        inherit AbstractReader<HashMapDelta<'Key, 'View>>(HashMapDelta.empty)

        let reader = input.GetReader()
        let state = UncheckedDictionary.create<'Key, HashSet<'Value>>()

        override x.Compute (token : AdaptiveToken) =
            reader.GetChanges token |> Seq.choose (fun op ->
                match op with
                | Add(_, (k, v)) ->
                    match state.TryGetValue k with
                    | (true, set) ->    
                        let newSet = HashSet.add v set
                        state.[k] <- newSet
                        Some (k, Set (view newSet))
                    | _ ->
                        let newSet = HashSet.single v
                        state.[k] <- newSet
                        Some (k, Set (view newSet))
                | Rem(_, (k, v)) ->
                    match state.TryGetValue k with
                    | (true, set) ->    
                        let newSet = HashSet.remove v set
                        if newSet.IsEmpty then 
                            state.Remove k |> ignore
                            Some (k, Remove)
                        else 
                            state.[k] <- newSet
                            Some (k, Set (view newSet))
                    | _ ->
                        None
            )
            |> HashMapDelta.ofSeq

    /// Gets the current content of the amap as HashMap.
    let inline force (map : amap<'Key, 'Value>) = 
        AVal.force map.Content

    /// Creates a constant map using the creation function.
    let inline constant (content : unit -> HashMap<'Key, 'Value>) = 
        ConstantMap(lazy(content())) :> amap<_,_> 

    /// Creates an adaptive map using the reader.
    let inline create (reader : unit -> #IOpReader<HashMapDelta<'Key, 'Value>>) =
        AdaptiveHashMapImpl(fun () -> reader() :> IOpReader<_>) :> amap<_,_>

/// Functional operators for amap<_,_>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AMap =
    open AdaptiveHashMapImplementation

    /// The empty map.
    let empty<'Key, 'Value> = EmptyMap<'Key, 'Value>.Instance
    
    /// A constant amap holding a single key/value.
    let single (key : 'Key) (value : 'Value) =
        constant (fun () -> HashMap.single key value)
        
    /// Creates an amap holding the given entries.
    let ofSeq (elements : seq<'Key * 'Value>) =
        constant (fun () -> HashMap.ofSeq elements)
        
    /// Creates an amap holding the given entries.
    let ofList (elements : list<'Key * 'Value>) =
        constant (fun () -> HashMap.ofList elements)
        
    /// Creates an amap holding the given entries.
    let ofArray (elements : array<'Key * 'Value>) =
        constant (fun () -> HashMap.ofArray elements)
        
    /// Creates an amap holding the given entries.
    let ofHashMap (elements : HashMap<'Key, 'Value>) =
        constant (fun () -> elements)

    /// Creates an amap for the given aval.
    let ofAVal (value : aval<#seq<'Key * 'Value>>) =
        if value.IsConstant then
            constant (fun () -> value |> AVal.force :> seq<_> |> HashMap.ofSeq)
        else
            create (fun () -> AValReader(value))

    /// Creates an amap from the given set and takes an arbitrary value for duplicate entries.
    let ofASetIgnoreDuplicates (set: aset<'Key * 'Value>) =
        if set.IsConstant then
            constant (fun () -> 
                let mutable result = HashMap.empty
                for (k,v) in AVal.force set.Content do
                    result <- HashMap.add k v result

                result
            )
        else
            create (fun () -> SetReader(set, Seq.head))
    
    /// Creates an amap from the given set while keeping all duplicate values for a key in a HashSet.           
    let ofASet (set: aset<'Key * 'Value>) =
        if set.IsConstant then
            constant (fun () -> 
                let mutable result = HashMap.empty
                for (k,v) in AVal.force set.Content do
                    result <- 
                        result |> HashMap.alter k (fun o ->
                            match o with
                            | Some o -> HashSet.add v o |> Some
                            | None -> HashSet.single v |> Some
                        )

                result
            )
        else
            create (fun () -> SetReader(set, id))

    /// Creates an aval providing access to the current content of the map.
    let toAVal (map : amap<'Key, 'Value>) = map.Content

    /// Adaptively maps over the given map.
    let map (mapping : 'Key -> 'Value1 -> 'Value2) (map : amap<'Key, 'Value1>) =
        if map.IsConstant then
            constant (fun () -> map |> force |> HashMap.map mapping)
        else
            create (fun () -> MapWithKeyReader(map, mapping))
    
    /// Creates an amap with the keys from the set and the values given by mapping.
    let mapSet (mapping : 'Key -> 'Value) (set : aset<'Key>) =
        if set.IsConstant then
            constant (fun () ->     
                let c = set.Content |> AVal.force
                let newStore = c.Store |> IntMap.map (List.map (fun key -> struct(key, mapping key)))
                HashMap(c.Count, newStore)
            )
        else
            create (fun () -> MapSetReader(set, mapping))

    /// Adaptively maps over the given map without exposing keys.
    let map' (mapping : 'Value1 -> 'Value2) (map : amap<'Key, 'Value1>) =
        if map.IsConstant then
            constant (fun () -> map |> force |> HashMap.map (fun _ -> mapping))
        else
            create (fun () -> MapReader(map, mapping))
        
    /// Adaptively chooses all elements returned by mapping.  
    let choose (mapping : 'Key -> 'Value1 -> option<'Value2>) (map : amap<'Key, 'Value1>) =
        if map.IsConstant then
            constant (fun () -> map |> force |> HashMap.choose mapping)
        else
            create (fun () -> ChooseWithKeyReader(map, mapping))
            
    /// Adaptively chooses all elements returned by mapping without exposing keys.  
    let choose' (mapping : 'Value1 -> option<'Value2>) (map : amap<'Key, 'Value1>) =
        if map.IsConstant then
            constant (fun () -> map |> force |> HashMap.choose (fun _ -> mapping))
        else
            create (fun () -> ChooseReader(map, mapping))
 
    /// Adaptively filters the set using the given predicate.
    let filter (predicate : 'Key -> 'Value -> bool) (map : amap<'Key, 'Value>) =
        choose (fun k v -> if predicate k v then Some v else None) map

    /// Adaptively filters the set using the given predicate without exposing keys.
    let filter' (predicate : 'Value -> bool) (map : amap<'Key, 'Value>) =
        choose' (fun v -> if predicate v then Some v else None) map

    /// Adaptively unions both maps using the given resolve functions when colliding entries are found.
    let unionWith (resolve : 'Key -> 'Value -> 'Value -> 'Value) (a : amap<'Key, 'Value>) (b : amap<'Key, 'Value>) =
        if a.IsConstant && b.IsConstant then
            constant (fun () ->
                let va = force a
                let vb = force b
                HashMap.unionWith resolve va vb
            )
        else
            create (fun () -> UnionWithReader(a, b, resolve))

    /// Adaptively unions both maps preferring the right value when colliding entries are found.
    let union (a : amap<'Key, 'Value>) (b : amap<'Key, 'Value>) =
        unionWith (fun _ _ r -> r) a b

    /// Adaptively maps over the given aval and returns the resulting map.
    let bind (mapping : 'T -> amap<'Key, 'Value>) (value : aval<'T>) =
        if value.IsConstant then
            mapping (AVal.force value)
        else
            create (fun () -> BindReader(value, mapping))

    /// Creates an aset holding all key/value tuples from the map.
    let toASet (map : amap<'Key, 'Value>) = 
        if map.IsConstant then
            ASet.delay (fun () -> map |> force |> HashMap.toSeq |> HashSet.ofSeq)
        else
            ASet.ofReader (fun () -> ToASetReader(map))

    /// Adaptively looks up the given key in the map.
    /// Note that this operation should not be used extensively since its resulting
    /// aval will be re-evaluated upon every change of the map.
    let tryFind (key: 'K) (map: amap<'K, 'V>) =
        map.Content |> AVal.map (HashMap.tryFind key)

    /// Evaluates the given adaptive map and returns its current content.
    /// This should not be used inside the adaptive evaluation
    /// of other AdaptiveObjects since it does not track dependencies.
    let force (set : amap<'K, 'V>) = AVal.force set.Content
    
    /// Adaptively tests if the map is empty.
    let isEmpty (l: amap<'K, 'V>) =
        l.Content |> AVal.map HashMap.isEmpty
        
    /// Adaptively gets the number of elements in the list.
    let count (l: amap<'K, 'V>) =
        l.Content |> AVal.map HashMap.count


    /// Reduces the map using the given `AdaptiveReduction` and returns
    /// the resulting adaptive value.
    let reduce (r : AdaptiveReduction<'a, 's, 'v>) (map: amap<'k, 'a>) =
        MapReductions.ReduceValue(r, map) :> aval<'v>
        
    /// Applies the mapping function to all elements of the map and reduces the results
    /// using the given `AdaptiveReduction`.
    /// Returns the resulting adaptive value.
    let reduceBy (r : AdaptiveReduction<'b, 's, 'v>) (mapping: 'k -> 'a -> 'b) (map: amap<'k, 'a>) =
        MapReductions.ReduceByValue(r, mapping, map) :> aval<'v>
        
    /// Applies the mapping function to all elements of the map and reduces the results
    /// using the given `AdaptiveReduction`.
    /// Returns the resulting adaptive value.
    let reduceByA (r : AdaptiveReduction<'b, 's, 'v>) (mapping: 'k -> 'a -> aval<'b>) (map: amap<'k, 'a>) =
        MapReductions.AdaptiveReduceByValue(r, mapping, map) :> aval<'v>
        
    let forall (predicate : 'K -> 'V -> bool) (map: amap<'K, 'V>) =
        let reduction = AdaptiveReduction.countNegative |> AdaptiveReduction.mapOut (fun v -> v = 0)
        reduceBy reduction predicate map
        
    let exists (predicate : 'K -> 'V -> bool) (map: amap<'K, 'V>) =
        let reduction = AdaptiveReduction.countPositive |> AdaptiveReduction.mapOut (fun v -> v <> 0)
        reduceBy reduction predicate map
        
    let forallA (predicate : 'K -> 'V -> aval<bool>) (map: amap<'K, 'V>) =
        let reduction = AdaptiveReduction.countNegative |> AdaptiveReduction.mapOut (fun v -> v = 0)
        reduceByA reduction predicate map
        
    let existsA (predicate : 'K -> 'V -> aval<bool>) (map: amap<'K, 'V>) =
        let reduction = AdaptiveReduction.countPositive |> AdaptiveReduction.mapOut (fun v -> v <> 0)
        reduceByA reduction predicate map
        
    let inline sumBy (mapping : 'K -> 'V -> 'T) (map : amap<'K, 'V>) =
        reduceBy (AdaptiveReduction.sum()) mapping map
        
    let inline sumByA (mapping : 'K -> 'V -> aval<'T>) (map : amap<'K, 'V>) =
        reduceByA (AdaptiveReduction.sum()) mapping map
        
    let inline averageBy (mapping : 'K -> 'V -> 'T) (map : amap<'K, 'V>) =
        reduceBy (AdaptiveReduction.average()) mapping map
        
    let inline averageByA (mapping : 'K -> 'V -> aval<'T>) (map : amap<'K, 'V>) =
        reduceByA (AdaptiveReduction.average()) mapping map
        
    let countBy (mapping : 'K -> 'V -> bool) (map : amap<'K, 'V>) =
        reduceBy (AdaptiveReduction.countPositive) mapping map
        
    let countByA (mapping : 'K -> 'V -> aval<bool>) (map : amap<'K, 'V>) =
        reduceByA (AdaptiveReduction.countPositive) mapping map

    /// Adaptively folds over the map using add for additions and trySubtract for removals.
    /// Note the trySubtract may return None indicating that the result needs to be recomputed.
    /// Also note that the order of elements given to add/trySubtract is undefined.
    let foldHalfGroup (add : 'S -> 'K -> 'V -> 'S) (trySub : 'S -> 'K -> 'V -> option<'S>) (zero : 'S) (map : amap<'K, 'V>) =
        let inline trySub s (struct(k,v)) =
            match trySub s k v with
            | Some v -> ValueSome v
            | None -> ValueNone

        let inline add s (struct(k,v)) = add s k v
        reduceBy (AdaptiveReduction.halfGroup zero add trySub) (fun k v -> struct(k,v)) map
        
    /// Adaptively folds over the map using add for additions and subtract for removals.
    /// Note that the order of elements given to add/subtract is undefined.
    let foldGroup (add : 'S -> 'K -> 'V -> 'S) (sub : 'S -> 'K -> 'V -> 'S) (zero : 'S) (map : amap<'K, 'V>) =
        let inline sub s (struct(k,v)) = sub s k v
        let inline add s (struct(k,v)) = add s k v
        reduceBy (AdaptiveReduction.group zero add sub) (fun k v -> struct(k,v)) map
        
    /// Adaptively folds over the map using add for additions and recomputes the value on every removal.
    /// Note that the order of elements given to add is undefined.
    let fold (add : 'S -> 'K -> 'V -> 'S) (zero : 'S) (map : amap<'K, 'V>) =
        let inline add s (struct(k,v)) = add s k v
        reduceBy (AdaptiveReduction.fold zero add) (fun k v -> struct(k,v)) map