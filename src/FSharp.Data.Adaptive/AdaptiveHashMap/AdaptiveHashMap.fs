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
        inherit AbstractReader<HashMapDelta<'Key, 'Value2>>(HashMapDelta.monoid)
        
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
        inherit AbstractReader<HashMapDelta<'Key, 'Value2>>(HashMapDelta.monoid)

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
        inherit AbstractReader<HashMapDelta<'Key, 'Value2>>(HashMapDelta.monoid)

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
        inherit AbstractReader<HashMapDelta<'Key, 'Value2>>(HashMapDelta.monoid)

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
        inherit AbstractReader<HashMapDelta<'Key, 'Value>>(HashMapDelta.monoid)

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
        inherit AbstractReader<HashMapDelta<'Key, 'Value>>(HashMapDelta.monoid)

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
        inherit AbstractReader<HashSetDelta<'Key * 'Value>>(HashSetDelta.monoid)

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
        inherit AbstractReader<HashMapDelta<'Key, 'Value>>(HashMapDelta.monoid)

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

        interface AdaptiveValue<'T> with
            member x.GetValue t = x.GetValue t  
            

    type internal KeyedMod<'Key, 'Value>(key : 'Key, m : aval<'Value>) =
        inherit AbstractVal<'Key * 'Value>()

        let mutable last = None
            
        member x.Key = key
            
        member x.UnsafeLast =
            last

        override x.Compute(token) =
            let v = m.GetValue(token)
            last <- Some v
            key, v

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

    /// Creates an amap for the given aval.
    let ofAVal (value : aval<#seq<'Key * 'Value>>) =
        if value.IsConstant then
            constant (fun () -> value |> AVal.force :> seq<_> |> HashMap.ofSeq)
        else
            create (fun () -> AValReader(value))

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
    let tryFind (key: 'K) (map: amap<'K, 'V>) =
        map.Content |> AVal.map (HashMap.tryFind key)


    /// Adaptively folds over the map using add for additions and trySubtract for removals.
    /// Note the trySubtract may return None indicating that the result needs to be recomputed.
    /// Also note that the order of elements given to add/trySubtract is undefined.
    let foldHalfGroup (add : 'S -> 'K -> 'V -> 'S) (trySub : 'S -> 'K -> 'V -> option<'S>) (zero : 'S) (map : amap<'K, 'V>) =
        let r = map.GetReader()
        let mutable res = zero

        let rec traverse (old : HashMap<'K, 'V>) (d : list<'K * ElementOperation<'V>>) =
            match d with
                | [] -> true
                | (k, op) :: rest ->
                    match op with
                    | Set v -> 
                        match HashMap.tryFind k old with
                        | None ->
                            res <- add res k v
                            traverse old rest
                        | Some o ->
                            match trySub res k o with
                            | Some r ->
                                res <- add r k v
                                traverse old rest
                            | None ->
                                false
                                
                        

                    | Remove ->
                        match HashMap.tryFind k old with
                        | Some o ->
                            match trySub res k o with
                            | Some s ->
                                res <- s
                                traverse old rest
                            | None ->
                                false
                        | None ->
                            traverse old rest
                                  

        AVal.custom (fun token ->
            
            let old = r.State
            let ops = r.GetChanges token
            let worked = traverse old (HashMapDelta.toList ops)

            if not worked then
                res <- r.State |> HashMap.fold add zero
                
            res
        )
        
    /// Adaptively folds over the map using add for additions and subtract for removals.
    /// Note that the order of elements given to add/subtract is undefined.
    let foldGroup (add : 'S -> 'K -> 'V -> 'S) (sub : 'S -> 'K -> 'V -> 'S) (zero : 'S) (map : amap<'K, 'V>) =
        foldHalfGroup add (fun s k v -> sub s k v |> Some) zero map
        
    /// Adaptively folds over the map using add for additions and recomputes the value on every removal.
    /// Note that the order of elements given to add is undefined.
    let fold (add : 'S -> 'K -> 'V -> 'S) (zero : 'S) (map : amap<'K, 'V>) =
        foldHalfGroup add (fun _ _ _ -> None) zero map