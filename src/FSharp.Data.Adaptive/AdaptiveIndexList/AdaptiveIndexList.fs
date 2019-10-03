namespace FSharp.Data.Adaptive

open FSharp.Data.Traceable

/// An adaptive reader for alist that allows to pull operations and exposes its current state.
type IIndexListReader<'T> = 
    IOpReader<IndexList<'T>, IndexListDelta<'T>>

/// Adaptive list datastructure.
[<Interface>]
type AdaptiveIndexList<'T> =
    /// Is the list constant?
    abstract member IsConstant : bool

    /// The current content of the list as aval.
    abstract member Content : aval<IndexList<'T>>
    
    /// Gets a new reader to the list.
    abstract member GetReader : unit -> IIndexListReader<'T>

/// Adaptive list datastructure.
and alist<'T> = AdaptiveIndexList<'T>

/// Internal implementations for alist operations.
module internal AdaptiveIndexListImplementation =

    let inline checkTag (value : 'a) (real : obj) = Unchecked.equals (value :> obj) real
    
    /// Core implementation for a dependent list.
    type AdaptiveIndexListImpl<'T>(createReader : unit -> IOpReader<IndexListDelta<'T>>) =
        let history = History(createReader, IndexList.trace)

        /// Gets a new reader to the list.
        member x.GetReader() : IIndexListReader<'T> =
            history.NewReader()

        /// Current content of the list as aval.
        member x.Content =
            history :> aval<_>

        interface AdaptiveIndexList<'T> with
            member x.IsConstant = false
            member x.GetReader() = x.GetReader()
            member x.Content = x.Content

    /// Efficient implementation for an empty adaptive list.
    type EmptyList<'T> private() =   
        static let instance = EmptyList<'T>() :> alist<_>
        let content = AVal.constant IndexList.empty
        let reader = new History.Readers.EmptyReader<IndexList<'T>, IndexListDelta<'T>>(IndexList.trace) :> IIndexListReader<'T>
        static member Instance = instance
        
        member x.Content = content
        member x.GetReader() = reader
        
        interface AdaptiveIndexList<'T> with
            member x.IsConstant = true
            member x.GetReader() = x.GetReader()
            member x.Content = x.Content

    /// Efficient implementation for a constant adaptive list.
    type ConstantList<'T>(content : Lazy<IndexList<'T>>) =
        let value = AVal.delay (fun () -> content.Value)

        member x.Content = value

        member x.GetReader() =
            new History.Readers.ConstantReader<_,_>(
                IndexList.trace,
                lazy (IndexList.computeDelta IndexList.empty content.Value),
                content
            ) :> IIndexListReader<_>

        interface AdaptiveIndexList<'T> with
            member x.IsConstant = true
            member x.GetReader() = x.GetReader()
            member x.Content = x.Content

    /// Reader for map operations.
    type MapReader<'a, 'b>(input : alist<'a>, mapping : Index -> 'a -> 'b) =
        inherit AbstractReader<IndexListDelta<'b>>(IndexListDelta.empty)

        let reader = input.GetReader()

        override x.Compute(token) =
            reader.GetChanges token |> IndexListDelta.map (fun i op ->
                match op with
                | Remove -> Remove
                | Set v -> Set (mapping i v)
            )

    /// Reader for choose operations.
    type ChooseReader<'a, 'b>(input : alist<'a>, mapping : Index -> 'a -> option<'b>) =
        inherit AbstractReader<IndexListDelta<'b>>(IndexListDelta.empty)

        let r = input.GetReader()
        let mapping = IndexCache mapping

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
    type FilterReader<'a>(input : alist<'a>, predicate : Index -> 'a -> bool) =
        inherit AbstractReader<IndexListDelta<'a>>(IndexListDelta.empty)

        let reader = input.GetReader()
        let mapping = IndexCache predicate

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

    /// Ulitity used by CollectReader.
    type MultiReader<'a>(mapping : IndexMapping<Index * Index>, list : alist<'a>, release : alist<'a> -> unit) =
        inherit AbstractReader<IndexListDelta<'a>>(IndexListDelta.empty)
            
        let targets = UncheckedHashSet.create<Index>()

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
                getReader().State.Content
                |> MapExt.mapMonotonic (fun ii v -> mapping.Invoke(oi, ii), Set v)
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
                x.Outputs.Consume() |> ignore
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
    type CollectReader<'a, 'b>(input : alist<'a>, f : Index -> 'a -> alist<'b>) =
        inherit AbstractDirtyReader<MultiReader<'b>, IndexListDelta<'b>>(IndexListDelta.monoid, checkTag "MultiReader")
            
        let mapping = IndexMapping<Index * Index>()
        let cache = UncheckedDictionary.create<Index, 'a * alist<'b>>()
        let readers = UncheckedDictionary.create<alist<'b>, MultiReader<'b>>()
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
                if Unchecked.equals oldValue v then
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
    type IndexedReader<'a>(mapping : IndexMapping<Index * Index>, index : Index, input : alist<'a>) =
        inherit AbstractReader<IndexListDelta<'a>>(IndexListDelta.empty) 

        let reader = input.GetReader()

        override x.Compute(token : AdaptiveToken) =
            reader.GetChanges(token)
            |> IndexListDelta.chooseMonotonic (fun i op ->
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
                            old.Outputs.Consume() |> ignore
                            IndexListDelta.combine remOld addNew
                        | None ->
                            addNew
                reader <- Some (v,newReader)
                deltas

    /// Reader for sortBy operations
    type SortByReader<'a, 'b when 'b : comparison>(input : alist<'a>, mapping : Index -> 'a -> 'b) =
        inherit AbstractReader<IndexListDelta<'a>>(IndexListDelta.empty)

        let reader = input.GetReader()
        let idx = IndexMapping<('b * Index)>()
        let cache = UncheckedDictionary.create<Index, 'b>()

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
    type AValReader<'s, 'a when 's :> seq<'a>>(input: aval<'s>) =
        inherit AbstractReader<IndexListDelta<'a>>(IndexListDelta.empty)

        let mutable last = IndexList.empty

        override x.Compute(token: AdaptiveToken) =
            let v = input.GetValue token |> IndexList.ofSeq
            let ops = IndexList.computeDelta last v
            last <- v
            ops

    /// Gets the current content of the alist as IndexList.
    let inline force (list : alist<'T>) = 
        AVal.force list.Content

    /// Creates a constant list using the creation function.
    let inline constant (content : unit -> IndexList<'T>) = 
        ConstantList(lazy(content())) :> alist<_> 

    /// Creates an adaptive list using the reader.
    let inline create (reader : unit -> #IOpReader<IndexListDelta<'T>>) =
        AdaptiveIndexListImpl(fun () -> reader() :> IOpReader<_>) :> alist<_>


/// Functional operators for the alist<_> type.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AList =
    open AdaptiveIndexListImplementation

    /// The empty alist.
    [<GeneralizableValue>]
    let empty<'T> : alist<'T> = 
        EmptyList<'T>.Instance

    /// A constant alist holding a single value.
    let single (value : 'T) =
        lazy (IndexList.single value) |> ConstantList :> alist<_>
        
    /// Creates an alist holding the given values.
    let ofSeq (s : seq<'T>) =
        lazy (IndexList.ofSeq s) |> ConstantList :> alist<_>
        
    /// Creates an alist holding the given values.
    let ofList (s : list<'T>) =
        lazy (IndexList.ofList s) |> ConstantList :> alist<_>
        
    /// Creates an alist holding the given values.
    let ofArray (s : 'T[]) =
        lazy (IndexList.ofArray s) |> ConstantList :> alist<_>
        
    /// Creates an alist holding the given values. `O(1)`
    let ofIndexList (elements : IndexList<'T>) =
        ConstantList(lazy elements) :> alist<_>

    /// Creates an alist using the given reader-creator.
    let ofReader (creator : unit -> #IOpReader<IndexListDelta<'T>>) =
        create creator

    /// Creates an alist from the given adaptive content
    let ofAVal (value: aval<#seq<'T>>) =
        if value.IsConstant then
            constant (fun () -> IndexList.ofSeq (AVal.force value))
        else
            create (fun () -> AValReader(value))


    /// Adaptively applies the given mapping function to all elements and returns a new alist containing the results.
    let mapi (mapping: Index -> 'T1 -> 'T2) (list : alist<'T1>) =
        if list.IsConstant then
            constant (fun () -> list |> force |> IndexList.mapi mapping)
        else
            create (fun () -> MapReader(list, mapping))

    /// Adaptively applies the given mapping function to all elements and returns a new alist containing the results.
    let map (mapping: 'T1 -> 'T2) (list : alist<'T1>) =
        if list.IsConstant then
            constant (fun () -> list |> force |> IndexList.map mapping)
        else
            // TODO: better implementation (caching possible since no Index needed)
            create (fun () -> MapReader(list, fun _ -> mapping))
  
    /// Adaptively chooses all elements returned by mapping.  
    let choosei (mapping: Index -> 'T1 -> option<'T2>) (list: alist<'T1>) =
        if list.IsConstant then
            constant (fun () -> list |> force |> IndexList.choosei mapping)
        else
            create (fun () -> ChooseReader(list, mapping))
  
    /// Adaptively chooses all elements returned by mapping.  
    let choose (mapping: 'T1 -> option<'T2>) (list: alist<'T1>) =
        if list.IsConstant then
            constant (fun () -> list |> force |> IndexList.choose mapping)
        else
            create (fun () -> ChooseReader(list, fun _ v -> mapping v))

    /// Adaptively filters the list using the given predicate.
    let filteri (predicate : Index -> 'T -> bool) (list: alist<'T>) =
        if list.IsConstant then
            constant (fun () -> list |> force |> IndexList.filteri predicate)
        else
            create (fun () -> FilterReader(list, predicate))
        
    /// Adaptively filters the list using the given predicate.
    let filter (predicate : 'T -> bool) (list: alist<'T>) =
        if list.IsConstant then
            constant (fun () -> list |> force |> IndexList.filter predicate)
        else
            create (fun () -> FilterReader(list, fun _ v -> predicate v))
        
    /// Adaptively applies the given mapping function to all elements and returns a new alist holding the concatenated results.
    let collecti (mapping: Index -> 'T1 -> alist<'T2>) (list : alist<'T1>) =
        if list.IsConstant then
            let content = force list |> IndexList.mapi mapping
            if content |> Seq.forall (fun l -> l.IsConstant) then
                constant (fun () -> content |> IndexList.collect force)
            else
                create (fun () -> ConcatReader(content))
        else
            create (fun () -> CollectReader(list, mapping))
                     
    /// Adaptively applies the given mapping function to all elements and returns a new alist holding the concatenated results.
    let collect (mapping: 'T1 -> alist<'T2>) (list : alist<'T1>) =
        // TODO: better implementation possible (caching?)
        collecti (fun _ v -> mapping v) list
         
    /// Adaptively concatenates the given lists.
    let concat (lists : #seq<alist<'T>>) =
        let lists = IndexList.ofSeq lists
        if lists.IsEmpty then 
            empty
        elif lists |> Seq.forall (fun l -> l.IsConstant) then
            constant (fun () -> lists |> IndexList.collect force)
        else
            create (fun () -> ConcatReader(lists))

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
            create (fun () -> BindReader(value, mapping))

    /// Sorts the list using the keys given by projection.
    /// Note that the sorting is stable.
    let sortByi (projection : Index -> 'T1 -> 'T2) (list : alist<'T1>) =
        if list.IsConstant then
            constant (fun () -> force list |> IndexList.sortByi projection)
        else
            create (fun () -> SortByReader(list, projection))
            
    /// Sorts the list using the keys given by projection.
    /// Note that the sorting is stable.
    let sortBy (projection : 'T1 -> 'T2) (list : alist<'T1>) =
        if list.IsConstant then
            constant (fun () -> force list |> IndexList.sortBy projection)
        else
            create (fun () -> SortByReader(list, fun _ v -> projection v))

    /// Sorts the list using the given compare function.
    /// Note that the sorting is stable.
    let sortWith (compare : 'T -> 'T -> int) (list : alist<'T>) =
        if list.IsConstant then
            constant (fun () -> force list |> IndexList.sortWith compare)
        else
            create (fun () -> SortWithReader(list, compare))

    /// Sorts the list.
    let inline sort (list: alist<'T>) = sortWith compare list

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


    /// Adaptively folds over the list using add for additions and trySubtract for removals.
    /// Note the trySubtract may return None indicating that the result needs to be recomputed.
    /// Also note that the order of elements given to add/trySubtract is undefined.
    let foldHalfGroup (add : 's -> 'a -> 's) (trySub : 's -> 'a -> Option<'s>) (zero : 's) (l : alist<'a>) =
        let r = l.GetReader()
        let mutable res = zero
        AVal.custom (fun token ->
            let old = r.State
            let ops = r.GetChanges token

            use e = (IndexListDelta.toSeq ops).GetEnumerator()
            let mutable working = true

            while working && e.MoveNext() do
                let (idx, op) = e.Current
                match op with
                | Remove ->
                    match IndexList.tryGet idx old with
                    | Some o -> 
                        match trySub res o with
                        | Some r -> res <- r
                        | None -> working <- false
                    | None -> 
                        () // strange
                | Set v ->
                    match IndexList.tryGet idx old with
                    | Some o ->
                        match trySub res o with
                        | Some r -> res <- add r v
                        | None -> working <- false
                    | None -> 
                        res <- add res v
               
            if not working then
                res <- r.State |> Seq.fold add zero

            res

        )

    /// Adaptively folds over the list using add for additions and subtract for removals.
    /// Note that the order of elements given to add/subtract is undefined.
    let foldGroup (add : 's -> 'a -> 's) (sub : 's -> 'a -> 's) (zero : 's) (s : alist<'a>) =
        foldHalfGroup add (fun a b -> Some (sub a b)) zero s

    /// Adaptively folds over the list using add for additions and recomputes the value on every removal.
    /// Note that the order of elements given to add is undefined.
    let fold (f : 's -> 'a -> 's) (seed : 's) (s : alist<'a>) = 
        foldHalfGroup f (fun _ _ -> None) seed s
        
    /// Adaptively computes the sum all entries in the list.
    let inline sum (s : alist<'a>) = foldGroup (+) (-) LanguagePrimitives.GenericZero s
    
    /// Adaptively computes the product of all entries in the list.
    let inline product (s : alist<'a>) = foldGroup (*) (/) LanguagePrimitives.GenericOne s
