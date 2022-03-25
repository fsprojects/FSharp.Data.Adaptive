module ABag

open System.Collections.Generic
open FSharp.Data.Adaptive
open FSharp.Data.Traceable

module Unique =
    let mutable private currentId = 0L

    let newId() =
        System.Threading.Interlocked.Increment(&currentId)

type IAdaptiveBag<'T> =
    /// Is the list constant?
    abstract member IsConstant : bool

    /// The current content of the list as aval.
    abstract member Content : aval<HashMap<int64, 'T>>
    
    /// Gets a new reader to the list.
    abstract member GetReader : unit -> IAdaptiveBagReader<'T>
    
    /// Gets the underlying History instance for the alist (if any)
    abstract member History : option<History<HashMap<int64, 'T>, HashMapDelta<int64, 'T>>>

and IAdaptiveBagReader<'T> = IOpReader<HashMap<int64, 'T>, HashMapDelta<int64, 'T>>

and abag<'T> = IAdaptiveBag<'T>

module ABag =

    type AdaptiveBag<'T>(getReader : unit -> IOpReader<HashMapDelta<int64, 'T>>) =
        
        let history = new History<_,_>(getReader, HashMap.trace, ignore)

        interface IAdaptiveBag<'T> with
            member x.GetReader() =
                history.NewReader()

            member x.History =
                Some history

            member x.IsConstant =
                false
            
            member x.Content =
                history :> aval<_>

    type ConstantBag<'T>(content : Lazy<HashMap<int64, 'T>>) =

        let initialDelta =
            lazy (
                HashMap.computeDelta HashMap.empty content.Value
            )

        interface IAdaptiveBag<'T> with
            member x.IsConstant = true
            member x.History = None
            member x.Content = AVal.constant content.Value
            member x.GetReader() =
                new History.Readers.ConstantReader<_,_>(
                    HashMap.trace, 
                    initialDelta,
                    content
                )
    
        new(content : seq<'T>) =
            ConstantBag<'T>(
                lazy (
                    content
                    |> Seq.map (fun e -> Unique.newId(), e)
                    |> HashMap.ofSeq
                )
            )

    type private IdCache<'a>() =
        let table = DefaultDictionary.create<'a, int64>()

        member x.Invoke(value : 'a) =
            match table.TryGetValue value with
            | (true, id) -> id
            | _ ->  
                let id = Unique.newId()
                table.[value] <- id
                id

        member x.TryRevoke(value : 'a) =
            match table.TryGetValue value with
            | (true, id) ->
                table.Remove value |> ignore
                Some id
            | _ ->
                None

        member x.Revoke(value : 'a) =
            let id = table.[value]
            table.Remove value |> ignore
            id

    let ofReader (create : unit -> #IOpReader<HashMapDelta<int64, 'T>>) =
        AdaptiveBag(fun () -> create() :> IOpReader<_>) :> abag<_>

    let private constantSeq (seq : unit -> seq<'T>) =
        ConstantBag(Seq.delay seq) :> abag<_>

    let private constantMap (seq : unit -> HashMap<int64, 'T>) =
        ConstantBag(lazy(seq())) :> abag<_>

    let ofASet (set : aset<'T>) =
        if set.IsConstant then
            constantSeq <| fun () -> set.Content |> AVal.force :> seq<_>
        else
            ofReader <| fun () ->
                let ids = IdCache<'T>()
                match set.History with
                | Some h -> 
                    h.NewReader(HashMap.trace, fun ops ->
                        let mutable delta = HashMap.empty
                        for op in ops do
                            match op with
                            | Add(_, v) -> 
                                let id = ids.Invoke v
                                delta <- HashMap.add id (Set v) delta
                            | Rem(_, v) ->
                                let id = ids.Revoke v
                                delta <- HashMap.add id Remove delta
                        HashMapDelta delta
                    ) :> IOpReader<_>
                | None ->
                    let r = set.GetReader()
                    { new AbstractReader<HashMapDelta<int64, 'T>>(HashMapDelta.empty) with
                        member x.Compute(token : AdaptiveToken) =
                            let ops = r.GetChanges token
                            let mutable delta = HashMap.empty
                            for op in ops do
                                match op with
                                | Add(_, v) -> 
                                    let id = ids.Invoke v
                                    delta <- HashMap.add id (Set v) delta
                                | Rem(_, v) ->
                                    let id = ids.Revoke v
                                    delta <- HashMap.add id Remove delta
                            HashMapDelta delta
                    } :> IOpReader<_>
        
    let ofAList (list : alist<'T>) =
        if list.IsConstant then
            constantSeq <| fun () -> list.Content |> AVal.force :> seq<_>
        else
            ofReader <| fun () ->
                let ids = IdCache<Index>()
                match list.History with
                | Some history ->
                    history.NewReader(HashMap.trace, fun ops ->
                        let mutable delta = HashMap.empty
                        for index, op in ops do
                            match op with
                            | Set v -> 
                                let id = ids.Invoke index
                                delta <- HashMap.add id (Set v) delta
                            | Remove ->
                                match ids.TryRevoke index with
                                | Some id ->
                                    delta <- HashMap.add id Remove delta
                                | _ -> ()
                        HashMapDelta delta
                    ) :> IOpReader<_>
                | None ->
                    let r = list.GetReader()
                    { new AbstractReader<HashMapDelta<int64, 'T>>(HashMapDelta.empty) with
                        member x.Compute(token : AdaptiveToken) =
                            let ops = r.GetChanges token
                            let mutable delta = HashMap.empty
                            for index, op in ops do
                                match op with
                                | Set v -> 
                                    let id = ids.Invoke index
                                    delta <- HashMap.add id (Set v) delta
                                | Remove ->
                                    match ids.TryRevoke index with
                                    | Some id ->
                                        delta <- HashMap.add id Remove delta
                                    | _ -> ()
                            HashMapDelta delta
                    } :> IOpReader<_>


    let mapAMap (mapping : 'K -> 'V -> 'T) (map : amap<'K, 'V>) =
        if map.IsConstant then
            constantSeq <| fun () -> map.Content |> AVal.force |> Seq.map (fun (k,v) -> mapping k v) :> seq<_>
        else
            let mapping = OptimizedClosures.FSharpFunc<_,_,_>.Adapt mapping
            ofReader <| fun () ->
                let cache = IdCache<'K>()
                let inline mapDelta (ops : HashMapDelta<'K, 'V>) =
                    ops
                    |> HashMapDelta.toHashMap
                    |> Seq.choose (fun (k, op) -> 
                        match op with
                        | Set v ->
                            Some (cache.Invoke k, Set (mapping.Invoke(k, v)))
                        | Remove -> 
                            match cache.TryRevoke k with
                            | Some id -> Some (id, Remove)
                            | None -> None
                    )
                    |> HashMapDelta.ofSeq

                match map.History with
                | Some history ->
                    history.NewReader(HashMap.trace, mapDelta) :> IOpReader<_>
                | None ->
                    let r = map.GetReader()
                    { new AbstractReader<HashMapDelta<int64, 'T>>(HashMapDelta.empty) with
                        member x.Compute(token : AdaptiveToken) =
                            r.GetChanges token
                            |> mapDelta
                    } :> IOpReader<_>

    let ofAMap (map : amap<'K, 'V>) =
        mapAMap (fun k v -> (k,v)) map

    let ofAMapValues (map : amap<'K, 'V>) =
        mapAMap (fun _ v -> v) map
    
    let ofAMapKeys (map : amap<'K, 'V>) =
        mapAMap (fun k _ -> k) map

    let map (mapping : 'T -> 'U) (bag : abag<'T>) =
        if bag.IsConstant then
            constantMap <| fun () -> bag.Content |> AVal.force |> HashMap.map (fun _ v -> mapping v) 
        else
            let inline mapDelta (ops : HashMapDelta<int64, 'T>) =
                ops
                |> HashMapDelta.toHashMap
                |> HashMap.map (fun k op ->
                    match op with
                    | Set v -> Set (mapping v)
                    | Remove -> Remove
                )
                |> HashMapDelta.ofHashMap

            ofReader <| fun () ->
                match bag.History with
                | Some history ->
                    history.NewReader(HashMap.trace, mapDelta) :> IOpReader<_>
                | None ->
                    let r = bag.GetReader()
                    { new AbstractReader<HashMapDelta<int64, 'U>>(HashMapDelta.empty) with
                        member x.Compute(token : AdaptiveToken) =
                            r.GetChanges token
                            |> mapDelta
                    } :> IOpReader<_>

    let choose (mapping : 'T -> option<'U>) (bag : abag<'T>) =
        if bag.IsConstant then
            constantMap <| fun () -> bag.Content |> AVal.force |> HashMap.choose (fun _ v -> mapping v) 
        else
            ofReader <| fun () ->
                let mutable existing = HashSet.empty

                let inline mapDelta (ops : HashMapDelta<int64, 'T>) =
                    ops
                    |> HashMapDelta.toHashMap
                    |> HashMap.choose (fun k op ->
                        match op with
                        | Set v -> 
                            match mapping v with
                            | Some v -> 
                                existing <- HashSet.add k existing
                                Some (Set v)
                            | None -> 
                                match HashSet.tryRemove k existing with
                                | Some n -> 
                                    existing <- n
                                    Some Remove
                                | None ->   
                                    None
                        | Remove -> 
                            match HashSet.tryRemove k existing with
                            | Some n -> 
                                existing <- n
                                Some Remove
                            | None ->   
                                None
                    )
                    |> HashMapDelta.ofHashMap

                match bag.History with
                | Some history ->
                    history.NewReader(HashMap.trace, mapDelta) :> IOpReader<_>
                | None ->
                    let r = bag.GetReader()
                    { new AbstractReader<HashMapDelta<int64, 'U>>(HashMapDelta.empty) with
                        member x.Compute(token : AdaptiveToken) =
                            r.GetChanges token
                            |> mapDelta
                    } :> IOpReader<_>
          
    let unionMany' (bags : #seq<abag<'a>>) =
        let bags = bags |> Seq.toArray
        if bags |> Array.forall (fun b -> b.IsConstant) then
            constantSeq <| fun () ->
                seq {
                    for b in bags do
                        yield! b.Content |> AVal.force |> HashMap.toValueSeq
                }
        else
            ofReader <| fun () ->
                let readers = bags |> Array.map (fun b -> b.GetReader())
                let caches = Array.init bags.Length (fun _ -> IdCache<int64>())
                { new AbstractReader<HashMapDelta<int64, 'a>>(HashMapDelta.empty) with
                    member x.Compute(token : AdaptiveToken) =
                        let mutable res = HashMap.empty

                        for bi in 0 .. bags.Length - 1 do
                            let cache = caches.[bi]
                            let ops = readers.[bi].GetChanges token
                            
                            for id, op in ops do
                                match op with
                                | Set v ->
                                    let oid = cache.Invoke id
                                    res <- HashMap.add oid (Set v) res
                                | Remove ->
                                    match cache.TryRevoke id with
                                    | Some oid ->
                                        res <- HashMap.add oid Remove res
                                    | _ ->  
                                        ()

                        HashMapDelta res
                }

    let union (a : abag<'a>) (b : abag<'a>) =
        unionMany' [a;b]

    let collect (mapping : 'a -> abag<'b>) (bag : abag<'a>) =
        ofReader <| fun () ->   
            let reader = bag.GetReader()
            let mutable readers = HashMap.empty<int64, abag<'b> * IOpReader<HashMap<int64, 'b>, HashMapDelta<int64, 'b>>>
            let dirty = ref <| System.Collections.Generic.HashSet()

            { new AbstractReader<HashMapDelta<int64, 'b>>(HashMapDelta.empty) with
                member x.InputChangedObject(_, o) = 
                    match o with
                    | :? IOpReader<HashMap<int64, 'b>, HashMapDelta<int64, 'b>> as r when not (System.Object.ReferenceEquals(r, reader)) ->
                        lock dirty (fun () -> dirty.Value.Add r |> ignore)
                    | _ ->
                        ()

                member x.Compute(token : AdaptiveToken) =
                    let dirty =
                        lock dirty (fun () ->
                            let d = dirty.Value
                            dirty.Value <- System.Collections.Generic.HashSet()
                            d
                        )
                    let mutable delta = HashMap.empty
                    for lid, op in reader.GetChanges token do
                        match op with
                        | Set nv ->
                            let newbag = mapping nv

                            let inline newReader() =
                                let newReader = newbag.GetReader()
                                let newCache = IdCache<int64>()
                                newReader.Tag <- newCache
                                dirty.Add newReader |> ignore
                                readers <- HashMap.add lid (newbag, newReader) readers

                            match HashMap.tryRemove lid readers with
                            | Some ((oldBag, oldReader), _) when oldBag = newbag ->
                                // replaced with the identical bag
                                dirty.Add oldReader |> ignore

                            | Some((_, oldReader), rest) ->
                                // replaced with a new bag =>
                                //   1. remove the old reader
                                //   2. add a new reader
                                readers <- rest
                                let oldCache = oldReader.Tag :?> IdCache<int64>
                                dirty.Remove oldReader |> ignore
                                oldReader.Outputs.Remove x |> ignore
                                for oid, _ in oldReader.State do
                                    match oldCache.TryRevoke oid with
                                    | Some id -> delta <- HashMap.add id Remove delta
                                    | None -> ()

                                newReader()
                           
                            | _ ->
                                // insert
                                newReader()

                        | Remove ->
                            match HashMap.tryRemove lid readers with
                            | Some((_, oldReader), rest) ->  
                                let oldCache = oldReader.Tag :?> IdCache<int64>
                                readers <- rest
                                dirty.Remove oldReader |> ignore
                                oldReader.Outputs.Remove x |> ignore
                                for oid, _ in oldReader.State do
                                    match oldCache.TryRevoke oid with
                                    | Some id -> delta <- HashMap.add id Remove delta
                                    | None -> ()
                            | _ ->
                                ()

                    for d in dirty do
                        let cache = d.Tag :?> IdCache<int64>
                        let ops = d.GetChanges token
                        for oid, op in ops do
                            match op with
                            | Set value ->
                                let id = cache.Invoke oid
                                delta <- HashMap.add id (Set value) delta
                            | Remove ->
                                let id = cache.Revoke oid
                                delta <- HashMap.add id Remove delta

                    dirty.Clear()

                    HashMapDelta.ofHashMap delta
            }


    let toASet (bag : abag<'a>) =
        if bag.IsConstant then
            ASet.delay (fun () -> bag.Content |> AVal.force |> HashMap.toValueSeq |> HashSet.ofSeq)
        else
            ASet.ofReader <| fun () ->
                let r = bag.GetReader()
                { new AbstractReader<HashSetDelta<'a>>(HashSetDelta.empty) with
                    member x.Compute(token : AdaptiveToken) =
                        let old = r.State
                        let ops = r.GetChanges token

                        let mutable delta = HashSetDelta.empty

                        for id, op in ops do
                            match op with
                            | Remove ->
                                match HashMap.tryFindV id old with
                                | ValueSome v ->
                                    delta <- HashSetDelta.add (Rem v) delta
                                | ValueNone ->
                                    ()
                            | Set n ->
                                match HashMap.tryFindV id old with
                                | ValueSome o ->
                                    delta <- HashSetDelta.add (Rem o) delta
                                | ValueNone ->
                                    ()
                                delta <- HashSetDelta.add (Add n) delta
                        delta

                }

    let private sortByInternal (cmp : 'b -> 'b -> int) (mapping : 'a -> 'b) (bag : abag<'a>) =
        if bag.IsConstant then
            AList.ofArray (
                bag.Content 
                |> AVal.force 
                |> HashMap.toValueArray 
                |> Array.map (fun v -> struct(mapping v, v))
                |> Array.sortWith (fun struct(a,_) struct(b,_) -> cmp a b)
                |> Array.map (fun struct(_, a) -> a)
            )
        else
            let cmp = OptimizedClosures.FSharpFunc<_,_,_>.Adapt cmp
            let cmpt = 
                OptimizedClosures.FSharpFunc<_,_,_>.Adapt (fun struct(ai, a) struct(bi, b) -> 
                    let c = cmp.Invoke(a, b)
                    if c = 0 then compare ai bi
                    else c
                )
            AList.ofReader <| fun () ->
                let r = bag.GetReader()
                let indices = CustomIndexMapping<struct(int64 * 'b)>(cmpt)
                let mutable cache = HashMap.empty<int64, 'b>
                { new AbstractReader<IndexListDelta<'a>>(IndexListDelta.empty) with
                    member x.Compute(token : AdaptiveToken) =
                        let ops = r.GetChanges token 
                        let mutable delta = IndexListDelta.empty
                        for id, op in ops do
                            match HashMap.tryRemoveV id cache with
                            | ValueSome struct(ob, rest) ->
                                cache <- rest
                                match indices.Revoke struct(id, ob) with
                                | Some oi -> delta <- IndexListDelta.add oi Remove delta
                                | None -> ()
                            | ValueNone ->
                                ()

                            match op with
                            | Remove -> ()
                            | Set n ->
                                let b = mapping n
                                cache <- HashMap.add id b cache
                                let ni = indices.Invoke struct(id, b)
                                delta <- IndexListDelta.add ni (Set n) delta
                        delta
                }

    let sortBy (mapping : 'a -> 'b) (bag : abag<'a>) =
        sortByInternal compare mapping bag

    let sortByDescending (mapping : 'a -> 'b) (bag : abag<'a>) =
        sortByInternal (fun a b -> compare b a) mapping bag

    let sortWith (cmp : 'a -> 'a -> int) (bag : abag<'a>) =
        if bag.IsConstant then
            AList.ofArray (
                bag.Content |> AVal.force |> HashMap.toValueArray |> Array.sortWith cmp
            )
        else
            let cmp = OptimizedClosures.FSharpFunc<_,_,_>.Adapt cmp
            let cmpt = 
                OptimizedClosures.FSharpFunc<_,_,_>.Adapt (fun struct(ai, a) struct(bi, b) -> 
                    let c = cmp.Invoke(a, b)
                    if c = 0 then compare ai bi
                    else c
                )

            AList.ofReader <| fun () ->
                let r = bag.GetReader()
                let indices = CustomIndexMapping<struct(int64 * 'a)>(cmpt)
                { new AbstractReader<IndexListDelta<'a>>(IndexListDelta.empty) with
                    member x.Compute(token : AdaptiveToken) =
                        let old = r.State
                        let ops = r.GetChanges token 
                        let mutable delta = IndexListDelta.empty
                        for id, op in ops do
                            match HashMap.tryFindV id old with
                            | ValueSome ov  ->
                                match indices.Revoke struct(id, ov) with
                                | Some oi -> delta <- IndexListDelta.add oi Remove delta
                                | None -> ()
                            | ValueNone ->
                                ()

                            match op with
                            | Remove -> ()
                            | Set n ->
                                let ni = indices.Invoke struct(id, n)
                                delta <- IndexListDelta.add ni (Set n) delta
                        delta
                }

    let sortWithDescending (cmp : 'a -> 'a -> int) (bag : abag<'a>) =
        bag |> sortWith (fun a b -> cmp b a)

    let sort (bag : abag<'a>) =
        bag |> sortWith compare

    let sortDescending (bag : abag<'a>) =
        bag |> sortWith (fun a b -> compare b a)



let run() =
    let a = cset [1;2;3]
    let b = clist [3;4;5]
    let c = cmap ["a", 10; "b", 20]

    
    let x = cset [1;2;3]
    let y = cset [4;5;6]

    let a = cset [0;1]

    let test = 
        a 
        |> ABag.ofASet
        |> ABag.collect (fun v -> if v % 2 = 0 then ABag.ofASet x else ABag.ofASet y)
        |> ABag.sortDescending

    let reader = test.GetReader()

    let print() =
        let ops = reader.GetChanges AdaptiveToken.Top
        printfn "  %0A" (IndexList.toList reader.State)


    print()

    transact (fun () -> a.Add 10 |> ignore)
    print()

    transact (fun () -> x.Add 5 |> ignore)
    print()

    transact (fun () -> a.Remove 10 |> ignore)
    print()

    transact (fun () -> a.Remove 0 |> ignore)
    print()

    exit 0

    let test =
        
        let ba = ABag.ofASet a
        let bb = ABag.ofAList b
        let bc = ABag.ofAMapValues c
        ABag.unionMany' [ba; bb; bc]
        |> ABag.choose (fun a -> if a > 0 then Some (a + 1) else None)

    let reader = test.GetReader()

    let print() =
        let ops = reader.GetChanges AdaptiveToken.Top
        printfn "  %0A" (HashMap.toValueList reader.State)

    printfn "initial"
    print()

    printfn "add(4)"
    transact (fun () -> a.Add 4 |> ignore)
    print()

    printfn "rem(2)"
    transact (fun () -> a.Remove 2 |> ignore)
    print()

    printfn "removeAt(2)"
    transact (fun () -> b.RemoveAt 2 |> ignore)
    print()

    printfn "\"a\" <- 100"
    transact (fun () -> c.["a"] <- 100)
    print()
