namespace CSharp.Data.Adaptive

open System
open System.Runtime.CompilerServices
open FSharp.Data.Adaptive
open FSharp.Data.Traceable

type AdaptiveHashMapBuilder<'Key, 'Value>() =
    let store = System.Collections.Generic.List<struct ('Key * 'Value)>()

    member x.Add(key : 'Key, value : 'Value) =
        store.Add (struct(key, value))

    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = (store :> System.Collections.IEnumerable).GetEnumerator()

    member x.ToAdaptiveHashSet() =
        AMap.ofHashMap (HashMap.OfSeq store)

module private AdaptiveMapHelpers =

    let inline addCallback (v : amap<'K, 'V>) (action: HashMap<'K, 'V> -> HashMapDelta<'K, 'V> -> unit) =
        v.AddCallback(action)
        
    let inline addWeakCallback (v : amap<'K, 'V>) (action: HashMap<'K, 'V> -> HashMapDelta<'K, 'V> -> unit) =
        v.AddWeakCallback(action)

[<AbstractClass; Sealed; Extension>]
type AdaptiveHashMap private() =

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Custom(compute : Func<AdaptiveToken, HashMap<'K, 'V>, HashMapDelta<'K, 'V>>) =
        AMap.custom (fun t s -> compute.Invoke(t,s))
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Empty<'K, 'V>() = AMap.empty<'K, 'V>

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Single<'K, 'V>(k : 'K, v : 'V) = AMap.single k v

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfArray<'K, 'V>(arr: ('K * 'V)[]) = AMap.ofArray arr

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfSeq<'K, 'V>(sq: seq<'K * 'V>) = AMap.ofSeq sq
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveHashMap(this: seq<'K * 'V>) = AMap.ofSeq this

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveHashMap(this: list<'K * 'V>) = AMap.ofList this

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveHashMap(this: ('K * 'V)[]) = AMap.ofArray this

    
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveHashMap(this: seq<struct ('K * 'V)>) = AMap.ofHashMap (HashMap.ofSeqV this)

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveHashMap(this: list<struct ('K * 'V)>) = AMap.ofHashMap (HashMap.ofListV this)

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveHashMap(this: struct('K * 'V)[]) = AMap.ofHashMap (HashMap.ofArrayV this)


    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveHashMap(this: HashMap<'K, 'V>) = AMap.ofHashMap this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveValue(this: amap<'K, 'V>) = AMap.toAVal this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveHashMap(this: aval<#seq<'K * 'V>>) = AMap.ofAVal this


    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Map(this: amap<'K, 'T1>, selector: Func<'K, 'T1, 'T2>) = AMap.map (fun k v -> selector.Invoke(k,v)) this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member MapUse(this: amap<'K, 'T1>, selector: Func<'K, 'T1, 'T2>) = 
        let (d, s) = AMap.mapUse (fun k v -> selector.Invoke(k,v)) this
        struct(d, s)

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Select(this: amap<'K, 'T1>, selector: Func<'K, 'T1, 'T2>) = AMap.map (fun k v -> selector.Invoke(k,v)) this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member MapNullable(this: amap<'K, 'T1>, selector: Func<'K, 'T1, Nullable<'T2>>) =
        this |> AMap.choose (fun k v ->
            let n = selector.Invoke(k, v)
            if n.HasValue then Some n.Value
            else None
        )

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member MapNullable(this: amap<'K, 'T1>, selector: Func<'K, 'T1, 'T2>) =
        this |> AMap.choose (fun k v ->
            let n = selector.Invoke(k, v)
            if isNull n then None
            else Some n
        )

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Choose(this: amap<'K, 'T1>, selector: Func<'K, 'T1, Option<'T2>>) =
        this |> AMap.choose (fun k v -> selector.Invoke(k,v))

        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Filter(this: amap<'K, 'T1>, predicate: Func<'K, 'T1, bool>) = 
        this |> AMap.filter (fun k v -> predicate.Invoke(k,v))
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Where(this: amap<'K, 'T1>, predicate: Func<'K, 'T1, bool>) = 
        this |> AMap.filter (fun k v -> predicate.Invoke(k,v))
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member UnionWith(map1: amap<'K, 'T>, map2: amap<'K, 'T>) = 
        AMap.union map1 map2

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member UnionWith(map1: amap<'K, 'T>, map2: amap<'K, 'T>, resolve: Func<'K, 'T, 'T, 'T>) = 
        AMap.unionWith (fun k l r -> resolve.Invoke(k,l,r)) map1 map2
        
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Bind(this: aval<'T1>, selector: Func<'T1, amap<'K, 'T2>>) = 
        AMap.bind selector.Invoke this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Bind(a: aval<'T1>, b : aval<'T2>, selector: Func<'T1, 'T2, amap<'K, 'T3>>) = AMap.bind2 (fun a b -> selector.Invoke(a,b)) a b

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Bind(a: aval<'T1>, b : aval<'T2>, c : aval<'T3>, selector: Func<'T1, 'T2, 'T3, amap<'K, 'T4>>) = AMap.bind3 (fun a b c -> selector.Invoke(a,b,c)) a b c
    
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member MapAdaptive(this: amap<'K, 'T1>, selector : Func<'K, 'T1, aval<'T2>>) = 
        AMap.mapA (fun k v -> selector.Invoke(k,v)) this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member SelectAdaptive(this: amap<'K, 'T1>, selector : Func<'K, 'T1, aval<'T2>>) = 
        AMap.mapA (fun k v -> selector.Invoke(k,v)) this
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member MapNullableAdaptive(this: amap<'K, 'T1>, selector : Func<'K, 'T1, aval<Nullable<'T2>>>) =
        this |> AMap.chooseA (fun k v ->
            selector.Invoke(k, v) |> AVal.map (fun v ->
                if v.HasValue then Some v.Value
                else None
            )
        )
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member MapNullableAdaptive(this: amap<'K, 'T1>, selector : Func<'K, 'T1, aval<'T2>>) =
        this |> AMap.chooseA (fun k v ->
            selector.Invoke(k, v) |> AVal.map (fun v ->
                if isNull v then None
                else Some v
            )
        )

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ChooseAdaptive(this: amap<'K, 'T1>, selector: Func<'K, 'T1, aval<Option<'T2>>>) =
        this |> AMap.chooseA (fun k v -> selector.Invoke(k,v))
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member FilterAdaptive(this: amap<'K, 'T1>, predicate : Func<'K, 'T1, aval<bool>>) = 
        AMap.filterA (fun k v -> predicate.Invoke(k,v)) this
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member WhereAdaptive(this: amap<'K, 'T1>, predicate : Func<'K, 'T1, aval<bool>>) = 
        AMap.filterA (fun k v -> predicate.Invoke(k,v)) this
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfReader(creator: Func<IOpReader<HashMapDelta<'K, 'V>>>) = AMap.ofReader creator.Invoke
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member GetValue(this: amap<'K, 'V>) = AMap.force this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member IsEmpty(this: amap<'K, 'V>) = AMap.isEmpty this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Count(set: amap<'K, 'V>) = AMap.count set
    
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member AddCallback(set: amap<'K, 'V>, action: Action<HashMap<'K, 'V>, HashMapDelta<'K, 'V>>) =
        AdaptiveMapHelpers.addCallback set (fun s d -> action.Invoke(s,d))
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member AddWeakCallback(set: amap<'K, 'V>, action: Action<HashMap<'K, 'V>, HashMapDelta<'K, 'V>>) =
        AdaptiveMapHelpers.addWeakCallback set (fun s d -> action.Invoke(s,d))

    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Forall(this: amap<'K, 'V>, predicate: Func<'K, 'V, bool>) =
        this |> AMap.forall (fun k v -> predicate.Invoke(k,v))
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Exists(this: amap<'K, 'V>, predicate: Func<'K, 'V, bool>) =
        this |> AMap.exists (fun k v -> predicate.Invoke(k,v))
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ForallAdaptive(this: amap<'K, 'V>, predicate: Func<'K, 'V, aval<bool>>) =
        this |> AMap.forallA (fun k v -> predicate.Invoke(k,v))
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ExistsAdaptive(this: amap<'K, 'V>, predicate: Func<'K, 'V, aval<bool>>) =
        this |> AMap.existsA (fun k v -> predicate.Invoke(k,v))
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member CountBy(this: amap<'K, 'V>, predicate: Func<'K, 'V, bool>) =
        this |> AMap.countBy (fun k v -> predicate.Invoke(k,v))

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member CountByAdaptive(this: amap<'K, 'V>, predicate: Func<'K, 'V, aval<bool>>) =
        this |> AMap.countByA (fun k v -> predicate.Invoke(k,v))
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Reduce(this: amap<'K, 'V>, reduction: AdaptiveReduction<'V, 'S, 'R>) =
        this |> AMap.reduce reduction
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ReduceBy(this: amap<'K, 'T1>, reduction: AdaptiveReduction<'T2, 'S, 'V>, mapping : Func<'K, 'T1, 'T2>) =
        this |> AMap.reduceBy reduction (fun k v -> mapping.Invoke(k,v))
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ReduceByAdaptive(this: amap<'K, 'T1>, reduction: AdaptiveReduction<'T2, 'S, 'V>, mapping : Func<'K, 'T1, aval<'T2>>) =
        this |> AMap.reduceByA reduction (fun k v -> mapping.Invoke(k,v))
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveIndexList(this: amap<Index, 'T>) =
        this |> AList.ofAMap

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveHashSet(this: amap<'K, 'V>) =
        this |> AMap.toASet 
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member TryFind(this: amap<'K, 'V>, key: 'K) =
        this |> AMap.tryFind key
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Keys(this: amap<'K, 'V>) =
        this |> AMap.keys

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Values(this: amap<'K, 'V>) =
        this |> AMap.toASetValues