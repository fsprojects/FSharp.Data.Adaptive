namespace CSharp.Data.Adaptive

open System
open System.Runtime.CompilerServices
open FSharp.Data.Adaptive
open FSharp.Data.Traceable

type AdaptiveHashSetBuilder<'T>() =
    let store = System.Collections.Generic.List<'T>()

    member x.Add(value : 'T) =
        store.Add value

    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = (store :> System.Collections.IEnumerable).GetEnumerator()

    member x.ToAdaptiveHashSet() =
        ASet.ofSeq store

module private AdaptiveSetHelpers =

    let inline addCallback (v : aset<'T>) (action: CountingHashSet<'T> -> HashSetDelta<'T> -> unit) =
        v.AddCallback(action)
        
    let inline addWeakCallback (v : aset<'T>) (action: CountingHashSet<'T> -> HashSetDelta<'T> -> unit) =
        v.AddWeakCallback(action)

[<AbstractClass; Sealed; Extension>]
type AdaptiveHashSet private() =

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Custom(compute : Func<AdaptiveToken, CountingHashSet<'T>, HashSetDelta<'T>>) =
        ASet.custom (fun t s -> compute.Invoke(t,s))
        

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Empty<'T>() = ASet.empty<'T>

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Single<'T>(item : 'T) = ASet.single item

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfArray<'T>(arr: 'T[]) = ASet.ofArray arr

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfSeq<'T>(sq: seq<'T>) = ASet.ofSeq sq

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfHashSet<'T>(set: HashSet<'T>) = ASet.ofHashSet set

    /// Creates an AdaptiveHashSet from a tree of lists
    /// NOTE: does not expect duplicates -> TODO
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfListTree(list: alist<'TNode>, getChildren : Func<'TNode, alist<'TNode>>) =
        list |> ASet.ofListTree getChildren.Invoke

    /// Creates an AdaptiveHashSet from a tree of sets
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfSetTree(set: aset<'TNode>, getChildren : Func<'TNode, aset<'TNode>>) =
        set |> ASet.ofSetTree getChildren.Invoke
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveHashSet(this: seq<'T>) = ASet.ofSeq this

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveHashSet(this: list<'T>) = ASet.ofList this

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveHashSet(this: 'T[]) = ASet.ofArray this

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveHashSet(this: HashSet<'T>) = ASet.ofHashSet this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveValue(this: aset<'T>) = ASet.toAVal this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveHashSet(this: aval<#seq<'T>>) = ASet.ofAVal this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Map(this: aset<'T1>, selector: Func<'T1, 'T2>) = ASet.map selector.Invoke this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member MapUse(this: aset<'T1>, selector: Func<'T1, 'T2>) = 
        let (d, s) = ASet.mapUse selector.Invoke this
        struct(d, s)

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Select(this: aset<'T1>, selector: Func<'T1, 'T2>) = ASet.map selector.Invoke this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member MapNullable(this: aset<'T1>, selector: Func<'T1, Nullable<'T2>>) =
        this |> ASet.choose (fun v ->
            let n = selector.Invoke(v)
            if n.HasValue then Some n.Value
            else None
        )

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member MapNullable(this: aset<'T1>, selector: Func<'T1, 'T2>) =
        this |> ASet.choose (fun v ->
            let n = selector.Invoke(v)
            if isNull n then None
            else Some n
        )

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Choose(this: aset<'T1>, selector: Func<'T1, Option<'T2>>) =
        this |> ASet.choose selector.Invoke

        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Filter(this: aset<'T1>, predicate: Func<'T1, bool>) = ASet.filter predicate.Invoke this

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Where(this: aset<'T1>, predicate: Func<'T1, bool>) = ASet.filter predicate.Invoke this


    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member UnionWith(set1: aset<'T>, set2 : aset<'T>) = ASet.union set1 set2
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Union(set: aset<aset<'T>>) = ASet.unionMany set

    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Collect(this: aset<'T1>, selector: Func<'T1, aset<'T2>>) = ASet.collect selector.Invoke this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Collect(this: aset<'T1>, selector: Func<'T1, seq<'T2>>) = ASet.collect' selector.Invoke this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member SelectMany(this: aset<'T1>, selector: Func<'T1, aset<'T2>>) = ASet.collect selector.Invoke this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member SelectMany(this: aset<'T1>, selector: Func<'T1, seq<'T2>>) = ASet.collect' selector.Invoke this

    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Bind(this: aval<'T1>, selector: Func<'T1, aset<'T2>>) = ASet.bind selector.Invoke this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Bind(a: aval<'T1>, b : aval<'T2>, selector: Func<'T1, 'T2, aset<'T3>>) = ASet.bind2 (fun a b -> selector.Invoke(a,b)) a b

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Bind(a: aval<'T1>, b : aval<'T2>, c : aval<'T3>, selector: Func<'T1, 'T2, 'T3, aset<'T4>>) = ASet.bind3 (fun a b c -> selector.Invoke(a,b,c)) a b c
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Flatten(this: aset<aval<'T>>) = ASet.flattenA this
    
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member MapAdaptive(this: aset<'T1>, selector : Func<'T1, aval<'T2>>) = ASet.mapA selector.Invoke this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member SelectAdaptive(this: aset<'T1>, selector : Func<'T1, aval<'T2>>) = ASet.mapA selector.Invoke this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member MapNullableAdaptive(this: aset<'T1>, selector : Func<'T1, aval<Nullable<'T2>>>) =
        this |> ASet.chooseA (fun v ->
            selector.Invoke(v) |> AVal.map (fun v ->
                if v.HasValue then Some v.Value
                else None
            )
        )
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member MapNullableAdaptive(this: aset<'T1>, selector : Func<'T1, aval<'T2>>) =
        this |> ASet.chooseA (fun v ->
            selector.Invoke(v) |> AVal.map (fun v ->
                if isNull v then None
                else Some v
            )
        )

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ChooseAdaptive(this: aset<'T1>, selector: Func<'T1, aval<Option<'T2>>>) =
        this |> ASet.chooseA selector.Invoke

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member FilterAdaptive(this: aset<'T1>, predicate : Func<'T1, aval<bool>>) = ASet.filterA predicate.Invoke this

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member WhereAdaptive(this: aset<'T1>, predicate : Func<'T1, aval<bool>>) = ASet.filterA predicate.Invoke this
    
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfReader(creator: Func<IOpReader<HashSetDelta<'T>>>) = ASet.ofReader creator.Invoke
    
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Delay(creator: Func<HashSet<'T>>) = ASet.delay creator.Invoke
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member GetValue(set: aset<'T>) = ASet.force set
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member IsEmpty(set: aset<'T>) = ASet.isEmpty set
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Count(set: aset<'T>) = ASet.count set
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member AddCallback(set: aset<'T>, action: Action<CountingHashSet<'T>, HashSetDelta<'T>>) =
        AdaptiveSetHelpers.addCallback set (fun s d -> action.Invoke(s,d))
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member AddWeakCallback(set: aset<'T>, action: Action<CountingHashSet<'T>, HashSetDelta<'T>>) =
        AdaptiveSetHelpers.addWeakCallback set (fun s d -> action.Invoke(s,d))

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Forall(this: aset<'T>, predicate: Func<'T, bool>) =
        this |> ASet.forall predicate.Invoke

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Exists(this: aset<'T>, predicate: Func<'T, bool>) =
        this |> ASet.exists predicate.Invoke
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ForallAdaptive(this: aset<'T>, predicate: Func<'T, aval<bool>>) =
        this |> ASet.forallA predicate.Invoke

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ExistsAdaptive(this: aset<'T>, predicate: Func<'T, aval<bool>>) =
        this |> ASet.existsA predicate.Invoke
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member CountBy(this: aset<'T>, predicate: Func<'T, bool>) =
        this |> ASet.countBy predicate.Invoke

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member CountByAdaptive(this: aset<'T>, predicate: Func<'T, aval<bool>>) =
        this |> ASet.countByA predicate.Invoke
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Reduce(this: aset<'T>, reduction: AdaptiveReduction<'T, 'S, 'V>) =
        this |> ASet.reduce reduction

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ReduceBy(this: aset<'T1>, reduction: AdaptiveReduction<'T2, 'S, 'V>, mapping : Func<'T1, 'T2>) =
        this |> ASet.reduceBy reduction mapping.Invoke

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ReduceByAdaptive(this: aset<'T1>, reduction: AdaptiveReduction<'T2, 'S, 'V>, mapping : Func<'T1, aval<'T2>>) =
        this |> ASet.reduceByA reduction mapping.Invoke

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Fold<'T1, 'T2>(this: aset<'T1>, add: Func<'T2, 'T1, 'T2>, zero: 'T2) =
        let addFun = fun s -> fun a -> add.Invoke(s, a)
        this |> ASet.reduce (AdaptiveReduction.fold zero addFun)

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member FoldGroup<'T1, 'T2>(this: aset<'T1>, add: Func<'T2, 'T1, 'T2>, sub: Func<'T2, 'T1, 'T2>, zero: 'T2) =
        let addFun = fun s -> fun a -> add.Invoke(s, a)
        let subFun = fun s -> fun a -> sub.Invoke(s, a)
        this |> ASet.reduce (AdaptiveReduction.group zero addFun subFun)

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member FoldHalfGroup<'T1, 'T2>(this: aset<'T1>, add: Func<'T2, 'T1, 'T2>, sub: Func<'T2, 'T1, ValueOption<'T2>>, zero: 'T2) =
        let addFun = fun s -> fun a -> add.Invoke(s, a)
        let subFun = fun s -> fun a -> sub.Invoke(s, a)
        this |> ASet.reduce (AdaptiveReduction.halfGroup zero addFun subFun)
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member SortBy(this: aset<'T>, projection: Func<'T, 'C>) =
        this |> ASet.sortBy projection.Invoke

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OrderBy(this: aset<'T>, projection: Func<'T, 'C>) =
        this |> ASet.sortBy projection.Invoke

    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member SortWith(this: aset<'T>, cmp : Func<'T, 'T, int>) =
        this |> ASet.sortWith (fun a b -> cmp.Invoke(a,b))

    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member SortWith(this: aset<'T>, cmp : System.Collections.Generic.IComparer<'T>) =
        this |> ASet.sortWith (fun a b -> cmp.Compare(a,b))

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveList(this: aset<'T>) =
        this |> ASet.toAList
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member MapToMap(this: aset<'T>, getValue: Func<'T, 'Value>) =
        this |> ASet.mapToAMap getValue.Invoke
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member GroupBy(this: aset<'T>, getKey: Func<'T, 'G>) =
        this |> ASet.groupBy getKey.Invoke
                
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveHashMap(this: aset<'K * 'V>) =
        AMap.ofASet this

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveHashMapIgnoreDuplicates(this: aset<'K * 'V>) =
        AMap.ofASetIgnoreDuplicates this

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveHashMapIgnoreDuplicates(this: aset<'TValue>, getKey : Func<'TValue, 'TKey>) =
        this |> AMap.ofASetMappedIgnoreDuplicates getKey.Invoke

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToArray(this: aset<'T>) =
        this |> ASet.force |> HashSet.toArray

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToSeq(this: aset<'T>) =
        this |> ASet.force :> seq<'T>


