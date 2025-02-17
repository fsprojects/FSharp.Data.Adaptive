namespace CSharp.Data.Adaptive

open System
open System.Runtime.CompilerServices
open FSharp.Data.Adaptive
open FSharp.Data.Traceable

type AdaptiveIndexListBuilder<'T>() =
    let store = System.Collections.Generic.List<'T>()

    member x.Add(value : 'T) =
        store.Add value

    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = (store :> System.Collections.IEnumerable).GetEnumerator()

    member x.ToAdaptiveHashSet() =
        AList.ofSeq store

module private AdaptiveListHelpers =

    let inline addCallback (v : alist<'T>) (action: IndexList<'T> -> IndexListDelta<'T> -> unit) =
        v.AddCallback(action)
    
    let inline addWeakCallback (v : alist<'T>) (action: IndexList<'T> -> IndexListDelta<'T> -> unit) =
        v.AddWeakCallback(action)

[<AbstractClass; Sealed; Extension>]
type AdaptiveIndexList private() =

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Custom(compute : Func<AdaptiveToken, IndexList<'T>, IndexListDelta<'T>>) =
        AList.custom (fun t s -> compute.Invoke(t,s))
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Empty<'T>() = AList.empty<'T>

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Single<'T>(item : 'T) = AList.single item

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfArray<'T>(arr: 'T[]) = AList.ofArray arr

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfSeq<'T>(sq: seq<'T>) = AList.ofSeq sq
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveIndexList(this: seq<'T>) = AList.ofSeq this

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveIndexList(this: list<'T>) = AList.ofList this

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveIndexList(this: 'T[]) = AList.ofArray this

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveIndexList(this: IndexList<'T>) = AList.ofIndexList this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveValue(this: alist<'T>) = AList.toAVal this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveIndexList(this: aval<#seq<'T>>) = AList.ofAVal this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Map(this: alist<'T1>, selector: Func<'T1, 'T2>) = AList.map selector.Invoke this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member MapUse(this: alist<'T1>, selector: Func<'T1, 'T2>) = 
        let (d, s) = AList.mapUse selector.Invoke this
        struct(d, s)

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Select(this: alist<'T1>, selector: Func<'T1, 'T2>) = AList.map selector.Invoke this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member MapNullable(this: alist<'T1>, selector: Func<'T1, Nullable<'T2>>) =
        this |> AList.choose (fun v ->
            let n = selector.Invoke(v)
            if n.HasValue then Some n.Value
            else None
        )

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member MapNullable(this: alist<'T1>, selector: Func<'T1, 'T2>) =
        this |> AList.choose (fun v ->
            let n = selector.Invoke(v)
            if isNull n then None
            else Some n
        )

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Choose(this: alist<'T1>, selector: Func<'T1, Option<'T2>>) =
        this |> AList.choose selector.Invoke

        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Filter(this: alist<'T1>, predicate: Func<'T1, bool>) = AList.filter predicate.Invoke this

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Where(this: alist<'T1>, predicate: Func<'T1, bool>) = AList.filter predicate.Invoke this


    
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Append(list1: alist<'T>, list2 : alist<'T>) = AList.append list1 list2
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Concat(set: seq<alist<'T>>) = AList.concat set

    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Collect(this: alist<'T1>, selector: Func<'T1, alist<'T2>>) = AList.collect selector.Invoke this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Collect(this: alist<'T1>, selector: Func<'T1, seq<'T2>>) = AList.collect' selector.Invoke this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member SelectMany(this: alist<'T1>, selector: Func<'T1, alist<'T2>>) = AList.collect selector.Invoke this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member SelectMany(this: alist<'T1>, selector: Func<'T1, seq<'T2>>) = AList.collect' selector.Invoke this

    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Bind(this: aval<'T1>, selector: Func<'T1, alist<'T2>>) = AList.bind selector.Invoke this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Bind(a: aval<'T1>, b : aval<'T2>, selector: Func<'T1, 'T2, alist<'T3>>) = AList.bind2 (fun a b -> selector.Invoke(a,b)) a b

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Bind(a: aval<'T1>, b : aval<'T2>, c : aval<'T3>, selector: Func<'T1, 'T2, 'T3, alist<'T4>>) = AList.bind3 (fun a b c -> selector.Invoke(a,b,c)) a b c
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member MapAdaptive(this: alist<'T1>, selector : Func<'T1, aval<'T2>>) = AList.mapA selector.Invoke this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member SelectAdaptive(this: alist<'T1>, selector : Func<'T1, aval<'T2>>) = AList.mapA selector.Invoke this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member MapNullableAdaptive(this: alist<'T1>, selector : Func<'T1, aval<Nullable<'T2>>>) =
        this |> AList.chooseA (fun v ->
            selector.Invoke(v) |> AVal.map (fun v ->
                if v.HasValue then Some v.Value
                else None
            )
        )
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member MapNullableAdaptive(this: alist<'T1>, selector : Func<'T1, aval<'T2>>) =
        this |> AList.chooseA (fun v ->
            selector.Invoke(v) |> AVal.map (fun v ->
                if isNull v then None
                else Some v
            )
        )

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ChooseAdaptive(this: alist<'T1>, selector: Func<'T1, aval<Option<'T2>>>) =
        this |> AList.chooseA selector.Invoke
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member FilterAdaptive(this: alist<'T1>, predicate : Func<'T1, aval<bool>>) = AList.filterA predicate.Invoke this

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member WhereAdaptive(this: alist<'T1>, predicate : Func<'T1, aval<bool>>) = AList.filterA predicate.Invoke this
    
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfReader(creator: Func<IOpReader<IndexListDelta<'T>>>) = AList.ofReader creator.Invoke
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member GetValue(this: alist<'T>) = AList.force this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member IsEmpty(this: alist<'T>) = AList.isEmpty this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Count(set: alist<'T>) = AList.count set

    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member AddCallback(set: alist<'T>, action: Action<IndexList<'T>, IndexListDelta<'T>>) =
        AdaptiveListHelpers.addCallback set (fun s d -> action.Invoke(s,d))
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member AddWeakCallback(set: alist<'T>, action: Action<IndexList<'T>, IndexListDelta<'T>>) =
        AdaptiveListHelpers.addWeakCallback set (fun s d -> action.Invoke(s,d))

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Forall(this: alist<'T>, predicate: Func<'T, bool>) =
        this |> AList.forall predicate.Invoke

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Exists(this: alist<'T>, predicate: Func<'T, bool>) =
        this |> AList.exists predicate.Invoke
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ForallAdaptive(this: alist<'T>, predicate: Func<'T, aval<bool>>) =
        this |> AList.forallA predicate.Invoke

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ExistsAdaptive(this: alist<'T>, predicate: Func<'T, aval<bool>>) =
        this |> AList.existsA predicate.Invoke
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member CountBy(this: alist<'T>, predicate: Func<'T, bool>) =
        this |> AList.countBy predicate.Invoke

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member CountByAdaptive(this: alist<'T>, predicate: Func<'T, aval<bool>>) =
        this |> AList.countByA predicate.Invoke
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Reduce(this: alist<'T>, reduction: AdaptiveReduction<'T, 'S, 'V>) =
        this |> AList.reduce reduction

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ReduceBy(this: alist<'T1>, reduction: AdaptiveReduction<'T2, 'S, 'V>, mapping : Func<'T1, 'T2>) =
        this |> AList.reduceBy reduction (fun _ v -> mapping.Invoke v)

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ReduceByAdaptive(this: alist<'T1>, reduction: AdaptiveReduction<'T2, 'S, 'V>, mapping : Func<'T1, aval<'T2>>) =
        this |> AList.reduceByA reduction (fun _ v -> mapping.Invoke v)
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Fold<'T1, 'T2>(this: alist<'T1>, add: Func<'T2, 'T1, 'T2>, zero: 'T2) =
        let addFun = fun s -> fun a -> add.Invoke(s, a)
        this |> AList.reduce (AdaptiveReduction.fold zero addFun)

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member FoldGroup<'T1, 'T2>(this: alist<'T1>, add: Func<'T2, 'T1, 'T2>, sub: Func<'T2, 'T1, 'T2>, zero: 'T2) =
        let addFun = fun s -> fun a -> add.Invoke(s, a)
        let subFun = fun s -> fun a -> sub.Invoke(s, a)
        this |> AList.reduce (AdaptiveReduction.group zero addFun subFun)

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member FoldHalfGroup<'T1, 'T2>(this: alist<'T1>, add: Func<'T2, 'T1, 'T2>, sub: Func<'T2, 'T1, ValueOption<'T2>>, zero: 'T2) =
        let addFun = fun s -> fun a -> add.Invoke(s, a)
        let subFun = fun s -> fun a -> sub.Invoke(s, a)
        this |> AList.reduce (AdaptiveReduction.halfGroup zero addFun subFun)

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Indexed(this: alist<'T>) =
        this |> AList.indexed

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member SortBy(this: alist<'T>, projection: Func<'T, 'C>) =
        this |> AList.sortBy projection.Invoke

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OrderBy(this: alist<'T>, projection: Func<'T, 'C>) =
        this |> AList.sortBy projection.Invoke

    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member SortWith(this: alist<'T>, cmp : Func<'T, 'T, int>) =
        this |> AList.sortWith (fun a b -> cmp.Invoke(a,b))

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member SortWith(this: alist<'T>, cmp : System.Collections.Generic.IComparer<'T>) =
        this |> AList.sortWith (fun a b -> cmp.Compare(a,b))
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveHashSet(this: alist<'T>) =
        this |> AList.toASet

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveHashMap(this: alist<'T>) =
        this |> AList.toAMap

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveHashSet(this: clist<'T>) =
        this |> AList.toASet

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveHashMap(this: clist<'T>) =
        this |> AList.toAMap

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member GroupBy(this: alist<'T>, mapping: Func<'T, 'G>) =
        this |> AList.groupBy mapping.Invoke

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Reverse(this: alist<'T>) =
        this |> AList.rev
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member TryAt(this: alist<'T>, index: int) =
        this |> AList.tryAt index

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member TryAt(this: alist<'T>, index: Index) =
        this |> AList.tryGet index

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToAdaptiveHashSet(this: alist<'T1>, mapping : Func<'T1, 'T2>) =
        this |> AList.mapToASet mapping.Invoke

