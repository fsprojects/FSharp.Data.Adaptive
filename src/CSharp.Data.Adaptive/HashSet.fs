namespace CSharp.Data.Adaptive

open System
open System.Runtime.CompilerServices
open FSharp.Data.Adaptive

[<CompiledName("FSharpHashSetBuilder")>]
type HashSetBuilder<'T>() =
    let mutable array : 'T[] = Array.zeroCreate 8
    let mutable cnt = 0

    member x.Add(value: 'T) =
        if cnt >= array.Length then Array.Resize(&array, cnt * 2)
        array.[cnt] <- value
        cnt <- cnt + 1

    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = (Seq.take cnt array).GetEnumerator() :> _

    [<CompiledName("ToFSharpHashSet")>]
    member x.ToHashSet() =
        HashSet<'T>.OfArrayRange(array, 0, cnt)

[<AbstractClass; Sealed; Extension; CompiledName("FSharpHashSet")>]
type HashSet private() =
    
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Empty<'T>() = HashSet.empty<'T>
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToArray(this: HashSet<'T>) =
        this |> HashSet.toArray
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Single<'T>(value: 'T) = HashSet.single value
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining); CompiledName("ToFSharpHashSet")>]
    static member ToHashSet(elements: seq<'T>) = HashSet.ofSeq elements
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining); CompiledName("ToFSharpHashSet")>]
    static member ToHashSet(elements: list<'T>) = HashSet.ofList elements
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining); CompiledName("ToFSharpHashSet")>]
    static member ToHashSet(elements: 'T[]) = HashSet.ofArray elements
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Map(this: HashSet<'T1>, action: Func<'T1, 'T2>) =
        this |> HashSet.map action.Invoke
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Choose(this: HashSet<'T1>, action: Func<'T1, Option<'T2>>) =
        this |> HashSet.choose action.Invoke
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Filter(this: HashSet<'T1>, action: Func<'T1, bool>) =
        this |> HashSet.filter action.Invoke
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Collect(this: HashSet<'T1>, action: Func<'T1, HashSet<'T2>>) =
        this |> HashSet.collect action.Invoke
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Alter(this: HashSet<'T>, value: 'T, action: Func<bool, bool>) =
        this |> HashSet.alter value action.Invoke
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Exists(this: HashSet<'T1>, predicate: Func<'T1, bool>) =
        this |> HashSet.exists predicate.Invoke

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Forall(this: HashSet<'T1>, predicate: Func<'T1, bool>) =
        this |> HashSet.forall predicate.Invoke

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member MapNullable(this: HashSet<'T1>, action: Func<'T1, Nullable<'T2>>) =
        this |> HashSet.choose (fun v -> 
            let r = action.Invoke(v)
            if r.HasValue then Some r.Value
            else None
        )
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member MapNullable(this: HashSet<'T1>, action: Func<'T1, 'T2>) =
        this |> HashSet.choose (fun v -> 
            let r = action.Invoke(v)
            if isNull (r :> obj) then None
            else Some r
        )
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member UnionWith(set1: HashSet<'T>, set2: HashSet<'T>) = 
        HashSet.union set1 set2
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member IntersectWith(set1: HashSet<'T>, set2: HashSet<'T>) = 
        HashSet.intersect set1 set2

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Subtract(set1: HashSet<'T>, set2: HashSet<'T>) = 
        HashSet.difference set1 set2
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ComputeDeltaTo(set1: HashSet<'T>, set2: HashSet<'T>) = 
        HashSet.computeDelta set1 set2

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ApplyDelta(set: HashSet<'T>, delta: byref<HashSetDelta<'T>>) = 
        let (a,b) = HashSet.applyDelta set delta
        delta <- b
        a
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ApplyDelta(set: HashSet<'T>, delta: HashSetDelta<'T>) = 
        let (a,b) = HashSet.applyDelta set delta
        struct(a,b)
     
[<AbstractClass; Sealed; Extension; CompiledName("FSharpHashSetDelta")>]
type HashSetDelta private() =
    
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Empty<'T>() = HashSetDelta.empty<'T>
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToArray(this: HashSetDelta<'T>) =
        this |> HashSetDelta.toArray
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Single<'T>(value: SetOperation<'T>) = HashSetDelta.single value
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining); CompiledName("ToFSharpHashSetDelta")>]
    static member ToHashSetDelta(elements: seq<SetOperation<'T>>) = HashSetDelta.ofSeq elements
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining); CompiledName("ToFSharpHashSetDelta")>]
    static member ToHashSetDelta(elements: list<SetOperation<'T>>) = HashSetDelta.ofList elements
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining); CompiledName("ToFSharpHashSetDelta")>]
    static member ToHashSetDelta(elements: SetOperation<'T>[]) = HashSetDelta.ofArray elements
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Map(this: HashSetDelta<'T1>, action: Func<SetOperation<'T1>, SetOperation<'T2>>) =
        this |> HashSetDelta.map action.Invoke
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Filter(this: HashSetDelta<'T1>, action: Func<SetOperation<'T1>, bool>) =
        this |> HashSetDelta.filter action.Invoke
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Collect(this: HashSetDelta<'T1>, action: Func<SetOperation<'T1>, HashSetDelta<'T2>>) =
        this |> HashSetDelta.collect action.Invoke
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Choose(this: HashSetDelta<'T1>, action: Func<SetOperation<'T1>, Option<SetOperation<'T2>>>) =
        this |> HashSetDelta.choose action.Invoke
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member CombineWith(set1: HashSetDelta<'T>, set2: HashSetDelta<'T>) = 
        HashSetDelta.combine set1 set2
        