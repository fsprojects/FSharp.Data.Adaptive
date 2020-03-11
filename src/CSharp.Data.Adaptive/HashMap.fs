namespace CSharp.Data.Adaptive

open System
open System.Runtime.CompilerServices
open FSharp.Data.Adaptive

type HashMapBuilder<'Key, 'Value>() =
    let mutable array : struct('Key * 'Value)[] = Array.zeroCreate 8
    let mutable cnt = 0

    member x.Add(key: 'Key, value: 'Value) =
        if cnt >= array.Length then Array.Resize(&array, cnt * 2)
        array.[cnt] <- struct(key, value)
        cnt <- cnt + 1

    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = (Seq.take cnt array).GetEnumerator() :> _

    member x.ToHashMap() =
        HashMap<'Key, 'Value>.OfArrayRangeV(array, 0, cnt)
   
[<AbstractClass; Sealed; Extension>]
type HashMap private() =
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Empty<'K, 'V>() = HashMap.empty<'K, 'V>

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Single<'K, 'V>(k : 'K, v : 'V) = HashMap.single k v

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToHashMap(this: seq<'K * 'V>) = HashMap.ofSeq this

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToHashMap(this: list<'K * 'V>) = HashMap.ofList this

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToHashMap(this: ('K * 'V)[]) = HashMap.ofArray this

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToHashMap(this: seq<struct ('K * 'V)>) = HashMap.OfSeqV this

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToHashMap(this: list<struct ('K * 'V)>) = HashMap.OfSeqV this

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ToHashMap(this: struct('K * 'V)[]) = HashMap.OfSeqV this
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Map(this: HashMap<'Key, 'Value>, mapping: Func<'Key, 'Value, 'T>) =
        this |> HashMap.map (fun k v -> mapping.Invoke(k,v))

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Choose(this: HashMap<'Key, 'Value>, mapping: Func<'Key, 'Value, Option<'T>>) =
        this |> HashMap.choose (fun k v -> mapping.Invoke(k,v))

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Filter(this: HashMap<'Key, 'Value>, predicate: Func<'Key, 'Value, bool>) =
        this |> HashMap.filter (fun k v -> predicate.Invoke(k,v))
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Exists(this: HashMap<'Key, 'Value>, predicate: Func<'Key, 'Value, bool>) =
        this |> HashMap.exists (fun k v -> predicate.Invoke(k,v))
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Forall(this: HashMap<'Key, 'Value>, predicate: Func<'Key, 'Value, bool>) =
        this |> HashMap.forall (fun k v -> predicate.Invoke(k,v))
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Choose2(this: HashMap<'Key, 'V1>, other : HashMap<'Key, 'V2>, mapping: Func<'Key, Option<'V1>, Option<'V2>, Option<'V3>>) =
        (this, other) ||> HashMap.choose2 (fun k l r -> mapping.Invoke(k, l, r))
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member UnionWith(this: HashMap<'Key, 'V>, other : HashMap<'Key, 'V>) =
        (this, other) ||> HashMap.union
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member UnionWith(this: HashMap<'K, 'V>, other : HashMap<'K, 'V>, resolve: Func<'K, 'V, 'V, 'V>) =
        (this, other) ||> HashMap.unionWith (fun k l r -> resolve.Invoke(k, l, r))

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Map2(this: HashMap<'Key, 'V1>, other : HashMap<'Key, 'V2>, mapping: Func<'Key, Option<'V1>, Option<'V2>, 'V3>) =
        (this, other) ||> HashMap.map2 (fun k l r -> mapping.Invoke(k, l, r))

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Alter<'Key, 'Value>(this: HashMap<'Key, 'Value>, key : 'Key, update: Func<Option<'Value>, Option<'Value>>) =
        this |> HashMap.alter key update.Invoke
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Alter<'Key, 'Value when 'Value : not struct>(this: HashMap<'Key, 'Value>, key : 'Key, update: Func<'Value, 'Value>) =
        this |> HashMap.alter key (fun o ->
            match o with
            | Some o ->
                let n = update.Invoke o
                if isNull (n :> obj) then None
                else Some n
            | None ->
                let n = update.Invoke Unchecked.defaultof<'Value>
                if isNull (n :> obj) then None
                else Some n
        )
             
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Alter<'Key, 'Value when 'Value : struct and 'Value : (new : unit -> 'Value) and 'Value :> ValueType>(this: HashMap<'Key, 'Value>, key : 'Key, update: Func<Nullable<'Value>, Nullable<'Value>>) =
        this |> HashMap.alter key (fun o ->
            match o with
            | Some o ->
                let n = update.Invoke (Nullable o)
                if n.HasValue then Some n.Value
                else None
            | None ->
                let n = update.Invoke Unchecked.defaultof<_>
                if n.HasValue then Some n.Value
                else None
        )
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ApplyDelta(this: HashMap<'K, 'V>, delta : byref<HashMapDelta<'K, 'V>>) =
        let (s, d) = HashMap.applyDelta this delta
        delta <- d
        s  
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ApplyDelta(this: HashMap<'K, 'V>, delta : HashMapDelta<'K, 'V>) =
        let (s, d) = HashMap.applyDelta this delta
        struct(s, d)

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ComputeDeltaTo(this: HashMap<'K, 'V>, dst : HashMap<'K, 'V>) =
        HashMap.computeDelta this dst
