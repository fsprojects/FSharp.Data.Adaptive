namespace FSharp.Data.Adaptive

open System
open System.Runtime.CompilerServices
open FSharp.Data.Adaptive

module private AdaptiveObjectExtensionHelpers =
    let inline addMarkingCallback (v : IAdaptiveObject) (action: unit -> unit) =
        v.AddMarkingCallback(action)

    let inline useTransaction() =
        let t = new Transaction()
        let d = Transaction.makeCurrent t

        { new IDisposable with
            member x.Dispose() =
                d.Dispose()
                t.Commit()
                t.Dispose()
        }

type HashSetBuilder<'T>() =
    let mutable array : 'T[] = Array.zeroCreate 8
    let mutable cnt = 0

    member x.Add(value: 'T) =
        if cnt >= array.Length then Array.Resize(&array, cnt * 2)
        array.[cnt] <- value
        cnt <- cnt + 1

    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = (Seq.take cnt array).GetEnumerator() :> _

    member x.ToHashSet() =
        HashSet<'T>.OfArrayRange(array, 0, cnt)

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

type IndexListBuilder<'T>() =
    let mutable array : 'T[] = Array.zeroCreate 8
    let mutable cnt = 0

    member x.Add(value: 'T) =
        if cnt >= array.Length then Array.Resize(&array, cnt * 2)
        array.[cnt] <- value
        cnt <- cnt + 1

    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = (Seq.take cnt array).GetEnumerator() :> _

    member x.ToIndexList() =
        let mutable result = IndexList.empty
        for i in 0 .. cnt - 1 do
            result <- IndexList.add array.[i] result
        result
        
[<AbstractClass; Sealed; Extension>]
type CollectionExtensions private() =
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Select(this: HashSet<'T1>, action: Func<'T1, 'T2>) =
        this |> HashSet.map action.Invoke
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member SelectNullable(this: HashSet<'T1>, action: Func<'T1, Nullable<'T2>>) =
        this |> HashSet.choose (fun v -> 
            let r = action.Invoke(v)
            if r.HasValue then Some r.Value
            else None
        )
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member SelectNullable(this: HashSet<'T1>, action: Func<'T1, 'T2>) =
        this |> HashSet.choose (fun v -> 
            let r = action.Invoke(v)
            if isNull (r :> obj) then None
            else Some r
        )
    
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Where(this: HashSet<'T1>, action: Func<'T1, bool>) =
        this |> HashSet.filter action.Invoke
        
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


[<AbstractClass; Sealed; Extension>]
type Adaptive private() =

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member AddMarkingCallback(this: IAdaptiveObject, action: Action) =
        AdaptiveObjectExtensionHelpers.addMarkingCallback this action.Invoke


    static member Transact : IDisposable =
        AdaptiveObjectExtensionHelpers.useTransaction()
