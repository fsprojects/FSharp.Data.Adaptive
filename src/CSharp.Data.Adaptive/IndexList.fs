namespace CSharp.Data.Adaptive

open System
open System.Runtime.CompilerServices
open FSharp.Data.Adaptive

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
type IndexList private() =
    
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Empty<'T>() = IndexList.empty<'T>

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfSeq<'T>(seq : seq<'T>) = IndexList.ofSeq seq

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfList<'T>(list : list<'T>) = IndexList.ofList list

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfArray<'T>(arr : 'T[]) = IndexList.ofArray arr

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ForEach<'T>(this : IndexList<'T>, action : Action<'T>) = 
        this |> IndexList.iter action.Invoke

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Fold<'T1, 'T2>(this : IndexList<'T1>, seed : 'T2, fold : Func<'T2, 'T1, 'T2>) = 
        this |> IndexList.fold (fun a b -> fold.Invoke(a,b)) seed 

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Map(this: IndexList<'T1>, map: Func<'T1, 'T2>) =
        this |> IndexList.map map.Invoke



