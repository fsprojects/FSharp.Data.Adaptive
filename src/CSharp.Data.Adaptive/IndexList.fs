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



