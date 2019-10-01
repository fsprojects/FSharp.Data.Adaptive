namespace FSharp.Data.Adaptive



#if FABLE_COMPILER
namespace System.Collections.Generic

type Queue<'T>() =
    let mutable store : 'T[] = Array.zeroCreate 16
    let mutable next = 0
    let mutable first = 0
    let mutable count = 0

    member x.Count = count

    member x.Enqueue(value : 'T) =
        if count < store.Length then
            store.[next] <- value
            next <- (next + 1) % store.Length
        else
            let res = Array.zeroCreate (2 * store.Length)
            let mutable si = first
            for di in 0 .. count - 1 do
                res.[di] <- store.[si]
                si <- (si + 1) % store.Length
            store <- res
            next <- count
            first <- 0
            x.Enqueue value

    member x.Dequeue() =
        if count = 0 then failwith "Queue empty"
        let v = store.[first]
        store.[first] <- Unchecked.defaultof<_>
        first <- (first + 1) % store.Length
        count <- count - 1
        v


namespace System

[<AllowNullLiteral>]
type WeakReference<'a when 'a : not struct>(value : 'a) =
    member x.TryGetTarget() =
        (true, value)

namespace System.Threading

type Monitor =
    static member inline Enter (_o : obj) = ()
    static member inline Exit (_o : obj) = ()
    static member inline IsEntered (_o : obj) = true
    static member inline TryEnter (_o : obj) = true

namespace Microsoft.FSharp.Core

module OptimizedClosures =
    type FSharpFunc<'T1, 'T2, 'T3>(value : 'T1 -> 'T2 -> 'T3) =
        member x.Invoke(a1, a2) = value a1 a2
        static member Adapt(value : 'T1 -> 'T2 -> 'T3) = FSharpFunc<'T1, 'T2, 'T3>(value)
        
    type FSharpFunc<'T1, 'T2, 'T3, 'T4>(value : 'T1 -> 'T2 -> 'T3 -> 'T4) =
        member x.Invoke(a1, a2, a3) = value a1 a2 a3
        static member Adapt(value : 'T1 -> 'T2 -> 'T3 -> 'T4) = FSharpFunc<'T1, 'T2, 'T3, 'T4>(value)

#endif
