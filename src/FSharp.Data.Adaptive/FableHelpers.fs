namespace FSharp.Data.Adaptive

[<AutoOpen>]
module internal Operators =
    let inline typesize<'T> =  
        #if FABLE_COMPILER
        4
        #else
        sizeof<'T>
        #endif

    let resizeArray (r : ref<'a[]>) (l : int) = 
        let len = r.Value.Length
        if l < len then 
            r := Array.take l r.Value
        elif l > len then 
            let res = Array.zeroCreate l
            res.[0..len-1] <- r.Value
            r := res


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
            count <- count + 1
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

type CancellationTokenRegistration() =
    member x.Unregister() = true
    member x.Dispose() = ()
    interface System.IDisposable with
        member x.Dispose() = ()

[<AllowNullLiteral>]
type CancellationToken =
    static member None : CancellationToken = null

    member x.IsCancellationRequested = false
    member x.CanBeCanceled = false
    member x.ThrowIfCancellationRequested() = ()
    member x.Register(action : System.Action) = CancellationTokenRegistration()

namespace Microsoft.FSharp.Core

module OptimizedClosures =
    
    type FSharpFunc<'T1, 'T2, 'T3>(value : 'T1 -> 'T2 -> 'T3) =
        let value = System.Func<'T1, 'T2, 'T3>(value)
        member x.Invoke(a1, a2) = value.Invoke(a1, a2)
        static member Adapt(value : 'T1 -> 'T2 -> 'T3) = FSharpFunc<'T1, 'T2, 'T3>(value)
        
    type FSharpFunc<'T1, 'T2, 'T3, 'T4>(value : 'T1 -> 'T2 -> 'T3 -> 'T4) =
        let value = System.Func<'T1, 'T2, 'T3, 'T4>(value)
        member x.Invoke(a1, a2, a3) = value.Invoke(a1, a2, a3)
        static member Adapt(value : 'T1 -> 'T2 -> 'T3 -> 'T4) = FSharpFunc<'T1, 'T2, 'T3, 'T4>(value)

namespace System.Runtime.CompilerServices


[<AutoOpen>]
module private WeakTableHelpers =
    open Fable.Core
    open Fable.Core.JsInterop

    type [<AllowNullLiteral>] WeakMap<'K, 'V> =
        abstract clear: unit -> unit
        abstract delete: key: 'K -> bool
        abstract get: key: 'K -> 'V
        abstract has: key: 'K -> bool
        abstract set: key: 'K * ?value: 'V -> WeakMap<'K, 'V>

    and [<AllowNullLiteral>] WeakMapConstructor =
        [<Emit("new $0($1...)")>] abstract Create: ?iterable: seq<'K * 'V> -> WeakMap<'K, 'V>

    let [<Global>] WeakMap: WeakMapConstructor = jsNative

type ConditionalWeakTable<'K, 'V when 'K : not struct and 'V : not struct>() =
    
    let m = WeakMap.Create<'K, 'V> []

    member x.TryGetValue(key : 'K) =
        if m.has key then (true, m.get key)
        else (false, Unchecked.defaultof<_>)

    member x.Add(key : 'K, value : 'V) =
        m.set(key, value) |> ignore

    member x.Remove(key : 'K) =
        m.delete key

namespace Microsoft.FSharp.Core



[<AutoOpen>]
module ValueOptionExtensions =
    type ValueOption<'a> with
        member inline x.IsSome = 
            match x with
            | ValueSome _ -> true
            | ValueNone -> false  
            
        member inline x.IsNone = 
            match x with
            | ValueSome _ -> false
            | ValueNone -> true  
            


module ValueOption =
    [<CompiledName("Map")>]
    let inline map (mapping : 'a -> 'b) (option : ValueOption<'a>) =
        match option with
        | ValueSome a -> ValueSome (mapping a)
        | ValueNone -> ValueNone
        
    [<CompiledName("IsSome")>]
    let inline isSome (o : ValueOption<'a>) =
        match o with
        | ValueSome _ -> true
        | ValueNone -> false

    [<CompiledName("IsNone")>]
    let inline isNone (o : ValueOption<'a>) =
        match o with
        | ValueSome _ -> false
        | ValueNone -> true


#endif
