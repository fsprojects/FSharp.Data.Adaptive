namespace FSharp.Data.Adaptive


#if FABLE_COMPILER
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
