namespace FSharp.Data.Adaptive

open System
open System.Runtime.CompilerServices
open FSharp.Data.Adaptive

[<AbstractClass; Sealed; Extension>]
type AdaptiveHashSet private() =

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Empty<'T>() = ASet.empty<'T>
    