namespace FSharp.Data.Adaptive

open System
open System.Runtime.CompilerServices
open FSharp.Data.Adaptive

module private AdaptiveValueHelpers =

    let inline addCallback (v : aval<'T>) (action: 'T -> unit) =
        v.AddCallback(action)


type ConstantValue<'T>(value : 'T) =
    inherit ConstantObject()
    
    member x.Value = value
    member x.ContentType = typeof<'T>
    member x.GetValue (_token : AdaptiveToken) = value
    member x.GetValueUntyped t = x.GetValue t :> obj

    override x.GetHashCode() = 
        Unchecked.hash value

    override x.Equals o =
        match o with
        | :? ConstantValue<'T> as o -> Unchecked.equals value o.Value
        | :? aval<'T> as o when o.IsConstant -> Unchecked.equals value (AVal.force o)
        | _ -> false

    override x.ToString() =
        sprintf "%A" value

    interface AdaptiveValue with
        member x.ContentType = x.ContentType
        member x.GetValueUntyped t = x.GetValueUntyped t

    interface AdaptiveValue<'T> with
        member x.GetValue t = x.GetValue t

[<AbstractClass; Sealed; Extension>]
type AdaptiveValue private() =

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Constant(value: 'T) =
        AVal.constant value

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Delay(value: Func<'T>) =
        AVal.delay value.Invoke
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Init(value: 'T) =
        ChangeableValue value

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Custom(compute: Func<AdaptiveToken, 'T>) =
        AVal.custom compute.Invoke

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member GetValue(this: aval<'T>) =
        AVal.force this

    /// Returns a new adaptive value that adaptively applies the mapping function to the given 
    /// adaptive inputs.
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Select(this: aval<'T1>, selector: Func<'T1, 'T2>) =
        AVal.map selector.Invoke this

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Select(value1: aval<'T1>, value2: aval<'T2>, selector: Func<'T1, 'T2, 'T3>) =
        AVal.map2 (fun l r -> selector.Invoke(l, r)) value1 value2
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Select(value1: aval<'T1>, value2: aval<'T2>, value3: aval<'T3>, selector: Func<'T1, 'T2, 'T3, 'T4>) =
        AVal.map3 (fun v0 v1 v2 -> selector.Invoke(v0, v1, v2)) value1 value2 value3
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Bind(this: aval<'T1>, selector: Func<'T1, aval<'T2>>) =
        AVal.bind selector.Invoke this

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Bind(value1: aval<'T1>, value2: aval<'T2>, selector: Func<'T1, 'T2, aval<'T3>>) =
        AVal.bind2 (fun l r -> selector.Invoke(l, r)) value1 value2
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Bind(value1: aval<'T1>, value2: aval<'T2>, value3: aval<'T3>, selector: Func<'T1, 'T2, 'T3, aval<'T4>>) =
        AVal.bind3 (fun v0 v1 v2 -> selector.Invoke(v0, v1, v2)) value1 value2 value3
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member AddCallback(value: aval<'T>, action: Action<'T>) =
        AdaptiveValueHelpers.addCallback value action.Invoke