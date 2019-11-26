namespace FSharp.Data.Adaptive

open System

type AdaptiveValue =
    inherit IAdaptiveObject
    abstract member GetValueUntyped: AdaptiveToken -> obj
    abstract member ContentType : Type

type AdaptiveValue<'T> =
    inherit AdaptiveValue
    abstract member GetValue: AdaptiveToken -> 'T

and aval<'T> = AdaptiveValue<'T>

[<Sealed; StructuredFormatDisplay("{AsString}")>]
type ChangeableValue<'T>(value : 'T) =
    inherit AdaptiveObject()
    let mutable value = value

    member x.Value
        with get() = value
        and set v =
            if not (Unchecked.equals value v) then
                value <- v
                x.MarkOutdated()
                
    member x.GetValue (token: AdaptiveToken) =
        x.EvaluateAlways token (fun _ ->
            value
        )

    member x.UpdateTo(newValue: 'T) =
        if not (Unchecked.equals value newValue) then
            value <- newValue
            x.MarkOutdated()
        

    interface AdaptiveValue with
        member x.GetValueUntyped t = x.GetValue t :> obj
        member x.ContentType =
            #if FABLE_COMPILER
            typeof<obj>
            #else
            typeof<'T>
            #endif

    interface AdaptiveValue<'T> with
        member x.GetValue t = x.GetValue t
        
    member private x.AsString = sprintf "cval(%A)" x.Value
    override x.ToString() = String.Format("cval({0})", x.Value)

and cval<'T> = ChangeableValue<'T>
    
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AVal =

    /// Base class for standard avals
    [<AbstractClass; StructuredFormatDisplay("{AsString}")>]
    type AbstractVal<'T>() =
        inherit AdaptiveObject()

        let mutable valueCache = Unchecked.defaultof<'T>

        abstract member Compute: AdaptiveToken -> 'T

        member x.GetValue(token: AdaptiveToken) =
            x.EvaluateAlways token (fun token ->
                if x.OutOfDate then
                    let v = x.Compute token
                    valueCache <- v
                    v
                else
                    valueCache                
            )

        member private x.AsString =
            if x.OutOfDate then sprintf "aval*(%A)" valueCache
            else sprintf "aval(%A)" valueCache

        override x.ToString() =
            if x.OutOfDate then String.Format("aval*({0})", valueCache)
            else String.Format("aval({0})", valueCache)
            
        interface AdaptiveValue with
            member x.GetValueUntyped t = x.GetValue t :> obj
            member x.ContentType =
                #if FABLE_COMPILER
                typeof<obj>
                #else
                typeof<'T>
                #endif

        interface AdaptiveValue<'T> with
            member x.GetValue t = x.GetValue t  
            
    /// Lazy without locking
    type LazyOrValue<'T> =
        val mutable public Create: unit -> 'T
        val mutable public Value: 'T
        val mutable public IsValue: bool

        new(value: 'T) = { Create = Unchecked.defaultof<_>; Value = value; IsValue = true }
        new(create: unit -> 'T) = { Create = create; Value = Unchecked.defaultof<_>; IsValue = false }
        
    /// A constant value that can either be a value or a lazy computation
    [<StructuredFormatDisplay("{AsString}")>]
    type ConstantVal<'T> private(data: LazyOrValue<'T>) =
        inherit ConstantObject()
        let mutable data = data

        member private x.GetValue(): 'T =
            if data.IsValue then 
                data.Value
            else
                let v = data.Create()
                data.IsValue <- true
                data.Value <- v
                data.Create <- Unchecked.defaultof<_>
                v

        member x.GetValue(_token: AdaptiveToken): 'T = 
            x.GetValue()
            
        interface AdaptiveValue with
            member x.GetValueUntyped t = x.GetValue t :> obj
            member x.ContentType = 
                #if FABLE_COMPILER
                typeof<obj>
                #else
                typeof<'T>
                #endif

        interface AdaptiveValue<'T> with
            member x.GetValue t = x.GetValue t    

        static member Lazy (create: unit -> 'T) =
            ConstantVal<'T>(LazyOrValue<'T> create) :> aval<_>

        static member Value (value: 'T) =
            ConstantVal<'T>(LazyOrValue<'T> value) :> aval<_>

        member private x.AsString =
            sprintf "constval(%A)" (x.GetValue())
            
        override x.ToString() =
            String.Format("constval({0})", x.GetValue())

        override x.GetHashCode() =
            let value = x.GetValue()
            Unchecked.hash value

        override x.Equals o =
            match o with
            | :? ConstantVal<'T> as o -> 
                let xv = x.GetValue()
                let ov = o.GetValue()
                Unchecked.equals xv ov
            | _ ->
                false
    /// Aval for mapping a single value
    type MapVal<'T1, 'T2>(mapping: 'T1 -> 'T2, input: aval<'T1>) =
        inherit AbstractVal<'T2>()

        // can we avoid double caching (here and in AbstractVal)
        let mutable cache: ValueOption<struct ('T1 * 'T2)> = ValueNone
        
        override x.Compute(token: AdaptiveToken) =
            let i = input.GetValue token
            match cache with
            | ValueSome (struct (a, b)) when cheapEqual a i ->
                b
            | _ ->
                let b = mapping i
                cache <- ValueSome(struct (i, b))
                b

        interface AdaptiveValue<'T2> with
            member x.GetValue t = x.GetValue t

    /// Aval for mapping 2 values in 'parallel'
    type Map2Val<'T1, 'T2, 'T3>(mapping: 'T1 -> 'T2 -> 'T3, a: aval<'T1>, b: aval<'T2>) =
        inherit AbstractVal<'T3>()

        let mapping = OptimizedClosures.FSharpFunc<'T1, 'T2, 'T3>.Adapt(mapping)
        let mutable cache: ValueOption<struct ('T1 * 'T2 * 'T3)> = ValueNone

        override x.Compute (token: AdaptiveToken) =
            let a = a.GetValue token
            let b = b.GetValue token
            match cache with
            | ValueSome(struct (oa, ob, oc)) when cheapEqual oa a && cheapEqual ob b ->
                oc
            | _ ->
                let c = mapping.Invoke (a, b)
                cache <- ValueSome(struct (a, b, c))
                c

    /// Aval for mapping 3 values in 'parallel'
    type Map3Val<'T1, 'T2, 'T3, 'T4>(mapping: 'T1 -> 'T2 -> 'T3 -> 'T4, a: aval<'T1>, b: aval<'T2>, c: aval<'T3>) =
        inherit AbstractVal<'T4>()

        let mapping = OptimizedClosures.FSharpFunc<'T1, 'T2, 'T3, 'T4>.Adapt(mapping)
        let mutable cache: ValueOption<struct ('T1 * 'T2 * 'T3 * 'T4)> = ValueNone

        override x.Compute (token: AdaptiveToken) =
            let a = a.GetValue token
            let b = b.GetValue token
            let c = c.GetValue token
            match cache with
            | ValueSome (struct (oa, ob, oc, od)) when cheapEqual oa a && cheapEqual ob b && cheapEqual oc c ->
                od
            | _ ->
                let d = mapping.Invoke (a, b, c)
                cache <- ValueSome (struct (a, b, c, d))
                d

    /// Aval for binding a single value
    type BindVal<'T1, 'T2>(mapping: 'T1 -> aval<'T2>, input: aval<'T1>) =
        inherit AbstractVal<'T2>()

        let mutable inner: ValueOption< struct ('T1 * aval<'T2>) > = ValueNone
        let mutable inputDirty = 1

        override x.InputChangedObject(_, o) =
            if Object.ReferenceEquals(o, input) then 
                inputDirty <- 1

        override x.Compute(token: AdaptiveToken) =
            let va = input.GetValue token
            #if FABLE_COMPILER
            let inputDirty = let v = inputDirty in inputDirty <- 0; v <> 0
            #else
            let inputDirty = System.Threading.Interlocked.Exchange(&inputDirty, 0) <> 0
            #endif
            match inner with
            | ValueNone ->
                let result = mapping va
                inner <- ValueSome (struct (va, result))
                result.GetValue token   
                
            | ValueSome(struct (oa, oldResult)) when not inputDirty || cheapEqual oa va ->
                oldResult.GetValue token

            | ValueSome(struct (_, old)) ->
                old.Outputs.Remove x |> ignore
                let result = mapping va
                inner <- ValueSome (struct (va, result))
                result.GetValue token     

    /// Aval for binding two values in 'parallel'
    type Bind2Val<'T1, 'T2, 'T3>(mapping: 'T1 -> 'T2 -> aval<'T3>, value1: aval<'T1>, value2: aval<'T2>) =
        inherit AbstractVal<'T3>()

        let mapping = OptimizedClosures.FSharpFunc<'T1, 'T2, aval<'T3>>.Adapt(mapping)
        let mutable inner: ValueOption< struct ('T1 * 'T2 * aval<'T3>) > = ValueNone
        let mutable inputDirty = 1

        override x.InputChangedObject(_, o) =
            if Object.ReferenceEquals(o, value1) || Object.ReferenceEquals(o, value2) then 
                inputDirty <- 1

        override x.Compute(token: AdaptiveToken) =
            let va = value1.GetValue token
            let vb = value2.GetValue token
            #if FABLE_COMPILER
            let inputDirty = let v = inputDirty in inputDirty <- 0; v <> 0
            #else
            let inputDirty = System.Threading.Interlocked.Exchange(&inputDirty, 0) <> 0
            #endif
            
            match inner with
            | ValueNone ->
                let res = mapping.Invoke (va, vb)
                inner <- ValueSome (struct (va, vb, res))
                res.GetValue token  

            | ValueSome(struct (oa, ob, res)) when not inputDirty || (cheapEqual oa va && cheapEqual ob vb) ->
                res.GetValue token

            | ValueSome(struct (_, _, old)) ->
                old.Outputs.Remove x |> ignore
                let res = mapping.Invoke (va, vb)
                inner <- ValueSome (struct (va, vb, res))
                res.GetValue token     
    
    /// Aval for binding three values in 'parallel'
    type Bind3Val<'T1, 'T2, 'T3, 'T4>(mapping: 'T1 -> 'T2 -> 'T3 -> aval<'T4>, value1: aval<'T1>, value2: aval<'T2>, value3: aval<'T3>) =
        inherit AbstractVal<'T4>()

        let mapping = OptimizedClosures.FSharpFunc<'T1, 'T2, 'T3, aval<'T4>>.Adapt(mapping)
        let mutable inner: ValueOption< struct ('T1 * 'T2 * 'T3 * aval<'T4>) > = ValueNone
        let mutable inputDirty = 1

        override x.InputChangedObject(_, o) =
            if Object.ReferenceEquals(o, value1) || Object.ReferenceEquals(o, value2) || Object.ReferenceEquals(o, value3) then 
                inputDirty <- 1

        override x.Compute(token: AdaptiveToken) =
            let va = value1.GetValue token
            let vb = value2.GetValue token
            let vc = value3.GetValue token
            #if FABLE_COMPILER
            let inputDirty = let v = inputDirty in inputDirty <- 0; v <> 0
            #else
            let inputDirty = System.Threading.Interlocked.Exchange(&inputDirty, 0) <> 0
            #endif
            
            match inner with
            | ValueNone ->
                let res = mapping.Invoke (va, vb, vc)
                inner <- ValueSome (struct (va, vb, vc, res))
                res.GetValue token  

            | ValueSome(struct (oa, ob, oc, res)) when not inputDirty || (cheapEqual oa va && cheapEqual ob vb && cheapEqual oc vc) ->
                res.GetValue token

            | ValueSome(struct (_, _, _, old)) ->
                old.Outputs.Remove x |> ignore
                let res = mapping.Invoke (va, vb, vc)
                inner <- ValueSome (struct (va, vb, vc, res))
                res.GetValue token     

    /// Aval for custom computations
    type CustomVal<'T>(compute: AdaptiveToken -> 'T) =
        inherit AbstractVal<'T>()

        override x.Compute(token: AdaptiveToken) =
            compute token

    let inline force (value: aval<'T>) =
        value.GetValue AdaptiveToken.Top

    let inline init (value: 'T) =
        cval value

    let constant (value: 'T) =
        ConstantVal.Value value
        
    let delay (value: unit -> 'T) =
        ConstantVal.Lazy value

    let map (mapping: 'T1 -> 'T2) (value: aval<'T1>) =
        if value.IsConstant then 
            ConstantVal.Lazy (fun () -> value |> force |> mapping)
        else
            MapVal(mapping, value) :> aval<_>
            
    let map2 (mapping: 'T1 -> 'T2 -> 'T3) (value1: aval<'T1>) (value2: aval<'T2>) =
        if value1.IsConstant && value2.IsConstant then 
            ConstantVal.Lazy (fun () -> 
                mapping (force value1) (force value2)
            )

        elif value1.IsConstant then
            let a = force value1
            map (fun b -> mapping a b) value2

        elif value2.IsConstant then
            let b = force value2
            map (fun a -> mapping a b) value1

        else
            Map2Val(mapping, value1, value2) :> aval<_>
            
    let map3 (mapping: 'T1 -> 'T2 -> 'T3 -> 'T4) (value1: aval<'T1>) (value2: aval<'T2>) (value3: aval<'T3>) =
        if value1.IsConstant && value2.IsConstant && value3.IsConstant then 
            ConstantVal.Lazy (fun () -> 
                mapping (force value1) (force value2) (force value3)
            )

        elif value1.IsConstant then
            let a = force value1
            map2 (fun b c -> mapping a b c) value2 value3

        elif value2.IsConstant then
            let b = force value2
            map2 (fun a c -> mapping a b c) value1 value3

        elif value3.IsConstant then
            let c = force value3
            map2 (fun a b -> mapping a b c) value1 value2

        else
            Map3Val(mapping, value1, value2, value3) :> aval<_>
              
    let bind (mapping: 'T1 -> aval<'T2>) (value: aval<'T1>) =
        if value.IsConstant then
            value |> force |> mapping
        else
            BindVal<'T1, 'T2>(mapping, value) :> aval<_>       

    let bind2 (mapping: 'T1 -> 'T2 -> aval<'T3>) (value1: aval<'T1>) (value2: aval<'T2>) =
        if value1.IsConstant && value2.IsConstant then
            mapping (force value1) (force value2)

        elif value1.IsConstant then
            let a = force value1
            bind (fun b -> mapping a b) value2

        elif value2.IsConstant then
            let b = force value2
            bind (fun a -> mapping a b) value1

        else
            Bind2Val<'T1, 'T2, 'T3>(mapping, value1, value2) :> aval<_>       

    let bind3 (mapping: 'T1 -> 'T2 -> 'T3 -> aval<'T4>) (value1: aval<'T1>) (value2: aval<'T2>) (value3: aval<'T3>) =
        if value1.IsConstant && value2.IsConstant && value3.IsConstant then
            mapping (force value1) (force value2) (force value3)

        elif value1.IsConstant then
            let a = force value1
            bind2 (fun b c -> mapping a b c) value2 value3

        elif value2.IsConstant then
            let b = force value2
            bind2 (fun a c -> mapping a b c) value1 value3
            
        elif value3.IsConstant then
            let c = force value3
            bind2 (fun a b -> mapping a b c) value1 value2
        else
            Bind3Val<'T1, 'T2, 'T3, 'T4>(mapping, value1, value2, value3) :> aval<_>       

    let custom (compute: AdaptiveToken -> 'T) =
        CustomVal compute :> aval<_>
