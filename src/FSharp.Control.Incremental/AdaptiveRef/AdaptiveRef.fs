namespace FSharp.Control.Incremental

open System

type AdaptiveRef<'T> =
    inherit IAdaptiveObject
    abstract member GetValue: AdaptiveToken -> 'T

and aref<'T> = AdaptiveRef<'T>

[<Sealed; StructuredFormatDisplay("{AsString}")>]
type ChangeableRef<'T> =
    inherit AdaptiveObject
    val mutable private value: 'T

    member x.Value
        with get() = x.value
        and set v =
            if not (cheapEqual x.value v) then
                x.value <- v
                x.MarkOutdated()
                
    member x.GetValue (token: AdaptiveToken) =
        x.EvaluateAlways token (fun _ ->
            x.value
        )

    interface aref<'T> with
        member x.GetValue t = x.GetValue t
        
    member private x.AsString = sprintf "cref(%A)" x.Value
    override x.ToString() = String.Format("cref({0})", x.Value)

    new(value: 'T) = { value = value }

and cref<'T> = ChangeableRef<'T>
    
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ARef =

    /// base class for standard arefs
    [<AbstractClass; StructuredFormatDisplay("{AsString}")>]
    type AbstractRef<'T>() =
        inherit AdaptiveObject()

        let mutable cache = Unchecked.defaultof<'T>

        abstract member Compute: AdaptiveToken -> 'T

        member x.GetValue(token: AdaptiveToken) =
            x.EvaluateAlways token (fun token ->
                if x.OutOfDate then
                    let v = x.Compute token
                    cache <- v
                    v
                else
                    cache                
            )

        member private x.AsString =
            if x.OutOfDate then sprintf "aref*(%A)" cache
            else sprintf "aref(%A)" cache

        override x.ToString() =
            if x.OutOfDate then String.Format("aref*({0})", cache)
            else String.Format("aref({0})", cache)

        interface aref<'T> with
            member x.GetValue t = x.GetValue t  
            
    /// lazy without locking
    type LazyOrValue<'T> =
        val mutable public Create: unit -> 'T
        val mutable public Value: 'T
        val mutable public IsValue: bool

        new(value: 'T) = { Create = Unchecked.defaultof<_>; Value = value; IsValue = true }
        new(create: unit -> 'T) = { Create = create; Value = Unchecked.defaultof<_>; IsValue = false }
        
    /// a constant ref that can either be a value or a lazy computation
    [<StructuredFormatDisplay("{AsString}")>]
    type ConstantRef<'T> private(data: LazyOrValue<'T>) =
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

        interface aref<'T> with
            member x.GetValue t = x.GetValue t    

        static member Lazy (create: unit -> 'T) =
            ConstantRef<'T>(LazyOrValue<'T> create) :> aref<_>

        static member Value (value: 'T) =
            ConstantRef<'T>(LazyOrValue<'T> value) :> aref<_>

        member private x.AsString =
            sprintf "constref(%A)" (x.GetValue())
            
        override x.ToString() =
            String.Format("constref({0})", x.GetValue())

        override x.GetHashCode() =
            let value = x.GetValue()
            cheapHash value

        override x.Equals o =
            match o with
            | :? ConstantRef<'T> as o -> 
                let xv = x.GetValue()
                let ov = o.GetValue()
                cheapEqual xv ov
            | _ ->
                false

    /// ref for mapping a single value
    type MapRef<'T1, 'T2>(mapping: 'T1 -> 'T2, input: aref<'T1>) =
        inherit AbstractRef<'T2>()

        // can we avoid double caching (here and in AbstractRef)
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

        interface aref<'T2> with
            member x.GetValue t = x.GetValue t

    /// ref for mapping 2 values in 'parallel'
    type Map2Ref<'T1, 'T2, 'T3>(mapping: 'T1 -> 'T2 -> 'T3, a: aref<'T1>, b: aref<'T2>) =
        inherit AbstractRef<'T3>()

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

    /// ref for mapping 3 values in 'parallel'
    type Map3Ref<'T1, 'T2, 'T3, 'T4>(mapping: 'T1 -> 'T2 -> 'T3 -> 'T4, a: aref<'T1>, b: aref<'T2>, c: aref<'T3>) =
        inherit AbstractRef<'T4>()

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

    /// ref for binding a single value
    type BindRef<'T1, 'T2>(mapping: 'T1 -> aref<'T2>, input: aref<'T1>) =
        inherit AbstractRef<'T2>()

        let mutable inner: ValueOption< struct ('T1 * aref<'T2>) > = ValueNone
        let mutable inputDirty = 1

        override x.InputChanged(_, o) =
            if Object.ReferenceEquals(o, input) then 
                inputDirty <- 1

        override x.Compute(token: AdaptiveToken) =
            let va = input.GetValue token
            let inputDirty = System.Threading.Interlocked.Exchange(&inputDirty, 0) <> 0

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

    /// ref for binding two values in 'parallel'
    type Bind2Ref<'T1, 'T2, 'T3>(mapping: 'T1 -> 'T2 -> aref<'T3>, ref1: aref<'T1>, ref2: aref<'T2>) =
        inherit AbstractRef<'T3>()

        let mapping = OptimizedClosures.FSharpFunc<'T1, 'T2, aref<'T3>>.Adapt(mapping)
        let mutable inner: ValueOption< struct ('T1 * 'T2 * aref<'T3>) > = ValueNone
        let mutable inputDirty = 1

        override x.InputChanged(_, o) =
            if Object.ReferenceEquals(o, ref1) || Object.ReferenceEquals(o, ref2) then 
                inputDirty <- 1

        override x.Compute(token: AdaptiveToken) =
            let va = ref1.GetValue token
            let vb = ref2.GetValue token
            let inputDirty = System.Threading.Interlocked.Exchange(&inputDirty, 0) <> 0

            match inner with
            | ValueNone ->
                let ref = mapping.Invoke (va, vb)
                inner <- ValueSome (struct (va, vb, ref))
                ref.GetValue token  

            | ValueSome(struct (oa, ob, res)) when not inputDirty || (cheapEqual oa va && cheapEqual ob vb) ->
                res.GetValue token

            | ValueSome(struct (_, _, old)) ->
                old.Outputs.Remove x |> ignore
                let ref = mapping.Invoke (va, vb)
                inner <- ValueSome (struct (va, vb, ref))
                ref.GetValue token     

    /// ref for custom computations
    type CustomRef<'T>(compute: AdaptiveToken -> 'T) =
        inherit AbstractRef<'T>()

        override x.Compute(token: AdaptiveToken) =
            compute token

    let inline force (ref: aref<'T>) =
        ref.GetValue AdaptiveToken.Top

    let inline init (value: 'T) =
        cref value

    let constant (value: 'T) =
        ConstantRef.Value value
        
    let delay (value: unit -> 'T) =
        ConstantRef.Lazy value

    let map (mapping: 'T1 -> 'T2) (ref: aref<'T1>) =
        if ref.IsConstant then 
            ConstantRef.Lazy (fun () -> ref |> force |> mapping)
        else
            MapRef(mapping, ref) :> aref<_>
            
    let map2 (mapping: 'T1 -> 'T2 -> 'T3) (ref1: aref<'T1>) (ref2: aref<'T2>) =
        if ref1.IsConstant && ref2.IsConstant then 
            ConstantRef.Lazy (fun () -> 
                mapping (force ref1) (force ref2)
            )

        elif ref1.IsConstant then
            let a = force ref1
            map (fun b -> mapping a b) ref2

        elif ref2.IsConstant then
            let b = force ref2
            map (fun a -> mapping a b) ref1

        else
            Map2Ref(mapping, ref1, ref2) :> aref<_>
            
    let map3 (mapping: 'T1 -> 'T2 -> 'T3 -> 'T4) (ref1: aref<'T1>) (ref2: aref<'T2>) (ref3: aref<'T3>) =
        if ref1.IsConstant && ref2.IsConstant && ref3.IsConstant then 
            ConstantRef.Lazy (fun () -> 
                mapping (force ref1) (force ref2) (force ref3)
            )

        elif ref1.IsConstant then
            let a = force ref1
            map2 (fun b c -> mapping a b c) ref2 ref3

        elif ref2.IsConstant then
            let b = force ref2
            map2 (fun a c -> mapping a b c) ref1 ref3

        elif ref3.IsConstant then
            let c = force ref3
            map2 (fun a b -> mapping a b c) ref1 ref2

        else
            Map3Ref(mapping, ref1, ref2, ref3) :> aref<_>
              
    let bind (mapping: 'T1 -> aref<'T2>) (ref: aref<'T1>) =
        if ref.IsConstant then
            ref |> force |> mapping
        else
            BindRef<'T1, 'T2>(mapping, ref) :> aref<_>       

    let bind2 (mapping: 'T1 -> 'T2 -> aref<'T3>) (ref1: aref<'T1>) (ref2: aref<'T2>) =
        if ref1.IsConstant && ref2.IsConstant then
            mapping (force ref1) (force ref2)

        elif ref1.IsConstant then
            let a = force ref1
            bind (fun b -> mapping a b) ref2

        elif ref2.IsConstant then
            let b = force ref2
            bind (fun a -> mapping a b) ref1

        else
            Bind2Ref<'T1, 'T2, 'T3>(mapping, ref1, ref2) :> aref<_>       

    let custom (compute: AdaptiveToken -> 'T) =
        CustomRef compute :> aref<_>
