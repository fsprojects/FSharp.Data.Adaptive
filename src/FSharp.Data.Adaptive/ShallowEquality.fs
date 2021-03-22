namespace FSharp.Data.Adaptive


#if FABLE_COMPILER

module private ShallowEqualityHelpers =
    open Fable.Core
    open Fable.Core.JsInterop

    [<Emit("$0 === $1")>]
    let equals (a : 'a) (b : 'a) : bool = jsNative

    let inline hash (a : 'a) = (a :> obj).GetHashCode()

type ShallowEqualityComparer<'a> private() =
    static let mutable instance = 
        { new System.Collections.Generic.IEqualityComparer<'a> with
            member x.GetHashCode v = ShallowEqualityHelpers.hash v
            member x.Equals(a,b) = ShallowEqualityHelpers.equals a b
        }

    static member Instance = instance
    
    static member Set(newInstance : System.Collections.Generic.IEqualityComparer<'a>) =
        instance <- newInstance

    static member ShallowHashCode v = instance.GetHashCode v
    static member ShallowEquals(a,b) = instance.Equals(a,b)

    interface System.Collections.Generic.IEqualityComparer<'a> with
        member x.GetHashCode v = instance.GetHashCode v
        member x.Equals(a,b) = instance.Equals(a,b)

#else
open System.Reflection.Emit
open System.Reflection
open System.Runtime.InteropServices
open System.Runtime.CompilerServices
open Microsoft.FSharp.Reflection

type private ShallowEqDelegate<'a> = delegate of 'a * 'a -> bool
type private ShallowHashDelegate<'a> = delegate of 'a -> int

[<AutoOpen>]
module ``DynamicMethod Extensions`` =
    let private canEmit =
        try 
            let meth = 
                DynamicMethod(
                    "testEmitMethod", 
                    MethodAttributes.Static ||| MethodAttributes.Public,
                    CallingConventions.Standard,
                    typeof<int>,
                    [| typeof<int> |],
                    typeof<obj>,
                    true
                )
            let il = meth.GetILGenerator()
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldc_I4, 10)
            il.Emit(OpCodes.Mul)
            il.Emit(OpCodes.Ret)

            let f = meth.CreateDelegate(typeof<System.Func<int, int>>) |> unbox<System.Func<int, int>>

            f.Invoke(1) = 10
        with _ ->
            false

    type DynamicMethod with
        static member IsSupported = canEmit


[<AbstractClass; Sealed>]
type private HashCodeHelpers private() =

    static let combineMeth = 
        typeof<HashCodeHelpers>.GetMethod(
            "Combine", 
            BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic,
            System.Type.DefaultBinder,
            [| typeof<int>; typeof<int> |],
            null
        )

    static member CombineMethod = combineMeth

    static member Combine(a : int, b : int) =   
        uint32 a ^^^ uint32 b + 0x9e3779b9u + ((uint32 a) <<< 6) + ((uint32 a) >>> 2) |> int

type ShallowEqualityComparer<'a> private() =
    static let typ = typeof<'a>
    static let self = typedefof<ShallowEqualityComparer<_>>

    static let isUnmanaged =
        if typ.IsEnum then
            true
        elif typ.IsValueType then
            let arr : 'a[] = [|Unchecked.defaultof<'a>|]
            try
                let g = GCHandle.Alloc(arr, GCHandleType.Pinned)
                g.Free()
                true
            with _ ->
                false
        else
            false

    static let isRecursive =
        let rec run (visited : System.Collections.Generic.HashSet<_>) (top : bool) (t : System.Type) =
            if not top && typ.IsAssignableFrom t then
                true
            elif visited.Add t then
                if FSharpType.IsUnion(t, true) then
                    let fields = FSharpType.GetUnionCases(t, true) |> Array.collect (fun ci -> ci.GetFields())
                    fields |> Array.exists (fun f -> run visited false f.PropertyType)
                else
                    let fields = t.GetFields(BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance)
                    fields |> Array.exists (fun f -> run visited false f.FieldType)
            else
                false

        if typ.IsValueType then false
        else run (System.Collections.Generic.HashSet()) true typ

    static let getTag (il : ILGenerator) (typ : System.Type) =
        
        let tag = FSharpValue.PreComputeUnionTagMemberInfo(typ, true)
         
        match tag with
        | :? MethodInfo as tag ->
            let cc = if tag.IsStatic then OpCodes.Call else OpCodes.Callvirt
            il.EmitCall(cc, tag, null)
                
        | :? PropertyInfo as tag ->
            il.EmitCall(OpCodes.Callvirt, tag.GetMethod, null)

        | :? FieldInfo as tag ->
            il.Emit(OpCodes.Ldfld, tag)

        | _ ->
            failwith "bad tag"

    static let getHashCode =
        if isUnmanaged then
            Unchecked.hash
        elif typ.IsValueType then
            if DynamicMethod.IsSupported then
                let fields =
                    typ.GetFields(BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public)
                
                let meth = 
                    DynamicMethod(
                        "shallowHash", 
                        MethodAttributes.Static ||| MethodAttributes.Public,
                        CallingConventions.Standard,
                        typeof<int>,
                        [| typeof<'a> |],
                        typeof<'a>,
                        true
                    )


                let il = meth.GetILGenerator()
                let l = il.DeclareLocal(typeof<int>)

                // l <- 0
                il.Emit(OpCodes.Ldc_I4_0)
                il.Emit(OpCodes.Stloc, l)

                for f in fields do
                    let self = self.MakeGenericType [| f.FieldType |]
                    let hash = 
                        self.GetMethod(
                            "ShallowHashCode", 
                            BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public, 
                            System.Type.DefaultBinder, 
                            [| f.FieldType |], 
                            null
                        )

                    il.Emit(OpCodes.Ldarg_0)
                    il.Emit(OpCodes.Ldfld, f)
                    il.EmitCall(OpCodes.Call, hash, null)
                    il.Emit(OpCodes.Ldloc, l)
                    il.EmitCall(OpCodes.Call, HashCodeHelpers.CombineMethod, null)
                    il.Emit(OpCodes.Stloc, l)
                    
                il.Emit(OpCodes.Ldloc, l)
                il.Emit(OpCodes.Ret)

                let del = meth.CreateDelegate(typeof<ShallowHashDelegate<'a>>) |> unbox<ShallowHashDelegate<'a>>
                del.Invoke
            else
                // TODO: any better way??
                fun (v : 'a) -> 0

        elif FSharpType.IsUnion typ && DynamicMethod.IsSupported && not isRecursive then
            let cases = FSharpType.GetUnionCases(typ, true)
            let meth = 
                DynamicMethod(
                    "shallowHash", 
                    MethodAttributes.Static ||| MethodAttributes.Public,
                    CallingConventions.Standard,
                    typeof<int>,
                    [| typeof<'a> |],
                    typeof<'a>,
                    true
                )

            let il = meth.GetILGenerator()

            let zero = il.DefineLabel()

            let maxTag = cases |> Seq.map (fun c -> c.Tag) |> Seq.max
            let labels = Array.zeroCreate cases.Length
            let jumpTable = Array.create (2 + maxTag) zero
            for i in 0 .. cases.Length-1 do 
                let l = il.DefineLabel()
                labels.[i] <- l
                jumpTable.[cases.[i].Tag] <- l


            il.Emit(OpCodes.Ldarg_0)
            getTag il typ
            il.Emit(OpCodes.Switch, jumpTable)

            for i in 0 .. cases.Length - 1 do
                let ci = cases.[i]
                il.MarkLabel labels.[i]

                let h = il.DeclareLocal(typeof<int>)
                il.Emit(OpCodes.Ldc_I4, ci.Tag)
                il.Emit(OpCodes.Stloc, h)

                for field in ci.GetFields() do
                    let cmp = typedefof<ShallowEqualityComparer<_>>.MakeGenericType [| field.PropertyType |]
                    let inst = cmp.GetProperty "Instance"
                    let cmpType = typedefof<System.Collections.Generic.IEqualityComparer<_>>.MakeGenericType [| field.PropertyType |]
                    let hash = cmpType.GetMethod("GetHashCode", [| field.PropertyType |])

                    il.Emit(OpCodes.Ldloc, h)

                    il.EmitCall(OpCodes.Call, inst.GetMethod, null)
                    il.Emit(OpCodes.Ldarg_0)
                    il.EmitCall(OpCodes.Callvirt, field.GetMethod, null)
                    il.EmitCall(OpCodes.Callvirt, hash, null)

                    il.EmitCall(OpCodes.Call, HashCodeHelpers.CombineMethod, null)
                    il.Emit(OpCodes.Stloc, h)

                il.Emit(OpCodes.Ldloc, h)
                il.Emit(OpCodes.Ret)

            il.MarkLabel zero
            il.Emit(OpCodes.Ldc_I4_0)
            il.Emit(OpCodes.Ret)
            
            let del = meth.CreateDelegate(typeof<ShallowHashDelegate<'a>>) |> unbox<ShallowHashDelegate<'a>>
            del.Invoke

        else
            fun (v : 'a) -> RuntimeHelpers.GetHashCode(v :> obj)

    static let equals =
        if isUnmanaged then
            Unchecked.equals
        elif typ.IsValueType then
            if DynamicMethod.IsSupported then
                let fields =
                    typ.GetFields(BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public)

                let meth = 
                    DynamicMethod(
                        "shallowEquals", 
                        MethodAttributes.Static ||| MethodAttributes.Public,
                        CallingConventions.Standard,
                        typeof<bool>,
                        [| typeof<'a>; typeof<'a> |],
                        typeof<'a>,
                        true
                    )

                let il = meth.GetILGenerator()
                let falseLabel = il.DefineLabel()

                for f in fields do
                    let self = self.MakeGenericType [| f.FieldType |]
                    let eq = 
                        self.GetMethod(
                            "ShallowEquals", 
                            BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public, 
                            System.Type.DefaultBinder, 
                            [| f.FieldType; f.FieldType |], 
                            null
                        )

                    il.Emit(OpCodes.Ldarg_0)
                    il.Emit(OpCodes.Ldfld, f)
                    
                    il.Emit(OpCodes.Ldarg_1)
                    il.Emit(OpCodes.Ldfld, f)

                    il.EmitCall(OpCodes.Call, eq, null)
                    il.Emit(OpCodes.Brfalse, falseLabel)

                il.Emit(OpCodes.Ldc_I4_1)
                il.Emit(OpCodes.Ret)
                il.MarkLabel(falseLabel)
                il.Emit(OpCodes.Ldc_I4_0)
                il.Emit(OpCodes.Ret)

                let del = meth.CreateDelegate(typeof<ShallowEqDelegate<'a>>) |> unbox<ShallowEqDelegate<'a>>
                fun (a : 'a) (b : 'a) -> del.Invoke(a, b)
            else
                /// TODO: better way?
                fun (a : 'a) (b : 'a) -> false
        elif FSharpType.IsUnion(typ, true) && DynamicMethod.IsSupported && not isRecursive then
            let cases = FSharpType.GetUnionCases(typ, true)
            
            let meth = 
                DynamicMethod(
                    "shallowEquals", 
                    MethodAttributes.Static ||| MethodAttributes.Public,
                    CallingConventions.Standard,
                    typeof<bool>,
                    [| typeof<'a>; typeof<'a> |],
                    typeof<'a>,
                    true
                )

            let il = meth.GetILGenerator()

            let t = il.DefineLabel()
            let f = il.DefineLabel()

            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Beq, t)

            let t0 = il.DeclareLocal(typeof<int>)
            let t1 = il.DeclareLocal(typeof<int>)

            il.Emit(OpCodes.Ldarg_0)
            getTag il typ
            il.Emit(OpCodes.Stloc, t0)

            il.Emit(OpCodes.Ldarg_1)
            getTag il typ
            il.Emit(OpCodes.Stloc, t1)

            il.Emit(OpCodes.Ldloc, t0)
            il.Emit(OpCodes.Ldloc, t1)
            il.Emit(OpCodes.Bne_Un, f)

            let maxTag = cases |> Seq.map (fun c -> c.Tag) |> Seq.max
            let labels = Array.zeroCreate cases.Length
            let jumpTable = Array.create (2 + maxTag) f
            for i in 0 .. cases.Length-1 do 
                let l = il.DefineLabel()
                labels.[i] <- l
                jumpTable.[cases.[i].Tag] <- l

            il.Emit(OpCodes.Ldloc, t0)
            il.Emit(OpCodes.Switch, jumpTable)

            for i in 0 .. cases.Length-1 do
                let ci = cases.[i]
                il.MarkLabel labels.[i]
                let fields = ci.GetFields()

                for field in fields do
                    let cmp = typedefof<ShallowEqualityComparer<_>>.MakeGenericType [| field.PropertyType |]
                    let inst = cmp.GetProperty "Instance"
                    let cmpType = typedefof<System.Collections.Generic.IEqualityComparer<_>>.MakeGenericType [| field.PropertyType |]
                    let eq = cmpType.GetMethod("Equals", [| field.PropertyType; field.PropertyType |])

                    il.EmitCall(OpCodes.Call, inst.GetMethod, null)

                    il.Emit(OpCodes.Ldarg_0)
                    il.EmitCall(OpCodes.Callvirt, field.GetMethod, null)
                    
                    il.Emit(OpCodes.Ldarg_1)
                    il.EmitCall(OpCodes.Callvirt, field.GetMethod, null)

                    il.EmitCall(OpCodes.Callvirt, eq, null)
                    il.Emit(OpCodes.Brfalse, f)
                    



            il.MarkLabel t
            il.Emit(OpCodes.Ldc_I4_1)
            il.Emit(OpCodes.Ret)


            il.MarkLabel f
            il.Emit(OpCodes.Ldc_I4_0)
            il.Emit(OpCodes.Ret)

            
            let del = meth.CreateDelegate(typeof<ShallowEqDelegate<'a>>) |> unbox<ShallowEqDelegate<'a>>
            fun (a : 'a) (b : 'a) -> del.Invoke(a, b)


        else

            fun (a : 'a) (b : 'a) -> System.Object.ReferenceEquals(a :> obj, b :> obj)
            
    static let mutable instance = 
        { new System.Collections.Generic.IEqualityComparer<'a> with
            member x.GetHashCode v = getHashCode v
            member x.Equals(a,b) = equals a b
        }

    static member Instance = instance

    static member Set(newInstance : System.Collections.Generic.IEqualityComparer<'a>) =
        instance <- newInstance

    static member ShallowHashCode v = instance.GetHashCode v
    static member ShallowEquals(a,b) = instance.Equals(a,b)

    interface System.Collections.Generic.IEqualityComparer<'a> with
        member x.GetHashCode v = instance.GetHashCode v
        member x.Equals(a,b) = instance.Equals(a,b)

#endif
