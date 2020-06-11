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
