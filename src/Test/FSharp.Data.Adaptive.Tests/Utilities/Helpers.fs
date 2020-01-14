namespace FsCheck

open FSharp.Data.Adaptive
open System

[<AutoOpen>]
module Helpers =
    open NUnit.Framework.Constraints

    let refequal (expected : 'T) =
        { new Constraint() with
            override x.ApplyTo<'B>(other : 'B) =    
                x.Description <- string <| System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode expected
                let otherHash = System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode other
                if System.Object.ReferenceEquals(expected, other) then
                    ConstraintResult(x, otherHash, true)
                else
                    ConstraintResult(x, otherHash, false)
        }

    let setequal (expected : seq<'T>) =
        let expected = FSharp.Data.Adaptive.HashSet.ofSeq expected
        { new Constraint() with 
            override x.ApplyTo<'B>(o : 'B) =
                x.Description <- string expected
                match o :> obj with
                | :? seq<'T> as o -> 
                    let should = System.Collections.Generic.HashSet expected
                    let is = System.Collections.Generic.HashSet o
                    if should.SetEquals is then
                        ConstraintResult(x, o, true)
                    else
                        ConstraintResult(x, o, false)
                        
                | _ -> 
                    ConstraintResult(x, o, false)
        }


    let mapequal (expected : seq<'K * 'V>) =
        let expected = FSharp.Data.Adaptive.HashSet.ofSeq expected
        { new Constraint() with 
            override x.ApplyTo<'B>(o : 'B) =
                x.Description <- string expected
                let real =
                    match o :> obj with
                    | :? HashMap<'K, 'V> as o -> HashMap.toArray o |> Some
                    | :? seq<'K * 'V> as o -> Seq.toArray o |> Some
                    | :? seq<System.Collections.Generic.KeyValuePair<'K, 'V>> as o ->
                        o |> Seq.map (fun kv -> kv.Key, kv.Value) |> Seq.toArray |> Some
                    | _ -> None

                match real with
                | Some real -> 
                    let expected = Seq.toArray expected

                    let mutable errors = []
                    let mutable rest = real
                    for (ek, ev) in expected do
                        match rest |> Array.tryFindIndex (fun (rk, rv) -> Unchecked.equals rk ek) with
                        | Some idx ->
                            let (rk, rv) = rest.[idx]
                            if Unchecked.equals rv ev then
                                rest <- Array.append (Array.take idx rest) (Array.skip (idx + 1) rest)
                            else
                                errors <- (sprintf "%A: %A vs %A" rk rv ev) :: errors
                        | None ->
                            errors <- (sprintf "%A: missing" ek) :: errors

                    for (rk,_) in rest do
                        errors <- (sprintf "%A: additional" rk) :: errors

                    if List.isEmpty errors then
                        ConstraintResult(x, real, true)
                    else
                        ConstraintResult(x, String.concat "\r\n" errors, false)

                | None ->
                    ConstraintResult(x, expected, false)
        }


    /// Force full garbage collection
    let ensureGC() =
        GC.Collect(3, GCCollectionMode.Forced, true, true)
        GC.WaitForFullGCComplete() |> ignore

    let allocDummy() = 
        let m0 = GC.GetTotalMemory(true)
        let arr : byte[] = Array.zeroCreate 1
        ()

    let getObjectOverhead() =
        let cnt = 1 <<< 20
        let run() = 
            Array.init cnt (fun _ -> obj())

        let warmup() =
            run() |> ignore

        warmup()
        let before = System.GC.GetTotalMemory(true)
        let a = run()
        let after = System.GC.GetTotalMemory(true)

        let size = float (after - before) / float a.Length
        let overhead = round size |> int64
        overhead

    
    let getArrayOverhead() =
        let cnt = 1 <<< 20
        let run() = 
            Array.init cnt (fun _ -> Array.zeroCreate<byte> 0)

        let warmup() =
            run() |> ignore

        warmup()
        let before = System.GC.GetTotalMemory(true)
        let a = run()
        let after = System.GC.GetTotalMemory(true)

        let size = float (after - before) / float a.Length
        let overhead = round size |> int
        overhead

    
    let pointerSize = int64 sizeof<nativeint>
    let granularity = 128
    let pageSize = 1L
    let objectOverhead = getObjectOverhead()
    let arrayOverhead = getArrayOverhead()


    /// Tries hard to get the actual amount of memory used.
    let inline getRealMemory() =
        ensureGC()
        allocDummy()
        ensureGC()
        let m0 = GC.GetTotalMemory(true)
        let mutable m1 = m0

        let mutable lastSize = 0L
        let mutable size = 0L
        let mutable res = []
        while m0 < m1 do
            let arr : byte[] = Array.zeroCreate (granularity - arrayOverhead)
            res <- arr :: res
            lastSize <- size
            size <- size + int64 granularity + objectOverhead + pointerSize
            ensureGC()
            m1 <- GC.GetTotalMemory(true)
        pageSize * ((m0 - lastSize) / pageSize)