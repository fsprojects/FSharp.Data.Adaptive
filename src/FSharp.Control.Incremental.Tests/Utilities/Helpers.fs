namespace FsCheck

open System

[<AutoOpen>]
module Helpers =
    let refequal (expected : 'a) =
        { new NHamcrest.Core.IsEqualMatcher<obj>(expected) with
            override x.Matches o =
                System.Object.ReferenceEquals(o, expected)
        }

    let setequal (expected : seq<'a>) =
        { new NHamcrest.Core.IsEqualMatcher<obj>(expected) with
            override x.Matches o =
                match o with
                | :? seq<'a> as o -> 
                    let should = System.Collections.Generic.HashSet expected
                    let is = System.Collections.Generic.HashSet o
                    should.SetEquals is
                | _ -> 
                    false
        }


    /// force full garbage collection
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


    /// tries hard to get the actual amount of memory used.
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