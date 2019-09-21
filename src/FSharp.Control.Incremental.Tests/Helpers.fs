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


    /// force garbage collection
    let ensureGC() =
        GC.Collect(3, GCCollectionMode.Forced, true, true)
        GC.WaitForFullGCComplete() |> ignore

    let getRealMemory() =
        ensureGC()
        let m0 = GC.GetTotalMemory(true)
        let arr : byte[] = Array.zeroCreate 1
        ensureGC()
        let m0 = GC.GetTotalMemory(true)
        let mutable m1 = m0

        let mutable size = 0L
        let mutable res = []
        while m0 < m1 do
            let arr : byte[] = Array.zeroCreate (32 - 24)
            res <- arr :: res
            size <- size + 32L + 56L
            m1 <- GC.GetTotalMemory(true)
        m0 - size