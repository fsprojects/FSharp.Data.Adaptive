namespace Benchmarks

open System.Reflection

type Sleeper private (n : int) =
    static let sleeper = typeof<Sleeper>.GetMethod("sleep", BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static)
    
    static member sleep (n : int) =
        let mutable sum = 0
        for i in 1 .. n do
            sum <- sum + i

    member x.run() = Sleeper.sleep n

    static member Create (ms : float) =
        Sleeper.sleep 1000
        System.Runtime.CompilerServices.RuntimeHelpers.PrepareMethod sleeper.MethodHandle

        let sw = System.Diagnostics.Stopwatch()
        let runtime (n : int) =
            sw.Restart()
            for i in 1 .. 50 do
                Sleeper.sleep n
            sw.Stop()
            sw.Elapsed.TotalMilliseconds / 50.0

        let mutable min = 0
        let mutable max = 1 <<< 25

        while max > min do
            let m = (min + max) / 2
            let t = runtime m
            if t > ms then max <- m
            else min <- m

        Sleeper min
