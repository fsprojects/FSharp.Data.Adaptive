module Arr

open FSharp.Data.Adaptive

let run() =
    
    
    
    
    let test = Arr.ofArray [|1..20|]

    let a = Arr.ofList [1;2;3;4]
    let b = Arr.ofList [1;2;5;6;4;10;20]
    let c = Arr.ofList [5;4]

    
    printfn "%A" a
    printfn "%A" (a.Split 3)
    
    let dab = Arr.computeDelta (=) a b
    let dbc = Arr.computeDelta (=) b c
    let dac = Arr.computeDelta (=) a c


    printfn "a: %0A" (Arr.toArray a)
    printfn "b: %0A" (Arr.toArray b)
    printfn "c: %0A" (Arr.toArray c)

    printfn "a->b: %A" (dab |> ArrDelta.toArray |> Array.map (fun op -> op.Index, op.Count, Arr.toArray op.Elements))
    printfn "b->c: %A" (dbc |> ArrDelta.toArray |> Array.map (fun op -> op.Index, op.Count, Arr.toArray op.Elements))
    printfn "a->c: %A" (dac |> ArrDelta.toArray |> Array.map (fun op -> op.Index, op.Count, Arr.toArray op.Elements))

    printfn "b(a): %0A" (Arr.applyDelta a dab |> Arr.toArray)
    printfn "c(a): %0A" (Arr.applyDelta a dac |> Arr.toArray)
    printfn "c(b): %0A" (Arr.applyDelta b dbc |> Arr.toArray)

    printfn "ac1: %0A" (ArrDelta.combine dab dbc |> ArrDelta.toArray |> Array.map (fun op -> op.Index, op.Count, Arr.toArray op.Elements))
    printfn "ac1: %0A" (Arr.applyDelta a (ArrDelta.combine dab dbc) |> Arr.toArray)