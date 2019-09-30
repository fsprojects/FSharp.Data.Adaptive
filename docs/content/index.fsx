(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/Release/netstandard2.0"
#r "FSharp.Data.Adaptive.dll"

(**

# FSharp.Data.Adaptive : bla bla

## Example

*)
open FSharp.Data.Adaptive

let test (a : aval<int>) =
    a |> AVal.map (fun v -> v * 2)


let setStuff() =
    let set = cset [1;2;3]

    let dependent = 
        set 
        |> ASet.map (fun v -> v * 2) 
        |> ASet.sum
    printfn "%A" (AVal.force dependent)

    transact (fun () -> set.Remove 1 |> ignore)
    printfn "%A" (AVal.force dependent)

    transact (fun () -> set.Add 5 |> ignore)
    printfn "%A" (AVal.force dependent)


