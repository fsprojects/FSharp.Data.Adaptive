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


