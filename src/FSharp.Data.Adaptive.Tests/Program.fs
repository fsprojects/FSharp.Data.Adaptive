module Program

open FSharp.Data.Adaptive

[<EntryPoint>]
let main _args =
    IndexList.``[IndexList] tryGetPosition`` [-2; 0]
    0
