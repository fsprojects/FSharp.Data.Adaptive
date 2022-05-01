open FSharp.Data.Adaptive


let a = cval 10

let b = a |> AVal.map (fun x -> x * 2)
printfn "%A" (AVal.force b)

transact (fun () -> a.Value <- 100)
printfn "%A" (AVal.force b)
