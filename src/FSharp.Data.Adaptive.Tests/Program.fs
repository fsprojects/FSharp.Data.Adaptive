module Program

open FSharp.Data.Adaptive

[<EntryPoint>]
let main _args =
    let test (value : aval<int>) =
        alist {
            let! max = value
            for i in 1 .. max do
                yield i
        }

    let v = AVal.init 2
    let res = test v

    res.Content |> AVal.force |> printfn "%A"
    transact (fun () -> v.Value <- 3)
    res.Content |> AVal.force |> printfn "%A"

    0
