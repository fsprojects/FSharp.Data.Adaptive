module Program

open FSharp.Control.Incremental

[<EntryPoint>]
let main args =

    let m = ARef.init 10
    let test = ARef.map (fun v -> v + 10) m

    test |> ARef.force |> printfn "%A"

    transact (fun () ->
        m.Value <- 100
    )
    
    test |> ARef.force |> printfn "%A"

    transact (fun () ->
        m.Value <- 0
    )
    
    test |> ARef.force |> printfn "%A"

    //WeakOutputSet.``[WeakOutputSet] actually weak``()
    0
