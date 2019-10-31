module Transaction

open NUnit.Framework
open FsUnit
open FSharp.Data.Adaptive

[<Test>]
let ``[Transaction] transact sets/restores current``() =
    
    transact (fun () ->
        let a = Transaction.Current
        transact (fun () ->
            let b = Transaction.Current
            a |> should not' (equal b)
        )
        Transaction.Current |> should equal a
    )
    Transaction.Current |> should equal Option<Transaction>.None
    
[<Test>]
let ``[Transaction] transact sets/restores current on exception``() =
    
    transact (fun () ->
        let a = Transaction.Current
        try
            transact (fun () ->
                failwith "inner exn"
            )
        with _ ->
            () // expected
        Transaction.Current |> should equal a
    )
