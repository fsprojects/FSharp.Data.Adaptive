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


[<Test>]
let ``[AVal] callbacks``() =
    let f = cval true

    let a = cval 10
    let b = cval 5

    let b' = b :> aval<_>
    let a' = a |> AVal.map id  |> AVal.map id  |> AVal.map id 
    let result = f |> AVal.bind (function true -> b' | false -> a')

    let mutable wasrun = false
    let mutable expected = 5
    use sub = result.AddCallback(fun v -> wasrun <- true; v |> should equal expected)

    wasrun |> should equal true
    wasrun <- false

    let change (action : unit -> Option<int>) =
        let shouldRun = 
            transact (fun () ->
                wasrun <- false
                let e = action()
                match e with
                | Some e -> expected <- e; true
                | None -> (); false
            )
        wasrun |> should equal shouldRun
        wasrun <- false

    change(fun () -> f.Value <- false; Some 10)
    change(fun () -> a.Value <- 7; Some 7)
    change(fun () -> b.Value <- 123; None)
    change(fun () -> f.Value <- true; Some 123)

    sub.Dispose()
    change(fun () -> b.Value <- 321; None)

    ()


[<Test>]
let ``[CSet] no transaction add``() =
    let set = cset [1;2;3;4]

    set.Add(5) |> ignore
    set.Remove(1) |> ignore

    set.Value |> ignore
    set.Add(10) |> ignore

[<Test>]
let ``[CSet] no transaction remove``() =  
    let set = cset [1;2;3;4]

    set.Remove(1) |> ignore
    set.Add(5) |> ignore

    set.Value |> ignore
    set.Remove(2) |> ignore


[<Test>]
let ``[CList] no transaction append``() =
    let list = clist [1;2;3;4]

    list.Append(5) |> ignore
    list.RemoveAt(0) |> ignore

    list |> AList.force |> ignore
    list.Append(10) |> ignore

[<Test>]
let ``[CList] no transaction remove``() =
    let list = clist [1;2;3;4]

    list.RemoveAt(0) |> ignore
    list.Append(5) |> ignore

    list |> AList.force |> ignore
    list.RemoveAt(0) |> ignore


[<Test>]
let ``[CVal] no transaction change``() =
    let cval = cval 5

    cval.Value <- 1

    cval |> AVal.force |> ignore
    cval.Value <- 2