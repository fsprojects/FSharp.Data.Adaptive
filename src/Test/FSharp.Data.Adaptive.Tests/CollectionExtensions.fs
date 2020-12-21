module CollectionExtensions

open NUnit.Framework
open FsCheck
open FSharp.Data.Adaptive
open FSharp.Data.Traceable
open FsUnit
open FsCheck.NUnit
open FSharp.Data
open Generators



[<Property(EndSize = 100)>]
let ``[Seq] existsA``(values : list<bool>) =
    let cs = values |> List.map cval
    let result = Seq.existsA id (Seq.cast cs)
    let ref = List.exists id values

    result |> AVal.force |> should equal ref

    transact (fun () ->
        for c in cs do c.Value <- false
    )
    result |> AVal.force |> should be False
    
    transact (fun () ->
        for c,v in List.zip cs values do c.Value <- v
    )
    result |> AVal.force |> should equal ref


[<Test>]
let ``[Seq] existsA basic``() =
    
    let a = cval false
    let b = cval false

    let l = seq { a :> aval<_>; b :> aval<_> }

    let result = Seq.existsA id l
    result |> AVal.force |> should be False

    transact (fun () -> a.Value <- true)
    result |> AVal.force |> should be True

    transact (fun () -> b.Value <- true; a.Value <- false)
    result |> AVal.force |> should be True

    transact (fun () -> b.Value <- false)
    result |> AVal.force |> should be False

    transact (fun () -> a.Value <- true)
    result |> AVal.force |> should be True

    transact (fun () -> b.Value <- true)
    result |> AVal.force |> should be True
    
[<Test>]
let ``[Seq] forallA basic``() =
    
    let a = cval false
    let b = cval false

    let l = seq { a :> aval<_>; b :> aval<_> }

    let result = Seq.forallA id l
    result |> AVal.force |> should be False

    transact (fun () -> a.Value <- true)
    result |> AVal.force |> should be False

    transact (fun () -> b.Value <- true; a.Value <- false)
    result |> AVal.force |> should be False

    transact (fun () -> b.Value <- false)
    result |> AVal.force |> should be False

    transact (fun () -> a.Value <- true)
    result |> AVal.force |> should be False

    transact (fun () -> b.Value <- true)
    result |> AVal.force |> should be True




    

