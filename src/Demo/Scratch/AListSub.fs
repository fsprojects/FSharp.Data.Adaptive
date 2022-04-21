module AListSub

open System.Collections.Generic
open FSharp.Data.Adaptive
open FSharp.Data.Traceable

let run() =
    let input = clist ["a"; "b"; "c"; "d"; "e"]


    let range =
        input 
        |> AList.sortDescending
        |> AList.sub 1 3
    
    let reader = range.GetReader()

    let check() =

        let rec skip (n : int) (l : list<'a>) =
            if n <= 0 then l
            else 
                match l with
                | [] -> []
                | _ :: r -> skip (n - 1) r


        reader.GetChanges AdaptiveToken.Top |> ignore
        let real = reader.State |> IndexList.toList
        let test = input.Value |> IndexList.toList |> List.sortDescending |> skip 1 |> List.truncate 3

        if real <> test then
            failwithf "bad: %0A vs %0A" real test
        else
            printfn "OK: %0A" test
    check()

    transact (fun () ->
        input.Prepend "x" |> ignore
        input.Prepend "y" |> ignore
        input.Prepend "z" |> ignore
        input.Prepend "w" |> ignore
        input.Prepend "r" |> ignore
    )
    check()

    transact (fun () ->
        input.Clear()
    )
    check()

    transact (fun () ->
        input.UpdateTo ["hans"; "sepp"; "hugo"; "franz"] |> ignore
    )
    check()

    transact (fun () ->
        input.Append "zitha" |> ignore
    )
    check()

    transact (fun () ->
        input.RemoveAt 0 |> ignore
        input.RemoveAt 0 |> ignore
        input.RemoveAt 0 |> ignore
        input.RemoveAt (input.Count - 1) |> ignore
    )
    check()

    transact (fun () ->
        input.Clear()
    )
    check()

    let rand = System.Random()

    for i in 1 .. 10000 do
        transact (fun () ->
            let values =
                List.init (rand.Next 5) (fun _ -> rand.Next() |> string)

            match rand.Next 3 with
            | 0 when not (List.isEmpty values) ->
                match rand.Next 3 with
                | 0 ->
                    printf "append "
                    for v in values do input.Append v |> ignore
                | 1 -> 
                    printf "prepend "
                    for v in values do input.Prepend v |> ignore
                | _ ->
                    printf "insert "
                    for v in values do input.InsertAt (rand.Next (input.Count + 1), v) |> ignore
            | 1 when input.Count > 0 ->
                printf "remove "
                let c = rand.Next(input.Count) + 1
                for i in 1 .. c do
                    input.RemoveAt (rand.Next input.Count) |> ignore
            | _ ->
                printf "update "
                input.UpdateTo values |> ignore
        )
        check()


    ()
