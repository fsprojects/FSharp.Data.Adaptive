module Program

open FSharp.Data.Adaptive
open Browser

let logh1 fmt =
    fmt |> Printf.kprintf (fun str ->
        let h3 = document.createElement("h1")
        h3.innerText <- str
        document.body.appendChild h3 |> ignore
    )
let logh2 fmt =
    fmt |> Printf.kprintf (fun str ->
        let h3 = document.createElement("h2")
        h3.innerText <- str
        document.body.appendChild h3 |> ignore
    )
let logh3 fmt =
    fmt |> Printf.kprintf (fun str ->
        let h3 = document.createElement("h3")
        h3.innerText <- str
        document.body.appendChild h3 |> ignore
    )

let log fmt =
    fmt |> Printf.kprintf (fun str ->
        let pre = document.createElement("pre")
        let code = document.createElement("code")
        code.innerText <- str
        pre.appendChild code |> ignore
        document.body.appendChild pre |> ignore
    )

let example() =
    logh2 "FSharp.Data.Adaptive on Fable"

    logh3 "AVal"
    let v = cval 20
    let d = v |> AVal.map (fun v -> v * 20)
    log "%A => %A" v.Value (AVal.force d)

    transact (fun () -> v.Value <- 100)
    log "%A => %A" v.Value (AVal.force d)

    transact (fun () -> v.Value <- 200)
    log "%A => %A" v.Value (AVal.force d)
    
    logh3 "ASet"
    let set = cset [1;2;3]
    let dependent = set |> ASet.map (fun v -> v * 2)
    let reader = dependent.GetReader()

    log "%A -> %A" (Seq.toList (reader.GetChanges AdaptiveToken.Top)) (Seq.toList reader.State)

    transact (fun () -> set.Add 4) |> ignore
    log "%A -> %A" (Seq.toList (reader.GetChanges AdaptiveToken.Top)) (Seq.toList reader.State)

    transact (fun () -> set.Value <- HashSet.ofList [5]) |> ignore
    log "%A -> %A" (Seq.toList (reader.GetChanges AdaptiveToken.Top)) (Seq.toList reader.State)
    
    logh3 "AMap"
    let map = cmap [1, "one"; 2, "two"]
    let dependent = map |> AMap.map (fun k v -> sprintf "'%s'" v)
    let reader = dependent.GetReader()
    
    log "%A -> %A" (Seq.toList (reader.GetChanges AdaptiveToken.Top)) (Seq.toList reader.State)
    
    transact (fun () -> map.[3] <- "three") |> ignore
    log "%A -> %A" (Seq.toList (reader.GetChanges AdaptiveToken.Top)) (Seq.toList reader.State)
    
    transact (fun () -> map.[2] <- "twa") |> ignore
    log "%A -> %A" (Seq.toList (reader.GetChanges AdaptiveToken.Top)) (Seq.toList reader.State)
    
    transact (fun () -> map.Remove 1) |> ignore
    log "%A -> %A" (Seq.toList (reader.GetChanges AdaptiveToken.Top)) (Seq.toList reader.State)

    logh3 "AList"
    let list = clist [1;2;3]
    let dependent = list |> AList.map (fun v -> v * 2)
    let reader = dependent.GetReader()
    
    log "%A -> %A" (IndexListDelta.toList (reader.GetChanges AdaptiveToken.Top)) (Seq.toList reader.State)

    transact (fun () -> list.Add 4) |> ignore
    log "%A -> %A" (IndexListDelta.toList (reader.GetChanges AdaptiveToken.Top)) (Seq.toList reader.State)

    
    transact (fun () -> list.Clear())
    log "%A -> %A" (IndexListDelta.toList (reader.GetChanges AdaptiveToken.Top)) (Seq.toList reader.State)



[<EntryPoint>]
let main argv =
    document.addEventListener("readystatechange", fun _ ->
        if document.readyState = "complete" then
            example()
    )

    0
