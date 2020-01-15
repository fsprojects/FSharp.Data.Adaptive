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
let logh4 fmt =
    fmt |> Printf.kprintf (fun str ->
        let h3 = document.createElement("h4")
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
    
    use __ = reader.AddMarkingCallback(fun () -> log "reader marked")

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


    logh3 "ASet unionMany"
    let a = cset [1;2;3]
    let b = cset [2;3;4]

    let set = cset [(a :> aset<_>); (b :> aset<_>)]
    let dependent = ASet.unionMany set
    let reader = dependent.GetReader()

    logh4 "initial"
    log "%A -> %A" (List.sort <| Seq.toList (reader.GetChanges AdaptiveToken.Top)) (List.sort <| Seq.toList reader.State)
    
    logh4 "Add 5"
    transact (fun () -> b.Add 5) |> ignore
    log "%A -> %A" (List.sort <| Seq.toList (reader.GetChanges AdaptiveToken.Top)) (List.sort <| Seq.toList reader.State)
    
    let c = cset [8;9]
    logh4 "Add [8;9]"
    transact (fun () -> set.Add (c :> aset<_>)) |> ignore
    log "%A -> %A" (List.sort <| Seq.toList (reader.GetChanges AdaptiveToken.Top)) (List.sort <| Seq.toList reader.State)
    
    logh4 "Rem [8;9]"
    transact (fun () -> set.Remove (c :> aset<_>) |> ignore; c.Add 10 |> ignore)
    log "%A -> %A" (List.sort <| Seq.toList (reader.GetChanges AdaptiveToken.Top)) (List.sort <| Seq.toList reader.State)
    
    let f = AVal.init 1
    
    logh3 "ASet mapA"
    let overkill = dependent |> ASet.mapA (fun v -> f |> AVal.map (fun f -> f * v))
    let reader = overkill.GetReader()
    
    logh4 "initial"
    log "%A -> %A" (List.sort <| Seq.toList (reader.GetChanges AdaptiveToken.Top)) (List.sort <| Seq.toList reader.State)
    
    transact (fun () -> f.Value <- 2)
    
    logh4 "f = 2"
    log "%A -> %A" (List.sort <| Seq.toList (reader.GetChanges AdaptiveToken.Top)) (List.sort <| Seq.toList reader.State)
    


    let dict = DefaultDictionary.create<obj, int>()

    let a = AVal.constant 1
    let b = AVal.constant 1

    log "%A" (DefaultEquality.equals a b)

    dict.[a] <- 1
    dict.[b] <- 2

    log "%A" (dict.TryGetValue a)




[<EntryPoint>]
let main argv =
    document.addEventListener("readystatechange", fun _ ->
        if document.readyState = "complete" then
            example()
    )

    0
