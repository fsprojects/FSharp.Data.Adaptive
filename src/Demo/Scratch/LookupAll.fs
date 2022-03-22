module LookupAll
open FSharp.Data.Adaptive

// a sketch for n-ary lookups in `amap` which might be useful for database-like scenarios
// where you're often confronted with a *linking-table* of some kind.

// links : asist<(<ai>, <bi>)>:
// (0, 1)
// (0, 2)

// a : amap< <ai>, ainfo>
// | 0 | ... a0info ... |
// ...

// b : amap< <ai>, ainfo>
// | 0 | ... a0info ... |
// ...

// (links, a) ||> AList.lookupAll (fun (ai,_) -> ai) (fun (_ai, bi))


module AList =
    open FSharp.Data.Traceable
    type LookupReader<'a, 'b, 'c, 'd>(key : 'a -> 'b, mapping : 'a -> 'b -> voption<'c> -> voption<'d>, values : alist<'a>, table : amap<'b, 'c>) =
        inherit AbstractReader<IndexList<'d>, IndexListDelta<'d>>(IndexList.trace)

        let vReader = values.GetReader()
        let tReader = table.GetReader()

        let mutable indexCache : HashMap<Index, 'a * 'b> = HashMap.empty
        let mutable affected : HashMap<'b, Set<Index>> = HashMap.empty

        override x.Compute(token : AdaptiveToken) : IndexListDelta<'d> =
            let oldState = x.State
            let vops = vReader.GetChanges token
            let tops = tReader.GetChanges token

            let mutable delta =
                vops |> IndexListDelta.choose (fun index op ->
                    match op with
                    | Set value ->  
                        match HashMap.tryFind index indexCache with
                        | Some (_, ok) ->
                            affected <-
                                affected |> HashMap.alter ok (fun old ->
                                    match old with
                                    | Some old ->
                                        let res = Set.remove index old
                                        if res.IsEmpty then None
                                        else Some res
                                    | None ->   
                                        None
                                )
                        | None ->
                            ()

                        let k = key value
                        indexCache <- HashMap.add index (value, k) indexCache
                        affected <- affected |> HashMap.alter k (function Some set -> Some (Set.add index set) | None -> Some (Set.singleton index))
                        let tv = HashMap.tryFindV k tReader.State
                        match mapping value k tv with
                        | ValueSome result ->   
                            Some (Set result)
                        | ValueNone ->
                            match IndexList.tryGet index oldState with
                            | Some _ -> Some Remove
                            | None -> None
                    | Remove ->
                        match HashMap.tryRemove index indexCache with
                        | Some ((_value, k), rest) ->
                            indexCache <- rest
                            affected <-
                                affected |> HashMap.alter k (fun old ->
                                    match old with
                                    | Some old ->
                                        let res = Set.remove index old
                                        if res.IsEmpty then None
                                        else Some res
                                    | None ->   
                                        None
                                )
                            Some Remove

                        | None ->
                            None
                )


            for key, op in tops do
                match HashMap.tryFind key affected with
                | Some indices ->
                    match op with
                    | Remove -> 
                        for index in indices do
                            match IndexList.tryGet index vReader.State with
                            | Some value ->
                                match mapping value key ValueNone with
                                | ValueSome res ->
                                    delta <- delta |> IndexListDelta.add index (Set res)
                                | ValueNone ->
                                    match IndexList.tryGet index oldState with
                                    | Some _ -> delta <- delta |> IndexListDelta.add index Remove
                                    | None -> ()
                            | None ->   
                                match IndexList.tryGet index oldState with
                                | Some _ -> delta <- delta |> IndexListDelta.add index Remove
                                | None -> ()


                    | Set newTableValue ->
                        for index in indices do
                            match IndexList.tryGet index vReader.State with
                            | Some value ->
                                match mapping value key (ValueSome newTableValue) with
                                | ValueSome res ->
                                    delta <- delta |> IndexListDelta.add index (Set res)
                                | ValueNone ->
                                    match IndexList.tryGet index oldState with
                                    | Some _ -> delta <- delta |> IndexListDelta.add index Remove
                                    | None -> ()
                            | None -> 
                                match IndexList.tryGet index oldState with
                                | Some _ -> delta <- delta |> IndexListDelta.add index Remove
                                | None -> ()
                | None ->
                    () // OK


            delta

    let lookupAll (key : 'a -> 'b) (mapping : 'a -> 'b -> voption<'c> -> voption<'d>) (values : alist<'a>) (table : amap<'b, 'c>) : alist<'d> =
        AList.ofReader <| fun () ->
            LookupReader(key, mapping, values, table)

    let lookupAll2 (ka : 'a -> 'ka) (kb : 'a -> 'kb) (values : alist<'a>) (tableA : amap<'ka, 'va>) (tableB : amap<'kb, 'vb>) : alist<'a * 'va * 'vb> =
        let withA =
            (values, tableA) ||> lookupAll ka (fun a _ka va ->
                match va with
                | ValueSome va ->
                    ValueSome (a, va)
                | ValueNone ->
                    ValueNone
            )
        let withB =
            (withA, tableB) ||> lookupAll (fun (a, _) -> kb a) (fun (a, va) _kb vb ->
                match vb with
                | ValueSome vb ->
                    ValueSome (a, va, vb)
                | ValueNone ->
                    ValueNone
            )
        withB

type Person =
    {
        Name        : string
        Surname     : string
    }

type Country =
    {
        Name        : string
        Population  : int
    }

open System

let example() =
    // a very typical person-table
    let persons =
        cmap [
            0, { Name = "John"; Surname = "Smith" }
            1, { Name = "Alicia"; Surname = "Suarez" }
            2, { Name = "Johanna"; Surname = "Maier" }
        ]

    // and a table for countries with population
    let countries =
        cmap [ // thanks to github-copilot ;)
            0, { Name = "Germany"; Population = 81726000 }
            1, { Name = "France"; Population = 65447374 }
            2, { Name = "United Kingdom"; Population = 6449600 }
            3, { Name = "United States"; Population = 327167434 }
            4, { Name = "China"; Population = 1344130000 }
            5, { Name = "India"; Population = 1344130000 }
            6, { Name = "Brazil"; Population = 209469000 }
            7, { Name = "Russia"; Population = 143989000 }
            8, { Name = "Canada"; Population = 35151728 }
            9, { Name = "Australia"; Population = 25475400 }
            10, { Name = "Mexico"; Population = 112336538 }
            11, { Name = "Argentina"; Population = 43590400 }
        ]

    // a table containing visit per person to each country with a date
    let visited =
        clist [
            0, 0, DateTime(2018, 7, 3)
            0, 2, DateTime(2019, 6, 3)
            1, 5, DateTime(2018, 7, 2)
            2, 9, DateTime(2020, 3, 4)
        ]

    // who visited which country when?
    let result =
        (visited, persons, countries) 
            |||> AList.lookupAll2 
                (fun (personId,_,_) -> personId)   // use the personId as key for persons
                (fun (_,countryId,_) -> countryId) // use the countryId as key for persons

            // finally throw away the ids and just keep the relevant data
            |> AList.map (fun ((_,_,date), va, vb) -> va, vb, date)

    let reader = result.GetReader()

    let datestr (d : DateTime) = sprintf "%04d-%02d-%02d" d.Year d.Month d.Day

    let print() =
        let oldState = reader.State
        let changes = reader.GetChanges AdaptiveToken.Top

        for index, op in changes do
            match op with
            | Set (person, country, date) ->
                match IndexList.tryGet index oldState with
                | Some(op, oc, od) ->
                    printfn "  changed %s %s's visit to %s on %s" op.Name op.Surname oc.Name (datestr od)
                    printfn "    => %s %s visited %s on %s" person.Name person.Surname country.Name (datestr date)
                | None ->
                    printfn "  %s %s added a visit to %s on %s" person.Name person.Surname country.Name (datestr date)
            | Remove ->
                match IndexList.tryGet index oldState with
                | Some(person, country, date) ->
                    printfn "  deleted %s %s's visit to %s on %s" person.Name person.Surname country.Name (datestr date)
                | None ->
                    ()
    
    printfn "initial"
    print()

    transact (fun () -> visited.Append (2,11, DateTime(2022, 1, 1)) |> ignore)
    printfn "links.Append (2,11,2022-01-01)"
    print()

    transact (fun () -> persons.[2] <- { Name = "Johanna"; Surname = "Huber" })
    printfn "Johanna Maier -> Johanna Huber"
    print()

    printfn "final"
    for (person, country, date) in reader.State do
        printfn "  %s %s visited %s on %s" person.Name person.Surname country.Name (datestr date)


let run() =
    let l = clist [1;2;3;4;1;2;5]

    let table =
        cmap [
            1L, "one"
            2L, "two"
            3L, "three"
            4L, "four"
        ]

    let test = 
        (l, table) ||> AList.lookupAll int64 (fun (a : int) (b : int64) (c : voption<string>) -> 
            match c with
            | ValueSome c -> ValueSome (a, c)
            | ValueNone -> ValueNone
        )

    let reader = test.GetReader()


    let print() =
        reader.GetChanges AdaptiveToken.Top |> ignore
        reader.State |> IndexList.toList |> printfn "  %0A"

    printfn "initial"
    print()

    printfn "remove(3)"
    transact (fun () -> table.Remove 3L |> ignore)
    print()

    printfn "five"
    transact (fun () -> table.[5L] <- "five")
    print()

    printfn "insert(0)"
    transact(fun () -> l.InsertAt(0, 0) |> ignore)
    print()

    printfn "three"
    transact (fun () -> table.[3L] <- "three")
    print()

    printfn "zero"
    transact (fun () -> table.[0L] <- "zero")
    print()

    printfn "append(0)"
    transact(fun () -> l.Append(0) |> ignore)
    print()

    printfn "zero -> null"
    transact (fun () -> table.[0L] <- "null")
    print()



    //Observable.run()