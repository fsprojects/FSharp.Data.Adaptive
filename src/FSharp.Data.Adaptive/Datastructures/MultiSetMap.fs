namespace FSharp.Data.Adaptive

    
/// A simple multi-map implementation.
type internal MultiSetMap<'k, 'v> = HashMap<'k, HashSet<'v>>
    
/// A simple multi-map implementation.
module internal MultiSetMap =
    [<GeneralizableValue>]
    let empty<'k, 'v> : MultiSetMap<'k, 'v> = HashMap.empty

    let add (key: 'k) (value: 'v) (m: MultiSetMap<'k, 'v>): MultiSetMap<'k, 'v> =
        m |> HashMap.alter key (fun old ->
            match old with
            | Some old -> Some (HashSet.add value old)
            | None -> Some (HashSet.single value)
        )

    let remove (key: 'k) (value: 'v) (m: MultiSetMap<'k, 'v>): bool * MultiSetMap<'k, 'v> =
        let wasLast = ref false
        let result = 
            m |> HashMap.alter key (fun old ->
                match old with
                | None -> None
                | Some old -> 
                    let s = HashSet.remove value old
                    if HashSet.isEmpty s then 
                        wasLast := true
                        None
                    else 
                        Some s
            )
        !wasLast, result

    let find (key: 'k) (m: MultiSetMap<'k, 'v>) =
        match HashMap.tryFind key m with
        | Some s -> s
        | None -> HashSet.empty
