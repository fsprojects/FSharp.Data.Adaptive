namespace FSharp.Control.Incremental

open System.Collections.Generic

/// <summary>
/// Cache represents a cached function which can be 
/// invoked and revoked. invoke increments the reference
/// count for a specific argument (possibly causing the 
/// function to be executed) whereas revoke decreases the
/// reference count and removes the cache entry whenever
/// the reference count is 0.
/// </summary>
type internal Cache<'a, 'b>(f : 'a -> 'b) =  
    let cache = Dictionary<obj, 'b * ref<int>>(1)
    let mutable nullCache = None

    /// <summary>
    /// Clear removes all entries from the Cache and
    /// executes a function for all removed cache entries.
    /// this function is helpful if the contained values
    /// are (for example) disposable resources.
    /// </summary>
    member x.Clear(remove : 'b -> unit) =
        for (KeyValue(_,(v,_))) in cache do 
            remove v
        cache.Clear()
        match nullCache with
            | Some (v,_) -> 
                remove v
                nullCache <- None
            | None -> ()

    /// <summary>
    /// invoke returns the function value associated
    /// with the given argument (possibly executing the function)
    /// and increases the associated reference count.
    /// </summary>
    member x.Invoke (v : 'a) =
        if isNull (v :> obj) then
            match nullCache with
                | Some (r, ref) -> 
                    ref := !ref + 1
                    r
                | None ->
                    let r = f v
                    nullCache <- Some(r, ref 1)
                    r
        else
            match cache.TryGetValue v with
                | (true, (r, ref)) -> 
                    ref := !ref + 1
                    r
                | _ ->
                    let r = f v
                    cache.[v] <- (r, ref 1)
                    r
    /// <summary>
    /// revoke returns the function value associated
    /// with the given argument and decreases its reference count.
    /// </summary>
    member x.RevokeAndGetDeleted (v : 'a) =
        if isNull (v :> obj) then
            match nullCache with
                | Some (r, ref) -> 
                    ref := !ref - 1
                    if !ref = 0 then
                        nullCache <- None
                        (true, r)
                    else
                        (false, r)
                | None -> failwithf "cannot revoke null"
        else
            match cache.TryGetValue v with
                | (true, (r, ref)) -> 
                    ref := !ref - 1
                    if !ref = 0 then
                        cache.Remove v |> ignore
                        (true, r)
                    else
                        (false, r)
                | _ -> failwithf "cannot revoke unknown value: %A" v

    member x.RevokeAndGetDeletedTotal (v : 'a) =
        if isNull (v :> obj) then
            match nullCache with
                | Some (r, ref) -> 
                    ref := !ref - 1
                    if !ref = 0 then
                        nullCache <- None
                        Some (true, r)
                    else
                        Some(false, r)
                | None -> 
                    None
        else
            match cache.TryGetValue v with
                | (true, (r, ref)) -> 
                    ref := !ref - 1
                    if !ref = 0 then
                        cache.Remove v |> ignore
                        Some(true, r)
                    else
                        Some(false, r)
                | _ -> 
                    None

    member x.Revoke (v : 'a) =
        x.RevokeAndGetDeleted v |> snd

    member x.Values = 
        cache.Values |> Seq.map fst
