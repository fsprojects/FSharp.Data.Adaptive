namespace FSharp.Data.Adaptive

open System.Collections.Generic


/// Cache represents a cached function which can be 
/// invoked and revoked. invoke increments the reference
/// count for a specific argument (possibly causing the 
/// function to be executed) whereas revoke decreases the
/// reference count and removes the cache entry whenever
/// the reference count is 0.
type internal Cache<'A, 'B>(mapping : 'A -> 'B) =  
    let cache = Dictionary<obj, 'B * ref<int>>(1)
    let mutable nullCache = None

    /// Clear removes all entries from the Cache and
    /// executes a function for all removed cache entries.
    /// this function is helpful if the contained values
    /// are (for example) disposable resources.
    member x.Clear(remove : 'B -> unit) =
        for (KeyValue(_,(v,_))) in cache do 
            remove v
        cache.Clear()
        match nullCache with
            | Some (v,_) -> 
                remove v
                nullCache <- None
            | None -> ()

    /// invoke returns the function value associated
    /// with the given argument (possibly executing the function)
    /// and increases the associated reference count.
    member x.Invoke (v : 'A) =
        if isNull (v :> obj) then
            match nullCache with
                | Some (r, ref) -> 
                    ref := !ref + 1
                    r
                | None ->
                    let r = mapping v
                    nullCache <- Some(r, ref 1)
                    r
        else
            match cache.TryGetValue v with
                | (true, (r, ref)) -> 
                    ref := !ref + 1
                    r
                | _ ->
                    let r = mapping v
                    cache.[v] <- (r, ref 1)
                    r
    /// revoke returns the function value associated
    /// with the given argument and decreases its reference count.
    member x.RevokeAndGetDeleted (v : 'A) =
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
                
    /// revoke returns the function value associated
    /// with the given argument and decreases its reference count.
    member x.RevokeAndGetDeletedTotal (v : 'A) =
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

    /// revoke the value and return its associated cache value.
    member x.Revoke (v : 'A) =
        x.RevokeAndGetDeleted v |> snd

    /// enumerate over all cache values.
    member x.Values = 
        cache.Values |> Seq.map fst
