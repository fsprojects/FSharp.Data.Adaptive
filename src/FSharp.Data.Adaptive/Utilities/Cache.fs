namespace FSharp.Data.Adaptive

open System.Collections.Generic

[<Struct; CustomEquality; NoComparison>]
type internal UEq<'T> =
    val mutable public Value : 'T
    new(value : 'T) = { Value = value }

    override x.GetHashCode() = 
        Unchecked.hash x.Value
    override x.Equals o =
        match o with
        | :? UEq<'T> as o -> Unchecked.equals x.Value o.Value
        | _ -> false

/// Cache represents a cached function which can be 
/// invoked and revoked. invoke increments the reference
/// count for a specific argument (possibly causing the 
/// function to be executed) whereas revoke decreases the
/// eeference count and removes the cache entry whenever
/// the reference count is 0.
type internal Cache<'A, 'B>(mapping : 'A -> 'B) =  
    static let isNull =
        if typeof<'A>.IsValueType then fun (_o : 'A) -> false
        else fun (o : 'A) -> isNull (o :> obj)

    let cache = Dictionary<UEq<'A>, 'B * ref<int>>(1)
    let mutable nullCache = None

    /// Clear removes all entries from the Cache and
    /// executes a function for all removed cache entries.
    /// This function is helpful if the contained values
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

    /// Invoke returns the function value associated
    /// With the given argument (possibly executing the function)
    /// And increases the associated reference count.
    member x.Invoke (v : 'A) =
        if isNull v then
            match nullCache with
                | Some (r, ref) -> 
                    ref := !ref + 1
                    r
                | None ->
                    let r = mapping v
                    nullCache <- Some(r, ref 1)
                    r
        else
            match cache.TryGetValue (UEq v) with
                | (true, (r, ref)) -> 
                    ref := !ref + 1
                    r
                | _ ->
                    let r = mapping v
                    cache.[UEq v] <- (r, ref 1)
                    r
    /// Revoke returns the function value associated
    /// With the given argument and decreases its reference count.
    member x.RevokeAndGetDeleted (v : 'A) =
        if isNull v then
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
            match cache.TryGetValue (UEq v) with
                | (true, (r, ref)) -> 
                    ref := !ref - 1
                    if !ref = 0 then
                        cache.Remove (UEq v) |> ignore
                        (true, r)
                    else
                        (false, r)
                | _ -> failwithf "cannot revoke unknown value: %A" v
                
    /// Revoke returns the function value associated
    /// With the given argument and decreases its reference count.
    member x.RevokeAndGetDeletedTotal (v : 'A) =
        if isNull v then
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
            match cache.TryGetValue (UEq v) with
                | (true, (r, ref)) -> 
                    ref := !ref - 1
                    if !ref = 0 then
                        cache.Remove (UEq v) |> ignore
                        Some(true, r)
                    else
                        Some(false, r)
                | _ -> 
                    None

    /// Revoke the value and return its associated cache value.
    member x.Revoke (v : 'A) =
        x.RevokeAndGetDeleted v |> snd

    /// Enumerate over all cache values.
    member x.Values = 
        cache.Values |> Seq.map fst
