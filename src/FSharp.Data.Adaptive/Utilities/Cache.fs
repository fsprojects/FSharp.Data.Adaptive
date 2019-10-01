namespace FSharp.Data.Adaptive

open System.Collections.Generic

/// Internal datastructure used for 'magically' making types equatable.
/// Workaround for `when 'T : equality`
[<Struct; CustomEquality; NoComparison>]
type internal UEq<'T> =
    val mutable public Value : 'T
    new(value : 'T) = { Value = value }

    override x.GetHashCode() = 
        Unchecked.hash x.Value
    override x.Equals o =
        #if FABLE_COMPILER
        let o = unbox<UEq<'T>> o
        Unchecked.equals x.Value o.Value
        #else
        match o with
        | :? UEq<'T> as o -> Unchecked.equals x.Value o.Value
        | _ -> false
        #endif
/// Cache represents a cached function which can be 
/// invoked and revoked. invoke increments the reference
/// count for a specific argument (possibly causing the 
/// function to be executed) whereas revoke decreases the
/// eeference count and removes the cache entry whenever
/// the reference count is 0.
type internal Cache<'T1, 'T2>(mapping : 'T1 -> 'T2) =  
    /// utility checking for null (if possible)
    static let isNull =
        #if FABLE_COMPILER
        fun (o : 'T1) -> not (unbox<bool> o)
        #else
        if typeof<'T1>.IsValueType then fun (_o : 'T1) -> false
        else fun (o : 'T1) -> isNull (o :> obj)
        #endif 

    /// cache for non-null values.
    let cache = Dictionary<UEq<'T1>, 'T2 * ref<int>>(1)

    /// cache for null values (needed for option, unit, etc.)
    let mutable nullCache = None

    /// Clear removes all entries from the Cache and
    /// executes a function for all removed cache entries.
    /// This function is helpful if the contained values
    /// are (for example) disposable resources.
    member x.Clear(remove : 'T2 -> unit) =
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
    member x.Invoke (v : 'T1) =
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
    member x.RevokeAndGetDeleted (v : 'T1) =
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
    member x.RevokeAndGetDeletedTotal (v : 'T1) =
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
    member x.Revoke (v : 'T1) =
        x.RevokeAndGetDeleted v |> snd

    /// Enumerate over all cache values.
    member x.Values = 
        cache.Values |> Seq.map fst
