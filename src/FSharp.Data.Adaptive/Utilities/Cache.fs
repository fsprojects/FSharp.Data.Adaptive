﻿namespace FSharp.Data.Adaptive

open System.Collections.Generic


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
        fun (o : 'T1) -> isNull (o :> obj)
        #else
        if typeof<'T1>.IsValueType then fun (_o : 'T1) -> false
        else fun (o : 'T1) -> isNull (o :> obj)
        #endif 

    /// cache for non-null values.
    let cache = DefaultDictionary.create<'T1, struct('T2 * ref<int>)>()

    /// cache for null values (needed for option, unit, etc.)
    let mutable nullCache : ValueOption<struct(_ * ref<_>)> = ValueNone

    /// Clear removes all entries from the Cache and
    /// executes a function for all removed cache entries.
    /// This function is helpful if the contained values
    /// are (for example) disposable resources.
    member x.Clear(remove : 'T2 -> unit) =
        for (KeyValue(_,(v,_))) in cache do 
            remove v
        cache.Clear()
        match nullCache with
            | ValueSome (v,_) -> 
                remove v
                nullCache <- ValueNone
            | ValueNone -> ()

    /// Invoke returns the function value associated
    /// With the given argument (possibly executing the function)
    /// And increases the associated reference count.
    member x.Invoke (v : 'T1) =
        if isNull v then
            match nullCache with
                | ValueSome (r, ref) -> 
                    ref.Value <- ref.Value + 1
                    r
                | ValueNone ->
                    let r = mapping v
                    nullCache <- ValueSome(r, ref 1)
                    r
        else
            match cache.TryGetValue v with
                | (true, (r, ref)) -> 
                    ref.Value <- ref.Value + 1
                    r
                | _ ->
                    let r = mapping v
                    cache.[v] <- (r, ref 1)
                    r
    /// Revoke returns the function value associated
    /// With the given argument and decreases its reference count.
    member x.RevokeAndGetDeleted (v : 'T1) =
        if isNull v then
            match nullCache with
                | ValueSome (r, ref) -> 
                    ref.Value <- ref.Value - 1
                    if ref.Value = 0 then
                        nullCache <- ValueNone
                        struct(true, r)
                    else
                        struct(false, r)
                | ValueNone -> failwithf "cannot revoke null"
        else
            match cache.TryGetValue v with
                | (true, (r, ref)) -> 
                    ref.Value <- ref.Value - 1
                    if ref.Value = 0 then
                        cache.Remove v |> ignore
                        struct(true, r)
                    else
                        struct(false, r)
                | _ -> failwithf "cannot revoke unknown value: %A" v
                
    /// Revoke returns the function value associated
    /// With the given argument and decreases its reference count.
    member x.RevokeAndGetDeletedTotal (v : 'T1) =
        if isNull v then
            match nullCache with
                | ValueSome (r, ref) -> 
                    ref.Value <- ref.Value - 1
                    if ref.Value = 0 then
                        nullCache <- ValueNone
                        ValueSome (true, r)
                    else
                        ValueSome(false, r)
                | ValueNone -> 
                    ValueNone
        else
            match cache.TryGetValue v with
                | (true, (r, ref)) -> 
                    ref.Value <- ref.Value - 1
                    if ref.Value = 0 then
                        cache.Remove v |> ignore
                        ValueSome(true, r)
                    else
                        ValueSome(false, r)
                | _ -> 
                    ValueNone

    /// Revoke the value and return its associated cache value.
    member x.Revoke (v : 'T1) =
        x.RevokeAndGetDeleted v |> (fun struct(_,b) -> b)

    /// Enumerate over all cache values.
    member x.Values = 
        cache.Values |> Seq.map (fun struct(a,_) -> a)
