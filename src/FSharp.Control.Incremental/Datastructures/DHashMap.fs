namespace FSharp.Control.Incremental

open System.Collections
open System.Collections.Generic
open FSharp.Control.Incremental

/// represents the difference of two HashMaps.
type DHashMap<'k, 'v>(store : HashMap<'k, ElementOperation<'v>>) =
    static let empty = DHashMap<'k, 'v>(HashMap.empty)

    /// the internal store used by the DHashMap.
    member internal x.Store = store

    /// the empty map.
    static member Empty = 
        empty

    /// combines two DHashMaps to one.
    member x.Combine(other : DHashMap<'k, 'v>) =
        DHashMap (HashMap.union store other.Store)

    interface IEnumerable with
        member x.GetEnumerator() = (store :> IEnumerable).GetEnumerator()

    interface IEnumerable<'k * ElementOperation<'v>> with
        member x.GetEnumerator() = (store :> seq<_>).GetEnumerator()

/// functional operators for DHashMap.
module DHashMap =
    /// the empty map.
    [<GeneralizableValue>]
    let empty<'k, 'v> = DHashMap<'k, 'v>.Empty

    /// combines two DHashMaps to one.
    let inline combine (l : DHashMap<'k, 'v>) (r : DHashMap<'k, 'v>) = l.Combine r

