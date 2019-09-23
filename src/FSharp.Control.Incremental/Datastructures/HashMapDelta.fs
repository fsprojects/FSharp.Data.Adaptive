namespace FSharp.Control.Incremental

open System.Collections
open System.Collections.Generic
open FSharp.Control.Incremental

/// represents the difference of two HashMaps.
type HashMapDelta<'k, 'v>(store : HashMap<'k, ElementOperation<'v>>) =
    static let empty = HashMapDelta<'k, 'v>(HashMap.empty)

    /// the internal store used by the HashMapDelta.
    member internal x.Store = store

    /// the empty map.
    static member Empty = 
        empty

    /// combines two DHashMaps to one.
    member x.Combine(other : HashMapDelta<'k, 'v>) =
        HashMapDelta (HashMap.union store other.Store)

    interface IEnumerable with
        member x.GetEnumerator() = (store :> IEnumerable).GetEnumerator()

    interface IEnumerable<'k * ElementOperation<'v>> with
        member x.GetEnumerator() = (store :> seq<_>).GetEnumerator()

/// functional operators for HashMapDelta.
module HashMapDelta =
    /// the empty map.
    [<GeneralizableValue>]
    let empty<'k, 'v> = HashMapDelta<'k, 'v>.Empty

    /// combines two DHashMaps to one.
    let inline combine (l : HashMapDelta<'k, 'v>) (r : HashMapDelta<'k, 'v>) = l.Combine r

