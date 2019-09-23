namespace FSharp.Control.Incremental

open System.Collections
open System.Collections.Generic
open FSharp.Control.Incremental

/// represents the difference of two HashMaps.
type HashMapDelta<'K, 'V>(store : HashMap<'K, ElementOperation<'V>>) =
    static let empty = HashMapDelta<'K, 'V>(HashMap.empty)

    /// the internal store used by the HashMapDelta.
    member internal x.Store = store

    /// the empty map.
    static member Empty = 
        empty

    /// combines two DHashMaps to one.
    member x.Combine(other : HashMapDelta<'K, 'V>) =
        HashMapDelta (HashMap.union store other.Store)

    interface IEnumerable with
        member x.GetEnumerator() = (store :> IEnumerable).GetEnumerator()

    interface IEnumerable<'K * ElementOperation<'V>> with
        member x.GetEnumerator() = (store :> seq<_>).GetEnumerator()

/// functional operators for HashMapDelta.
module HashMapDelta =
    /// the empty map.
    [<GeneralizableValue>]
    let empty<'K, 'V> = HashMapDelta<'K, 'V>.Empty

    /// combines two DHashMaps to one.
    let inline combine (l : HashMapDelta<'K, 'V>) (r : HashMapDelta<'K, 'V>) = l.Combine r

