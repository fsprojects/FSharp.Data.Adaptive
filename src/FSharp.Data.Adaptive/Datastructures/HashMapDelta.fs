namespace FSharp.Data.Adaptive

open System.Collections
open System.Collections.Generic
open FSharp.Data.Adaptive

/// Represents the difference of two HashMaps.
type HashMapDelta<'K, 'V>(store : HashMap<'K, ElementOperation<'V>>) =
    static let empty = HashMapDelta<'K, 'V>(HashMap.empty)

    /// The internal store used by the HashMapDelta.
    member internal x.Store = store

    /// The empty map.
    static member Empty = 
        empty

    /// Combines two DHashMaps to one.
    member x.Combine(other : HashMapDelta<'K, 'V>) =
        HashMapDelta (HashMap.union store other.Store)

    interface IEnumerable with
        member x.GetEnumerator() = (store :> IEnumerable).GetEnumerator()

    interface IEnumerable<'K * ElementOperation<'V>> with
        member x.GetEnumerator() = (store :> seq<_>).GetEnumerator()

/// Functional operators for HashMapDelta.
module HashMapDelta =
    /// The empty map.
    [<GeneralizableValue>]
    let empty<'K, 'V> = HashMapDelta<'K, 'V>.Empty

    /// Combines two DHashMaps to one.
    let inline combine (l : HashMapDelta<'K, 'V>) (r : HashMapDelta<'K, 'V>) = l.Combine r

