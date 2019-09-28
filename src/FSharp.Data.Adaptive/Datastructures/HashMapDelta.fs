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

    let single (key : 'K) (value: ElementOperation<'Value>) =
        HashMapDelta(HashMap.single key value)

    let ofHashMap (elements : HashMap<'K, ElementOperation<'V>>) =
        HashMapDelta elements

    let ofSeq (elements : seq<'K * ElementOperation<'V>>) = 
        HashMapDelta(HashMap.ofSeq elements)

    let ofList (elements : list<'K * ElementOperation<'V>>) = 
        HashMapDelta(HashMap.ofList elements)

    let ofArray (elements : array<'K * ElementOperation<'V>>) = 
        HashMapDelta(HashMap.ofArray elements)

    let toSeq (map : HashMapDelta<'K, 'V>) = 
        map.Store :> seq<_>

    let toList (map : HashMapDelta<'K, 'V>) = 
        map.Store |> HashMap.toList

    let toArray (map : HashMapDelta<'K, 'V>) = 
        map.Store |> HashMap.toArray

    let toHashMap (map : HashMapDelta<'K, 'V>) =
        map.Store

    /// Combines two DHashMaps to one.
    let inline combine (l : HashMapDelta<'K, 'V>) (r : HashMapDelta<'K, 'V>) = l.Combine r

