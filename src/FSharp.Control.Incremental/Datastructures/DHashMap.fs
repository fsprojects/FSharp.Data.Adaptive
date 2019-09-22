namespace FSharp.Control.Incremental

// TODO: documentation

open System
open System.Collections
open System.Collections.Generic
open FSharp.Control.Incremental

type DHashMap<'k, 'v>(store : HashMap<'k, ElementOperation<'v>>) =
    static let empty = DHashMap<'k, 'v>(HashMap.empty)
    
    member internal x.Store = store

    static member Empty = 
        empty

    static member Single (key : 'k, op : ElementOperation<'v>) = 
        DHashMap(HashMap.single key op)
        
    static member OfSeq (ops : seq<'k * ElementOperation<'v>>) = 
        DHashMap(HashMap.ofSeq ops)

    static member OfList (ops : list<'k * ElementOperation<'v>>) = 
        DHashMap(HashMap.ofList ops)
        
    static member OfArray (ops : array<'k * ElementOperation<'v>>) = 
        DHashMap(HashMap.ofArray ops)

    static member OfHashMap (ops : HashMap<'k, ElementOperation<'v>>) = 
        DHashMap(ops)


    member x.ToSeq() = store |> HashMap.toSeq
    member x.ToList() = store |> HashMap.toList
    member x.ToArray() = store |> HashMap.toArray
    member x.ToHashMap() = store

    member x.IsEmpty = store.IsEmpty
    member x.Count = store.Count

    member x.TryFind(key : 'k) = HashMap.tryFind key store
    member x.ContainsKey(key : 'k) = HashMap.containsKey key store


    member x.Combine(other : DHashMap<'k, 'v>) =
        DHashMap (HashMap.union store other.Store)

    interface IEnumerable with
        member x.GetEnumerator() = (store :> IEnumerable).GetEnumerator()

    interface IEnumerable<'k * ElementOperation<'v>> with
        member x.GetEnumerator() = (store :> seq<_>).GetEnumerator()

module DHashMap =
    [<GeneralizableValue>]
    let empty<'k, 'v> = DHashMap<'k, 'v>.Empty

    let inline single (key : 'k) (op : ElementOperation<'v>) = DHashMap.Single(key, op)
    let inline ofSeq (elements : seq<'k * ElementOperation<'v>>) = DHashMap.OfSeq elements
    let inline ofList (elements : list<'k * ElementOperation<'v>>) = DHashMap.OfList elements
    let inline ofArray (elements : array<'k * ElementOperation<'v>>) = DHashMap.OfArray elements
    let inline ofHashMap (elements : HashMap<'k, ElementOperation<'v>>) = DHashMap.OfHashMap elements
        
    let inline toSeq (map : DHashMap<'k, 'v>) = map.ToSeq()
    let inline toList (map : DHashMap<'k, 'v>) = map.ToList()
    let inline toArray (map : DHashMap<'k, 'v>) = map.ToArray()
    let inline toHashMap (map : DHashMap<'k, 'v>) = map.ToHashMap()

    let inline combine (l : DHashMap<'k, 'v>) (r : DHashMap<'k, 'v>) = l.Combine r

