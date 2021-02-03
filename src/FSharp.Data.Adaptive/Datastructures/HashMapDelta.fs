namespace FSharp.Data.Adaptive

open System.Collections
open System.Collections.Generic
open FSharp.Data.Adaptive

/// Represents the difference of two HashMaps.
[<Struct; CustomEquality; NoComparison>]
[<StructuredFormatDisplay("{AsString}"); CompiledName("FSharpHashMapDelta`2")>]
type HashMapDelta<'K, [<EqualityConditionalOn>] 'V>(store : HashMap<'K, ElementOperation<'V>>) =
    static let empty = HashMapDelta<'K, 'V>(HashMap.empty)

    /// The internal store used by the HashMapDelta.
    member internal x.Store = store

    /// The empty map.
    static member Empty = 
        empty

    member x.IsEmpty = store.IsEmpty
    member x.Count = store.Count

    member private x.AsString = x.ToString()

    override x.ToString() =
        let suffix =
            if store.Count > 5 then "; ..."
            else ""

        let elements = 
            store 
            |> Seq.truncate 5
            |> Seq.map (fun (k, op) ->
                match op with
                | Set v -> sprintf "[%A]<-%A" k v
                | Remove -> sprintf "Rem(%A)" k
            ) 
            |> String.concat "; "

        sprintf "HashMapDelta [%s%s]" elements suffix

    override x.GetHashCode() =
        DefaultEquality.hash store

    override x.Equals o =
        match o with
        | :? HashMapDelta<'K, 'V> as o -> DefaultEquality.equals store o.Store
        | _ -> false
    /// Combines two DHashMaps to one.
    member x.Combine(other : HashMapDelta<'K, 'V>) =
        HashMapDelta (HashMap.union store other.Store)

    member x.GetEnumerator() = store.GetEnumerator()

    interface IEnumerable with
        member x.GetEnumerator() = (store :> IEnumerable).GetEnumerator()

    interface IEnumerable<'K * ElementOperation<'V>> with
        member x.GetEnumerator() = (store :> seq<_>).GetEnumerator()


/// Functional operators for HashMapDelta.
[<CompiledName("FSharpHashMapDeltaModule")>]
module HashMapDelta =
    /// The empty map delta.
    [<GeneralizableValue>]
    let empty<'K, 'V> = HashMapDelta<'K, 'V>.Empty

    /// A single map delta containing one operation.
    let single (key : 'K) (value: ElementOperation<'Value>) =
        HashMapDelta(HashMap.single key value)

    /// A HashMapDelta containing all the given deltas.
    let ofHashMap (elements : HashMap<'K, ElementOperation<'V>>) =
        HashMapDelta elements
        
    /// A HashMapDelta containing all the given deltas.
    let ofSeq (elements : seq<'K * ElementOperation<'V>>) = 
        HashMapDelta(HashMap.ofSeq elements)
        
    /// A HashMapDelta containing all the given deltas.
    let ofList (elements : list<'K * ElementOperation<'V>>) = 
        HashMapDelta(HashMap.ofList elements)
        
    /// A HashMapDelta containing all the given deltas.
    let ofArray (elements : array<'K * ElementOperation<'V>>) = 
        HashMapDelta(HashMap.ofArray elements)

    /// Is the map empty?
    let inline isEmpty (map : HashMapDelta<'K, 'V>) =
        map.IsEmpty

    /// The number of deltas in the map.
    let inline count (map : HashMapDelta<'K, 'V>) =
        map.Count

    /// All deltas contained in the map.
    let toSeq (map : HashMapDelta<'K, 'V>) = 
        map.Store :> seq<_>
        
    /// All deltas contained in the map.
    let toList (map : HashMapDelta<'K, 'V>) = 
        map.Store |> HashMap.toList
        
    /// All deltas contained in the map.
    let toArray (map : HashMapDelta<'K, 'V>) = 
        map.Store |> HashMap.toArray
        
    /// All deltas contained in the map.
    let toHashMap (map : HashMapDelta<'K, 'V>) =
        map.Store

    /// Combines two HashMapDeltas to one.
    let inline combine (l : HashMapDelta<'K, 'V>) (r : HashMapDelta<'K, 'V>) = l.Combine r

