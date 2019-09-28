namespace FSharp.Data.Adaptive.Reference

open FSharp.Data.Adaptive
open FSharp.Data.Adaptive.Reference


/// The reference implementation for IHashMapReader.
type IHashMapReader<'Key, 'Value> = IOpReader<HashMap<'Key, 'Value>, HashMapDelta<'Key, 'Value>>

/// The reference implementation for amap.
type AdaptiveHashMap<'Key, 'Value> =
    abstract member GetReader: unit -> IHashMapReader<'Key, 'Value>
    abstract member Content: aval<HashMap<'Key, 'Value>>

and amap<'Key, 'Value> = AdaptiveHashMap<'Key, 'Value>

/// A simple reader using computeDelta for getting deltas.
type internal AMapReader<'Key, 'Value>(set: amap<'Key, 'Value>) =

    let mutable last = HashMap.empty

    member x.State =
        last

    member x.GetChanges(t) =
        let c = set.Content.GetValue t
        let ops = HashMap.computeDelta last c
        last <- c
        ops

    interface IOpReader<HashMapDelta<'Key, 'Value>> with
        member x.GetChanges t = x.GetChanges t
        
    interface IOpReader<HashMap<'Key, 'Value>, HashMapDelta<'Key, 'Value>> with
        member x.State = x.State


/// A reference implementation for cset.
type ChangeableHashMap<'Key, 'Value>(value: HashMap<'Key, 'Value>) =
    let mutable content = value

    // the current content as aval<_>
    let contentRef =
        { new aval<HashMap<'Key, 'Value>> with
            member x.GetValue _ = content
        }

    /// Indicates if the set is empty
    member x.IsEmpty = content.IsEmpty

    /// Indicates the number of entries in the set.
    member x.Count = content.Count

    /// Checks whether the given value is contained in the set.
    member x.ContainsKey(value: 'Key) = HashMap.containsKey value content

    member x.Item
        with get (key: 'Key) = 
            content.[key]
        and set (key: 'Key) (value: 'Value) =
            content <- HashMap.add key value content
        

    /// Adds the given value to the set and returns true if the element was new.
    member x.Add(key: 'Key, value: 'Value) =
        let w = HashMap.containsKey key content
        content <- HashMap.add key value content
        not w

    /// Removes the given element from the set and returns true if the element was deleted.
    member x.Remove(key: 'Key) =
        let w = HashMap.containsKey key content
        content <- HashMap.remove key content
        w

    /// Removes all entries from the set.
    member x.Clear() =
        content <- HashMap.empty

    /// Gets or sets the current immutable state of the set.
    member x.Value 
        with get() = content
        and set v = content <- v
        
    member x.GetReader() = AMapReader(x) :> IHashMapReader<_,_>

    interface AdaptiveHashMap<'Key, 'Value> with
        member x.Content = contentRef
        member x.GetReader() = AMapReader(x) :> IHashMapReader<_,_>

    /// Creates a new empty cset.
    new() = ChangeableHashMap<'Key, 'Value>(HashMap.empty)
 
    /// Creates a new cset with all the given values.
    new(es: seq<'Key * 'Value>) = ChangeableHashMap(HashMap.ofSeq es)

and cmap<'Key, 'Value> = ChangeableHashMap<'Key, 'Value>


/// Functional operators for the aset reference-implementation.
module AMap =

    /// Creates an amap from the given aval.
    let internal ofRef (r: aval<HashMap<'Key, 'Value>>) =
        { new amap<'Key, 'Value> with 
            member x.Content = r
            member x.GetReader() = AMapReader(x) :> IHashMapReader<_, _>    
        }
        
     
    /// The empty aset.
    let empty<'Key, 'Value> = ofRef (AVal.constant HashMap.empty<'Key, 'Value>)
    
    /// A constant amap containing a single value
    let single (key: 'Key) (value: 'Value) = ofRef (AVal.constant (HashMap.single key value))

    /// Creates a constant amap from the given values.
    let ofSeq (values: seq<'Key * 'Value>) = ofRef (AVal.constant (HashMap.ofSeq values))
    
    /// Creates a constant amap from the given values.
    let ofList (values: list<'Key * 'Value>) = ofRef (AVal.constant (HashMap.ofList values))
    
    /// Creates a constant amap from the given values.
    let ofArray (values: array<'Key * 'Value>) = ofRef (AVal.constant (HashMap.ofArray values))
    
    /// Creates a constant amap from the given values.
    let ofHashMap (values: HashMap<'Key, 'Value>) = ofRef (AVal.constant values)
    
    /// Creates an adaptive value holding the set'State content.
    let toAVal (set: amap<'Key, 'Value>) = set.Content
    

    let map (mapping: 'Key -> 'Value -> 'T) (set: amap<'Key, 'Value>) =
        set.Content |> AVal.map (HashMap.map mapping) |> ofRef
    
    let map' (mapping: 'Value -> 'T) (set: amap<'Key, 'Value>) =
        map (fun _ -> mapping) set

    let mapSet (mapping: 'Key -> 'Value) (set: aset<'Key>) =
        set.Content |> AVal.map (fun a -> a |> Seq.map (fun k -> k, mapping k) |> HashMap.ofSeq) |> ofRef

    let choose (mapping: 'Key -> 'Value -> Option<'T>) (set: amap<'Key, 'Value>) =
        set.Content |> AVal.map (HashMap.choose mapping) |> ofRef
    
    let choose' (mapping: 'Value -> Option<'T>) (set: amap<'Key, 'Value>) =
        choose (fun _ -> mapping) set

    let filter (mapping: 'Key -> 'Value -> bool) (set: amap<'Key, 'Value>) =
        set.Content |> AVal.map (HashMap.filter mapping) |> ofRef

    let filter' (mapping: 'Value -> bool) (set: amap<'Key, 'Value>) =
        filter (fun _ -> mapping) set
    
    let unionWith (resolve: 'Key -> 'Value -> 'Value -> 'Value) (l: amap<'Key, 'Value>) (r: amap<'Key, 'Value>) =
        (l.Content, r.Content) ||> AVal.map2 (fun l r ->
            HashMap.unionWith resolve l r
        ) |> ofRef
    
    let union (l: amap<'Key, 'Value>) (r: amap<'Key, 'Value>) = 
        unionWith (fun _ _ r -> r) l r

    let ofAVal (value:aval<#seq<'Key * 'Value>>) : amap<'Key,'Value> =
        value |> AVal.map (fun e -> e :> seq<_> |> HashMap.ofSeq) |> ofRef

    let bind (mapping : 'T -> amap<'Key,'Value>) (value: aval<'T>) =
        value |> AVal.bind (fun v ->
            mapping(v).Content
        ) |> ofRef

    let toASet (value : amap<'Key, 'Value>) =
        value.Content |> AVal.map (fun v ->
            v |> HashSet.ofSeq
        ) |> ASet.ofRef