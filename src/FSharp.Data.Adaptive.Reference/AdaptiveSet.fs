namespace FSharp.Data.Adaptive.Reference

open FSharp.Data.Adaptive
open FSharp.Data.Adaptive.Reference

/// the reference implementation for IOpReader<_>.
type IOpReader<'Delta> =
    abstract member GetChanges: AdaptiveToken -> 'Delta
    
/// the reference implementation for IOpReader<_,_>.
type IOpReader<'State, 'Delta> =
    inherit IOpReader<'Delta>
    abstract member State: 'State
    
/// the reference implementation for IHashSetReader.
type IHashSetReader<'T> = IOpReader<HashSet<'T>, HashSetDelta<'T>>

/// the reference implementation for aset.
type AdaptiveHashSet<'T> =
    abstract member GetReader: unit -> IHashSetReader<'T>
    abstract member Content: aref<HashSet<'T>>

and aset<'T> = AdaptiveHashSet<'T>

/// A simple reader using differentiate for getting deltas.
type internal ASetReader<'T>(set: aset<'T>) =

    let mutable last = HashSet.empty

    member x.State =
        last

    member x.GetChanges(t) =
        let c = set.Content.GetValue t
        let ops = HashSet.differentiate last c
        last <- c
        ops

    interface IOpReader<HashSetDelta<'T>> with
        member x.GetChanges t = x.GetChanges t
        
    interface IOpReader<HashSet<'T>, HashSetDelta<'T>> with
        member x.State = x.State

/// A reference implementation for cset.
type ChangeableHashSet<'T>(value: HashSet<'T>) =
    let mutable content = value

    // the current content as aref<_>
    let contentRef =
        { new aref<HashSet<'T>> with
            member x.GetValue _ = content
        }

    /// Indicates if the set is empty
    member x.IsEmpty = content.IsEmpty

    /// Indicates the number of entries in the set.
    member x.Count = content.Count

    /// Checks whether the given value is contained in the set.
    member x.Contains(value: 'T) = HashSet.contains value content

    /// Adds the given value to the set and returns true if the element was new.
    member x.Add(value: 'T) =
        let w = HashSet.contains value content
        content <- HashSet.add value content
        not w

    /// Removes the given element from the set and returns true if the element was deleted.
    member x.Remove(value: 'T) =
        let w = HashSet.contains value content
        content <- HashSet.remove value content
        w

    /// Removes all entries from the set.
    member x.Clear() =
        content <- HashSet.empty

    /// Adds all given values to the set.
    member x.UnionWith (other: seq<'T>) =
        content <- HashSet.union content (HashSet.ofSeq other)

    /// Removes all given values from the set.
    member x.ExceptWith (other: seq<'T>) =
        content <- HashSet.difference content (HashSet.ofSeq other)

    /// Gets or sets the current immutable state of the set.
    member x.Value 
        with get() = content
        and set v = content <- v

    interface aset<'T> with
        member x.Content = contentRef
        member x.GetReader() = ASetReader(x) :> IHashSetReader<_>

    /// Creates a new empty cset.
    new() = cset<'T>(HashSet.empty)
 
    /// Creates a new cset with all the given values.
    new(es: seq<'T>) = cset(HashSet.ofSeq es)

and cset<'T> = ChangeableHashSet<'T>

/// functional operators for the aset reference-implementation.
module ASet =

    /// Creates an aset from the given aref.
    let internal ofRef (r: aref<HashSet<'T>>) =
        { new aset<'T> with 
            member x.Content = r
            member x.GetReader() = ASetReader(x) :> IHashSetReader<_>    
        }
        
    /// The empty aset.
    let empty<'T> = ofRef (ARef.constant HashSet.empty<'T>)
    
    /// A constant aset containing a single value
    let single (value: 'T) = ofRef (ARef.constant (HashSet.single value))

    /// Creates a constant aset from the given values.
    let ofSeq (values: seq<'T>) = ofRef (ARef.constant (HashSet.ofSeq values))
    
    /// Creates a constant aset from the given values.
    let ofList (values: list<'T>) = ofRef (ARef.constant (HashSet.ofList values))
    
    /// Creates a constant aset from the given values.
    let ofArray (values: 'T[]) = ofRef (ARef.constant (HashSet.ofArray values))
    
    /// Creates a constant aset from the given values.
    let ofHashSet (values: HashSet<'T>) = ofRef (ARef.constant values)
    
    /// Creates an adaptive value holding the set'State content.
    let toARef (set: aset<'T>) = set.Content

    /// Applies mapping to all elements of the set and returns the resulting set.
    let map (mapping: 'T -> 'b) (set: aset<'T>) =
        set.Content |> ARef.map (HashSet.map mapping) |> ofRef

    /// Applies mapping to all elements of the set and returns the resulting set.
    let choose (mapping: 'T -> option<'b>) (set: aset<'T>) =
        set.Content |> ARef.map (HashSet.choose mapping) |> ofRef
        
    /// Filters the set using the given predicate.
    let filter (predicate: 'T -> bool) (set: aset<'T>) =
        set.Content |> ARef.map (HashSet.filter predicate) |> ofRef

    /// Unions all the sets.
    let union (sets: aset<aset<'T>>) =
        sets.Content |> ARef.map (fun sets ->
            sets |> HashSet.collect (fun s -> s.Content.GetValue AdaptiveToken.Top)
        ) |> ofRef

    /// Unions all the sets.
    let collect (mapping: 'T -> aset<'b>) (set: aset<'T>) =
        set.Content |> ARef.map (fun values ->
            values |> HashSet.collect (fun s -> (mapping s).Content.GetValue AdaptiveToken.Top)
        ) |> ofRef

    let ofARef (ref: aref<#seq<'T>>) =
        { new aset<'T> with 
            member x.Content = ref |> ARef.map HashSet.ofSeq
            member x.GetReader() = ASetReader(x) :> IHashSetReader<_>    
        }
