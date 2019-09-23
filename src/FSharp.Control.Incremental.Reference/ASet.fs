namespace FSharp.Control.Incremental.Reference

open FSharp.Control.Incremental
open FSharp.Control.Incremental.Reference

/// the reference implementation for IOpReader<_>.
type IOpReader<'op> =
    abstract member GetOperations : AdaptiveToken -> 'op
    
/// the reference implementation for IOpReader<_,_>.
type IOpReader<'s, 'op> =
    inherit IOpReader<'op>
    abstract member State : 's
    
/// the reference implementation for ISetReader.
type ISetReader<'a> = IOpReader<HashSet<'a>, DHashSet<'a>>

/// the reference implementation for aset.
type aset<'a> =
    abstract member GetReader : unit -> ISetReader<'a>
    abstract member Content : aref<HashSet<'a>>

/// a simple reader using differentiate for getting deltas.
type internal ASetReader<'a>(set : aset<'a>) =

    let mutable last = HashSet.empty

    member x.State =
        last

    member x.GetOperations(t) =
        let c = set.Content.GetValue t
        let ops = HashSet.differentiate last c
        last <- c
        ops

    interface IOpReader<DHashSet<'a>> with
        member x.GetOperations t = x.GetOperations t
        
    interface IOpReader<HashSet<'a>, DHashSet<'a>> with
        member x.State = x.State

/// reference implementation for cset.
type cset<'a>(value : HashSet<'a>) =
    let mutable content = value

    /// current content as aref<_>
    let contentRef =
        { new aref<HashSet<'a>> with
            member x.GetValue _ = content
        }

    /// is the set empty?
    member x.IsEmpty = content.IsEmpty
    /// the number of entries in the set.
    member x.Count = content.Count
    /// checks whether the given value is contained in the set.
    member x.Contains(value : 'a) = HashSet.contains value content
    /// adds the given value to the set and returns true if the element was new.
    member x.Add(value : 'a) =
        let w = HashSet.contains value content
        content <- HashSet.add value content
        not w
    /// removes the given element from the set and returns true if the element was deleted.
    member x.Remove(value : 'a) =
        let w = HashSet.contains value content
        content <- HashSet.remove value content
        w
    /// removes all entries from the set.
    member x.Clear() =
        content <- HashSet.empty
    /// adds all given values to the set.
    member x.UnionWith (other : seq<'a>) =
        content <- HashSet.union content (HashSet.ofSeq other)
    /// removes all given values from the set.
    member x.ExceptWith (other : seq<'a>) =
        content <- HashSet.difference content (HashSet.ofSeq other)
    /// gets or sets the current immutable state of the set.
    member x.Value 
        with get() = content
        and set v = content <- v

    interface aset<'a> with
        member x.Content = contentRef
        member x.GetReader() = ASetReader(x) :> ISetReader<_>
            
    /// creates a new empty cset.
    new() = cset<'a>(HashSet.empty)
    /// creates a new cset with all the given values.
    new(es : seq<'a>) = cset(HashSet.ofSeq es)

/// functional operators for the aset reference-implementation.
module ASet =
    /// creates an aset from the given aref.
    let internal ofRef (r : aref<HashSet<'a>>) =
        { new aset<'a> with 
            member x.Content = r
            member x.GetReader() = ASetReader(x) :> ISetReader<_>    
        }
        
    /// the empty aset.
    let empty<'a> = ofRef (ARef.constant HashSet.empty<'a>)
    
    /// a constant aset containing a single value
    let single (value : 'a) = ofRef (ARef.constant (HashSet.single value))

    /// creates a constant aset from the given values.
    let ofSeq (values : seq<'a>) = ofRef (ARef.constant (HashSet.ofSeq values))
    
    /// creates a constant aset from the given values.
    let ofList (values : list<'a>) = ofRef (ARef.constant (HashSet.ofList values))
    
    /// creates a constant aset from the given values.
    let ofArray (values : 'a[]) = ofRef (ARef.constant (HashSet.ofArray values))
    
    /// creates a constant aset from the given values.
    let ofHashSet (values : HashSet<'a>) = ofRef (ARef.constant values)
    
    /// creates an aref holding the set's content.
    let toARef (set : aset<'a>) = set.Content

    /// applies mapping to all elements of the set and returns the resulting set.
    let map (mapping : 'a -> 'b) (set : aset<'a>) =
        set.Content |> ARef.map (HashSet.map mapping) |> ofRef

    /// applies mapping to all elements of the set and returns the resulting set.
    let choose (mapping : 'a -> Option<'b>) (set : aset<'a>) =
        set.Content |> ARef.map (HashSet.choose mapping) |> ofRef
        
    /// filters the set using the given predicate.
    let filter (predicate : 'a -> bool) (set : aset<'a>) =
        set.Content |> ARef.map (HashSet.filter predicate) |> ofRef

    /// unions all the sets.
    let union (sets : aset<aset<'a>>) =
        sets.Content |> ARef.map (fun sets ->
            sets |> HashSet.collect (fun s -> s.Content.GetValue AdaptiveToken.Top)
        ) |> ofRef

    /// unions all the sets.
    let collect (mapping : 'a -> aset<'b>) (set : aset<'a>) =
        set.Content |> ARef.map (fun values ->
            values |> HashSet.collect (fun s -> (mapping s).Content.GetValue AdaptiveToken.Top)
        ) |> ofRef

    let ofARef (ref : aref<#seq<'a>>) =
        { new aset<'a> with 
            member x.Content = ref |> ARef.map HashSet.ofSeq
            member x.GetReader() = ASetReader(x) :> ISetReader<_>    
        }