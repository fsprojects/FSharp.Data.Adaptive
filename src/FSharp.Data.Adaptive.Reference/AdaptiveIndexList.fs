namespace FSharp.Data.Adaptive.Reference

open FSharp.Data.Adaptive
open FSharp.Data.Adaptive.Reference

/// The reference implementation for IIndexListReader.
type IIndexListReader<'T> = FSharp.Data.Adaptive.Reference.IOpReader<IndexList<'T>, IndexListDelta<'T>>

/// The reference implementation for alist.
type AdaptiveIndexList<'T> =
    abstract member GetReader: unit -> IIndexListReader<'T>
    abstract member Content: FSharp.Data.Adaptive.Reference.aval<IndexList<'T>>

and alist<'T> = AdaptiveIndexList<'T>

/// A simple reader using computeDelta for getting deltas.
type internal AListReader<'T>(list: alist<'T>) =

    let mutable last = IndexList.empty

    member x.State =
        last

    member x.GetChanges(t : FSharp.Data.Adaptive.Reference.AdaptiveToken) =
        let c = list.Content.GetValue t
        let ops = IndexList.computeDelta last c
        last <- c
        ops

    interface IOpReader<IndexListDelta<'T>> with
        member x.GetChanges t = x.GetChanges t
        
    interface IOpReader<IndexList<'T>, IndexListDelta<'T>> with
        member x.State = x.State

/// A reference implementation for clist.
type ChangeableIndexList<'T>(value: IndexList<'T>) =
    let mutable content = value

    // the current content as aval<_>
    let contentRef =
        { new aval<IndexList<'T>> with
            member x.GetValue _ = content
        }

    /// Gets or sets the current immutable state of the set.
    member x.Value 
        with get() = content
        and set v = content <- v

    interface AdaptiveIndexList<'T> with
        member x.Content = contentRef
        member x.GetReader() = AListReader(x) :> IIndexListReader<_>

    /// Creates a new empty cset.
    new() = clist<'T>(IndexList.empty)
 
    /// Creates a new cset with all the given values.
    new(es: seq<'T>) = clist(IndexList.ofSeq es)

and clist<'T> = ChangeableIndexList<'T>



/// Functional operators for the alist reference-implementation.
module AList =

    /// Creates an alist from the given aval.
    let ofRef (r: aval<IndexList<'T>>) =
        { new alist<'T> with 
            member x.Content = r
            member x.GetReader() = AListReader(x) :> IIndexListReader<_>    
        }
        
    /// The empty alist.
    let empty<'T> = ofRef (AVal.constant IndexList.empty<'T>)
    
    /// A constant alist containing a single value
    let single (value: 'T) = ofRef (AVal.constant (IndexList.single value))

    /// Creates a constant alist from the given values.
    let ofSeq (values: seq<'T>) = ofRef (AVal.constant (IndexList.ofSeq values))
    
    /// Creates a constant alist from the given values.
    let ofList (values: list<'T>) = ofRef (AVal.constant (IndexList.ofList values))
    
    /// Creates a constant alist from the given values.
    let ofArray (values: array<'T>) = ofRef (AVal.constant (IndexList.ofArray values))
    
    /// Creates a constant alist from the given values.
    let ofIndexList (values: IndexList<'T>) = ofRef (AVal.constant values)
    
    let mapi (mapping: Index -> 'T1 -> 'T2) (list: alist<'T1>) =
        list.Content |> AVal.map (IndexList.mapi mapping) |> ofRef
        
    let map (mapping: 'T1 -> 'T2) (list: alist<'T1>) =
        list.Content |> AVal.map (IndexList.map mapping) |> ofRef

    let choosei (mapping: Index -> 'T1 -> option<'T2>) (list : alist<'T1>) =
        list.Content |> AVal.map (IndexList.choosei mapping) |> ofRef
        
    let choose (mapping: 'T1 -> option<'T2>) (list : alist<'T1>) =
        list.Content |> AVal.map (IndexList.choose mapping) |> ofRef

    let filteri (predicate: Index -> 'T -> bool) (list: alist<'T>) =
        list.Content |> AVal.map (IndexList.filteri predicate) |> ofRef

    let filter (predicate: 'T -> bool) (list: alist<'T>) =
        list.Content |> AVal.map (IndexList.filter predicate) |> ofRef
        
    let append (l : alist<'T>) (r : alist<'T>) =
        (l.Content, r.Content) ||> AVal.map2 IndexList.append |> ofRef


    let collecti (mapping: Index -> 'T1 -> alist<'T2>) (list: alist<'T1>) =
        list.Content |> AVal.map (IndexList.collecti (fun i v -> (mapping i v).Content |> AVal.force)) |> ofRef
        
    let collect (mapping: 'T1 -> alist<'T2>) (list: alist<'T1>) =
        list.Content |> AVal.map (IndexList.collect (fun v -> (mapping v).Content |> AVal.force)) |> ofRef
        
    let sortBy (mapping: 'T1 -> 'T2) (list : alist<'T1>) =
        list.Content |> AVal.map (IndexList.sortBy mapping) |> ofRef

    let sortWith (compare: 'T -> 'T -> int) (list : alist<'T>) =
        list.Content |> AVal.map (IndexList.sortWith compare) |> ofRef
