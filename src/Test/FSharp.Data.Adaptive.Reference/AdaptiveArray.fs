namespace FSharp.Data.Adaptive.Reference

open FSharp.Data.Adaptive
open FSharp.Data.Adaptive.Reference

/// The reference implementation for IIndexListReader.
type IArrayReader<'T> = FSharp.Data.Adaptive.Reference.IOpReader<arr<'T>, arrdelta<'T>>

/// The reference implementation for alist.
type IAdaptiveArray<'T> =
    abstract member GetReader: unit -> IArrayReader<'T>
    abstract member Content: FSharp.Data.Adaptive.Reference.aval<arr<'T>>

and aarr<'T> = IAdaptiveArray<'T>

/// A simple reader using computeDelta for getting deltas.
type internal AArrReader<'T>(list: aarr<'T>) =

    let mutable last = Arr.empty

    member x.State =
        last

    member x.GetChanges(t : FSharp.Data.Adaptive.Reference.AdaptiveToken) =
        let c = list.Content.GetValue t
        let ops = Arr.computeDelta DefaultEqualityComparer.Instance last c
        last <- c
        ops

    interface IOpReader<arrdelta<'T>> with
        member x.GetChanges t = x.GetChanges t
        
    interface IOpReader<arr<'T>, arrdelta<'T>> with
        member x.State = x.State


/// A reference implementation for clist.
type ChangeableArray<'T>(value: arr<'T>) =
    let mutable content = value

    // the current content as aval<_>
    let contentRef =
        { new aval<arr<'T>> with
            member x.GetValue _ = content
        }

    /// Gets or sets the current immutable state of the set.
    member x.Value 
        with get() = content
        and set v = content <- v

    interface IAdaptiveArray<'T> with
        member x.Content = contentRef
        member x.GetReader() = AArrReader(x) :> IArrayReader<_>

    /// Creates a new empty cset.
    new() = carr<'T>(IndexList.empty)
 
    /// Creates a new cset with all the given values.
    new(es: seq<'T>) = carr(IndexList.ofSeq es)

and carr<'T> = ChangeableArray<'T>


/// Functional operators for the alist reference-implementation.
module AArr =

    /// Creates an alist from the given aval.
    let internal ofRef (r: aval<arr<'T>>) =
        { new aarr<'T> with 
            member x.Content = r
            member x.GetReader() = AArrReader(x) :> IArrayReader<_>    
        }
        
    /// The empty alist.
    let empty<'T> = ofRef (AVal.constant Arr.empty<'T>)
    
    /// A constant alist containing a single value
    let single (value: 'T) = ofRef (AVal.constant (Arr.single value))

    /// Creates a constant alist from the given values.
    let ofSeq (values: seq<'T>) = ofRef (AVal.constant (Arr.ofSeq values))
    
    /// Creates a constant alist from the given values.
    let ofList (values: list<'T>) = ofRef (AVal.constant (Arr.ofList values))
    
    /// Creates a constant alist from the given values.
    let ofArray (values: array<'T>) = ofRef (AVal.constant (Arr.ofArray values))
    
    /// Creates a constant alist from the given values.
    let ofArr (values: arr<'T>) = ofRef (AVal.constant values)
    
    let map (mapping: 'T1 -> 'T2) (list: aarr<'T1>) =
        list.Content |> AVal.map (Arr.map mapping) |> ofRef
        
    let choose (mapping: 'T1 -> option<'T2>) (list: aarr<'T1>) =
        list.Content |> AVal.map (Arr.choose mapping) |> ofRef
        
    let collect (mapping: 'T1 -> aarr<'T2>) (list: aarr<'T1>) =
        list.Content |> AVal.map (Arr.collect (fun v -> mapping(v).Content.GetValue(AdaptiveToken.Top))) |> ofRef