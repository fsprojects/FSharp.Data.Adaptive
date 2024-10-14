namespace FSharp.Data.Adaptive

open System
open FSharp.Data.Traceable
open FSharp.Data.Adaptive

/// An adaptive reader for aarr that allows to pull operations and exposes its current state.
type IArrayReader<'T> = 
    IOpReader<arr<'T>, arrdelta<'T>>

/// Adaptive array datastructure.
[<Interface>]
type IAdaptiveArray<'T> =
    /// Is the array constant?
    abstract member IsConstant : bool

    /// The current content of the array as aval.
    abstract member Content : aval<arr<'T>>
    
    /// Gets a new reader to the array.
    abstract member GetReader : unit -> IArrayReader<'T>
    
    /// Gets the underlying History instance for the alist (if any)
    abstract member History : option<History<arr<'T>, arrdelta<'T>>>

/// Adaptive list datastructure.
type aarr<'T> = IAdaptiveArray<'T>


/// Functional operators for the alist<_> type.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AArr =

    /// The empty aarr.
    [<GeneralizableValue>]
    val empty<'T> : aarr<'T>

    /// A constant aarr holding a single value.
    val single : value: 'T -> aarr<'T>

    /// Creates an aarr holding the given values.
    val ofSeq : elements: seq<'T> -> aarr<'T>

    /// Creates an aarr holding the given values.
    val ofList : elements: list<'T> -> aarr<'T>

    /// Creates an aarr holding the given values.
    val ofArray : elements: 'T[] -> aarr<'T>

    /// Creates an aarr holding the given values.
    val ofArr : elements: arr<'T> -> aarr<'T>
    
    /// Creates an aarr using the given reader-creator.
    val ofReader : create: (unit -> #IOpReader<arrdelta<'T>>) -> aarr<'T>

    /// Adaptively applies the given mapping function to all elements and returns a new aarr containing the results.
    val map : mapping: ('T1 -> 'T2) -> input: aarr<'T1> -> aarr<'T2>
