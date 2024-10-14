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
    /// Efficient implementation for a constant adaptive array.
    [<Sealed>]
    type ConstantArray<'T>(content : Lazy<arr<'T>>) =
        let value = AVal.delay (fun () -> content.Value)

        member x.Content = value

        member x.GetReader() =
            History.Readers.ConstantReader<_,_>(
                Arr.trace,
                lazy (Arr.computeDelta DefaultEqualityComparer.Instance Arr.empty content.Value),
                content
            ) :> IArrayReader<_>

        interface IAdaptiveArray<'T> with
            member x.IsConstant = true
            member x.GetReader() = x.GetReader()
            member x.Content = x.Content
            member x.History = None

    /// Core implementation for a dependent array.
    [<Sealed>]
    type AdaptiveArray<'T>(createReader : unit -> IOpReader<arrdelta<'T>>) =
        let history = History(createReader, Arr.trace)

        /// Gets a new reader to the set.
        member x.GetReader() : IArrayReader<'T> =
            history.NewReader()

        /// Current content of the set as aval.
        member x.Content =
            history :> aval<_>

        interface IAdaptiveArray<'T> with
            member x.IsConstant = false
            member x.GetReader() = x.GetReader()
            member x.Content = x.Content
            member x.History = Some history

    /// Efficient implementation for an empty adaptive array.
    [<Sealed>]
    type EmptyArray<'T> private() =   
        static let instance = EmptyArray<'T>() :> aarr<_>
        let content = AVal.constant Arr.empty
        let reader = History.Readers.EmptyReader<arr<'T>, arrdelta<'T>>(Arr.trace) :> IArrayReader<'T>
        static member Instance = instance
        
        member x.Content = content
        member x.GetReader() = reader
        
        interface IAdaptiveArray<'T> with
            member x.IsConstant = true
            member x.GetReader() = x.GetReader()
            member x.Content = x.Content
            member x.History = None
            
    module Readers =
        type MapReader<'T1, 'T2>(input : aarr<'T1>, mapping : 'T1 -> 'T2) =
            inherit AbstractReader<arrdelta<'T2>>(ArrDelta.empty)
            
            let reader = input.GetReader()
            
            override x.Compute(token : AdaptiveToken) =
                let delta = reader.GetChanges(token)
                ArrDelta.map mapping delta
                
    /// The empty aarr.
    [<GeneralizableValue>]
    let empty<'T> : aarr<'T> =
        EmptyArray<'T>.Instance

    /// A constant aarr holding a single value.
    let single (value : 'T) : aarr<'T> =
        ConstantArray(Lazy<_>.CreateFromValue(Arr.single value)) :> aarr<_>

    /// Creates an aarr holding the given values.
    let ofSeq (elements: seq<'T>) : aarr<'T> =
        ConstantArray(Lazy<_>.CreateFromValue(Arr.ofSeq elements)) :> aarr<_>
        

    /// Creates an aarr holding the given values.
    let ofList (elements: list<'T>) : aarr<'T> =
        ConstantArray(Lazy<_>.CreateFromValue(Arr.ofList elements)) :> aarr<_>

    /// Creates an aarr holding the given values.
    let ofArray (elements: 'T[]) : aarr<'T> =
        ConstantArray(Lazy<_>.CreateFromValue(Arr.ofArray elements)) :> aarr<_>

    /// Creates an aarr holding the given values.
    let ofArr (elements: arr<'T>) : aarr<'T> =
        ConstantArray(Lazy<_>.CreateFromValue(elements)) :> aarr<_>

    /// Creates an aarr using the given reader-creator.
    let ofReader (create : (unit -> #IOpReader<arrdelta<'T>>)) : aarr<'T> =
        AdaptiveArray<'T>(fun () -> create() :> IOpReader<arrdelta<'T>>) :> aarr<'T>

    /// Adaptively applies the given mapping function to all elements and returns a new aarr containing the results.
    let map (mapping : 'T1 -> 'T2) (input : aarr<'T1>) : aarr<'T2> =
        ofReader <| fun () ->
            Readers.MapReader(input, mapping)
            