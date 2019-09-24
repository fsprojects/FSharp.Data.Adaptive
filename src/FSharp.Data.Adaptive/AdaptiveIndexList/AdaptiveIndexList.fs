namespace FSharp.Data.Adaptive

open FSharp.Data.Traceable

/// An adaptive reader for alist that allows to pull operations and exposes its current state.
type IIndexListReader<'T> = 
    IOpReader<IndexList<'T>, IndexListDelta<'T>>

/// Adaptive list datastructure.
[<Interface>]
type AdaptiveIndexList<'T> =
    /// Is the list constant?
    abstract member IsConstant : bool

    /// The current content of the list as aval.
    abstract member Content : aval<IndexList<'T>>
    
    /// Gets a new reader to the list.
    abstract member GetReader : unit -> IIndexListReader<'T>

/// Adaptive list datastructure.
and alist<'T> = AdaptiveIndexList<'T>


/// Internal implementations for alist operations.
module AdaptiveIndexListImplementation =

    /// Core implementation for a dependent list.
    type AdaptiveIndexListImpl<'T>(createReader : unit -> IOpReader<IndexListDelta<'T>>) =
        let history = History(createReader, IndexList.trace)

        /// Gets a new reader to the list.
        member x.GetReader() : IIndexListReader<'T> =
            history.NewReader()

        /// Current content of the list as aval.
        member x.Content =
            history :> aval<_>

        interface AdaptiveIndexList<'T> with
            member x.IsConstant = false
            member x.GetReader() = x.GetReader()
            member x.Content = x.Content

    /// Efficient implementation for an empty adaptive list.
    type EmptyList<'T> private() =   
        static let instance = EmptyList<'T>() :> alist<_>
        let content = AVal.constant IndexList.empty
        let reader = new History.Readers.EmptyReader<IndexList<'T>, IndexListDelta<'T>>(IndexList.trace) :> IIndexListReader<'T>
        static member Instance = instance
        
        member x.Content = content
        member x.GetReader() = reader
        
        interface AdaptiveIndexList<'T> with
            member x.IsConstant = true
            member x.GetReader() = x.GetReader()
            member x.Content = x.Content

    /// Efficient implementation for a constant adaptive list.
    type ConstantList<'T>(content : Lazy<IndexList<'T>>) =
        let value = AVal.delay (fun () -> content.Value)

        member x.Content = value

        member x.GetReader() =
            new History.Readers.ConstantReader<_,_>(
                IndexList.trace,
                lazy (IndexList.differentiate IndexList.empty content.Value),
                content
            ) :> IIndexListReader<_>

        interface AdaptiveIndexList<'T> with
            member x.IsConstant = true
            member x.GetReader() = x.GetReader()
            member x.Content = x.Content




    /// Gets the current content of the alist as IndexList.
    let inline force (list : alist<'T>) = 
        AVal.force list.Content

    /// Creates a constant list using the creation function.
    let inline constant (content : unit -> IndexList<'T>) = 
        ConstantList(lazy(content())) :> alist<_> 

    /// Creates an adaptive list using the reader.
    let inline create (reader : unit -> #IOpReader<IndexListDelta<'T>>) =
        AdaptiveIndexListImpl(fun () -> reader() :> IOpReader<_>) :> alist<_>


module AList =
    open AdaptiveIndexListImplementation

    /// The empty alist.
    [<GeneralizableValue>]
    let empty<'T> : alist<'T> = 
        EmptyList<'T>.Instance

    /// A constant alist holding a single value.
    let single (value : 'T) =
        lazy (IndexList.single value) |> ConstantList :> alist<_>
        
    /// Creates an alist holding the given values.
    let ofSeq (s : seq<'T>) =
        lazy (IndexList.ofSeq s) |> ConstantList :> alist<_>
        
    /// Creates an alist holding the given values.
    let ofList (s : list<'T>) =
        lazy (IndexList.ofList s) |> ConstantList :> alist<_>
        
    /// Creates an alist holding the given values.
    let ofArray (s : 'T[]) =
        lazy (IndexList.ofArray s) |> ConstantList :> alist<_>
        
    /// Creates an alist holding the given values. `O(1)`
    let ofIndexList (elements : IndexList<'T>) =
        ConstantList(lazy elements) :> alist<_>

    /// Creates an aval providing access to the current content of the list.
    let toAVal (list : alist<'T>) =
        list.Content
