namespace FSharp.Data.Adaptive

open System.Collections.Generic
open FSharp.Data.Traceable

/// Changeable adaptive array that allows mutation by user-code and implements aarr.
[<Sealed>]
type ChangeableArray<'T> =
    interface IAdaptiveArray<'T>
    interface System.Collections.Generic.IEnumerable<'T>
    interface System.Collections.Generic.ICollection<'T>
    interface System.Collections.Generic.IList<'T>

    /// is the array currently empty?
    member IsEmpty : bool

    /// the number of elements currently in the array.
    member Length : int

    /// Gets or sets the value for the array.
    member Value : arr<'T> with get, set
    
    member Clear : unit -> unit
    member Add : 'T -> unit
    member Insert : index: int * value: 'T -> unit
    member Item : int -> 'T with get, set
    
    new : unit -> ChangeableArray<'T>
    new : arr<'T> -> ChangeableArray<'T>
    new : seq<'T> -> ChangeableArray<'T>
    
/// Changeable adaptive array that allows mutation by user-code and implements aarr.
type carr<'T> = ChangeableArray<'T>