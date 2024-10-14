namespace FSharp.Data.Adaptive

open System.Collections.Generic
open FSharp.Data.Traceable

/// Changeable adaptive array that allows mutation by user-code and implements aarr.
[<Sealed>]
type ChangeableArray<'T>(state : arr<'T>) =
    let history = History<arr<'T>, arrdelta<'T>>(Arr.trace)
    
    do history.Perform (ArrDelta.single { Index = 0; Count = 0; Elements = state }) |> ignore
    
    /// is the array currently empty?
    member x.IsEmpty = history.State.IsEmpty

    /// the number of elements currently in the array.
    member x.Length = history.State.Length

    /// Gets or sets the value for the array.
    member x.Value
        with get() =
            history.State
        and set (v : arr<'T>) =
            let delta = Arr.computeDelta DefaultEqualityComparer.Instance history.State v
            history.Perform delta |> ignore
            
    member x.Clear() =
        let delta = ArrDelta.single { Index = 0; Count = history.State.Length; Elements = Arr.empty }
        history.Perform delta |> ignore
            
    member x.Add (item : 'T) =
        let delta = ArrDelta.single { Index = history.State.Length; Count = 0; Elements = Arr.single item }
        history.Perform delta |> ignore
        
    member x.Insert (index : int, value : 'T) =
        if index < 0 || index > history.State.Length then raise <| System.IndexOutOfRangeException()
        let delta = ArrDelta.single { Index = index; Count = 0; Elements = Arr.single value }
        history.Perform delta |> ignore
        
    member x.RemoveAt(index : int) =
        if index < 0 || index >= history.State.Length then raise <| System.IndexOutOfRangeException()
        let delta = ArrDelta.single { Index = index; Count = 1; Elements = Arr.empty }
        history.Perform delta |> ignore
        
    member x.CopyTo(array, arrayIndex) = history.State.CopyTo(array, arrayIndex)
        
    member x.Item
        with get (index : int) =
            history.State.[index]
        and set (index : int) (value : 'T) =
            let delta = ArrDelta.single { Index = index; Count = 1; Elements = Arr.single value }
            history.Perform delta |> ignore
            
    new(elements : seq<'T>) = ChangeableArray<'T>(Arr.ofSeq elements)
    new() = ChangeableArray<'T>(Arr.empty)
            
    member x.GetEnumerator() = history.State.GetEnumerator()
    
    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = history.State.GetEnumerator()
    
    interface System.Collections.Generic.IEnumerable<'T> with
        member x.GetEnumerator() = history.State.GetEnumerator()
        
    interface System.Collections.Generic.ICollection<'T> with
        member x.Add(item) = x.Add item
        member x.Clear() = x.Clear()
        member x.Contains(item) = history.State |> Arr.exists (fun v -> DefaultEquality.equals v item)
        member x.CopyTo(array, arrayIndex) = x.CopyTo(array, arrayIndex)
        member x.Remove(item) =
            match Arr.tryFindIndex (fun v -> DefaultEquality.equals v item) history.State with
            | Some index -> x.RemoveAt index; true
            | None -> false
        member x.Count = x.Length
        member x.IsReadOnly = false
        
    interface System.Collections.Generic.IList<'T> with
        member x.IndexOf(item) =
            match Arr.tryFindIndex (fun v -> DefaultEquality.equals v item) history.State with
            | Some index -> index
            | None -> -1
        member x.Insert(index,item) = x.Insert(index, item)
        member x.RemoveAt(index) = x.RemoveAt(index)
        member x.Item
            with get(i : int) = x.[i]
            and set (i : int) (value : 'T) = x.[i] <- value
            
    interface IAdaptiveArray<'T> with
        member x.IsConstant = false
        member x.GetReader() = history.NewReader()
        member x.Content = history :> aval<_>
        member x.History = Some history
    
            
/// Changeable adaptive array that allows mutation by user-code and implements aarr.
type carr<'T> = ChangeableArray<'T>