namespace CSharp.Data.Adaptive

open System
open System.Runtime.CompilerServices
open FSharp.Data.Adaptive
open FSharp.Data.Traceable


/// C# friendly base-class exposing evaluation-utilities.
[<AbstractClass>]
type AbstractAdaptiveObject() =
    inherit AdaptiveObject()
    
    member x.EvaluateAlways<'T>(token : AdaptiveToken, compute : Func<AdaptiveToken, 'T>) : 'T =
        (x :> AdaptiveObject).EvaluateAlways token compute.Invoke
        
    member x.EvaluateIfNeeded<'T>(token : AdaptiveToken, def : 'T, compute : Func<AdaptiveToken, 'T>) : 'T =
        (x :> AdaptiveObject).EvaluateIfNeeded token def compute.Invoke


[<Struct>]
type TransactionDisposable(active : Transaction, old : ValueOption<Transaction>) =

    interface IDisposable with
        member x.Dispose() = 
            Transaction.Current <- old // reset old
            active.Commit() // mark
            active.Dispose() // finalizers


module private AdaptiveObjectExtensionHelpers =
    let inline addMarkingCallback (v : IAdaptiveObject) (action: unit -> unit) =
        v.AddMarkingCallback(action)
        
    let inline addWeakMarkingCallback (v : IAdaptiveObject) (action: unit -> unit) =
        v.AddWeakMarkingCallback(action)

    let inline useTransaction() =
        let t = new Transaction()
        let old = Transaction.Current
        Transaction.Current <- ValueSome t
        new TransactionDisposable(t, old)

    let inline markOutdated (v : IAdaptiveObject) =
        v.MarkOutdated()

    let inline markOutdated' (v : IAdaptiveObject) (fin : unit -> unit) =
        v.MarkOutdated(fin)

[<AbstractClass; Sealed; Extension>]
type Adaptive private() =

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member AddMarkingCallback(this: IAdaptiveObject, action: Action) =
        AdaptiveObjectExtensionHelpers.addMarkingCallback this action.Invoke
        
    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member AddWeakMarkingCallback(this: IAdaptiveObject, action: Action) =
        AdaptiveObjectExtensionHelpers.addWeakMarkingCallback this action.Invoke

    static member Transact : IDisposable =
        AdaptiveObjectExtensionHelpers.useTransaction() :> IDisposable

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member UseTransaction(t : Transaction) : TransactionDisposable =
        let old = Transaction.Current
        Transaction.Current <- ValueSome t
        new TransactionDisposable(t, old)

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member GetValueUntyped(this: IAdaptiveValue) =
        this.GetValueUntyped(AdaptiveToken.Top)

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member GetChanges<'T>(this: IOpReader<'T>) =
        this.GetChanges(AdaptiveToken.Top)

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member MarkOutdated(this: IAdaptiveObject) =
        AdaptiveObjectExtensionHelpers.markOutdated this

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member MarkOutdated(this: IAdaptiveObject, fin : Action) =
        AdaptiveObjectExtensionHelpers.markOutdated' this fin.Invoke

