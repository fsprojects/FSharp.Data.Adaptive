namespace FSharp.Data.Adaptive

open System
open System.Runtime.CompilerServices
open FSharp.Data.Adaptive

module private AdaptiveObjectExtensionHelpers =
    let inline addMarkingCallback (v : IAdaptiveObject) (action: unit -> unit) =
        v.AddMarkingCallback(action)

    let inline useTransaction() =
        let t = new Transaction()
        let d = Transaction.makeCurrent t

        { new IDisposable with
            member x.Dispose() =
                d.Dispose()
                t.Commit()
                t.Dispose()
        }


[<AbstractClass; Sealed; Extension>]
type Adaptive private() =

    [<Extension; MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member AddMarkingCallback(this: IAdaptiveObject, action: Action) =
        AdaptiveObjectExtensionHelpers.addMarkingCallback this action.Invoke


    static member Transact : IDisposable =
        AdaptiveObjectExtensionHelpers.useTransaction()
