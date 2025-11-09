module ChangeableModelListList

open FSharp.Data.Adaptive
open FSharp.Data.Traceable

type Thing =
    {
        Id : int
        Name : string
    }
    
type AdaptiveThing(initial : Thing) =
    let _id = cval initial.Id
    let _name = cval initial.Name
    member x.Id = _id :> aval<int>
    member x.Name = _name :> aval<string>
    member x.Update(t : Thing) =
        _id.Value <- t.Id
        if _name.Value <> t.Name then
            printfn "%d.Name <- %s" t.Id t.Name
            _name.Value <- t.Name
        
    
    override x.ToString() =
        sprintf "AdaptiveThing(Id = %d, Name = %s)" _id.Value _name.Value
    
let run() =
    
    let cmp (a : Thing) (b : Thing) = a.Id = b.Id
    
    let values = [ { Id = 1; Name = "Hello" }; { Id = 2; Name = "World" } ]
    let adaptiveThings = ChangeableModelListList(values, cmp, AdaptiveThing, (fun (t : AdaptiveThing) (a : Thing) -> t.Update a; t), id)
    
    let ll = adaptiveThings :> alist<_>
    
    let r = ll.GetReader()
    printfn "Initial:"
    let ops = r.GetChanges AdaptiveToken.Top
    printfn "%A" ops
    printfn "%A" r.State
    
    
    printfn "Change 1:"
    transact (fun () ->
        adaptiveThings.Update [ { Id = 1; Name = "Sepp" };  { Id = 3; Name = "says" }; { Id = 2; Name = "Hello" } ]
    )
    
    let ops = r.GetChanges AdaptiveToken.Top
    printfn "%A" ops
    printfn "%A" r.State
    
    ()