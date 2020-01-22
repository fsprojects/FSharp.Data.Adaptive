module AdaptifyHelpers


open NUnit.Framework
open FsUnit
open FSharp.Data.Adaptive
open FSharp.Data.Traceable
open FsCheck
open FsCheck.NUnit

type Enum =
    | A = 1
    | B = 2

[<Struct>]
type Bla(a : obj) = member x.A = a
type Del = delegate of unit -> unit

[<Test>]
let ``[ShallowEquals] special types``() =
    let bla = Bla (obj())
    let del = Del id

    ShallowEqualityComparer.Instance.Equals(Enum.A, Enum.A) |> should be True
    ShallowEqualityComparer.Instance.Equals(Enum.A, Enum.B) |> should be False
    ShallowEqualityComparer.Instance.Equals(1, 1) |> should be True
    ShallowEqualityComparer.Instance.Equals(obj(), obj()) |> should be False
    ShallowEqualityComparer.Instance.Equals(bla.A, bla.A) |> should be True

    ShallowEqualityComparer.Instance.Equals(bla, bla) |> should be True
    ShallowEqualityComparer.Instance.Equals(del, del) |> should be True


[<Property>]
let ``[CModelMap] update``(m : Map<int, string>) =
    let init v = cval v
    let update (c : cval<_>) v = c.Value <- v; c
    let view c = c :> aval<_>

    let mutable content = HashMap.ofList (Map.toList m)
    content <- HashMap.remove 4 content
    content <- HashMap.remove 2 content

    let m = ChangeableModelMap(content, init, update, view)

    let d = m |> AMap.map (fun _ v -> v)
    let r = d.GetReader()

    let check (shouldBeOutdated : bool) =
        transact (fun () -> m.Update content)
        if r.OutOfDate <> shouldBeOutdated then 
            if shouldBeOutdated then
                failwith "expected CModelMap to be outdated but wasn't"
            else
                failwith "expected CModelMap to be up-to-date but wasn't"
                
        r.GetChanges AdaptiveToken.Top |> ignore
        let res = r.State |> HashMap.map (fun _ v -> AVal.force v)
        if res <> content then 
            let ops = HashMap.computeDelta content res
            let error =
                String.concat "\r\n" [
                    yield sprintf "expected %A to be equal to %A but deltas are:" res content
                    for (k, op) in HashMapDelta.toHashMap ops do
                        yield sprintf "  %A: %A" k op
                
                ]
            
            failwith error


    check true


    content <- HashMap.add 4 "4" content
    check true

    
    content <- HashMap.add 4 "4" content
    check false
    
    content <- HashMap.add 4 "6" content
    check false
    
    content <- HashMap.add 2 "4" content
    check true

    content <- HashMap.remove 2 content
    check true
    
    content <- HashMap.empty
    check true


[<Property>]
let ``[CModelList] update``(m : list<int>) =
    let init v = cval v
    let update (c : cval<_>) v = c.Value <- v; c
    let view c = c :> aval<_>

    let mutable content = IndexList.ofList m

    let m = ChangeableModelList(content, init, update, view)

    let d = m |> AList.mapi (fun _ v -> v)
    let r = d.GetReader()

    let check (shouldBeOutdated : bool) =
        transact (fun () -> m.Update content)
        if r.OutOfDate <> shouldBeOutdated then 
            if shouldBeOutdated then
                failwith "expected CModelMap to be outdated but wasn't"
            else
                failwith "expected CModelMap to be up-to-date but wasn't"
                
        r.GetChanges AdaptiveToken.Top |> ignore
        let res = r.State |> IndexList.map AVal.force
        if res <> content then 
            let ops = IndexList.computeDelta content res
            let error =
                String.concat "\r\n" [
                    yield sprintf "expected %A to be equal to %A but deltas are:" res content
                    for (k, op) in IndexListDelta.toSeq ops do
                        yield sprintf "  %A: %A" k op
                
                ]
            
            failwith error


    check true


    content <- IndexList.add 4 content
    check true

    
    content <- IndexList.setAt (content.Count - 1) 4 content
    check false
    
    
    content <- IndexList.setAt (content.Count - 1) 6 content
    check false
    
    content <- IndexList.add 2 content
    check true

    content <- IndexList.removeAt (content.Count - 1) content
    check true
    
    content <- IndexList.empty
    check true






