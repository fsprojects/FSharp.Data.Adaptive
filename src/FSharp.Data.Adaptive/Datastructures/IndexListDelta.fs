namespace FSharp.Data.Adaptive

open System.Collections
open System.Collections.Generic

[<StructuredFormatDisplay("{AsString}")>]
[<Struct; StructuralEquality; NoComparison>]
type IndexListDelta< [<EqualityConditionalOn>] 'T> internal(content : MapExt<Index, ElementOperation<'T>>) =
    static let empty = IndexListDelta<'T>(MapExt.empty)

    static member Empty = empty

    member internal x.Content = content

    member x.Count = content.Count

    member x.IsEmpty = content.IsEmpty

    member x.Add(i : Index, op : ElementOperation<'T>) =
        IndexListDelta(MapExt.add i op content)

    member x.Remove(i : Index) =
        IndexListDelta(MapExt.remove i content)

    member x.ToSeq() = content |> MapExt.toSeq
    member x.ToList() = content |> MapExt.toList
    member x.ToArray() = content |> MapExt.toArray

    member x.Combine(r : IndexListDelta<'T>) =
        if x.IsEmpty then r
        elif r.IsEmpty then x
        else MapExt.unionWith (fun l r -> r) x.Content r.Content |> IndexListDelta

    member x.Map(mapping : Index -> ElementOperation<'T> -> ElementOperation<'T2>) =
        IndexListDelta(MapExt.map mapping content)
        
    member x.Choose(mapping : Index -> ElementOperation<'T> -> Option<ElementOperation<'T2>>) =
        IndexListDelta(MapExt.choose mapping content)

    member x.MapMonotonic(mapping : Index -> ElementOperation<'T> -> Index * ElementOperation<'T2>) =
        IndexListDelta(MapExt.mapMonotonic mapping content)

    member x.Filter(mapping : Index -> ElementOperation<'T> -> bool) =
        IndexListDelta(MapExt.filter mapping content)
        
    override x.ToString() =
        let suffix =
            if content.Count > 4 then "; ..."
            else ""
        
        let content =
            content |> Seq.truncate 4 |> Seq.map (fun (KeyValue(i,op)) ->
                match op with
                    | Set v -> sprintf "set(%A,%A)" i v
                    | Remove -> sprintf "rem(%A)" i
            ) |> String.concat "; "

        "IndexListDelta [" + content + suffix + "]"

    member private x.AsString = x.ToString()

    member x.Collect (mapping : Index -> ElementOperation<'T> -> IndexListDelta<'T2>) =
        let mutable res = IndexListDelta<'T2>.Empty
        for (KeyValue(i,v)) in content do
            res <- res.Combine(mapping i v)

        res

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module IndexListDelta =
    let empty<'T> = IndexListDelta<'T>.Empty

    let inline isEmpty (l : IndexListDelta<'T>) = l.IsEmpty

    let inline add (i : Index) (v : ElementOperation<'T>) (l : IndexListDelta<'T>) = l.Add(i, v)
    let inline remove (i : Index) (l : IndexListDelta<'T>) = l.Remove(i)

    let internal ofMap (m : MapExt<Index, ElementOperation<'T>>) = IndexListDelta(m)
    
    let single (i : Index) (op : ElementOperation<'T>) = IndexListDelta(MapExt.singleton i op)
    let ofSeq (s : seq<Index * ElementOperation<'T>>) = IndexListDelta(MapExt.ofSeq s)
    let ofList (s : list<Index * ElementOperation<'T>>) = IndexListDelta(MapExt.ofList s)
    let ofArray (s : array<Index * ElementOperation<'T>>) = IndexListDelta(MapExt.ofArray s)

    let inline toSeq (l : IndexListDelta<'T>) = l.ToSeq()
    let inline toList (l : IndexListDelta<'T>) = l.ToList()
    let inline toArray (l : IndexListDelta<'T>) = l.ToArray()

    
    let inline mapMonotonic (mapping : Index -> ElementOperation<'T1> -> Index * ElementOperation<'T2>) (l : IndexListDelta<'T1>) = 
        l.MapMonotonic mapping

    let inline map (mapping : Index -> ElementOperation<'T1> -> ElementOperation<'T2>) (l : IndexListDelta<'T1>) = 
        l.Map mapping
        
    let inline choose (mapping : Index -> ElementOperation<'T1> -> Option<ElementOperation<'T2>>) (l : IndexListDelta<'T1>) = 
        l.Choose mapping

    let inline combine (l : IndexListDelta<'T1>) (r : IndexListDelta<'T1>) =
        l.Combine(r)

    let inline collect (mapping : Index -> ElementOperation<'T1> -> IndexListDelta<'T2>) (l : IndexListDelta<'T1>) = 
        l.Collect mapping
        
    let inline filter (predicate : Index -> ElementOperation<'T> -> bool) (l : IndexListDelta<'T>) =
        l.Filter predicate
