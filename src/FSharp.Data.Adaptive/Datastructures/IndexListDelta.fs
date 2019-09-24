namespace FSharp.Data.Adaptive

open System.Collections
open System.Collections.Generic

[<StructuredFormatDisplay("{AsString}")>]
[<Struct; StructuralEquality; NoComparison>]
type IndexListDelta< [<EqualityConditionalOn>] 'a> internal(content : MapExt<Index, ElementOperation<'a>>) =
    static let empty = IndexListDelta<'a>(MapExt.empty)

    static member Empty = empty

    member internal x.Content = content

    member x.Count = content.Count

    member x.IsEmpty = content.IsEmpty

    member x.Add(i : Index, op : ElementOperation<'a>) =
        IndexListDelta(MapExt.add i op content)

    member x.Remove(i : Index) =
        IndexListDelta(MapExt.remove i content)

    member x.ToSeq() = content |> MapExt.toSeq
    member x.ToList() = content |> MapExt.toList
    member x.ToArray() = content |> MapExt.toArray

    member x.Combine(r : IndexListDelta<'a>) =
        if x.IsEmpty then r
        elif r.IsEmpty then x
        else MapExt.unionWith (fun l r -> r) x.Content r.Content |> IndexListDelta

    member x.Map(mapping : Index -> ElementOperation<'a> -> ElementOperation<'b>) =
        IndexListDelta(MapExt.map mapping content)
        
    member x.Choose(mapping : Index -> ElementOperation<'a> -> Option<ElementOperation<'b>>) =
        IndexListDelta(MapExt.choose mapping content)

    member x.MapMonotonic(mapping : Index -> ElementOperation<'a> -> Index * ElementOperation<'b>) =
        IndexListDelta(MapExt.mapMonotonic mapping content)

    member x.Filter(mapping : Index -> ElementOperation<'a> -> bool) =
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

    member x.Collect (mapping : Index -> ElementOperation<'a> -> IndexListDelta<'b>) =
        let mutable res = IndexListDelta<'b>.Empty
        for (KeyValue(i,v)) in content do
            res <- res.Combine(mapping i v)

        res

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module IndexListDelta =
    let empty<'a> = IndexListDelta<'a>.Empty

    let inline isEmpty (l : IndexListDelta<'a>) = l.IsEmpty

    let inline add (i : Index) (v : ElementOperation<'a>) (l : IndexListDelta<'a>) = l.Add(i, v)
    let inline remove (i : Index) (l : IndexListDelta<'a>) = l.Remove(i)

    let internal ofMap (m : MapExt<Index, ElementOperation<'a>>) = IndexListDelta(m)
    
    let single (i : Index) (op : ElementOperation<'a>) = IndexListDelta(MapExt.singleton i op)
    let ofSeq (s : seq<Index * ElementOperation<'a>>) = IndexListDelta(MapExt.ofSeq s)
    let ofList (s : list<Index * ElementOperation<'a>>) = IndexListDelta(MapExt.ofList s)
    let ofArray (s : array<Index * ElementOperation<'a>>) = IndexListDelta(MapExt.ofArray s)

    let inline toSeq (l : IndexListDelta<'a>) = l.ToSeq()
    let inline toList (l : IndexListDelta<'a>) = l.ToList()
    let inline toArray (l : IndexListDelta<'a>) = l.ToArray()

    
    let inline mapMonotonic (mapping : Index -> ElementOperation<'a> -> Index * ElementOperation<'b>) (l : IndexListDelta<'a>) = 
        l.MapMonotonic mapping

    let inline map (mapping : Index -> ElementOperation<'a> -> ElementOperation<'b>) (l : IndexListDelta<'a>) = 
        l.Map mapping
        
    let inline choose (mapping : Index -> ElementOperation<'a> -> Option<ElementOperation<'b>>) (l : IndexListDelta<'a>) = 
        l.Choose mapping

    let inline combine (l : IndexListDelta<'a>) (r : IndexListDelta<'a>) =
        l.Combine(r)

    let inline collect (mapping : Index -> ElementOperation<'a> -> IndexListDelta<'b>) (l : IndexListDelta<'a>) = 
        l.Collect mapping
        
    let inline filter (predicate : Index -> ElementOperation<'a> -> bool) (l : IndexListDelta<'a>) =
        l.Filter predicate
