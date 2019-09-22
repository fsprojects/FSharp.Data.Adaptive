namespace FSharp.Control.Incremental.Reference

open FSharp.Control.Incremental
open FSharp.Control.Incremental.Reference

type aset<'a> =
    abstract member GetContent : unit -> HashSet<'a>

type cset<'a>(value : HashSet<'a>) =
    let mutable content = value

    member x.IsEmpty = content.IsEmpty
    member x.Count = content.Count

    member x.Contains(value : 'a) = HashSet.contains value content

    member x.GetContent() =
        content

    member x.Add(value : 'a) =
        let w = HashSet.contains value content
        content <- HashSet.add value content
        not w

    member x.Remove(value : 'a) =
        let w = HashSet.contains value content
        content <- HashSet.remove value content
        w
        
    member x.Clear() =
        content <- HashSet.empty
        
    member x.UnionWith (other : seq<'a>) =
        content <- HashSet.union content (HashSet.ofSeq other)
        
    member x.ExceptWith (other : seq<'a>) =
        content <- HashSet.difference content (HashSet.ofSeq other)

    member x.Value 
        with get() = content
        and set v = content <- v

    interface aset<'a> with
        member x.GetContent() = x.GetContent()

    new() = cset<'a>(HashSet.empty)
    new(es : seq<'a>) = cset(HashSet.ofSeq es)

module ASet =
    
    let empty<'a> =
        { new aset<'a> with member x.GetContent() = HashSet.empty }

    let single (value : 'a) =
        let c = HashSet.single value
        { new aset<'a> with member x.GetContent() = c }

    let ofSeq (values : seq<'a>) =
        let c = HashSet.ofSeq values
        { new aset<'a> with member x.GetContent() = c }

    let ofList (values : list<'a>) =
        let c = HashSet.ofList values
        { new aset<'a> with member x.GetContent() = c }

    let ofArray (values : 'a[]) =
        let c = HashSet.ofArray values
        { new aset<'a> with member x.GetContent() = c }

    let ofHashSet (values : HashSet<'a>) =
        { new aset<'a> with member x.GetContent() = values }

    let toARef (set : aset<'a>) =
        { new aref<HashSet<'a>> with member x.GetValue() = set.GetContent() }

    let map (mapping : 'a -> 'b) (set : aset<'a>) =
        { new aset<'b> with 
            member x.GetContent() =  
                set.GetContent() |> HashSet.map mapping
        }
        
        