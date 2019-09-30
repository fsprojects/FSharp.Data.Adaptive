#if COMPILED
namespace FSharp.Data.Adaptive.Tests.DomNode
#else
#r @"C:\GitHub\dsyme\FSharp.Data.Adaptive\bin\Debug\netstandard2.0\FSharp.Data.Adaptive.dll"
#endif

open FSharp.Data.Adaptive

[<AllowNullLiteral>]
type HTMLElement(tag: string) =
    let kids = HTMLElements()
    member __.createElement(tag: string) = 
        printfn "HTMLElement.createElement(%s)" tag
        HTMLElement(tag) 
    member __.children = kids
    member __.remove () =
        printfn "HTMLElement(%s).remove () " tag 
        ()
    member __.innerText 
       with get () = "" 
       and set (v: string) = 
           printfn "HTMLElement(%s).innerText <- %s" tag v 
           () 
    member __.insertBefore (x: HTMLElement, y: HTMLElement) = 
        printfn "HTMLElement(%s).insertBefore(...)" tag
        ()
    member __.appendChild (x: HTMLElement) = 
        printfn "HTMLElement(%s).appendChild(...)" tag
        ()
    member __.removeAttribute (x: string) = 
        printfn "HTMLElement(%s).removeAttribute(%s)" tag x 
        ()
    member __.setAttribute (x: string, v: string) = 
        printfn "HTMLElement(%s).setAttribute(%s, %s)" tag x v 
        ()
and HTMLElements() =
    let kids : HTMLElement[] = [| |]
    member __.length = 0
    member __.remove (e: HTMLElement) = ()
    member __.Item with get v = kids.[v]

[<AutoOpen>]
module Document = 
    let document = HTMLElement("document")

type AttributeValue =
    | String of string
        
type AttributeMap(values : amap<string, AttributeValue>) =  

    static let union (key : string) (l : AttributeValue) (r : AttributeValue) =
        match key with
        | "class" ->
            match l, r with
            | String l, String r -> String (l + " " + r)
        | "style" ->
            match l, r with
            | String l, String r -> String (l + "; " + r)
        | _ -> r

    member x.Values = values

    member x.Add(key : string, value : AttributeValue) = 
        let v = AMap.ofList [key, value]
        AttributeMap(AMap.unionWith union values v)

    member x.Add(key : string, value : string) = 
        x.Add(key, String value)
        
    static member Empty = AttributeMap(AMap.empty)
    static member Single(key : string, value : AttributeValue) = AttributeMap(AMap.ofList [key, value])

    static member Union(l : AttributeMap, r : AttributeMap) =
        AttributeMap(AMap.unionWith union l.Values r.Values)

    static member OfAMap (map : amap<string, AttributeValue>) =
        AttributeMap map

    static member OfSeq (seq : seq<string * AttributeValue>) =
        let merge (o : Option<AttributeValue>) (key : string, n : AttributeValue) =
            match o with
            | Some o -> union key o n |> Some
            | None -> n |> Some

        seq
        |> Seq.groupBy fst
        |> Seq.map (fun (key, vs) -> key, vs |> Seq.fold merge None |> FSharp.Core.Option.get)
        |> AMap.ofSeq
        |> AttributeMap

    static member OfList (list : list<string * AttributeValue>) = AttributeMap.OfSeq list
    static member OfArray (arr : array<string * AttributeValue>) = AttributeMap.OfSeq arr
       
module AttributeMap =   
    let empty = AttributeMap.Empty

    let inline ofSeq (seq : seq<string * AttributeValue>) = AttributeMap.OfSeq seq
    let inline ofList (list : list<string * AttributeValue>) = AttributeMap.OfList list
    let inline ofArray (arr : array<string * AttributeValue>) = AttributeMap.OfArray arr

    let inline single (key : string) (value : AttributeValue) = AttributeMap.Single(key, value)
    let inline add (key : string) (value : AttributeValue) (m : AttributeMap) = m.Add(key, value)

    let inline union (l : #seq<AttributeMap>) = 
        let mutable res = empty
        for e in l do
            res <- AttributeMap.Union(res, e)
        res

    let inline toAMap (m : AttributeMap) = m.Values

[<AutoOpen>]
module AttributeMapBuilder =
    type AttributeMapBuilder() =
        member inline x.Yield((key : string, value : string)) = AttributeMap.single key (String value)
        member inline x.Yield((key : string, value : AttributeValue)) = AttributeMap.single key value

        member inline x.Yield((key : string, value : aval<string>)) = value |> AMap.bind (fun v -> AMap.ofList [key, String v]) |> AttributeMap
        member inline x.Yield((key : string, value : aval<AttributeValue>)) = value |> AMap.bind (fun v -> AMap.ofList [key, v]) |> AttributeMap
        
        member inline x.Yield((key : string, value : aval<Option<string>>)) = value |> AMap.bind (function Some v -> AMap.ofList [key, String v] | _ -> AMap.empty) |> AttributeMap
        member inline x.Yield((key : string, value : aval<Option<AttributeValue>>)) = value |> AMap.bind (function Some v -> AMap.ofList [key, v] | _ -> AMap.empty) |> AttributeMap

        member inline x.YieldFrom(m : AttributeMap) = m
        member inline x.YieldFrom(m : seq<string * AttributeValue>) = AttributeMap.ofSeq m
        member inline x.YieldFrom(m : amap<string, AttributeValue>) = AttributeMap m
        
        member inline x.YieldFrom(m : seq<string * string>) = m |> Seq.map (fun (k,v) -> k, String v) |> AttributeMap.ofSeq
        member inline x.YieldFrom(m : amap<string, string>) = m |> AMap.map (fun k v -> String v) |> AttributeMap
        
        member inline x.Combine (l : AttributeMap, r : AttributeMap) = AttributeMap.union [l;r]
        member inline x.Delay(f : unit -> AttributeMap) = f()
        member inline x.Zero() = AttributeMap.empty
        member inline x.Bind(m : aval<'a>, f : 'a -> AttributeMap) = m |> AMap.bind (fun v -> f(v).Values) |> AttributeMap

        member inline x.For(s : seq<'a>, f : 'a -> AttributeMap) = 
            s |> Seq.map f |> AttributeMap.union

        member inline x.For(s : aset<'a>, f : 'a -> AttributeMap) = 
            s |> ASet.map f |> ASet.toAVal |> AMap.bind (AttributeMap.union >> AttributeMap.toAMap) |> AttributeMap

        member inline x.For(s : alist<'a>, f : 'a -> AttributeMap) = 
            s |> AList.map f |> AList.toAVal |> AMap.bind (AttributeMap.union >> AttributeMap.toAMap) |> AttributeMap
            
    let attributes = AttributeMapBuilder()
    let inline att k v = (k,v)

[<RequireQualifiedAccess>]
type DomNode =
    internal
    | NEmpty
    | NText of tag : string * attributes : AttributeMap * value : aval<string>
    | NNode of tag : string * attributes : AttributeMap * children : alist<DomNode>
    
    member x.Attributes : AttributeMap = 
        match x with
        | NEmpty -> AttributeMap.empty
        | NText(_,a,_) -> a 
        | NNode(_,a,_) -> a
    
    member x.WithAttributes(map : AttributeMap -> AttributeMap) : DomNode =
        match x with
        | NEmpty -> NEmpty
        | NText(tag, att, v) -> NText(tag, map att, v)
        | NNode(tag, att, c) -> NNode(tag, map att, c)

    member x.TagName : Option<string> =
        match x with
        | NEmpty -> None
        | NText(t,_,_) -> Some t
        | NNode(t,_,_) -> Some t

    static member Empty : DomNode = NEmpty

    static member Text(tag : string, attributes : AttributeMap, value : aval<string>) = DomNode.NText(tag, attributes, value)
    static member Node(tag : string, attributes : AttributeMap, children : alist<DomNode>) = DomNode.NNode(tag, attributes, children)

module Updater = 

    type Scope = NoScope

    [<AbstractClass>] 
    type NodeUpdater(s : Scope) =
        inherit AdaptiveObject()


        static let create (scope : Scope) (createNode : string -> HTMLElement) (n : DomNode) =
            match n with
            | DomNode.NEmpty -> EmptyUpdater(scope) :> NodeUpdater
            | DomNode.NText(a,b,c) -> TextUpdater(scope, createNode(a), a, b, c) :> NodeUpdater
            | DomNode.NNode(a,b,c) -> InnerNodeUpdater(scope, createNode(a), a, b, c) :> NodeUpdater

        static member Create(parent : HTMLElement, scope : Scope, n : DomNode) =
            let createNode (tag : string) =
                let n = document.createElement(tag)
                let arr = FSharp.Collections.Array.init (int parent.children.length) (fun i -> parent.children.[i])
                for c in arr do c.remove()
                parent.appendChild n |> ignore
                n
            create scope createNode n
            
        static member Create(createNode : string -> HTMLElement, scope : Scope, n : DomNode) =
            create scope createNode n

        member x.Scope = s

        abstract member Node : HTMLElement
        abstract member Compute : AdaptiveToken -> unit
        abstract member Kill : unit -> unit
        abstract member TryReplace : AdaptiveToken * DomNode  -> bool

        member x.Update(t : AdaptiveToken) =
            x.EvaluateIfNeeded t () (fun t ->
                x.Compute(t)
            )

        member x.Destroy() =
            let foo = ref 0
            x.Kill()

    and AttributeUpdater(node : HTMLElement, attributes : AttributeMap) =
        inherit AdaptiveObject()
        let mutable attributes = attributes
        let mutable reader = attributes.Values.GetReader()
        let mutable shutdown : list<System.IDisposable> = []

        let update (ops : HashMapDelta<string,AttributeValue>) (s : Scope) =
            for (k, o) in ops do
                match o with
                | Remove ->
                        node.removeAttribute k

                | Set value ->
                    match value with
                    | AttributeValue.String v -> 
                        node.setAttribute(k, v)

        member x.Replace(newAttributes : AttributeMap, t : AdaptiveToken, s : Scope) =
            x.EvaluateAlways t (fun t ->
                let newReader = newAttributes.Values.GetReader()
                let _ = newReader.GetChanges(t)
                let ops = HashMap.computeDelta reader.State newReader.State

                attributes <- newAttributes
                reader <- newReader
                shutdown |> List.iter (fun d -> d.Dispose())
                shutdown <- []
                update ops s
            )

        member x.UpdateAttributes(t : AdaptiveToken, s : Scope) =
            x.EvaluateIfNeeded t () (fun t -> 
                let ops = reader.GetChanges t
                update ops s
            )

        member x.Destroy() =
            for (k, _) in reader.State do
                    node.removeAttribute k

    and EmptyUpdater(s : Scope) =
        inherit NodeUpdater(s)

        override x.Node = null
        override x.Compute(_) = ()
        override x.Kill() = ()
        override x.TryReplace (_t : AdaptiveToken, n : DomNode) = 
            match n with
            | DomNode.NEmpty -> true
            | _ -> false

    and TextUpdater(scope : Scope, node : HTMLElement, tag : string, attributes : AttributeMap, content : aval<string>) =
        inherit NodeUpdater(scope)

        let mutable attributes = attributes
        let mutable content = content

        let mutable lastValue = None
        let att = AttributeUpdater(node, attributes)
        
        override x.Node = node
        override x.Compute(t) =
            let v = content.GetValue t
            match lastValue with
            | Some o when o = v -> ()
            | _ ->
                //Log.warn "%s.innerText = \"%s\"" node.tagName v
                node.innerText <- v
                lastValue <- Some v
            
            att.UpdateAttributes(t, scope)

        override x.Kill() =
            att.Destroy()
            node.innerText <- ""
            lastValue <- None
            
        override x.TryReplace (t : AdaptiveToken, n : DomNode) =
            match n with
            | DomNode.NText(nt, a, c) when nt = tag ->
                x.EvaluateAlways t (fun t ->
                    att.Replace(a, t, scope)
                    attributes <- a

                    //content.RemoveOutput x // TBD
                    content <- c
                    let v = c.GetValue t
                    match lastValue with
                    | Some o when o = v -> ()
                    | _ ->
                        //Log.warn "repl %s.innerText = \"%s\"" node.tagName v
                        node.innerText <- v
                        lastValue <- Some v

                    true
                )
            | _ ->
                false

    and InnerNodeUpdater(scope : Scope, node : HTMLElement, tag : string, attributes : AttributeMap, children : alist<DomNode>) =
        inherit NodeUpdater(scope)

        let mutable attributes = attributes
        let mutable children = children
        let mutable reader = children.GetReader()

        let mutable nodes : MapExt<Index, NodeUpdater> = MapExt.empty
        let att = AttributeUpdater(node, attributes)
        
        let update (t : AdaptiveToken) (ops : IndexListDelta<DomNode>) =
            for (i, op) in IndexListDelta.toSeq ops do
                match op with
                | Remove ->
                    match MapExt.tryRemove i nodes with
                    | Some (u, rest) ->
                        nodes <- rest
                        u.Destroy()
                        if unbox u.Node then
                            //Log.warn "remove %s" u.Node.tagName
                            u.Node.remove()
                    | None ->
                        ()
                        //Log.warn "strange"

                | Set value ->
                    let (_l, s, r) = MapExt.neighbours i nodes

                    let insert (tag : string) =
                        //Log.warn "insert %s" tag
                        match r with
                        | Some (_ri, r) when unbox r.Node ->
                            let n = document.createElement(tag)
                            node.insertBefore(n, r.Node) |> ignore
                            n
                                
                        | _ ->
                            let n = document.createElement(tag)
                            node.appendChild n |> ignore
                            n

                    match s with
                    | Some (_,s) ->
                        if not (s.TryReplace(t, value)) then
                            s.Destroy()
                            if unbox s.Node then s.Node.remove()
                            let n = NodeUpdater.Create(insert, scope, value)
                            n.Update(t)
                            nodes <- MapExt.add i n nodes

                    | None ->
                        let n = NodeUpdater.Create(insert, scope, value)
                        n.Update(t)
                        nodes <- MapExt.add i n nodes

            for (_,s) in MapExt.toSeq nodes do
                s.Update(t)

        override x.Node = node
        override x.Compute(t) =

            let ops = reader.GetChanges t
            update t ops
            att.UpdateAttributes(t, scope)
            
        override x.Kill() =
            att.Destroy()

            for (k, v) in MapExt.toSeq nodes do
                v.Destroy()
                if unbox v.Node then v.Node.remove() 

            nodes <- MapExt.empty


        override x.TryReplace (t : AdaptiveToken, n : DomNode) =
            match n with
            | DomNode.NNode(ntag, natt, nchildren) when tag = ntag ->
                x.EvaluateAlways t (fun t ->
                    att.Replace(natt, t, scope)
                    attributes <- natt

                    let r = nchildren.GetReader()
                    let _ = r.GetChanges t
                    let nState = MapExt.ofSeq r.State.AsSeqIndexed 
                    let oState = nodes

                    let tryUpdate (key : Index) (o : Option<NodeUpdater>) (n : Option<DomNode>) =
                        match o, n with
                        | Some o, Some n -> 
                            if o.TryReplace(t, n) then
                                None
                            else
                                Some (ElementOperation.Set n)
                        | None, Some n ->
                            Some (ElementOperation.Set n)
                        | Some o, None ->
                            nodes <- MapExt.remove key nodes
                            o.Destroy()
                            Some (ElementOperation.Remove)
                        | None, None ->
                            None

                    let deltas = MapExt.choose2 tryUpdate oState nState |> IndexListDelta.ofMap

                    children <- nchildren
                    reader <- r

                    update t deltas
                    true
                )
            | _ ->
                false

    and MapUpdater(scope : Scope, inner : DomNode, createNode : string -> HTMLElement (* , mapping : obj -> seq *)) =
        inherit NodeUpdater(scope)

        //let mutable mapping = mapping
        let innerScope = scope
        //let innerScope =
        //    { 
        //        app = scope.app
        //        emit = fun v -> v |> Seq.collect mapping |> scope.emit
        //        subscribeObject = scope.subscribeObject
        //    }

        let inner = NodeUpdater.Create(createNode, innerScope, inner)
        
        override x.Kill() = inner.Destroy()
        override x.Node = inner.Node
        override x.Compute(t) = x.EvaluateIfNeeded t () (fun t -> inner.Update(t))
        
        override x.TryReplace (t : AdaptiveToken, n : DomNode) =
                false

module DomNode =

    let inline text (tag : string) (att : AttributeMap) (value : aval<string>) = DomNode.Text(tag, att, value)
    let inline node (tag : string) (att : AttributeMap) (children : alist<DomNode>) = DomNode.Node(tag, att, children)
    let inline tag (n : DomNode) = n.TagName


module Test = 
    let text1 = cval "hello"
    let text1b = cval "goodbye"
    let node1 =  DomNode.Text("a", AttributeMap.OfAMap AMap.empty, text1)
    let node1b =  DomNode.Text("b", AttributeMap.OfAMap AMap.empty, text1b)
    let node2nodes = clist [ node1 ]
    let node2 =  DomNode.Node("c", AttributeMap.OfAMap AMap.empty, node2nodes)
    let updater = Updater.NodeUpdater.Create(document, Updater.NoScope, node2)
    updater.Compute(AdaptiveToken.Top)

    transact (fun () -> text1.Value <- "hello world")
    updater.Compute(AdaptiveToken.Top)
    transact (fun () -> text1b.Value <- "goodbye")
    updater.Compute(AdaptiveToken.Top)
    transact (fun () -> node2nodes.Append node1b)
    updater.Compute(AdaptiveToken.Top)
    