#if COMPILED
namespace FSharp.Data.Adaptive.Tests.DomNode
#else
#r @"C:\GitHub\dsyme\FSharp.Data.Adaptive\bin\Debug\netstandard2.0\FSharp.Data.Adaptive.dll"
#endif

//---------------------------------------------------------------------------
// The purpose of this sample is to show how to define a tree of
// adaptive data, where each node carries an attribute map and a
// list of children.  A node-updater is then defined that tracks changes in the 
// tree and execute corresponding maintenance on pseudo-HTML elements,
// as happens in a DOM implemention.
//
// The end result is an adaptive DOM.
//
// The example can be extended in numerous ways, this is a basic outline.

open FSharp.Data.Adaptive

type AttributeValue =
    | String of string
        
type AttributeMap(values: amap<string, AttributeValue>) =  

    static let empty = AttributeMap(AMap.empty)

    static let union (key: string) (l: AttributeValue) (r: AttributeValue) =
        match key with
        | "class" ->
            match l, r with
            | String l, String r -> String (l + " " + r)
        | "style" ->
            match l, r with
            | String l, String r -> String (l + "; " + r)
        | _ -> r

    member x.Values = values

    member x.Add(key: string, value: AttributeValue) = 
        let v = AMap.ofList [key, value]
        AttributeMap(AMap.unionWith union values v)

    member x.Add(key: string, value: string) = 
        x.Add(key, String value)
        
    static member Empty = empty

    static member Single(key: string, value: AttributeValue) =
        AttributeMap(AMap.ofList [key, value])

    static member Union(l: AttributeMap, r: AttributeMap) =
        AttributeMap(AMap.unionWith union l.Values r.Values)

    static member OfSeq (seq: seq<string * AttributeValue>) =
        let merge (o: Option<AttributeValue>) (key: string, n: AttributeValue) =
            match o with
            | Some o -> union key o n |> Some
            | None -> n |> Some

        seq
        |> Seq.groupBy fst
        |> Seq.map (fun (key, vs) -> key, vs |> Seq.fold merge None |> FSharp.Core.Option.get)
        |> AMap.ofSeq
        |> AttributeMap

       
module AttributeMap =   
    let empty = AttributeMap.Empty

    let ofSeq seq = AttributeMap.OfSeq seq

    let ofList list = AttributeMap.OfSeq list

    let ofArray arr = AttributeMap.OfSeq arr

    let single key value = AttributeMap.Single(key, value)

    let add (key: string) (value: AttributeValue) (m: AttributeMap) = m.Add(key, value)

    let union (l: seq<AttributeMap>) = 
        let mutable res = empty
        for e in l do
            res <- AttributeMap.Union(res, e)
        res

    let toAMap (m: AttributeMap) = m.Values


[<AutoOpen>]
module AttributeMapBuilder =
    type AttributeMapBuilder() =
        member x.Yield((key: string, value: string)) = AttributeMap.single key (String value)

        member x.Yield((key: string, value: AttributeValue)) = AttributeMap.single key value

        member x.Yield((key: string, value: aval<string>)) =
            value |> AMap.bind (fun v -> AMap.ofList [key, String v]) |> AttributeMap

        member x.Yield((key: string, value: aval<AttributeValue>)) =
            value |> AMap.bind (fun v -> AMap.ofList [key, v]) |> AttributeMap
        
        member x.Yield((key: string, value: aval<Option<string>>)) =
            value |> AMap.bind (function Some v -> AMap.ofList [key, String v] | _ -> AMap.empty) |> AttributeMap

        member x.Yield((key: string, value: aval<Option<AttributeValue>>)) =
            value |> AMap.bind (function Some v -> AMap.ofList [key, v] | _ -> AMap.empty) |> AttributeMap

        member x.YieldFrom(m: AttributeMap) = m
        member x.YieldFrom(m: seq<string * AttributeValue>) = AttributeMap.ofSeq m
        member x.YieldFrom(m: amap<string, AttributeValue>) = AttributeMap m
        
        member x.YieldFrom(m: seq<string * string>) = m |> Seq.map (fun (k, v) -> k, String v) |> AttributeMap.ofSeq
        member x.YieldFrom(m: amap<string, string>) = m |> AMap.map (fun k v -> String v) |> AttributeMap
        
        member x.Combine (l: AttributeMap, r: AttributeMap) = AttributeMap.union [l;r]
        member x.Delay(f: unit -> AttributeMap) = f()
        member x.Zero() = AttributeMap.empty
        member x.Bind(m: aval<'a>, f: 'a -> AttributeMap) = m |> AMap.bind (fun v -> f(v).Values) |> AttributeMap

        member x.For(s: seq<'a>, f: 'a -> AttributeMap) = 
            s |> Seq.map f |> AttributeMap.union

        member x.For(s: aset<'a>, f: 'a -> AttributeMap) = 
            s |> ASet.map f |> ASet.toAVal |> AMap.bind (AttributeMap.union >> AttributeMap.toAMap) |> AttributeMap

        member x.For(s: alist<'a>, f: 'a -> AttributeMap) = 
            s |> AList.map f |> AList.toAVal |> AMap.bind (AttributeMap.union >> AttributeMap.toAMap) |> AttributeMap
            
    let attribs = AttributeMapBuilder()
    let att k v = (k, v)

/// Represents a tree of adaptive data 
[<RequireQualifiedAccess>]
type DomNode =
    | Node of tag: string * attribs: AttributeMap * children: alist<DomNode>
    
    member x.Attributes = 
        match x with
        | Node(_, attribs, _) -> attribs
    
    member x.TagName =
        match x with
        | Node(tag, _, _) -> Some tag

/// The pseudo-HTML DOM type we will maintain in response to updates.
[<AllowNullLiteral>]
type HTMLElement(tag: string) =

    static let document = HTMLElement("document")
    member __.tag = tag

    member __.createElement(tag: string) = 
        printfn "%s = createElement()" tag
        HTMLElement(tag) 

    member __.children : HTMLElement[] = [| |]

    member __.remove () =
        printfn "%s.remove() " tag 

    member __.insertBefore (x: HTMLElement, y: HTMLElement) = 
        printfn "%s.insertBefore(%s, %s)" x.tag y.tag

    member __.appendChild (x: HTMLElement) = 
        printfn "%s.appendChild(%s)" tag x.tag

    member __.removeAttribute (x: string) = 
        printfn "%s.removeAttribute(%s)" tag x 

    member __.setAttribute (x: string, v: string) = 
        printfn "%s.setAttribute(%O, %O)" tag x v 

    static member Document = document

module Updater = 

    type AttributeUpdater(node: HTMLElement, attribs: AttributeMap) =
        inherit AdaptiveObject()
        let mutable reader = attribs.Values.GetReader()

        let update (ops: HashMapDelta<string, AttributeValue>) =
            for (k, o) in ops do
                match o with
                | Remove ->
                    node.removeAttribute k

                | Set value ->
                    match value with
                    | AttributeValue.String v -> 
                        node.setAttribute(k, v)

        /// Do wholesale replacement of the attributes and apply appropriate deltas
        member x.ReplaceAttributes(tok: AdaptiveToken, newAttributes: AttributeMap) =
            x.EvaluateAlways tok (fun tok ->
                let newReader = newAttributes.Values.GetReader()

                // Roll forward to current since we're about to fully recompute the delta
                let _ = newReader.GetChanges(tok)

                let ops = HashMap.computeDelta reader.State newReader.State

                reader <- newReader
                update ops
            )

        member x.UpdateAttributes(tok: AdaptiveToken) =
            x.EvaluateIfNeeded tok () (fun tok -> 
                let ops = reader.GetChanges tok
                update ops
            )

        member x.Destroy() =
            for (k, _) in reader.State do
                node.removeAttribute k

    type NodeUpdater(htmlNode: HTMLElement, tag: string, attribs: AttributeMap, children: alist<DomNode>) =
        inherit AdaptiveObject()
        let mutable reader = children.GetReader()

        let mutable innerNodes: MapExt<Index, NodeUpdater> = MapExt.empty
        let attribUpdater = AttributeUpdater(htmlNode, attribs)
        
        let update (tok: AdaptiveToken) (ops: IndexListDelta<DomNode>) =

            for (i, op) in IndexListDelta.toSeq ops do
                match op with
                | Remove ->
                    match MapExt.tryRemove i innerNodes with
                    | Some (removedNode, rest) ->
                        innerNodes <- rest
                        removedNode.HtmlNode.remove()
                    | None ->
                        ()

                // Indicates node at index 'i' is set to 'value' 
                | Set value ->
                    let (_, innerNodeOpt, rightInnerNodeOpt) = MapExt.neighbours i innerNodes

                    let insert (tag: string) =
                        //Log.warn "insert %s" tag
                        match rightInnerNodeOpt with
                        | Some (_, rightInnerNode) ->
                            let newInnerNode = HTMLElement.Document.createElement(tag)
                            htmlNode.insertBefore(newInnerNode, rightInnerNode.HtmlNode) |> ignore
                            newInnerNode
                                
                        | None ->
                            let newInnerNode = HTMLElement.Document.createElement(tag)
                            htmlNode.appendChild newInnerNode |> ignore
                            newInnerNode

                    match innerNodeOpt with
                    | Some (_, innerNode) ->
                        if not (innerNode.TryReplace(tok, value)) then
                            innerNode.HtmlNode.remove()
                            let newInnerNode = NodeUpdater.Create(insert, value)
                            innerNodes <- MapExt.add i newInnerNode innerNodes

                    | None ->
                        let newInnerNode = NodeUpdater.Create(insert, value)
                        innerNodes <- MapExt.add i newInnerNode innerNodes

            for (_, innerNode) in MapExt.toSeq innerNodes do
                innerNode.Update(tok)

        static let create (createNode: string -> HTMLElement) (n: DomNode) =
            match n with
            | DomNode.Node(a, b, c) -> NodeUpdater(createNode a, a, b, c)

        static member Create (parent: HTMLElement, n: DomNode) =
            let createNode (tag: string) =
                let n = HTMLElement.Document.createElement(tag)

                // TBD: suspicious remove of all other children here?
                let arr = Array.copy parent.children
                for c in arr do c.remove()

                parent.appendChild n |> ignore
                n
            create createNode n
            
        static member Create (createNode: string -> HTMLElement, n: DomNode) =
            create createNode n

        member x.Update(tok: AdaptiveToken) =
            x.EvaluateIfNeeded tok () (fun tok ->
                x.Execute(tok)
            )

        /// The HTML element the updater is updating
        member x.HtmlNode : HTMLElement = htmlNode

        /// Execute the updates for the updater
        member x.Execute(tok) =
            let ops = reader.GetChanges tok
            update tok ops
            attribUpdater.UpdateAttributes(tok)
            
        /// Attempt to do in-place replacement of the contents of the target node
        /// by the contents of the given node, if the tags match
        member x.TryReplace (tok: AdaptiveToken, n: DomNode) =
            match n with
            | DomNode.Node(newTag, newAttrs, newChildren) when tag = newTag ->
                x.EvaluateAlways tok (fun tok ->
                    attribUpdater.ReplaceAttributes(tok, newAttrs)

                    let newReader = newChildren.GetReader()

                    // Roll forward since we're about to fully recompute the delta
                    let _ = newReader.GetChanges tok 

                    let newState = MapExt.ofSeq newReader.State.AsSeqIndexed 
                    let oldState = innerNodes

                    let tryUpdate (key: Index) (o: Option<NodeUpdater>) (n: Option<DomNode>) =
                        match o, n with
                        | Some o, Some n -> 
                            if o.TryReplace(tok, n) then
                                None
                            else
                                Some (ElementOperation.Set n)
                        | None, Some n ->
                            Some (ElementOperation.Set n)
                        | Some o, None ->
                            innerNodes <- MapExt.remove key innerNodes
                            Some (ElementOperation.Remove)
                        | None, None ->
                            None

                    let deltas = MapExt.choose2 tryUpdate oldState newState |> IndexListDelta.ofMap

                    reader <- newReader

                    update tok deltas
                    true
                )
            | _ ->
                false

module DomNode =

    let text (tag: string) (text: aval<string>) =
        DomNode.Node(tag, attribs { yield  "text", text }, AList.empty)

    let node (tag: string) (attrs: AttributeMap) (children: alist<DomNode>) =
        DomNode.Node(tag, attrs, children)

    let tag (n: DomNode) = n.TagName

module Test = 
    let text1 = cval "hello"
    let text1b = cval "goodie"
    let node1 =  DomNode.text "a" text1
    let node1b =  DomNode.text "b" text1b
    let node2nodes = clist [ node1 ]
    let node2 =  DomNode.node "c" AttributeMap.Empty node2nodes
    let updater = Updater.NodeUpdater.Create(HTMLElement.Document, node2)
    updater.Execute(AdaptiveToken.Top)

    transact (fun () -> text1.Value <- "hello world")
    updater.Execute(AdaptiveToken.Top)

    transact (fun () -> text1b.Value <- "goodbye")
    updater.Execute(AdaptiveToken.Top)

    transact (fun () -> node2nodes.Append node1b) |> ignore
    updater.Execute(AdaptiveToken.Top)

    transact (fun () -> text1b.Value <- "goodbye lenin")
    updater.Execute(AdaptiveToken.Top)

    updater.Execute(AdaptiveToken.Top)
    