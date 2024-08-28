// Scratch contains sketches for several new ideas and concepts extending FSharp.Data.Adaptive for reviewing purposes.
open System
open System.Collections.Generic
open FSharp.Data.Adaptive
open FSharp.Data.Traceable

//ABag.run()
//Observable.run()
//LookupAll.example()
//AListSub.run()
      

module AdaptiveTree =

    type Node<'a, 'b> =
        {
            attributes : HashMap<string, 'a>
            children : IndexList<Node<'a, 'b>>
            values : IndexList<'b>
        }
        
        member x.IsEmpty =
            x.attributes.IsEmpty &&
            x.children.IsEmpty &&
            x.values.IsEmpty
        
        static member inline Empty : Node<'a, 'b> =
            {
                attributes = HashMap.empty
                children = IndexList.empty
                values = IndexList.empty
            }

        
    type NodeDelta<'a, 'b> =
        {
            dattributes : HashMapDelta<string, 'a>
            dchildren0 : IndexListDelta<Node<'a, 'b>>
            dchildren1 : IndexList<NodeDelta<'a, 'b>>
            dvalues : IndexListDelta<'b>
        }
        
        member x.IsEmpty =
            x.dattributes.IsEmpty &&
            x.dchildren0.IsEmpty &&
            x.dchildren1.IsEmpty &&
            x.dvalues.IsEmpty
        
        static member inline Empty : NodeDelta<'a, 'b> =
            {
                dattributes = HashMapDelta.empty
                dchildren0 = IndexListDelta.empty
                dchildren1 = IndexList.empty
                dvalues = IndexListDelta.empty
            }
    
    module Node =
        open FSharp.Data.Adaptive
        open FSharp.Data.Traceable
        
        [<GeneralizableValue>]
        let empty<'a, 'b> : Node<'a, 'b> =
            Node<'a, 'b>.Empty 

    module NodeDelta =
        
        [<GeneralizableValue>]
        let empty<'a, 'b> : NodeDelta<'a, 'b> =
            NodeDelta<'a, 'b>.Empty 
        
        let inline isEmpty (d : NodeDelta<'a, 'b>) = d.IsEmpty
 
    [<AutoOpen>]
    module ``Node Delta Extensions`` =
        module Node =
            let rec computeDelta (a : Node<'a, 'b>) (b : Node<'a, 'b>) : NodeDelta<'a, 'b> =
                if System.Object.ReferenceEquals(a :> obj, b :> obj) then
                    NodeDelta.empty
                else
                    
                    let datt = HashMap.computeDelta a.attributes b.attributes
                    
                    let inline add (index : Index) (element : Node<'a, 'b>) =
                        Set element
                    
                    let inline remove (index : Index) (element : Node<'a, 'b>) =
                        Remove
                    
                    let mutable dchildren1 = IndexList.empty
                    
                    let inline update (index : Index) (o : Node<'a, 'b>) (n : Node<'a, 'b>) =
                        let d = computeDelta o n
                        if not d.IsEmpty then
                            dchildren1 <- IndexList.set index d dchildren1
                        ValueNone
                        
                    let dchildren = (a.children, b.children) ||> IndexList.computeDeltaCustom add remove update
                    let dvalues = (a.values, b.values) ||> IndexList.computeDelta
                    
                    {
                        dattributes = datt 
                        dchildren0 = dchildren 
                        dchildren1 = dchildren1
                        dvalues = dvalues 
                    }
                    
            let rec applyDelta (a : Node<'a, 'b>) (d : NodeDelta<'a, 'b>) : Node<'a, 'b> * NodeDelta<'a, 'b>=
                if d.IsEmpty then
                    a, NodeDelta.Empty
                else
                    let attributes, dattEff = HashMap.applyDelta a.attributes d.dattributes
                    let mutable children, dchildrenEff = (a.children, d.dchildren0) ||> IndexList.applyDelta
                    
                    let mutable dchildren1Eff = IndexList.empty
                    // TODO: efficient update
                    for idx, delta in IndexList.toSeqIndexed d.dchildren1 do
                        match IndexList.tryGet idx children with
                        | Some oldChild ->
                            let newChild, deff = applyDelta oldChild delta
                            dchildren1Eff <- IndexList.set idx deff dchildren1Eff
                            children <- IndexList.set idx newChild children
                        | None ->
                            ()
                    let values, dvaluesEff = (a.values, d.dvalues) ||> IndexList.applyDelta
                    { attributes = attributes; children = children; values = values },
                    { dattributes = dattEff; dchildren0 = dchildrenEff; dchildren1 = dchildren1Eff; dvalues = dvaluesEff }

            let rec internal combineDelta (a : NodeDelta<'a, 'b>) (b : NodeDelta<'a, 'b>) : NodeDelta<'a, 'b> =
                
                let mutable dchildren0 = IndexListDelta.combine a.dchildren0 b.dchildren0
                
                let dchildren1 =
                    (a.dchildren1, b.dchildren1) ||> IndexList.choose2 (fun idx da db ->
                        match da with
                        | Some a ->
                            match db with
                            | Some b ->
                                let d = combineDelta a b
                                if d.IsEmpty then None
                                else Some d
                            | None ->
                                if b.dchildren0.Content.ContainsKey idx then None
                                else Some a
                        | None ->
                            match db with
                            | Some db ->
                                match MapExt.tryFind idx a.dchildren0.Content with
                                | Some Remove ->
                                    None
                                | Some (Set v) ->
                                    let vn, _ = applyDelta v db
                                    dchildren0 <- IndexListDelta.add idx (Set vn) dchildren0
                                    None
                                | None ->
                                    Some db
                            | None ->
                                None
                    )
                
                {
                    dattributes = HashMapDelta.combine a.dattributes b.dattributes
                    dchildren0 = IndexListDelta.combine a.dchildren0 b.dchildren0
                    dchildren1 = dchildren1
                    dvalues = IndexListDelta.combine a.dvalues b.dvalues
                }        

            let rec map (mapping : 'a -> 'b) (node : Node<'att, 'a>) =
                {
                    attributes = node.attributes
                    children = node.children |> IndexList.map (map mapping)
                    values = node.values |> IndexList.map mapping
                }
            
            
            let trace<'a, 'b> : Traceable<Node<'a, 'b>, NodeDelta<'a, 'b>> =
                {
                    tcomputeDelta = computeDelta
                    tapplyDelta = applyDelta
                    tmonoid =
                        {
                            mempty = NodeDelta.Empty
                            mappend = combineDelta
                            misEmpty = NodeDelta.isEmpty
                        }
                    tempty = Node.empty
                    tsize = fun _ -> 0
                    tprune = None 
                }
     
        module NodeDelta =
            let combine (a : NodeDelta<'a, 'b>) (b : NodeDelta<'a, 'b>) : NodeDelta<'a, 'b> =
                Node.combineDelta a b
            
            let rec map (mapping : 'a -> 'b) (d : NodeDelta<'att, 'a>) =
                let opMap mapping _index op =
                    match op with
                    | Set v -> Set (mapping v)
                    | Remove -> Remove
                
                
                {
                    dattributes = d.dattributes
                    dchildren0 = d.dchildren0 |> IndexListDelta.map (opMap (Node.map mapping))
                    dchildren1 = d.dchildren1 |> IndexList.map (map mapping)
                    dvalues = d.dvalues |> IndexListDelta.map (opMap mapping)
                }
            
            
            let monoid<'a, 'b> : Monoid<NodeDelta<'a, 'b>> =
                {
                    mempty = NodeDelta.Empty
                    mappend = combine
                    misEmpty = NodeDelta.isEmpty
                }   
            
        [<Struct>]
        type Value<'a> = Value of 'a
        
          
        [<AbstractClass; Sealed; AutoOpen>]
        type AttCreators private() =
            static member Att(name : string, value : 'a) = HashMap.single name value
            static member Att(values : list<string * 'a>) = HashMap.ofList values
            
        type NodeBuilder() =
            
            member x.Delay(f : unit -> Node<'a, 'b>) =
                f()
            
            member x.Yield(Value value) =
                { Node.empty with values = IndexList.single value }
            
            member x.Yield(value : IndexList<'a>) =
                { Node.empty with values = value }
            
            member x.Yield(value : Node<'a, 'b>) =
                { Node.empty with children = IndexList.single value }
            
            member x.Yield(atts : HashMap<string, 'b>) =
                { Node.empty with attributes = atts }
            
            member x.Yield((key : string, value : 'a)) =
                { Node.empty with attributes = HashMap.single key value }
            
            member x.Combine(l : Node<'a, 'b>, r : Node<'a, 'b>) =
                {
                    attributes = HashMap.union l.attributes r.attributes
                    children = IndexList.append l.children r.children
                    values = IndexList.append l.values r.values
                }
            
        let node = NodeBuilder()
        
        
        
        type NodeIndex = list<Index>
            
        [<AbstractClass>]
        type Lens<'a, 'b>() =
            abstract Get : 'a -> 'b
            abstract Set : 'a * 'b -> 'a
                
        [<AbstractClass>]
        type Prism<'a, 'b>() =
            inherit Lens<'a, option<'b>>()
            abstract Alter : 'a * (option<'b> -> option<'b>) -> 'a
            default x.Alter(a, update) =
                let v = x.Get(a)
                x.Set(a, update v)
                
        let rec internal createNode (index : list<Index>) (newNode : Node<'a, 'b>) =
            match index with
            | [] -> newNode
            | h :: t ->
                { Node.empty with children = IndexList.set h (createNode t newNode) IndexList.empty }
            
        let lens (index : NodeIndex) =
            
            let rec get (index : list<Index>) (n : Node<'a, 'b>) =
                match index with
                | [] -> Some n
                | h :: t ->
                    match IndexList.tryGet h n.children with
                    | Some c -> get t c
                    | None -> None
                    
            let rec set (index : list<Index>) (n : Node<'a, 'b>) (newNode : option<Node<'a, 'b>>) =
                match index with
                | [] -> newNode
                | h :: t ->
                    match IndexList.tryGet h n.children with
                    | Some c ->
                        match set t c newNode with
                        | Some newChild ->
                            Some { n with children = IndexList.set h newChild n.children }
                        | None ->
                            Some { n with children = IndexList.remove h n.children }
                    | None ->
                        match newNode with
                        | Some newNode ->
                            Some (createNode index newNode)
                        | None ->
                            None
              
            let rec alter (index : list<Index>) (n : Node<'a, 'b>) (update : option<Node<'a, 'b>> -> option<Node<'a, 'b>>) =
                match index with
                | [] ->
                    update (Some n)
                | h :: t ->
                    match IndexList.tryGet h n.children with
                    | Some c ->
                        match alter t c update with
                        | Some newChild ->
                            Some { n with children = IndexList.set h newChild n.children }
                        | None ->
                            Some { n with children = IndexList.remove h n.children }
                    | None ->
                        match update None with
                        | Some newNode ->
                            Some (createNode index newNode)
                        | None ->
                            None
        
            
            { new Prism<Node<'a, 'b>, Node<'a, 'b>>() with
                member x.Get(n : Node<'a, 'b>) =
                    get index n
                member s.Set(node : Node<'a, 'b>, newNode : option<Node<'a, 'b>>) =
                    match set index node newNode with
                    | Some newNode -> newNode
                    | None -> Node.Empty
                    
                member x.Alter(node : Node<'a, 'b>, update : option<Node<'a, 'b>> -> option<Node<'a, 'b>>) =
                    match alter index node update with
                    | Some newNode -> newNode
                    | None -> Node.Empty
                    
            }
           
        let appendChild (index : NodeIndex) (child : Node<'a, 'b>) (parent : Node<'a, 'b>) =
            let l = lens index
            l.Alter (parent, function
                | Some o ->
                    Some { o with children = IndexList.add child o.children }
                | None ->
                    Some { Node.Empty with children = IndexList.single child }
            )
             
        let prependChild (index : NodeIndex) (child : Node<'a, 'b>) (parent : Node<'a, 'b>) =
            let l = lens index
            l.Alter (parent, function
                | Some o ->
                    Some { o with children = IndexList.prepend child o.children }
                | None ->
                    Some { Node.Empty with children = IndexList.single child }
            )
            
        let remove (index : NodeIndex) (parent : Node<'a, 'b>) =
            let lens = lens index
            lens.Set(parent, None)

        
        
        let rec printNode (indent : string) (index : option<Index>) (n : Node<string, 'b>) =
            let att =
                n.attributes
                |> Seq.map (fun (k, v) -> sprintf "%s: \"%s\"" k v)
                |> String.concat ", "
                |> fun s -> if System.String.IsNullOrWhiteSpace s then "" else sprintf " { %s }" s
            
            let name =
                match index with
                | Some i -> string i
                | None -> "root"
            printfn "%s%s%s" indent name att
            
            for v in n.values do
                printfn "%s %A" indent v
            
            for i, c in IndexList.toSeqIndexed n.children do
                printNode (indent + "  ") (Some i) c
        
        let run() =
            let n = Node.empty
            
            let n = n |> appendChild [] { Node.empty with attributes = HashMap.single "Id" "A" }
            let n = n |> appendChild [] { Node.empty with attributes = HashMap.single "Id" "B" }
            let n = n |> appendChild [] { Node.empty with attributes = HashMap.single "Id" "C" }
            
            let i0 = n.children |> IndexList.firstIndex
            let n = n |> appendChild [i0] { Node.empty with attributes = HashMap.single "Id" "A.A" }
            let n = n |> prependChild [i0] { Node.empty with attributes = HashMap.single "Id" "A.0" }
            
            printNode "" None n
            
            
            let n = n |> remove [i0]
            printNode "" None n
            
    type IAdaptiveNode<'a, 'b> =
        abstract GetReader : unit -> IAdaptiveTreeReader<'a, 'b>
        
    and IAdaptiveTreeReader<'a, 'b> = IOpReader<Node<'a, 'b>, NodeDelta<'a, 'b>>
    
    and atree<'a, 'b> = IAdaptiveNode<'a, 'b>
    
    type ctree<'a, 'b>(value : Node<'a, 'b>) =
        let mutable value = value
        let history = History(Node.trace)
        do history.PerformUnsafe(value, Node.computeDelta Node.empty value) |> ignore
        
        member x.Value
            with get() = value
            and set v =
                let d = Node.computeDelta value v
                value <- v
                history.Perform d |> ignore
                
        member x.GetReader() =
            history.NewReader()
        
        interface atree<'a, 'b> with
            member x.GetReader() = x.GetReader()
        
    module ATree =
        
        type AdaptiveTree<'a, 'b>(createReader : unit -> IOpReader<NodeDelta<'a, 'b>>) =
            let history = History.ofReader Node.trace createReader
        
            member x.GetReader() = history.NewReader()
            
            interface atree<'a, 'b> with
                member x.GetReader() = history.NewReader()
        
        type MapReader<'att, 'a, 'b>(mapping : 'a -> 'b, input : IAdaptiveTreeReader<'att, 'a>) =
            inherit AbstractReader<NodeDelta<'att, 'b>>(NodeDelta.empty)
            
            override x.Compute(t : AdaptiveToken) =
                let d = input.GetChanges t
                d |> NodeDelta.map mapping
           
        type internal State<'att, 'value> =
            {
                MinValueIndex : Index
                MinChildIndex : Index
                MaxIndex : Index
                Attributes  : HashMap<string, 'att>
                //SelfResults : Set<Index>
                Children    : MapExt<Index, State<'att, 'value>>
                Values      : MapExt<Index, Index * 'value>
            }
            
            // member x.GetResults() =
            //     let mutable res = x.SelfResults
            //     for KeyValue(_, c) in x.Children do
            //         res <- Set.union res (c.GetResults())
            //         
            //     res
            
            
            
           
           
        type FlattenReader<'att, 'value>(input : atree<'att, 'value>, mergeAttributes : string -> 'att -> 'att -> 'att) =
            inherit AbstractReader<IndexList<HashMap<string, 'att> * 'value>, IndexListDelta<HashMap<string, 'att> * 'value>>(IndexList.trace)
                
            let reader = input.GetReader()
                
                
                
            let mutable root =
                let i0 = Index.zero
                let i1 = Index.after i0
                let i2 = Index.after i1
                {
                    MinValueIndex = i0
                    MinChildIndex = i1
                    MaxIndex = i2
                    Attributes = HashMap.empty
                    //SelfResults = Set.empty
                    Children = MapExt.empty
                    Values = MapExt.empty
                }
                
            override x.Compute(t : AdaptiveToken) =
                let ops = reader.GetChanges t
                
                // let pureUnion (l : MapExt<'k, 'v>) (r : MapExt<'k, 'v>) =
                //     let merge k vl vr =
                //         mergeAttributes k vl vr
                //         // if not (Unchecked.equals vl vr) then
                //         //     failwithf "duplicate entry for %A: %A vs %A" k vl vr
                //         // else
                //         //     vr
                //     MapExt.unionWith merge l r
                
                let rec run (scopeChanged : bool) (scopeAttributes : HashMap<string, 'att>) (state : State<'att, 'value>) (delta : NodeDelta<'att, 'value>) =
                    let attChanged = not (HashMapDelta.isEmpty delta.dattributes)
                    let newAttributes, _ = HashMap.applyDelta state.Attributes delta.dattributes
                    let scopeChanged = scopeChanged || attChanged
                    
                    
                    let mutable state = { state with Attributes = newAttributes }
                    let mutable resultDeltas = MapExt.empty
                    
                    let scopeAttributes = HashMap.unionWith mergeAttributes scopeAttributes newAttributes
               
                    for vi, op in delta.dvalues do
                        match op with
                        | Remove ->
                            match MapExt.tryRemove vi state.Values with
                            | Some ((vii, v), rest) ->
                                // remove
                                state <- { state with Values = rest; } //SelfResults = Set.remove vii state.SelfResults }
                                resultDeltas <- MapExt.add vii Remove resultDeltas
                            | None ->
                                ()
                        | Set v ->
                            match MapExt.tryFind vi state.Values with
                            | Some (vii, oldValue) ->
                                // update
                                state <- { state with Values = MapExt.add vi (vii, v) state.Values }
                                resultDeltas <- MapExt.add vii (Set (scopeAttributes, v)) resultDeltas
                            | None ->
                                // insert
                                
                                let li =
                                    let l, _, _ = MapExt.neighbours vi state.Values
                                    match l with
                                    | Some (_, (li,_)) -> li
                                    | None -> state.MinValueIndex
                                
                                let ri =
                                    let _, _, r = MapExt.neighbours vi state.Values
                                    match r with
                                    | Some (_, (ri,_)) -> ri
                                    | None -> state.MinChildIndex
                                
                                let vii = Index.between li ri
                                //printfn "insert (%A,%A,%A): (%A, %A)=>%A" state.MinValueIndex state.MinChildIndex state.MaxIndex li ri vii
                                state <- { state with Values = MapExt.add vi (vii, v) state.Values; } //SelfResults = Set.add vii state.SelfResults }
                                resultDeltas <- MapExt.add vii (Set (scopeAttributes, v)) resultDeltas
                              
                           
                    for ci, childDelta in IndexList.toSeqIndexed delta.dchildren1 do
                        match MapExt.tryFind ci state.Children with
                        | Some childState ->
                            let newChildState, childDeltas = run scopeChanged scopeAttributes childState childDelta
                            state <- { state with Children = MapExt.add ci newChildState state.Children }
                            resultDeltas <- MapExt.union resultDeltas childDeltas
                        | None ->
                            failwith "cannot update non-existing state"
                            
                    for ci, op in delta.dchildren0 do
                        match op with
                        | Remove ->
                            match MapExt.tryRemove ci state.Children with
                            | Some (childState, rest) ->
                                let s = x.State
                                let rem = s.[childState.MinValueIndex .. childState.MaxIndex]
                                for cci, _ in IndexList.toSeqIndexed rem do
                                    resultDeltas <- MapExt.add cci Remove resultDeltas
                                state <- { state with Children = rest }
                            | None ->
                                // swallow remove of non-existing things
                                ()
                        | Set v ->
                            match MapExt.tryFind ci state.Children with
                            | Some childState ->
                                failwith "update should be in dchildren1"
                            | None ->
                                let l, _, r = MapExt.neighbours ci state.Children
                                
                                let ln =
                                    match l with
                                    | Some (_, ls) -> ls.MaxIndex
                                    | None -> state.MinChildIndex
                                    
                                let rn =
                                    match r with
                                    | Some (_, rs) -> rs.MinValueIndex
                                    | None -> state.MaxIndex
                                
                                let lvi = Index.between ln rn
                                let lci = Index.between lvi rn
                                let ri = Index.between lci rn
                                let childState =
                                    {
                                        MinValueIndex = lvi
                                        MinChildIndex = lci 
                                        MaxIndex = ri
                                        Attributes = HashMap.empty
                                        Children = MapExt.empty
                                        Values = MapExt.empty
                                    }
                                    
                                let delta = Node.computeDelta Node.empty v
                                let newChildState, childDeltas = run scopeChanged scopeAttributes childState delta
                                state <-
                                    { state with
                                        Children = MapExt.add ci newChildState state.Children 
                                    }
                                resultDeltas <- MapExt.union resultDeltas childDeltas

                    if scopeChanged then
                        for KeyValue(_, (vii, v)) in state.Values do
                            resultDeltas <- MapExt.add vii (Set(scopeAttributes, v)) resultDeltas
                          
                        let newStates =
                            state.Children |> MapExt.map (fun _ s ->
                                let newState, res = run true scopeAttributes s NodeDelta.empty
                                resultDeltas <- MapExt.union resultDeltas res
                                newState
                            )
                        state <- { state with Children = newStates }
                                
                                
                
                    state, resultDeltas
                
                let newRoot, delta = run false HashMap.empty root ops
                root <- newRoot
                IndexListDelta delta
             
        let ofReader (create : unit -> #IOpReader<NodeDelta<'att, 'a>>) =
            new AdaptiveTree<'att, 'a>(fun () -> create() :> IOpReader<_>) :> atree<_,_>
            
        
        let map (mapping : 'a -> 'b) (t : atree<_, 'a>) : atree<_, 'b> =
            ofReader <| fun () ->
                MapReader(mapping, t.GetReader())
        
        let flatten (tree : atree<'att, 'value>) : alist<HashMap<string, 'att> * 'value> =
            AList.ofReader <| fun () ->
                FlattenReader(tree, fun _ _ v -> v)
        
        
    
        
    type MutableTree<'att, 'value>(tree : Node<'att, 'value>) as this =
        let tree = ctree tree
        let root = MutableNode(tree.Value, [WeakReference<_> this, []])
        
        member x.Value
            with get() = tree.Value
            and set v = tree.Value <- v
      
        
        member x.Root = root
            
        interface atree<'att, 'value> with
            member x.GetReader() = tree.GetReader()
            
        new() = MutableTree(Node.empty)
            
    and MutableNode<'att, 'value>(value : Node<'att, 'value>, refs : list<WeakReference<MutableTree<'att, 'value>> * list<Index>>) as this =
        let mutable value = Some value
        let mutable refs = refs
        let children =  MutableChildList(this) :> IList<_>
        
        
        // let rec get (index : list<Index>) (node : Node<_,_>) =
        //     Option.get value
        //     // match index with
        //     // | [] -> node
        //     // | h :: t -> node.children.[h] |> get t
        //     
        let rec alter (index : list<Index>) (update : option<Node<_,_>> -> option<Node<_,_>>) (node : Node<_,_>) =
            
            match index with
            | [] ->
                update (Some node)
            | h :: t ->
                match IndexList.tryGet h node.children with
                | Some c ->
                    match alter t update c with
                    | Some nc ->
                        Some { node with children = IndexList.set h nc node.children }
                    | None ->
                        Some { node with children = IndexList.remove h node.children }
                | None ->
                    match update None with
                    | None -> Some node
                    | Some newNode ->
                        let rec create (index : list<Index>) (innerValue : Node<_,_>) =
                            match index with
                            | [] -> innerValue
                            | h :: t ->
                                let inner = create t innerValue
                                { Node.empty with children = IndexList.empty |> IndexList.set h inner  }
                        Some { node with children = IndexList.set h (create t newNode) node.children }
        
        // member x.Tree
        //     with get() = tree
        //     and set t = tree <- t
        // member x.Index
        //     with get() = index
        //     and set i = index <- i
        
        member x.Refs = refs
        
        member x.AddRef(tree : WeakReference<MutableTree<'att, 'value>>, index : list<Index>) =
            refs <- (tree, index) :: refs
        
        member x.AddRefs(newRefs : list<WeakReference<MutableTree<'att, 'value>> * list<Index>>) =
            refs <- newRefs @ refs
        
        member x.Node = value
        
        member x.ImmutableChildren
            with get() =
                match value with
                | Some value -> value.children
                | None -> IndexList.empty
            and set c =
                match value with
                | Some v ->
                    value <- Some { v with children = c }
                    for tree, index in refs do
                        match tree.TryGetTarget() with
                        | (true, tree) ->
                            let newTree =
                                tree.Value |> alter index (fun old ->
                                    match old with
                                    | Some old ->
                                        Some { old with children = c }
                                    | None ->
                                        Some { Node.empty with children = c }
                                )
                            tree.Value <- Option.get newTree
                        | _ ->
                            ()
                | None ->
                    ()
        
        member x.ImmutableAttributes
            with get() =
                match value with
                | Some value -> value.attributes
                | None -> HashMap.empty
            and set (v : HashMap<string, 'att>) =
                match value with
                | Some node ->
                    value <- Some { node with attributes = v }
                    for tree, index in refs do
                        match tree.TryGetTarget() with
                        | (true, tree) ->
                            let newTree =
                                tree.Value |> alter index (fun old ->
                                    match old with
                                    | Some old -> Some { old with attributes = v }
                                    | None -> Some { Node.empty with attributes = v }
                                )
                            tree.Value <- Option.get newTree
                        | _ ->
                            ()
                | None ->
                    ()
           
        member x.ImmutableValues
            with get() =
                match value with
                | Some value -> value.values
                | None -> IndexList.empty
            and set v =
                match value with
                | Some node ->
                    value <- Some { node with values = v }
                    for tree, index in refs do
                        match tree.TryGetTarget() with
                        | (true, tree) ->
                            let newTree =
                                tree.Value |> alter index (fun old ->
                                    match old with
                                    | Some old -> Some { old with values = v }
                                    | None -> Some { Node.empty with values = v }
                                )
                            tree.Value <- Option.get newTree
                        | _ ->
                            ()
                | None ->
                    ()
                    
        member x.Children = children :> IList<_>
        
        member x.Attributes = MutableAttributeMap(x) :> IDictionary<_,_>
          
        member x.Values = MutableValueList(x) :> IList<_>
          
        new(node : Node<'att, 'value>) =
            MutableNode(node, [])
                
    and MutableChildList<'att, 'value>(node : MutableNode<'att, 'value>) =
        
        let rec tryRemovePrefix (prefix : list<'a>) (value : list<'a>) =
            match prefix with
            | [] -> Some value
            | p :: ps ->
                match value with
                | v :: vs when Unchecked.equals p v -> tryRemovePrefix ps vs
                | _ -> None
        
        let tryGetIndexOfChild (child : MutableNode<'att, 'value>) =
            child.Refs |> List.tryPick (fun (ct, ci) ->
                node.Refs |> List.tryPick (fun (nt, ni) ->
                    if ct = nt then
                        match tryRemovePrefix ni ci with
                        | Some [idx] -> Some idx
                        | _ -> None
                    else
                        None
                )
            )
        
        let childCache = Dictionary<Index, MutableNode<'att, 'value>>()
        
        let getChild (idx : Index) =
            lock childCache (fun () ->
                match childCache.TryGetValue idx with
                | (true, child) ->
                    child
                | _ ->
                    let list = node.ImmutableChildren
                    let v = list.[idx]
                    let res = MutableNode(v, node.Refs |> List.map (fun (t, i) -> (t, i @ [idx])))
                    childCache.[idx] <- res
                    res
            )
        member x.Count = node.ImmutableChildren.Count
        
        member x.Clear() =
            node.ImmutableChildren <- IndexList.empty
        
        member x.Add(newNode : MutableNode<'att, 'value>) =
            match newNode.Node with
            | Some value ->
                let list = node.ImmutableChildren
                let index =
                    if list.IsEmpty then Index.after Index.zero
                    else Index.after list.MaxIndex
                
                node.ImmutableChildren <- node.ImmutableChildren |> IndexList.set index value
                
                newNode.AddRefs (node.Refs |> List.map (fun (t, i) -> (t, i @ [index])))
            | _ ->
                ()
            
        member x.Item
            with get(i : int) =
                let list = node.ImmutableChildren
                match list.TryGetIndex i with
                | Some idx -> getChild idx
                | None -> raise <| System.IndexOutOfRangeException()
            and set (i : int) (v : MutableNode<'att, 'value>) =
                let list = node.ImmutableChildren
                match list.TryGetIndex i with
                | Some idx ->
                    match v.Node with
                    | Some value ->
                        v.AddRefs(node.Refs |> List.map (fun (t, i) -> (t, i @ [idx])))
                        lock childCache (fun () -> childCache.[idx] <- v)
                        node.ImmutableChildren <- node.ImmutableChildren |> IndexList.set idx value
                    | None ->
                        node.ImmutableChildren <- node.ImmutableChildren |> IndexList.remove idx
                        lock childCache (fun () -> childCache.Remove idx |> ignore)
                | None ->
                    raise <| System.IndexOutOfRangeException()
               
        interface IEnumerable<MutableNode<'att, 'value>> with
            member this.GetEnumerator(): IEnumerator<MutableNode<'att,'value>> =
                let l = node.ImmutableChildren |> IndexList.mapi (fun i _ -> getChild i)
                l.GetEnumerator() :> _
            member this.GetEnumerator(): Collections.IEnumerator = 
                let l = node.ImmutableChildren |> IndexList.mapi (fun i _ -> getChild i)
                l.GetEnumerator() :> _
               
        interface ICollection<MutableNode<'att, 'value>> with
            member this.Add(item) = this.Add item
            member this.Clear() = this.Clear()
            member this.Contains(item) =
                match tryGetIndexOfChild item with
                | Some _ -> true
                | None -> false
                    
            member this.CopyTo(array,arrayIndex) =
                let mutable oi = arrayIndex
                for idx, _ in IndexList.toArrayIndexed node.ImmutableChildren do
                    let n = getChild idx
                    array.[oi] <- n
                    oi <- oi + 1
                
            member this.Remove(item) =
                match tryGetIndexOfChild item with
                | Some idx ->
                    node.ImmutableChildren <- IndexList.remove idx node.ImmutableChildren
                    true
                | None ->
                    false
                
            member this.Count = this.Count
            member this.IsReadOnly = false
               
        interface IList<MutableNode<'att, 'value>> with
            member this.IndexOf(item) =
                match tryGetIndexOfChild item with
                | Some idx -> node.ImmutableChildren.IndexOf idx
                | None -> -1
            member this.Insert(index,item) =
                let l = node.ImmutableChildren
                if index >= l.Count then
                    this.Add item
                else
                    match l.TryGetIndex index with
                    | Some afterIndex ->
                        match item.Node with
                        | Some n ->
                            let idx = l.NewIndexBefore afterIndex
                            item.AddRefs(node.Refs |> List.map (fun (t, i) -> (t, i @ [idx])))
                            lock childCache (fun () -> childCache.[idx] <- item)
                            node.ImmutableChildren <- l |> IndexList.set idx n
                        | None ->
                            ()
                    | None ->
                        raise <| IndexOutOfRangeException()
                    
            member this.RemoveAt(index) = 
                let l = node.ImmutableChildren
                match l.TryGetIndex index with
                | Some idx ->
                    node.ImmutableChildren <- l |> IndexList.remove idx
                    lock childCache (fun () -> childCache.Remove idx |> ignore)
                | None ->
                    raise <| IndexOutOfRangeException()
            member this.Item
                with get(i : int) = this.[i]
                and set (i : int) (v : MutableNode<'att, 'value>) = this.[i] <- v
                 
    and MutableAttributeMap<'att, 'value>(node : MutableNode<'att, 'value>) =
        member x.Count = node.ImmutableAttributes.Count
        
        member x.Item
            with get (key : string) =
                node.ImmutableAttributes.[key]
            and set (key : string) (value : 'att) =
                node.ImmutableAttributes <- node.ImmutableAttributes |> HashMap.add key value
        
        member x.Remove (key : string) =
            match HashMap.tryRemove key node.ImmutableAttributes with
            | Some (_, rest) ->
                node.ImmutableAttributes <- rest
                true
            | None ->
                false
        
        member x.TryRemove (key : string) =
            match HashMap.tryRemove key node.ImmutableAttributes with
            | Some (v, rest) ->
                node.ImmutableAttributes <- rest
                Some v
            | None ->
                None
        
        member x.ContainsKey (key : string) =
            node.ImmutableAttributes.ContainsKey key
        
        member x.Add(item : KeyValuePair<string, 'att>) =
            node.ImmutableAttributes <- node.ImmutableAttributes |> HashMap.add item.Key item.Value
            
        member x.Clear() =
            node.ImmutableAttributes <- HashMap.empty
            
        member x.TryGetValue(key : string, value : outref<'att>) =
            match HashMap.tryFind key node.ImmutableAttributes with
            | Some v ->
                value <- v
                true
            | None ->
                false
            
        interface IEnumerable<KeyValuePair<string, 'att>> with
            member this.GetEnumerator(): IEnumerator<KeyValuePair<string,'att>> =
                let seq = node.ImmutableAttributes |> HashMap.toSeqV |> Seq.map (fun struct(k, v) -> KeyValuePair(k,v))
                seq.GetEnumerator()
            member this.GetEnumerator(): Collections.IEnumerator =
                let seq = node.ImmutableAttributes |> HashMap.toSeqV |> Seq.map (fun struct(k, v) -> KeyValuePair(k,v))
                seq.GetEnumerator()
            
        interface ICollection<KeyValuePair<string, 'att>> with
            member this.Add(item) = 
                node.ImmutableAttributes <- node.ImmutableAttributes |> HashMap.add item.Key item.Value
            member this.Clear() =
                this.Clear()
            member this.Contains(item) =
                match this.TryGetValue item.Key with
                | (true, v) -> Unchecked.equals v item.Value
                | _ -> false
                
            member this.CopyTo(array,arrayIndex) =
                let arr = node.ImmutableAttributes |> HashMap.toArrayV
                let mutable oi = arrayIndex
                for struct(k, v) in arr do
                    array.[oi] <- KeyValuePair(k, v)
                    oi <- oi + 1
                
            member this.Remove(item) = this.Remove item.Key
            member this.Count = this.Count
            member this.IsReadOnly = false
        
        interface IDictionary<string, 'att> with
            member this.Add(key,value) = this.[key] <- value
            member this.ContainsKey(key) = this.ContainsKey key
            member this.Remove(key: string): bool = this.Remove key
            member this.TryGetValue(key,value) = this.TryGetValue(key, &value)
            member this.Item
                with get(key) = this.[key]
                and set key value = this.[key] <- value
                
            member this.Keys = node.ImmutableAttributes |> HashMap.toKeyArray :> _
            member this.Values = node.ImmutableAttributes |> HashMap.toValueArray :> _
        
    and MutableValueList<'att, 'value>(node : MutableNode<'att, 'value>) =
        member x.Count = node.ImmutableValues.Count
        
        member x.Clear() =
            node.ImmutableValues <- IndexList.empty
        
        member x.Add(value : 'value) =
            node.ImmutableValues <- IndexList.add value node.ImmutableValues
        
        member x.Insert(index : int, value : 'value) =
            let list = node.ImmutableValues
            if index >= list.Count then
                node.ImmutableValues <- IndexList.add value list
            else
                match list.TryGetIndex index with
                | Some idx ->
                    let ni = list.NewIndexBefore idx
                    node.ImmutableValues <- list |> IndexList.set ni value
                | None ->
                    raise <| IndexOutOfRangeException()
                    
        member x.RemoveAt(index : int) =
            let list = node.ImmutableValues
            match list.TryGetIndex index with
            | Some idx ->
                node.ImmutableValues <- IndexList.remove idx list
            | None ->
                raise <| IndexOutOfRangeException()
                
        member x.Item
            with get(i : int) =
                let list = node.ImmutableValues
                list.[i]
            and set (i : int) (value : 'value) =
                let list = node.ImmutableValues
                match list.TryGetIndex i with
                | Some idx ->
                    node.ImmutableValues <- IndexList.set idx value list
                | None ->
                    raise <| IndexOutOfRangeException()
                
        interface ICollection<'value> with
            member this.Add(item) = this.Add item
            member this.Clear() = this.Clear()
            member this.Contains(item) = node.ImmutableValues |> IndexList.exists (fun _ e -> Unchecked.equals e item)
            member this.CopyTo(array,arrayIndex) =
                node.ImmutableValues.CopyTo(array, arrayIndex)
            member this.GetEnumerator(): IEnumerator<'value> = node.ImmutableValues.GetEnumerator()
            member this.GetEnumerator(): Collections.IEnumerator = node.ImmutableValues.GetEnumerator()
            member this.Remove(item) =
                match node.ImmutableValues |> IndexList.tryPick (fun idx e -> if Unchecked.equals e item then Some idx else None) with
                | Some idx ->
                    node.ImmutableValues <- IndexList.remove idx node.ImmutableValues
                    true
                | None ->
                    false
            member this.Count = this.Count
            member this.IsReadOnly = false
        
        interface IList<'value> with
            member this.IndexOf(item) =
                let list = node.ImmutableValues
                match list |> IndexList.tryPick (fun idx e -> if Unchecked.equals e item then Some idx else None) with
                | Some idx ->
                    list.IndexOf idx
                | None ->
                    -1
                    
            member this.Insert(index,item) = this.Insert(index, item)
            member this.RemoveAt(index) = this.RemoveAt index
            member this.Item
                with get i = this.[i]
                and set i v = this.[i] <- v
        
        
    type MutableNodeBuilder() =
        inherit NodeBuilder()
        
        member x.Run(n : Node<'a, 'b>) =
            MutableNode n
        
    let mnode = MutableNodeBuilder()
        
    let test() =
        // create an empty tree
        let thing = MutableTree()
        let root = thing.Root
        
        // add a new node
        let newNode =
            mnode {
                "color", "red"
                Value 1
            }
           
        transact (fun () ->
            root.Children.Add newNode
        )
        
        // adaptively flatten the structure (getting all attributes and leaves)
        let list : alist<HashMap<string, string> * int> =
            ATree.flatten thing
            
        let reader =
            list.GetReader()
        
        let print() =
            let ops = reader.GetChanges AdaptiveToken.Top
            reader.State |> Seq.map (fun (att, v) -> sprintf "%s: %A" att.["color"] v) |> String.concat "; " |> printfn "state: [%s]"
            ops |> Seq.map (fun (idx, op) -> match op with | Remove -> sprintf "Remove(%A)" idx | Set (att, v) -> sprintf "Set(%A, %s, %A)" idx att.["color"] v) |> String.concat "; " |> printfn "ops:   [%s]"
        
        print()
        // state: [red: 1]
        // ops:   [Set(0.656250, red, 1)]

        
        // add a new child
        transact (fun () ->
            newNode.Children.Add (
                mnode {
                    "color", "white"
                    Value 2
                }
            )
        )
        print()
        // state: [red: 1; white: 2]
        // ops:   [Set(0.707031, white, 2)]

        
        // add a new child
        transact (fun () ->
            newNode.Children.Add (
                mnode {
                    Value 3
                }
            )
        )
        print()
        // state: [red: 1; white: 2; red: 3]
        // ops:   [Set(0.717285, red, 3)]

        
        
        transact (fun () ->
            newNode.Attributes.["color"] <- "blue"
        )
        print()
        // state: [blue: 1; white: 2; blue: 3]
        // ops:   [Set(0.656250, blue, 1); Set(0.717285, blue, 3)]

        
        
        exit 0
        
        transact (fun () ->
            thing.Root.ImmutableValues <- IndexList.single 123
            thing.Root.Children.[0].ImmutableAttributes <- HashMap.ofList ["color", "red"]
        )
        print()
        
        
        let testy = MutableNode(node { Value 321 })
        
        transact (fun () -> thing.Root.Children.Add testy)
        print()
        
        transact (fun () -> thing.Root.Children.[0].Children.Add testy)
        print()
        
        transact (fun () -> testy.Values.Add 456)
        print()
        
        
        exit 0
        
            
        printNode "" None t0
        let t1 =
            { t0 with attributes = HashMap.add "color" "green" t0.attributes }
            
        let t2 =
            let newChild =
                node {
                    yield Value 25
                    yield Value 356
                }
            { t1 with children = IndexList.add newChild t1.children }
                  
        let t3 =
            { t2 with children = IndexList.removeAt 0 t2.children }
            
        printNode "" None t0
        printNode "" None t1
        printNode "" None t2
        let test = ctree t0
        let list = ATree.flatten test
        
        let r = list.GetReader()
        
        let print() =
            let ops = r.GetChanges AdaptiveToken.Top
            r.State |> Seq.map (fun (att, v) -> sprintf "%s: %A" att.["color"] v) |> String.concat "; " |> printfn "state: [%s]"
            ops |> Seq.map (fun (idx, op) -> match op with | Remove -> sprintf "Remove(%A)" idx | Set (att, v) -> sprintf "Set(%A, %s, %A)" idx att.["color"] v) |> String.concat "; " |> printfn "ops:   [%s]"
        
        print()
        
        transact(fun () -> test.Value <- t1)
        print()
        
        transact(fun () -> test.Value <- t2)
        print()
        
        
        transact(fun () -> test.Value <- t3)
        print()
        
        transact(fun () -> test.Value <- t0)
        print()
        
        
        
        
        
        
        
        
        
            
            
        ()
        
        
open AdaptiveTree
test() 
//             
// let input = clist [1;2;3] 
//
//
// let test = input |> AList.map (fun v -> v * 2) |> AList.lift
//
//
// let r = test.GetReader()
//
//
// let print() =
//     printfn "eval"
//     let ops = r.GetChanges AdaptiveToken.Top
//     for i, o in ops do
//         printfn "  %A: %A" i o
//
//     for i, v in IndexList.toSeqIndexed r.State do
//         let oo = v.OutOfDate
//         printfn "  %A: %A (%A)" i (AVal.force v) oo
//
//
// print()
//
//
// transact (fun () ->
//     input.[1] <- 10    
// )
// print()
//
//
//
// transact (fun () ->
//     input.[1] <- 0
//     input.Add 25 |> ignore
// )
// print()


