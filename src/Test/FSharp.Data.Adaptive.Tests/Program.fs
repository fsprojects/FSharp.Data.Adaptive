module Program

open BenchmarkDotNet.Running

module Profile =
    open FSharp.Data.Adaptive
    let inputDiamond = AVal.custom (fun _ -> 0)
    let diamondSizes =
        [1;3;9;21;45;93;189;381;765;1533;3069;6141;12285; 24573;49149;98301;196605;393213;786429;1572861; 3145725;6291453;12582909;25165821;50331645; 100663293;201326589;402653181;805306365; 1610612733]


    let rec buildFan (inner : ref<int>) (n : int) =
        if n <= 0 then
            [inputDiamond]
        else
            buildFan inner (n - 1)
            |> List.collect (fun v ->
                inner := !inner + 2
                [AVal.map id v; AVal.map id v]
            )

    let rec reduce (inner : ref<int>) (l : list<aval<_>>) =
        match l with
        | [] -> inputDiamond
        | [v] -> v
        | l -> l |> List.chunkBySize 2 |> List.map (function [l;r] -> inner := !inner + 1; AVal.map2 (+) l r | _ -> failwith "bad") |> reduce inner

    let rec buildDiamond (depth : int) =
        let inner = ref 0
        let res = buildFan inner depth |> reduce inner
        printfn "DEPTH: %d / %d" depth !inner
        res


    let run() = 
        let d = buildDiamond 20
        for i in 1 .. 4 do
            AVal.force d |> ignore
            transact (fun () -> inputDiamond.MarkOutdated())
            
        AVal.force d |> ignore

        printfn "ready"
        System.Console.ReadLine() |> ignore
        transact (fun () -> inputDiamond.MarkOutdated())
        
        printfn "done"
        System.Console.ReadLine() |> ignore



open FSharp.Data.Traceable
open FSharp.Data.Adaptive
open System.Runtime.CompilerServices

[<Sealed>]
type ChangeableOrderedHashSet<'T>(initial : seq<'T>) =
    let mutable indices = 
        let mutable lastIndex = Index.zero
        let mutable res = HashMap.empty
        for e in initial do
            let i = Index.after lastIndex
            res <- HashMap.add e i res
            lastIndex <- i
        res

    let listHistory = History IndexList.trace
    let setHistory = History CountingHashSet.traceNoRefCountTup

    do if not (HashMap.isEmpty indices) then
        let setDelta = indices |> HashMap.map (fun _ _ -> 1) |> HashSetDelta.ofHashMap
        let listDelta = indices |> HashMap.toArray |> Array.map (fun (a,b) -> b, Set a) |> IndexListDelta.ofArray
        listHistory.Perform listDelta |> ignore
        setHistory.Perform setDelta |> ignore

    let setContent = lazy (setHistory |> AVal.map snd)

    member x.Remove(element : 'T) =
        match HashMap.tryRemoveV element indices with
        | ValueSome (index, rest) ->
            indices <- rest
            Rem element |> HashSetDelta.single |> setHistory.Perform |> ignore
            Remove |> IndexListDelta.single index |> listHistory.Perform |> ignore
            true
        | _ ->
            false

    member x.Append(element : 'T) =
        if indices.ContainsKey element then
            false
        else
            let list = listHistory.State
            let index = Index.after list.MaxIndex
            indices <- HashMap.add element index indices
            Add element |> HashSetDelta.single |> setHistory.Perform |> ignore
            Set element |> IndexListDelta.single index |> listHistory.Perform |> ignore
            true
            
    member x.Add(element : 'T) = x.Append element

    member x.Prepend(element : 'T) =
        if indices.ContainsKey element then
            false
        else
            let list = listHistory.State
            let index = 
                if list.IsEmpty then Index.after Index.zero
                else Index.before list.MinIndex
            indices <- HashMap.add element index indices
            Add element |> HashSetDelta.single |> setHistory.Perform |> ignore
            Set element |> IndexListDelta.single index |> listHistory.Perform |> ignore
            true

    member x.InsertAfter(ref : 'T, element : 'T) =
        match HashMap.tryFindV ref indices with
        | ValueSome refIndex ->
            if indices.ContainsKey element then
                false
            else
                let list = listHistory.State
                let index = list.NewIndexAfter refIndex
                indices <- HashMap.add element index indices
                Add element |> HashSetDelta.single |> setHistory.Perform |> ignore
                Set element |> IndexListDelta.single index |> listHistory.Perform |> ignore
                true
        | _ ->
            false

    member x.InsertBefore(ref : 'T, element : 'T) =
        match HashMap.tryFindV ref indices with
        | ValueSome refIndex ->
            if indices.ContainsKey element then
                false
            else
                let list = listHistory.State
                let index = list.NewIndexBefore refIndex
                indices <- HashMap.add element index indices
                Add element |> HashSetDelta.single |> setHistory.Perform |> ignore
                Set element |> IndexListDelta.single index |> listHistory.Perform |> ignore
                true
        | _ ->
            false

    member x.Clear() =
        if indices.Count > 0 then
            indices <- HashMap.empty
            listHistory.Perform (IndexList.computeDelta listHistory.State IndexList.empty) |> ignore
            setHistory.Perform (CountingHashSet.computeDelta (fst setHistory.State) CountingHashSet.empty) |> ignore

    member x.Item
        with get(i : int) = 
            listHistory.State.[i]
        and set (i : int) (value : 'T) =
            match listHistory.State.TryGetIndexV i with
            | ValueSome dstIndex ->
                let oldValue = listHistory.State.[dstIndex]
                match indices.TryRemoveV value with
                | ValueSome (oldIndex, rest) ->
                    indices <- rest
                    if oldIndex <> dstIndex then
                        IndexListDelta.single oldIndex Remove
                        |> IndexListDelta.add dstIndex (Set value)
                        |> listHistory.Perform
                        |> ignore

                    HashSetDelta.single (Rem oldValue)
                    |> HashSetDelta.add (Add value)
                    |> setHistory.Perform
                    |> ignore

                | ValueNone ->
                    indices <- HashMap.add value dstIndex indices

                    IndexListDelta.single dstIndex (Set value)
                    |> listHistory.Perform
                    |> ignore

                    HashSetDelta.single (Rem oldValue)
                    |> HashSetDelta.add (Add value)
                    |> setHistory.Perform
                    |> ignore
            | ValueNone ->
                raise <| System.IndexOutOfRangeException()
            


    member x.Contains(element : 'T) =
        indices.ContainsKey element
    
    member x.Count =
        indices.Count

    member x.GetEnumerator() = new ChangeableOrderedHashSetEnumerator<_>(indices) 
    
    override x.ToString() =
        let suffix =
            if x.Count > 5 then "; ..."
            else ""

        let content =
            listHistory.State |> Seq.truncate 5 |> Seq.map (sprintf "%A") |> String.concat "; "

        "corderedset [" + content + suffix + "]"

    interface IAdaptiveHashSet<'T> with
        member x.IsConstant = false
        member x.GetReader() = setHistory.NewReader()
        member x.History = Some setHistory
        member x.Content = setContent.Value
        
    interface IAdaptiveIndexList<'T> with
        member x.IsConstant = false
        member x.GetReader() = listHistory.NewReader()
        member x.History = Some listHistory
        member x.Content = listHistory :> aval<_>


    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = new ChangeableOrderedHashSetEnumerator<_>(indices) :> _

    interface System.Collections.Generic.IEnumerable<'T> with
        member x.GetEnumerator() = new ChangeableOrderedHashSetEnumerator<_>(indices) :> _

    new() = ChangeableOrderedHashSet Seq.empty

and corderedset<'T> = ChangeableOrderedHashSet<'T>

and ChangeableOrderedHashSetEnumerator<'T> =
    struct
        val mutable public Inner : HashMapStructEnumerator<'T, Index>

        member x.MoveNext() = x.Inner.MoveNext()
        member x.Reset() = x.Inner.Reset()
        member x.Dispose() = x.Inner.Dispose()
        member x.Current = 
            let struct(k,_) = x.Inner.Current
            k

        interface System.Collections.IEnumerator with
            member x.MoveNext() = x.MoveNext()
            member x.Reset() = x.Reset()
            member x.Current = x.Current :> obj
            
        interface System.Collections.Generic.IEnumerator<'T> with
            member x.Dispose() = x.Dispose()
            member x.Current = x.Current

        internal new(map : HashMap<'T, Index>) = { Inner = map.GetStructEnumerator() }

    end
[<AutoOpen>]
module NumberCrunching =
    type Mask = uint32

    let inline highestBitMask x =
        let mutable x = x
        x <- x ||| (x >>> 1)
        x <- x ||| (x >>> 2)
        x <- x ||| (x >>> 4)
        x <- x ||| (x >>> 8)
        x <- x ||| (x >>> 16)
        x ^^^ (x >>> 1)

    let inline getPrefix (k: uint32) (m: Mask) =
        k &&& ~~~((m <<< 1) - 1u)

    let inline zeroBit (k: uint32) (m: uint32) =
        if (k &&& m) <> 0u then 1u else 0u
        
    let inline matchPrefixAndGetBit (hash: uint32) (prefix: uint32) (m: uint32) =
        if getPrefix hash m = prefix then zeroBit hash m
        else 2u

    let inline compareMasks (l : Mask) (r : Mask) =
        compare r l

    let inline getMask (p0 : uint32) (p1 : uint32) =
        //lowestBitMask (p0 ^^^ p1) // little endian
        highestBitMask (p0 ^^^ p1) // big endian
            

module CountingHashSetNew =
    open System.Collections.Generic

    [<AllowNullLiteral>]
    type internal OverflowList<'T> =
        inherit HashSetLinked<'T>
        val mutable public RefCount : int

        member x.RefNext = x.Next |> unbox<OverflowList<'T>>

        new(k : 'T, v : int, n : OverflowList<'T>) = 
            { 
                inherit HashSetLinked<'T>(k, n :> HashSetLinked<_>)
                RefCount = v
            }

    module internal OverflowList =

        let rec applyDeltaNoState (delta : byref<HashMapLinked<'T, int>>) =
            if isNull delta then
                null
            elif delta.Value > 0 then
                let mutable restDelta = delta.Next
                let restState = applyDeltaNoState &restDelta
                delta <- HashMapLinked(delta.Key, 1, restDelta)
                OverflowList(delta.Key, delta.Value, restState)
            else
                delta <- delta.Next
                applyDeltaNoState &delta
                

        let rec toHashMapLinked (positive : bool) (l : OverflowList<'T>) =
            if isNull l then
                null
            else
                HashMapLinked(l.Value, (if positive then l.RefCount else -l.RefCount), toHashMapLinked positive l.RefNext)

        let rec addRefCount (cmp : IEqualityComparer<'T>) (key : 'T) (delta : int) (l : OverflowList<'T>) =
            if isNull l then
                if delta > 0 then OverflowList(key, delta, null)
                else null
            elif cmp.Equals(l.Value, key) then
                let newRefCount = l.RefCount + delta
                if newRefCount > 0 then OverflowList(key, newRefCount, l.RefNext)
                else l.RefNext
            else
                OverflowList(l.Value, l.RefCount, addRefCount cmp key delta l.RefNext)
                
        let rec addInPlace (cmp : IEqualityComparer<'T>) (key : 'T) (delta : int) (l : byref<HashSetLinked<'T>>) =
            if isNull l then
                if delta > 0 then l <- OverflowList(key, delta, null)

            elif cmp.Equals(l.Value, key) then
                let lo = l |> unbox<OverflowList<'T>>
                let newRefCount = lo.RefCount + delta
                if newRefCount > 0 then lo.RefCount <- newRefCount
                else l <- l.Next
            else
                addInPlace cmp key delta &l.Next

        let rec tryRemove (cmp : IEqualityComparer<'T>) (key : 'T) (oldRefCount : byref<int>) (l : byref<OverflowList<'T>>) = 
            if isNull l then
                false
            elif cmp.Equals(key, l.Value) then
                oldRefCount <- l.RefCount
                l <- l.RefNext
                true
            else
                let mutable c = l.RefNext
                if tryRemove cmp key &oldRefCount &c then
                    l <- OverflowList(l.Value, l.RefCount, c)
                    true
                else
                    false

        let rec getRefCount (cmp : IEqualityComparer<'T>) (key : 'T) (l : OverflowList<'T>) =
            if isNull l then 0
            elif cmp.Equals(l.Value, key) then l.RefCount
            else getRefCount cmp key l.RefNext
            
        let rec applyDelta (cmp : IEqualityComparer<'T>) (state : byref<OverflowList<'T>>) (delta : HashMapLinked<'T, int>) =
            if isNull delta then
                delta
            else
                let k = delta.Key
                let v = delta.Value
                let mutable o = 0
                if tryRemove cmp k &o &state then
                    let n = o + v
                    if n > 0 then
                        let mutable next = state.RefNext
                        let delta = applyDelta cmp &next delta.Next
                        state <- OverflowList(k, n, next)
                        delta
                    else
                        let delta = applyDelta cmp &state delta.Next
                        HashMapLinked(k, -1, delta)
                elif v > 0 then
                    let mutable next = state
                    let delta = applyDelta cmp &state delta.Next
                    HashMapLinked(k, 1, delta)
                else
                    applyDelta cmp &state delta.Next
                    


        let rec union (cmp : IEqualityComparer<'T>) (a : OverflowList<'T>) (b : OverflowList<'T>) =
            if isNull a then b
            elif isNull b then a
            else
                let na = a.RefNext
                let mutable nb = b
                let mutable r = 0
                if tryRemove cmp a.Value &r &nb then
                    let r = r + a.RefCount
                    OverflowList(a.Value, r, union cmp na nb)
                else
                    OverflowList(a.Value, a.RefCount, union cmp na b)

        let rec computeDelta (cmp : IEqualityComparer<'T>) (a : OverflowList<'T>) (b : OverflowList<'T>) =
            if isNull a then 
                if isNull b then
                    null
                else
                    HashMapLinked(b.Value, b.RefCount, computeDelta cmp null b.RefNext)
            elif isNull b then
                HashMapLinked(a.Value, -a.RefCount, computeDelta cmp a.RefNext null)
            else
                let na = a.RefNext
                let mutable nb = b
                let mutable r = 0
                if tryRemove cmp a.Value &r &nb then
                    let delta = r - a.RefCount
                    if delta <> 0 then
                        HashMapLinked(a.Value, delta, computeDelta cmp na nb)
                    else
                        computeDelta cmp na nb
                else
                    HashMapLinked(a.Value, -a.RefCount, computeDelta cmp na b)

        let rec toList (acc : list<'T>) (l : OverflowList<'T>) =
            if isNull l then
                acc
            else
                l.Value :: toList acc l.RefNext
                
        let rec toRefCountList (acc : list<'T * int>) (l : OverflowList<'T>) =
            if isNull l then
                acc
            else
                (l.Value, l.RefCount) :: toRefCountList acc l.RefNext

    [<AutoOpen>]
    module internal Nodes =

        [<AllowNullLiteral>]
        type Node<'T> = interface end

        type NoCollisionLeaf<'T> =
            inherit HashSetNoCollisionLeaf<'T>
            interface Node<'T>
            val mutable public RefCount : int

            new(h, k, c) = 
                { 
                    inherit HashSetNoCollisionLeaf<'T>(Hash = h, Value = k)
                    RefCount = c
                }
        
        type Leaf<'T> =
            inherit HashSetCollisionLeaf<'T>
            interface Node<'T>

            val mutable public RefCount : int

            member x.Key = x.Value
            member x.RefNext = x.Next |> unbox<OverflowList<'T>>


            new(h : uint32, k : 'T, v : int, n : OverflowList<'T>) = 
                { 
                    inherit HashSetCollisionLeaf<'T>(Hash = h, Value = k, Next = n) 
                    RefCount = v
                }

        type Inner<'T> =
            inherit HashSetInner<'T>
            interface Node<'T>

            member x.MapLeft = x.Left |> unbox<Node<'T>>
            member x.MapRight = x.Right |> unbox<Node<'T>>
            
            new(p,m,l,r,c) = 
                let inline toSetNode (node : Node<'T>) =
                    if isNull node then 
                        HashSetEmpty.Instance
                    else 
                        unbox<HashSetNode<'T>> node

                {
                    inherit HashSetInner<'T>(
                        Prefix = p,
                        Mask = m, 
                        Left = toSetNode l, 
                        Right = toSetNode r, 
                        _Count = c
                    ) 
                }
            new(p,m,l,r) = 
                let inline toSetNode (node : Node<'T>) =
                    if isNull node then 
                        HashSetEmpty.Instance
                    else 
                        unbox<HashSetNode<'T>> node

                let ls = toSetNode l
                let rs = toSetNode r

                {
                    inherit HashSetInner<'T>(
                        Prefix = p,
                        Mask = m, 
                        Left = ls,
                        Right = rs, 
                        _Count = ls.Count + rs.Count
                    ) 
                }

        let inline count (node : Node<'T>) =
            if isNull node then 0
            else (unbox<HashSetNode<'T>> node).Count

        let join (h0 : uint32) (n0 : Node<'T>) (h1 : uint32) (n1 : Node<'T>) : Node<'T> =
            if isNull n0 then n1
            elif isNull n1 then n0
            else
                let m = getMask h0 h1
                if zeroBit h0 m = 0u then Inner(getPrefix h0 m, m, n0, n1, count n0 + count n1) :> Node<_>
                else Inner(getPrefix h0 m, m, n1, n0, count n0 + count n1) :> Node<_>

        let rec add (cmp : IEqualityComparer<'T>) (hash : uint32) (key : 'T) (delta : int) (node : Node<'T>) : Node<'T> =
            if isNull node then
                if delta > 0 then NoCollisionLeaf(hash, key, delta) :> Node<_>
                else null
            else
                match node with
                | :? NoCollisionLeaf<'T> as node ->
                    if node.Hash = hash then
                        if cmp.Equals(node.Value, key) then
                            let newRefCount = node.RefCount + delta
                            if newRefCount > 0 then NoCollisionLeaf(hash, key, newRefCount) :> Node<_>
                            else null
                        else
                            if delta > 0 then
                                Leaf(node.Hash, node.Value, node.RefCount, OverflowList(key, delta, null)) :> Node<_>
                            else
                                node :> Node<_>
                    else
                        if delta > 0 then
                            join node.Hash node hash (NoCollisionLeaf(hash, key, delta))
                        else
                            node :> Node<_>

                | :? Leaf<'T> as node ->
                    if node.Hash = hash then
                        if cmp.Equals(node.Key, key) then
                            let newRefCount = node.RefCount + delta
                            if newRefCount > 0 then Leaf(hash, key, newRefCount, node.RefNext) :> Node<_>
                            else
                                let next = node.RefNext
                                if isNull next.Next then NoCollisionLeaf(hash, next.Value, next.RefCount) :> Node<_>
                                else Leaf(hash, next.Value, next.RefCount, next.RefNext) :> Node<_>
                        else
                            let newNext = OverflowList.addRefCount cmp key delta node.RefNext
                            if isNull newNext then
                                NoCollisionLeaf(hash, node.Value, node.RefCount) :> Node<_>
                            else
                                Leaf(hash, node.Key, node.RefCount, newNext) :> Node<_>
                    else
                        if delta > 0 then
                            join node.Hash node hash (NoCollisionLeaf(hash, key, delta))
                        else
                            node :> Node<_>
                
                | :? Inner<'T> as node ->
                    match matchPrefixAndGetBit hash node.Prefix node.Mask with
                    | 0u ->
                        let newLeft = add cmp hash key delta node.MapLeft
                        if isNull newLeft then node.MapRight
                        else Inner(node.Prefix, node.Mask, newLeft, node.MapRight, count newLeft + count node.MapRight) :> Node<_>
                    | 1u ->
                        let newRight = add cmp hash key delta node.MapRight
                        if isNull newRight then node.MapLeft
                        else Inner(node.Prefix, node.Mask, node.MapLeft, newRight, count node.MapLeft + count newRight) :> Node<_>
                    | _ ->
                        if delta > 0 then
                            join node.Prefix node hash (NoCollisionLeaf(hash, key, delta))
                        else
                            node :> Node<_>
                | _ ->
                    node

        let rec addInPlace (cmp : IEqualityComparer<'T>) (hash : uint32) (key : 'T) (delta : int) (node : byref<Node<'T>>) : bool =
            if isNull node then
                if delta > 0 then 
                    node <- NoCollisionLeaf(hash, key, delta) :> Node<_>
                    true
                else
                    false
            else
                match node with
                | :? NoCollisionLeaf<'T> as n ->
                    if n.Hash = hash then
                        if cmp.Equals(n.Value, key) then
                            let newRefCount = n.RefCount + delta
                            if newRefCount > 0 then 
                                n.RefCount <- newRefCount
                                false
                            else 
                                node <- null
                                true
                        elif delta > 0 then
                            node <- Leaf(n.Hash, n.Value, n.RefCount, OverflowList(key, delta, null)) :> Node<_>
                            true
                        else
                            false
                    elif delta > 0 then
                        node <- join n.Hash n hash (NoCollisionLeaf(hash, key, delta))
                        true
                    else
                        false

                | :? Leaf<'T> as n ->
                    if n.Hash = hash then
                        if cmp.Equals(n.Key, key) then
                            let newRefCount = n.RefCount + delta
                            if newRefCount > 0 then 
                                n.RefCount <- newRefCount
                                false
                            else 
                                let next = n.RefNext
                                if isNull next.Next then
                                    node <- NoCollisionLeaf(hash, next.Value, next.RefCount) :> Node<_>
                                    true
                                else
                                    n.Value <- next.Value
                                    n.RefCount <- next.RefCount
                                    n.Next <- next.Next
                                    false
                        else
                            OverflowList.addInPlace cmp key delta &n.Next
                            if isNull n.Next then 
                                node <- NoCollisionLeaf(hash, n.Value, n.RefCount)
                                true
                            else
                                false
                    else
                        if delta > 0 then
                            node <- join n.Hash n hash (NoCollisionLeaf(hash, key, delta))
                            true
                        else
                            false
                
                | :? Inner<'T> as n ->
                    match matchPrefixAndGetBit hash n.Prefix n.Mask with
                    | 0u ->
                        let mutable l = n.MapLeft
                        if addInPlace cmp hash key delta &l then
                            if isNull l then 
                                node <- n.MapRight 
                                true
                            else
                                n.Left <- unbox l
                                n._Count <- count l + count n.MapRight
                                false
                        else
                            n._Count <- count n.MapLeft + count n.MapRight
                            false
                    
                    | 1u ->
                        let mutable r = n.MapRight
                        if addInPlace cmp hash key delta &r then
                            if isNull r then
                                node <- n.MapLeft
                                true
                            else
                                n.Right <- unbox r
                                n._Count <- count n.MapLeft + count r
                                false
                        else
                            n._Count <- count n.MapLeft + count n.MapRight
                            false
                     
                    | _ ->
                        if delta > 0 then
                            node <- join n.Prefix n hash (NoCollisionLeaf(hash, key, delta))
                            true
                        else
                            false
                | _ ->
                    false

        let rec getRefCount (cmp : IEqualityComparer<'T>) (hash : uint32) (key : 'T) (node : Node<'T>) =
            if isNull node then
                0
            else
                match node with
                | :? NoCollisionLeaf<'T> as n ->
                    if cmp.Equals(key, n.Value) then n.RefCount
                    else 0
                | :? Leaf<'T> as n ->
                    if cmp.Equals(key, n.Value) then n.RefCount
                    else OverflowList.getRefCount cmp key n.RefNext
                | :? Inner<'T> as n ->
                    match matchPrefixAndGetBit hash n.Prefix n.Mask with
                    | 0u -> getRefCount cmp hash key n.MapLeft
                    | 1u -> getRefCount cmp hash key n.MapRight
                    | _ -> 0
                | _ ->
                    0
        
        let rec tryRemove (cmp : IEqualityComparer<'T>) (hash : uint32) (key : 'T) (value : byref<int>) (node : byref<Node<'T>>) : bool =
            if isNull node then
                false
            else
                match node :> obj with
                | :? NoCollisionLeaf<'T> as n ->
                    if hash = n.Hash && cmp.Equals(key, n.Value) then
                        node <- null
                        value <- n.RefCount
                        true
                    else
                        false
                | :? Leaf<'T> as n ->
                    if hash = n.Hash then
                        if cmp.Equals(key, n.Key) then
                            let next = n.RefNext
                            value <- n.RefCount
                            if isNull next.Next then node <- NoCollisionLeaf(n.Hash, next.Value, next.RefCount)
                            else node <- Leaf(n.Hash, next.Value, next.RefCount, next.RefNext)
                            true
                        else
                            let mutable next = n.RefNext
                            if OverflowList.tryRemove cmp key &value &next then
                                if isNull next then node <- NoCollisionLeaf(n.Hash, n.Key, n.RefCount)
                                else node <- Leaf(n.Hash, n.Key, n.RefCount, next)
                                true
                            else
                                false
                        
                    else
                        false
                | :? Inner<'T> as n ->
                    match matchPrefixAndGetBit hash n.Prefix n.Mask with
                    | 0u ->
                        let mutable l = n.MapLeft
                        if tryRemove cmp hash key &value &l then
                            if isNull l then node <- n.MapRight
                            else node <- Inner(n.Prefix, n.Mask, l, n.MapRight, count l + count n.MapRight)
                            true
                        else
                            false
                    | 1u ->
                        let mutable r = n.MapRight
                        if tryRemove cmp hash key &value &r then
                            if isNull r then node <- n.MapLeft
                            else node <- Inner(n.Prefix, n.Mask, n.MapLeft, r, count n.MapLeft + count r)
                            true
                        else
                            false
                    | _ ->
                        false
                | _ ->
                    false


        let rec toRefCountList (acc : list<'T * int>) (node : Node<'T>) =
            if isNull node then
                acc
            else
                match node with
                | :? NoCollisionLeaf<'T> as n -> (n.Value, n.RefCount) :: acc
                | :? Leaf<'T> as n -> (n.Value, n.RefCount) :: OverflowList.toRefCountList acc n.RefNext
                | :? Inner<'T> as n ->
                    toRefCountList (toRefCountList acc n.MapRight) n.MapLeft
                | _ ->
                    acc

        let rec copyToRefCount (dst : ('T * int)[]) (index : int) (node : Node<'T>) =
            if isNull node then
                index
            else
                match node with
                | :? NoCollisionLeaf<'T> as n ->
                    dst.[index] <- (n.Value, n.RefCount)
                    index + 1
                | :? Leaf<'T> as n ->
                    let mutable index = index
                    dst.[index] <- (n.Value, n.RefCount)
                    index <- index + 1
                    let mutable c = n.RefNext
                    while not (isNull c) do
                        dst.[index] <- (c.Value, c.RefCount)
                        index <- index + 1
                        c <- c.RefNext
                    index
                | :? Inner<'T> as n ->
                    let i1 = copyToRefCount dst index n.MapLeft
                    copyToRefCount dst i1 n.MapRight
                | _ ->
                    index

        let rec union (cmp : IEqualityComparer<'T>) (a : Node<'T>) (b : Node<'T>) =
            if isNull a then b
            elif isNull b then a
            else
                match a with
                | :? NoCollisionLeaf<'T> as a ->
                    match b with
                    | :? NoCollisionLeaf<'T> as b ->
                        if a.Hash = b.Hash && cmp.Equals(a.Value, b.Value) then
                            let r = a.RefCount + b.RefCount
                            NoCollisionLeaf(a.Hash, b.Value, r) :> Node<_>
                        else
                            join a.Hash a b.Hash b
                    | :? Leaf<'T> as b ->
                        if a.Hash = b.Hash then
                            if cmp.Equals(a.Value, b.Value) then
                                let r = a.RefCount + b.RefCount
                                Leaf(a.Hash, b.Value, r, b.RefNext) :> Node<_>
                            else
                                Leaf(b.Hash, b.Value, b.RefCount, OverflowList.addRefCount cmp a.Value a.RefCount b.RefNext) :> Node<_>
                        else
                            join a.Hash a b.Hash b
                    | :? Inner<'T> as b ->
                        match matchPrefixAndGetBit a.Hash b.Prefix b.Mask with
                        | 0u ->
                            Inner(b.Prefix, b.Mask, union cmp a b.MapLeft, b.MapRight) :> Node<_>
                        | 1u ->
                            Inner(b.Prefix, b.Mask, b.MapLeft, union cmp a b.MapRight) :> Node<_>
                        | _ ->
                            join a.Hash a b.Prefix b
                    | _ ->
                        null // unreachable

                | :? Leaf<'T> as a ->
                    match b with
                    | :? NoCollisionLeaf<'T> as b ->
                        if a.Hash = b.Hash then
                            if cmp.Equals(a.Value, b.Value) then
                                let r = a.RefCount + b.RefCount
                                Leaf(a.Hash, b.Value, r, a.RefNext) :> Node<_>
                            else
                                Leaf(a.Hash, a.Value, a.RefCount, OverflowList.addRefCount cmp b.Value b.RefCount a.RefNext) :> Node<_>
                        else
                            join a.Hash a b.Hash b
                    | :? Leaf<'T> as b ->
                        if a.Hash = b.Hash then
                            if cmp.Equals(a.Value, b.Value) then
                                let r = a.RefCount + b.RefCount
                                Leaf(a.Hash, b.Value, r, OverflowList.union cmp a.RefNext b.RefNext) :> Node<_>
                            else    
                                let la = OverflowList(a.Value, a.RefCount, a.RefNext)
                                let lb = OverflowList(b.Value, b.RefCount, b.RefNext)
                                let l = OverflowList.union cmp la lb
                                Leaf(a.Hash, l.Value, l.RefCount, l.RefNext) :> Node<_>
                                 
                        else
                            join a.Hash a b.Hash b
                    | :? Inner<'T> as b ->
                        match matchPrefixAndGetBit a.Hash b.Prefix b.Mask with
                        | 0u ->
                            Inner(b.Prefix, b.Mask, union cmp a b.MapLeft, b.MapRight) :> Node<_>
                        | 1u ->
                            Inner(b.Prefix, b.Mask, b.MapLeft, union cmp a b.MapRight) :> Node<_>
                        | _ ->
                            join a.Hash a b.Prefix b
                    | _ ->
                        null // unreachable

                | :? Inner<'T> as a ->
                    match b with
                    | :? NoCollisionLeaf<'T> as b ->
                        match matchPrefixAndGetBit b.Hash a.Prefix a.Mask with
                        | 0u -> Inner(a.Prefix, a.Mask, union cmp a.MapLeft b, a.MapRight) :> Node<_>
                        | 1u -> Inner(a.Prefix, a.Mask, a.MapLeft, union cmp a.MapRight b) :> Node<_>
                        | _ -> join a.Prefix a b.Hash b
                    | :? Leaf<'T> as b ->
                        match matchPrefixAndGetBit b.Hash a.Prefix a.Mask with
                        | 0u -> Inner(a.Prefix, a.Mask, union cmp a.MapLeft b, a.MapRight) :> Node<_>
                        | 1u -> Inner(a.Prefix, a.Mask, a.MapLeft, union cmp a.MapRight b) :> Node<_>
                        | _ -> join a.Prefix a b.Hash b
                    | :? Inner<'T> as b ->
                        let cc = compareMasks a.Mask b.Mask
                        if cc > 0 then
                            // a in b
                            match matchPrefixAndGetBit a.Prefix b.Prefix b.Mask with
                            | 0u -> Inner(b.Prefix, b.Mask, union cmp a b.MapLeft, b.MapRight) :> Node<_>
                            | 1u -> Inner(b.Prefix, b.Mask, b.MapLeft, union cmp a b.MapRight) :> Node<_>
                            | _ -> join a.Prefix a b.Prefix b
                        elif cc < 0 then  
                            // b in a 
                            match matchPrefixAndGetBit b.Prefix a.Prefix a.Mask with
                            | 0u -> Inner(a.Prefix, a.Mask, union cmp a.MapLeft b, a.MapRight) :> Node<_>
                            | 1u -> Inner(a.Prefix, a.Mask, a.MapLeft, union cmp a.MapRight b) :> Node<_>
                            | _ -> join a.Prefix a b.Prefix b
                        elif a.Prefix = b.Prefix then
                            Inner(a.Prefix, a.Mask, union cmp a.MapLeft b.MapLeft, union cmp a.MapRight b.MapRight) :> Node<_>
                        else 
                            join a.Prefix a b.Prefix b
                    | _ -> 
                        null // unreachable
                | _ ->
                    null // unreachable
                    
        let rec private toHashMapNode (positive : bool) (node : Node<'T>) =
            if isNull node then
                HashMapEmpty<'T, int>.Instance
            else
                match node with
                | :? NoCollisionLeaf<'T> as n ->
                    HashMapNoCollisionLeaf.New(n.Hash, n.Value, (if positive then n.RefCount else -n.RefCount)) 
                | :? Leaf<'T> as n ->
                    let mutable c = n.RefNext
                    HashMapCollisionLeaf.New(n.Hash, n.Value, (if positive then n.RefCount else -n.RefCount), OverflowList.toHashMapLinked positive n.RefNext)
                | :? Inner<'T> as n ->
                    HashMapInner<'T, int>.New(n.Prefix, n.Mask, toHashMapNode positive n.MapLeft, toHashMapNode positive n.MapRight)
                | _ ->
                    HashMapEmpty<'T, int>.Instance

        let rec computeDelta (cmp : IEqualityComparer<'T>) (a : Node<'T>) (b : Node<'T>) =
            if isNull a then toHashMapNode true b
            elif isNull b then toHashMapNode false a
            elif System.Object.ReferenceEquals(a, b) then HashMapEmpty.Instance
            else
                match a with
                | :? NoCollisionLeaf<'T> as a ->
                    match b with
                    | :? NoCollisionLeaf<'T> as b ->
                        if a.Hash = b.Hash && cmp.Equals(a.Value, b.Value) then
                            let r = b.RefCount - a.RefCount
                            if r <> 0 then HashMapNoCollisionLeaf.New(a.Hash, a.Value, r)
                            else HashMapEmpty.Instance
                        else
                            let add = toHashMapNode true b
                            let rem = toHashMapNode false a
                            HashMapInner.Join(a.Hash, rem, b.Hash, add)
                    | :? Leaf<'T> as b ->
                        if a.Hash = b.Hash then
                            if cmp.Equals(a.Value, b.Value) then
                                let r = b.RefCount - a.RefCount
                                if r <> 0 then
                                    HashMapCollisionLeaf.New(a.Hash, a.Value, r, OverflowList.toHashMapLinked false b.RefNext)
                                else
                                    let l = OverflowList.toHashMapLinked false b.RefNext
                                    if isNull l.Next then HashMapNoCollisionLeaf.New(a.Hash, l.Key, l.Value)
                                    else HashMapCollisionLeaf.New(a.Hash, l.Key, l.Value, l.Next)
                            else
                                let mutable bn = b.RefNext
                                let mutable r = 0
                                if OverflowList.tryRemove cmp a.Value &r &bn then
                                    let d = r - a.RefCount
                                    let next =
                                        if d <> 0 then HashMapLinked(a.Value, d, OverflowList.toHashMapLinked true bn)
                                        else OverflowList.toHashMapLinked true bn

                                    if isNull next then
                                        HashMapNoCollisionLeaf.New(a.Hash, b.Value, b.RefCount)
                                    else
                                        HashMapCollisionLeaf.New(a.Hash, b.Value, b.RefCount, next)
                                else
                                    let next = HashMapLinked(b.Value, b.RefCount, OverflowList.toHashMapLinked true b.RefNext)
                                    HashMapCollisionLeaf.New(a.Hash, a.Value, -a.RefCount, next)
                        else
                            let add = toHashMapNode true b
                            let rem = toHashMapNode false a
                            HashMapInner.Join(a.Hash, rem, b.Hash, add)
                    | :? Inner<'T> as b ->
                        match matchPrefixAndGetBit a.Hash b.Prefix b.Mask with
                        | 0u ->
                            HashMapInner.Create(b.Prefix, b.Mask, computeDelta cmp a b.MapLeft, toHashMapNode true b.MapRight)
                        | 1u ->
                            HashMapInner.Create(b.Prefix, b.Mask, toHashMapNode true b.MapLeft, computeDelta cmp a b.MapRight)
                        | _ ->
                            let add = toHashMapNode true b
                            let rem = toHashMapNode false a
                            HashMapInner.Join(a.Hash, rem, b.Prefix, add)
                    | _ ->
                        HashMapEmpty.Instance // unreachable

                | :? Leaf<'T> as a ->
                    match b with
                    | :? NoCollisionLeaf<'T> as b ->
                        if a.Hash = b.Hash then
                            if cmp.Equals(a.Value, b.Value) then
                                let r = b.RefCount - a.RefCount
                                if r <> 0 then HashMapCollisionLeaf.New(a.Hash, a.Value, r, OverflowList.toHashMapLinked false a.RefNext) 
                                else
                                    let l = OverflowList.toHashMapLinked false a.RefNext
                                    if isNull l.Next then
                                        HashMapNoCollisionLeaf.New(a.Hash, l.Key, l.Value)
                                    else
                                        HashMapCollisionLeaf.New(a.Hash, l.Key, l.Value, l.Next)
                            else
                                let mutable an = a.RefNext
                                let mutable r = 0
                                if OverflowList.tryRemove cmp b.Value &r &an then
                                    let d = b.RefCount - r
                                    let next =
                                        if d <> 0 then HashMapLinked(b.Value, d, OverflowList.toHashMapLinked false an)
                                        else OverflowList.toHashMapLinked false an
                                        
                                    if isNull next then
                                        HashMapNoCollisionLeaf.New(a.Hash, a.Value, -a.RefCount)
                                    else
                                        HashMapCollisionLeaf.New(a.Hash, a.Value, -a.RefCount, next)
                                else
                                    let next = HashMapLinked(a.Value, -a.RefCount, OverflowList.toHashMapLinked false a.RefNext)
                                    HashMapCollisionLeaf.New(a.Hash, b.Value, b.RefCount, next)
                        else
                            let add = toHashMapNode true b
                            let rem = toHashMapNode false a
                            HashMapInner.Join(a.Hash, rem, b.Hash, add)
                    | :? Leaf<'T> as b ->
                        if a.Hash = b.Hash then
                            if cmp.Equals(a.Value, b.Value) then
                                let r = b.RefCount - a.RefCount
                                let next = OverflowList.computeDelta cmp a.RefNext b.RefNext
                                if r <> 0 then
                                    if isNull next then HashMapNoCollisionLeaf.New(a.Hash, a.Value, r)
                                    else HashMapCollisionLeaf.New(a.Hash, a.Value, r, next)
                                else
                                    if isNull next then HashMapEmpty.Instance
                                    elif isNull next.Next then HashMapNoCollisionLeaf.New(a.Hash, next.Key, next.Value)
                                    else HashMapLeaf.New(a.Hash, next.Key, next.Value, next.Next)
                            else    
                                let la = OverflowList(a.Value, a.RefCount, a.RefNext)
                                let lb = OverflowList(b.Value, b.RefCount, b.RefNext)
                                let l = OverflowList.computeDelta cmp la lb
                                if isNull l then HashMapEmpty.Instance
                                elif isNull l.Next then HashMapNoCollisionLeaf.New(a.Hash, l.Key, l.Value)
                                else HashMapCollisionLeaf.New(a.Hash, l.Key, l.Value, l.Next)
                                 
                        else
                            let add = toHashMapNode true b
                            let rem = toHashMapNode false a
                            HashMapInner.Join(a.Hash, rem, b.Hash, add)
                    | :? Inner<'T> as b ->
                        match matchPrefixAndGetBit a.Hash b.Prefix b.Mask with
                        | 0u ->
                            HashMapInner.Create(b.Prefix, b.Mask, computeDelta cmp a b.MapLeft, toHashMapNode true b.MapRight)
                        | 1u ->
                            HashMapInner.Create(b.Prefix, b.Mask, toHashMapNode true b.MapLeft, computeDelta cmp a b.MapRight)
                        | _ ->
                            let add = toHashMapNode true b
                            let rem = toHashMapNode false a
                            HashMapInner.Join(a.Hash, rem, b.Prefix, add)
                    | _ ->
                        HashMapEmpty.Instance // unreachable

                | :? Inner<'T> as a ->
                    match b with
                    | :? NoCollisionLeaf<'T> as b ->
                        match matchPrefixAndGetBit b.Hash a.Prefix a.Mask with
                        | 0u -> HashMapInner.Create(a.Prefix, a.Mask, computeDelta cmp a.MapLeft b, toHashMapNode false a.MapRight)
                        | 1u -> HashMapInner.Create(a.Prefix, a.Mask, toHashMapNode false a.MapLeft, computeDelta cmp a.MapRight b)
                        | _ ->
                            let add = toHashMapNode true b
                            let rem = toHashMapNode false a
                            HashMapInner.Join(a.Prefix, rem, b.Hash, add)

                    | :? Leaf<'T> as b ->
                        match matchPrefixAndGetBit b.Hash a.Prefix a.Mask with
                        | 0u -> HashMapInner.Create(a.Prefix, a.Mask, computeDelta cmp a.MapLeft b, toHashMapNode false a.MapRight)
                        | 1u -> HashMapInner.Create(a.Prefix, a.Mask, toHashMapNode false a.MapLeft, computeDelta cmp a.MapRight b)
                        | _ ->
                            let add = toHashMapNode true b
                            let rem = toHashMapNode false a
                            HashMapInner.Join(a.Prefix, rem, b.Hash, add)
                    | :? Inner<'T> as b ->
                        let cc = compareMasks a.Mask b.Mask
                        if cc > 0 then
                            // a in b
                            match matchPrefixAndGetBit a.Prefix b.Prefix b.Mask with
                            | 0u -> HashMapInner.Create(b.Prefix, b.Mask, computeDelta cmp a b.MapLeft, toHashMapNode true b.MapRight)
                            | 1u -> HashMapInner.Create(b.Prefix, b.Mask, toHashMapNode true b.MapLeft, computeDelta cmp a b.MapRight)
                            | _ ->
                                let add = toHashMapNode true b
                                let rem = toHashMapNode false a
                                HashMapInner.Join(a.Prefix, rem, b.Prefix, add)
                        elif cc < 0 then  
                            // b in a 
                            match matchPrefixAndGetBit b.Prefix a.Prefix a.Mask with
                            | 0u -> HashMapInner.Create(a.Prefix, a.Mask, computeDelta cmp a.MapLeft b, toHashMapNode false a.MapRight)
                            | 1u -> HashMapInner.Create(a.Prefix, a.Mask, toHashMapNode false a.MapLeft, computeDelta cmp a.MapRight b)
                            | _ -> 
                                let add = toHashMapNode true b
                                let rem = toHashMapNode false a
                                HashMapInner.Join(a.Prefix, rem, b.Prefix, add)
                        elif a.Prefix = b.Prefix then
                            HashMapInner.Create(
                                a.Prefix, a.Mask, 
                                computeDelta cmp a.MapLeft b.MapLeft, 
                                computeDelta cmp a.MapRight b.MapRight
                            )
                        else 
                            let add = toHashMapNode true b
                            let rem = toHashMapNode false a
                            HashMapInner.Join(a.Prefix, rem, b.Prefix, add)
                    | _ -> 
                        HashMapEmpty.Instance // unreachable
                | _ ->
                    HashMapEmpty.Instance // unreachable

        let rec applyDeltaEmptyState (delta : byref<HashMapNode<'T, int>>) =
            match delta with
            | :? HashMapEmpty<'T, int> ->
                null

            | :? HashMapNoCollisionLeaf<'T, int> as d ->
                if d.Value > 0 then
                    if d.Value > 1 then
                        delta <- HashMapNoCollisionLeaf.New(d.Hash, d.Key, 1)
                    NoCollisionLeaf(d.Hash, d.Key, d.Value) :> Node<_>
                else
                    delta <- HashMapEmpty.Instance
                    null

            | :? HashMapCollisionLeaf<'T, int> as d ->
                if d.Value > 0 then
                    let mutable deltaNext = d.Next
                    let stateNext = OverflowList.applyDeltaNoState &deltaNext

                    if isNull deltaNext then delta <- HashMapNoCollisionLeaf.New(d.Hash, d.Key, 1)
                    else delta <- HashMapCollisionLeaf.New(d.Hash, d.Key, 1, deltaNext)

                    if isNull stateNext then NoCollisionLeaf(d.Hash, d.Key, d.Value) :> Node<_>
                    else Leaf(d.Hash, d.Key, d.Value, stateNext) :> Node<_>
                else
                    let mutable dd = d.Next
                    let state = OverflowList.applyDeltaNoState &dd

                    if isNull dd then delta <- HashMapEmpty.Instance
                    elif isNull dd.Next then delta <- HashMapNoCollisionLeaf.New(d.Hash, dd.Key, dd.Value)
                    else delta <- HashMapCollisionLeaf.New(d.Hash, dd.Key, dd.Value, dd.Next)

                    if isNull state then null
                    elif isNull state.Next then NoCollisionLeaf(d.Hash, state.Value, state.RefCount) :> Node<_>
                    else Leaf(d.Hash, state.Value, state.RefCount, state.RefNext) :> Node<_>

            | :? HashMapInner<'T, int> as d ->
                let mutable ld = d.Left
                let mutable rd = d.Right
                let ls = applyDeltaEmptyState &ld
                let rs = applyDeltaEmptyState &rd
                delta <- HashMapInner.Create(d.Prefix, d.Mask, ld, rd)
                if isNull ls then rs
                elif isNull rs then ls
                else Inner(d.Prefix, d.Mask, ls, rs) :> Node<_>

            | _ ->
                null

        let rec applyDelta (cmp : IEqualityComparer<'T>) (state : byref<Node<'T>>) (delta : HashMapNode<'T, int>) =
            if delta.IsEmpty then
                delta

            elif isNull state then
                let mutable delta = delta
                state <- applyDeltaEmptyState &delta
                delta
            
            else
                match state with
                | :? NoCollisionLeaf<'T> as s ->
                    match delta with
                    | :? HashMapNoCollisionLeaf<'T, int> as d ->
                        if s.Hash = d.Hash && cmp.Equals(s.Value, d.Key) then
                            let r = s.RefCount + d.Value
                            if r > 0 then 
                                state <- NoCollisionLeaf(s.Hash, s.Value, r)
                                HashMapEmpty.Instance
                            else 
                                state <- null
                                HashMapNoCollisionLeaf.New(s.Hash, s.Value, -1)
                        elif d.Value > 0 then
                            state <- join s.Hash s d.Hash (NoCollisionLeaf(d.Hash, d.Key, d.Value))
                            HashMapNoCollisionLeaf.New(d.Hash, d.Key, 1)
                        else
                            HashMapEmpty.Instance

                    | :? HashMapCollisionLeaf<'T, int> as d ->
                        if s.Hash = d.Hash then
                            let ld = HashMapLinked(d.Key, d.Value, d.Next)
                            match HashMapLinked.tryRemove cmp s.Value ld with
                            | ValueSome struct(delta, ld) ->
                                let r = s.RefCount + delta
                                if r > 0 then
                                    let mutable ld = ld
                                    let s = OverflowList.applyDeltaNoState &ld
                                    if isNull s then  state <- NoCollisionLeaf(d.Hash, d.Key, r)
                                    else state <- Leaf(d.Hash, d.Key, r, s)
                                    
                                    if isNull ld then HashMapNoCollisionLeaf.New(d.Hash, d.Key, 1)
                                    else HashMapCollisionLeaf.New(d.Hash, d.Key, 1, ld)
                                else
                                    let mutable ld = ld
                                    let s = OverflowList.applyDeltaNoState &ld

                                    if isNull s then state <- null
                                    elif isNull s.Next then state <- NoCollisionLeaf(d.Hash, s.Value, s.RefCount)
                                    else state <- Leaf(d.Hash, s.Value, s.RefCount, s.RefNext)

                                    if isNull ld then HashMapEmpty.Instance
                                    elif isNull ld.Next then HashMapNoCollisionLeaf.New(d.Hash, ld.Key, ld.Value)
                                    else HashMapCollisionLeaf.New(d.Hash, ld.Key, ld.Value, ld.Next)

                            | ValueNone ->
                                failwith ""
                        else
                            let mutable delta = delta
                            state <- applyDeltaEmptyState &delta
                            delta

                    | :? HashMapInner<'T, int> as d ->
                        match matchPrefixAndGetBit s.Hash d.Prefix d.Mask with
                        | 0u -> 
                            let mutable l = state
                            let mutable r = null
                            let lops = applyDelta cmp &l d.Left
                            let rops = applyDelta cmp &r d.Right
                            state <- 
                                if isNull r then l
                                elif isNull l then r
                                else Inner(d.Prefix, d.Mask, l, r) :> Node<_>
                            HashMapInner.Create(d.Prefix, d.Mask, lops, rops)
                        | 1u ->
                            let mutable l = null
                            let mutable r = state
                            let lops = applyDelta cmp &l d.Left
                            let rops = applyDelta cmp &r d.Right
                            state <- 
                                if isNull r then l
                                elif isNull l then r
                                else Inner(d.Prefix, d.Mask, l, r) :> Node<_>
                            HashMapInner.Create(d.Prefix, d.Mask, lops, rops)
                        | _ ->
                            let mutable l = null
                            let mutable r = null
                            let lops = applyDelta cmp &l d.Left
                            let rops = applyDelta cmp &r d.Right
                            state <- 
                                if isNull r then l
                                elif isNull l then r
                                else Inner(d.Prefix, d.Mask, l, r) :> Node<_>
                            HashMapInner.Create(d.Prefix, d.Mask, lops, rops)

                    | _ ->
                        HashMapEmpty.Instance // uncreachable
                | :? Leaf<'T> as s ->
                    match delta with
                    | :? HashMapNoCollisionLeaf<'T, int> as d ->
                        if s.Hash = d.Hash then
                            let mutable l = OverflowList(s.Value, s.RefCount, s.RefNext)
                            let mutable o = 0
                            if OverflowList.tryRemove cmp d.Key &o &l then
                                let r = o + d.Value
                                if r > 0 then
                                    if isNull l then state <- NoCollisionLeaf(d.Hash, d.Key, r)
                                    else state <- Leaf(d.Hash, d.Key, r, l)
                                    HashMapNoCollisionLeaf.New(d.Hash, d.Key, 1)
                                else
                                    if isNull l then state <- null
                                    elif isNull l.Next then state <- NoCollisionLeaf(d.Hash, l.Value, l.RefCount)
                                    else state <- Leaf(d.Hash, l.Value, l.RefCount, l.RefNext)
                                    HashMapNoCollisionLeaf.New(d.Hash, d.Key, -1)

                            else
                                let mutable delta = delta
                                let newState = applyDeltaEmptyState &delta
                                state <- join s.Hash state d.Hash newState
                                delta
                        else
                            let mutable delta = delta
                            let newState = applyDeltaEmptyState &delta
                            state <- join s.Hash state d.Hash newState
                            delta

                    | :? HashMapCollisionLeaf<'T, int> as d ->
                        if s.Hash = d.Hash then
                            let hash = d.Hash
                            let mutable s = OverflowList(s.Value, s.RefCount, s.RefNext)
                            let d = HashMapLinked(d.Key, d.Value, d.Next)

                            let d = OverflowList.applyDelta cmp &s d
                            if isNull s then state <- null
                            elif isNull s.Next then state <- NoCollisionLeaf(hash, s.Value, s.RefCount)
                            else state <- Leaf(hash, s.Value, s.RefCount, s.RefNext)

                            if isNull d then HashMapEmpty.Instance
                            elif isNull d.Next then HashMapNoCollisionLeaf.New(hash, d.Key, d.Value)
                            else HashMapCollisionLeaf.New(hash, d.Key, d.Value, d.Next)

                        else
                            let mutable delta = delta
                            let newState = applyDeltaEmptyState &delta
                            state <- join s.Hash state d.Hash newState
                            delta

                    | :? HashMapInner<'T, int> as d ->
                        match matchPrefixAndGetBit s.Hash d.Prefix d.Mask with
                        | 0u -> 
                            let mutable l = state
                            let mutable r = null
                            let lops = applyDelta cmp &l d.Left
                            let rops = applyDelta cmp &r d.Right
                            state <- 
                                if isNull r then l
                                elif isNull l then r
                                else Inner(d.Prefix, d.Mask, l, r) :> Node<_>
                            HashMapInner.Create(d.Prefix, d.Mask, lops, rops)
                        | 1u ->
                            let mutable l = null
                            let mutable r = state
                            let lops = applyDelta cmp &l d.Left
                            let rops = applyDelta cmp &r d.Right
                            state <- 
                                if isNull r then l
                                elif isNull l then r
                                else Inner(d.Prefix, d.Mask, l, r) :> Node<_>
                            HashMapInner.Create(d.Prefix, d.Mask, lops, rops)
                        | _ ->
                            let mutable l = null
                            let mutable r = null
                            let lops = applyDelta cmp &l d.Left
                            let rops = applyDelta cmp &r d.Right
                            state <- 
                                if isNull r then l
                                elif isNull l then r
                                else Inner(d.Prefix, d.Mask, l, r) :> Node<_>
                            HashMapInner.Create(d.Prefix, d.Mask, lops, rops)
                    | _ ->
                        HashMapEmpty.Instance // uncreachable
                | :? Inner<'T> as s ->  
                    match delta with
                    | :? HashMapLeaf<'T, int> as d ->
                        match matchPrefixAndGetBit d.LHash s.Prefix s.Mask with
                        | 0u ->
                            let mutable l = s.MapLeft
                            let ops = applyDelta cmp &l delta
                            if isNull l then state <- s.MapRight
                            else state <- Inner(s.Prefix, s.Mask, l, s.MapRight)
                            ops
                        | 1u ->
                            let mutable r = s.MapRight
                            let ops = applyDelta cmp &r delta
                            if isNull r then state <- s.MapLeft
                            else state <- Inner(s.Prefix, s.Mask, s.MapLeft, r)
                            ops
                        | _ ->
                            let mutable delta = delta
                            let newState = applyDeltaEmptyState &delta
                            state <- join d.LHash newState s.Prefix s
                            delta

                    | :? HashMapInner<'T, int> as d ->
                        let cc = compareMasks s.Mask d.Mask
                        if cc > 0 then
                            // s in d
                            match matchPrefixAndGetBit d.Prefix d.Prefix d.Mask with
                            | 0u -> 
                                let mutable ls = state
                                let mutable rs = null
                                let ld = applyDelta cmp &ls d.Left
                                let rd = applyDelta cmp &rs d.Right

                                if isNull rs then state <- ls
                                elif isNull ls then state <- rs
                                else state <- Inner(d.Prefix, d.Mask, ls, rs)

                                if ld.IsEmpty then rd
                                elif rd.IsEmpty then ld
                                else HashMapInner.New(d.Prefix, d.Mask, ld, rd)
                            | 1u ->
                                let mutable ls = null
                                let mutable rs = state
                                let ld = applyDelta cmp &ls d.Left
                                let rd = applyDelta cmp &rs d.Right

                                if isNull rs then state <- ls
                                elif isNull ls then state <- rs
                                else state <- Inner(d.Prefix, d.Mask, ls, rs)

                                if ld.IsEmpty then rd
                                elif rd.IsEmpty then ld
                                else HashMapInner.New(d.Prefix, d.Mask, ld, rd)
                            | _ ->
                                let mutable delta = delta
                                let newState = applyDeltaEmptyState &delta
                                state <- join d.Prefix newState s.Prefix s
                                delta
                                
                        elif cc < 0 then
                            // d in s
                            match matchPrefixAndGetBit d.Prefix s.Prefix s.Mask with
                            | 0u -> 
                                let mutable ls = s.MapLeft
                                let delta = applyDelta cmp &ls delta
                                if isNull ls then state <- s.MapRight
                                else state <- Inner(s.Prefix, s.Mask, ls, s.MapRight)
                                delta
                            | 1u ->
                                let mutable rs = s.MapRight
                                let delta = applyDelta cmp &rs delta
                                if isNull rs then state <- s.MapLeft
                                else state <- Inner(s.Prefix, s.Mask, s.MapLeft, rs)
                                delta

                            | _ ->
                                let mutable delta = delta
                                let newState = applyDeltaEmptyState &delta
                                state <- join d.Prefix newState s.Prefix s
                                delta
                        else
                            //equal
                            let mutable ls = s.MapLeft
                            let mutable rs = s.MapRight
                            let ld = applyDelta cmp &ls d.Left
                            let rd = applyDelta cmp &rs d.Right

                            if isNull ls then state <- rs
                            elif isNull rs then state <- ls
                            else state <- Inner(s.Prefix, s.Mask, ls, rs)

                            if ld.IsEmpty then rd
                            elif rd.IsEmpty then ld
                            else HashMapInner.New(d.Prefix, d.Mask, ld, rd)
                    | _ ->
                        HashMapEmpty.Instance // uncreachable
                | _ ->
                    HashMapEmpty.Instance // uncreachable

        let toSetNode (node : Node<'T>) =
            if isNull node then HashSetEmpty.Instance
            else unbox<HashSetNode<'T>> node

    [<StructuredFormatDisplay("{AsString}")>]
    type CountingHashSet<'T> internal(comparer : IEqualityComparer<'T>, root : Nodes.Node<'T>) =

        static let defaultComparer = DefaultEqualityComparer<'T>.Instance


        static let empty = CountingHashSet<'T>(defaultComparer, null)

        static member Empty = empty
        
        static member Singleton(value : 'T) = 
            let cmp = defaultComparer
            let hash = uint32 (cmp.GetHashCode value)
            CountingHashSet<'T>(cmp, NoCollisionLeaf(hash, value, 1))

        member private x.AsString = x.ToString()

        
        static member FromSeq(elements : seq<'T>) =
            let mutable root = null
            let cmp = defaultComparer
            for e in elements do
                addInPlace cmp (uint32 (cmp.GetHashCode e)) e 1 &root |> ignore
            CountingHashSet(cmp, root)

        static member FromList(elements : list<'T>) =
            let mutable root = null
            let cmp = defaultComparer
            for e in elements do
                addInPlace cmp (uint32 (cmp.GetHashCode e)) e 1 &root |> ignore
            CountingHashSet(cmp, root)

        static member FromArray(elements : 'T[]) =
            let mutable root = null
            let cmp = defaultComparer
            for e in elements do
                addInPlace cmp (uint32 (cmp.GetHashCode e)) e 1 &root |> ignore
            CountingHashSet(cmp, root)

        member x.Count = count root
        member private x.Root = root

        member x.ToHashSet() =
            FSharp.Data.Adaptive.HashSet<'T>(comparer, toSetNode root)

        member x.Add(key : 'T) =
            let hash = uint32 (comparer.GetHashCode key)
            let newRoot = add comparer hash key 1  root
            CountingHashSet<'T>(comparer, newRoot)

        member x.Remove(key : 'T) =
            let hash = uint32 (comparer.GetHashCode key)
            let newRoot = add comparer hash key -1 root
            CountingHashSet<'T>(comparer, newRoot)
   
        member x.RemoveAll(key : 'T) =
            let hash = uint32 (comparer.GetHashCode key)
            let mutable old = 0
            let mutable r = root
            if tryRemove comparer hash key &old &r then
                CountingHashSet<'T>(comparer, r)
            else
                x

        member x.TryRemoveAll(key : 'T) =
            let hash = uint32 (comparer.GetHashCode key)
            let mutable old = 0
            let mutable r = root
            if tryRemove comparer hash key &old &r then
                Some(old, CountingHashSet<'T>(comparer, r))
            else
                None
            
            
        member x.TryRemoveAllV(key : 'T) =
            let hash = uint32 (comparer.GetHashCode key)
            let mutable old = 0
            let mutable r = root
            if tryRemove comparer hash key &old &r then
                ValueSome struct(old, CountingHashSet<'T>(comparer, r))
            else
                ValueNone

        member x.UnionWith(other : CountingHashSet<'T>) =
            CountingHashSet<'T>(comparer, union comparer root other.Root)

        member x.ComputeDeltaTo(other : CountingHashSet<'T>) =
            HashMap<'T, int>(comparer, computeDelta comparer root other.Root)
            |> HashSetDelta

        member x.ApplyDelta(delta : HashSetDelta<'T>) =
            let mutable root = root
            let newDelta = HashMap<'T, int>(comparer, applyDelta comparer &root delta.Store.Root) |> HashSetDelta
            CountingHashSet(comparer, root), newDelta


        member x.GetRefCount(key : 'T) =
            let hash = uint32 (comparer.GetHashCode key)
            getRefCount comparer hash key root

        member x.ToList() =
            if isNull root then []
            else (unbox<HashSetNode<'T>> root).ToList []
            
        member x.ToArray() =
            if isNull root then 
                [||]
            else 
                let r = root |> unbox<HashSetNode<'T>>
                let dst = Array.zeroCreate r.Count
                r.CopyTo(dst, 0) |> ignore
                dst
                
        member x.ToRefCountList() =
            toRefCountList [] root
            
        member x.ToRefCountArray() =
            let arr = Array.zeroCreate x.Count
            copyToRefCount arr 0 root |> ignore
            arr

        member x.GetEnumerator() = new CountingHashSetEnumerator<_>(root)
        
        interface System.Collections.IEnumerable with
            member x.GetEnumerator() = new CountingHashSetEnumerator<_>(root) :> _

        interface System.Collections.Generic.IEnumerable<'T> with
            member x.GetEnumerator() = new CountingHashSetEnumerator<_>(root) :> _
         
        override x.ToString() =
            if x.Count > 10 then
                x 
                |> Seq.truncate 10 
                |> Seq.map (sprintf "%A") 
                |> String.concat "; "
                |> sprintf "CountingHashSet { %s; ... }"
            else
                x 
                |> Seq.map (sprintf "%A") 
                |> String.concat "; "
                |> sprintf "CountingHashSet { %s }"


    and [<Struct>] CountingHashSetEnumerator<'T> =
        val mutable internal Root : Node<'T>
        val mutable internal Head : Node<'T>
        val mutable internal Stack : list<Node<'T>>
        val mutable internal Next : OverflowList<'T>
        val mutable internal Value : 'T

        member x.MoveNext() =
            if not (isNull x.Next) then
                let h = x.Next
                x.Next <- h.RefNext
                x.Value <- h.Value
                true
            elif isNull x.Head then
                false
            else
                let h = x.Head
                if List.isEmpty x.Stack then x.Head <- null
                else x.Head <- List.head x.Stack; x.Stack <- List.tail x.Stack

                match h with
                | :? NoCollisionLeaf<'T> as h ->
                    x.Value <- h.Value
                    true
                | :? Leaf<'T> as h -> 
                    x.Value <- h.Value
                    x.Next <- h.RefNext
                    true
                | :? Inner<'T> as h ->
                    x.Stack <- h.MapRight :: x.Head :: x.Stack
                    x.Head <- h.MapLeft
                    x.MoveNext()
                | _ ->
                    false

        member x.Current = x.Value

        member x.Reset() =
            x.Head <- x.Root
            x.Stack <- []
            x.Next <- null
            x.Value <- Unchecked.defaultof<_>

        member x.Dispose() =
            x.Root <- null
            x.Head <- null
            x.Stack <- []
            x.Next <- null
            x.Value <- Unchecked.defaultof<_>
            
        interface System.Collections.IEnumerator with
            member x.MoveNext() = x.MoveNext()
            member x.Reset() = x.Reset()
            member x.Current = x.Current :> obj

        interface System.Collections.Generic.IEnumerator<'T> with
            member x.Current = x.Current
            member x.Dispose() = x.Dispose()

        internal new(root : Node<'T>) =
            {
                Root = root
                Head = root
                Stack = []
                Next = null
                Value = Unchecked.defaultof<_>
            }


    [<GeneralizableValue>]
    let empty<'T> = CountingHashSet<'T>.Empty
    let inline single (value : 'T) = CountingHashSet<'T>.Singleton value

    let inline ofSeq (elements : list<'T>) = CountingHashSet<'T>.FromSeq elements
    let inline ofList (elements : list<'T>) = CountingHashSet<'T>.FromList elements
    let inline ofArray (elements : 'T[]) = CountingHashSet<'T>.FromArray elements

    let inline add value (map : CountingHashSet<'T>) = map.Add(value)
    let inline remove value (map : CountingHashSet<'T>) = map.Remove(value)
    let inline removeAll value (map : CountingHashSet<'T>) = map.RemoveAll(value)
    let inline tryRemoveAll value (map : CountingHashSet<'T>) = map.TryRemoveAll(value)
    let inline tryRemoveAllV value (map : CountingHashSet<'T>) = map.TryRemoveAllV(value)
    
    let inline refCount (value : 'T) (set : CountingHashSet<'T>) = set.GetRefCount value
    let inline contains (value : 'T) (set : CountingHashSet<'T>) = refCount value set > 0

    let inline toSeq (map : CountingHashSet<'T>) = map :> seq<_>
    let inline toList (map : CountingHashSet<'T>) = map.ToList()
    let inline toArray (map : CountingHashSet<'T>) = map.ToArray()
    let inline toHashSet (map : CountingHashSet<'T>) = map.ToHashSet()

    let inline union (l : CountingHashSet<'T>) (r : CountingHashSet<'T>) = l.UnionWith r
    let inline computeDelta (l : CountingHashSet<'T>) (r : CountingHashSet<'T>) = l.ComputeDeltaTo r
    let inline applyDelta (l : CountingHashSet<'T>) (r : HashSetDelta<'T>) = l.ApplyDelta r

    let inline toRefCountList (map : CountingHashSet<'T>) = map.ToRefCountList()
    let inline toRefCountArray (map : CountingHashSet<'T>) = map.ToRefCountArray()

[<AutoOpen>]
module HAMT =
    open System.Collections.Generic

    [<AutoOpen>]
    module internal Implementation = 
        [<AllowNullLiteral>]
        type Linked<'K, 'V> =
            val mutable public Key : 'K
            val mutable public Value : 'V
            val mutable public Next : Linked<'K, 'V>

            new(k, v, n) = { Key = k; Value = v; Next = n }

        type NodeKind =
            | Leaf = 0
            | Inner = 1
        
        [<AllowNullLiteral; AbstractClass>]
        type Node<'K, 'V> =
            abstract member Kind : NodeKind
            //val mutable public Kind : NodeKind
            new(k : NodeKind) = { }

        [<Sealed; AllowNullLiteral>]
        type Leaf<'K, 'V> =
            inherit Node<'K, 'V>
            val mutable public Next : Linked<'K, 'V>
            val mutable public Key : 'K
            val mutable public Value : 'V
            val mutable public Hash : uint32

            override x.Kind = NodeKind.Leaf

            new(hash : uint32, key : 'K, value : 'V, next : Linked<'K, 'V>) =
                { inherit Node<'K, 'V>(NodeKind.Leaf); Hash = hash; Key = key; Value = value; Next = next }

        [<Sealed; AllowNullLiteral>]
        type Inner<'K, 'V> =
            inherit Node<'K, 'V>
            val mutable public Left : Node<'K, 'V>
            val mutable public Right : Node<'K, 'V>
            val mutable public Prefix : uint32
            val mutable public Mask : uint32
            override x.Kind = NodeKind.Inner

            member inline x.WithChildren(l : Node<'K, 'V>, r : Node<'K, 'V>) =
                Inner(x.Prefix, x.Mask, l, r) :> Node<_,_>

            new(prefix : uint32, mask : uint32, left : Node<'K, 'V>, right : Node<'K, 'V>) =
                { inherit Node<'K, 'V>(NodeKind.Inner); Prefix = prefix; Mask = mask; Left = left; Right = right}

        module Linked =
            let rec toList (prepend : OptimizedClosures.FSharpFunc<'K, 'V, list<'T>, list<'T>>) (acc : list<'T>) (node : Linked<'K, 'V>) =
                if isNull node then 
                    acc
                else
                    prepend.Invoke(node.Key, node.Value, toList prepend acc node.Next)

            let rec add (cmp : IEqualityComparer<'K>) (count : byref<int>) (key : 'K) (value : 'V) (node : Linked<'K, 'V>) =
                if isNull node then
                    count <- count + 1
                    Linked(key, value, null)
                elif cmp.Equals(key, node.Key) then
                    Linked(key, value, node.Next)
                else
                    Linked(node.Key, node.Value, add cmp &count key value node.Next)
                
            let rec change (cmp : IEqualityComparer<'K>) (count : byref<int>) (key : 'K) (update : voption<'V> -> voption<'V>) (node : Linked<'K, 'V>) =
                if isNull node then
                    match update ValueNone with
                    | ValueNone -> null
                    | ValueSome value -> 
                        count <- count + 1
                        Linked(key, value, null)
                elif cmp.Equals(key, node.Key) then
                    match update (ValueSome node.Value) with
                    | ValueSome value -> Linked(key, value, node.Next)
                    | ValueNone -> 
                        count <- count - 1
                        node.Next
                else
                    Linked(node.Key, node.Value, change cmp &count key update node.Next)

        let rec toList (prepend : OptimizedClosures.FSharpFunc<'K, 'V, list<'T>, list<'T>>) (acc : list<'T>) (node : Node<'K, 'V>) =
            if isNull node then
                acc
            elif node.Kind = NodeKind.Leaf then
                let node : Leaf<'K, 'V> = downcast node
                let acc = Linked.toList prepend acc node.Next
                prepend.Invoke(node.Key, node.Value, acc)
            else
                let node = node :?> Inner<'K, 'V>
                toList prepend (toList prepend acc node.Right) node.Left

        let join (lh : uint32) (l : Node<'K, 'V>) (rh : uint32) (r : Node<'K, 'V>) =
            if isNull l then r
            elif isNull r then l
            else
                let mask = getMask lh rh
                let prefix = getPrefix lh mask
                Inner(prefix, mask, l, r) :> Node<_,_>

        let rec add (cmp : IEqualityComparer<'K>) (count : byref<int>) (hash : uint32) (key : 'K) (value : 'V) (node : Node<'K, 'V>) =
            if isNull node then
                count <- count + 1
                Leaf(hash, key, value, null) :> Node<_,_>

            elif node.Kind = NodeKind.Inner then
                let node : Inner<'K, 'V> = downcast node
                match matchPrefixAndGetBit hash node.Prefix node.Mask with
                | 0u -> node.WithChildren(add cmp &count hash key value node.Left, node.Right)
                | 1u -> node.WithChildren(node.Left, add cmp &count hash key value node.Right)
                | _ -> 
                    count <- count + 1
                    join node.Prefix node hash (Leaf(hash, key, value, null))

            else
                let node : Leaf<'K, 'V> = downcast node
                if node.Hash = hash then
                    if cmp.Equals(node.Key, key) then   
                        Leaf(hash, key, value, node.Next) :> Node<_,_>
                    else
                        Leaf(hash, node.Key, node.Value, Linked.add cmp &count key value node.Next) :> Node<_,_>
                else
                    count <- count + 1
                    join node.Hash node hash (Leaf(hash, key, value, null))

        let rec change (cmp : IEqualityComparer<'K>) (count : byref<int>) (hash : uint32) (key : 'K) (update : voption<'V> -> voption<'V>) (node : Node<'K, 'V>) =
            if isNull node then
                match update ValueNone with
                | ValueSome value -> 
                    count <- count + 1
                    Leaf(hash, key, value, null) :> Node<_,_>
                | ValueNone ->
                    null

            elif node.Kind = NodeKind.Leaf then
                let node = node :?> Leaf<'K, 'V>
                if node.Hash = hash then
                    if cmp.Equals(node.Key, key) then   
                        match update (ValueSome node.Value) with
                        | ValueSome value ->
                            Leaf(hash, key, value, node.Next) :> Node<_,_>
                        | ValueNone ->
                            count <- count - 1
                            let n = node.Next
                            if isNull n then null
                            else Leaf(hash, n.Key, n.Value, n.Next) :> Node<_,_>
                    else
                        Leaf(hash, node.Key, node.Value, Linked.change cmp &count key update node.Next) :> Node<_,_>
                else
                    match update ValueNone with
                    | ValueSome value -> 
                        count <- count + 1
                        join node.Hash node hash (Leaf(hash, key, value, null))
                    | ValueNone ->
                        node :> Node<_,_>

            else    
                let node = node :?> Inner<'K, 'V>
                match matchPrefixAndGetBit hash node.Prefix node.Mask with
                | 0u -> node.WithChildren(change cmp &count hash key update node.Left, node.Right)
                | 1u -> node.WithChildren(node.Left, change cmp &count hash key update node.Right)
                | _ -> 
                    match update ValueNone with
                    | ValueSome value ->
                        count <- count + 1
                        join node.Prefix node hash (Leaf(hash, key, value, null))
                    | ValueNone ->
                        node :> Node<_,_>


        let rec size (node : Node<'K, 'V>) =
            if isNull node then 
                0
            elif node.Kind = NodeKind.Leaf then
                let node = node :?> Leaf<'K, 'V>
                let mutable cnt = 1
                let mutable next = node.Next
                while not (isNull next) do
                    next <- next.Next
                    cnt <- cnt + 1
                cnt
            else
                let node = node :?> Inner<'K, 'V>
                size node.Left + size node.Right

    [<Sealed>]
    type HAMT<'K, 'V> internal(comparer : IEqualityComparer<'K>, root : Node<'K, 'V>, count : int) =
        
        static let prependKey =
            OptimizedClosures.FSharpFunc<'K, 'V, list<'K>, list<'K>>.Adapt (fun k v t -> k :: t)
            
        static let prependValue =
            OptimizedClosures.FSharpFunc<'K, 'V, list<'V>, list<'V>>.Adapt (fun k v t -> v :: t)
            
        static let prependTup =
            OptimizedClosures.FSharpFunc<'K, 'V, list<'K * 'V>, list<'K * 'V>>.Adapt (fun k v t -> (k,v) :: t)
            
        static let prependVTup =
            OptimizedClosures.FSharpFunc<'K, 'V, list<struct('K * 'V)>, list<struct('K * 'V)>>.Adapt (fun k v t -> struct(k,v) :: t)

        static let empty = HAMT<'K, 'V>(DefaultEqualityComparer<'K>.Instance, null, 0)

        member internal x.Root = root
        
        static member Empty = empty
        
        member x.Count = count
        
        member x.ComputeCount() = size root
        
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member x.Add(key, value) = 
            let hash = uint32 (comparer.GetHashCode key)
            let mutable count = count
            HAMT<'K, 'V>(comparer, add comparer &count hash key value root, count)
            
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member x.ChangeV(key, update) = 
            let hash = uint32 (abs (comparer.GetHashCode key))
            let mutable count = count
            HAMT<'K, 'V>(comparer, change comparer &count hash key update root, count)

        member x.ToList() = toList prependTup [] root
        member x.ToKeyList() = toList prependKey [] root
        member x.ToValueList() = toList prependValue [] root
        member x.ToListV() = toList prependVTup [] root

    module HAMT =
        [<GeneralizableValue>]
        let empty<'K, 'V> = HAMT<'K, 'V>.Empty

        let inline add k v (m : HAMT<'K, 'V>) = m.Add(k, v)
        let inline changeV k v (m : HAMT<'K, 'V>) = m.ChangeV(k, v)

        let inline toList (m : HAMT<'K, 'V>) = m.ToList()

open BenchmarkDotNet.Attributes
[<PlainExporter; MemoryDiagnoser>]
type HAMTBench() =
    let rand = System.Random()

    let randomArray(n : int) =
        let res = Array.zeroCreate n
        let m = System.Collections.Generic.HashSet<int>()
        let mutable o = 0
        while m.Count < n do
            let v = rand.Next()
            if m.Add v then
                res.[o] <- (v, o)
                o <- o + 1

        res
               
    let n = 1024

    let all = randomArray (2 * n)
    let data = Array.take n all
    let test = Array.skip n all

    let hashMap = HashMap.ofArray data

    let hamt = 
        let mutable r = HAMT.empty
        for (k, v) in data do r <- HAMT.add k v r
        r
        
    [<Benchmark>]
    member x.HAMT() =
        let mutable r = hamt
        for (k, v) in test do
            r <- r |> HAMT.add k v
        r

        
    [<Benchmark>]
    member x.HashMap() =
        let mutable r = hashMap
        for (k, v) in test do
            r <- r |> HashMap.add k v
        r


[<EntryPoint>]
let main _args =
  
    //Profile.run()
    //BenchmarkRunner.Run<Benchmarks.TransactBenchmark>() |> ignore
    BenchmarkRunner.Run<HAMTBench>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.MapBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.CollectBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.EnumeratorBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.IndexListEnumeratorBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.HashSetEnumeratorBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.HashSetBenchmarks>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.HashMapEnumeratorBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.HashMapStructEnumeratorBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.CountingHashSetEnumeratorBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.IndexListDeltaEnumeratorBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.HashSetDeltaEnumeratorBenchmark>() |> ignore
    //BenchmarkRunner.Run<Benchmarks.HashMapDeltaEnumeratorBenchmark>() |> ignore

    0
