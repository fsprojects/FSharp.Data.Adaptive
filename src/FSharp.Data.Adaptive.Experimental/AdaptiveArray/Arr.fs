namespace FSharp.Data.Adaptive

namespace FSharp.Data.Adaptive

#nowarn "7331"

open System.Diagnostics
open FSharp.Data.Adaptive.ComputeListDeltaHelpers

module internal ArrNodeImplementation =

    [<AllowNullLiteral>]
    type Node<'a> =
        val mutable public Height : byte
        val mutable public Value : 'a

        new(h, v) = { Height = h; Value = v }

    type Inner<'a> =
        inherit Node<'a>
        val mutable public Left : Node<'a>
        val mutable public Right : Node<'a>
        val mutable public Count : int

        static member inline GetCount(node : Node<'a>) =
            if isNull node then 0
            elif node.Height = 1uy then 1
            else (node :?> Inner<'a>).Count

        static member inline GetHeight(node : Node<'a>) =
            if isNull node then 0uy
            else node.Height

        static member inline FixHeightAndCount(inner : Inner<'a>) =
            let lc = Inner.GetCount inner.Left
            let rc = Inner.GetCount inner.Right
            let lh = if lc > 0 then inner.Left.Height else 0uy
            let rh = if rc > 0 then inner.Right.Height else 0uy
            inner.Count <- 1 + lc + rc
            inner.Height <- 1uy + max lh rh

        static member New(l : Node<'a>, value : 'a, r : Node<'a>) =
            if isNull l && isNull r then Node(1uy, value)
            else 
                let h = 1uy + max (Inner.GetHeight l) (Inner.GetHeight r)
                let c = 1 + Inner.GetCount l + Inner.GetCount r
                Inner(h, value, l, r, c) :> Node<_>

        new(h, v, l, r, c) = { inherit Node<'a>(h, v); Left = l; Right = r; Count = c }

    module Node =

        let inline empty<'a> : Node<'a> = null

        let inline height (n : Node<'a>) =
            if isNull n then 0uy
            else n.Height

        let inline isEmpty (n : Node<'a>) =
            if isNull n then true
            else false

        let inline count (n : Node<'a>) =
            if isNull n then 0
            elif n.Height = 1uy then 1
            else (n :?> Inner<'a>).Count


        let rec copyTo (index : int) (dst : 'a[]) (n : Node<'a>) =
            if not (isNull n) then
                if n.Height = 1uy then dst.[index] <- n.Value
                else
                    let n = n :?> Inner<'a>
                    let lc = count n.Left
                    copyTo index dst n.Left
                    dst.[index + lc] <- n.Value
                    copyTo (index + lc + 1) dst n.Right

        let rec toList (acc : list<'a>) (node : Node<'a>) =
            if isNull node then
                acc
            elif node.Height = 1uy then
                node.Value :: acc
            else
                let n = node :?> Inner<'a>
                toList (n.Value :: toList acc n.Right) n.Left

        let toArray (n : Node<'a>) =
            let c = count n
            let a = Array.zeroCreate c
            copyTo 0 a n
            a

        let inline balance (n : Inner<'a>) =
            int (height n.Right) - int (height n.Left)

        let rec ofSpan (span : System.ReadOnlySpan<'a>)  =
            if span.Length = 0 then 
                null
            elif span.Length = 1 then 
                Node(1uy, span.[0])
            else
                let m = span.Length / 2
                let lt = ofSpan (span.Slice(0, m))
                let rt = ofSpan (span.Slice(m+1))
                Inner(1uy + max (height lt) (height rt), span.[m], lt, rt, 1 + count lt + count rt) :> Node<_>

        let rec binary (l : Node<'a>) (v : 'a) (r : Node<'a>) =
            let lc = count l
            let rc = count r
            let lh = if lc > 0 then l.Height else 0uy
            let rh = if rc > 0 then r.Height else 0uy

            let b = int rh - int lh
            if b > 2 then
                // rh > lh + 2
                let r = r :?> Inner<'a>
                let rb = balance r
                if rb > 0 then
                    // right right 
                    binary 
                        (binary l v r.Left)
                        r.Value
                        r.Right
                else
                    // right left
                    let rl = r.Left :?> Inner<'a>
                    binary
                        (binary l v rl.Left)
                        rl.Value
                        (binary rl.Right r.Value r.Right)

            elif b < -2 then
                // lh > rh + 2
                let l = l :?> Inner<'a>
                let lb = balance l
                if lb < 0 then
                    // left left
                    binary 
                        l.Left
                        l.Value
                        (binary l.Right v r)
                else
                    // left right
                    let lr = l.Right :?> Inner<'a>
                    binary 
                        (binary l.Left l.Value lr.Left)
                        lr.Value
                        (binary lr.Right v r)

            elif lh = 0uy && rh = 0uy then Node(1uy, v)
            else Inner(1uy + max lh rh, v, l, r, 1 + lc + rc) :> Node<_>
        
        let inline unsafeInner l v r = Inner.New(l, v, r)

        let unsafeBinary (l : Node<'a>) (v : 'a) (r : Node<'a>) =
            let lc = count l
            let rc = count r
            let lh = if lc > 0 then l.Height else 0uy
            let rh = if rc > 0 then r.Height else 0uy

            let b = int rh - int lh
            if b > 2 then
                // rh > lh + 2
                let r = r :?> Inner<'a>
                let rb = balance r
                if rb > 0 then
                    // right right 
                    unsafeInner 
                        (unsafeInner l v r.Left)
                        r.Value
                        r.Right
                else
                    // right left
                    let rl = r.Left :?> Inner<'a>
                    unsafeInner
                        (unsafeInner l v rl.Left)
                        rl.Value
                        (unsafeInner rl.Right r.Value r.Right)

            elif b < -2 then
                // lh > rh + 2
                let l = l :?> Inner<'a>
                let lb = balance l
                if lb < 0 then
                    // left left
                    unsafeInner 
                        l.Left
                        l.Value
                        (unsafeInner l.Right v r)
                else
                    // left right
                    let lr = l.Right :?> Inner<'a>
                    unsafeInner 
                        (unsafeInner l.Left l.Value lr.Left)
                        lr.Value
                        (unsafeInner lr.Right v r)

            elif lh = 0uy && rh = 0uy then Node(1uy, v)
            else Inner(1uy + max lh rh, v, l, r, 1 + lc + rc) :> Node<_>


        let rec unsafeRemoveMin (n : Node<'a>) =
            if n.Height = 1uy then
                struct(n.Value, Unchecked.defaultof<_>)
            else
                let n = n :?> Inner<'a>
                if isNull n.Left then
                    struct(n.Value, n.Right)
                else
                    let struct(value, newLeft) = unsafeRemoveMin n.Left
                    let node = unsafeBinary newLeft n.Value n.Right
                    struct(value, node)

        let rec unsafeRemoveMax (n : Node<'a>) =
            if n.Height = 1uy then
                struct(n.Value, null)
            else
                let n = n :?> Inner<'a>
                if isNull n.Right then
                    struct(n.Value, n.Left)
                else
                    let struct(value, newRight) = unsafeRemoveMax n.Right
                    let node = unsafeBinary n.Left n.Value newRight
                    struct(value, node)


        let rec join (l : Node<'a>) (r : Node<'a>) =
            if isNull l then r
            elif isNull r then l
            else
                let lh = l.Height
                let rh = r.Height
                if lh > rh then
                    let struct(v, ln) = unsafeRemoveMax l
                    binary ln v r
                else
                    let struct(v, rn) = unsafeRemoveMin r
                    binary l v rn

        let rec insertAt (index : int) (value : 'a) (n : Node<'a>) =
            if isNull n then
                Node(1uy, value)
            elif n.Height = 1uy then
                if index = 0 then binary null value n
                else binary n value null
            else
                let n = n :?> Inner<'a>
                let c = count n.Left
                if index <= c then
                    binary (insertAt index value n.Left) n.Value n.Right
                else
                    binary n.Left n.Value (insertAt (index - c - 1) value n.Right)

        let rec append (value : 'a) (n : Node<'a>) =
            if isNull n then
                Node(1uy, value)
            elif n.Height = 1uy then
                binary n value null
            else
                let n = n :?> Inner<'a>
                binary n.Left n.Value (append value n.Right)

        let rec prepend (value : 'a) (n : Node<'a>) =
            if isNull n then
                Node(1uy, value)
            elif n.Height = 1uy then
                binary null value n
            else
                let n = n :?> Inner<'a>
                binary (prepend value n.Left) n.Value n.Right

        let rec skip (n : int) (node : Node<'a>) =
            if n <= 0 then node 
            elif isNull node || node.Height = 1uy then
                null
            else
                let node = node :?> Inner<'a>
                if n >= node.Count then
                    null
                else
                    let lc = count node.Left
                    if n < lc then binary (skip n node.Left) node.Value node.Right
                    elif n = lc then prepend node.Value node.Right
                    else skip (n - lc - 1) node.Right

        let rec take (n : int) (node : Node<'a>) =
            if n <= 0 then null
            elif isNull node || node.Height = 1uy then
                node
            else
                let node = node :?> Inner<'a>
                if n >= node.Count then
                    node
                else
                    let lc = count node.Left
                    if n <= lc then take n node.Left
                    elif n = lc then append node.Value node.Left
                    else binary node.Left node.Value (take (n - lc - 1) node.Right)

        let rec sub (l : int) (r : int) (node : Node<'a>) =
        
            if isNull node then
                null
            elif node.Height = 1uy then
                if l <= 0 && r >= 0 then node
                else null
            else
                let node = node :?> Inner<'a>
                let lc = count node.Left
                if r < lc then 
                    sub l r node.Left
                elif l > lc then 
                    sub (l - lc - 1) (r - lc - 1) node.Right
                elif l = lc then
                    prepend node.Value (take (r - l) node.Right)
                elif r = lc then
                    append node.Value (skip l node.Left)
                else
                    binary (skip l node.Left) node.Value (take (r - lc) node.Right)

        let join3 (a : Node<'a>) (b : Node<'a>) (c : Node<'a>) =
            join a (join b c) // TODO!!!

        let rec removeRange (l : int) (r : int) (node : Node<'a>) = 
            if isNull node then
                null
            elif node.Height = 1uy then
                if l <= 0 && r >= 0 then null
                else node
            else
                let node = node :?> Inner<'a>
                let lc = count node.Left
                if r < lc then 
                    binary (removeRange l r node.Left) node.Value node.Right
                elif l > lc then 
                    binary node.Left node.Value (removeRange (l - lc - 1) (r - lc - 1) node.Right)
                elif l = lc then
                    join node.Left (skip (r - l) node.Right)
                elif r = lc then
                    join (take l node.Left) node.Right
                else
                    let a = take l node.Left 
                    let b = skip (r - lc) node.Right
                    join a b

        let rec insertRange (index : int) (repl : Node<'a>) (node : Node<'a>) =
            if isNull node then
                repl
            elif node.Height = 1uy then
                if index <= 0 then append node.Value repl
                else prepend node.Value repl
            else
                let node = node :?> Inner<'a>
                let lc = count node.Left
                if index < lc then
                    binary (insertRange index repl node.Left) node.Value node.Right
                elif index = lc then
                    let lh = height node.Left
                    let rh = height node.Right
                    if lh < rh then
                        let l = append node.Value node.Left
                        join (join l repl) node.Right
                    else
                        let r = prepend node.Value node.Right
                        join node.Left (join repl r)
                else
                    binary node.Left node.Value (insertRange (index - lc - 1) repl node.Right)

        let rec replaceRange (l : int) (r : int) (repl : Node<'a>) (node : Node<'a>) =
            if isNull node then
                repl
            elif node.Height = 1uy then
                if l <= 0 && r >= 0 then repl
                elif r < 0 then append node.Value repl
                else prepend node.Value repl
            else
                let node = node :?> Inner<'a>
                let lc = count node.Left
                if r < lc then 
                    binary (replaceRange l r repl node.Left) node.Value node.Right
                elif l > lc then 
                    binary node.Left node.Value (replaceRange (l - lc - 1) (r - lc - 1) repl node.Right)
                elif l = lc then
                    join3 node.Left repl (skip (r - l) node.Right)
                elif r = lc then
                    join3 (take l node.Left) repl node.Right
                else
                    let a = take l node.Left 
                    let b = skip (r - lc) node.Right
                    join3 a repl b

        
open ArrNodeImplementation



[<Struct; StructuredFormatDisplay("{AsString}"); DebuggerTypeProxy(typedefof<ArrProxy<_>>)>]
type arr<'a> internal(store : Node<'a>) =
    static member Empty : arr<'a> = arr<'a> null

    static member FromArray(elements : 'a[]) = arr (Node.ofSpan (System.ReadOnlySpan elements))
    static member FromSpan(elements : System.ReadOnlySpan<'a>) = arr (Node.ofSpan elements)
    static member FromSpan(elements : System.Span<'a>) = arr (Node.ofSpan (System.Span.op_Implicit elements))
    static member FromMemory(elements : System.Memory<'a>) = arr (Node.ofSpan (System.Span.op_Implicit elements.Span))
    static member FromMemory(elements : System.ReadOnlyMemory<'a>) = arr (Node.ofSpan elements.Span)

    member internal x.Store = store
    member x.IsEmpty = Node.isEmpty store
    member x.Length = Node.count store
    member x.InsertAt(index : int, value : 'a) = arr(Node.insertAt index value store)
    member x.Add(value : 'a) = arr(Node.insertAt x.Length value store)
    
    member x.Skip(n : int) =
        if n <= 0 then x
        elif n >= x.Length then arr.Empty
        else arr(Node.skip n store)
        
    member x.Take(n : int) =
        if n <= 0 then arr.Empty
        elif n < x.Length then arr(Node.take n store)
        else x
        
    member x.Sub(offset : int, count : int) =
        arr(Node.sub offset (offset + count - 1) store)

    member x.RemoveRange(offset : int, count : int) =
        arr(Node.removeRange offset (offset + count - 1) store)

    member x.ReplaceRange(offset : int, count : int, replacement : arr<'a>) =
        if count <= 0 then arr(Node.insertRange offset replacement.Store store)
        else arr(Node.replaceRange offset (offset + count - 1) replacement.Store store)

    member x.ToArray() =
        let res = Array.zeroCreate x.Length
        Node.copyTo 0 res store
        res

    member x.ToList() =
        Node.toList [] store

    override x.ToString() =
        if Node.count store > 10 then Node.toArray (Node.take 10 store) |> Seq.map (sprintf "%A") |> String.concat "; " |> sprintf "arr [%s]"
        else Node.toArray store |> Seq.map (sprintf "%A") |> String.concat "; " |> sprintf "arr [%s]"

    member private x.AsString = x.ToString()
    member x.GetEnumerator() = new ArrEnumerator<'a>(store)

    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = new ArrEnumerator<'a>(store) :> _

    interface System.Collections.Generic.IEnumerable<'a> with
        member x.GetEnumerator() = new ArrEnumerator<'a>(store) :> _

and [<Struct>] ArrEnumerator<'a> =
    struct
        val mutable internal Root : Node<'a>
        val mutable internal Head : struct(Node<'a> * bool)
        val mutable internal Tail : list<struct(Node<'a> * bool)>
        val mutable internal CurrentNode : Node<'a>
        
        member x.MoveNext() =
            let struct(n, deep) = x.Head
            if not (isNull n) then

                if n.Height > 1uy && deep then
                    let inner = n :?> Inner<'a>

                    if isNull inner.Left then
                        if isNull inner.Right then
                            if x.Tail.IsEmpty then
                                x.Head <- Unchecked.defaultof<_>
                                x.Tail <- []
                            else
                                x.Head <- x.Tail.Head
                                x.Tail <- x.Tail.Tail
                        else
                            x.Head <- struct(inner.Right, true)

                        x.CurrentNode <- n
                        true
                    else
                        x.Head <- struct(inner.Left, true)
                        if isNull inner.Right then
                            x.Tail <- struct(n, false) :: x.Tail
                        else
                            x.Tail <- struct(n, false) :: struct(inner.Right, true) :: x.Tail
                        x.MoveNext()
                else
                    x.CurrentNode <- n
                    if x.Tail.IsEmpty then 
                        x.Head <- Unchecked.defaultof<_>
                        x.Tail <- []
                    else
                        x.Head <- x.Tail.Head
                        x.Tail <- x.Tail.Tail
                    true

            else
                false


        member x.Reset() =
            x.Head <- if isNull x.Root then Unchecked.defaultof<_> else struct(x.Root, true)                
            x.Tail <- []
            x.CurrentNode <- null

        member x.Dispose() =
            x.Root <- null
            x.CurrentNode <- null
            x.Head <- Unchecked.defaultof<_>
            x.Tail <- []

        member x.Current =
            x.CurrentNode.Value

        interface System.Collections.IEnumerator with
            member x.MoveNext() = x.MoveNext()
            member x.Reset() = x.Reset()
            member x.Current = x.Current :> obj

        interface System.Collections.Generic.IEnumerator<'a> with
            member x.Current = x.Current
            member x.Dispose() = x.Dispose()

        internal new(root : Node<'a>) =
            {
                Root = root
                Head = if isNull root then Unchecked.defaultof<_> else struct(root, true)
                Tail = []
                CurrentNode = null
            }
    end  


and internal ArrProxy<'a>(arr : arr<'a>) =
    let items = arr.Take(10000).ToArray()

    member x.Length = arr.Length
    
    [<DebuggerBrowsable(DebuggerBrowsableState.RootHidden)>]
    member x.Items = items


[<Struct; StructuredFormatDisplay("{AsString}")>]
type ArrOperation<'a> = { Index : int; Count : int; Elements : arr<'a>} with
    member inline x.MinIndex = x.Index
    member inline x.MaxIndex = x.Index + x.Count - 1
    member inline x.Balance = x.Elements.Length - x.Count
    member inline x.IsEmpty = x.Count <= 0 && x.Elements.Length <= 0

    member private x.AsString = x.ToString()
    
    override x.ToString() =
        if x.Elements.IsEmpty && x.Count <= 0 then
            "nop"
        elif x.Elements.IsEmpty then
            if x.Count = 1 then
                sprintf "del(%d)" x.Index
            else
                sprintf "del(%d, %d)" x.Index x.Count
        elif x.Count = 0 then
            sprintf "ins(%d, %A)" x.Index x.Elements
        else
            sprintf "splice(%d, %d, %A)" x.Index x.Count x.Elements
            
[<Struct; DebuggerTypeProxy(typedefof<ArrDeltaProxy<_>>); StructuredFormatDisplay("{AsString}")>]
type arrdelta<'a> internal(store : Node<ArrOperation<'a>>) =
    member internal x.Store = store
    member x.Length = Node.count store

    new(delta : arr<ArrOperation<'a>>) = arrdelta(delta.Store)

    override x.ToString() =
        if Node.count store > 20 then
            Node.toArray store |> Seq.map (sprintf "%A") |> String.concat "; " |> sprintf "arrdelta [%s; ...]"
        else
            Node.toArray store |> Seq.map (sprintf "%A") |> String.concat "; " |> sprintf "arrdelta [%s]"
    
    member x.ToArr() = arr(store)
    
    member x.GetEnumerator() = new ArrEnumerator<ArrOperation<'a>>(store)

    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = new ArrEnumerator<ArrOperation<'a>>(store) :> _

    interface System.Collections.Generic.IEnumerable<ArrOperation<'a>> with
        member x.GetEnumerator() = new ArrEnumerator<ArrOperation<'a>>(store) :> _

and internal ArrDeltaProxy<'a>(delta : arrdelta<'a>) =
    let items = delta.ToArr().Take(10000).ToArray()
    [<DebuggerBrowsable(DebuggerBrowsableState.RootHidden)>]
    member x.Items = items


module Arr =
    let inline empty<'a> : arr<'a> = arr.Empty

    let inline insertAt (index : int) (value : 'a) (arr : arr<'a>) = arr.InsertAt(index, value)
    let inline add (value : 'a) (arr : arr<'a>) = arr.Add value

    let inline ofSeq (seq : seq<'a>) = arr.FromArray (Seq.toArray seq)
    let inline ofList (array : list<'a>) = arr.FromArray (List.toArray array)
    let inline ofArray (array : 'a[]) = arr.FromArray array
    let inline ofSpan (array : System.Span<'a>) = arr.FromSpan array
    let inline ofMemory (array : System.Memory<'a>) = arr.FromMemory array

    let inline toSeq (arr : arr<'a>) = arr :> seq<_>
    let inline toList (arr : arr<'a>) = arr.ToList()
    let inline toArray (arr : arr<'a>) = arr.ToArray()

    let computeDelta (equal : 'a -> 'a -> bool) (src : arr<'a>) (dst : arr<'a>) : arrdelta<'a> =
        let srcArr = toArray src
        let dstArr = toArray dst

        let mutable steps = DeltaOperationList.ofArrayMyers equal srcArr dstArr

        let mutable si = 0
        let mutable di = 0
        let mutable balance = 0
        let mutable delta = empty<ArrOperation<'a>>

        while not steps.IsNil do
            let struct(h, t) = steps.UnsafeUnconsV()

            match h with
            | DeltaOperation.Equal ->
                // step both => no delta
                steps <- t
                si <- si + 1
                di <- di + 1
            | _ ->
            
                let mutable remCnt = 0
                let mutable addCnt = 0
                let mutable struct(h, t) = steps.UnsafeUnconsV()
                steps <- t

                while h <> DeltaOperation.Equal do
                    if h = DeltaOperation.Remove then remCnt <- remCnt + 1
                    else addCnt <- addCnt + 1

                    if steps.IsNil then 
                        h <- DeltaOperation.Equal
                    else 
                        let struct(hh, tt) = steps.UnsafeUnconsV()
                        if hh <> DeltaOperation.Equal then 
                            h <- hh
                            steps <- tt
                        else
                            h <- DeltaOperation.Equal

            
                let op = { Index = si + balance; Count = remCnt; Elements = dst.Sub(di, addCnt) }
                si <- si + remCnt
                di <- di + addCnt
                delta <- delta.Add(op)
                balance <- balance + addCnt - remCnt

        arrdelta delta

    let applyDelta (state : arr<'a>) (delta : arrdelta<'a>) =
        let mutable res = state
        for op in delta do
            res <- res.ReplaceRange(op.Index, op.Count, op.Elements)
        res


module ArrDelta =

    let empty<'a> : arrdelta<'a> = arrdelta null

    let toSeq (a : arrdelta<'a>) = a :> seq<_>
    let toList (a : arrdelta<'a>) = Node.toList [] a.Store
    let toArray (a : arrdelta<'a>) = Node.toArray a.Store

    let combine (a : arrdelta<'a>) (b : arrdelta<'a>) : arrdelta<'a> =
        use mutable ea = a.GetEnumerator()
        use mutable eb = b.GetEnumerator()

        let mutable va = ea.MoveNext()
        let mutable vb = eb.MoveNext()

        let mutable abalance = 0

        let mutable pending : option<ArrOperation<'a>> = None
        let mutable final = Arr.empty

        let inline flush() =
            match pending with
            | Some last ->
                if last.Count > 0 || last.Elements.Length > 0 then final <- Arr.add last final
                pending <- None
            | None ->
                ()

        let inline append c =
            match pending with
            | Some last ->
                if last.Index = c.Index  then
                    let n = { Index = last.Index; Count = last.Count + (c.Count - last.Elements.Length); Elements = arr (Node.join c.Elements.Store (Node.skip c.Count last.Elements.Store)) }
                    pending <- Some n
                
                else
                    let l = last.Index 
                    let h = last.Index + last.Elements.Length - 1
                    if c.Index >= l && c.Index <= h && c.Count >= 0 then
                        let li = c.Index - l
                        let remLocal = last.Elements.Length - li
                        let remRest = c.Count - remLocal

                        let newLastElements = last.Elements.ReplaceRange(li, remLocal, c.Elements)
                        pending <- Some { Index = last.Index; Count = last.Count + remRest; Elements = newLastElements }
                        
                    else
                        flush()
                        pending <- Some c
            | None ->
                pending <- Some c

        while va && vb do
            let a0 = { ea.Current with Index = ea.Current.Index + abalance }
            let b0 = eb.Current

            if a0.Index <= b0.Index then
                append a0
                va <- ea.MoveNext()
            else
                append b0
                abalance <- abalance + b0.Balance
                vb <- eb.MoveNext()

        while va do
            let a0 = { ea.Current with Index = ea.Current.Index + abalance }
            append a0
            va <- ea.MoveNext()

        while vb do
            let b0 = eb.Current
            append b0
            vb <- eb.MoveNext()

        flush()

        arrdelta final

