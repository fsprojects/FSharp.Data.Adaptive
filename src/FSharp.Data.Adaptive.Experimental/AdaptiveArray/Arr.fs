namespace FSharp.Data.Adaptive

namespace FSharp.Data.Adaptive

#nowarn "7331"

open System
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

        let rec insert (index : int) (value : 'a) (n : Node<'a>) =
            if isNull n then
                Node(1uy, value)
            elif n.Height = 1uy then
                if index = 0 then binary null value n
                else binary n value null
            else
                let n = n :?> Inner<'a>
                let c = count n.Left
                if index <= c then
                    binary (insert index value n.Left) n.Value n.Right
                else
                    binary n.Left n.Value (insert (index - c - 1) value n.Right)

        let rec unsafeSet (index : int) (value : 'a) (n : Node<'a>) =
            if isNull n then
                null
            elif n.Height = 1uy then
                if index = 0 then Node(1uy, value)
                else n
            else
                let n = n :?> Inner<'a>
                let c = count n.Left
                if index < c then
                    binary (unsafeSet index value n.Left) n.Value n.Right
                elif index > c then
                    binary n.Left n.Value (unsafeSet (index - c - 1) value n.Right)
                else
                    binary n.Left value n.Right

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
                    if n < lc then take n node.Left
                    elif n = lc then node.Left
                    else binary node.Left node.Value (take (n - lc - 1) node.Right)

        
        let rec split (n : int) (node : Node<'a>) =
            if n <= 0 then
                null, node
            elif isNull node || node.Height = 1uy then
                node, null
            else
                let node = node :?> Inner<'a>
                if n >= node.Count then
                    node, null
                else
                    let lc = count node.Left
                    if n < lc then
                        let ll, lr = split n node.Left
                        ll, binary lr node.Value node.Right
                    elif n = lc then
                        node.Left, prepend node.Value node.Right
                    else
                        let rl, rr = split (n - lc - 1) node.Right
                        binary node.Left node.Value rl, rr
        
        
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
            if isNull a then join b c
            elif isNull b then join a c
            elif isNull c then join a b
            elif a.Height = 1uy then
                join (prepend a.Value b) c
            elif b.Height = 1uy then
                if a.Height < c.Height then join (append b.Value a) c
                else join a (prepend b.Value c)
            elif c.Height = 1uy then
                join a (append c.Value b)
            else
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
                        join3 l repl node.Right
                    else
                        let r = prepend node.Value node.Right
                        join3 node.Left repl r
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

        let rec tryAt (i : int) (n : Node<'a>) =
            if isNull n then None
            elif n.Height = 1uy then
                if i = 0 then Some n.Value
                else None
            else
                let n = n :?> Inner<'a>
                let lc = count n.Left
                if i < lc then
                    tryAt i n.Left
                elif i > lc then
                    tryAt (i - lc - 1) n.Right
                else
                    Some n.Value
                    
        let rec tryAtV (i : int) (n : Node<'a>) =
            if isNull n then ValueNone
            elif n.Height = 1uy then
                if i = 0 then ValueSome n.Value
                else ValueNone
            else
                let n = n :?> Inner<'a>
                let lc = count n.Left
                if i < lc then
                    tryAtV i n.Left
                elif i > lc then
                    tryAtV (i - lc - 1) n.Right
                else
                    ValueSome n.Value
        
        
        let rec unsafeAt (i : int) (n : Node<'a>) =
            if isNull n then Unchecked.defaultof<'a>
            elif n.Height = 1uy then
                if i = 0 then n.Value
                else Unchecked.defaultof<'a>
            else
                let n = n :?> Inner<'a>
                let lc = count n.Left
                if i < lc then
                    unsafeAt i n.Left
                elif i > lc then
                    unsafeAt (i - lc - 1) n.Right
                else
                    n.Value
               
        
        let rec exists (predicate : 'a -> bool) (n : Node<'a>) =
            if isNull n then false
            elif n.Height = 1uy then predicate n.Value
            else
                let n = n :?> Inner<'a>
                exists predicate n.Left || predicate n.Value || exists predicate n.Right
        
        let rec forall (predicate : 'a -> bool) (n : Node<'a>) =
            if isNull n then true
            elif n.Height = 1uy then predicate n.Value
            else
                let n = n :?> Inner<'a>
                forall predicate n.Left && predicate n.Value && forall predicate n.Right
        
        let rec tryFindIndex (predicate : 'a -> bool) (n : Node<'a>) =
            if isNull n then None
            elif n.Height = 1uy then
                if predicate n.Value then
                    Some 0
                else
                    None
            else
                let n = n :?> Inner<'a>
                
                match tryFindIndex predicate n.Left with
                | Some i -> Some i
                | None ->
                    let lc = count n.Left
                    if predicate n.Value then
                        Some lc
                    else
                        match tryFindIndex predicate n.Right with
                        | Some i ->
                            Some (i + lc + 1)
                        | None ->
                            None
        
        let rec map (mapping : 'a -> 'b) (n : Node<'a>) =
            if isNull n then null
            elif n.Height = 1uy then Node(1uy, mapping n.Value)
            else
                let n = n :?> Inner<'a>
                Inner(n.Height, mapping n.Value, map mapping n.Left, map mapping n.Right, n.Count) :> Node<_>

        let rec choose (mapping : 'a -> option<'b>) (n : Node<'a>) =
            if isNull n then null
            elif n.Height = 1uy then
                match mapping n.Value with
                | Some value -> Node(1uy, value)
                | None -> null
            else
                let n = n :?> Inner<'a>
                let l = choose mapping n.Left
                let value = mapping n.Value
                let r = choose mapping n.Right
                match value with
                | Some value ->
                    binary l value r
                | None ->
                    join l r

        let rec filter (predicate : 'a -> bool) (n : Node<'a>) =
            if isNull n then null
            elif n.Height = 1uy then
                if predicate n.Value then n
                else null
            else
                let n = n :?> Inner<'a>
                let l = filter predicate n.Left
                let takeSelf = predicate n.Value
                let r = filter predicate n.Right
                if takeSelf then
                    binary l n.Value r
                else 
                    join l r

        let rec collect (mapping : 'a -> Node<'b>) (n : Node<'a>) =
            if isNull n then null
            elif n.Height = 1uy then
                mapping n.Value
            else
                let n = n :?> Inner<'a>
                let l = collect mapping n.Left
                let s = mapping n.Value
                let r = collect mapping n.Right
                match count s with
                | 0 -> join l r
                | 1 -> binary l s.Value r
                | _ -> join3 l s r
                
module ArrNodeImplementationAggregate =

    type Aggregator<'a, 's> =
        {
            View    : 'a -> 's
            Add     : OptimizedClosures.FSharpFunc<'s, 's, 's>
            Zero    : 's
        }
        
        
    [<AllowNullLiteral>]
    type AggregateNode<'a, 's> =
        val mutable public Height : byte
        val mutable public Value : 'a

        new(h, v) = { Height = h; Value = v }

    type AggregateInner<'a, 's> =
        inherit AggregateNode<'a, 's>
        val mutable public Left : AggregateNode<'a, 's>
        val mutable public Right : AggregateNode<'a, 's>
        val mutable public Count : int
        val mutable public Aggregate : 's

        static member inline GetCount(node : AggregateNode<'a, 's>) =
            if isNull node then 0
            elif node.Height = 1uy then 1
            else (node :?> AggregateInner<'a, 's>).Count

        static member inline GetHeight(node : AggregateNode<'a, 's>) =
            if isNull node then 0uy
            else node.Height

        static member inline GetAggregate(a : Aggregator<'a, 's>, n : AggregateNode<'a, 's>) =
            if isNull n then a.Zero
            elif n.Height = 1uy then a.View n.Value
            else
                let n = n :?> AggregateInner<'a, 's>
                n.Aggregate
        
        static member inline FixHeightAndCount(inner : AggregateInner<'a, 's>) =
            let lc = AggregateInner.GetCount inner.Left
            let rc = AggregateInner.GetCount inner.Right
            let lh = if lc > 0 then inner.Left.Height else 0uy
            let rh = if rc > 0 then inner.Right.Height else 0uy
            inner.Count <- 1 + lc + rc
            inner.Height <- 1uy + max lh rh

        static member New(a : Aggregator<'a, 's>, l : AggregateNode<'a, 's>, value : 'a, r : AggregateNode<'a, 's>) : AggregateNode<'a, 's> =
            if isNull l && isNull r then AggregateNode(1uy, value)
            else 
                let h = 1uy + max (AggregateInner.GetHeight l) (AggregateInner.GetHeight r)
                let c = 1 + AggregateInner.GetCount l + AggregateInner.GetCount r
                AggregateInner<'a, 's>(a, h, value, l, r, c) :> AggregateNode<_,_>

        new(a : Aggregator<'a, 's>, h : byte, v : 'a, l : AggregateNode<'a, 's>, r : AggregateNode<'a, 's>, c : int) =
            let va = a.Add.Invoke(a.Add.Invoke(AggregateInner.GetAggregate(a, l), a.View v), AggregateInner.GetAggregate(a, r))
            { inherit AggregateNode<'a, 's>(h, v); Left = l; Right = r; Count = c; Aggregate = va }

    module AggregateNode = 
        let empty<'a, 's> : AggregateNode<'a, 's> = null
        
        let inline height (n : AggregateNode<'a, 's>) =
            if isNull n then 0uy
            else n.Height

        let inline aggregate (a : Aggregator<'a, 's>) (n : AggregateNode<'a, 's>) =
            if isNull n then a.Zero
            elif n.Height = 1uy then a.View n.Value
            else
                let n = n :?> AggregateInner<'a, 's>
                n.Aggregate
        
        let inline isEmpty (n : AggregateNode<'a, 's>) =
            isNull n

        let inline count (n : AggregateNode<'a, 's>) =
            if isNull n then 0
            elif n.Height = 1uy then 1
            else (n :?> AggregateInner<'a, 's>).Count


        let inline balance (n : AggregateInner<'a, 's>) =
            int (height n.Right) - int (height n.Left)

        let rec binary (a : Aggregator<'a, 's>) (l : AggregateNode<'a, 's>) (v : 'a) (r : AggregateNode<'a, 's>) =
            let lc = count l
            let rc = count r
            let lh = if lc > 0 then l.Height else 0uy
            let rh = if rc > 0 then r.Height else 0uy

            let b = int rh - int lh
            if b > 2 then
                // rh > lh + 2
                let r = r :?> AggregateInner<'a, 's>
                let rb = balance r
                if rb > 0 then
                    // right right 
                    binary
                        a
                        (binary a l v r.Left)
                        r.Value
                        r.Right
                else
                    // right left
                    let rl = r.Left :?> AggregateInner<'a, 's>
                    binary a
                        (binary a l v rl.Left)
                        rl.Value
                        (binary a rl.Right r.Value r.Right)

            elif b < -2 then
                // lh > rh + 2
                let l = l :?> AggregateInner<'a, 's>
                let lb = balance l
                if lb < 0 then
                    // left left
                    binary a
                        l.Left
                        l.Value
                        (binary a l.Right v r)
                else
                    // left right
                    let lr = l.Right :?> AggregateInner<'a, 's>
                    binary a
                        (binary a l.Left l.Value lr.Left)
                        lr.Value
                        (binary a lr.Right v r)

            elif lh = 0uy && rh = 0uy then AggregateNode(1uy, v)
            else AggregateInner(a, 1uy + max lh rh, v, l, r, 1 + lc + rc) :> AggregateNode<_, _>
        
        let inline unsafeInner a l v r = AggregateInner.New(a, l, v, r)

        let unsafeBinary (a : Aggregator<'a, 's>) (l : AggregateNode<'a, 's>) (v : 'a) (r : AggregateNode<'a, 's>) =
            let lc = count l
            let rc = count r
            let lh = if lc > 0 then l.Height else 0uy
            let rh = if rc > 0 then r.Height else 0uy

            let b = int rh - int lh
            if b > 2 then
                // rh > lh + 2
                let r = r :?> AggregateInner<'a, 's>
                let rb = balance r
                if rb > 0 then
                    // right right 
                    unsafeInner a
                        (unsafeInner a l v r.Left)
                        r.Value
                        r.Right
                else
                    // right left
                    let rl = r.Left :?> AggregateInner<'a, 's>
                    unsafeInner a
                        (unsafeInner a l v rl.Left)
                        rl.Value
                        (unsafeInner a rl.Right r.Value r.Right)

            elif b < -2 then
                // lh > rh + 2
                let l = l :?> AggregateInner<'a, 's>
                let lb = balance l
                if lb < 0 then
                    // left left
                    unsafeInner a
                        l.Left
                        l.Value
                        (unsafeInner a l.Right v r)
                else
                    // left right
                    let lr = l.Right :?> AggregateInner<'a, 's>
                    unsafeInner a
                        (unsafeInner a l.Left l.Value lr.Left)
                        lr.Value
                        (unsafeInner a lr.Right v r)

            elif lh = 0uy && rh = 0uy then AggregateNode(1uy, v)
            else AggregateInner(a, 1uy + max lh rh, v, l, r, 1 + lc + rc) :> AggregateNode<_,_>



        let rec unsafeRemoveMin (a : Aggregator<'a, 's>) (n : AggregateNode<'a, 's>) =
            if n.Height = 1uy then
                struct(n.Value, null)
            else
                let n = n :?> AggregateInner<'a, 's>
                if isNull n.Left then
                    struct(n.Value, n.Right)
                else
                    let struct(value, newLeft) = unsafeRemoveMin a n.Left
                    let node = unsafeBinary a newLeft n.Value n.Right
                    struct(value, node)

        let rec unsafeRemoveMax (a : Aggregator<'a, 's>) (n : AggregateNode<'a, 's>) =
            if n.Height = 1uy then
                struct(n.Value, null)
            else
                let n = n :?> AggregateInner<'a, 's>
                if isNull n.Right then
                    struct(n.Value, n.Left)
                else
                    let struct(value, newRight) = unsafeRemoveMax a n.Right
                    let node = unsafeBinary a n.Left n.Value newRight
                    struct(value, node)


        let rec join (a : Aggregator<'a, 's>) (l : AggregateNode<'a, 's>) (r : AggregateNode<'a, 's>) =
            if isNull l then r
            elif isNull r then l
            else
                let lh = l.Height
                let rh = r.Height
                if lh > rh then
                    let struct(v, ln) = unsafeRemoveMax a l
                    binary a ln v r
                else
                    let struct(v, rn) = unsafeRemoveMin a r
                    binary a l v rn

        let rec insertAt (a : Aggregator<'a, 's>) (index : int) (value : 'a) (n : AggregateNode<'a, 's>) =
            if isNull n then
                AggregateNode(1uy, value)
            elif n.Height = 1uy then
                if index = 0 then binary a null value n
                else binary a n value null
            else
                let n = n :?> AggregateInner<'a, 's>
                let c = count n.Left
                if index <= c then
                    binary a (insertAt a index value n.Left) n.Value n.Right
                else
                    binary a n.Left n.Value (insertAt a (index - c - 1) value n.Right)
                    
        let rec set (a : Aggregator<'a, 's>) (index : int) (value : 'a) (n : AggregateNode<'a, 's>) =
            if isNull n then
                null
            elif n.Height = 1uy then
                if index = 0 then AggregateNode(1uy, value)
                else n
            else
                let n = n :?> AggregateInner<'a, 's>
                let c = count n.Left
                if index < c then
                    let newLeft = set a index value n.Left
                    binary a newLeft n.Value n.Right
                elif index > c then
                    let newRight = set a (index - c - 1) value n.Right
                    binary a n.Left n.Value newRight
                else
                    binary a n.Left value n.Right

        let rec append (a : Aggregator<'a, 's>) (value : 'a) (n : AggregateNode<'a, 's>) =
            if isNull n then
                AggregateNode(1uy, value)
            elif n.Height = 1uy then
                binary a n value null
            else
                let n = n :?> AggregateInner<'a, 's>
                binary a n.Left n.Value (append a value n.Right)

        let rec prepend (a : Aggregator<'a, 's>) (value : 'a) (n : AggregateNode<'a, 's>) =
            if isNull n then
                AggregateNode(1uy, value)
            elif n.Height = 1uy then
                binary a null value n
            else
                let n = n :?> AggregateInner<'a, 's>
                binary a (prepend a value n.Left) n.Value n.Right


        let rec skip (a : Aggregator<'a, 's>) (n : int) (node : AggregateNode<'a, 's>) =
            if n <= 0 then node 
            elif isNull node || node.Height = 1uy then
                null
            else
                let node = node :?> AggregateInner<'a, 's>
                if n >= node.Count then
                    null
                else
                    let lc = count node.Left
                    if n < lc then binary a (skip a n node.Left) node.Value node.Right
                    elif n = lc then prepend a node.Value node.Right
                    else skip a (n - lc - 1) node.Right

        let rec take (a : Aggregator<'a, 's>) (n : int) (node : AggregateNode<'a, 's>) =
            if n <= 0 then null
            elif isNull node || node.Height = 1uy then
                node
            else
                let node = node :?> AggregateInner<'a, 's>
                if n >= node.Count then
                    node
                else
                    let lc = count node.Left
                    if n <= lc then take a n node.Left
                    elif n = lc then append a node.Value node.Left
                    else binary a node.Left node.Value (take a (n - lc - 1) node.Right)

        let rec sub (a : Aggregator<'a, 's>) (l : int) (r : int) (node : AggregateNode<'a, 's>) =
        
            if isNull node then
                null
            elif node.Height = 1uy then
                if l <= 0 && r >= 0 then node
                else null
            else
                let node = node :?> AggregateInner<'a, 's>
                let lc = count node.Left
                if r < lc then 
                    sub a l r node.Left
                elif l > lc then 
                    sub a (l - lc - 1) (r - lc - 1) node.Right
                elif l = lc then
                    prepend a node.Value (take a (r - l) node.Right)
                elif r = lc then
                    append a node.Value (skip a l node.Left)
                else
                    binary a (skip a l node.Left) node.Value (take a (r - lc) node.Right)


        let join3 (agg : Aggregator<'a, 's>) (a : AggregateNode<'a, 's>) (b : AggregateNode<'a, 's>) (c : AggregateNode<'a, 's>) =
            join agg a (join agg b c) // TODO!!!

        let rec removeRange (a : Aggregator<'a, 's>) (l : int) (r : int) (node : AggregateNode<'a, 's>) = 
            if isNull node then
                null
            elif node.Height = 1uy then
                if l <= 0 && r >= 0 then null
                else node
            else
                let node = node :?> AggregateInner<'a, 's>
                let lc = count node.Left
                if r < lc then 
                    binary a (removeRange a l r node.Left) node.Value node.Right
                elif l > lc then 
                    binary a node.Left node.Value (removeRange a (l - lc - 1) (r - lc - 1) node.Right)
                elif l = lc then
                    join a node.Left (skip a (r - l) node.Right)
                elif r = lc then
                    join a (take a l node.Left) node.Right
                else
                    let na = take a l node.Left 
                    let nb = skip a (r - lc) node.Right
                    join a na nb

        let rec tryRemove (a : Aggregator<'a, 's>) (index : int) (node : AggregateNode<'a, 's>) =
            if isNull node then
                None
            elif node.Height = 1uy then
                if index = 0 then Some (null, node.Value)
                else None
            else
                let node = node :?> AggregateInner<'a, 's>
                let lc = count node.Left
                
                if index < lc then
                    match tryRemove a index node.Left with
                    | Some (rest, v) ->
                        Some (binary a rest node.Value node.Right, v)
                    | None ->
                        None
                elif index > lc then
                    match tryRemove a (index - lc - 1) node.Right with
                    | Some (rest, v) ->
                        Some (binary a node.Left node.Value rest, v)
                    | None ->
                        None
                else
                    Some (join a node.Left node.Right, node.Value)
        
        
        let rec insertRange (a : Aggregator<'a, 's>) (index : int) (repl : AggregateNode<'a, 's>) (node : AggregateNode<'a, 's>) =
            if isNull node then
                repl
            elif node.Height = 1uy then
                if index <= 0 then append a node.Value repl
                else prepend a node.Value repl
            else
                let node = node :?> AggregateInner<'a, 's>
                let lc = count node.Left
                if index < lc then
                    binary a (insertRange a index repl node.Left) node.Value node.Right
                elif index = lc then
                    let lh = height node.Left
                    let rh = height node.Right
                    if lh < rh then
                        let l = append a node.Value node.Left
                        join a (join a l repl) node.Right
                    else
                        let r = prepend a node.Value node.Right
                        join a node.Left (join a repl r)
                else
                    binary a node.Left node.Value (insertRange a (index - lc - 1) repl node.Right)

        let rec replaceRange (a : Aggregator<'a, 's>) (l : int) (r : int) (repl : AggregateNode<'a, 's>) (node : AggregateNode<'a, 's>) =
            if isNull node then
                repl
            elif node.Height = 1uy then
                if l <= 0 && r >= 0 then repl
                elif r < 0 then append a node.Value repl
                else prepend a node.Value repl
            else
                let node = node :?> AggregateInner<'a, 's>
                let lc = count node.Left
                if r < lc then 
                    binary a (replaceRange a l r repl node.Left) node.Value node.Right
                elif l > lc then 
                    binary a node.Left node.Value (replaceRange a (l - lc - 1) (r - lc - 1) repl node.Right)
                elif l = lc then
                    join3 a node.Left repl (skip a (r - l) node.Right)
                elif r = lc then
                    join3 a (take a l node.Left) repl node.Right
                else
                    let na = take a l node.Left 
                    let nb = skip a (r - lc) node.Right
                    join3 a na repl nb

        let rec tryAt (i : int) (n : AggregateNode<'a, 's>) =
            if isNull n then None
            elif n.Height = 1uy then
                if i = 0 then Some n.Value
                else None
            else
                let n = n :?> AggregateInner<'a, 's>
                let lc = count n.Left
                if i < lc then
                    tryAt i n.Left
                elif i > lc then
                    tryAt (i - lc - 1) n.Right
                else
                    Some n.Value
                    
        let rec tryAtV (i : int) (n : AggregateNode<'a, 's>) =
            if isNull n then ValueNone
            elif n.Height = 1uy then
                if i = 0 then ValueSome n.Value
                else ValueNone
            else
                let n = n :?> AggregateInner<'a, 's>
                let lc = count n.Left
                if i < lc then
                    tryAtV i n.Left
                elif i > lc then
                    tryAtV (i - lc - 1) n.Right
                else
                    ValueSome n.Value
        
        
        let rec tryAggregateAtIncl (a : Aggregator<'a, 's>) (i : int) (n : AggregateNode<'a, 's>) =
            if isNull n then
                None
            elif n.Height = 1uy then
                if i = 0 then Some (a.View n.Value)
                else None
            else
                let n = n :?> AggregateInner<'a, 's>
                let lc = count n.Left
                if i < lc then
                    tryAggregateAtIncl a i n.Left
                elif i > lc then
                    match tryAggregateAtIncl a (i - lc - 1) n.Right with
                    | None -> None
                    | Some rv ->
                        let lv = a.Add.Invoke(aggregate a n.Left, a.View n.Value)
                        Some (a.Add.Invoke(lv, rv))
                else
                    a.Add.Invoke(aggregate a n.Left, a.View n.Value) |> Some
                    
        
        
        let rec exists (predicate : 'a -> bool) (n : AggregateNode<'a, 's>) =
            if isNull n then false
            elif n.Height = 1uy then predicate n.Value
            else
                let n = n :?> AggregateInner<'a, 's>
                exists predicate n.Left || predicate n.Value || exists predicate n.Right
        
        let rec forall (predicate : 'a -> bool) (n : AggregateNode<'a, 's>) =
            if isNull n then true
            elif n.Height = 1uy then predicate n.Value
            else
                let n = n :?> AggregateInner<'a, 's>
                forall predicate n.Left && predicate n.Value && forall predicate n.Right
        
        let rec tryFindIndex (predicate : 'a -> bool) (n : AggregateNode<'a, 's>) =
            if isNull n then None
            elif n.Height = 1uy then
                if predicate n.Value then
                    Some 0
                else
                    None
            else
                let n = n :?> AggregateInner<'a, 's>
                
                match tryFindIndex predicate n.Left with
                | Some i -> Some i
                | None ->
                    let lc = count n.Left
                    if predicate n.Value then
                        Some lc
                    else
                        match tryFindIndex predicate n.Right with
                        | Some i ->
                            Some (i + lc + 1)
                        | None ->
                            None
        
        let rec copyTo (index : int) (dst : 'a[]) (n : AggregateNode<'a, 's>) =
            if not (isNull n) then
                if n.Height = 1uy then dst.[index] <- n.Value
                else
                    let n = n :?> AggregateInner<'a, 's>
                    let lc = count n.Left
                    copyTo index dst n.Left
                    dst.[index + lc] <- n.Value
                    copyTo (index + lc + 1) dst n.Right

        let rec toList (acc : list<'a>) (node : AggregateNode<'a, 's>) =
            if isNull node then
                acc
            elif node.Height = 1uy then
                node.Value :: acc
            else
                let n = node :?> AggregateInner<'a, 's>
                toList (n.Value :: toList acc n.Right) n.Left

        let toArray (n : AggregateNode<'a, 's>) =
            let c = count n
            let a = Array.zeroCreate c
            copyTo 0 a n
            a



        
    [<Struct; StructuredFormatDisplay("{AsString}")>]
    type arr<'a, 's> internal(a : Aggregator<'a, 's>, store : AggregateNode<'a, 's>) =
        static member Empty(a : Aggregator<'a, 's>) : arr<'a, 's> = arr<'a, 's>(a, null)
        
        member internal x.Store = store
        member x.IsEmpty = AggregateNode.isEmpty store
        member x.Length = AggregateNode.count store
        
        member x.InsertAt(index : int, value : 'a) = arr(a, AggregateNode.insertAt a index value store)
        member x.Prepend(value : 'a) = arr(a, AggregateNode.prepend a value store)
        member x.Append(value : 'a) = arr(a, AggregateNode.append a value store)
        member x.Add(value : 'a) = x.Append(value)
        member x.Set(i : int, value : 'a) =
            if i < 0 || i >= x.Length then raise <| IndexOutOfRangeException()
            else arr(a, AggregateNode.set a i value store)
        member x.RemoveAt(index : int) = arr(a, AggregateNode.removeRange a index index store)
        
        member x.Skip(n : int) =
            if n <= 0 then x
            elif n >= x.Length then arr.Empty a
            else arr(a, AggregateNode.skip a n store)
            
        member x.Take(n : int) =
            if n <= 0 then arr.Empty a
            elif n < x.Length then arr(a, AggregateNode.take a n store)
            else x
            
            
        member x.Sub(offset : int, count : int) =
            arr(a, AggregateNode.sub a offset (offset + count - 1) store)

        member x.TryRemove(index : int) =
            match AggregateNode.tryRemove a index store with
            | Some (node, value) ->
                Some(arr(a, node), value)
            | None ->
                None
        member x.GetSlice(min : option<int>, max : option<int>) =
            match min with
            | Some min ->
                match max with
                | Some max ->
                    x.Sub(min, 1 + max - min)
                | None ->
                    x.Skip min
            | None ->
                match max with
                | Some max ->
                    x.Take(max + 1)
                | None ->
                    x
        
        member x.TryGetAggregateAt(i : int) =
            AggregateNode.tryAggregateAtIncl a i store
    
        member x.TryGetAggregateAtExcl(i : int) =
            if i = 0 then
                if isNull store then None
                else Some a.Zero
            else
                AggregateNode.tryAggregateAtIncl a (i - 1) store
        
        member x.ToArray() =
            AggregateNode.toArray store

        member x.Item
            with get(i : int) =
                match AggregateNode.tryAt i store with
                | Some v -> v
                | None -> failwith "index out of range"
    
    let test() =
        
        let a = { View = List.length; Add = OptimizedClosures.FSharpFunc<_,_,_>.Adapt((+)); Zero = 0 }
        
        let thing = arr.Empty(a).Add([1;2;3]).Add([4;5;6]).Add([2]).Add([10])
        
        thing.TryGetAggregateAtExcl(0) |> printfn "%A"
        thing.TryGetAggregateAtExcl(1) |> printfn "%A"
        thing.TryGetAggregateAtExcl(2) |> printfn "%A"
        thing.TryGetAggregateAtExcl(3) |> printfn "%A"
        thing.TryGetAggregateAtExcl(4) |> printfn "%A"
        
        
        let thing = thing.Skip(1)
        
        thing.TryGetAggregateAtExcl(0) |> printfn "%A"
        thing.TryGetAggregateAtExcl(1) |> printfn "%A"
        thing.TryGetAggregateAtExcl(2) |> printfn "%A"
        thing.TryGetAggregateAtExcl(3) |> printfn "%A"
        
        
        let a = { View = id; Add = OptimizedClosures.FSharpFunc<_,_,_>.Adapt((+)); Zero = 0 }
        
        let thing = arr.Empty(a).Add(1).Add(2).Add(3).Add(4).Add(5)
        
        printfn "%A" (thing.ToArray())
        match thing.TryRemove(1) with
        | Some (a, b) ->
            printfn "%A" (a.ToArray())
            printfn "%A" b
        | None ->
            printfn "out of bounds"
        
open ArrNodeImplementation



[<Struct; StructuredFormatDisplay("{AsString}"); DebuggerTypeProxy(typedefof<ArrProxy<_>>)>]
type arr<'a> internal(store : Node<'a>) =
    static member Empty : arr<'a> = arr<'a> null

    static member Single(value : 'a) = arr(Node<'a>(1uy, value)) 
    static member FromArray(elements : 'a[]) = arr (Node.ofSpan (System.ReadOnlySpan elements))
    static member FromSpan(elements : System.ReadOnlySpan<'a>) = arr (Node.ofSpan elements)
    static member FromSpan(elements : System.Span<'a>) = arr (Node.ofSpan (System.Span.op_Implicit elements))
    static member FromMemory(elements : System.Memory<'a>) = arr (Node.ofSpan (System.Span.op_Implicit elements.Span))
    static member FromMemory(elements : System.ReadOnlyMemory<'a>) = arr (Node.ofSpan elements.Span)

    static member Concat([<ParamArray>] arrs : arr<'a>[]) =
        if arrs.Length > 0 then
            let mutable res = arrs.[0]
            for i in 1 .. arrs.Length - 1 do
                res <- arr(Node.join res.Store arrs.[i].Store)
            res
        else
            arr<'a>.Empty
    
    member internal x.Store = store
    member x.IsEmpty = Node.isEmpty store
    member x.Length = Node.count store
    member x.Insert(index : int, value : 'a) = arr(Node.insert index value store)
    member x.Add(value : 'a) = arr(Node.append value store)
    member x.Prepend(value : 'a) = arr(Node.prepend value store)
    member x.Append(value : 'a) = arr(Node.append value store)
    member x.Skip(n : int) =
        if n <= 0 then x
        elif n >= x.Length then arr.Empty
        else arr(Node.skip n store)
        
    member x.Take(n : int) =
        if n <= 0 then arr.Empty
        elif n < x.Length then arr(Node.take n store)
        else x
        
    member x.Split(n : int) =
        if n <= 0 then arr.Empty, x
        elif n >= x.Length then x, arr.Empty
        else
            let l, r = Node.split n store
            arr(l), arr(r)
        
    member x.Sub(offset : int, count : int) =
        arr(Node.sub offset (offset + count - 1) store)

    member x.GetSlice(min : option<int>, max : option<int>) =
        match min with
        | Some min ->
            match max with
            | Some max ->
                x.Sub(min, 1 + max - min)
            | None ->
                x.Skip min
        | None ->
            match max with
            | Some max ->
                x.Take(max + 1)
            | None ->
                x
    
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

    member this.CopyTo(array,arrayIndex) =
        Node.copyTo arrayIndex array store
        
    override x.ToString() =
        if Node.count store > 10 then Node.toArray (Node.take 10 store) |> Seq.map (sprintf "%A") |> String.concat "; " |> sprintf "arr [%s]"
        else Node.toArray store |> Seq.map (sprintf "%A") |> String.concat "; " |> sprintf "arr [%s]"

    member private x.AsString = x.ToString()
    member x.GetEnumerator() = new ArrEnumerator<'a>(store)

    member x.Set(index : int, value : 'a) =
        if index >= 0 && index < x.Length then
            arr(Node.unsafeSet index value store)
        else
            raise <| IndexOutOfRangeException()
            
    member x.Item
        with get(i : int) = 
            match Node.tryAtV i store with
            | ValueSome v -> v
            | ValueNone -> raise <| IndexOutOfRangeException()

            
    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = new ArrEnumerator<'a>(store) :> _

    interface System.Collections.Generic.IEnumerable<'a> with
        member x.GetEnumerator() = new ArrEnumerator<'a>(store) :> _

    interface System.Collections.Generic.ICollection<'a> with
        member this.Add(item) = failwith "readonly"
        member this.Clear() = failwith "readonly"
        member this.Contains(item) = Node.exists (Unchecked.equals item) store
        member this.CopyTo(array,arrayIndex) = this.CopyTo(array, arrayIndex)
        member this.Remove(item) = failwith "readonly"
        member this.Count = this.Length
        member this.IsReadOnly = true
    
    interface System.Collections.Generic.IList<'a> with
        member this.Insert(index,item) = failwith "readonly"
        member this.RemoveAt(index) = failwith "readonly"
        member this.IndexOf(item) =
            match Node.tryFindIndex (Unchecked.equals item) store with
            | Some idx -> idx
            | None -> -1
        member this.Item
            with get index = this.[index]
            and set _ _ = failwith "readonly"
            
    interface System.Collections.Generic.IReadOnlyCollection<'a> with
        member this.Count = this.Length
        
    interface System.Collections.Generic.IReadOnlyList<'a> with
        member this.Item
            with get index = this.[index]

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

    [<DebuggerBrowsable(DebuggerBrowsableState.Collapsed)>]
    member x.Length = arr.Length
    
    [<DebuggerBrowsable(DebuggerBrowsableState.RootHidden)>]
    member x.Items = items


[<Struct; StructuredFormatDisplay("{AsString}")>]
type ArrOperation<'a> = { Index : int; Count : int; Elements : arr<'a> } with
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
    
    member private x.AsString = x.ToString()
    
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
    let inline length (a : arr<'a>) = a.Length

    let inline insert (index : int) (value : 'a) (arr : arr<'a>) = arr.Insert(index, value)
    let inline add (value : 'a) (arr : arr<'a>) = arr.Add value
    let inline append (value : 'a) (arr : arr<'a>) = arr.Append value
    let inline prepend (value : 'a) (arr : arr<'a>) = arr.Prepend value
    let inline set (index : int) (value : 'a) (arr : arr<'a>) = arr.Set(index, value)

    let inline single (value : 'a) = arr.Single(value)
    let inline ofSeq (seq : seq<'a>) = arr.FromArray (Seq.toArray seq)
    let inline ofList (array : list<'a>) = arr.FromArray (List.toArray array)
    let inline ofArray (array : 'a[]) = arr.FromArray array
    let inline ofSpan (array : System.Span<'a>) = arr.FromSpan array
    let inline ofMemory (array : System.Memory<'a>) = arr.FromMemory array

    let inline toSeq (arr : arr<'a>) = arr :> seq<_>
    let inline toList (arr : arr<'a>) = arr.ToList()
    let inline toArray (arr : arr<'a>) = arr.ToArray()

    let inline take (n : int) (arr : arr<'a>) = arr.Take n
    let inline skip (n : int) (arr : arr<'a>) = arr.Skip n
    let inline sub (index : int) (count : int) (arr : arr<'a>) = arr.Sub(index, count)
    let inline split (index : int) (arr : arr<'a>) = arr.Split(index)
    
    let uncons (array : arr<'a>) =
        if array.Length > 0 then
            let struct(value, rest) = Node.unsafeRemoveMin array.Store
            value, arr(rest)
        else
            raise <| IndexOutOfRangeException()
            
    let unsnoc (array : arr<'a>) =
        if array.Length > 0 then
            let struct(value, rest) = Node.unsafeRemoveMax array.Store
            value, arr(rest)
        else
            raise <| IndexOutOfRangeException()
    
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

    let map (mapping : 'a -> 'b) (delta : arr<'a>) =
        delta.Store |> Node.map mapping |> arr

    let choose (mapping : 'a -> option<'b>) (delta : arr<'a>) =
        delta.Store |> Node.choose mapping |> arr

    let filter (predicate : 'a -> bool) (delta : arr<'a>) =
        delta.Store |> Node.filter predicate |> arr

    let collect (mapping : 'a -> arr<'b>) (array : arr<'a>) =
        array.Store |> Node.collect (fun v -> mapping(v).Store) |> arr
        
    
            
module ArrDelta =

    let isEmpty (d : arrdelta<'a>) = d.Length = 0
    
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

        let append c =
            match pending with
            | Some last ->
                if last.Index = c.Index  then
                    let n = { Index = last.Index; Count = last.Count + (c.Count - last.Elements.Length); Elements = arr (Node.join c.Elements.Store (Node.skip c.Count last.Elements.Store)) }
                    pending <- Some n
                
                else
                    let l = last.Index 
                    let h = last.Index + last.Elements.Length - 1
                    
                    if c.Index = h + 1 then
                        let newOp =
                            { Index = last.Index; Count = last.Count + c.Count; Elements = arr(Node.join last.Elements.Store c.Elements.Store) }
                        pending <- Some newOp
                    
                    elif c.Index >= l && c.Index <= h && c.Count >= 0 then
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


    let map (mapping : 'a -> 'b) (delta : arrdelta<'a>) =
        delta.Store |> Node.map (fun op ->
            { Index = op.Index; Count = op.Count; Elements = op.Elements |> Arr.map mapping }    
        ) |> arrdelta

[<AutoOpen>]
module ``ArrDelta Extensions`` =
    open FSharp.Data.Traceable
    
    module ArrDelta =
        [<GeneralizableValue>]
        let monoid<'a> : Monoid<arrdelta<'a>> =
            {
                mempty = ArrDelta.empty
                misEmpty = ArrDelta.isEmpty
                mappend = ArrDelta.combine 
            }

    module Arr =
        [<GeneralizableValue>]
        let trace<'a> : Traceable<arr<'a>, arrdelta<'a>> =
            {
                tempty =  Arr.empty
                tcomputeDelta = Arr.computeDelta Unchecked.equals
                tapplyDelta = fun m d -> Arr.applyDelta m d, d
                tmonoid = ArrDelta.monoid
                tsize = fun _ -> 0
                tprune = None
            }
            
open FSharp.Data.Traceable
type IArrayReader<'a> = IOpReader<arr<'a>, arrdelta<'a>>
            
type IAdaptiveArray<'a> =
    abstract IsConstant : bool
    abstract Content : aval<arr<'a>>
    abstract History : option<History<arr<'a>, arrdelta<'a>>>
    abstract GetReader : unit -> IArrayReader<'a>
            
and aarr<'a> = IAdaptiveArray<'a>


/// Changeable adaptive list that allows mutation by user-code and implements alist.
[<Sealed>]
type ChangeableArray<'T>(elements: arr<'T>) =
    let history = 
        let h = History(Arr.trace)
        h.Perform(Arr.trace.tcomputeDelta Arr.empty elements) |> ignore
        h

    override x.ToString() =
        history.State |> Seq.map (sprintf "%A") |> String.concat "; " |> sprintf "carr [%s]"

    /// is the list currently empty?
    member x.IsEmpty = history.State.IsEmpty

    /// the number of elements currently in the list.
    member x.Length = history.State.Length

    member x.Value
        with get() = history.State
        and set v = history.Perform (Arr.trace.tcomputeDelta history.State v) |> ignore

    interface IAdaptiveArray<'T> with
        member x.GetReader() = history.NewReader()
        member x.Content = history :> aval<_>
        member x.IsConstant = false
        member x.History = Some history


module AArr =
    open FSharp.Data.Traceable
    
    module Readers = 
        /// Efficient implementation for a constant adaptive array.
        [<Sealed>]
        type ConstantArray<'T>(content : Lazy<arr<'T>>) =
            let value = AVal.delay (fun () -> content.Value)

            member x.Content = value

            member x.GetReader() =
                new History.Readers.ConstantReader<_,_>(
                    Arr.trace,
                    lazy (Arr.computeDelta DefaultEquality.equals Arr.empty content.Value),
                    content
                ) :> IArrayReader<_>

            interface IAdaptiveArray<'T> with
                member x.IsConstant = true
                member x.GetReader() = x.GetReader()
                member x.Content = x.Content
                member x.History = None

        /// Core implementation for a dependent array.
        [<Sealed>]
        type AdaptiveArrayImpl<'T>(createReader : unit -> IOpReader<arrdelta<'T>>) =
            let history = History(createReader, Arr.trace)

            /// Gets a new reader to the set.
            member x.GetReader() : IArrayReader<'T> =
                history.NewReader()

            /// Current content of the set as aval.
            member x.Content =
                history :> aval<_>

            interface IAdaptiveArray<'T> with
                member x.IsConstant = false
                member x.GetReader() = x.GetReader()
                member x.Content = x.Content
                member x.History = Some history

        /// Efficient implementation for an empty adaptive array.
        [<Sealed>]
        type EmptyArray<'T> private() =   
            static let instance = EmptyArray<'T>() :> aarr<_>
            let content = AVal.constant Arr.empty
            let reader = new History.Readers.EmptyReader<arr<'T>, arrdelta<'T>>(Arr.trace) :> IArrayReader<'T>
            static member Instance = instance
            
            member x.Content = content
            member x.GetReader() = reader
            
            interface IAdaptiveArray<'T> with
                member x.IsConstant = true
                member x.GetReader() = x.GetReader()
                member x.Content = x.Content
                member x.History = None

    
        type MapReader<'a, 'b>(input : IArrayReader<'a>, mapping : 'a -> 'b) =
            inherit AbstractReader<arrdelta<'b>>(ArrDelta.empty)
            
            override x.Compute(t : AdaptiveToken) =
                let ops = input.GetChanges t
                ops |> ArrDelta.map mapping
          
        type CollectReader<'a, 'b>(input : IArrayReader<'a>, mapping : 'a -> aarr<'b>) =
            inherit AbstractReader<arrdelta<'b>>(ArrDelta.empty)
              
            static let aggregate =
                {
                    ArrNodeImplementationAggregate.Zero = 0
                    ArrNodeImplementationAggregate.Add = OptimizedClosures.FSharpFunc<_,_,_>.Adapt((+))
                    ArrNodeImplementationAggregate.View = fun (v : arr<'b>) -> v.Length
                }
              
            let mutable readers = IndexList.empty<IArrayReader<'b>>
            let mutable prefix = ArrNodeImplementationAggregate.arr(aggregate, null)
            let dirtyLock = obj()
            let mutable dirtyReaders = IndexList.empty<IArrayReader<'b>>
            
            override x.InputChangedObject(_, o) =
                match o with
                | :? IArrayReader<'b> as o ->
                    match o.Tag with
                    | :? FSharp.Data.Adaptive.Index as i ->
                        lock dirtyLock (fun () ->
                            dirtyReaders <- IndexList.set i o dirtyReaders
                        )
                    | _ ->
                        ()
                | _ ->
                    ()
            
            
            override x.Compute(t : AdaptiveToken) =
                let ops = input.GetChanges t
                
                let ops =
                    ops |> ArrDelta.map (fun op ->
                        mapping(op).GetReader()
                    )
                    
                let mutable res = ArrDelta.empty
                    
                let emit (op : ArrOperation<_>) =
                    res <- ArrDelta.combine res (arrdelta (Arr.single op))
                    
                let emitArr (op : arr<ArrOperation<_>>) =
                    for e in op do emit e
                    
                    
                let mutable dirtyReaders =
                    lock dirtyLock (fun () ->
                        let v = dirtyReaders
                        dirtyReaders <- IndexList.empty
                        v
                    )
                    
                    
                for o in ops do
                    for r in 1 .. o.Count do
                        match readers.TryGetIndex o.Index with
                        | Some idx ->
                            match readers.TryRemove idx with
                            | Some (reader, rest) ->
                                lock reader.Outputs (fun () -> reader.Outputs.Remove x |> ignore)
                                readers <- rest
                                dirtyReaders <- IndexList.remove idx dirtyReaders
                                
                                match prefix.TryGetAggregateAtExcl o.Index with
                                | Some offset ->
                                    match prefix.TryRemove(o.Index) with
                                    | Some (rest, removed) ->
                                        prefix <- rest
                                        let cnt = removed.Length
                                        emit { Index = offset; Count = cnt; Elements = Arr.empty }
                                    | None ->
                                        ()
                                | None ->
                                    ()
                                
                            | None ->
                                printfn "ERROR: reader must exist"
                        | None ->
                            printfn "ERROR: reader at %d must exist" o.Index
                    
                    let mutable index = o.Index
                    for newReader in o.Elements do
                        let before =
                            match readers.TryGetIndex (index - 1) with
                            | Some idx -> idx
                            | None -> Index.zero
                        
                        let idx = readers.NewIndexAfter before
                        newReader.Tag <- idx
                        readers <- IndexList.set idx newReader readers
                        prefix <- prefix.InsertAt(index, Arr.empty)
                        dirtyReaders <- IndexList.set idx newReader dirtyReaders
                        index <- index + 1
                    
                for ridx, reader in IndexList.toSeqIndexed dirtyReaders do
                    
                    let ri = readers.IndexOf ridx
                    
                    if ri >= 0 && ri < prefix.Length then
                        match prefix.TryGetAggregateAtExcl ri with
                        | Some offset ->
                            let op = reader.GetChanges t
                            op.ToArr()
                            |> Arr.map (fun op -> { op with Index = op.Index + offset })
                            |> emitArr
                            
                            prefix <- prefix.Set(ri, reader.State)
                            
                            
                            
                        | None ->
                            printfn "ERROR: no offset"
                    else
                        printfn "ERROR: no reader for index"
                    
                res
                
                
              
    

    /// Creates a constant set using the creation function.
    let constant (value : unit -> arr<'T>) = 
        Readers.ConstantArray(lazy(value())) :> aarr<_> 

    /// Creates an aset using the given reader-creator.
    let ofReader (create : unit -> #IOpReader<arrdelta<'T>>) =
        Readers.AdaptiveArrayImpl(fun () -> create() :> IOpReader<_>) :> aarr<_>
        
    /// The empty aset.
    [<GeneralizableValue>]
    let empty<'T> : aarr<'T> = 
        Readers.EmptyArray<'T>.Instance

    /// A constant aset holding a single value.
    let single (value : 'T) =
        constant (fun () -> Arr.single value)
        
    /// Creates an aset holding the given values.
    let ofSeq (elements : seq<'T>) =
        constant (fun () -> Arr.ofSeq elements)
        
    /// Creates an aset holding the given values.
    let ofList (elements : list<'T>) =
        constant (fun () -> Arr.ofList elements)
        
    /// Creates an aset holding the given values.
    let ofArray (elements : 'T[]) =
        constant (fun () -> Arr.ofArray elements)

    /// Creates an aval providing access to the current content of the set.
    let toAVal (set : aarr<'T>) =
        set.Content

    let map (mapping : 'a -> 'b) (a : aarr<'a>) =
        if a.IsConstant then
            constant( fun () -> Arr.map mapping (AVal.force a.Content))
        else
            ofReader <| fun () ->
                Readers.MapReader(a.GetReader(), mapping)
                
    let collect (mapping : 'a -> aarr<'b>) (a : aarr<'a>) =
        if false && a.IsConstant then
            failwith "TODO"
        else
            ofReader <| fun () ->
                Readers.CollectReader(a.GetReader(), mapping)
                         
                         
                         
module AArrTest =
    let run() =
        let a = ChangeableArray(Arr.ofList [1;2;6;8;3;4])
        let even = ChangeableArray(Arr.ofList [2])
        let odd = ChangeableArray(Arr.ofList [1])
        
        let result = 
            a |> AArr.collect (fun v ->
                if v % 2 = 0 then even
                else odd
            )
        
        let r = result.GetReader()
        
        let print() = 
            let ops = r.GetChanges AdaptiveToken.Top
            let state = r.State
            
            printfn "  %A" ops
            printfn "  %A" state
            
        printfn "initial"
        print()
        
        transact (fun () ->
            a.Value <- Arr.add 5 a.Value
        )
        printfn "add(5)"
        print()
        
        transact (fun () ->
            even.Value <- Arr.ofList [4;4]
        )
        printfn "even <- [|4|]"
        print()
        
        
        
        
        
        
        