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
            
        let rec ofArray (data : 'a[]) (l : int) (r : int)  =
            if l > r then 
                null
            elif l = r then 
                Node(1uy, data.[l])
            else
                let m = (l + r) / 2
                let lt = ofArray data l (m - 1)
                let rt = ofArray data (m + 1) r
                Inner(1uy + max (height lt) (height rt), data.[m], lt, rt, 1 + count lt + count rt) :> Node<_>

        
        let rec init (cnt : int) (offset : int) (initializer : int -> 'a) =
            if cnt <= 0 then
                null
            elif cnt = 1 then
                Node(1uy, initializer offset)
            else
                let lc = (cnt - 1) / 2
                let rc = cnt - 1 - lc
                
                let lt = init lc offset initializer
                let v = initializer (offset + lc)
                let rt = init rc (offset + lc + 1) initializer
                Inner(1uy + max (height lt) (height rt), v, lt, rt, cnt) :> Node<_>
                
                
        
        #if NET6_0_OR_GREATER
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

        #endif

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

        
        let rec updateRange (l : int) (r : int) (repl : Node<'a> -> Node<'a>) (node : Node<'a>) =
            if isNull node then
                repl null
            elif node.Height = 1uy then
                if l <= 0 && r >= 0 then repl node
                elif r < 0 then append node.Value (repl null)
                else prepend node.Value (repl null)
            else
                let node = node :?> Inner<'a>
                let lc = count node.Left
                if r < lc then 
                    binary (updateRange l r repl node.Left) node.Value node.Right
                elif l > lc then 
                    binary node.Left node.Value (updateRange (l - lc - 1) (r - lc - 1) repl node.Right)
                elif l = lc then
                    let old = prepend node.Value (take (r - l) node.Right)
                    join3 node.Left (repl old) (skip (r - l) node.Right)
                elif r = lc then
                    let old = append node.Value (skip l node.Left)
                    join3 (take l node.Left) (repl old) node.Right
                else
                    let a = take l node.Left 
                    let b = skip (r - lc) node.Right
                    
                    let inner =
                        binary (skip l node.Left) node.Value (take (r - lc) node.Right)
                    
                    join3 a (repl inner) b

        
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
        
        let rec fold (folder : 's -> 'a -> 's) (state : 's) (n : Node<'a>) =
            if isNull n then
                state
            elif n.Height = 1uy then
                folder state n.Value
            else
                let n = n :?> Inner<'a>
                let s1 = fold folder state n.Left
                let s2 = folder s1 n.Value
                fold folder s2 n.Right
        
        let rec pairwise (lastValue : voption<'a>) (n : Node<'a>) =
            if isNull n then
                null, lastValue
            elif n.Height = 1uy then
                match lastValue with
                | ValueSome l -> Node(1uy, (l, n.Value)), ValueSome n.Value
                | ValueNone -> null, ValueSome n.Value
            else
                let n = n :?> Inner<'a>
                let l, lv = pairwise lastValue n.Left
                
                let ownValue = 
                    match lv with
                    | ValueSome lv -> ValueSome (lv, n.Value)
                    | ValueNone -> ValueNone
                    
                let r, rv = pairwise (ValueSome n.Value) n.Right
                
                let newNode = 
                    match ownValue with
                    | ValueSome o -> binary l o r
                    | ValueNone -> join l r
                    
                newNode, rv
                    
               
        let rec mapPairwise (lastValue : voption<'a>) (mapping : 'a -> 'a -> 'b) (n : Node<'a>) =
            if isNull n then
                null, lastValue
            elif n.Height = 1uy then
                match lastValue with
                | ValueSome l -> Node(1uy, mapping l n.Value), ValueSome n.Value
                | ValueNone -> null, ValueSome n.Value
            else
                let n = n :?> Inner<'a>
                let l, lv = mapPairwise lastValue mapping n.Left
                
                let ownValue = 
                    match lv with
                    | ValueSome lv -> ValueSome (mapping lv n.Value)
                    | ValueNone -> ValueNone
                    
                let r, rv = mapPairwise (ValueSome n.Value) mapping n.Right
                
                let newNode = 
                    match ownValue with
                    | ValueSome o -> binary l o r
                    | ValueNone -> join l r
                    
                newNode, rv
                  
        let rec reverse (n : Node<'a>) =
            if isNull n then
                null
            elif n.Height = 1uy then
                n
            else
                let n = n :?> Inner<'a>
                Inner(n.Height, n.Value, reverse n.Right, reverse n.Left, n.Count) :> Node<_>                  
                     
        let rec foldr (folder : 'a -> 's -> 's) (n : Node<'a>) (state : 's) =
            if isNull n then
                state
            elif n.Height = 1uy then
                folder n.Value state
            else
                let n = n :?> Inner<'a>
                let s1 = foldr folder n.Right state
                let s2 = folder n.Value s1
                foldr folder n.Left s2
                
        let rec tryPick (mapping : 'a -> option<'b>) (n : Node<'a>) =
            if isNull n then
                None
            elif n.Height = 1uy then
                mapping n.Value
            else
                let n = n :?> Inner<'a>
                match tryPick mapping n.Left with
                | None ->
                    match mapping n.Value with
                    | None ->
                        tryPick mapping n.Right
                    | res ->
                        res
                | res ->
                    res
                          
        let rec tryPickV (mapping : 'a -> voption<'b>) (n : Node<'a>) =
            if isNull n then
                ValueNone
            elif n.Height = 1uy then
                mapping n.Value
            else
                let n = n :?> Inner<'a>
                match tryPickV mapping n.Left with
                | ValueNone ->
                    match mapping n.Value with
                    | ValueNone ->
                        tryPickV mapping n.Right
                    | res ->
                        res
                | res ->
                    res
                    
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
                let l = map mapping n.Left
                let v = mapping n.Value
                let r = map mapping n.Right
                Inner(n.Height, v, l, r, n.Count) :> Node<_>
                
        let rec mapi (i : int) (mapping : int -> 'a -> 'b) (n : Node<'a>) =
            if isNull n then null
            elif n.Height = 1uy then Node(1uy, mapping i n.Value)
            else
                let n = n :?> Inner<'a>
                let o = i + count n.Left
                let l = mapi i mapping n.Left
                let v = mapping o n.Value
                let r = mapi (o + 1) mapping n.Right
                Inner(n.Height, v, l, r, n.Count) :> Node<_>
                
        let rec iter (action : 'a -> unit) (n : Node<'a>) =
            if isNull n then
                ()
            elif n.Height = 1uy then
                action n.Value
            else
                let n = n :?> Inner<'a>
                iter action n.Left
                action n.Value
                iter action n.Right
     
        let rec iteri (offset : int) (action : int -> 'a -> unit) (n : Node<'a>) =
            if isNull n then
                ()
            elif n.Height = 1uy then
                action offset n.Value
            else
                let n = n :?> Inner<'a>
                let ro = offset + count n.Left
                iteri offset action n.Left
                action ro n.Value
                iteri (ro + 1) action n.Right

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

        let rec choosei (offset : int) (mapping : int -> 'a -> option<'b>) (n : Node<'a>) =
            if isNull n then null
            elif n.Height = 1uy then
                match mapping offset n.Value with
                | Some value -> Node(1uy, value)
                | None -> null
            else
                let n = n :?> Inner<'a>
                let ro = offset + count n.Left
                let l = choosei offset mapping n.Left
                let value = mapping ro n.Value
                let r = choosei (ro + 1) mapping n.Right
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

        let rec filteri (offset : int) (predicate : int -> 'a -> bool) (n : Node<'a>) =
            if isNull n then null
            elif n.Height = 1uy then
                if predicate offset n.Value then n
                else null
            else
                let n = n :?> Inner<'a>
                let ro = offset + count n.Left
                let l = filteri offset predicate n.Left
                let takeSelf = predicate ro n.Value
                let r = filteri (ro + 1) predicate n.Right
                if takeSelf then
                    binary l n.Value r
                else 
                    join l r

        let rec partition (predicate : 'a -> bool) (n : Node<'a>) =
            if isNull n then
                null, null
            elif n.Height = 1uy then
                if predicate n.Value then n, null
                else null, n
            else
                let n = n :?> Inner<'a>
                let lt, lf = partition predicate n.Left
                let takeSelf = predicate n.Value
                let rt, rf = partition predicate n.Right
                if takeSelf then
                    binary lt n.Value rt, join lf rf
                else 
                    join lt rt, binary lf n.Value rf

        
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
                
        let rec collecti (offset : int) (mapping : int -> 'a -> Node<'b>) (n : Node<'a>) =
            if isNull n then null
            elif n.Height = 1uy then
                mapping offset n.Value
            else
                let n = n :?> Inner<'a>
                let ro = offset + count n.Left
                let l = collecti offset mapping n.Left
                let s = mapping ro n.Value
                let r = collecti (ro + 1) mapping n.Right
                match count s with
                | 0 -> join l r
                | 1 -> binary l s.Value r
                | _ -> join3 l s r
                
        let rec concat (n : Node<Node<'a>>) =
            if isNull n then null
            elif n.Height = 1uy then
                n.Value
            else
                let n = n :?> Inner<Node<'a>>
                let l = concat n.Left
                let s = n.Value
                let r = concat n.Right
                match count s with
                | 0 -> join l r
                | 1 -> binary l s.Value r
                | _ -> join3 l s r
                  
        let rec map2 (mapping : 'a -> 'b -> 'c) (a : Node<'a>) (b : Node<'b>) =
            if isNull a then
                if isNull b then null
                else failwith "zipWith: different lengths"
            elif a.Height = 1uy then
                if b.Height = 1uy then Node(1uy, mapping a.Value b.Value)
                else failwith "zipWith: different lengths"
            else
                let a = a :?> Inner<'a>
                let b = b :?> Inner<'b>
                
                let ca = count a.Left
                let cb = count b.Left
                if ca = cb then
                    let l = map2 mapping a.Left b.Left
                    let v = mapping a.Value b.Value
                    let r = map2 mapping a.Right b.Right
                    Inner(a.Height, v, l, r, a.Count) :> Node<_>
                else
                    let bl, br = split ca b
                    let struct(v, br) = unsafeRemoveMin br
                    let l = map2 mapping a.Left bl
                    let v = mapping a.Value v
                    let r = map2 mapping a.Right br
                    Inner(a.Height, v, l, r, a.Count) :> Node<_>
               
               
              
        let inline combineHash (a: int) (b: int) =
            uint32 a ^^^ uint32 b + 0x9e3779b9u + ((uint32 a) <<< 6) + ((uint32 a) >>> 2) |> int

        let rec hash (valueHash : 'a -> int) (acc : int) (node : Node<'a>) =  
            if isNull node then
                acc
            elif node.Height = 1uy then
                combineHash acc (valueHash node.Value)
            else
                let node = node :?> Inner<'a>
                let a = hash valueHash acc node.Left
                let b = combineHash a (valueHash node.Value)
                hash valueHash b node.Right
                
                
        let rec equals (valueEquals : 'a -> 'a -> bool) (a : Node<'a>) (b : Node<'a>) =
            if count a <> count b then
                false
            else
                if isNull a then
                    if isNull b then true
                    else false
                elif a.Height = 1uy then
                    if b.Height = 1uy then valueEquals a.Value b.Value
                    else false
                else
                    let a = a :?> Inner<'a>
                    let b = b :?> Inner<'a>
                    let ca = count a.Left
                    let cb = count b.Left
                    if ca = cb then
                        valueEquals a.Value b.Value &&
                        equals valueEquals a.Left b.Left &&
                        equals valueEquals a.Right b.Right
                    else
                        let bl, br = split ca b
                        if isNull br then
                            false
                        else
                            let struct(v, br) = unsafeRemoveMin br
                            valueEquals a.Value v &&
                            equals valueEquals a.Left bl &&
                            equals valueEquals a.Right br
                    
open ArrNodeImplementation

/// A persitent array-like structure that allows lookup/insertion/deletion of entries in O(log N).
[<Struct; StructuredFormatDisplay("{AsString}"); DebuggerTypeProxy(typedefof<ArrProxy<_>>); CompiledName("FSharpArr`1"); CustomEquality; NoComparison>]
type arr<'a> internal(store : Node<'a>) =
    member internal x.Store = store
    
    /// Creates an empty arr.
    static member Empty : arr<'a> = arr<'a> null

    /// Creates an arr with a single element.
    static member Single(value : 'a) = arr(Node<'a>(1uy, value))
    
    /// Creates an arr from an array.
    static member FromArray(elements : 'a[]) : arr<'a> = arr (Node.ofArray elements 0 (elements.Length - 1))
    
    #if NET6_0_OR_GREATER
    
    /// Creates an arr from a ReadOnlySpan.
    static member FromSpan(elements : System.ReadOnlySpan<'a>) = arr (Node.ofSpan elements)
    
    /// Creates an arr from a Span.
    static member FromSpan(elements : System.Span<'a>) = arr (Node.ofSpan (System.Span.op_Implicit elements))
    
    /// Creates an arr from a Memory.
    static member FromMemory(elements : System.Memory<'a>) = arr (Node.ofSpan (System.Span.op_Implicit elements.Span))
    
    /// Creates an arr from a ReadOnlyMemory.
    static member FromMemory(elements : System.ReadOnlyMemory<'a>) = arr (Node.ofSpan elements.Span)
    #endif
    
    override x.GetHashCode() =
        Node.hash DefaultEquality.hash 0 store
    
    override x.Equals(o : obj) =
        match o with
        | :? arr<'a> as o -> Node.equals DefaultEquality.equals store o.Store
        | _ -> false
    
    /// Concatenates multiple arrs into a single arr.
    static member Concat([<ParamArray>] arrs : arr<'a>[]) =
        if arrs.Length = 0 then arr.Empty
        elif arrs.Length = 1 then arrs.[0]
        else
            arr.FromArray(arrs).Store
            |> Node.collect (fun (a : arr<'a>) -> a.Store)
            |> arr<'a>
        
    /// Is the arr empty?
    member x.IsEmpty = Node.isEmpty store
    
    /// The number of elements in the arr.
    member x.Length = Node.count store
    
    /// Creates a new arr with an inserted element at the specified index.
    member x.Insert(index : int, value : 'a) = arr(Node.insert index value store)
    
    /// Creates a new arr with an inserted element at the end.
    member x.Add(value : 'a) = arr(Node.append value store)
    
    /// Creates a new arr with an inserted element at the beginning.
    member x.Prepend(value : 'a) = arr(Node.prepend value store)
    
    /// Creates a new arr with an inserted element at the end.
    member x.Append(value : 'a) = arr(Node.append value store)
    
    
    /// Creates a new arr by skipping the first n elements.
    /// returns an empty arr if n is greater than the length of the arr.
    member x.Skip(n : int) =
        if n <= 0 then x
        elif n >= x.Length then arr.Empty
        else arr(Node.skip n store)
        
    /// Creates a new arr by taking the first n elements.
    /// returns the full arr if n is greater than the length of the arr.
    member x.Take(n : int) =
        if n <= 0 then arr.Empty
        elif n < x.Length then arr(Node.take n store)
        else x
        
    /// Splits the arr into two parts at the specified index.
    /// the two parts are equivalent to calling Take and Skip with the same index.
    member x.Split(n : int) =
        if n <= 0 then arr.Empty, x
        elif n >= x.Length then x, arr.Empty
        else
            let l, r = Node.split n store
            arr(l), arr(r)
        
    /// Creates a new arr by taking the elements in the given range.
    member x.Sub(offset : int, count : int) =
        arr(Node.sub offset (offset + count - 1) store)

    /// F# style slicing.
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
    
    /// Creates a new arr by removing the elements in the given range.
    member x.RemoveRange(offset : int, count : int) =
        arr(Node.removeRange offset (offset + count - 1) store)

    /// Creates a new arr by replacing the elements in the given range with the given elements.
    member x.ReplaceRange(offset : int, count : int, replacement : arr<'a>) =
        if count <= 0 then arr(Node.insertRange offset replacement.Store store)
        else arr(Node.replaceRange offset (offset + count - 1) replacement.Store store)
    
    /// Creates a new arr by replacing the elements in the given range with the given elements.
    member x.UpdateRange(offset : int, count : int, replacement : arr<'a> -> arr<'a>) =
        arr(Node.updateRange offset (offset + count - 1) (fun r -> replacement(arr r).Store) store)

    /// Copies the elements to a standard .NET array.
    member x.ToArray() =
        let res = Array.zeroCreate x.Length
        Node.copyTo 0 res store
        res

    /// Copies the elements to a standard F# list.
    member x.ToList() =
        Node.toList [] store

    /// Copies the elements to a standard .NET array at the specified index.
    member this.CopyTo(array,arrayIndex) =
        Node.copyTo arrayIndex array store
        
    /// pretty print for arr
    override x.ToString() =
        if Node.count store > 10 then Node.toArray (Node.take 10 store) |> Seq.map (sprintf "%A") |> String.concat "; " |> sprintf "arr [%s; ...]"
        else Node.toArray store |> Seq.map (sprintf "%A") |> String.concat "; " |> sprintf "arr [%s]"

    member private x.AsString = x.ToString()
    
    /// Standard enumerator for the arr.
    member x.GetEnumerator() = new ArrEnumerator<'a>(store)

    /// Creates a new arr by replacing the element at the specified index.
    member x.Set(index : int, value : 'a) =
        if index >= 0 && index < x.Length then
            arr(Node.unsafeSet index value store)
        else
            raise <| IndexOutOfRangeException()
            
    /// Gets the element at the specified index.
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
        member this.Add(_item) = failwith "readonly"
        member this.Clear() = failwith "readonly"
        member this.Contains(item) = Node.exists (Unchecked.equals item) store
        member this.CopyTo(array,arrayIndex) = this.CopyTo(array, arrayIndex)
        member this.Remove(_item) = failwith "readonly"
        member this.Count = this.Length
        member this.IsReadOnly = true
    
    interface System.Collections.Generic.IList<'a> with
        member this.Insert(_index,_item) = failwith "readonly"
        member this.RemoveAt(_index) = failwith "readonly"
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

/// Enumerator for arr.
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

/// Debugger proxy for arr.
and internal ArrProxy<'a>(arr : arr<'a>) =
    let items = arr.Take(10000).ToArray()

    [<DebuggerBrowsable(DebuggerBrowsableState.Collapsed)>]
    member x.Length = arr.Length
    
    [<DebuggerBrowsable(DebuggerBrowsableState.RootHidden)>]
    member x.Items = items

/// The Arr module contains functions for creating and manipulating the `arr` data structure.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module Arr =
    
    /// Empty arr.
    let inline empty<'a> : arr<'a> = arr.Empty
    
    /// Singleton arr.
    let inline single (value : 'a) = arr.Single(value)
    
    /// Creates an arr from a sequence.
    let inline ofSeq (seq : seq<'a>) =
        match seq with
        | :? arr<'a> as arr -> arr
        | _ -> arr.FromArray (Seq.toArray seq)
        
    /// Creates an arr from a list.
    let inline ofList (array : list<'a>) = arr.FromArray (List.toArray array)
    
    /// Creates an arr from an array.
    let inline ofArray (array : 'a[]) = arr.FromArray array
    
    #if NET6_0_OR_GREATER
    /// Creates an arr from a Span.
    let inline ofSpan (array : System.Span<'a>) = arr.FromSpan array
    
    /// Creates an arr from a Memory.
    let inline ofMemory (array : System.Memory<'a>) = arr.FromMemory array
    #endif
    
    /// Number of Elements in the arr.
    let inline length (a : arr<'a>) = a.Length

    /// Inserts an element at the specified index (s.t. `arr.[index] = value` after the operation).
    /// Note that the index must be in the range `[0, length arr]`.
    let inline insert (index : int) (value : 'a) (arr : arr<'a>) = arr.Insert(index, value)
    
    /// Appends an element to the end of the arr.
    let inline add (value : 'a) (arr : arr<'a>) = arr.Add value
    
    /// Appends an element to the end of the arr.
    let inline append (value : 'a) (arr : arr<'a>) = arr.Append value
    
    /// Inserts an element at the beginning of the arr.
    let inline prepend (value : 'a) (arr : arr<'a>) = arr.Prepend value
    
    /// Replaces the element at the specified index.
    let inline set (index : int) (value : 'a) (arr : arr<'a>) = arr.Set(index, value)

    /// seq representation of the arr.
    let inline toSeq (arr : arr<'a>) = arr :> seq<_>
    
    /// all elements from the arr.
    let inline toList (arr : arr<'a>) = arr.ToList()
    
    /// all elements from the arr.
    let inline toArray (arr : arr<'a>) = arr.ToArray()
    
    /// Creates a new arr by taking the first n elements.
    /// returns the full arr if n is greater than the length of the arr.
    let inline take (n : int) (arr : arr<'a>) = arr.Take n
    
    /// Creates a new arr by skipping the first n elements.
    /// returns an empty arr if n is greater than the length of the arr.
    let inline skip (n : int) (arr : arr<'a>) = arr.Skip n
    
    /// Creates a new arr by taking the elements in the given range.
    let inline sub (index : int) (count : int) (arr : arr<'a>) = arr.Sub(index, count)
    
    /// Splits the arr into two parts at the specified index.
    /// the two parts are equivalent to calling take and skip with the same index.
    let inline split (index : int) (arr : arr<'a>) = arr.Split(index)
    
    /// Concatenates multiple arrs into a single arr.
    let inline concat (arrs : seq<arr<'a>>) = arr.Concat(Seq.toArray arrs)
    
    /// removes the first element of an array and returns it together with the rest of the array.
    let uncons (array : arr<'a>) =
        if array.Length > 0 then
            let struct(value, rest) = Node.unsafeRemoveMin array.Store
            value, arr(rest)
        else
            raise <| IndexOutOfRangeException()
            
    /// removes the last element of an array and returns it together with the rest of the array.
    let unsnoc (array : arr<'a>) =
        if array.Length > 0 then
            let struct(value, rest) = Node.unsafeRemoveMax array.Store
            value, arr(rest)
        else
            raise <| IndexOutOfRangeException()

    /// creates a new arr by applying `mapping` to all elements.
    let map (mapping : 'a -> 'b) (array : arr<'a>) =
        array.Store |> Node.map mapping |> arr

    /// creates a new arr by applying `mapping` to all elements.
    let mapi (mapping : int -> 'a -> 'b) (array : arr<'a>) =
        array.Store |> Node.mapi 0 mapping |> arr

    /// creates a new arr by applying `mapping` and removing None values.
    let choose (mapping : 'a -> option<'b>) (array : arr<'a>) =
        array.Store |> Node.choose mapping |> arr

    /// creates a new arr by applying `mapping` and removing None values.
    let choosei (mapping : int -> 'a -> option<'b>) (array : arr<'a>) =
        array.Store |> Node.choosei 0 mapping |> arr

    /// filters the arr based on the given predicate.
    let filter (predicate : 'a -> bool) (array : arr<'a>) =
        array.Store |> Node.filter predicate |> arr

    /// filters the arr based on the given predicate.
    let filteri (predicate : int -> 'a -> bool) (array : arr<'a>) =
        array.Store |> Node.filteri 0 predicate |> arr

    let partition (predicate : 'a -> bool) (array : arr<'a>) =
        let l, r = array.Store |> Node.partition predicate
        arr(l), arr(r)
    
    /// creates a new arr by applying `mapping` to all elements and concerting the results.
    let collect (mapping : 'a -> arr<'b>) (array : arr<'a>) =
        array.Store |> Node.collect (fun v -> mapping(v).Store) |> arr
        
    /// creates a new arr by applying `mapping` to all elements and concerting the results.
    let collecti (mapping : int -> 'a -> arr<'b>) (array : arr<'a>) =
        array.Store |> Node.collecti 0 (fun i v -> (mapping i v).Store) |> arr
        
    /// iterates over the arr and invokes the action for each element.
    let iter (action : 'a -> unit) (array : arr<'a>) =
        Node.iter action array.Store
        
    /// iterates over the arr and invokes the action for each element.
    let iteri (action : int -> 'a -> unit) (array : arr<'a>) =
        Node.iteri 0 action array.Store
        
    /// Tests if any element in the arr satisfies the given predicate.
    let exists (predicate : 'a -> bool) (array : arr<'a>) =
        Node.exists predicate array.Store
        
    /// Tests if all elements in the arr satisfy the given predicate.
    let forall (predicate : 'a -> bool) (array : arr<'a>) =
        Node.forall predicate array.Store
        
    /// folds the arr from left to right.
    let fold (folder : 's -> 'a -> 's) (seed : 's) (array : arr<'a>) =
        Node.fold folder seed array.Store
        
    /// folds the arr from right to left.
    let foldBack (folder : 'a -> 's -> 's) (array : arr<'a>) (seed : 's) =
        Node.foldr folder array.Store seed
        
    let inline sum (array : arr< ^a >) =
        fold (+) LanguagePrimitives.GenericZero array
    
    let inline product (array : arr< ^a >) =
        fold (*) LanguagePrimitives.GenericOne array
    
    let inline average (array : arr< ^a >) =
        let s = fold (+) LanguagePrimitives.GenericZero array
        LanguagePrimitives.DivideByInt s array.Length
        
    let min< 'a when 'a : comparison> (array : arr<'a>) =
        let node = array.Store
        if isNull node then
            raise <| ArgumentException("The input array is empty.")
        elif node.Height = 1uy then
            node.Value
        else
            let s = Node.fold min node.Value node
            Node.fold min s node
            
    let max< 'a when 'a : comparison> (array : arr<'a>) =
        let node = array.Store
        if isNull node then
            raise <| ArgumentException("The input array is empty.")
        elif node.Height = 1uy then
            node.Value
        else
            let s = Node.fold max node.Value node
            Node.fold max s node
            
    let indexed (array : arr<'a>) =
        array |> mapi (fun i v -> (i, v))
    
    let init (count : int) (initializer : int -> 'a) =
        Node.init count 0 initializer |> arr
    
    let item (index : int) (arr : arr<'a>) =
        arr.[index]
    
    let tryItem (index : int) (arr : arr<'a>) =
        Node.tryAt index arr.Store
        
    let tryPick (mapping : 'a -> option<'b>) (arr : arr<'a>) =
        Node.tryPick mapping arr.Store
        
    let tryPickV (mapping : 'a -> voption<'b>) (arr : arr<'a>) =
        Node.tryPickV mapping arr.Store
        
    let tryFindIndex (predicate : 'a -> bool) (arr : arr<'a>) =
        Node.tryFindIndex predicate arr.Store
        
    let pairwise (array : arr<'a>) =
        let node, _ = Node.pairwise ValueNone array.Store
        arr(node)
        
    let pairwiseV (array : arr<'a>) =
        let node, _ = Node.mapPairwise ValueNone (fun a b -> struct(a, b)) array.Store
        arr(node)
    
    let rev (array : arr<'a>) =
        array.Store |> Node.reverse |> arr
        
    let zip (a : arr<'a>) (b : arr<'b>) =
        Node.map2 (fun a b -> a,b) a.Store b.Store |> arr
        
    let map2 (mapping : 'a -> 'b -> 'c) (a : arr<'a>) (b : arr<'b>) =
        Node.map2 mapping a.Store b.Store |> arr