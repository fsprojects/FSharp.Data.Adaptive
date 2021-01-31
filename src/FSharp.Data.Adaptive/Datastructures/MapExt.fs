namespace FSharp.Data.Adaptive

open System.Collections.Generic
open System.Runtime.InteropServices


module internal MapExtImplementation =
    module private Memory = 
        let mem() =
            let garbage() = 
                Array.init 10000 (fun i -> System.Guid.NewGuid()) |> ignore

            for i in 1 .. 10 do garbage()
            System.GC.Collect(3, System.GCCollectionMode.Forced, true, true)
            System.GC.WaitForFullGCComplete() |> ignore
            System.GC.GetTotalMemory(true)

        let bla(create : int -> 'A) =
            let cnt = 1000
            let arrayOverhead = 
                if typeof<'A>.IsValueType then 2L * int64 sizeof<nativeint>
                else (int64 cnt + 2L) * int64 sizeof<nativeint>

            let warmup() = Array.init cnt create |> ignore
            warmup()

            let before = mem()
            let arr = Array.init cnt create
            let diff = mem() - before - arrayOverhead
            let pseudo = float (Unchecked.hash arr % 2) - 0.5 |> int64
            float (diff + pseudo) / float cnt 
            
    let inline combineHash (a: int) (b: int) =
        uint32 a ^^^ uint32 b + 0x9e3779b9u + ((uint32 a) <<< 6) + ((uint32 a) >>> 2) |> int

    [<AllowNullLiteral; NoEquality; NoComparison>]
    type Node<'Key, 'Value> =
        val mutable public Height : byte
        val mutable public Key : 'Key
        val mutable public Value : 'Value
        new(k, v, h) = { Key = k; Value = v; Height = h }
        new(k, v) = { Key = k; Value = v; Height = 1uy }
        
    [<Sealed; AllowNullLiteral; NoEquality; NoComparison>]
    type Inner<'Key, 'Value> =
        inherit Node<'Key, 'Value>
        val mutable public Count : int
        val mutable public Left : Node<'Key, 'Value>
        val mutable public Right : Node<'Key, 'Value>
       
        static member inline GetCount(node : Node<'Key, 'Value>) =
            if isNull node then 0
            elif node.Height = 1uy then 1
            else (node :?> Inner<'Key, 'Value>).Count

        static member inline GetHeight(node : Node<'Key, 'Value>) =
            if isNull node then 0uy
            else node.Height

        static member inline FixHeightAndCount(inner : Inner<'Key, 'Value>) =
            let lc = Inner.GetCount inner.Left
            let rc = Inner.GetCount inner.Right
            let lh = if lc > 0 then inner.Left.Height else 0uy
            let rh = if rc > 0 then inner.Right.Height else 0uy
            inner.Count <- 1 + lc + rc
            inner.Height <- 1uy + max lh rh

        new(l : Node<'Key, 'Value>, k : 'Key, v : 'Value, r : Node<'Key, 'Value>, h : byte, cnt : int) =
            { inherit  Node<'Key, 'Value>(k, v, h); Left = l; Right = r; Count = cnt }

        static member Create(l : Node<'Key, 'Value>, k : 'Key, v : 'Value, r : Node<'Key, 'Value>) =
            if isNull l && isNull r then Node(k, v)
            else 
                let lc = Inner.GetCount l
                let rc = Inner.GetCount r
                let lh = if lc > 0 then l.Height else 0uy
                let rh = if rc > 0 then r.Height else 0uy
                Inner(l, k, v, r, 1uy + max lh rh, 1 + lc + rc) :> Node<_,_>
               
        
    let inline height (n : Node<'Key, 'Value>) =
        if isNull n then 0uy
        else n.Height
            
    let rec count (n : Node<'Key, 'Value>) =
        if isNull n then 0
        elif n.Height = 1uy then 1
        else (n :?> Inner<'Key, 'Value>).Count
            
    let inline balance (n : Inner<'Key, 'Value>) =
        int (height n.Right) - int (height n.Left)

    let unsafeBinary (l : Node<'Key, 'Value>) (k : 'Key) (v : 'Value) (r : Node<'Key, 'Value>) =
        let lc = Inner.GetCount l
        let rc = Inner.GetCount r
        let lh = if lc > 0 then l.Height else 0uy
        let rh = if rc > 0 then r.Height else 0uy

        let b = int rh - int lh
        if b > 2 then
            // rh > lh + 2
            let r = r :?> Inner<'Key, 'Value>
            let rb = balance r
            if rb > 0 then
                // right right 
                Inner.Create( 
                    Inner.Create(l, k, v, r.Left),
                    r.Key, r.Value,
                    r.Right
                )
            else
                // right left
                let rl = r.Left :?> Inner<'Key, 'Value>
                Inner.Create( 
                    Inner.Create(l, k, v, rl.Left),
                    rl.Key, rl.Value,
                    Inner.Create(rl.Right, r.Key, r.Value, r.Right)
                )

        elif b < -2 then
            // lh > rh + 2
            let l = l :?> Inner<'Key, 'Value>
            let lb = balance l
            if lb < 0 then
                // left left
                Inner.Create(
                    l.Left,
                    l.Key, l.Value,
                    Inner.Create(l.Right, k, v, r)
                )
            else
                // left right
                let lr = l.Right :?> Inner<'Key, 'Value>
                Inner.Create(
                    Inner.Create(l.Left, l.Key, l.Value, lr.Left),
                    lr.Key, lr.Value,
                    Inner.Create(lr.Right, k, v, r)
                )

        elif lh = 0uy && rh = 0uy then Node(k, v)
        else Inner(l, k, v, r, 1uy + max lh rh, 1 + lc + rc) :> Node<_,_>

    let rec unsafeRemoveMin (key : byref<'Key>) (value : byref<'Value>) (n : Node<'Key, 'Value>) =
        if n.Height = 1uy then
            key <- n.Key
            value <- n.Value
            null
        else
            let n = n :?> Inner<'Key, 'Value>
            if isNull n.Left then
                key <- n.Key
                value <- n.Value
                n.Right
            else
                let newLeft = unsafeRemoveMin &key &value n.Left
                unsafeBinary newLeft n.Key n.Value n.Right
                    
    let rec unsafeRemoveMax (key : byref<'Key>) (value : byref<'Value>) (n : Node<'Key, 'Value>) =
        if n.Height = 1uy then
            key <- n.Key
            value <- n.Value
            null
        else
            let n = n :?> Inner<'Key, 'Value>
            if isNull n.Right then
                key <- n.Key
                value <- n.Value
                n.Left
            else
                let newRight = unsafeRemoveMax &key &value n.Right
                unsafeBinary n.Left n.Key n.Value newRight


    let rebalanceUnsafe (node : Inner<'Key, 'Value>) =
        let lh = height node.Left
        let rh = height node.Right
        let b = int rh - int lh
        if b > 2 then
            let r = node.Right :?> Inner<'Key, 'Value>
            let br = balance r
            if br >= 0 then
                // right right
                //     (k01, v01)                           (k12,v12)
                //    t0        (k12,v12)      =>      (k01, v01)    t2
                //             t1       t2            t0        t1

                let t0 = node.Left
                let k01 = node.Key
                let v01 = node.Value
                let t1 = r.Left
                let k12 = r.Key
                let v12 = r.Value
                let t2 = r.Right

                r.Key <- k01
                r.Value <- v01
                r.Left <- t0
                r.Right <- t1
                Inner.FixHeightAndCount r

                node.Key <- k12
                node.Value <- v12
                node.Left <- r
                node.Right <- t2
                node.Count <- 1 + r.Count + (count t2)
                node.Height <- 1uy + max r.Height (height t2)

            else
                let rl = r.Left :?> Inner<'Key, 'Value>
                // right left
                //     (k01, v01)                             (k12,v12)
                //    t0        (k23,v23)      =>      (k01,v01)     (k23,v23)
                //        (k12,v12)       t3         t0        t1   t2       t3
                //       t1       t2

                let t0 = node.Left
                let k01 = node.Key
                let v01 = node.Value
                let t1 = rl.Left
                let k12 = rl.Key
                let v12 = rl.Value
                let t2 = rl.Right
                let k23 = r.Key
                let v23 = r.Value
                let t3 = r.Right

                let a = rl
                let b = r

                a.Key <- k01
                a.Value <- v01
                a.Left <- t0
                a.Right <- t1
                Inner.FixHeightAndCount a

                b.Key <- k23
                b.Value <- v23
                b.Left <- t2
                b.Right <- t3
                Inner.FixHeightAndCount b
                
                node.Key <- k12
                node.Value <- v12
                node.Left <- a
                node.Right <- b
                node.Count <- 1 + a.Count + b.Count
                node.Height <- 1uy + max a.Height b.Height

        elif b < -2 then
            let l = node.Left :?> Inner<'Key, 'Value>
            let bl = balance l
            if bl <= 0 then
                // left left
                //         (k12, v12)                   (k01,v01)
                //    (k01,v01)       t2    =>         t0       (k12,v12)
                //  t0        t1                               t1       t2

                let t0 = l.Left
                let k01 = l.Key
                let v01 = l.Value
                let t1 = l.Right
                let k12 = node.Key
                let v12 = node.Value
                let t2 = node.Right

                let a = l

                a.Key <- k12
                a.Value <- v12
                a.Left <- t1
                a.Right <- t2
                Inner.FixHeightAndCount a

                node.Key <- k01
                node.Value <- v01
                node.Left <- t0
                node.Right <- a
                node.Count <- 1 + (count t0) + a.Count
                node.Height <- 1uy + max (height t0) a.Height

            else
                let lr = l.Right :?> Inner<'Key, 'Value>
                // left right
                //            (k23, v23)                         (k12,v12)
                //    (k01,v01)         t3    =>         (k01,v01)       (k23,v23)
                //  t0      (k12,v12)                   t0       t1     t2      t3
                //         t1       t2

                let t0 = l.Left
                let k01 = l.Key
                let v01 = l.Value
                let t1 = lr.Left
                let k12 = lr.Key
                let v12 = lr.Value
                let t2 = lr.Right
                let k23 = node.Key
                let v23 = node.Value
                let t3 = node.Right

                let a = l
                let b = lr

                a.Key <- k01
                a.Value <- v01
                a.Left <- t0
                a.Right <- t1
                Inner.FixHeightAndCount a

                b.Key <- k23
                b.Value <- v23
                b.Left <- t2
                b.Right <- t3
                Inner.FixHeightAndCount b


                node.Key <- k12
                node.Value <- v12
                node.Left <- a
                node.Right <- b
                node.Count <- 1 + a.Count + b.Count
                node.Height <- 1uy + max a.Height b.Height
        else
            Inner.FixHeightAndCount node
            


    // abs (balance l r) <= 3 (as caused by add/remove)
    let unsafeJoin (l : Node<'Key, 'Value>) (r : Node<'Key, 'Value>) : Node<'Key, 'Value> =
        if isNull l then r
        elif isNull r then l
        else
            let lc = l.Height
            let rc = r.Height
            let mutable k = Unchecked.defaultof<_>
            let mutable v = Unchecked.defaultof<_>
            if lc > rc then
                let ln = unsafeRemoveMax &k &v l
                unsafeBinary ln k v r
            else
                let rn = unsafeRemoveMin &k &v r
                unsafeBinary l k v rn
                
    let rec binary (l : Node<'Key, 'Value>) (k : 'Key) (v : 'Value) (r : Node<'Key, 'Value>) =
        let lc = Inner.GetCount l
        let rc = Inner.GetCount r
        let lh = if lc > 0 then l.Height else 0uy
        let rh = if rc > 0 then r.Height else 0uy

        let b = int rh - int lh
        if b > 2 then
            // rh > lh + 2
            let r = r :?> Inner<'Key, 'Value>
            let rb = balance r
            if rb > 0 then
                // right right 
                binary 
                    (binary l k v r.Left)
                    r.Key r.Value
                    r.Right
            else
                // right left
                let rl = r.Left :?> Inner<'Key, 'Value>
                binary
                    (binary l k v rl.Left)
                    rl.Key rl.Value
                    (binary rl.Right r.Key r.Value r.Right)

        elif b < -2 then
            // lh > rh + 2
            let l = l :?> Inner<'Key, 'Value>
            let lb = balance l
            if lb < 0 then
                // left left
                binary 
                    l.Left
                    l.Key l.Value
                    (binary l.Right k v r)
            else
                // left right
                let lr = l.Right :?> Inner<'Key, 'Value>
                binary 
                    (binary l.Left l.Key l.Value lr.Left)
                    lr.Key lr.Value
                    (binary lr.Right k v r)

        elif lh = 0uy && rh = 0uy then Node(k, v)
        else Inner(l, k, v, r, 1uy + max lh rh, 1 + lc + rc) :> Node<_,_>
        
    let rec join (l : Node<'Key, 'Value>) (r : Node<'Key, 'Value>) : Node<'Key, 'Value> =
        if isNull l then r
        elif isNull r then l
        else
            let lh = l.Height
            let rh = r.Height
            let mutable k = Unchecked.defaultof<_>
            let mutable v = Unchecked.defaultof<_>
            if lh > rh then
                let ln = unsafeRemoveMax &k &v l
                binary ln k v r
            else
                let rn = unsafeRemoveMin &k &v r
                binary l k v rn

    let rec find (cmp : IComparer<'Key>) (key : 'Key) (node : Node<'Key, 'Value>) =
        if isNull node then
            raise <| KeyNotFoundException()
        elif node.Height = 1uy then
            if cmp.Compare(key, node.Key) = 0 then node.Value
            else raise <| KeyNotFoundException()
        else
            let node = node :?> Inner<'Key, 'Value>
            let c = cmp.Compare(key, node.Key)
            if c > 0 then find cmp key node.Right
            elif c < 0 then find cmp key node.Left
            else node.Value
            
    let rec tryGetValue (cmp : IComparer<'Key>) (key : 'Key) (result : outref<'Value>) (node : Node<'Key, 'Value>) =
        if isNull node then
            false
        elif node.Height = 1uy then
            if cmp.Compare(key, node.Key) = 0 then 
                result <- node.Value
                true
            else 
                false
        else
            let node = node :?> Inner<'Key, 'Value>
            let c = cmp.Compare(key, node.Key)
            if c > 0 then tryGetValue cmp key &result node.Right
            elif c < 0 then tryGetValue cmp key &result node.Left
            else 
                result <- node.Value
                true

    let rec tryFind (cmp : IComparer<'Key>) (key : 'Key) (node : Node<'Key, 'Value>) =
        if isNull node then
            None
        elif node.Height = 1uy then
            if cmp.Compare(key, node.Key) = 0 then Some node.Value
            else None
        else
            let node = node :?> Inner<'Key, 'Value>
            let c = cmp.Compare(key, node.Key)
            if c > 0 then tryFind cmp key node.Right
            elif c < 0 then tryFind cmp key node.Left
            else Some node.Value
            
    let rec tryGetItem (index : int) (key : byref<'Key>) (value : byref<'Value>) (node : Node<'Key, 'Value>) =
        if isNull node then
            false
        elif node.Height = 1uy then
            if index = 0 then 
                key <- node.Key
                value <- node.Value
                true
            else
                false
        else
            let node = node :?> Inner<'Key, 'Value>
            let id = index - count node.Left
            if id > 0 then
                tryGetItem (id - 1) &key &value node.Right
            elif id < 0 then
                tryGetItem index &key &value node.Left
            else
                key <- node.Key
                value <- node.Value
                true

    let rec tryGetIndex (cmp : IComparer<'Key>) (key : 'Key) (offset : int) (node : Node<'Key, 'Value>) =
        if isNull node then 
            -1
        elif node.Height = 1uy then
            let c = cmp.Compare(key, node.Key)
            if c = 0 then offset
            else -1
        else
            let node = node :?> Inner<'Key, 'Value>
            let c = cmp.Compare(key, node.Key)
            if c > 0 then
                tryGetIndex cmp key (offset + count node.Left + 1) node.Right
            elif c < 0 then
                tryGetIndex cmp key offset node.Left
            else
                offset + count node.Left

    let rec tryGetMin (minKey : byref<'Key>) (minValue : byref<'Value>) (node : Node<'Key, 'Value>) =
        if isNull node then
            false
        elif node.Height = 1uy then
            minKey <- node.Key
            minValue <- node.Value
            true
        else
            let node = node :?> Inner<'Key, 'Value>
            if isNull node.Left then 
                minKey <- node.Key
                minValue <- node.Value
                true
            else
                tryGetMin &minKey &minValue node.Left

    let rec tryGetMax (maxKey : byref<'Key>) (maxValue : byref<'Value>) (node : Node<'Key, 'Value>) =
        if isNull node then
            false
        elif node.Height = 1uy then
            maxKey <- node.Key
            maxValue <- node.Value
            true
        else
            let node = node :?> Inner<'Key, 'Value>
            if isNull node.Right then
                maxKey <- node.Key
                maxValue <- node.Value
                true
            else
                tryGetMax &maxKey &maxValue node.Right


    let rec containsKey (cmp : IComparer<'Key>) (key : 'Key) (node : Node<'Key, 'Value>) =
        if isNull node then
            false
        elif node.Height = 1uy then
            cmp.Compare(key, node.Key) = 0
        else
            let node = node :?> Inner<'Key, 'Value>
            let c = cmp.Compare(key, node.Key)
            if c > 0 then containsKey cmp key node.Right
            elif c < 0 then containsKey cmp key node.Left
            else true

    let rec addIfNotPresent (cmp : IComparer<'Key>) (key : 'Key) (value : 'Value) (node : Node<'Key, 'Value>) =
        if isNull node then
            // empty
            Node(key, value)

        elif node.Height = 1uy then
            // leaf
            let c = cmp.Compare(key, node.Key)
            if c > 0 then Inner(node, key, value, null, 2uy, 2) :> Node<_,_>
            elif c < 0 then Inner(null, key, value, node, 2uy, 2) :> Node<_,_>
            else node

        else
            // inner
            let n = node :?> Inner<'Key, 'Value>
            let c = cmp.Compare(key, n.Key)
            if c > 0 then
                unsafeBinary n.Left n.Key n.Value (addIfNotPresent cmp key value n.Right)
            elif c < 0 then
                unsafeBinary (addIfNotPresent cmp key value n.Left) n.Key n.Value n.Right
            else    
                node

    let rec add (cmp : IComparer<'Key>) (key : 'Key) (value : 'Value) (node : Node<'Key, 'Value>) =
        if isNull node then
            // empty
            Node(key, value)

        elif node.Height = 1uy then
            // leaf
            let c = cmp.Compare(key, node.Key)
            if c > 0 then Inner(node, key, value, null, 2uy, 2) :> Node<_,_>
            elif c < 0 then Inner(null, key, value, node, 2uy, 2) :> Node<_,_>
            else Node(key, value)

        else
            // inner
            let n = node :?> Inner<'Key, 'Value>
            let c = cmp.Compare(key, n.Key)
            if c > 0 then
                unsafeBinary n.Left n.Key n.Value (add cmp key value n.Right)
            elif c < 0 then
                unsafeBinary (add cmp key value n.Left) n.Key n.Value n.Right
            else    
                Inner(n.Left, key, value, n.Right, n.Height, n.Count) :> Node<_,_>
          
    let rec unsafeAddMinimum (key : 'Key) (value : 'Value) (node : Node<'Key, 'Value>) =
        if isNull node then
            // empty
            Node(key, value)

        elif node.Height = 1uy then
            // leaf
            Inner(null, key, value, node, 2uy, 2) :> Node<_,_>

        else
            // inner
            let n = node :?> Inner<'Key, 'Value>
            unsafeBinary (unsafeAddMinimum key value n.Left) n.Key n.Value n.Right
          
    let rec unsafeAddMaximum (key : 'Key) (value : 'Value) (node : Node<'Key, 'Value>) =
        if isNull node then
            // empty
            Node(key, value)

        elif node.Height = 1uy then
            // leaf
            Inner(node, key, value, null, 2uy, 2) :> Node<_,_>

        else
            // inner
            let n = node :?> Inner<'Key, 'Value>
            unsafeBinary n.Left n.Key n.Value (unsafeAddMaximum key value n.Right)
          
          
    let rec addInPlace (cmp : IComparer<'Key>) (key : 'Key) (value : 'Value) (node : Node<'Key, 'Value>) =
        if isNull node then
            // empty
            Node(key, value)

        elif node.Height = 1uy then
            // leaf
            let c = cmp.Compare(key, node.Key)
            if c > 0 then Inner(node, key, value, null, 2uy, 2) :> Node<_,_>
            elif c < 0 then Inner(null, key, value, node, 2uy, 2) :> Node<_,_>
            else 
                node.Key <- key
                node.Value <- value
                node

        else
            // inner
            let n = node :?> Inner<'Key, 'Value>
            let c = cmp.Compare(key, n.Key)
            if c > 0 then
                n.Right <- addInPlace cmp key value n.Right
                rebalanceUnsafe n
                node

            elif c < 0 then
                n.Left <- addInPlace cmp key value n.Left
                rebalanceUnsafe n
                node

            else
                n.Key <- key
                n.Value <- value
                node
     
             
    let rec tryRemove' (cmp : IComparer<'Key>) (key : 'Key)  (result : byref<Node<'Key, 'Value>>) (node : Node<'Key, 'Value>) =
        if isNull node then 
            false
        elif node.Height = 1uy then
            let c = cmp.Compare(key, node.Key)
            if c = 0 then 
                result <- null
                true
            else
                false
        else
            let node = node :?> Inner<'Key, 'Value>
            let c = cmp.Compare(key, node.Key)
            if c > 0 then 
                if tryRemove' cmp key &result node.Right then
                    result <- unsafeBinary node.Left node.Key node.Value result
                    true
                else
                    false
            elif c < 0 then 
                if tryRemove' cmp key &result node.Left then
                    result <- unsafeBinary result node.Key node.Value node.Right
                    true
                else
                    false
            else 
                result <- unsafeJoin node.Left node.Right
                true
       
    let rec tryRemove (cmp : IComparer<'Key>) (key : 'Key) (removedValue : byref<'Value>) (result : byref<Node<'Key, 'Value>>) (node : Node<'Key, 'Value>) =
        if isNull node then 
            false
        elif node.Height = 1uy then
            let c = cmp.Compare(key, node.Key)
            if c = 0 then 
                removedValue <- node.Value
                result <- null
                true
            else
                false
        else
            let node = node :?> Inner<'Key, 'Value>
            let c = cmp.Compare(key, node.Key)
            if c > 0 then 
                if tryRemove cmp key &removedValue &result node.Right then
                    result <- unsafeBinary node.Left node.Key node.Value result
                    true
                else
                    false
            elif c < 0 then 
                if tryRemove cmp key &removedValue &result node.Left then
                    result <- unsafeBinary result node.Key node.Value node.Right
                    true
                else
                    false
            else 
                result <- unsafeJoin node.Left node.Right
                removedValue <- node.Value
                true
     
     
    let rec removeAt (index : int) (removedKey : byref<'Key>) (removedValue : byref<'Value>) (node : Node<'Key, 'Value>) =
        if isNull node then 
            null
        elif node.Height = 1uy then
            if index = 0 then
                removedKey <- node.Key
                removedValue <- node.Value
                null
            else
                node
        else
            let node = node :?> Inner<'Key, 'Value>
            let id = index - count node.Left
            if id > 0 then 
                let result = removeAt (id - 1) &removedKey &removedValue node.Right
                unsafeBinary node.Left node.Key node.Value result
            elif id < 0 then 
                let result = removeAt index &removedKey &removedValue node.Left
                unsafeBinary result node.Key node.Value node.Right
            else 
                removedKey <- node.Key
                removedValue <- node.Value
                unsafeJoin node.Left node.Right
     

    let rec changeV (cmp : IComparer<'Key>) (key : 'Key) (update : voption<'Value> -> voption<'Value>) (node : Node<'Key, 'Value>) =
        if isNull node then
            match update ValueNone with
            | ValueNone -> null
            | ValueSome v -> Node(key, v)

        elif node.Height = 1uy then
            let c = cmp.Compare(key, node.Key)
            if c > 0 then
                match update ValueNone with
                | ValueNone -> node
                | ValueSome n ->
                    Inner(node, key, n, null, 2uy, 2) :> Node<_,_>
            elif c < 0 then
                match update ValueNone with
                | ValueNone -> node
                | ValueSome n ->
                    Inner(null, key, n, node, 2uy, 2) :> Node<_,_>
            else
                match update (ValueSome node.Value) with
                | ValueNone -> null
                | ValueSome v -> Node(key, v)
        else    
            let node = node :?> Inner<'Key, 'Value>
            let c = cmp.Compare(key, node.Key)
            if c > 0 then
                unsafeBinary node.Left node.Key node.Value (changeV cmp key update node.Right)
            elif c < 0 then
                unsafeBinary (changeV cmp key update node.Left) node.Key node.Value node.Right
            else
                match update (ValueSome node.Value) with
                | ValueSome n ->
                    Inner(node.Left, key, n, node.Right, node.Height, node.Count) :> Node<_,_>
                | ValueNone ->
                    unsafeJoin node.Left node.Right

    let rec change (cmp : IComparer<'Key>) (key : 'Key) (update : option<'Value> -> option<'Value>) (node : Node<'Key, 'Value>) =
        if isNull node then
            match update None with
            | None -> null
            | Some v -> Node(key, v)

        elif node.Height = 1uy then
            let c = cmp.Compare(key, node.Key)
            if c > 0 then
                match update None with
                | None -> node
                | Some n ->
                    Inner(node, key, n, null, 2uy, 2) :> Node<_,_>
            elif c < 0 then
                match update None with
                | None -> node
                | Some n ->
                    Inner(null, key, n, node, 2uy, 2) :> Node<_,_>
            else
                match update (Some node.Value) with
                | None -> null
                | Some v -> Node(key, v)
        else    
            let node = node :?> Inner<'Key, 'Value>
            let c = cmp.Compare(key, node.Key)
            if c > 0 then
                unsafeBinary node.Left node.Key node.Value (change cmp key update node.Right)
            elif c < 0 then
                unsafeBinary (change cmp key update node.Left) node.Key node.Value node.Right
            else
                match update (Some node.Value) with
                | Some n ->
                    Inner(node.Left, key, n, node.Right, node.Height, node.Count) :> Node<_,_>
                | None ->
                    unsafeJoin node.Left node.Right


    let rec copyToV (array : struct('Key * 'Value)[]) (index : int) (node : Node<'Key, 'Value>) =
        if isNull node then
            index
        elif node.Height = 1uy then
            array.[index] <- struct(node.Key, node.Value)
            index + 1
        else
            let node = node :?> Inner<'Key, 'Value>
            let i1 = copyToV array index node.Left
            array.[i1] <- struct(node.Key, node.Value)
            copyToV array (i1 + 1) node.Right
            
    let rec copyTo (array : ('Key * 'Value)[]) (index : int) (node : Node<'Key, 'Value>) =
        if isNull node then
            index
        elif node.Height = 1uy then
            array.[index] <- (node.Key, node.Value)
            index + 1
        else
            let node = node :?> Inner<'Key, 'Value>
            let i1 = copyTo array index node.Left
            array.[i1] <- (node.Key, node.Value)
            copyTo array (i1 + 1) node.Right
            
    let rec copyValuesTo (array : 'Value[]) (index : int) (node : Node<'Key, 'Value>) =
        if isNull node then
            index
        elif node.Height = 1uy then
            array.[index] <- node.Value
            index + 1
        else
            let node = node :?> Inner<'Key, 'Value>
            let i1 = copyValuesTo array index node.Left
            array.[i1] <- node.Value
            copyValuesTo array (i1 + 1) node.Right
            
    let rec copyKeysTo (array : 'Key[]) (index : int) (node : Node<'Key, 'Value>) =
        if isNull node then
            index
        elif node.Height = 1uy then
            array.[index] <- node.Key
            index + 1
        else
            let node = node :?> Inner<'Key, 'Value>
            let i1 = copyKeysTo array index node.Left
            array.[i1] <- node.Key
            copyKeysTo array (i1 + 1) node.Right

    let rec toListV (acc : list<struct('Key * 'Value)>) (node : Node<'Key, 'Value>) =
        if isNull node then acc
        elif node.Height = 1uy then
            struct(node.Key, node.Value) :: acc
        else
            let node = node :?> Inner<'Key, 'Value>
            toListV (struct(node.Key, node.Value) :: toListV acc node.Right) node.Left
                
    let rec toList (acc : list<'Key * 'Value>) (node : Node<'Key, 'Value>) =
        if isNull node then acc
        elif node.Height = 1uy then
            (node.Key, node.Value) :: acc
        else
            let node = node :?> Inner<'Key, 'Value>
            toList ((node.Key, node.Value) :: toList acc node.Right) node.Left
            
    let rec toValueList (acc : list<'Value>) (node : Node<'Key, 'Value>) =
        if isNull node then acc
        elif node.Height = 1uy then
            node.Value :: acc
        else
            let node = node :?> Inner<'Key, 'Value>
            toValueList (node.Value :: toValueList acc node.Right) node.Left
            
    let rec toKeyList (acc : list<'Key>) (node : Node<'Key, 'Value>) =
        if isNull node then acc
        elif node.Height = 1uy then
            node.Key :: acc
        else
            let node = node :?> Inner<'Key, 'Value>
            toKeyList (node.Key :: toKeyList acc node.Right) node.Left
  


    let rec copyToBackwardV (array : struct('Key * 'Value)[]) (index : int) (node : Node<'Key, 'Value>) =
        if isNull node then
            index
        elif node.Height = 1uy then
            array.[index] <- struct(node.Key, node.Value)
            index + 1
        else
            let node = node :?> Inner<'Key, 'Value>
            let i1 = copyToBackwardV array index node.Right
            array.[i1] <- struct(node.Key, node.Value)
            copyToBackwardV array (i1 + 1) node.Left
            
    let rec copyToBackward (array : ('Key * 'Value)[]) (index : int) (node : Node<'Key, 'Value>) =
        if isNull node then
            index
        elif node.Height = 1uy then
            array.[index] <- (node.Key, node.Value)
            index + 1
        else
            let node = node :?> Inner<'Key, 'Value>
            let i1 = copyToBackward array index node.Right
            array.[i1] <- (node.Key, node.Value)
            copyToBackward array (i1 + 1) node.Left
            
    let rec copyValuesBackwardTo (array : 'Value[]) (index : int) (node : Node<'Key, 'Value>) =
        if isNull node then
            index
        elif node.Height = 1uy then
            array.[index] <- node.Value
            index + 1
        else
            let node = node :?> Inner<'Key, 'Value>
            let i1 = copyValuesBackwardTo array index node.Right
            array.[i1] <- node.Value
            copyValuesBackwardTo array (i1 + 1) node.Left
            
    let rec copyKeysBackwardTo (array : 'Key[]) (index : int) (node : Node<'Key, 'Value>) =
        if isNull node then
            index
        elif node.Height = 1uy then
            array.[index] <- node.Key
            index + 1
        else
            let node = node :?> Inner<'Key, 'Value>
            let i1 = copyKeysBackwardTo array index node.Right
            array.[i1] <- node.Key
            copyKeysBackwardTo array (i1 + 1) node.Left



    let rec toListBackV (acc : list<struct('Key * 'Value)>) (node : Node<'Key, 'Value>) =
        if isNull node then acc
        elif node.Height = 1uy then
            struct(node.Key, node.Value) :: acc
        else
            let node = node :?> Inner<'Key, 'Value>
            toListBackV (struct(node.Key, node.Value) :: toListBackV acc node.Left) node.Right
                
    let rec toListBack (acc : list<'Key * 'Value>) (node : Node<'Key, 'Value>) =
        if isNull node then acc
        elif node.Height = 1uy then
            (node.Key, node.Value) :: acc
        else
            let node = node :?> Inner<'Key, 'Value>
            toListBack ((node.Key, node.Value) :: toListBack acc node.Left) node.Right
            
    let rec toValueListBack (acc : list<'Value>) (node : Node<'Key, 'Value>) =
        if isNull node then acc
        elif node.Height = 1uy then
            node.Value :: acc
        else
            let node = node :?> Inner<'Key, 'Value>
            toValueListBack (node.Value :: toValueListBack acc node.Left) node.Right
            
    let rec toKeyListBack (acc : list<'Key>) (node : Node<'Key, 'Value>) =
        if isNull node then acc
        elif node.Height = 1uy then
            node.Key :: acc
        else
            let node = node :?> Inner<'Key, 'Value>
            toKeyListBack (node.Key :: toKeyListBack acc node.Left) node.Right


    let rec iter (action : OptimizedClosures.FSharpFunc<'Key, 'Value, unit>) (node : Node<'Key, 'Value>) =
        if isNull node then
            ()
        elif node.Height = 1uy then
            action.Invoke(node.Key, node.Value)
        else
            let node = node :?> Inner<'Key, 'Value>
            iter action node.Left
            action.Invoke(node.Key, node.Value)
            iter action node.Right
            
    let rec iterValue (action : 'Value -> unit) (node : Node<'Key, 'Value>) =
        if isNull node then
            ()
        elif node.Height = 1uy then
            action node.Value
        else
            let node = node :?> Inner<'Key, 'Value>
            iterValue action node.Left
            action node.Value
            iterValue action node.Right

    let rec map (mapping : OptimizedClosures.FSharpFunc<'Key, 'Value, 'T>) (node : Node<'Key, 'Value>) =
        if isNull node then
            null
        elif node.Height = 1uy then
            Node(node.Key, mapping.Invoke(node.Key, node.Value))
        else
            let node = node :?> Inner<'Key, 'Value>
            let l = map mapping node.Left
            let s = mapping.Invoke(node.Key, node.Value)
            let r = map mapping node.Right
            Inner(l, node.Key, s, r, node.Height, node.Count) :> Node<_,_>
            
    let rec mapMonotonic (mapping : OptimizedClosures.FSharpFunc<'Key, 'Value, struct('K * 'T)>) (node : Node<'Key, 'Value>) =
        if isNull node then
            null
        elif node.Height = 1uy then
            let struct(k, v) = mapping.Invoke(node.Key, node.Value)
            Node(k, v)
        else
            let node = node :?> Inner<'Key, 'Value>
            let l = mapMonotonic mapping node.Left
            let struct(k, v) = mapping.Invoke(node.Key, node.Value)
            let r = mapMonotonic mapping node.Right
            Inner(l, k, v, r, node.Height, node.Count) :> Node<_,_>
            
    let rec chooseV (mapping : OptimizedClosures.FSharpFunc<'Key, 'Value, voption<'T>>) (node : Node<'Key, 'Value>) =
        if isNull node then
            null
        elif node.Height = 1uy then
            match mapping.Invoke(node.Key, node.Value) with
            | ValueSome v -> Node(node.Key, v)
            | ValueNone -> null
        else
            let node = node :?> Inner<'Key, 'Value>
            let l = chooseV mapping node.Left
            let s = mapping.Invoke(node.Key, node.Value)
            let r = chooseV mapping node.Right
            match s with
            | ValueSome s -> binary l node.Key s r
            | ValueNone -> join l r

    let rec choose (mapping : OptimizedClosures.FSharpFunc<'Key, 'Value, option<'T>>) (node : Node<'Key, 'Value>) =
        if isNull node then
            null
        elif node.Height = 1uy then
            match mapping.Invoke(node.Key, node.Value) with
            | Some v -> Node(node.Key, v)
            | None -> null
        else
            let node = node :?> Inner<'Key, 'Value>
            let l = choose mapping node.Left
            let s = mapping.Invoke(node.Key, node.Value)
            let r = choose mapping node.Right
            match s with
            | Some s -> binary l node.Key s r
            | None -> join l r

    let rec filter (predicate : OptimizedClosures.FSharpFunc<'Key, 'Value, bool>) (node : Node<'Key, 'Value>) =
        if isNull node then
            null
        elif node.Height = 1uy then
            if predicate.Invoke(node.Key, node.Value) then
                node
            else
                null
        else
            let node = node :?> Inner<'Key, 'Value>
            let l = filter predicate node.Left
            let s = predicate.Invoke(node.Key, node.Value)
            let r = filter predicate node.Right
            if s then binary l node.Key node.Value r
            else join l r
            
    let rec tryPickBack (mapping : OptimizedClosures.FSharpFunc<'Key, 'Value, option<'T>>) (node : Node<'Key, 'Value>) =
        if isNull node then
            None
        elif node.Height = 1uy then
            mapping.Invoke(node.Key, node.Value)
        else
            let node = node :?> Inner<'Key, 'Value>
            match tryPickBack mapping node.Right with
            | None ->
                match mapping.Invoke(node.Key, node.Value) with
                | None -> tryPickBack mapping node.Left
                | res -> res
            | res -> res
            
    let rec tryPickBackV (mapping : OptimizedClosures.FSharpFunc<'Key, 'Value, voption<'T>>) (node : Node<'Key, 'Value>) =
        if isNull node then
            ValueNone
        elif node.Height = 1uy then
            mapping.Invoke(node.Key, node.Value)
        else
            let node = node :?> Inner<'Key, 'Value>
            match tryPickBackV mapping node.Right with
            | ValueNone ->
                match mapping.Invoke(node.Key, node.Value) with
                | ValueNone -> tryPickBackV mapping node.Left
                | res -> res
            | res -> res


    let rec tryPick (mapping : OptimizedClosures.FSharpFunc<'Key, 'Value, option<'T>>) (node : Node<'Key, 'Value>) =
        if isNull node then
            None
        elif node.Height = 1uy then
            mapping.Invoke(node.Key, node.Value)
        else
            let node = node :?> Inner<'Key, 'Value>
            match tryPick mapping node.Left with
            | None ->
                match mapping.Invoke(node.Key, node.Value) with
                | None -> tryPick mapping node.Right
                | res -> res
            | res -> res
            
    let rec tryPickV (mapping : OptimizedClosures.FSharpFunc<'Key, 'Value, voption<'T>>) (node : Node<'Key, 'Value>) =
        if isNull node then
            ValueNone
        elif node.Height = 1uy then
            mapping.Invoke(node.Key, node.Value)
        else
            let node = node :?> Inner<'Key, 'Value>
            match tryPickV mapping node.Left with
            | ValueNone ->
                match mapping.Invoke(node.Key, node.Value) with
                | ValueNone -> tryPickV mapping node.Right
                | res -> res
            | res -> res

                



    let rec fold (state : 'State) (folder : OptimizedClosures.FSharpFunc<'State, 'Key, 'Value, 'State>) (node : Node<'Key, 'Value>) =
        if isNull node then
            state
        elif node.Height = 1uy then
            folder.Invoke(state, node.Key, node.Value)
        else
            let node = node :?> Inner<'Key, 'Value>
            let s1 = fold state folder node.Left
            let s2 = folder.Invoke(s1, node.Key, node.Value)
            fold s2 folder node.Right

    let rec foldBack (state : 'State) (folder : OptimizedClosures.FSharpFunc<'Key, 'Value, 'State, 'State>) (node : Node<'Key, 'Value>) =
        if isNull node then
            state
        elif node.Height = 1uy then
            folder.Invoke(node.Key, node.Value, state)
        else
            let node = node :?> Inner<'Key, 'Value>
            let s1 = foldBack state folder node.Right
            let s2 = folder.Invoke(node.Key, node.Value, s1)
            foldBack s2 folder node.Left
    
    let rec exists (predicate : OptimizedClosures.FSharpFunc<'Key, 'Value, bool>) (node : Node<'Key, 'Value>) =
        if isNull node then
            false
        elif node.Height = 1uy then
            predicate.Invoke(node.Key, node.Value)
        else
            let node = node :?> Inner<'Key, 'Value>
            exists predicate node.Left ||
            predicate.Invoke(node.Key, node.Value) ||
            exists predicate node.Right
            
    let rec forall (predicate : OptimizedClosures.FSharpFunc<'Key, 'Value, bool>) (node : Node<'Key, 'Value>) =
        if isNull node then
            true
        elif node.Height = 1uy then
            predicate.Invoke(node.Key, node.Value)
        else
            let node = node :?> Inner<'Key, 'Value>
            forall predicate node.Left &&
            predicate.Invoke(node.Key, node.Value) &&
            forall predicate node.Right

    let rec partition (predicate : OptimizedClosures.FSharpFunc<'Key, 'Value, bool>) (t : byref<Node<'Key, 'Value>>) (f : byref<Node<'Key, 'Value>>) (node : Node<'Key, 'Value>) =
        if isNull node then
            t <- null
            f <- null
        elif node.Height = 1uy then
            if predicate.Invoke(node.Key, node.Value) then 
                t <- node
                f <- null
            else 
                t <- null
                f <- node
        else
            let node = node :?> Inner<'Key, 'Value>

            let mutable lt = null
            let mutable lf = null

            partition predicate &lt &lf node.Left
            let c = predicate.Invoke(node.Key, node.Value)
            partition predicate &t &f node.Right

            if c then
                t <- binary lt node.Key node.Value t
                f <- join lf f
            else
                t <- join lt t
                f <- binary lf node.Key node.Value f


    let rec split 
        (cmp : IComparer<'Key>) (key : 'Key) 
        (left : byref<Node<'Key, 'Value>>) (self : byref<'Value>) (right : byref<Node<'Key, 'Value>>) 
        (node : Node<'Key, 'Value>) =
        
        if isNull node then
            left <- null
            right <- null
            false
        elif node.Height = 1uy then
            let c = cmp.Compare(key, node.Key)
            if c > 0 then
                left <- node
                right <- null
                false
            elif c < 0 then
                left <- null
                right <- node
                false
            else
                left <- null
                right <- null
                self <- node.Value
                true
        else
            let node = node :?> Inner<'Key,'Value>
            let c = cmp.Compare(key, node.Key)
            if c > 0 then
                let res = split cmp key &left &self &right node.Right
                left <- binary node.Left node.Key node.Value left
                res
            elif c < 0 then
                let res = split cmp key &left &self &right node.Left
                right <- binary right node.Key node.Value node.Right
                res
            else
                left <- node.Left
                right <- node.Right
                self <- node.Value
                true


    let rec private changeWithLeft
        (cmp : IComparer<'Key>)
        (key : 'Key)
        (value : 'Value)
        (resolve : OptimizedClosures.FSharpFunc<'Key, 'Value, 'Value, 'Value>) 
        (node : Node<'Key, 'Value>) =
        if isNull node then
            Node(key, value)
        elif node.Height = 1uy then
            let c = cmp.Compare(key, node.Key)
            if c > 0 then
                Inner(node, key, value, null, 2uy, 2) :> Node<_,_>
            elif c < 0 then
                Inner(null, key, value, node, 2uy, 2) :> Node<_,_>
            else
                let value = resolve.Invoke(key, value, node.Value)
                Node(key, value)
        else
            let node = node :?> Inner<'Key, 'Value>
            let c = cmp.Compare(key, node.Key)
            if c > 0 then
                unsafeBinary node.Left node.Key node.Value (changeWithLeft cmp key value resolve node.Right)
            elif c < 0 then
                unsafeBinary (changeWithLeft cmp key value resolve node.Left) node.Key node.Value node.Right
            else
                let value = resolve.Invoke(key, value, node.Value)
                Inner(node.Left, node.Key, value, node.Right, node.Height, node.Count) :> Node<_,_>
                
    let rec private changeWithRight
        (cmp : IComparer<'Key>)
        (key : 'Key)
        (value : 'Value)
        (resolve : OptimizedClosures.FSharpFunc<'Key, 'Value, 'Value, 'Value>) 
        (node : Node<'Key, 'Value>) =
        if isNull node then
            Node(key, value)
        elif node.Height = 1uy then
            let c = cmp.Compare(key, node.Key)
            if c > 0 then
                Inner(node, key, value, null, 2uy, 2) :> Node<_,_>
            elif c < 0 then
                Inner(null, key, value, node, 2uy, 2) :> Node<_,_>
            else
                let value = resolve.Invoke(key, node.Value, value)
                Node(key, value)
        else
            let node = node :?> Inner<'Key, 'Value>
            let c = cmp.Compare(key, node.Key)
            if c > 0 then
                unsafeBinary node.Left node.Key node.Value (changeWithRight cmp key value resolve node.Right)
            elif c < 0 then
                unsafeBinary (changeWithRight cmp key value resolve node.Left) node.Key node.Value node.Right
            else
                let value = resolve.Invoke(key, node.Value, value)
                Inner(node.Left, node.Key, value, node.Right, node.Height, node.Count) :> Node<_,_>
                
            
        

    let rec unionWith (cmp : IComparer<'Key>) (resolve : OptimizedClosures.FSharpFunc<'Key, 'Value, 'Value, 'Value>) (l : Node<'Key, 'Value>) (r : Node<'Key, 'Value>) =
        if isNull l then r
        elif isNull r then l

        elif l.Height = 1uy then
            // left leaf
            changeWithLeft cmp l.Key l.Value resolve r

        elif r.Height = 1uy then
            // right leaf
            changeWithRight cmp r.Key r.Value resolve l

        else
            // both inner
            let l = l :?> Inner<'Key, 'Value>
            let r = r :?> Inner<'Key, 'Value>

            if l.Height < r.Height then
                let key = r.Key
                let mutable ll = null
                let mutable lv = Unchecked.defaultof<_>
                let mutable lr = null
                let hasValue = split cmp key &ll &lv &lr (l :> Node<_,_>)
                //let struct(ll, lv, lr) = splitV cmp key l

                let newLeft = unionWith cmp resolve ll r.Left

                let value =
                    if hasValue then resolve.Invoke(key, lv, r.Value)
                    else r.Value
                let newRight = unionWith cmp resolve lr r.Right

                binary newLeft key value newRight
            else
                let key = l.Key
                let mutable rl = null
                let mutable rv = Unchecked.defaultof<_>
                let mutable rr = null
                let hasValue = split cmp key &rl &rv &rr (r :> Node<_,_>)

                let newLeft = unionWith cmp resolve l.Left rl

                let value =
                    if hasValue then resolve.Invoke(key, l.Value, rv)
                    else l.Value

                let newRight = unionWith cmp resolve l.Right rr
                
                binary newLeft key value newRight
      

    let rec union (cmp : IComparer<'Key>) (map1 : Node<'Key, 'Value>) (map2 : Node<'Key, 'Value>) =
        if System.Object.ReferenceEquals(map1, map2) then map1

        elif isNull map1 then map2
        elif isNull map2 then map1

        elif map1.Height = 1uy then
            // map1 leaf
            map2 |> addIfNotPresent cmp map1.Key map1.Value
        elif map2.Height = 1uy then
            // map2 leaf
            map1 |> add cmp map2.Key map2.Value

        else
            // both inner
            let map1 = map1 :?> Inner<'Key, 'Value>
            let map2 = map2 :?> Inner<'Key, 'Value>

            if map1.Height < map2.Height then
                let key = map2.Key
                let mutable l1 = null
                let mutable v1 = Unchecked.defaultof<_>
                let mutable r1 = null
                split cmp key &l1 &v1 &r1 (map1 :> Node<_,_>) |> ignore
                let newLeft = union cmp l1 map2.Left
                let value = map2.Value
                let newRight = union cmp r1 map2.Right

                binary newLeft key value newRight
            else
                let key = map1.Key
                let mutable l2 = null
                let mutable v2 = Unchecked.defaultof<_>
                let mutable r2 = null
                let hasValue = split cmp key &l2 &v2 &r2 (map2 :> Node<_,_>)
                let newLeft = union cmp map1.Left l2
                let value = if hasValue then v2 else map1.Value
                let newRight = union cmp map1.Right r2
                binary newLeft key value newRight

    [<System.Flags>]
    type NeighbourFlags =
        | None = 0
        | Left = 1
        | Self = 2
        | Right = 4

    let rec getNeighbours 
        (cmp : IComparer<'Key>) (key : 'Key) 
        (flags : NeighbourFlags)
        (leftKey : byref<'Key>) (leftValue : byref<'Value>)
        (selfValue : byref<'Value>)
        (rightKey : byref<'Key>) (rightValue : byref<'Value>)
        (node : Node<'Key, 'Value>) =

        if isNull node then
            flags

        elif node.Height = 1uy then
            let c = cmp.Compare(key, node.Key)
            if c > 0 then
                leftKey <- node.Key
                leftValue <- node.Value
                flags ||| NeighbourFlags.Left
            elif c < 0 then
                rightKey <- node.Key
                rightValue <- node.Value
                flags ||| NeighbourFlags.Right
            else
                selfValue <- node.Value
                flags ||| NeighbourFlags.Self
        else
            let node = node :?> Inner<'Key, 'Value>
            let c = cmp.Compare(key, node.Key)
            if c > 0 then
                leftKey <- node.Key
                leftValue <- node.Value
                getNeighbours cmp key (flags ||| NeighbourFlags.Left) &leftKey &leftValue &selfValue &rightKey &rightValue node.Right
            elif c < 0 then
                rightKey <- node.Key
                rightValue <- node.Value
                getNeighbours cmp key (flags ||| NeighbourFlags.Right) &leftKey &leftValue &selfValue &rightKey &rightValue node.Left
            else    
                selfValue <- node.Value
                let mutable flags = flags 
                if tryGetMin &rightKey &rightValue node.Right then flags <- flags ||| NeighbourFlags.Right
                if tryGetMax &leftKey &leftValue node.Left then flags <- flags ||| NeighbourFlags.Left
                flags ||| NeighbourFlags.Self

    let rec getNeighboursAt
        (index : int) 
        (flags : NeighbourFlags)
        (leftKey : byref<'Key>) (leftValue : byref<'Value>)
        (selfKey : byref<'Key>) (selfValue : byref<'Value>)
        (rightKey : byref<'Key>) (rightValue : byref<'Value>)
        (node : Node<'Key, 'Value>) =

        if isNull node then
            flags

        elif node.Height = 1uy then
            if index > 0 then
                leftKey <- node.Key
                leftValue <- node.Value
                flags ||| NeighbourFlags.Left
            elif index < 0 then
                rightKey <- node.Key
                rightValue <- node.Value
                flags ||| NeighbourFlags.Right
            else
                selfKey <- node.Key
                selfValue <- node.Value
                flags ||| NeighbourFlags.Self
        else
            let node = node :?> Inner<'Key, 'Value>
            let id = index - count node.Left
            if id > 0 then
                leftKey <- node.Key
                leftValue <- node.Value
                getNeighboursAt (id - 1) (flags ||| NeighbourFlags.Left) &leftKey &leftValue &selfKey &selfValue &rightKey &rightValue node.Right
            elif id < 0 then
                rightKey <- node.Key
                rightValue <- node.Value
                getNeighboursAt index (flags ||| NeighbourFlags.Right) &leftKey &leftValue &selfKey &selfValue &rightKey &rightValue node.Left
            else    
                selfKey <- node.Key
                selfValue <- node.Value
                let mutable flags = flags 
                if tryGetMin &rightKey &rightValue node.Right then flags <- flags ||| NeighbourFlags.Right
                if tryGetMax &leftKey &leftValue node.Left then flags <- flags ||| NeighbourFlags.Left
                flags ||| NeighbourFlags.Self



    let rec withMin (cmp : IComparer<'Key>) (minKey : 'Key) (node : Node<'Key, 'Value>) =
        if isNull node then
            node
        elif node.Height = 1uy then
            let c = cmp.Compare(node.Key, minKey)
            if c >= 0 then node
            else null
        else
            let node = node :?> Inner<'Key, 'Value>
            let c = cmp.Compare(node.Key, minKey)
            if c > 0 then
                binary (withMin cmp minKey node.Left) node.Key node.Value node.Right
            elif c < 0 then
                withMin cmp minKey node.Right
            else
                withMin cmp minKey node.Right |> add cmp node.Key node.Value
                
    let rec withMax (cmp : IComparer<'Key>) (maxKey : 'Key) (node : Node<'Key, 'Value>) =
        if isNull node then
            node
        elif node.Height = 1uy then
            let c = cmp.Compare(node.Key, maxKey)
            if c <= 0 then node
            else null
        else
            let node = node :?> Inner<'Key, 'Value>
            let c = cmp.Compare(node.Key, maxKey)

            if c > 0 then
                withMax cmp maxKey node.Left
            elif c < 0 then
                binary node.Left node.Key node.Value (withMax cmp maxKey node.Right)
            else
                withMax cmp maxKey node.Left |> add cmp node.Key node.Value
  
    let rec withMinExclusiveN 
        (cmp : IComparer<'Key>) (minKey : 'Key) 
        (firstKey : byref<'Key>) (firstValue : byref<'Value>)
        (node : Node<'Key, 'Value>) =
        if isNull node then
            node
        elif node.Height = 1uy then
            let c = cmp.Compare(node.Key, minKey)
            if c > 0 then 
                firstKey <- node.Key
                firstValue <- node.Value
                node
            else 
                null
        else
            let node = node :?> Inner<'Key, 'Value>
            let c = cmp.Compare(node.Key, minKey)
            if c > 0 then
                let newLeft = withMinExclusiveN cmp minKey &firstKey &firstValue node.Left
                if isNull newLeft then
                    firstKey <- node.Key
                    firstValue <- node.Value
                binary newLeft node.Key node.Value node.Right
            else
                withMinExclusiveN cmp minKey &firstKey &firstValue node.Right
 
    let rec withMaxExclusiveN 
        (cmp : IComparer<'Key>) (maxKey : 'Key) 
        (lastKey : byref<'Key>) (lastValue : byref<'Value>)
        (node : Node<'Key, 'Value>) =
        if isNull node then
            node
        elif node.Height = 1uy then
            let c = cmp.Compare(node.Key, maxKey)
            if c < 0 then 
                lastKey <- node.Key
                lastValue <- node.Value
                node
            else 
                null
        else
            let node = node :?> Inner<'Key, 'Value>
            let c = cmp.Compare(node.Key, maxKey)
            if c < 0 then
                let newRight = withMaxExclusiveN cmp maxKey &lastKey &lastValue node.Right
                if isNull newRight then
                    lastKey <- node.Key
                    lastValue <- node.Value
                binary node.Left node.Key node.Value newRight
            else
                withMaxExclusiveN cmp maxKey &lastKey &lastValue node.Left


    let rec slice (cmp : IComparer<'Key>) (minKey : 'Key) (maxKey : 'Key) (node : Node<'Key, 'Value>) =
        if isNull node then
            node
        elif node.Height = 1uy then
            let cMin = cmp.Compare(minKey, node.Key)
            if cMin <= 0 then
                let cMax = cmp.Compare(maxKey, node.Key)
                if cMax >= 0 then
                    node
                else
                    null
            else
                null
        else
            let node = node :?> Inner<'Key, 'Value>
            let cMin = cmp.Compare(minKey, node.Key)
            let cMax = cmp.Compare(maxKey, node.Key)

            if cMin <= 0 && cMax >= 0 then
                // split-key contained
                binary (withMin cmp minKey node.Left) node.Key node.Value (withMax cmp maxKey node.Right)
            elif cMin > 0 then  
                // min larger than split-key
                slice cmp minKey maxKey node.Right
            else (* cMax < 0 *)
                // max smaller than split-key
                slice cmp minKey maxKey node.Left

    let rec take (n : int) (node : Node<'Key, 'Value>) =
        if n <= 0 || isNull node then
            null
        elif node.Height = 1uy then
            node
        else
            let inner = node :?> Inner<'Key, 'Value>
            if inner.Count <= n then 
                node
            else
                let lc = count inner.Left
                if lc < n then
                    let newRight = take (n - 1 - lc) inner.Right
                    binary inner.Left inner.Key inner.Value newRight
                elif lc = n then
                    inner.Left
                else    
                    take n inner.Left
            

    let rec skip (n : int) (node : Node<'Key, 'Value>) =
        if n <= 0 || isNull node then
            node
        elif node.Height = 1uy then
            null
        else
            let inner = node :?> Inner<'Key, 'Value>
            if inner.Count <= n then
                null
            else
                let lc = count inner.Left
                if n > lc then
                    skip (n - 1 - lc) inner.Right
                elif n = lc then
                    unsafeAddMinimum inner.Key inner.Value inner.Right
                else
                    binary (skip n inner.Left) inner.Key inner.Value inner.Right
     

    let rec sliceAt (minIndex : int) (maxIndex : int) (node : Node<'Key, 'Value>) =
        if isNull node then
            node

        elif node.Height = 1uy then
            if minIndex <= 0 && maxIndex >= 0 then node
            else null

        else
            let node = node :?> Inner<'Key, 'Value>
            let lc = count node.Left

            if minIndex > lc then
                // only right
                let newMin = minIndex - lc - 1
                let newMax = maxIndex - lc - 1
                sliceAt newMin newMax node.Right

            elif maxIndex < lc then
                // only left
                let newMin = minIndex
                let newMax = maxIndex
                sliceAt newMin newMax node.Left

            else
                // contained

                let skipLeft = minIndex
                let takeRight = maxIndex - lc

                let l = 
                    if skipLeft <= 0 then node.Left
                    else skip skipLeft node.Left

                let r = 
                    if takeRight >= count node.Right then node.Right
                    else take takeRight node.Right

                binary l node.Key node.Value r





    let inline private ofTwoOption (min : 'Key) (max : 'Key) (minValue : voption<'Value>) (maxValue : voption<'Value>) =
        match minValue with
        | ValueSome va ->
            match maxValue with
            | ValueSome vb ->
                Inner(null, min, va, Node(max, vb), 2uy, 2) :> Node<_,_>

            | ValueNone ->
                Node(min, va)
        | ValueNone ->
            match maxValue with
            | ValueSome vb ->
                Node(max, vb)
            | ValueNone ->
                null

    let rec changeWithNeighbours
        (cmp : IComparer<'Key>) 
        (key : 'Key)
        (l : voption<struct('Key * 'Value)>)
        (r : voption<struct('Key * 'Value)>)
        (replacement : voption<struct('Key * 'Value)> -> voption<'Value> -> voption<struct('Key * 'Value)> -> voption<'Value>)
        (node : Node<'Key, 'Value>) =

        if isNull node then
            let newValue = replacement l ValueNone r
            match newValue with
            | ValueSome b -> Node(key, b)
            | ValueNone -> null

        elif node.Height = 1uy then
            let c = cmp.Compare(key, node.Key)
            if c > 0 then
                let newValue = replacement (ValueSome struct(node.Key, node.Value)) ValueNone r
                match newValue with
                | ValueSome newValue ->
                    Inner(node, key, newValue, null, 2uy, 2) :> Node<_,_>
                | ValueNone ->
                    node
            elif c < 0 then
                let newValue = replacement l ValueNone (ValueSome struct(node.Key, node.Value))
                match newValue with
                | ValueSome newValue ->
                    Inner(null, key, newValue, node, 2uy, 2) :> Node<_,_>
                | ValueNone ->
                    node
            else
                let newValue = replacement l (ValueSome node.Value) r
                match newValue with
                | ValueSome newValue ->
                    Node(key, newValue)
                | ValueNone ->
                    null
                
        else
            let node = node :?> Inner<'Key, 'Value>
            let c = cmp.Compare(key, node.Key)
            if c > 0 then
                binary node.Left node.Key node.Value (changeWithNeighbours cmp key (ValueSome struct(node.Key, node.Value)) r replacement node.Right)
            elif c < 0 then 
                binary (changeWithNeighbours cmp key l (ValueSome struct(node.Key, node.Value)) replacement node.Left) node.Key node.Value node.Right
            else
                let rNeighbour =
                    let mutable k = Unchecked.defaultof<_>
                    let mutable v = Unchecked.defaultof<_>
                    if tryGetMin &k &v node.Right then
                        ValueSome struct(k,v)
                    else
                        r

                let lNeighbour =
                    let mutable k = Unchecked.defaultof<_>
                    let mutable v = Unchecked.defaultof<_>
                    if tryGetMax &k &v node.Right then
                        ValueSome struct(k,v)
                    else
                        l

                let newValue = replacement lNeighbour (ValueSome node.Value) rNeighbour
                match newValue with
                | ValueSome newValue ->
                    Inner(node.Left, key, newValue, node.Right, node.Height, node.Count) :> Node<_,_>
                | ValueNone ->
                    unsafeJoin node.Left node.Right
           
    let rec replaceRange 
        (cmp : IComparer<'Key>) 
        (min : 'Key) (max : 'Key) 
        (l : voption<struct('Key * 'Value)>)
        (r : voption<struct('Key * 'Value)>)
        (replacement : voption<struct('Key * 'Value)> -> voption<struct('Key * 'Value)> -> struct(voption<'Value> * voption<'Value>))
        (node : Node<'Key, 'Value>) =
        
        if isNull node then
            let struct (a, b) = replacement l r
            ofTwoOption min max a b

        elif node.Height = 1uy then
            let cMin = cmp.Compare(node.Key, min)
            let cMax = cmp.Compare(node.Key, max)
            if cMin >= 0 && cMax <= 0 then
                let struct(a, b) = replacement l r
                ofTwoOption min max a b
            elif cMin < 0 then
                // node is left
                let struct(a, b) = replacement (ValueSome struct(node.Key, node.Value)) r

                match a with
                | ValueSome va ->
                    match b with
                    | ValueSome vb ->
                        Inner(node, min, va, Node(max, vb), 2uy, 3) :> Node<_,_>
                    | ValueNone ->
                        Inner(node, min, va, null, 2uy, 2) :> Node<_,_>
                | ValueNone ->
                    match b with
                    | ValueSome vb ->
                        Inner(node, max, vb, null, 2uy, 2) :> Node<_,_>
                    | ValueNone ->
                        node
            else
                // node is right
                let struct(a, b) = replacement l (ValueSome struct(node.Key, node.Value))

                match a with
                | ValueSome va ->
                    match b with
                    | ValueSome vb ->
                        Inner(Node(min, va), max, vb, node, 2uy, 3) :> Node<_,_>
                    | ValueNone ->
                        Inner(null, min, va, node, 2uy, 2) :> Node<_,_>
                | ValueNone ->
                    match b with
                    | ValueSome vb ->
                        Inner(null, max, vb, node, 2uy, 2) :> Node<_,_>
                    | ValueNone ->
                        node
        else
            let node = node :?> Inner<'Key, 'Value>
            let cMin = cmp.Compare(node.Key, min)
            let cMax = cmp.Compare(node.Key, max)
            if cMin >= 0 && cMax <= 0 then

                let mutable minKey = Unchecked.defaultof<_>
                let mutable minValue = Unchecked.defaultof<_>
                let mutable maxKey = Unchecked.defaultof<_>
                let mutable maxValue = Unchecked.defaultof<_>

                let l1 = withMaxExclusiveN cmp min &minKey &minValue node.Left
                let r1 = withMinExclusiveN cmp max &maxKey &maxValue node.Right

                let ln = 
                    if isNull l1 then l
                    else ValueSome struct(minKey, minValue)
                let rn = 
                    if isNull r1 then r
                    else ValueSome struct(maxKey, maxValue)

                let struct(a, b) = replacement ln rn

                match a with
                | ValueSome va ->
                    match b with
                    | ValueSome vb ->
                        if height l1 < height r1 then
                            binary (add cmp min va l1) max vb r1
                        else
                            binary l1 min va (add cmp max vb r1)
                    | ValueNone ->
                        binary l1 min va r1
                | ValueNone ->
                    match b with
                    | ValueSome vb ->
                        binary l1 max vb r1
                    | ValueNone ->
                        join l1 r1

            elif cMin < 0 then
                // only right
                let r1 = replaceRange cmp min max (ValueSome struct(node.Key, node.Value)) r replacement node.Right
                binary node.Left node.Key node.Value r1
            else    
                // only left
                let l1 = replaceRange cmp min max l (ValueSome struct(node.Key, node.Value)) replacement node.Left
                binary l1 node.Key node.Value node.Right

    let rec computeDelta
        (cmp : IComparer<'Key>) 
        (node1 : Node<'Key, 'Value1>)
        (node2 : Node<'Key, 'Value2>)
        (update : OptimizedClosures.FSharpFunc<'Key, 'Value1, 'Value2, voption<'OP>>)
        (invoke : OptimizedClosures.FSharpFunc<'Key, 'Value2, 'OP>)
        (revoke : OptimizedClosures.FSharpFunc<'Key, 'Value1, 'OP>) =
        if isNull node1 then
            map invoke node2
        elif isNull node2 then
            map revoke node1
        elif System.Object.ReferenceEquals(node1, node2) then
            null
        elif node1.Height = 1uy then
            // node1 is leaf
            if node2.Height = 1uy then
                // both are leaves
                let c = cmp.Compare(node2.Key, node1.Key)
                if c > 0 then
                    let a = revoke.Invoke(node1.Key, node1.Value)
                    let b = invoke.Invoke(node2.Key, node2.Value)
                    Inner(Node(node1.Key, a), node2.Key, b, null, 2uy, 2) :> Node<_,_>
                elif c < 0 then 
                    let b = invoke.Invoke(node2.Key, node2.Value)
                    let a = revoke.Invoke(node1.Key, node1.Value)
                    Inner(null, node2.Key, b, Node(node1.Key, a), 2uy, 2) :> Node<_,_>
                else
                    match update.Invoke(node1.Key, node1.Value, node2.Value) with
                    | ValueSome op -> Node(node1.Key, op)
                    | ValueNone -> null
            else
                // node1 is leaf
                // node2 is inner
                let node2 = node2 :?> Inner<'Key, 'Value2>
                let c = cmp.Compare(node1.Key, node2.Key)

                if c > 0 then
                    let l1 = map invoke node2.Left
                    let s = invoke.Invoke(node2.Key, node2.Value)
                    let r1 = computeDelta cmp node1 node2.Right update invoke revoke
                    binary l1 node2.Key s r1
                elif c < 0 then
                    let l1 = computeDelta cmp node1 node2.Left update invoke revoke
                    let s = invoke.Invoke(node2.Key, node2.Value)
                    let r1 = map invoke node2.Right
                    binary l1 node2.Key s r1
                else
                    let l1 = map invoke node2.Left
                    let s = update.Invoke(node1.Key, node1.Value, node2.Value)
                    let r1 = map invoke node2.Right
                    match s with
                    | ValueSome op ->
                        Inner(l1, node1.Key, op, r1, node2.Height, node2.Count) :> Node<_,_>
                    | ValueNone ->
                        join l1 r1
                        
        elif node2.Height = 1uy then
            // node2 is leaf
            // node1 is inner
            let node1 = node1 :?> Inner<'Key, 'Value1>
            let c = cmp.Compare(node2.Key, node1.Key)
            if c > 0 then
                let l1 = map revoke node1.Left
                let s = revoke.Invoke(node1.Key, node1.Value)
                let r1 = computeDelta cmp node1.Right node2 update invoke revoke
                binary l1 node1.Key s r1
            elif c < 0 then     
                let l1 = computeDelta cmp node1.Left node2 update invoke revoke
                let s = revoke.Invoke(node1.Key, node1.Value)
                let r1 = map revoke node1.Right
                binary l1 node1.Key s r1
            else
                let l1 = map revoke node1.Left
                let s = update.Invoke(node1.Key, node1.Value, node2.Value)
                let r1 = map revoke node1.Right
                match s with
                | ValueSome op ->
                    Inner(l1, node1.Key, op, r1, node1.Height, node1.Count) :> Node<_,_>
                | ValueNone ->
                    join l1 r1

        elif node1.Height > node2.Height then
            // both are inner h1 > h2
            let node1 = node1 :?> Inner<'Key, 'Value1>
            let mutable l2 = null
            let mutable s2 = Unchecked.defaultof<_>
            let mutable r2 = null
            let hasValue = split cmp node1.Key &l2 &s2 &r2 node2
            if hasValue then
                let ld = computeDelta cmp node1.Left l2 update invoke revoke
                let self = update.Invoke(node1.Key, node1.Value, s2)
                let rd = computeDelta cmp node1.Right r2 update invoke revoke
                match self with
                | ValueSome self -> binary ld node1.Key self rd
                | ValueNone -> join ld rd
            else
                let ld = computeDelta cmp node1.Left l2 update invoke revoke
                let op = revoke.Invoke(node1.Key, node1.Value)
                let rd = computeDelta cmp node1.Right r2 update invoke revoke
                binary ld node1.Key op rd
        else
            // both are inner h2 > h1
            let node2 = node2 :?> Inner<'Key, 'Value2>
            let mutable l1 = null
            let mutable s1 = Unchecked.defaultof<_>
            let mutable r1 = null
            let hasValue = split cmp node2.Key &l1 &s1 &r1 node1
            if hasValue then
                let ld = computeDelta cmp l1 node2.Left update invoke revoke
                let self = update.Invoke(node2.Key, s1, node2.Value)
                let rd = computeDelta cmp r1 node2.Right update invoke revoke
                match self with
                | ValueSome self -> binary ld node2.Key self rd
                | ValueNone -> join ld rd
            else
                let ld = computeDelta cmp l1 node2.Left update invoke revoke
                let self = invoke.Invoke(node2.Key, node2.Value)
                let rd = computeDelta cmp r1 node2.Right update invoke revoke
                binary ld node2.Key self rd
           
    module ApplyDelta = 
        let rec applyDeltaSingletonState 
            (cmp : IComparer<'Key>)
            (specialKey : 'Key)
            (specialValue : 'T)
            (mapping : OptimizedClosures.FSharpFunc<'Key, 'Value, voption<'T>>) 
            (mapping2 : OptimizedClosures.FSharpFunc<'Key, 'T, 'Value, voption<'T>>) 
            (node : Node<'Key, 'Value>) = 
                if isNull node then
                    Node(specialKey, specialValue)
                elif node.Height = 1uy then
                    let c = cmp.Compare(specialKey, node.Key)
                    if c > 0 then
                        match mapping.Invoke(node.Key, node.Value) with
                        | ValueSome v -> 
                            Inner(null, node.Key, v, Node(specialKey, specialValue), 2uy, 2) :> Node<_,_>
                        | ValueNone ->
                            Node(specialKey, specialValue)
                    elif c < 0 then
                        match mapping.Invoke(node.Key, node.Value) with
                        | ValueSome v -> 
                            Inner(Node(specialKey, specialValue), node.Key, v, null, 2uy, 2) :> Node<_,_>
                        | ValueNone ->
                            Node(specialKey, specialValue)
                    else
                        match mapping2.Invoke(node.Key, specialValue, node.Value) with
                        | ValueSome v ->
                            Node(node.Key, v)
                        | ValueNone ->
                            null
                else
                    let node = node :?> Inner<'Key, 'Value>
                    let c = cmp.Compare(specialKey, node.Key)
                    if c > 0 then
                        let l = chooseV mapping node.Left
                        let s = mapping.Invoke(node.Key, node.Value)
                        let r = applyDeltaSingletonState cmp specialKey specialValue mapping mapping2 node.Right
                        match s with
                        | ValueSome value -> 
                            binary l node.Key value r
                        | ValueNone ->
                            join l r
                    elif c < 0 then
                        let l = applyDeltaSingletonState cmp specialKey specialValue mapping mapping2 node.Left
                        let s = mapping.Invoke(node.Key, node.Value)
                        let r = chooseV mapping node.Right
                        match s with
                        | ValueSome value -> 
                            binary l node.Key value r
                        | ValueNone ->
                            join l r
                   
                    else
                        let l = chooseV mapping node.Left
                        let self = mapping2.Invoke(node.Key, specialValue, node.Value)
                        let r = chooseV mapping node.Right
                        match self with
                        | ValueSome res ->
                            binary l node.Key res r
                        | ValueNone ->
                            join l r

                        
        let rec applyDeltaSingle 
            (cmp : IComparer<'Key>) 
            (specialKey : 'Key) 
            (specialValue : 'T)
            (update : OptimizedClosures.FSharpFunc<'Key, 'T, voption<'Value>>)
            (update2 : OptimizedClosures.FSharpFunc<'Key, 'Value, 'T, voption<'Value>>) 
            (node : Node<'Key, 'Value>) : Node<'Key, 'Value> =
            if isNull node then
                match update.Invoke(specialKey, specialValue) with
                | ValueNone -> null
                | ValueSome v -> Node(specialKey, v)

            elif node.Height = 1uy then
                let c = cmp.Compare(specialKey, node.Key)
                if c > 0 then
                    match update.Invoke(specialKey, specialValue) with
                    | ValueNone -> node
                    | ValueSome n ->
                        Inner(node, specialKey, n, null, 2uy, 2) :> Node<_,_>
                elif c < 0 then
                    match update.Invoke(specialKey, specialValue) with
                    | ValueNone -> node
                    | ValueSome n ->
                        Inner(null, specialKey, n, node, 2uy, 2) :> Node<_,_>
                else
                    match update2.Invoke(specialKey, node.Value, specialValue) with
                    | ValueNone -> null
                    | ValueSome v -> Node(specialKey, v)
            else    
                let node = node :?> Inner<'Key, 'Value>
                let c = cmp.Compare(specialKey, node.Key)
                if c > 0 then
                    unsafeBinary node.Left node.Key node.Value (applyDeltaSingle cmp specialKey specialValue update update2 node.Right)
                elif c < 0 then
                    unsafeBinary (applyDeltaSingle cmp specialKey specialValue update update2 node.Left) node.Key node.Value node.Right
                else
                    match update2.Invoke(specialKey, node.Value, specialValue) with
                    | ValueSome n ->
                        Inner(node.Left, specialKey, n, node.Right, node.Height, node.Count) :> Node<_,_>
                    | ValueNone ->
                        unsafeJoin node.Left node.Right


        let rec applyDelta
            (cmp : IComparer<'Key>)
            (state : Node<'Key, 'Value>)
            (delta : Node<'Key, 'OP>)
            (applyNoState : OptimizedClosures.FSharpFunc<'Key, 'OP, voption<'Value>>) 
            (apply : OptimizedClosures.FSharpFunc<'Key, 'Value, 'OP, voption<'Value>>) =

            if isNull delta then
                // delta empty
                state
            elif isNull state then
                // state empty
                chooseV applyNoState delta

            elif delta.Height = 1uy then
                // delta leaf
                applyDeltaSingle cmp delta.Key delta.Value applyNoState apply state

            elif state.Height = 1uy then
                // state leaf
                applyDeltaSingletonState cmp state.Key state.Value applyNoState apply delta
                
            else
                // both inner
                let state = state :?> Inner<'Key, 'Value>
                let mutable dl = null
                let mutable ds = Unchecked.defaultof<_>
                let mutable dr = null
                let hasValue = split cmp state.Key &dl &ds &dr delta
                let delta = ()

                if hasValue then
                    let l = applyDelta cmp state.Left dl applyNoState apply
                    let self = apply.Invoke(state.Key, state.Value, ds)
                    let r = applyDelta cmp state.Right dr applyNoState apply
                    match self with
                    | ValueSome self -> binary l state.Key self r
                    | ValueNone -> join l r
                else
                    let l = applyDelta cmp state.Left dl applyNoState apply
                    let r = applyDelta cmp state.Right dr applyNoState apply
                    binary l state.Key state.Value r
            

        let rec chooseVAndGetEffective 
            (mapping : OptimizedClosures.FSharpFunc<'Key, 'Value, struct(voption<'T> * voption<'T2>)>) 
            (effective : byref<Node<'Key, 'T2>>) 
            (node : Node<'Key, 'Value>) =
            if isNull node then
                effective <- null
                null
            elif node.Height = 1uy then
                let struct(s, op) = mapping.Invoke(node.Key, node.Value)

                match op with
                | ValueNone -> effective <- null
                | ValueSome op -> effective <- Node(node.Key, op)

                match s with
                | ValueSome v -> Node(node.Key, v)
                | ValueNone -> null
            else
                let node = node :?> Inner<'Key, 'Value>
                let mutable re = null
                let l = chooseVAndGetEffective mapping &effective node.Left
                let struct(s, op) = mapping.Invoke(node.Key, node.Value)
                let r = chooseVAndGetEffective mapping &re node.Right
                
                match op with
                | ValueNone -> effective <- join effective re
                | ValueSome op -> effective <- binary effective node.Key op re

                match s with
                | ValueSome s -> binary l node.Key s r
                | ValueNone -> join l r

        let rec private applyDeltaSingletonStateEff 
            (cmp : IComparer<'Key>)
            (specialKey : 'Key)
            (specialValue : 'Value)
            (mapping : OptimizedClosures.FSharpFunc<'Key, 'OP, struct(voption<'Value> * voption<'OP2>)>) 
            (mapping2 : OptimizedClosures.FSharpFunc<'Key, 'Value, 'OP, struct(voption<'Value> * voption<'OP2>)>) 
            (effective : byref<Node<'Key, 'OP2>>)
            (delta : Node<'Key, 'OP>) = 
                if isNull delta then
                    effective <- null
                    Node(specialKey, specialValue)

                elif delta.Height = 1uy then
                    let c = cmp.Compare(specialKey, delta.Key)
                    if c > 0 then
                        let struct(state, op) = mapping.Invoke(delta.Key, delta.Value)
                        match op with
                        | ValueNone -> effective <- null
                        | ValueSome op -> effective <- Node(delta.Key, op)

                        match state with
                        | ValueSome v -> Inner(null, delta.Key, v, Node(specialKey, specialValue), 2uy, 2) :> Node<_,_>
                        | ValueNone -> Node(specialKey, specialValue)
                    elif c < 0 then
                        let struct(state, op) = mapping.Invoke(delta.Key, delta.Value)
                        
                        match op with
                        | ValueNone -> effective <- null
                        | ValueSome op -> effective <- Node(delta.Key, op)

                        match state with
                        | ValueSome v -> Inner(Node(specialKey, specialValue), delta.Key, v, null, 2uy, 2) :> Node<_,_>
                        | ValueNone -> Node(specialKey, specialValue)
                    else
                        let struct(state, op) = mapping2.Invoke(delta.Key, specialValue, delta.Value)

                        match op with
                        | ValueNone -> effective <- null
                        | ValueSome op -> effective <- Node(delta.Key, op)

                        match state with
                        | ValueSome v -> Node(delta.Key, v)
                        | ValueNone -> null
                else
                    let delta = delta :?> Inner<'Key, 'OP>
                    let c = cmp.Compare(specialKey, delta.Key)
                    if c > 0 then
                        let mutable re = null
                        let l = chooseVAndGetEffective mapping &effective delta.Left
                        let struct(s, op) = mapping.Invoke(delta.Key, delta.Value)
                        let r = applyDeltaSingletonStateEff cmp specialKey specialValue mapping mapping2 &re delta.Right
                        
                        match op with
                        | ValueNone -> effective <- join effective re
                        | ValueSome op -> effective <- binary effective delta.Key op re

                        match s with
                        | ValueSome value -> binary l delta.Key value r
                        | ValueNone -> join l r
                    elif c < 0 then
                        let mutable re = null
                        let l = applyDeltaSingletonStateEff cmp specialKey specialValue mapping mapping2 &effective delta.Left
                        let struct(s, op) = mapping.Invoke(delta.Key, delta.Value)
                        let r = chooseVAndGetEffective mapping &re delta.Right

                        match op with
                        | ValueNone -> effective <- join effective re
                        | ValueSome op -> effective <- binary effective delta.Key op re

                        match s with
                        | ValueSome value -> binary l delta.Key value r
                        | ValueNone -> join l r
                   
                    else
                        let mutable re = null
                        let l = chooseVAndGetEffective mapping &effective delta.Left
                        let struct(s, op) = mapping2.Invoke(delta.Key, specialValue, delta.Value)
                        let r = chooseVAndGetEffective mapping &re delta.Right
                        
                        match op with
                        | ValueNone -> effective <- join effective re
                        | ValueSome op -> effective <- binary effective delta.Key op re

                        match s with
                        | ValueSome res -> binary l delta.Key res r
                        | ValueNone -> join l r


        let rec private applyDeltaSingleEff
            (cmp : IComparer<'Key>) 
            (specialKey : 'Key) 
            (specialValue : 'T)
            (update : OptimizedClosures.FSharpFunc<'Key, 'T, struct(voption<'Value> * voption<'OP2>)>)
            (update2 : OptimizedClosures.FSharpFunc<'Key, 'Value, 'T, struct(voption<'Value> * voption<'OP2>)>) 
            (effective : byref<Node<'Key, _>>)
            (node : Node<'Key, 'Value>) : Node<'Key, 'Value> =
            if isNull node then
                let struct(state, delta) = update.Invoke(specialKey, specialValue)

                match delta with
                | ValueNone -> effective <- null
                | ValueSome op -> effective <- Node(specialKey, op)

                match state with
                | ValueNone -> null
                | ValueSome v -> Node(specialKey, v)

            elif node.Height = 1uy then
                let c = cmp.Compare(specialKey, node.Key)
                if c > 0 then
                    let struct(state, delta) = update.Invoke(specialKey, specialValue)
                    match delta with
                    | ValueNone -> effective <- null
                    | ValueSome op -> effective <- Node(specialKey, op)
                    match state with
                    | ValueNone -> node
                    | ValueSome n -> Inner(node, specialKey, n, null, 2uy, 2) :> Node<_,_>
                elif c < 0 then
                    let struct(state, delta) = update.Invoke(specialKey, specialValue)
                    match delta with
                    | ValueNone -> effective <- null
                    | ValueSome op -> effective <- Node(specialKey, op)
                    match state with
                    | ValueNone -> node
                    | ValueSome n -> Inner(null, specialKey, n, node, 2uy, 2) :> Node<_,_>
                else
                    let struct(state, delta) = update2.Invoke(specialKey, node.Value, specialValue)
                    match delta with
                    | ValueSome op -> effective <- Node(specialKey, op)
                    | ValueNone -> effective <- null
                    match state with
                    | ValueNone -> null
                    | ValueSome v -> Node(specialKey, v)
            else    
                let node = node :?> Inner<'Key, 'Value>
                let c = cmp.Compare(specialKey, node.Key)
                if c > 0 then
                    unsafeBinary node.Left node.Key node.Value (applyDeltaSingleEff cmp specialKey specialValue update update2 &effective node.Right)
                elif c < 0 then
                    unsafeBinary (applyDeltaSingleEff cmp specialKey specialValue update update2 &effective node.Left) node.Key node.Value node.Right
                else
                    let struct(state, delta) = update2.Invoke(specialKey, node.Value, specialValue)
                    match delta with
                    | ValueNone -> effective <- null
                    | ValueSome op -> effective <- Node(specialKey, op)
                    match state with
                    | ValueSome n -> Inner(node.Left, specialKey, n, node.Right, node.Height, node.Count) :> Node<_,_>
                    | ValueNone -> unsafeJoin node.Left node.Right

        let rec applyDeltaAndGetEffective
            (cmp : IComparer<'Key>)
            (state : Node<'Key, 'Value>)
            (delta : Node<'Key, 'OP>)
            (applyNoState : OptimizedClosures.FSharpFunc<'Key, 'OP, struct(voption<'Value> * voption<'OP2>)>) 
            (apply : OptimizedClosures.FSharpFunc<'Key, 'Value, 'OP, struct(voption<'Value> * voption<'OP2>)>) 
            (effective : byref<Node<'Key, 'OP2>>) =

            if isNull delta then
                // delta empty
                effective <- null
                state

            elif isNull state then
                // state empty
                chooseVAndGetEffective applyNoState &effective delta

            elif delta.Height = 1uy then
                // delta leaf
                applyDeltaSingleEff cmp delta.Key delta.Value applyNoState apply &effective state
      
            elif state.Height = 1uy then
                // state leaf
                applyDeltaSingletonStateEff cmp state.Key state.Value applyNoState apply &effective delta
                
            else
                // both inner
                let state = state :?> Inner<'Key, 'Value>
                let mutable dl = null
                let mutable ds = Unchecked.defaultof<_>
                let mutable dr = null
                let hasValue = split cmp state.Key &dl &ds &dr delta
                let delta = ()

                let mutable re = null

                if hasValue then
                    let l = applyDeltaAndGetEffective cmp state.Left dl applyNoState apply &effective
                    let struct(s, op) = apply.Invoke(state.Key, state.Value, ds)
                    let r = applyDeltaAndGetEffective cmp state.Right dr applyNoState apply &re

                    match op with
                    | ValueNone -> effective <- join effective re
                    | ValueSome op -> effective <- binary effective state.Key op re

                    match s with
                    | ValueSome self -> binary l state.Key self r
                    | ValueNone -> join l r
                else
                    let l = applyDeltaAndGetEffective cmp state.Left dl applyNoState apply &effective
                    let r = applyDeltaAndGetEffective cmp state.Right dr applyNoState apply &re
                    effective <- join effective re
                    binary l state.Key state.Value r
            


    let rec private choose2Helper 
        (cmp : IComparer<'Key>)
        (specialKey : 'Key)
        (specialValue : 'T)
        (only : OptimizedClosures.FSharpFunc<'Key, 'Value, voption<'X>>) 
        (both : OptimizedClosures.FSharpFunc<'Key, 'T, 'Value, voption<'X>>) 
        (onlySpecial : OptimizedClosures.FSharpFunc<'Key, 'T, voption<'X>>) 
        (node : Node<'Key, 'Value>) = 
            if isNull node then
                match onlySpecial.Invoke(specialKey, specialValue) with
                | ValueSome v ->
                    Node(specialKey, v)
                | ValueNone ->
                    null
            elif node.Height = 1uy then
                let c = cmp.Compare(specialKey, node.Key)
                if c > 0 then
                    match only.Invoke(node.Key, node.Value) with
                    | ValueSome v -> 
                        match onlySpecial.Invoke(specialKey, specialValue) with
                        | ValueSome specialValue ->
                            Inner(null, node.Key, v, Node(specialKey, specialValue), 2uy, 2) :> Node<_,_>
                        | ValueNone ->
                            Node(node.Key, v)
                    | ValueNone ->
                        match onlySpecial.Invoke(specialKey, specialValue) with
                        | ValueSome specialValue ->
                            Node(specialKey, specialValue)
                        | ValueNone ->
                            null
                elif c < 0 then
                    match only.Invoke(node.Key, node.Value) with
                    | ValueSome v -> 
                        match onlySpecial.Invoke(specialKey, specialValue) with
                        | ValueSome specialValue ->
                            Inner(Node(specialKey, specialValue), node.Key, v, null, 2uy, 2) :> Node<_,_>
                        | ValueNone ->
                            Node(node.Key, v)
                    | ValueNone ->
                        match onlySpecial.Invoke(specialKey, specialValue) with
                        | ValueSome specialValue ->
                            Node(specialKey, specialValue)
                        | ValueNone ->
                            null
                else
                    match both.Invoke(node.Key, specialValue, node.Value) with
                    | ValueSome v ->
                        Node(node.Key, v)
                    | ValueNone ->
                        null
            else
                let node = node :?> Inner<'Key, 'Value>
                let c = cmp.Compare(specialKey, node.Key)
                if c > 0 then
                    let l = chooseV only node.Left
                    let s = only.Invoke(node.Key, node.Value)
                    let r = choose2Helper cmp specialKey specialValue only both onlySpecial node.Right
                    match s with
                    | ValueSome value -> 
                        binary l node.Key value r
                    | ValueNone ->
                        join l r
                elif c < 0 then
                    let l = choose2Helper cmp specialKey specialValue only both onlySpecial node.Left
                    let s = only.Invoke(node.Key, node.Value)
                    let r = chooseV only node.Right
                    match s with
                    | ValueSome value -> 
                        binary l node.Key value r
                    | ValueNone ->
                        join l r
                   
                else
                    let l = chooseV only node.Left
                    let self = both.Invoke(node.Key, specialValue, node.Value)
                    let r = chooseV only node.Right
                    match self with
                    | ValueSome res ->
                        binary l node.Key res r
                    | ValueNone ->
                        join l r

    let rec choose2
        (cmp : IComparer<'Key>)
        (onlyLeft : OptimizedClosures.FSharpFunc<'Key, 'A, voption<'C>>)
        (both : OptimizedClosures.FSharpFunc<'Key, 'A, 'B, voption<'C>>)
        (onlyRight : OptimizedClosures.FSharpFunc<'Key, 'B, voption<'C>>)
        (a : Node<'Key, 'A>)
        (b : Node<'Key, 'B>) =

        if isNull a then 
            chooseV onlyRight b

        elif isNull b then
            chooseV onlyLeft a

        elif a.Height = 1uy then
            choose2Helper cmp a.Key a.Value onlyRight both onlyLeft b

        elif b.Height = 1uy then
            let flipped =
                OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt (fun a b c ->
                    both.Invoke(a, c, b)
                )
            choose2Helper cmp b.Key b.Value onlyLeft flipped onlyRight a

        else
            let a = a :?> Inner<'Key, 'A>
            let b = b :?> Inner<'Key, 'B>
            
            if a.Height > b.Height then
                let mutable bl = null
                let mutable bs = Unchecked.defaultof<_>
                let mutable br = null
                if split cmp a.Key &bl &bs &br (b :> Node<_,_>) then
                    let l = choose2 cmp onlyLeft both onlyRight a.Left bl
                    let s = both.Invoke(a.Key, a.Value, bs)
                    let r = choose2 cmp onlyLeft both onlyRight a.Right br
                    match s with
                    | ValueSome s -> binary l a.Key s r
                    | ValueNone -> join l r
                else
                    let l = choose2 cmp onlyLeft both onlyRight a.Left bl
                    let s = onlyLeft.Invoke(a.Key, a.Value)
                    let r = choose2 cmp onlyLeft both onlyRight a.Right br
                    match s with
                    | ValueSome s -> binary l a.Key s r
                    | ValueNone -> join l r
                    
            else
                let mutable al = null
                let mutable aa = Unchecked.defaultof<_>
                let mutable ar = null
                if split cmp b.Key &al &aa &ar (a :> Node<_,_>) then
                    let l = choose2 cmp onlyLeft both onlyRight al b.Left
                    let s = both.Invoke(a.Key, aa, b.Value)
                    let r = choose2 cmp onlyLeft both onlyRight ar b.Right
                    match s with
                    | ValueSome s -> binary l a.Key s r
                    | ValueNone -> join l r
                else
                    let l = choose2 cmp onlyLeft both onlyRight al b.Left
                    let s = onlyRight.Invoke(b.Key, b.Value)
                    let r = choose2 cmp onlyLeft both onlyRight ar b.Right
                    match s with
                    | ValueSome s -> binary l a.Key s r
                    | ValueNone -> join l r


    let rec hash (acc : int) (node : Node<'Key, 'Value>) =  
        if isNull node then
            acc
        elif node.Height = 1uy then
            combineHash acc (combineHash (Unchecked.hash node.Key) (Unchecked.hash node.Value))
        else
            let node = node :?> Inner<'Key, 'Value>
            let a = hash acc node.Left
            let b = combineHash a (combineHash (Unchecked.hash node.Key) (Unchecked.hash node.Value))
            hash b node.Right
            
    let rec equals (cmp : IComparer<'Key>) (a : Node<'Key, 'Value>) (b : Node<'Key, 'Value>) =  
        if System.Object.ReferenceEquals(a, b) then
            true
        elif count a <> count b then 
            false

        elif a.Height = 1uy then  
            Unchecked.equals a.Key b.Key &&
            Unchecked.equals a.Value b.Value

        else
            let na = a :?> Inner<'Key, 'Value>
            let nb = b :?> Inner<'Key, 'Value>
            
            if na.Height > nb.Height then
                let mutable bl = null
                let mutable bs = Unchecked.defaultof<_>
                let mutable br = null
                if split cmp na.Key &bl &bs &br b then
                    Unchecked.equals na.Value bs &&
                    equals cmp na.Left bl &&
                    equals cmp na.Right br
                else
                    false
            else 
                let mutable al = null
                let mutable aa = Unchecked.defaultof<_>
                let mutable ar = null
                if split cmp nb.Key &al &aa &ar a then
                    Unchecked.equals aa nb.Value &&
                    equals cmp al nb.Left &&
                    equals cmp ar nb.Right

                else
                    false
                


    type MapExtMappingEnumerator<'Key, 'Value, 'T> =
        struct
            val mutable internal Mapping : Node<'Key, 'Value> -> 'T
            val mutable internal Root : Node<'Key, 'Value>
            val mutable internal Head : struct(Node<'Key, 'Value> * bool)
            val mutable internal Tail : list<struct(Node<'Key, 'Value> * bool)>
            val mutable internal CurrentNode : Node<'Key, 'Value>
            
            member x.MoveNext() =
                let struct(n, deep) = x.Head
                if not (isNull n) then

                    if n.Height > 1uy && deep then
                        let inner = n :?> Inner<'Key, 'Value>

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
                x.Mapping x.CurrentNode

            interface System.Collections.IEnumerator with
                member x.MoveNext() = x.MoveNext()
                member x.Reset() = x.Reset()
                member x.Current = x.Current :> obj

            interface System.Collections.Generic.IEnumerator<'T> with
                member x.Current = x.Current
                member x.Dispose() = x.Dispose()

            new(root : Node<'Key, 'Value>, mapping : Node<'Key, 'Value> -> 'T) =
                {
                    Root = root
                    Head = if isNull root then Unchecked.defaultof<_> else struct(root, true)
                    Tail = []
                    CurrentNode = null
                    Mapping = mapping
                }

        end
   
    type MapExtMappingEnumerable<'Key, 'Value, 'T>(root : Node<'Key, 'Value>, mapping : Node<'Key, 'Value> -> 'T) =
        member x.GetEnumerator() = new MapExtMappingEnumerator<_,_,_>(root, mapping)

        interface System.Collections.IEnumerable with
            member x.GetEnumerator() = x.GetEnumerator() :> _
            
        interface System.Collections.Generic.IEnumerable<'T> with
            member x.GetEnumerator() = x.GetEnumerator() :> _


    type MapExtBackwardMappingEnumerator<'Key, 'Value, 'T> =
        struct
            val mutable internal Mapping : Node<'Key, 'Value> -> 'T
            val mutable internal Root : Node<'Key, 'Value>
            val mutable internal Stack : list<struct(Node<'Key, 'Value> * bool)>
            val mutable internal CurrentNode : Node<'Key, 'Value>
            
            member x.MoveNext() =
                match x.Stack with
                | struct(n, deep) :: t ->
                    x.Stack <- t

                    if n.Height > 1uy then
                        if deep then
                            let inner = n :?> Inner<'Key, 'Value>

                            if not (isNull inner.Left) then 
                                x.Stack <- struct(inner.Left, true) :: x.Stack
                                
                            if isNull inner.Right then 
                                x.CurrentNode <- n
                                true
                            else
                                x.Stack <- struct(inner.Right, true) :: struct(n, false) :: x.Stack
                                x.MoveNext()
                        else
                            x.CurrentNode <- n
                            true
                    else
                        x.CurrentNode <- n
                        true

                | [] ->
                    false


            member x.Reset() =
                if isNull x.Root then
                    x.Stack <- []
                    x.CurrentNode <- null
                else
                    x.Stack <- [struct(x.Root, true)]
                    x.CurrentNode <- null

            member x.Dispose() =
                x.Root <- null
                x.CurrentNode <- null
                x.Stack <- []

            member x.Current =
                x.Mapping x.CurrentNode

            interface System.Collections.IEnumerator with
                member x.MoveNext() = x.MoveNext()
                member x.Reset() = x.Reset()
                member x.Current = x.Current :> obj

            interface System.Collections.Generic.IEnumerator<'T> with
                member x.Current = x.Current
                member x.Dispose() = x.Dispose()

            new(root : Node<'Key, 'Value>, mapping : Node<'Key, 'Value> -> 'T) =
                {
                    Root = root
                    Stack = if isNull root then [] else [struct(root, true)]
                    CurrentNode = null
                    Mapping = mapping
                }

        end
   
    type MapExtBackwardMappingEnumerable<'Key, 'Value, 'T>(root : Node<'Key, 'Value>, mapping : Node<'Key, 'Value> -> 'T) =
        member x.GetEnumerator() = new MapExtBackwardMappingEnumerator<_,_,_>(root, mapping)

        interface System.Collections.IEnumerable with
            member x.GetEnumerator() = x.GetEnumerator() :> _
            
        interface System.Collections.Generic.IEnumerable<'T> with
            member x.GetEnumerator() = x.GetEnumerator() :> _



type internal MapExt<'Key, 'Value when 'Key : comparison>(comparer : IComparer<'Key>, root : MapExtImplementation.Node<'Key, 'Value>) =


    static let defaultComparer = LanguagePrimitives.FastGenericComparer<'Key>
    static let empty = MapExt<'Key, 'Value>(defaultComparer, null)

    member internal x.Comparer = comparer
    member internal x.Root = root

    static member Empty = empty
        
    static member FromSeq(elements : #seq<'Key * 'Value>) =
        let cmp = defaultComparer
        let mutable root = null
        for (k, v) in elements do
            root <- MapExtImplementation.addInPlace cmp k v root
        MapExt(cmp, root)

    static member FromList(elements : list<'Key * 'Value>) =
        let cmp = defaultComparer
        let mutable root = null
        for (k, v) in elements do
            root <- MapExtImplementation.addInPlace cmp k v root
        MapExt(cmp, root)

    static member FromArray(elements : ('Key * 'Value)[]) =
        let cmp = defaultComparer
        let mutable root = null
        for (k, v) in elements do
            root <- MapExtImplementation.addInPlace cmp k v root
        MapExt(cmp, root)
        
    static member FromSeqV(elements : #seq<struct('Key * 'Value)>) =
        let cmp = defaultComparer
        let mutable root = null
        for struct(k, v) in elements do
            root <- MapExtImplementation.addInPlace cmp k v root
        MapExt(cmp, root)

    static member FromListV(elements : list<struct('Key * 'Value)>) =
        let cmp = defaultComparer
        let mutable root = null
        for struct(k, v) in elements do
            root <- MapExtImplementation.addInPlace cmp k v root
        MapExt(cmp, root)

    static member FromArrayV(elements : struct('Key * 'Value)[]) =
        let cmp = defaultComparer
        let mutable root = null
        for struct(k, v) in elements do
            root <- MapExtImplementation.addInPlace cmp k v root
        MapExt(cmp, root)

    member x.Count = 
        MapExtImplementation.count root

    member x.IsEmpty =
        isNull root

    member x.Add(key : 'Key, value : 'Value) =
        MapExt(comparer, MapExtImplementation.add comparer key value root)
            
    member x.Remove(key : 'Key) =
        let mutable newRoot = null
        if MapExtImplementation.tryRemove' comparer key &newRoot root then
            MapExt(comparer, newRoot)
        else    
            x

    member x.RemoveAt(index : int) =
        if index < 0 || index >= x.Count then x
        else 
            let mutable k = Unchecked.defaultof<_>
            let mutable v = Unchecked.defaultof<_>
            MapExt(comparer, MapExtImplementation.removeAt index &k &v root)
            
    member x.TryRemoveAt(index : int) =
        if index < 0 || index >= x.Count then 
            None
        else 
            let mutable k = Unchecked.defaultof<_>
            let mutable v = Unchecked.defaultof<_>
            let res = MapExt(comparer, MapExtImplementation.removeAt index &k &v root)
            Some ((k, v), res)
            
    member x.TryRemoveAtV(index : int) =
        if index < 0 || index >= x.Count then 
            ValueNone
        else 
            let mutable k = Unchecked.defaultof<_>
            let mutable v = Unchecked.defaultof<_>
            let res = MapExt(comparer, MapExtImplementation.removeAt index &k &v root)
            ValueSome struct(k, v, res)
            
    member x.TryRemove(key : 'Key, [<Out>] result : byref<MapExt<'Key, 'Value>>, [<Out>] removedValue : byref<'Value>) =
        let mutable newRoot = null
        if MapExtImplementation.tryRemove comparer key &removedValue &newRoot root then
            result <- MapExt(comparer, newRoot)
            true
        else    
            result <- x
            false

    member x.TryRemove(key : 'Key) =
        let mutable newRoot = null
        let mutable removedValue = Unchecked.defaultof<_>
        if MapExtImplementation.tryRemove comparer key &removedValue &newRoot root then
            Some(removedValue, MapExt(comparer, newRoot))
        else    
            None

    member x.TryRemoveV(key : 'Key) =
        let mutable newRoot = null
        let mutable removedValue = Unchecked.defaultof<_>
        if MapExtImplementation.tryRemove comparer key &removedValue &newRoot root then
            ValueSome struct(removedValue, MapExt(comparer, newRoot))
        else    
            ValueNone
        

        
    member x.Change(key : 'Key, update : option<'Value> -> option<'Value>) =
        MapExt(comparer, MapExtImplementation.change comparer key update root)

    member x.Change(key : 'Key, update : voption<'Value> -> voption<'Value>) =
        MapExt(comparer, MapExtImplementation.changeV comparer key update root)
            
    member x.Item
        with get(key : 'Key) = MapExtImplementation.find comparer key root

    member x.Find(key : 'Key) =
        MapExtImplementation.find comparer key root


    member x.TryItem(index : int) =
        if index < 0 || index >= x.Count then None
        else
            let mutable k = Unchecked.defaultof<_>
            let mutable v = Unchecked.defaultof<_>
            MapExtImplementation.tryGetItem index &k &v root |> ignore
            Some (k, v)
            
    member x.TryItemV(index : int) =
        if index < 0 || index >= x.Count then ValueNone
        else
            let mutable k = Unchecked.defaultof<_>
            let mutable v = Unchecked.defaultof<_>
            MapExtImplementation.tryGetItem index &k &v root |> ignore
            ValueSome struct(k, v)

    member x.TryGetItem(index : int, [<Out>] key : byref<'Key>, [<Out>] value : byref<'Value>) =
        if index < 0 || index >= x.Count then false
        else MapExtImplementation.tryGetItem index &key &value root 

    member x.GetItem(index : int) =
        if index < 0 || index >= x.Count then
            raise <| System.IndexOutOfRangeException()
        else
            let mutable k = Unchecked.defaultof<_>
            let mutable v = Unchecked.defaultof<_>
            MapExtImplementation.tryGetItem index &k &v root |> ignore
            (k, v)
            
    member x.GetItemV(index : int) =
        if index < 0 || index >= x.Count then
            raise <| System.IndexOutOfRangeException()
        else
            let mutable k = Unchecked.defaultof<_>
            let mutable v = Unchecked.defaultof<_>
            MapExtImplementation.tryGetItem index &k &v root |> ignore
            struct(k, v)
        
            
    member x.GetIndex(key : 'Key) =
        MapExtImplementation.tryGetIndex comparer key 0 root
        
    member x.TryGetIndex(key : 'Key) =
        let index = MapExtImplementation.tryGetIndex comparer key 0 root
        if index >= 0 then Some index
        else None

    member x.TryGetIndexV(key : 'Key) =
        let index = MapExtImplementation.tryGetIndex comparer key 0 root
        if index >= 0 then ValueSome index
        else ValueNone

    member x.TryGetValue(key : 'Key, [<Out>] value : byref<'Value>) =
        MapExtImplementation.tryGetValue comparer key &value root

    member x.TryFind(key : 'Key) =
        MapExtImplementation.tryFind comparer key root
        
    member x.TryFindV(key : 'Key) =
        let mutable res = Unchecked.defaultof<_>
        if MapExtImplementation.tryGetValue comparer key &res root then
            ValueSome res
        else
            ValueNone


    member x.ContainsKey(key : 'Key) =
        MapExtImplementation.containsKey comparer key root
        
    member x.GetMinKey() =
        let mutable minKey = Unchecked.defaultof<_>
        let mutable minValue = Unchecked.defaultof<_>
        if MapExtImplementation.tryGetMin &minKey &minValue root then
            minKey
        else    
            raise <| System.IndexOutOfRangeException()
            
    member x.GetMaxKey() =
        let mutable maxKey = Unchecked.defaultof<_>
        let mutable maxValue = Unchecked.defaultof<_>
        if MapExtImplementation.tryGetMax &maxKey &maxValue root then
            maxKey
        else    
            raise <| System.IndexOutOfRangeException()
        
    member x.TryMin() =
        let mutable minKey = Unchecked.defaultof<_>
        let mutable minValue = Unchecked.defaultof<_>
        if MapExtImplementation.tryGetMin &minKey &minValue root then
            Some (minKey, minValue)
        else    
            None
        
    member x.TryMax() =
        let mutable maxKey = Unchecked.defaultof<_>
        let mutable maxValue = Unchecked.defaultof<_>
        if MapExtImplementation.tryGetMax &maxKey &maxValue root then
            Some (maxKey, maxValue)
        else    
            None
        
    member x.TryMinV() =
        let mutable minKey = Unchecked.defaultof<_>
        let mutable minValue = Unchecked.defaultof<_>
        if MapExtImplementation.tryGetMin &minKey &minValue root then
            ValueSome struct(minKey, minValue)
        else    
            ValueNone
        
    member x.TryMaxV() =
        let mutable maxKey = Unchecked.defaultof<_>
        let mutable maxValue = Unchecked.defaultof<_>
        if MapExtImplementation.tryGetMax &maxKey &maxValue root then
            ValueSome struct(maxKey, maxValue)
        else    
            ValueNone

            
    member x.TryMinKey() =
        let mutable minKey = Unchecked.defaultof<_>
        let mutable minValue = Unchecked.defaultof<_>
        if MapExtImplementation.tryGetMin &minKey &minValue root then
            Some minKey
        else    
            None
        
    member x.TryMaxKey() =
        let mutable maxKey = Unchecked.defaultof<_>
        let mutable maxValue = Unchecked.defaultof<_>
        if MapExtImplementation.tryGetMax &maxKey &maxValue root then
            Some maxKey
        else    
            None
            
    member x.TryMinKeyV() =
        let mutable minKey = Unchecked.defaultof<_>
        let mutable minValue = Unchecked.defaultof<_>
        if MapExtImplementation.tryGetMin &minKey &minValue root then
            ValueSome minKey
        else    
            ValueNone
        
    member x.TryMaxKeyV() =
        let mutable maxKey = Unchecked.defaultof<_>
        let mutable maxValue = Unchecked.defaultof<_>
        if MapExtImplementation.tryGetMax &maxKey &maxValue root then
            ValueSome maxKey
        else    
            ValueNone
            
    member x.TryMinValue() =
        let mutable minKey = Unchecked.defaultof<_>
        let mutable minValue = Unchecked.defaultof<_>
        if MapExtImplementation.tryGetMin &minKey &minValue root then
            Some minValue
        else    
            None
        
    member x.TryMaxValue() =
        let mutable maxKey = Unchecked.defaultof<_>
        let mutable maxValue = Unchecked.defaultof<_>
        if MapExtImplementation.tryGetMax &maxKey &maxValue root then
            Some maxValue
        else    
            None
    member x.TryRemoveMin() =
        if isNull root then 
            None
        else 
            let mutable k = Unchecked.defaultof<_>
            let mutable v = Unchecked.defaultof<_>
            let rest = MapExtImplementation.unsafeRemoveMin &k &v root
            Some (k,v,MapExt(comparer, rest))
            
    member x.TryRemoveMax() =
        if isNull root then 
            None
        else 
            let mutable k = Unchecked.defaultof<_>
            let mutable v = Unchecked.defaultof<_>
            let rest = MapExtImplementation.unsafeRemoveMax &k &v root
            Some (k,v,MapExt(comparer, rest))
            
    member x.TryRemoveMinV() =
        if isNull root then 
            ValueNone
        else 
            let mutable k = Unchecked.defaultof<_>
            let mutable v = Unchecked.defaultof<_>
            let rest = MapExtImplementation.unsafeRemoveMin &k &v root
            ValueSome struct(k,v,MapExt(comparer, rest))
            
    member x.TryRemoveMaxV() =
        if isNull root then 
            ValueNone
        else 
            let mutable k = Unchecked.defaultof<_>
            let mutable v = Unchecked.defaultof<_>
            let rest = MapExtImplementation.unsafeRemoveMax &k &v root
            ValueSome struct(k,v,MapExt(comparer, rest))

    
    
    member x.ToSeq() =
        MapExtImplementation.MapExtMappingEnumerable(root, fun n -> (n.Key, n.Value)) :> seq<_>
            
    member x.ToSeqV() =
        MapExtImplementation.MapExtMappingEnumerable(root, fun n -> struct(n.Key, n.Value)) :> seq<_>
        
    member x.ToKeySeq() =
        MapExtImplementation.MapExtMappingEnumerable(root, fun n -> n.Key) :> seq<_>

    member x.ToValueSeq() =
        MapExtImplementation.MapExtMappingEnumerable(root, fun n -> n.Value) :> seq<_>
        
        
    member x.ToSeqBack() =
        MapExtImplementation.MapExtBackwardMappingEnumerable(root, fun n -> (n.Key, n.Value)) :> seq<_>
            
    member x.ToSeqBackV() =
        MapExtImplementation.MapExtBackwardMappingEnumerable(root, fun n -> struct(n.Key, n.Value)) :> seq<_>
            
    member x.ToKeySeqBack() =
        MapExtImplementation.MapExtBackwardMappingEnumerable(root, fun n -> n.Key) :> seq<_>
            
    member x.ToValueSeqBack() =
        MapExtImplementation.MapExtBackwardMappingEnumerable(root, fun n -> n.Value) :> seq<_>
            
        
    member x.ToListBack() =
        MapExtImplementation.toListBack [] root
            
    member x.ToListBackV() =
        MapExtImplementation.toListBackV [] root
            
    member x.ToKeyListBack() =
        MapExtImplementation.toKeyListBack [] root
            
    member x.ToValueListBack() =
        MapExtImplementation.toValueListBack [] root
            

    member x.ToList() =
        MapExtImplementation.toList [] root
            
    member x.ToListV() =
        MapExtImplementation.toListV [] root
        
    member x.ToValueList() =
        MapExtImplementation.toValueList [] root
        
    member x.ToKeyList() =
        MapExtImplementation.toKeyList [] root

        
    member x.ToArrayBack() =
        let arr = Array.zeroCreate (MapExtImplementation.count root)
        MapExtImplementation.copyToBackward arr 0 root |> ignore
        arr
        
    member x.ToArrayBackV() =
        let arr = Array.zeroCreate (MapExtImplementation.count root)
        MapExtImplementation.copyToBackwardV arr 0 root |> ignore
        arr
        
    member x.ToValueArrayBack() =
        let arr = Array.zeroCreate (MapExtImplementation.count root)
        MapExtImplementation.copyValuesTo arr 0 root |> ignore
        arr
        
    member x.ToKeyArrayBack() =
        let arr = Array.zeroCreate (MapExtImplementation.count root)
        MapExtImplementation.copyKeysTo arr 0 root |> ignore
        arr

    member x.CopyTo(dst : ('Key * 'Value)[], index : int) =
        MapExtImplementation.copyTo dst index root |> ignore

    member x.CopyToV(dst : struct('Key * 'Value)[], index : int) =
        MapExtImplementation.copyToV dst index root |> ignore
        
    member x.CopyValuesTo(dst : 'Value[], index : int) =
        MapExtImplementation.copyValuesTo dst index root |> ignore

    member x.CopyKeysTo(dst : 'Key[], index : int) =
        MapExtImplementation.copyKeysTo dst index root |> ignore


    member x.ToArray() =
        let arr = Array.zeroCreate (MapExtImplementation.count root)
        MapExtImplementation.copyTo arr 0 root |> ignore
        arr
        
    member x.ToArrayV() =
        let arr = Array.zeroCreate (MapExtImplementation.count root)
        MapExtImplementation.copyToV arr 0 root |> ignore
        arr
        
    member x.ToValueArray() =
        let arr = Array.zeroCreate (MapExtImplementation.count root)
        MapExtImplementation.copyValuesTo arr 0 root |> ignore
        arr
        
    member x.ToKeyArray() =
        let arr = Array.zeroCreate (MapExtImplementation.count root)
        MapExtImplementation.copyKeysTo arr 0 root |> ignore
        arr
        
    member x.Iter(action : 'Key -> 'Value -> unit) =
        let action = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(action)
        MapExtImplementation.iter action root
        
    member x.IterValue(action : 'Value -> unit) =
        MapExtImplementation.iterValue action root
        
    member x.Fold(state : 'State, folder : 'State -> 'Key -> 'Value -> 'State) =
        let folder = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(folder)
        MapExtImplementation.fold state folder root
        
    member x.FoldBack(state : 'State, folder : 'Key -> 'Value -> 'State -> 'State) =
        let folder = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(folder)
        MapExtImplementation.foldBack state folder root

    member x.Map(mapping : 'Key -> 'Value -> 'T) =
        let mapping = OptimizedClosures.FSharpFunc<_,_,_>.Adapt mapping
        MapExt(comparer, MapExtImplementation.map mapping root)
        
    member x.MapMonotonicV(mapping : 'Key -> 'Value -> struct('K * 'V)) =
        let mapping = OptimizedClosures.FSharpFunc<_,_,_>.Adapt mapping
        MapExt(LanguagePrimitives.FastGenericComparer, MapExtImplementation.mapMonotonic mapping root)
        
    member x.MapMonotonic(mapping : 'Key -> 'Value -> ('K * 'V)) =
        let mapping = OptimizedClosures.FSharpFunc<_,_,_>.Adapt mapping
        let mapping = 
            OptimizedClosures.FSharpFunc<_,_,_>.Adapt (fun k v -> 
                let (a,b) = mapping.Invoke(k,v)
                struct(a,b)
            )
        MapExt(LanguagePrimitives.FastGenericComparer, MapExtImplementation.mapMonotonic mapping root)

    member x.ChooseV(mapping : 'Key -> 'Value -> voption<'T>) =
        let mapping = OptimizedClosures.FSharpFunc<_,_,_>.Adapt mapping
        MapExt(comparer, MapExtImplementation.chooseV mapping root)

    member x.Choose(mapping : 'Key -> 'Value -> option<'T>) =
        let mapping = OptimizedClosures.FSharpFunc<_,_,_>.Adapt mapping
        MapExt(comparer, MapExtImplementation.choose mapping root)

    member x.Filter(predicate : 'Key -> 'Value -> bool) =
        let predicate = OptimizedClosures.FSharpFunc<_,_,_>.Adapt predicate
        MapExt(comparer, MapExtImplementation.filter predicate root)
        
    member x.TryPick(mapping : 'Key -> 'Value -> option<'T>) =
        let mapping = OptimizedClosures.FSharpFunc<_,_,_>.Adapt mapping
        MapExtImplementation.tryPick mapping root
        
    member x.TryPickV(mapping : 'Key -> 'Value -> voption<'T>) =
        let mapping = OptimizedClosures.FSharpFunc<_,_,_>.Adapt mapping
        MapExtImplementation.tryPickV mapping root

        
    member x.TryPickBack(mapping : 'Key -> 'Value -> option<'T>) =
        let mapping = OptimizedClosures.FSharpFunc<_,_,_>.Adapt mapping
        MapExtImplementation.tryPickBack mapping root
        
    member x.TryPickBackV(mapping : 'Key -> 'Value -> voption<'T>) =
        let mapping = OptimizedClosures.FSharpFunc<_,_,_>.Adapt mapping
        MapExtImplementation.tryPickBackV mapping root

    member x.Exists(predicate : 'Key -> 'Value -> bool) =
        let predicate = OptimizedClosures.FSharpFunc<_,_,_>.Adapt predicate
        MapExtImplementation.exists predicate root
        
    member x.ForAll(predicate : 'Key -> 'Value -> bool) =
        let predicate = OptimizedClosures.FSharpFunc<_,_,_>.Adapt predicate
        MapExtImplementation.forall predicate root
        
    member x.Partition(predicate : 'Key -> 'Value -> bool) =
        let predicate = OptimizedClosures.FSharpFunc<_,_,_>.Adapt predicate
        let mutable t = null
        let mutable f = null
        MapExtImplementation.partition predicate &t &f root
        MapExt(comparer, t), MapExt(comparer, f)



    member x.WithMin(min : 'Key) = MapExt(comparer, MapExtImplementation.withMin comparer min root)
    member x.WithMax(max : 'Key) = MapExt(comparer, MapExtImplementation.withMax comparer max root)
    member x.Slice(min : 'Key, max : 'Key) = MapExt(comparer, MapExtImplementation.slice comparer min max root)
    member x.SliceAt(min : int, max : int) = MapExt(comparer, MapExtImplementation.sliceAt min max root)
    
    
    member x.Take(n : int) =
        MapExt(comparer, MapExtImplementation.take n root)
        
    member x.Skip(n : int) =
        MapExt(comparer, MapExtImplementation.skip n root)

    member x.Split(key : 'Key) =
        let mutable l = Unchecked.defaultof<_>
        let mutable s = Unchecked.defaultof<_>
        let mutable r = Unchecked.defaultof<_>
        if MapExtImplementation.split comparer key &l &s &r root then
            MapExt(comparer, l), Some s, MapExt(comparer, r)
        else
            MapExt(comparer, l), None, MapExt(comparer, r)
            
    member x.SplitV(key : 'Key) =
        let mutable l = Unchecked.defaultof<_>
        let mutable s = Unchecked.defaultof<_>
        let mutable r = Unchecked.defaultof<_>
        if MapExtImplementation.split comparer key &l &s &r root then
            struct(MapExt(comparer, l), ValueSome s, MapExt(comparer, r))
        else
            struct(MapExt(comparer, l), ValueNone, MapExt(comparer, r))

    

    member x.ReplaceRangeV(min : 'Key, max : 'Key, replace : voption<struct('Key * 'Value)> -> voption<struct('Key * 'Value)> -> struct(voption<'Value> * voption<'Value>)) =
        let c = comparer.Compare(min, max) 
        if c > 0 then
            // min > max
            x
        elif c < 0 then
            // min < max
            let newRoot = MapExtImplementation.replaceRange comparer min max ValueNone ValueNone replace root
            MapExt(comparer, newRoot)
        else
            // min = max
            let replacement l _ r =
                let struct(a, b) = replace l r
                match b with
                | ValueNone -> a
                | b -> b

            let newRoot = MapExtImplementation.changeWithNeighbours comparer max ValueNone ValueNone replacement root
            MapExt(comparer, newRoot)

    member x.ReplaceRange(min : 'Key, max : 'Key, replace : option<('Key * 'Value)> -> option<('Key * 'Value)> -> (option<'Value> * option<'Value>)) =
        let inline v (o : option<_>) =
            match o with
            | Some v -> ValueSome v
            | None -> ValueNone
        
        let replacement (l : voption<_>) (r : voption<_>) =
            match l with
            | ValueSome struct(lk, lv) ->
                match r with
                | ValueSome struct(rk, rv) ->
                    let (l, r) = replace (Some (lk, lv)) (Some (rk, rv))
                    struct(v l, v r)
                | ValueNone ->
                    let (l, r) = replace (Some (lk, lv)) None
                    struct(v l, v r)
            | ValueNone ->
                match r with
                | ValueSome struct(rk, rv) ->
                    let (l, r) = replace None (Some (rk, rv))
                    struct(v l, v r)
                | ValueNone ->
                    let (l, r) = replace None None
                    struct(v l, v r)
               
        x.ReplaceRangeV(min, max, replacement)

    member x.ChangeWithNeighboursV(key : 'Key, replace : voption<struct('Key * 'Value)> -> voption<'Value> -> voption<struct('Key * 'Value)> -> voption<'Value>) =
        let newRoot = MapExtImplementation.changeWithNeighbours comparer key ValueNone ValueNone replace root
        MapExt(comparer, newRoot)

    member x.ChangeWithNeighbours(key : 'Key, replace : option<('Key * 'Value)> -> option<'Value> -> option<('Key * 'Value)> -> option<'Value>) =
        x.ChangeWithNeighboursV(key, fun l s r ->
            let l = match l with | ValueSome struct(k,v) -> Some (k,v) | _ -> None
            let r = match r with | ValueSome struct(k,v) -> Some (k,v) | _ -> None
            let s = match s with | ValueSome v -> Some v | _ -> None
            match replace l s r with
            | Some v -> ValueSome v
            | None -> ValueNone
        )

    member x.GetSlice(min : option<'Key>, max : option<'Key>) =
        match min with
        | Some min ->
            match max with
            | Some max -> x.Slice(min, max)
            | None -> x.WithMin min
        | None ->
            match max with
            | Some max -> x.WithMax max
            | None -> x

    member x.Neighbours(key : 'Key) =
        let mutable lKey = Unchecked.defaultof<_>
        let mutable rKey = Unchecked.defaultof<_>
        let mutable lValue = Unchecked.defaultof<_>
        let mutable rValue = Unchecked.defaultof<_>
        let mutable sValue = Unchecked.defaultof<_>
        let flags = MapExtImplementation.getNeighbours comparer key MapExtImplementation.NeighbourFlags.None &lKey &lValue &sValue &rKey &rValue root

        let left =
            if flags.HasFlag MapExtImplementation.NeighbourFlags.Left then Some (lKey, lValue)
            else None
            
        let self =
            if flags.HasFlag MapExtImplementation.NeighbourFlags.Self then Some (sValue)
            else None
            
        let right =
            if flags.HasFlag MapExtImplementation.NeighbourFlags.Right then Some (rKey, rValue)
            else None

        left, self, right
        

    member x.NeighboursV(key : 'Key) =
        let mutable lKey = Unchecked.defaultof<_>
        let mutable rKey = Unchecked.defaultof<_>
        let mutable lValue = Unchecked.defaultof<_>
        let mutable rValue = Unchecked.defaultof<_>
        let mutable sValue = Unchecked.defaultof<_>
        let flags = MapExtImplementation.getNeighbours comparer key MapExtImplementation.NeighbourFlags.None &lKey &lValue &sValue &rKey &rValue root

        let left =
            if flags.HasFlag MapExtImplementation.NeighbourFlags.Left then ValueSome struct(lKey, lValue)
            else ValueNone
            
        let self =
            if flags.HasFlag MapExtImplementation.NeighbourFlags.Self then ValueSome (sValue)
            else ValueNone
            
        let right =
            if flags.HasFlag MapExtImplementation.NeighbourFlags.Right then ValueSome struct(rKey, rValue)
            else ValueNone

        struct(left, self, right)

    member x.NeighboursAt(index : int) =
        let mutable lKey = Unchecked.defaultof<_>
        let mutable rKey = Unchecked.defaultof<_>
        let mutable sKey = Unchecked.defaultof<_>
        let mutable lValue = Unchecked.defaultof<_>
        let mutable rValue = Unchecked.defaultof<_>
        let mutable sValue = Unchecked.defaultof<_>
        let flags = MapExtImplementation.getNeighboursAt index MapExtImplementation.NeighbourFlags.None &lKey &lValue &sKey &sValue &rKey &rValue root

        let left =
            if flags.HasFlag MapExtImplementation.NeighbourFlags.Left then Some (lKey, lValue)
            else None
            
        let self =
            if flags.HasFlag MapExtImplementation.NeighbourFlags.Self then Some (sKey, sValue)
            else None
            
        let right =
            if flags.HasFlag MapExtImplementation.NeighbourFlags.Right then Some (rKey, rValue)
            else None

        left, self, right
        
    member x.NeighboursAtV(index : int) =
        let mutable lKey = Unchecked.defaultof<_>
        let mutable rKey = Unchecked.defaultof<_>
        let mutable sKey = Unchecked.defaultof<_>
        let mutable lValue = Unchecked.defaultof<_>
        let mutable rValue = Unchecked.defaultof<_>
        let mutable sValue = Unchecked.defaultof<_>
        let flags = MapExtImplementation.getNeighboursAt index MapExtImplementation.NeighbourFlags.None &lKey &lValue &sKey &sValue &rKey &rValue root

        let left =
            if flags.HasFlag MapExtImplementation.NeighbourFlags.Left then ValueSome struct(lKey, lValue)
            else ValueNone
            
        let self =
            if flags.HasFlag MapExtImplementation.NeighbourFlags.Self then ValueSome struct(sKey, sValue)
            else ValueNone
            
        let right =
            if flags.HasFlag MapExtImplementation.NeighbourFlags.Right then ValueSome struct(rKey, rValue)
            else ValueNone

        struct(left, self, right)
        


    static member Union(l : MapExt<'Key, 'Value>, r : MapExt<'Key, 'Value>) =
        let cmp = defaultComparer
        MapExt(cmp, MapExtImplementation.union cmp l.Root r.Root)
        
    static member UnionWith(l : MapExt<'Key, 'Value>, r : MapExt<'Key, 'Value>, resolve : 'Key -> 'Value -> 'Value -> 'Value) =
        let cmp = defaultComparer
        let resolve = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt resolve
        MapExt(cmp, MapExtImplementation.unionWith cmp resolve l.Root r.Root)

        
    static member Choose2V(l : MapExt<'Key, 'A>, r : MapExt<'Key, 'B>, mapping : 'Key -> voption<'A> -> voption<'B> -> voption<'T>) =
        let cmp = defaultComparer
        let mapping = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt mapping

        let both = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt (fun k l r -> mapping.Invoke(k, ValueSome l, ValueSome r))
        let onlyLeft = OptimizedClosures.FSharpFunc<_,_,_>.Adapt (fun k l -> mapping.Invoke(k, ValueSome l, ValueNone))
        let onlyRight = OptimizedClosures.FSharpFunc<_,_,_>.Adapt (fun k r -> mapping.Invoke(k, ValueNone, ValueSome r))

        MapExt(cmp, MapExtImplementation.choose2 cmp onlyLeft both onlyRight l.Root r.Root)

         
    static member Choose2(l : MapExt<'Key, 'A>, r : MapExt<'Key, 'B>, mapping : 'Key -> option<'A> -> option<'B> -> option<'T>) =
        let cmp = defaultComparer
        let mapping = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt mapping

        let inline oo o = match o with | Some v -> ValueSome v | None -> ValueNone

        let both = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt (fun k l r -> mapping.Invoke(k, Some l, Some r) |> oo)
        let onlyLeft = OptimizedClosures.FSharpFunc<_,_,_>.Adapt (fun k l -> mapping.Invoke(k, Some l, None) |> oo)
        let onlyRight = OptimizedClosures.FSharpFunc<_,_,_>.Adapt (fun k r -> mapping.Invoke(k, None, Some r) |> oo)

        MapExt(cmp, MapExtImplementation.choose2 cmp onlyLeft both onlyRight l.Root r.Root)

    member x.ComputeDeltaTo( r : MapExt<'Key, 'Value2>, 
                             add : 'Key -> 'Value2 -> 'OP, 
                             update : 'Key -> 'Value -> 'Value2 -> voption<'OP>,
                             remove : 'Key -> 'Value -> 'OP) =
        let add = OptimizedClosures.FSharpFunc<_,_,_>.Adapt add
        let update = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt update
        let remove = OptimizedClosures.FSharpFunc<_,_,_>.Adapt remove
        MapExt(comparer, MapExtImplementation.computeDelta comparer root r.Root update add remove)
        
    member x.ApplyDelta(delta : MapExt<'Key, 'OP>, apply : 'Key -> voption<'Value> -> 'OP -> voption<'Value>) =
        let apply = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt apply
        let applyNoState = OptimizedClosures.FSharpFunc<_,_,_>.Adapt (fun k o -> apply.Invoke(k, ValueNone, o))
        let applyReal = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt (fun k v o -> apply.Invoke(k, ValueSome v, o))
        MapExt(comparer, MapExtImplementation.ApplyDelta.applyDelta comparer root delta.Root applyNoState applyReal)
        
        
    member x.ApplyDeltaAndGetEffective(delta : MapExt<'Key, 'OP>, apply : 'Key -> voption<'Value> -> 'OP -> struct(voption<'Value> * voption<'OP2>)) =
        let apply = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt apply
        let applyNoState = OptimizedClosures.FSharpFunc<_,_,_>.Adapt (fun k o -> apply.Invoke(k, ValueNone, o))
        let applyReal = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt (fun k v o -> apply.Invoke(k, ValueSome v, o))

        let mutable effective = null
        let root = MapExtImplementation.ApplyDelta.applyDeltaAndGetEffective comparer root delta.Root applyNoState applyReal &effective
        MapExt(comparer, root), MapExt(comparer, effective)
        
    override x.GetHashCode() =
        MapExtImplementation.hash 0 root

    override x.Equals o =
        match o with
        | :? MapExt<'Key, 'Value> as o ->
            x.Count = o.Count &&
            MapExtImplementation.equals comparer root o.Root
        | _ ->
            false


    new() = MapExt(defaultComparer, null)
    new(key : 'Key, value : 'Value) =
        let cmp = defaultComparer
        MapExt(cmp, MapExtImplementation.Node(key, value))

    new(elements : seq<'Key * 'Value>) =
        let thing = MapExt.FromSeq elements
        MapExt(thing.Comparer, thing.Root)

    new(elements : list<'Key * 'Value>) =
        let thing = MapExt.FromList elements
        MapExt(thing.Comparer, thing.Root)

    new(elements : ('Key * 'Value)[]) =
        let thing = MapExt.FromArray elements
        MapExt(thing.Comparer, thing.Root)

        
    new(elements : seq<struct('Key * 'Value)>) =
        let thing = MapExt.FromSeqV elements
        MapExt(thing.Comparer, thing.Root)

    new(elements : list<struct('Key * 'Value)>) =
        let thing = MapExt.FromListV elements
        MapExt(thing.Comparer, thing.Root)

    new(elements : struct('Key * 'Value)[]) =
        let thing = MapExt.FromArrayV elements
        MapExt(thing.Comparer, thing.Root)     

    member x.GetEnumerator() = new MapExtEnumerator<_,_>(root)

    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = x.GetEnumerator() :> _
            
    interface System.Collections.Generic.IEnumerable<KeyValuePair<'Key, 'Value>> with
        member x.GetEnumerator() = x.GetEnumerator() :> _

    interface System.Collections.Generic.IReadOnlyCollection<KeyValuePair<'Key, 'Value>> with
        member x.Count = x.Count

    interface System.Collections.Generic.IReadOnlyDictionary<'Key, 'Value> with
        member x.Keys = MapExtImplementation.MapExtMappingEnumerable(root, fun n -> n.Key) :> seq<_>
        member x.Values = MapExtImplementation.MapExtMappingEnumerable(root, fun n -> n.Value) :> seq<_>
        member x.Item with get (key : 'Key) = x.[key]
        member x.ContainsKey(key : 'Key) = x.ContainsKey key
        member x.TryGetValue(key : 'Key, value : byref<'Value>) = x.TryGetValue(key, &value)
        


and internal MapExtEnumerator<'Key, 'Value> =
    struct
        val mutable internal Root : MapExtImplementation.Node<'Key, 'Value>
        val mutable internal Head : struct(MapExtImplementation.Node<'Key, 'Value> * bool)
        val mutable internal Tail : list<struct(MapExtImplementation.Node<'Key, 'Value> * bool)>
        val mutable internal CurrentNode : MapExtImplementation.Node<'Key, 'Value>
        
        member x.MoveNext() =
            let struct(n, deep) = x.Head
            if not (isNull n) then

                if n.Height > 1uy && deep then
                    let inner = n :?> MapExtImplementation.Inner<'Key, 'Value>

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
            KeyValuePair(x.CurrentNode.Key, x.CurrentNode.Value)

        interface System.Collections.IEnumerator with
            member x.MoveNext() = x.MoveNext()
            member x.Reset() = x.Reset()
            member x.Current = x.Current :> obj

        interface System.Collections.Generic.IEnumerator<KeyValuePair<'Key, 'Value>> with
            member x.Current = x.Current
            member x.Dispose() = x.Dispose()

        new(root : MapExtImplementation.Node<'Key, 'Value>) =
            {
                Root = root
                Head = if isNull root then Unchecked.defaultof<_> else struct(root, true)
                Tail = []
                CurrentNode = null
            }
    end


module internal MapExt =
    let empty<'Key, 'Value when 'Key : comparison> = MapExt<'Key, 'Value>.Empty
    
    let singleton (key : 'Key) (value : 'Value) = MapExt<'Key, 'Value>(key, value)

    let isEmpty (map : MapExt<'Key, 'Value>) = map.IsEmpty
    let count (map : MapExt<'Key, 'Value>) = map.Count

    let add (key : 'Key) (value : 'Value) (map : MapExt<'Key, 'Value>) = map.Add(key, value)
    let change (key : 'Key) (update : option<'Value> -> option<'Value>) (map : MapExt<'Key, 'Value>) = map.Change(key, update)
    let changeV (key : 'Key) (update : voption<'Value> -> voption<'Value>) (map : MapExt<'Key, 'Value>) = map.Change(key, update)

    let remove (key : 'Key) (map : MapExt<'Key, 'Value>) = map.Remove(key)
    let tryRemove (key : 'Key) (map : MapExt<'Key, 'Value>) = map.TryRemove(key)
    let tryRemoveV (key : 'Key) (map : MapExt<'Key, 'Value>) = map.TryRemoveV(key)
    
    let removeAt (index : int) (map : MapExt<'Key, 'Value>) = map.RemoveAt(index)
    let tryRemoveAt (index : int) (map : MapExt<'Key, 'Value>) = map.TryRemoveAt(index)
    let tryRemoveAtV (index : int) (map : MapExt<'Key, 'Value>) = map.TryRemoveAtV(index)

    let tryRemoveMin (map : MapExt<'Key, 'Value>) = map.TryRemoveMin()
    let tryRemoveMinV (map : MapExt<'Key, 'Value>) = map.TryRemoveMinV()
    let tryRemoveMax (map : MapExt<'Key, 'Value>) = map.TryRemoveMax()
    let tryRemoveMaxV (map : MapExt<'Key, 'Value>) = map.TryRemoveMaxV()


    let tryFind (key : 'Key) (map : MapExt<'Key, 'Value>) = map.TryFind key
    let tryFindV (key : 'Key) (map : MapExt<'Key, 'Value>) = map.TryFindV key
    let find (key : 'Key) (map : MapExt<'Key, 'Value>) = map.Find key
    
    let tryItem (index : int) (map : MapExt<'Key, 'Value>) = map.TryItem index
    let tryItemV (index : int) (map : MapExt<'Key, 'Value>) = map.TryItemV index
    let item (index : int) (map : MapExt<'Key, 'Value>) = map.GetItem index
    let itemV (index : int) (map : MapExt<'Key, 'Value>) = map.GetItemV index
    
    let getIndex (key : 'Key) (map : MapExt<'Key, 'Value>) = map.GetIndex key
    let tryGetIndex (key : 'Key) (map : MapExt<'Key, 'Value>) = map.TryGetIndex key
    let tryGetIndexV (key : 'Key) (map : MapExt<'Key, 'Value>) = map.TryGetIndexV key

    let containsKey (key : 'Key) (map : MapExt<'Key, 'Value>) = map.ContainsKey key

    let tryMin (map : MapExt<'Key, 'Value>) = map.TryMin()
    let tryMax (map : MapExt<'Key, 'Value>) = map.TryMax()
    let tryMinV (map : MapExt<'Key, 'Value>) = map.TryMinV()
    let tryMaxV (map : MapExt<'Key, 'Value>) = map.TryMaxV()
    let minKey (map : MapExt<'Key, 'Value>) = map.GetMinKey()
    let maxKey (map : MapExt<'Key, 'Value>) = map.GetMaxKey()
    let tryMinKeyV (map : MapExt<'Key, 'Value>) = map.TryMinKeyV()
    let tryMaxKeyV (map : MapExt<'Key, 'Value>) = map.TryMaxKeyV()

    let iter (action : 'Key -> 'Value -> unit) (map : MapExt<'Key, 'Value>) = map.Iter action
    let iterValue (action : 'Value -> unit) (map : MapExt<'Key, 'Value>) = map.IterValue action
    let fold (folder : 'State -> 'Key -> 'Value -> 'State) (state : 'State) (map : MapExt<'Key, 'Value>) = map.Fold(state, folder)
    let foldBack (folder : 'Key -> 'Value -> 'State -> 'State) (map : MapExt<'Key, 'Value>) (state : 'State) =  map.FoldBack(state, folder)
    
    let map (mapping : 'Key -> 'Value -> 'T) (map : MapExt<'Key, 'Value>) = map.Map mapping
    let choose (mapping : 'Key -> 'Value -> option<'T>) (map : MapExt<'Key, 'Value>) = map.Choose mapping
    let chooseV (mapping : 'Key -> 'Value -> voption<'T>) (map : MapExt<'Key, 'Value>) = map.ChooseV mapping
    let filter (predicate : 'Key -> 'Value -> bool) (map : MapExt<'Key, 'Value>) = map.Filter predicate
    
    let tryPick (mapping : 'Key -> 'Value -> option<'T>) (map : MapExt<'Key, 'Value>) = map.TryPick mapping
    let tryPickV (mapping : 'Key -> 'Value -> voption<'T>) (map : MapExt<'Key, 'Value>) = map.TryPickV mapping
    
    let tryPickBack (mapping : 'Key -> 'Value -> option<'T>) (map : MapExt<'Key, 'Value>) = map.TryPickBack mapping
    let tryPickBackV (mapping : 'Key -> 'Value -> voption<'T>) (map : MapExt<'Key, 'Value>) = map.TryPickBackV mapping

    let exists (predicate : 'Key -> 'Value -> bool) (map : MapExt<'Key, 'Value>) = map.Exists predicate
    let forall (predicate : 'Key -> 'Value -> bool) (map : MapExt<'Key, 'Value>) = map.ForAll predicate
    let partition (predicate : 'Key -> 'Value -> bool) (map : MapExt<'Key, 'Value>) = map.Partition predicate
    
    let withMin (min : 'Key) (map : MapExt<'Key, 'Value>) = map.WithMin min
    let withMax (min : 'Key) (map : MapExt<'Key, 'Value>) = map.WithMax min
    let slice (min : 'Key) (max : 'Key) (map : MapExt<'Key, 'Value>) = map.Slice(min, max)
    let skip (n : int) (map : MapExt<'Key, 'Value>) = map.Skip n
    let take (n : int) (map : MapExt<'Key, 'Value>) = map.Take n

    let replaceRange (min : 'Key) (max : 'Key) (replacement : option<'Key * 'Value> -> option<'Key * 'Value> -> (option<'Value> * option<'Value>)) (map : MapExt<'Key, 'Value>) =
        map.ReplaceRange(min, max, replacement)

    let replaceRangeV (min : 'Key) (max : 'Key) (replacement : voption<struct('Key * 'Value)> -> voption<struct('Key * 'Value)> -> struct(voption<'Value> * voption<'Value>)) (map : MapExt<'Key, 'Value>) =
        map.ReplaceRangeV(min, max, replacement)
        
    let changeWithNeighbours (key : 'Key) (update : option<'Key * 'Value> -> option<'Value> -> option<'Key * 'Value> -> option<'Value>) (map : MapExt<'Key, 'Value>) =
        map.ChangeWithNeighbours(key, update)
        
    let changeWithNeighboursV (key : 'Key) (update : voption<struct('Key * 'Value)> -> voption<'Value> -> voption<struct('Key * 'Value)> -> voption<'Value>) (map : MapExt<'Key, 'Value>) =
        map.ChangeWithNeighboursV(key, update)

    let neighbours (key : 'Key) (map : MapExt<'Key, 'Value>) = map.Neighbours key
    let neighboursV (key : 'Key) (map : MapExt<'Key, 'Value>) = map.NeighboursV key
    let neighboursAt (index : int) (map : MapExt<'Key, 'Value>) = map.NeighboursAt index
    let neighboursAtV (index : int) (map : MapExt<'Key, 'Value>) = map.NeighboursAtV index


    let ofSeq (elements : seq<'Key * 'Value>) = MapExt.FromSeq elements
    let ofSeqV (elements : seq<struct('Key * 'Value)>) = MapExt.FromSeqV elements
    let ofList (elements : list<'Key * 'Value>) = MapExt.FromList elements
    let ofListV (elements : list<struct('Key * 'Value)>) = MapExt.FromListV elements
    let ofArray (elements : ('Key * 'Value)[]) = MapExt.FromArray elements
    let ofArrayV (elements : struct('Key * 'Value)[]) = MapExt.FromArrayV elements
    
    let toSeq (map : MapExt<'Key, 'Value>) = map.ToSeq()
    let toSeqV (map : MapExt<'Key, 'Value>) = map.ToSeqV()
    let toList (map : MapExt<'Key, 'Value>) = map.ToList()
    let toListV (map : MapExt<'Key, 'Value>) = map.ToListV()
    let toArray (map : MapExt<'Key, 'Value>) = map.ToArray()
    let toArrayV (map : MapExt<'Key, 'Value>) = map.ToArrayV()
    
    let toValueSeq (map : MapExt<'Key, 'Value>) = map.ToValueSeq()
    let toValueList (map : MapExt<'Key, 'Value>) = map.ToValueList()
    let toValueArray (map : MapExt<'Key, 'Value>) = map.ToValueArray()
    let toKeySeq (map : MapExt<'Key, 'Value>) = map.ToKeySeq()
    let toKeyList (map : MapExt<'Key, 'Value>) = map.ToKeyList()
    let toKeyArray (map : MapExt<'Key, 'Value>) = map.ToKeyArray()

    let toSeqBack (map : MapExt<'Key, 'Value>) = map.ToSeqBack()
    let toSeqBackV (map : MapExt<'Key, 'Value>) = map.ToSeqBackV()
    let toKeySeqBack (map : MapExt<'Key, 'Value>) = map.ToKeySeqBack()
    let toValueSeqBack (map : MapExt<'Key, 'Value>) = map.ToValueSeqBack()
    
    let toListBack (map : MapExt<'Key, 'Value>) = map.ToListBack()
    let toListBackV (map : MapExt<'Key, 'Value>) = map.ToListBackV()
    let toKeyListBack (map : MapExt<'Key, 'Value>) = map.ToKeyListBack()
    let toValueListBack (map : MapExt<'Key, 'Value>) = map.ToValueListBack()
    
    let union (l : MapExt<'Key, 'Value>) (r : MapExt<'Key, 'Value>) = MapExt.Union(l, r)
    let unionMany (maps : #seq<MapExt<'Key, 'Value>>) = (empty, maps) ||> Seq.fold union
    let unionWith (resolve : 'Key -> 'Value -> 'Value -> 'Value) (l : MapExt<'Key, 'Value>) (r : MapExt<'Key, 'Value>) = MapExt.UnionWith(l, r, resolve)
    
    let choose2V (mapping : 'Key -> voption<'A> -> voption<'B> -> voption<'C>) (a : MapExt<'Key, 'A>) (b : MapExt<'Key, 'B>) =
        MapExt<'Key, 'A>.Choose2V(a, b, mapping)
        
    let choose2 (mapping : 'Key -> option<'A> -> option<'B> -> option<'C>) (a : MapExt<'Key, 'A>) (b : MapExt<'Key, 'B>) =
        MapExt<'Key, 'A>.Choose2(a, b, mapping)

    let computeDelta 
        (add : 'Key -> 'Value2 -> 'OP)
        (update : 'Key -> 'Value1 -> 'Value2 -> voption<'OP>)
        (remove : 'Key -> 'Value1 -> 'OP)
        (l : MapExt<'Key, 'Value1>) (r : MapExt<'Key, 'Value2>) =
        l.ComputeDeltaTo(r, add, update, remove)
                     
    let applyDelta 
        (apply : 'Key -> voption<'Value> -> 'OP -> voption<'Value>)
        (state : MapExt<'Key, 'Value>) (delta : MapExt<'Key, 'OP>) =
        state.ApplyDelta(delta, apply)
                                
    let applyDeltaAndGetEffective
        (apply : 'Key -> voption<'Value> -> 'OP -> struct(voption<'Value> * voption<'OP2>))
        (state : MapExt<'Key, 'Value>) (delta : MapExt<'Key, 'OP>) =
        state.ApplyDeltaAndGetEffective(delta, apply)
                                
