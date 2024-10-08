namespace FSharp.Data.Adaptive

#nowarn "7331"

open System
open System.Diagnostics
open FSharp.Data.Adaptive.ComputeListDeltaHelpers
           
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
    type AggregateArr<'a, 's> internal(a : Aggregator<'a, 's>, store : AggregateNode<'a, 's>) =
        static member Empty(a : Aggregator<'a, 's>) : AggregateArr<'a, 's> = AggregateArr<'a, 's>(a, null)
        
        member internal x.Store = store
        member x.IsEmpty = AggregateNode.isEmpty store
        member x.Length = AggregateNode.count store
        
        member x.InsertAt(index : int, value : 'a) = AggregateArr(a, AggregateNode.insertAt a index value store)
        member x.Prepend(value : 'a) = AggregateArr(a, AggregateNode.prepend a value store)
        member x.Append(value : 'a) = AggregateArr(a, AggregateNode.append a value store)
        member x.Add(value : 'a) = x.Append(value)
        member x.Set(i : int, value : 'a) =
            if i < 0 || i >= x.Length then raise <| IndexOutOfRangeException()
            else AggregateArr(a, AggregateNode.set a i value store)
        member x.RemoveAt(index : int) = AggregateArr(a, AggregateNode.removeRange a index index store)
        
        member x.Skip(n : int) =
            if n <= 0 then x
            elif n >= x.Length then AggregateArr.Empty a
            else AggregateArr(a, AggregateNode.skip a n store)
            
        member x.Take(n : int) =
            if n <= 0 then AggregateArr.Empty a
            elif n < x.Length then AggregateArr(a, AggregateNode.take a n store)
            else x
            
            
        member x.Sub(offset : int, count : int) =
            AggregateArr(a, AggregateNode.sub a offset (offset + count - 1) store)

        member x.TryRemove(index : int) =
            match AggregateNode.tryRemove a index store with
            | Some (node, value) ->
                Some(AggregateArr(a, node), value)
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
        
        let thing = AggregateArr.Empty(a).Add([1;2;3]).Add([4;5;6]).Add([2]).Add([10])
        
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
        
        let thing = AggregateArr.Empty(a).Add(1).Add(2).Add(3).Add(4).Add(5)
        
        printfn "%A" (thing.ToArray())
        match thing.TryRemove(1) with
        | Some (a, b) ->
            printfn "%A" (a.ToArray())
            printfn "%A" b
        | None ->
            printfn "out of bounds"
        
  
            
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

    let perform (op : ArrOperation<'T>) =
        history.Perform (ArrDelta.single op)
    
    override x.ToString() =
        history.State |> Seq.map (sprintf "%A") |> String.concat "; " |> sprintf "carr [%s]"

    /// is the list currently empty?
    member x.IsEmpty = history.State.IsEmpty

    /// the number of elements currently in the list.
    member x.Length = history.State.Length

    member x.Value
        with get() = history.State
        and set v = history.Perform (Arr.trace.tcomputeDelta history.State v) |> ignore

    member x.Add(value : 'T) =
        perform { Index = history.State.Length; Count = 0; Elements = Arr.single value } |> ignore
    
    member x.Prepend(value : 'T) =
        perform { Index = 0; Count = 0; Elements = Arr.single value } |> ignore
    
    member x.Insert(index : int, value : 'T) =
        if index < 0 || index > history.State.Length then raise <| IndexOutOfRangeException()
        perform { Index = index; Count = 0; Elements = Arr.single value } |> ignore
    
    member x.Remove(index : int) =
        if index < 0 || index >= history.State.Length then raise <| IndexOutOfRangeException()
        perform { Index = index; Count = 1; Elements = Arr.empty } |> ignore
    
    interface IAdaptiveArray<'T> with
        member x.GetReader() = history.NewReader()
        member x.Content = history :> aval<_>
        member x.IsConstant = false
        member x.History = Some history


module AArr =
    module Readers = 
        /// Efficient implementation for a constant adaptive array.
        [<Sealed>]
        type ConstantArray<'T>(content : Lazy<arr<'T>>) =
            let value = AVal.delay (fun () -> content.Value)

            member x.Content = value

            member x.GetReader() =
                History.Readers.ConstantReader<_,_>(
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
            let reader = History.Readers.EmptyReader<arr<'T>, arrdelta<'T>>(Arr.trace) :> IArrayReader<'T>
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
            let mutable prefix = ArrNodeImplementationAggregate.AggregateArr(aggregate, null)
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
        Readers.ConstantArray(lazy value()) :> aarr<_> 

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
        printfn "even <- [|4;4|]"
        print()
        
        transact (fun () ->
            even.Remove 0    
        )
        printfn "even.Remove 0"
        print()
        
        
        
        
        
        
        