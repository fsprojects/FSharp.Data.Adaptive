namespace FSharp.Data.Adaptive

#nowarn "7331"

open System
open System.Diagnostics

open ArrNodeImplementation
open FSharp.Data.Adaptive.ComputeListDeltaHelpers

/// ArrOperation represents an operation on an arr.
/// The Operations replaces the range given by Index and Count with the Elements.
/// Note that 'Insert' and 'Delete' operations are represented as ArrOperation with Count = 0 and Elements = [] respectively.
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
   
    static member TryMerge(a : ArrOperation<'a>, b : ArrOperation<'a>) =
      
        let ai0 = a.Index
        let ai1 = ai0 + a.Elements.Length - 1
        let b0 = b.Index
        let bn1 = b0 + b.Count - 1
        
        
        
        if b0 = ai1 + 1 then
            // directly adjacent
            Some { a with Count = a.Count + b.Count; Elements = arr(Node.join a.Elements.Store b.Elements.Store) }
        
        
        elif b0 >= ai0 && b0 <= ai1 then
            if bn1 <= ai1 then
                // b is contained in a
                let newElements =
                    let idx = b0 - ai0
                    if b.Count = 0 then Node.insertRange idx b.Elements.Store a.Elements.Store
                    elif b.Elements.IsEmpty then Node.removeRange idx (idx + b.Count - 1) a.Elements.Store
                    else Node.replaceRange idx (idx + b.Count - 1) b.Elements.Store a.Elements.Store
                Some { a with Elements = arr newElements }
            else
                // b starts in a
                let rest = Node.take (b0 - ai0) a.Elements.Store
                let overflowCount = bn1 - ai1
                Some { a with Count = a.Count + overflowCount; Elements = arr(Node.join rest b.Elements.Store) }
                
        elif b0 < ai0 && bn1 >= ai0 then
            if bn1 <= ai1 then
                // b ends in a
                
                // splice(3, 1, [a;b;c]) + splice(1, 4, [x;y])
                // 1 2 3 4 5 6 7 8 9 0
                // 1 2 3 a b c 5 6 7 8 9 0 
                // 1 x y c 5 6 7 8 9 0
                
                let delCnt = bn1 - ai0 + 1
                let newElements = Node.join b.Elements.Store (Node.skip delCnt a.Elements.Store)
                Some { b with Count = b.Count + a.Count - delCnt; Elements = arr newElements }
                
                
                // let br = { b with Count = b.Count - delCnt }
                //
                // Some { a with Elements = arr newElements }
            else
                // b contains a
                
                // splice(3, 1, [a;b]) + splice(0, 10, [x;y;Z])
                
                // 1 2 3 4 5 6 7 8 9 0
                // 1 2 3 a b 5 6 7 8 9 0 
                // x y z 0
                
                let ba = a.Elements.Length - a.Count
                Some { b with Count = b.Count - ba }
                
        else
            None
            
    
    static member TryMergeV(a : ArrOperation<'a>, b : ArrOperation<'a>) =
      
        let ai0 = a.Index
        let ai1 = ai0 + a.Elements.Length - 1
        let b0 = b.Index
        let bn1 = b0 + b.Count - 1
        
        
        
        if b0 = ai1 + 1 then
            // directly adjacent
            ValueSome { a with Count = a.Count + b.Count; Elements = arr(Node.join a.Elements.Store b.Elements.Store) }
        
        
        elif b0 >= ai0 && b0 <= ai1 then
            if bn1 <= ai1 then
                // b is contained in a
                let newElements =
                    let idx = b0 - ai0
                    if b.Count = 0 then Node.insertRange idx b.Elements.Store a.Elements.Store
                    elif b.Elements.IsEmpty then Node.removeRange idx (idx + b.Count - 1) a.Elements.Store
                    else Node.replaceRange idx (idx + b.Count - 1) b.Elements.Store a.Elements.Store
                ValueSome { a with Elements = arr newElements }
            else
                // b starts in a
                let rest = Node.take (b0 - ai0) a.Elements.Store
                let overflowCount = bn1 - ai1
                ValueSome { a with Count = a.Count + overflowCount; Elements = arr(Node.join rest b.Elements.Store) }
                
        elif b0 < ai0 && bn1 >= ai0 then
            if bn1 <= ai1 then
                // b ends in a
                
                // splice(3, 1, [a;b;c]) + splice(1, 4, [x;y])
                // 1 2 3 4 5 6 7 8 9 0
                // 1 2 3 a b c 5 6 7 8 9 0 
                // 1 x y c 5 6 7 8 9 0
                
                let delCnt = bn1 - ai0 + 1
                let newElements = Node.join b.Elements.Store (Node.skip delCnt a.Elements.Store)
                ValueSome { b with Count = b.Count + a.Count - delCnt; Elements = arr newElements }
                
                
                // let br = { b with Count = b.Count - delCnt }
                //
                // Some { a with Elements = arr newElements }
            else
                // b contains a
                
                // splice(3, 1, [a;b]) + splice(0, 10, [x;y;Z])
                
                // 1 2 3 4 5 6 7 8 9 0
                // 1 2 3 a b 5 6 7 8 9 0 
                // x y z 0
                
                let ba = a.Elements.Length - a.Count
                ValueSome { b with Count = b.Count - ba }
                
        else
            ValueNone
                 
   
/// arrdelta holds a sequence of minimal ArrOperations that represent the difference between two arrs.
[<Struct; DebuggerTypeProxy(typedefof<ArrDeltaProxy<_>>); StructuredFormatDisplay("{AsString}"); CustomEquality; NoComparison>]
type arrdelta<'a> internal(store : Node<ArrOperation<'a>>) =
    member internal x.Store = store
    member x.Length = Node.count store

    override x.GetHashCode() =
        Node.hash DefaultEquality.hash 0 store
        
    override x.Equals(o) =
        match o with
        | :? arrdelta<'a> as o -> Node.equals DefaultEquality.equals store o.Store
        | _ -> false
    
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

/// Debugger proxy for arrdelta
and internal ArrDeltaProxy<'a>(delta : arrdelta<'a>) =
    let items = delta.ToArr().Take(10000).ToArray()
    [<DebuggerBrowsable(DebuggerBrowsableState.RootHidden)>]
    member x.Items = items

module ArrDelta =

    let isEmpty (d : arrdelta<'a>) = d.Length = 0
    
    let empty<'a> : arrdelta<'a> = arrdelta null

    let single (value : ArrOperation<'a>) = arrdelta(Node(1uy, value))
    
    let toSeq (a : arrdelta<'a>) = a :> seq<_>
    let toList (a : arrdelta<'a>) = Node.toList [] a.Store
    let toArray (a : arrdelta<'a>) = Node.toArray a.Store

    let ofArr (a : arr<ArrOperation<'a>>) = arrdelta(a.Store)
    
    module internal Node =
        
        let rec binary (l : Node<ArrOperation<'a>>) (op : ArrOperation<'a>) (r : Node<ArrOperation<'a>>) =
            if isNull l then
                if isNull r then
                    if op.IsEmpty then null
                    else Node(1uy, op)
                else
                    let struct(r0, rr) = Node.unsafeRemoveMin r
                    match ArrOperation.TryMergeV(op, r0) with
                    | ValueSome op ->
                        binary null op rr
                    | ValueNone ->
                        if op.IsEmpty then r
                        else Node.binary null op r
                        
            elif isNull r then
                let struct(ln, ll) = Node.unsafeRemoveMax l
                match ArrOperation.TryMergeV(ln, op) with
                | ValueSome op ->
                    binary ll op null
                | ValueNone ->
                    if op.IsEmpty then l
                    else Node.binary l op null
            else
                let struct(r0, rr) = Node.unsafeRemoveMin r
                let struct(ln, ll) = Node.unsafeRemoveMax l
                
                match ArrOperation.TryMergeV(ln, op) with
                | ValueSome op ->
                    match ArrOperation.TryMergeV(op, r0) with
                    | ValueSome op ->
                        binary ll op rr
                    | ValueNone ->
                        binary ll op r
                | ValueNone ->
                    match ArrOperation.TryMergeV(op, r0) with
                    | ValueSome op ->
                        binary l op rr
                    | ValueNone ->
                        if op.IsEmpty then Node.join l r
                        else Node.binary l op r
                
        let rec insert (op : ArrOperation<'a>) (n : Node<ArrOperation<'a>>) =
            if op.IsEmpty then
                n, true
            else
                if isNull n then
                    Node(1uy, op), true
                    
                elif n.Height = 1uy then
                    match ArrOperation.TryMergeV(n.Value, op) with
                    | ValueSome res ->
                        if res.IsEmpty then null, true
                        else Node(1uy, res), true
                    | ValueNone ->
                        if op.Index < n.Value.Index then
                            Node(1uy, { n.Value with Index = n.Value.Index + op.Balance }), false
                        else
                            binary null n.Value (Node(1uy, op)), true
                else
                    let n = n :?> Inner<ArrOperation<'a>>
                
                    if op.MaxIndex < n.Value.Index then
                        let r = Node.map (fun x -> { x with Index = x.Index + op.Balance }) n.Right
                        let s = { n.Value with Index = n.Value.Index + op.Balance }
                        let l, rest = insert op n.Left
                        binary l s r, rest
                    else
                        let r, rest = insert op n.Right
                        match rest with
                        | false ->
                            match ArrOperation.TryMergeV(n.Value, op) with
                            | ValueSome res ->
                                if res.IsEmpty then Node.join n.Left r, true
                                else binary n.Left res r, true
                            | ValueNone ->
                                if op.Index < n.Value.Index then
                                    let l, rest = insert op n.Left
                                    binary l { n.Value with Index = n.Value.Index + op.Balance } r, rest
                                else
                                    let l, newRest = insert n.Value n.Left
                                    binary l op r, newRest
                        | true ->
                            binary n.Left n.Value r, true
                  
                
                    
                
                
        
        
    
    let combine (a : arrdelta<'a>) (b : arrdelta<'a>) : arrdelta<'a> = 
        let mutable res = a.Store
        for op in b do
            let r, rest = Node.insert op res
            res <- 
                match rest with
                | false ->
                    if op.IsEmpty then r
                    else Node.binary null op r
                | true -> r
        arrdelta res
        //
        //
        // use mutable ea = a.GetEnumerator()
        // use mutable eb = b.GetEnumerator()
        //
        // let mutable va = ea.MoveNext()
        // let mutable vb = eb.MoveNext()
        //
        // let mutable abalance = 0
        //
        // let mutable pending : option<ArrOperation<'a>> = None
        // let mutable final = Arr.empty
        //
        // let inline flush() =
        //     match pending with
        //     | Some last ->
        //         if last.Count > 0 || last.Elements.Length > 0 then final <- Arr.add last final
        //         pending <- None
        //     | None ->
        //         ()
        //
        // let append c =
        //     match pending with
        //     | Some last ->
        //         
        //         
        //         if last.Index = c.Index  then
        //             let n = { Index = last.Index; Count = last.Count + (c.Count - last.Elements.Length); Elements = arr (Node.join c.Elements.Store (Node.skip c.Count last.Elements.Store)) }
        //             pending <- Some n
        //         else
        //             let l = last.Index 
        //             let h = last.Index + last.Elements.Length - 1
        //             
        //             if c.Index = h + 1 then
        //                 let newOp =
        //                     { Index = last.Index; Count = last.Count + c.Count; Elements = arr(Node.join last.Elements.Store c.Elements.Store) }
        //                 pending <- Some newOp
        //             
        //             elif c.Index >= l && c.Index <= h then
        //                 let li = c.Index - l
        //                 let remLocal = last.Elements.Length - li
        //                 let remRest = c.Count - remLocal
        //
        //                 let newLastElements = last.Elements.ReplaceRange(li, remLocal, c.Elements)
        //                 pending <- Some { Index = last.Index; Count = last.Count + remRest; Elements = newLastElements }
        //                 
        //             else
        //                 flush()
        //                 pending <- Some c
        //     | None ->
        //         pending <- Some c
        //
        // while va && vb do
        //     let a0 = { ea.Current with Index = ea.Current.Index + abalance }
        //     let b0 = eb.Current
        //
        //     if a0.Index <= b0.Index then
        //         append a0
        //         va <- ea.MoveNext()
        //     else
        //         append b0
        //         abalance <- abalance + b0.Balance
        //         vb <- eb.MoveNext()
        //
        // while va do
        //     let a0 = { ea.Current with Index = ea.Current.Index + abalance }
        //     append a0
        //     va <- ea.MoveNext()
        //
        // while vb do
        //     let b0 = eb.Current
        //     append b0
        //     vb <- eb.MoveNext()
        //
        // flush()
        //
        // arrdelta final

    let map (mapping : 'a -> 'b) (delta : arrdelta<'a>) =
        delta.Store |> Node.map (fun op ->
            { Index = op.Index; Count = op.Count; Elements = op.Elements |> Arr.map mapping }    
        ) |> arrdelta

    let mapOp (mapping : ArrOperation<'a> -> ArrOperation<'b>) (delta : arrdelta<'a>) =
        delta.Store |> Node.map (fun op ->
            mapping op
        ) |> arrdelta

[<AutoOpen>]
module ``ArrDelta Extensions`` =
    
    module Arr =
        let computeDelta (cmp : System.Collections.Generic.IEqualityComparer<'a>) (src : arr<'a>) (dst : arr<'a>) : arrdelta<'a> =
            let srcArr = Arr.toArray src
            let dstArr = Arr.toArray dst

            let mutable steps =
                if srcArr.Length = 0 && dstArr.Length = 0 then DeltaOperationList.DeltaOperationList.Empty
                else DeltaOperationList.ofArrayMyersComparer cmp srcArr dstArr

            let mutable si = 0
            let mutable di = 0
            let mutable balance = 0
            let mutable delta = Arr.empty<ArrOperation<'a>>

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

        let applyDeltaAndGetEffective (cmp : System.Collections.Generic.IEqualityComparer<'a>) (state : arr<'a>) (delta : arrdelta<'a>) =
            let mutable effective = ArrDelta.empty
            let mutable res = state
            for op in delta do
                res <-
                    res.UpdateRange(op.Index, op.Count, fun old ->
                        let real =
                            computeDelta cmp old op.Elements
                            |> ArrDelta.mapOp (fun oo -> { oo with Index = oo.Index + op.Index })
                        effective <- ArrDelta.combine effective real
                        op.Elements
                    )
            res, effective
        
        let applyDelta (state : arr<'a>) (delta : arrdelta<'a>) =
            let mutable res = state
            for op in delta do
                res <- res.ReplaceRange(op.Index, op.Count, op.Elements)
            res
   