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
   
/// arrdelta holds a sequence of minimal ArrOperations that represent the difference between two arrs.
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
    
    module Arr =
        let computeDelta (equal : 'a -> 'a -> bool) (src : arr<'a>) (dst : arr<'a>) : arrdelta<'a> =
            let srcArr = Arr.toArray src
            let dstArr = Arr.toArray dst

            let mutable steps = DeltaOperationList.ofArrayMyers equal srcArr dstArr

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

        let applyDelta (state : arr<'a>) (delta : arrdelta<'a>) =
            let mutable res = state
            for op in delta do
                res <- res.ReplaceRange(op.Index, op.Count, op.Elements)
            res
   