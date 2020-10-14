// THIS IS A MODIFIED VERSION OF F#'s Map<'Key, 'Value> !!!!
// THE ORIGINAL CAN BE FOUND AT https://github.com/fsharp/fsharp/blob/master/src/fsharp/FSharp.Core/map.fs

// Copyright (c) Microsoft Corporation.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace FSharp.Data.Adaptive

open System
open System.Collections.Generic
open System.Diagnostics
open Microsoft.FSharp.Core
open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
open System.Runtime.CompilerServices

module internal MapExtImplementation = 
    [<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
    [<NoEquality; NoComparison>]
    type MapTree<'Key,'Value> = 
        | MapEmpty 
        | MapOne of 'Key * 'Value
        | MapNode of 'Key * 'Value * MapTree<'Key,'Value> *  MapTree<'Key,'Value> * int * int
            // REVIEW: performance rumour has it that the data held in MapNode and MapOne should be
            // exactly one cache line. It is currently ~7 and 4 words respectively. 

    type MapExtReference<'v> =
        | NonExisting of index : int
        | Existing of index : int * value : 'v


    type internal EnumeratorEnumerable<'T>(get : unit -> IEnumerator<'T>) =
        interface System.Collections.IEnumerable with
            member x.GetEnumerator() = get() :> System.Collections.IEnumerator

        interface IEnumerable<'T> with
            member x.GetEnumerator() = get()

    

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module MapTree = 

        let empty = MapEmpty 

        let height = function
            | MapEmpty -> 0
            | MapOne _ -> 1
            | MapNode(_,_,_,_,h,_) -> h

        let size = function
            | MapEmpty -> 0
            | MapOne _ -> 1
            | MapNode(_,_,_,_,_,s) -> s

        let isEmpty m = 
            match m with 
            | MapEmpty -> true
            | _ -> false

        let mk l k v r = 
            match l,r with 
            | MapEmpty,MapEmpty -> MapOne(k,v)
            | _ -> 
                let hl = height l 
                let hr = height r 
                let m = if hl < hr then hr else hl 
                let res = MapNode(k,v,l,r,m+1, 1 + size l + size r)
                res

        let rec rebalance t1 k v t2 =
            let t1h = height t1 
            let t2h = height t2 
            if  t2h > t1h + 2 then (* right is heavier than left *)
                match t2 with 
                | MapNode(t2k,t2v,t2l,t2r,_,_) -> 
                    (* one of the nodes must have height > height t1 + 1 *)
                   if height t2l > t1h + 1 then  (* balance left: combination *)
                       match t2l with 
                       | MapNode(t2lk,t2lv,t2ll,t2lr,_,_) ->
                           mk (mk t1 k v t2ll) t2lk t2lv (rebalance t2lr t2k t2v t2r) 
                       | _ -> failwith "rebalance"
                   else (* rotate left *)
                       mk (mk t1 k v t2l) t2k t2v t2r
                | _ -> failwith "rebalance"
            else
                if  t1h > t2h + 2 then (* left is heavier than right *)
                    match t1 with 
                    | MapNode(t1k,t1v,t1l,t1r,_,_) -> 
                        (* one of the nodes must have height > height t2 + 1 *)
                        if height t1r > t2h + 1 then 
                            (* balance right: combination *)
                            match t1r with 
                            | MapNode(t1rk,t1rv,t1rl,t1rr,_,_) ->
                                mk (rebalance t1l t1k t1v t1rl) t1rk t1rv (mk t1rr k v t2)
                            | _ -> failwith "rebalance"
                        else
                            mk t1l t1k t1v (mk t1r k v t2)
                    | _ -> failwith "rebalance"
                else mk t1 k v t2

        let rec add (comparer: IComparer<'Value>) k v m = 
            match m with 
            | MapEmpty -> MapOne(k,v)
            | MapOne(k2,_) -> 
                let c = comparer.Compare(k,k2) 
                if c < 0   then MapNode (k,v,MapEmpty,m,2, 2)
                elif c = 0 then MapOne(k,v)
                else            MapNode (k,v,m,MapEmpty,2, 2)
            | MapNode(k2,v2,l,r,h,s) -> 
                let c = comparer.Compare(k,k2) 
                if c < 0 then rebalance (add comparer k v l) k2 v2 r
                elif c = 0 then MapNode(k,v,l,r,h,s)
                else rebalance l k2 v2 (add comparer k v r) 

        let rec find (comparer: IComparer<'Value>) k m = 
            match m with 
            | MapEmpty -> raise (KeyNotFoundException())
            | MapOne(k2,v2) -> 
                let c = comparer.Compare(k,k2) 
                if c = 0 then v2
                else raise (KeyNotFoundException())
            | MapNode(k2,v2,l,r,_,_) -> 
                let c = comparer.Compare(k,k2) 
                if c < 0 then find comparer k l
                elif c = 0 then v2
                else find comparer k r

        let rec tryFind (comparer: IComparer<'Value>) k m = 
            match m with 
            | MapEmpty -> None
            | MapOne(k2,v2) -> 
                let c = comparer.Compare(k,k2) 
                if c = 0 then Some v2
                else None
            | MapNode(k2,v2,l,r,_,_) -> 
                let c = comparer.Compare(k,k2) 
                if c < 0 then tryFind comparer k l
                elif c = 0 then Some v2
                else tryFind comparer k r
                
        let rec tryFindV (comparer: IComparer<'Value>) k m = 
            match m with 
            | MapEmpty -> ValueNone
            | MapOne(k2,v2) -> 
                let c = comparer.Compare(k,k2) 
                if c = 0 then ValueSome v2
                else ValueNone
            | MapNode(k2,v2,l,r,_,_) -> 
                let c = comparer.Compare(k,k2) 
                if c < 0 then tryFindV comparer k l
                elif c = 0 then ValueSome v2
                else tryFindV comparer k r

        let partition1 (comparer: IComparer<'Value>) (f:OptimizedClosures.FSharpFunc<_,_,_>) k v (acc1,acc2) = 
            if f.Invoke(k, v) then (add comparer k v acc1,acc2) else (acc1,add comparer k v acc2) 
        
        let rec partitionAux (comparer: IComparer<'Value>) (f:OptimizedClosures.FSharpFunc<_,_,_>) s acc = 
            match s with 
            | MapEmpty -> acc
            | MapOne(k,v) -> partition1 comparer f k v acc
            | MapNode(k,v,l,r,_,_) -> 
                let acc = partitionAux comparer f r acc 
                let acc = partition1 comparer f k v acc
                partitionAux comparer f l acc

        let partition (comparer: IComparer<'Value>) f s = partitionAux comparer (OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)) s (empty,empty)

        let filter1 (comparer: IComparer<'Value>) (f:OptimizedClosures.FSharpFunc<_,_,_>) k v acc = if f.Invoke(k, v) then add comparer k v acc else acc 

        let rec filterAux (comparer: IComparer<'Value>) (f:OptimizedClosures.FSharpFunc<_,_,_>) s acc = 
            match s with 
            | MapEmpty -> acc
            | MapOne(k,v) -> filter1 comparer f k v acc
            | MapNode(k,v,l,r,_,_) ->
                let acc = filterAux comparer f l acc
                let acc = filter1 comparer f k v acc
                filterAux comparer f r acc

        let filter (comparer: IComparer<'Value>) f s = filterAux comparer (OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)) s empty

        let rec spliceOutSuccessor m = 
            match m with 
            | MapEmpty -> failwith "internal error: MapExt.spliceOutSuccessor"
            | MapOne(k2,v2) -> k2,v2,MapEmpty
            | MapNode(k2,v2,l,r,_,_) ->
                match l with 
                | MapEmpty -> k2,v2,r
                | _ -> let k3,v3,l' = spliceOutSuccessor l in k3,v3,rebalance l' k2 v2 r

        let rec remove (comparer: IComparer<'Value>) k m = 
            match m with 
            | MapEmpty -> empty
            | MapOne(k2,_) -> 
                let c = comparer.Compare(k,k2) 
                if c = 0 then MapEmpty else m
            | MapNode(k2,v2,l,r,_,_) -> 
                let c = comparer.Compare(k,k2) 
                if c < 0 then rebalance (remove comparer k l) k2 v2 r
                elif c = 0 then 
                    match l with
                    | MapEmpty -> r
                    | _ ->
                        match r with
                        | MapEmpty -> l
                        | _ -> 
                        let sk,sv,r' = spliceOutSuccessor r 
                        rebalance l sk sv r'
                else 
                    rebalance l k2 v2 (remove comparer k r) 


        let rec tryRemove (comparer: IComparer<'Value>) k m = 
            match m with 
            | MapEmpty -> None
            | MapOne(k2,v) -> 
                let c = comparer.Compare(k,k2) 
                if c = 0 then Some (v, MapEmpty) else None
            | MapNode(k2,v2,l,r,_,_) -> 
                let c = comparer.Compare(k,k2) 
                if c < 0 then 
                    match tryRemove comparer k l with
                    | Some (v,l) ->
                        Some (v, rebalance l k2 v2 r)
                    | None ->
                        None
                elif c = 0 then 
                    match l with
                    | MapEmpty -> Some(v2, r)
                    | _ ->
                        match r with
                        | MapEmpty -> Some(v2, l)
                        | _ -> 
                            let sk,sv,r' = spliceOutSuccessor r 
                            Some(v2, rebalance l sk sv r')
                else 
                    match tryRemove comparer k r with
                    | Some (v,r) ->
                        Some (v, rebalance l k2 v2 r)
                    | None ->
                        None

        let rec tryRemoveV (comparer: IComparer<'Value>) k m = 
            match m with 
            | MapEmpty -> ValueNone
            | MapOne(k2,v) -> 
                let c = comparer.Compare(k,k2) 
                if c = 0 then ValueSome (struct(v, MapEmpty)) else ValueNone
            | MapNode(k2,v2,l,r,_,_) -> 
                let c = comparer.Compare(k,k2) 
                if c < 0 then 
                    match tryRemoveV comparer k l with
                    | ValueSome (v,l) ->
                        ValueSome (struct (v, rebalance l k2 v2 r))
                    | ValueNone ->
                        ValueNone
                elif c = 0 then 
                    match l with
                    | MapEmpty -> ValueSome(struct(v2, r))
                    | _ ->
                        match r with
                        | MapEmpty -> ValueSome(struct(v2, l))
                        | _ -> 
                            let sk,sv,r' = spliceOutSuccessor r 
                            ValueSome(struct(v2, rebalance l sk sv r'))
                else 
                    match tryRemoveV comparer k r with
                    | ValueSome (v,r) ->
                        ValueSome (struct(v, rebalance l k2 v2 r))
                    | ValueNone ->
                        ValueNone



        let rec tryRemoveMin cmp m = 
            match m with 
            | MapEmpty -> 
                None

            | MapOne(k2,v) ->
                Some (k2, v, MapEmpty)

            | MapNode(k2,v2,l,r,_,_) -> 
                match tryRemoveMin cmp l with
                | Some (k,v,rest) ->
                    match rest with
                    | MapEmpty -> Some (k, v, add cmp k2 v2 r)
                    | _ -> Some (k, v, rebalance rest k2 v2 r)
                | None ->
                    Some(k2, v2, r)

        let rec tryRemoveMax cmp m = 
            match m with 
            | MapEmpty -> 
                None

            | MapOne(k2,v) ->
                Some (k2, v, MapEmpty)

            | MapNode(k2,v2,l,r,_,_) -> 
                match tryRemoveMax cmp r with
                | Some (k,v,rest) ->
                    match rest with
                    | MapEmpty -> Some (k, v, add cmp k2 v2 l)
                    | _ -> Some (k, v, rebalance l k2 v2 rest)
                | None ->
                    Some(k2, v2, l)


        let rec tryMinVAux acc m =
            match m with
            | MapEmpty -> acc
            | MapOne(k,v) -> ValueSome (struct(k,v))
            | MapNode(k,v,l,_,_,_) -> tryMinVAux (ValueSome (struct(k,v))) l

        let inline tryMinV m = tryMinVAux ValueNone m
        
        let rec tryMaxVAux acc m =
            match m with
            | MapEmpty -> acc
            | MapOne(k,v) -> ValueSome (struct(k,v))
            | MapNode(k,v,_,r,_,_) -> tryMaxVAux (ValueSome (struct(k,v))) r

        let inline tryMaxV m = tryMaxVAux ValueNone m

    
        
        let rec tryMinKeyVAux acc m =
            match m with
            | MapEmpty -> acc
            | MapOne(k,_) -> ValueSome k
            | MapNode(k,_,l,_,_,_) -> tryMinKeyVAux (ValueSome k) l

        let inline tryMinKeyV m = tryMinKeyVAux ValueNone m
        
        let rec tryMaxKeyVAux acc m =
            match m with
            | MapEmpty -> acc
            | MapOne(k,_) -> ValueSome k
            | MapNode(k,_,_,r,_,_) -> tryMaxKeyVAux (ValueSome k) r

        let inline tryMaxKeyV m = tryMaxKeyVAux ValueNone m

    

        let rec tryRemoveMinV cmp next m = 
            match m with 
            | MapEmpty -> 
                ValueNone

            | MapOne(k2,v) ->
                ValueSome (struct (k2, v, next, MapEmpty))

            | MapNode(k2,v2,l,r,_,_) -> 
                match tryRemoveMinV cmp (ValueSome k2) l with
                | ValueSome (k,v,next,rest) ->
                    match rest with
                    | MapEmpty -> ValueSome (struct(k, v, next, add cmp k2 v2 r))
                    | _ -> ValueSome (struct(k, v, next, rebalance rest k2 v2 r))
                | ValueNone ->
                    let next = tryMinKeyV r
                    ValueSome(struct(k2, v2, next, r))


        let rec tryRemoveMaxV cmp prev m = 
            match m with 
            | MapEmpty -> 
                ValueNone

            | MapOne(k2,v) ->
                ValueSome (struct(k2, v, prev, MapEmpty))

            | MapNode(k2,v2,l,r,_,_) -> 
                match tryRemoveMaxV cmp (ValueSome k2) r with
                | ValueSome (struct(k, v, prev, rest)) ->
                    match rest with
                    | MapEmpty -> ValueSome (struct(k, v, prev, add cmp k2 v2 l))
                    | _ -> ValueSome (struct(k, v, prev, rebalance l k2 v2 rest))
                | ValueNone ->
                    let prev = tryMaxKeyV l
                    ValueSome(k2, v2, prev, l)


        
        let rec alter (comparer : IComparer<'Value>) k f m =
            match m with   
            | MapEmpty ->
                match f None with
                    | Some v -> MapOne(k,v)
                    | None -> MapEmpty

            | MapOne(k2, v2) ->
                let c = comparer.Compare(k,k2) 
                if c = 0 then
                    match f (Some v2) with
                        | Some v3 -> MapOne(k2, v3)
                        | None -> MapEmpty
                else
                    match f None with
                        | None -> 
                            MapOne(k2, v2)

                        | Some v3 -> 
                            if c > 0 then MapNode (k2,v2,MapEmpty,MapOne(k, v3),2, 2)
                            else MapNode(k2, v2, MapOne(k, v3), MapEmpty, 2, 2)
            | MapNode(k2, v2, l, r, h, cnt) ->
            
                let c = comparer.Compare(k, k2)

                if c = 0 then
                    match f (Some v2) with
                        | Some v3 -> 
                            MapNode(k2, v3, l, r, h, cnt)

                        | None ->
                            match l with
                            | MapEmpty -> r
                            | _ ->
                                match r with
                                | MapEmpty -> l
                                | _ -> 
                                    let sk,sv,r' = spliceOutSuccessor r 
                                    rebalance l sk sv r'
                elif c > 0 then
                    rebalance l k2 v2 (alter comparer k f r) 
                else
                    rebalance (alter comparer k f l)  k2 v2 r
               
        let rec join left k v right =
            let lh = height left
            let rh = height right
            if lh > rh + 2 then
                match left with
                    | MapNode(k2,v2,l,r,_,_) ->
                        // the join-result can at most be one level higher than r
                        // therefore rebalance is sufficient here
                        rebalance l k2 v2 (join r k v right)
                    | _ ->
                        failwith "join"
            elif rh > lh + 2 then
                match right with
                    | MapNode(k2,v2,l,r,_,_) ->
                        // the join-result can at most be one level higher than l
                        // therefore rebalance is sufficient here
                        rebalance (join left k v l) k2 v2 r
                    | _ ->
                        failwith "join"
            else
                mk left k v right


        let inline merge l r =
            match r with
            | MapEmpty -> l
            | _ ->
                match l with
                | MapEmpty -> r
                | _ ->
                    let (k,v,r) = spliceOutSuccessor r
                    join l k v r

        let inline joinV left k v right =
            match v with
            | ValueSome v -> join left k v right
            | ValueNone -> merge left right

        let rec split (comparer: IComparer<'Value>) k m =
            match m with
            | MapEmpty -> 
                MapEmpty, None, MapEmpty

            | MapOne(k2,v2) ->
                let c = comparer.Compare(k, k2)
                if c < 0 then MapEmpty, None, MapOne(k2,v2)
                elif c = 0 then MapEmpty, Some(v2), MapEmpty
                else MapOne(k2,v2), None, MapEmpty

            | MapNode(k2,v2,l,r,_,_) ->
                let c = comparer.Compare(k, k2)
                if c > 0 then
                    let rl, res, rr = split comparer k r
                    join l k2 v2 rl, res, rr

                elif c = 0 then 
                    l, Some(v2), r

                else
                    let ll, res, lr = split comparer k l
                    ll, res, join lr k2 v2 r

        let inline private createTwo k0 v0 k1 v1 =
            match v0 with
            | ValueSome v0 ->
                match v1 with
                | ValueSome v1 ->
                    MapNode(k0, v0, MapEmpty, MapOne(k1, v1), 2, 2)
                | ValueNone ->
                    MapOne(k0, v0)
            | ValueNone ->
                match v1 with
                | ValueSome v1 ->
                    MapOne(k1, v1)
                | ValueNone ->
                    MapEmpty

                    
        let rec withMaxExclusive (comparer: IComparer<'Key>) (key : 'Key) (m : MapTree<'Key, 'Value>) =
            match m with
            | MapEmpty -> 
                struct(MapEmpty, ValueNone)

            | MapOne(k,v) ->
                let c = comparer.Compare(k, key)
                if c < 0 then struct(m, ValueNone)
                else struct(MapEmpty, ValueSome(struct(k, v)))

            | MapNode(k, v, l, r, _, _) ->
                let c = comparer.Compare(k, key)
                if c >= 0 then
                    let struct(rest, n) = withMaxExclusive comparer key l
                    match n with
                    | ValueNone -> struct(rest, ValueSome(struct(k, v)))
                    | ValueSome t -> struct(rest, ValueSome t)
                else
                    let struct(r1, n) = withMaxExclusive comparer key r
                    struct(rebalance l k v r1, n)

        let rec withMinExclusive (comparer: IComparer<'Key>) (key : 'Key) (m : MapTree<'Key, 'Value>) =
            match m with
            | MapEmpty -> 
                struct(ValueNone, MapEmpty)
            | MapOne(k, v) ->
                let c = comparer.Compare(k, key)
                if c > 0 then struct(ValueNone, m)
                else struct(ValueSome(struct(k, v)), MapEmpty)
            | MapNode(k, v, l, r, _, _) ->
                let c = comparer.Compare(k, key)
                if c <= 0 then
                    let struct(ln, l1) = withMinExclusive comparer key r
                    match ln with
                    | ValueSome t -> ValueSome t, l1
                    | ValueNone -> ValueSome(struct(k,v)), l1
                else
                    let struct(ln, l1) = withMinExclusive comparer key l
                    struct(ln, rebalance l1 k v r)
               
        let rec replaceRange 
            (comparer: IComparer<'Key>) 
            (replacement : OptimizedClosures.FSharpFunc<voption<struct('Key * 'Value)>, voption<'Value>, bool, voption<'Value>, voption<struct('Key * 'Value)>, struct(voption<'Value> * voption<'Value>)>) 
            (k0 : 'Key) (k1 : 'Key) 
            (l : voption<struct('Key * 'Value)>) (r : voption<struct('Key * 'Value)>)
            (m : MapTree<'Key, 'Value>) =
            match m with
            | MapEmpty -> 
                let struct(v0, v1) = replacement.Invoke(l, ValueNone, false, ValueNone, r)
                createTwo k0 v0 k1 v1

            | MapOne(k,v) ->
                let c = comparer.Compare(k, k0)

                if c < 0 then
                    let struct(v0, v1) = replacement.Invoke(ValueSome(struct(k,v)), ValueNone, false, ValueNone, r)
                    createTwo k0 v0 k1 v1
                    |> add comparer k v

                elif c = 0 then
                    let struct(v0, v1) = replacement.Invoke(l, ValueSome v, false, ValueNone, r)
                    createTwo k0 v0 k1 v1

                else
                    let c = comparer.Compare(k, k1)
                    if c > 0 then
                        let struct(v0, v1) = replacement.Invoke(l, ValueNone, false, ValueNone, ValueSome(struct(k,v)))
                        createTwo k0 v0 k1 v1
                        |> add comparer k v

                    elif c = 0 then 
                        let struct(v0, v1) = replacement.Invoke(l, ValueNone, false, ValueSome(v), r)
                        createTwo k0 v0 k1 v1

                    else
                        let struct(v0, v1) = replacement.Invoke(l, ValueNone, true, ValueNone, r)
                        createTwo k0 v0 k1 v1
                        
            | MapNode(k,v,lc,rc,_,_) ->
                let c0 = comparer.Compare(k, k0)
                let c1 = comparer.Compare(k, k1)
                if c0 < 0 then
                    rebalance lc k v (replaceRange comparer replacement k0 k1 (ValueSome(struct(k, v))) r rc)

                elif c1 > 0 then
                    rebalance (replaceRange comparer replacement k0 k1 l (ValueSome(struct(k, v))) lc) k v rc

                //elif c0 = 0 then
                //    let struct (inner, r1) = withMinExclusive comparer k1 r

                //    let kv1 =
                //        match inner with
                //        | ValueSome (kk, vv) when comparer.Compare(kk, k1) = 0 -> ValueSome vv
                //        | _ -> ValueNone

                //    let struct(v0, v1) = replacement.Invoke(tryMaxV l, ValueSome v, false, kv1, tryMinV r1)

                //    failwith ""
                else
                    let struct (l1, lv) = withMaxExclusive comparer k0 lc
                    let struct (rv, r1) = withMinExclusive comparer k1 rc
                    
                    let lv1 =
                        match lv with
                        | ValueSome (kk, vv) when comparer.Compare(kk, k0) = 0 -> ValueSome vv
                        | _ -> 
                            if c0 = 0 then ValueSome v
                            else ValueNone
                        
                    let rv1 =
                        match rv with
                        | ValueSome (kk, vv) when comparer.Compare(kk, k1) = 0 -> ValueSome vv
                        | _ -> 
                            if c1 = 0 then ValueSome v
                            else ValueNone
                        
                    let ln =
                        match tryMaxV l1 with
                        | ValueSome m -> ValueSome m
                        | ValueNone -> l
                        
                    let rn =
                        match tryMinV r1 with
                        | ValueSome m -> ValueSome m
                        | ValueNone -> r

                    let struct(v0, v1) = replacement.Invoke(ln, lv1, true, rv1, rn)

                    match v0 with
                    | ValueNone ->
                        match v1 with
                        | ValueNone ->
                            merge l1 r1
                        | ValueSome v1 ->
                            rebalance l1 k1 v1 r1
                    | ValueSome v0 ->
                        match v1 with
                        | ValueNone ->
                            rebalance l1 k0 v0 r1
                        | ValueSome v1 ->
                            rebalance l1 k0 v0 (add comparer k1 v1 r1)
                            

        let rec tryMinAux acc m =
            match m with
            | MapEmpty -> acc
            | MapOne(k,v) -> Some (k,v)
            | MapNode(k,v,l,_,_,_) -> tryMinAux (Some (k,v)) l

        let rec tryMin m = tryMinAux None m
    

        let rec tryMaxAux acc m =
            match m with
            | MapEmpty -> acc
            | MapOne(k,v) -> Some (k,v)
            | MapNode(k,v,_,r,_,_) -> tryMaxAux (Some (k,v)) r

        let rec tryMax m = tryMaxAux None m
        let rec splitV (comparer: IComparer<'Value>) k m =
            match m with
            | MapEmpty -> 
                struct (MapEmpty, ValueNone, ValueNone, ValueNone, MapEmpty)

            | MapOne(k2,v2) ->
                let c = comparer.Compare(k, k2)
                if c < 0 then struct (MapEmpty, ValueNone, ValueNone, ValueSome k2, MapOne(k2,v2))
                elif c = 0 then struct (MapEmpty, ValueNone, ValueSome(v2), ValueNone, MapEmpty)
                else struct (MapOne(k2,v2), ValueSome k2, ValueNone, ValueNone, MapEmpty)

            | MapNode(k2,v2,l,r,_,_) ->
                let c = comparer.Compare(k, k2)
                if c > 0 then
                    let struct (rl, lmax, res, rmin, rr) = splitV comparer k r
                    struct (join l k2 v2 rl, lmax, res, rmin, rr)

                elif c = 0 then 
                    let lmax = tryMaxV l |> ValueOption.map (fun struct(k,_) -> k)
                    let rmin = tryMinV r |> ValueOption.map (fun struct(k,_) -> k)
                    struct (l, lmax, ValueSome(v2), rmin, r)

                else
                    let struct (ll, lmax, res, rmin, lr) = splitV comparer k l
                    struct (ll, lmax, res, rmin, join lr k2 v2 r)

        let rec getReference (comparer: IComparer<'Value>) (current : int) k m =
            match m with
                | MapEmpty -> 
                    NonExisting current

                | MapOne(key,v) ->
                    let c = comparer.Compare(k, key)
                    
                    if c > 0 then NonExisting (current + 1)
                    elif c < 0 then NonExisting current
                    else Existing(current, v)

                | MapNode(key,v,l,r,_,s) ->
                    let c = comparer.Compare(k, key)
                    if c > 0 then getReference comparer (current + size l + 1) k r
                    elif c < 0 then getReference comparer current k l
                    else Existing(current+size l, v)


                    
        let rec union (comparer: IComparer<'Key>) l r =
            match struct (l, r) with
            | struct (MapEmpty, r) -> r
            | struct (l, MapEmpty) -> l
            | struct (MapOne(lk, _lv), MapOne(rk, rv)) ->
                if Object.ReferenceEquals(l, r) then 
                    r
                else 
                    let c = comparer.Compare(lk, rk)
                    if c = 0 then r
                    elif c > 0 then mk MapEmpty rk rv l
                    else mk l rk rv MapEmpty
                    
            | struct (MapOne(lk, _), MapNode(rk, rv, rl, rr, rh, rc)) ->
                let c = comparer.Compare(lk, rk)
                if c = 0 then r
                elif c < 0 then join (union comparer l rl) rk rv rr
                else join rl rk rv (union comparer l rr)

            | struct (MapNode(lk, lv, ll, lr, lh, lc), MapOne(rk, rv)) ->
                let c = comparer.Compare(lk, rk)
                if c = 0 then MapNode(lk, rv, ll, lr, lh, lc)
                elif c < 0 then join ll lk lv (union comparer lr r)
                else join (union comparer ll r) lk lv lr

            | struct (MapNode(lk, lv, ll, lr, lh, lc), MapNode(rk, rv, rl, rr, rh, rc)) ->
                if Object.ReferenceEquals(l, r) then 
                    r
                else
                    let c = comparer.Compare(lk, rk)
                    if c = 0 then 
                        let l = union comparer ll rl
                        let r = union comparer lr rr
                        join l rk rv r
                    elif lh > rh then
                        let struct (rl, _lmax, rv, _rmin, rr) = splitV comparer lk r
                        let key = lk
                        let l = union comparer ll rl
                        let r = union comparer lr rr

                        match rv with
                        | ValueSome rv -> join l key rv r
                        | ValueNone -> join l key lv r
                    else
                        let struct (ll, _lmax, _, _rmin, lr) = splitV comparer rk l
                        let key = rk
                        let l = union comparer ll rl
                        let r = union comparer lr rr
                        join l key rv r

        let rec unionWithOpt (comparer: IComparer<'Value>) (f : OptimizedClosures.FSharpFunc<_,_,_>) l r =
            match struct (l, r) with
            | struct (MapEmpty, r) -> r
            | struct (l, MapEmpty) -> l
            | struct (MapOne(lk, lv), MapOne(rk, rv)) ->
                let c = comparer.Compare(lk, rk)
                if c = 0 then MapOne(rk, f.Invoke(lv, rv))
                elif c > 0 then mk MapEmpty rk rv l
                else mk l rk rv MapEmpty
                    
            | struct (MapOne(lk, lv), MapNode(rk, rv, rl, rr, rh, rc)) ->
                let c = comparer.Compare(lk, rk)
                if c = 0 then MapNode(rk, f.Invoke(lv, rv), rl, rr, rh, rc)
                elif c < 0 then join (unionWithOpt comparer f l rl) rk rv rr
                else join rl rk rv (unionWithOpt comparer f l rr)

            | struct (MapNode(lk, lv, ll, lr, lh, lc), MapOne(rk, rv)) ->
                let c = comparer.Compare(lk, rk)
                if c = 0 then MapNode(lk, f.Invoke(lv, rv), ll, lr, lh, lc)
                elif c < 0 then join ll lk lv (unionWithOpt comparer f lr r)
                else join (unionWithOpt comparer f ll r) lk lv lr

            | struct (MapNode(lk, lv, ll, lr, lh, lc), MapNode(rk, rv, rl, rr, rh, rc)) ->
                let c = comparer.Compare(lk, rk)
                if c = 0 then 
                    let l = unionWithOpt comparer f ll rl
                    let r = unionWithOpt comparer f lr rr
                    join l rk (f.Invoke(lv, rv)) r
                elif lh > rh then
                    let struct (rl, _lmax, rv, _rmin, rr) = splitV comparer lk r
                    let key = lk
                    let l = unionWithOpt comparer f ll rl
                    let r = unionWithOpt comparer f lr rr

                    match rv with
                    | ValueSome rv -> join l key (f.Invoke(lv, rv)) r
                    | ValueNone -> join l key lv r
                else
                    let struct (ll, _lmax, lv, _rmin, lr) = splitV comparer rk l
                    let key = rk
                    let l = unionWithOpt comparer f ll rl
                    let r = unionWithOpt comparer f lr rr
                    match lv with
                    | ValueSome lv -> join l key (f.Invoke(lv, rv)) r
                    | ValueNone -> join l key rv r
  
        let unionWith(comparer: IComparer<'Value>) f l r =
            unionWithOpt comparer (OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)) l r



        let rec stupidHeight m =
            match m with
                | MapEmpty -> 0
                | MapOne _ -> 1
                | MapNode(_,_,l,r,_,_) -> max (stupidHeight l) (stupidHeight r) + 1

        let rec stupidCount m =
            match m with
                | MapEmpty -> 0
                | MapOne _ -> 1
                | MapNode(_,_,l,r,_,_) ->
                    1 + stupidCount l + stupidCount r

        let rec validateAux (comparer: IComparer<'Value>) (min : option<_>) (max : option<_>) m =
            match m with
                | MapNode(k,v,l,r,h,c) ->
                    let lh = height l
                    let rh = height r

                    if Option.isSome min && comparer.Compare(k, min.Value) <= 0 then failwith "invalid order"
                    if Option.isSome max && comparer.Compare(k, max.Value) >= 0 then failwith "invalid order"
                    if stupidCount m <> c then failwith "invalid count"
                    if stupidHeight l <> lh then failwith "invalid height"
                    if stupidHeight r <> rh then failwith "invalid height"
                    if abs (lh - rh) > 2 then failwith "imbalanced"   
                    
                    validateAux comparer min (Some k) l
                    validateAux comparer (Some k) max r

                | MapOne(k,v) ->
                    if Option.isSome min && comparer.Compare(k, min.Value) <= 0 then failwith "invalid order"
                    if Option.isSome max && comparer.Compare(k, max.Value) >= 0 then failwith "invalid order"

                | MapEmpty ->
                    ()

        let validate (comparer: IComparer<'Value>) m =
            validateAux comparer None None m


        let rec mem (comparer: IComparer<'Value>) k m = 
            match m with 
            | MapEmpty -> false
            | MapOne(k2,_) -> (comparer.Compare(k,k2) = 0)
            | MapNode(k2,_,l,r,_,_) -> 
                let c = comparer.Compare(k,k2) 
                if c < 0 then mem comparer k l
                else (c = 0 || mem comparer k r)

        let rec iterOpt (f:OptimizedClosures.FSharpFunc<_,_,_>) m =
            match m with 
            | MapEmpty -> ()
            | MapOne(k2,v2) -> f.Invoke(k2, v2)
            | MapNode(k2,v2,l,r,_,_) -> iterOpt f l; f.Invoke(k2, v2); iterOpt f r

        let iter f m = iterOpt (OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)) m
        
        let rec iterValue (f: 'v -> unit) m =
            match m with 
            | MapEmpty -> ()
            | MapOne(k2,v2) -> f v2
            | MapNode(k2,v2,l,r,_,_) -> iterValue f l; f v2; iterValue f r

        let rec tryPickOpt (f:OptimizedClosures.FSharpFunc<_,_,_>) m =
            match m with 
            | MapEmpty -> None
            | MapOne(k2,v2) -> f.Invoke(k2, v2) 
            | MapNode(k2,v2,l,r,_,_) -> 
                match tryPickOpt f l with 
                | Some _ as res -> res 
                | None -> 
                match f.Invoke(k2, v2) with 
                | Some _ as res -> res 
                | None -> 
                tryPickOpt f r

        let tryPick f m = tryPickOpt (OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)) m

        let rec tryPickOptBack (f:OptimizedClosures.FSharpFunc<_,_,_>) m =
            match m with 
            | MapEmpty -> None
            | MapOne(k2,v2) -> f.Invoke(k2, v2) 
            | MapNode(k2,v2,l,r,_,_) -> 
                match tryPickOptBack f r with 
                | Some _ as res -> res 
                | None -> 
                match f.Invoke(k2, v2) with 
                | Some _ as res -> res 
                | None -> 
                tryPickOptBack f l

        let tryPickBack f m = tryPickOptBack (OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)) m


        let rec existsOpt (f:OptimizedClosures.FSharpFunc<_,_,_>) m = 
            match m with 
            | MapEmpty -> false
            | MapOne(k2,v2) -> f.Invoke(k2, v2)
            | MapNode(k2,v2,l,r,_,_) -> existsOpt f l || f.Invoke(k2, v2) || existsOpt f r

        let exists f m = existsOpt (OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)) m

        let rec forallOpt (f:OptimizedClosures.FSharpFunc<_,_,_>) m = 
            match m with 
            | MapEmpty -> true
            | MapOne(k2,v2) -> f.Invoke(k2, v2)
            | MapNode(k2,v2,l,r,_,_) -> forallOpt f l && f.Invoke(k2, v2) && forallOpt f r

        let forall f m = forallOpt (OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)) m

        let rec map f m = 
            match m with 
            | MapEmpty -> empty
            | MapOne(k,v) -> MapOne(k,f v)
            | MapNode(k,v,l,r,h,c) -> 
                let l2 = map f l 
                let v2 = f v 
                let r2 = map f r 
                MapNode(k,v2,l2, r2,h,c)

        let rec mapiOpt (f:OptimizedClosures.FSharpFunc<_,_,_>) m = 
            match m with
            | MapEmpty -> empty
            | MapOne(k,v) -> MapOne(k, f.Invoke(k, v))
            | MapNode(k,v,l,r,h,c) -> 
                let l2 = mapiOpt f l 
                let v2 = f.Invoke(k, v) 
                let r2 = mapiOpt f r 
                MapNode(k,v2, l2, r2,h,c)

        let mapi f m = mapiOpt (OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)) m

        let rec mapiMonotonicAux (f:OptimizedClosures.FSharpFunc<_,_,_>) m =
            match m with
            | MapEmpty -> empty
            | MapOne(k,v) -> 
                let (k2, v2) = f.Invoke(k, v)
                MapOne(k2, v2)
            | MapNode(k,v,l,r,h,c) -> 
                let l2 = mapiMonotonicAux f l 
                let k2, v2 = f.Invoke(k, v) 
                let r2 = mapiMonotonicAux f r 
                MapNode(k2,v2, l2, r2,h,c)

        let mapiMonotonic f m = mapiMonotonicAux (OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)) m
    

        let rec chooseiMonotonicAux (f:OptimizedClosures.FSharpFunc<_,_,_>) m =
            match m with
            | MapEmpty -> empty
            | MapOne(k,v) -> 
                match f.Invoke(k, v) with
                | Some (k2, v2) ->
                    MapOne(k2, v2)
                | None ->
                    MapEmpty
            | MapNode(k,v,l,r,h,c) -> 
                let l2 = chooseiMonotonicAux f l 
                let self = f.Invoke(k, v) 
                let r2 = chooseiMonotonicAux f r 
                match self with
                | Some (k2, v2) ->
                    join l2 k2 v2 r2
                | None ->
                    match l2 with
                    | MapEmpty -> r2
                    | _ ->
                        match r2 with
                        | MapEmpty -> l2
                        | _ ->
                            let k,v,r2 = spliceOutSuccessor r2
                            join l2 k v r2

        let chooseiMonotonic f m = chooseiMonotonicAux (OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)) m
    
        let rec chooseiOpt (f:OptimizedClosures.FSharpFunc<'Key,'T1,option<'T2>>) m =
            match m with
            | MapEmpty -> empty
            | MapOne(k,v) ->
                match f.Invoke(k,v) with
                | Some v -> MapOne(k,v)
                | None -> MapEmpty

            | MapNode(k,v,l,r,h,c) ->
                let l' = chooseiOpt f l
                let s' = f.Invoke(k,v)
                let r' = chooseiOpt f r
                match s' with
                | None -> merge l' r'
                | Some v -> join l' k v r'

        let choosei f m = chooseiOpt (OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)) m
    
        let rec chooseiOptV (f:OptimizedClosures.FSharpFunc<'Key,'T1,voption<'T2>>) m =
            match m with
            | MapEmpty -> empty
            | MapOne(k,v) ->
                match f.Invoke(k,v) with
                | ValueSome v -> MapOne(k,v)
                | ValueNone -> MapEmpty

            | MapNode(k,v,l,r,h,c) ->
                let l' = chooseiOptV f l
                let s' = f.Invoke(k,v)
                let r' = chooseiOptV f r
                match s' with
                | ValueNone -> merge l' r'
                | ValueSome v -> join l' k v r'

        let rec chooseiOptV2 (f:OptimizedClosures.FSharpFunc<'Key,'T1,struct (voption<'T2> * voption<'T3>)>) m =
            match m with
                | MapEmpty -> 
                    struct(MapEmpty, MapEmpty)

                | MapOne(k,v) ->
                    let struct (a, b) = f.Invoke(k,v)
                    let a = match a with | ValueSome v -> MapOne(k,v) | ValueNone -> MapEmpty
                    let b = match b with | ValueSome v -> MapOne(k,v) | ValueNone -> MapEmpty
                    struct (a, b)

                | MapNode(k,v,l,r,h,c) ->
                    let struct(la', lb') = chooseiOptV2 f l
                    let struct (sa', sb') = f.Invoke(k,v)
                    let struct (ra', rb') = chooseiOptV2 f r

                    let a = joinV la' k sa' ra'
                    let b = joinV lb' k sb' rb'
                    struct (a, b)


        let rec neighboursAux (comparer: IComparer<'Value>) k l r m =
            match m with
                | MapEmpty -> l, None, r
                | MapOne(k2,v2) -> 
                    let c = comparer.Compare(k, k2)
                    if c > 0 then Some(k2,v2), None, r
                    elif c = 0 then l, Some(k2,v2), r
                    else l, None, Some(k2,v2)

                | MapNode(k2,v2,l2,r2,_,_) ->
                    let c = comparer.Compare(k, k2)
                    if c > 0 then 
                        let l = Some(k2, v2)
                        neighboursAux comparer k l r r2

                    elif c = 0 then
                        let l =
                            match tryMax l2 with
                                | None -> l
                                | l -> l

                        let r =
                            match tryMin r2 with
                                | None -> r
                                | r -> r

                        l,Some(k2, v2),r

                    else
                        let r = Some(k2, v2)
                        neighboursAux comparer k l r l2

        let neighbours (comparer: IComparer<'Value>) k m =
            neighboursAux comparer k None None m
    
        let rec neighboursiAux idx l r m =
            match m with
                | MapEmpty -> 
                    l, None, r

                | MapOne(k2,v2) -> 
                    if idx > 0 then Some(k2,v2), None, r
                    elif idx = 0 then l, Some(k2,v2), r
                    else l, None, Some(k2,v2)

                | MapNode(k2,v2,l2,r2,_,cnt) ->
                    if idx < 0 then
                        None, None, tryMin m
                    elif idx >= cnt then
                        tryMax m, None, None
                    else
                        let lc = size l2
                        if idx < lc then 
                            let r = Some(k2, v2)
                            neighboursiAux idx l r l2

                        elif idx = lc then
                            let l =
                                match tryMax l2 with
                                    | None -> l
                                    | l -> l

                            let r =
                                match tryMin r2 with
                                    | None -> r
                                    | r -> r

                            l, Some(k2, v2), r
                        else
                            let l = Some(k2, v2)
                            neighboursiAux (idx-lc-1) l r r2

        let neighboursi idx m =
            neighboursiAux idx None None m
    

    
        let rec neighboursVAux (comparer: IComparer<'Value>) k l r m =
            match m with
                | MapEmpty -> struct(l, ValueNone, r)
                | MapOne(k2,v2) -> 
                    let c = comparer.Compare(k, k2)
                    if c > 0 then struct(ValueSome(struct(k2,v2)), ValueNone, r)
                    elif c = 0 then struct(l, ValueSome(struct(k2,v2)), r)
                    else struct(l, ValueNone, ValueSome(struct(k2,v2)))

                | MapNode(k2,v2,l2,r2,_,_) ->
                    let c = comparer.Compare(k, k2)
                    if c > 0 then 
                        let l = ValueSome(struct(k2, v2))
                        neighboursVAux comparer k l r r2

                    elif c = 0 then
                        let l =
                            match tryMaxV l2 with
                            | ValueNone -> l
                            | l -> l

                        let r =
                            match tryMinV r2 with
                            | ValueNone -> r
                            | r -> r

                        struct(l,ValueSome(struct(k2, v2)),r)

                    else
                        let r = ValueSome(struct(k2, v2))
                        neighboursVAux comparer k l r l2

        let neighboursV (comparer: IComparer<'Value>) k m =
            neighboursVAux comparer k ValueNone ValueNone m
    
    
        let rec neighboursViAux idx l r m =
            match m with
                | MapEmpty -> 
                    struct(l, ValueNone, r)

                | MapOne(k2,v2) -> 
                    if idx > 0 then struct(ValueSome(struct(k2,v2)), ValueNone, r)
                    elif idx = 0 then struct(l, ValueSome(struct(k2,v2)), r)
                    else struct(l, ValueNone, ValueSome(struct(k2,v2)))

                | MapNode(k2,v2,l2,r2,_,cnt) ->
                    if idx < 0 then
                        struct(ValueNone, ValueNone, tryMinV m)
                    elif idx >= cnt then
                        struct(tryMaxV m, ValueNone, ValueNone)
                    else
                        let lc = size l2
                        if idx < lc then 
                            let r = ValueSome(struct(k2, v2))
                            neighboursViAux idx l r l2

                        elif idx = lc then
                            let l =
                                match tryMaxV l2 with
                                | ValueNone -> l
                                | l -> l

                            let r =
                                match tryMinV r2 with
                                | ValueNone -> r
                                | r -> r

                            struct(l, ValueSome(struct(k2, v2)), r)
                        else
                            let l = ValueSome(struct(k2, v2))
                            neighboursViAux (idx-lc-1) l r r2

        let neighboursVi idx m =
            neighboursViAux idx ValueNone ValueNone m
    


        let rec tryAt i m =
            match m with
            | MapEmpty -> 
                None

            | MapOne(k,v) -> 
                if i = 0 then Some (k,v)
                else None

            | MapNode(k,v,l,r,_,c) ->
                if i < 0 || i >= c then
                    None
                else
                    let ls = size l
                    if i = ls then
                        Some (k,v)
                    elif i < ls then
                        tryAt i l
                    else
                        tryAt (i - ls - 1) r

        let rec tryAtV i m =
            match m with
            | MapEmpty -> 
                ValueNone

            | MapOne(k,v) -> 
                if i = 0 then ValueSome (struct(k,v))
                else ValueNone

            | MapNode(k,v,l,r,_,c) ->
                if i < 0 || i >= c then
                    ValueNone
                else
                    let ls = size l
                    if i = ls then
                        ValueSome (struct(k,v))
                    elif i < ls then
                        tryAtV i l
                    else
                        tryAtV (i - ls - 1) r

        let rec private tryGetIndexAux (comparer: IComparer<'Key>) (i : int) (key : 'Key) (m : MapTree<'Key, 'Value>) =
            match m with
            | MapEmpty -> 
                None
            | MapOne(k,_value) -> 
                if comparer.Compare(key, k) = 0 then Some i
                else None
            | MapNode(k,_value,left,right,_,_) ->
                let cmp = comparer.Compare(key, k)
                if cmp > 0 then
                    tryGetIndexAux comparer (i + size left + 1) key right
                elif cmp < 0 then
                    tryGetIndexAux comparer i key left
                else
                    Some (i + size left)
                
        let tryGetIndex (comparer: IComparer<'Key>) (key : 'Key) (m : MapTree<'Key, 'Value>) =
            tryGetIndexAux comparer 0 key m
            
        let rec private tryGetIndexVAux (comparer: IComparer<'Key>) (i : int) (key : 'Key) (m : MapTree<'Key, 'Value>) =
            match m with
            | MapEmpty -> 
                ValueNone
            | MapOne(k,_value) -> 
                if comparer.Compare(key, k) = 0 then ValueSome i
                else ValueNone
            | MapNode(k,_value,left,right,_,_) ->
                let cmp = comparer.Compare(key, k)
                if cmp > 0 then
                    tryGetIndexVAux comparer (i + size left + 1) key right
                elif cmp < 0 then
                    tryGetIndexVAux comparer i key left
                else
                    ValueSome (i + size left)
                
        let tryGetIndexV (comparer: IComparer<'Key>) (key : 'Key) (m : MapTree<'Key, 'Value>) =
            tryGetIndexVAux comparer 0 key m

        let rec map2 (comparer: IComparer<'Value>) f l r =
            match l, r with
                | MapEmpty, r -> mapi (fun i rv -> f i None (Some rv)) r
                | l, MapEmpty -> mapi (fun i lv -> f i (Some lv) None) l
                | MapOne(k,v), r ->
                    let mutable found = false
                    let res = 
                        r |> mapi (fun i rv -> 
                            if i = k then 
                                found <- true
                                f i (Some v) (Some rv)
                            else 
                                f i None (Some rv)
                        )
                    if found then res 
                    else res |> add comparer k (f k (Some v) None)

                | l, MapOne(k,v) ->
                    let mutable found = false
                    let res = 
                        l |> mapi (fun i lv -> 
                            if i = k then 
                                found <- true
                                f i (Some lv) (Some v)
                            else 
                                f i None (Some v)
                        )
                    if found then res 
                    else res |> add comparer k (f k None (Some v))

                | MapNode(k,v,ll,lr,_,_),r ->
                    let rs, self, rg = split comparer k r
                    
                    let v = 
                        match self with
                            | Some rv -> f k (Some v) (Some rv)
                            | None -> f k (Some v) None
                    join (map2 comparer f ll rs) k v (map2 comparer f lr rg)

        let rec choose2VOpt 
            (comparer: IComparer<'Key>) 
            (f : OptimizedClosures.FSharpFunc<'Key, voption<'T1>, voption<'T2>, voption<'T3>>) 
            (lo : OptimizedClosures.FSharpFunc<'Key, 'T1, voption<'T3>>) 
            (ro : OptimizedClosures.FSharpFunc<'Key, 'T2, voption<'T3>>)
            l r =
            match l, r with
                | MapEmpty, r -> chooseiOptV ro r
                | l, MapEmpty -> chooseiOptV lo l
                | MapOne(lk, lv), MapOne(rk, rv) ->
                    let c = comparer.Compare(lk, rk)
                    if c = 0 then
                        match f.Invoke(lk, ValueSome lv, ValueSome rv) with
                        | ValueSome r -> MapOne(lk, r)
                        | ValueNone -> MapEmpty
                    else
                        let l = match f.Invoke(lk, ValueSome lv, ValueNone) with | ValueSome r -> MapOne(lk, r) | ValueNone -> MapEmpty
                        let r = match f.Invoke(rk, ValueNone, ValueSome rv) with | ValueSome r -> MapOne(rk, r) | ValueNone -> MapEmpty
                        merge l r

                | MapNode(lk, lv, ll, lr, lh, lc), MapOne(rk, rv) ->
                    let c = comparer.Compare(lk, rk)
                    if c = 0 then
                        let ll = chooseiOptV lo ll
                        let lr = chooseiOptV lo lr
                        match f.Invoke(lk, ValueSome lv, ValueSome rv) with
                        | ValueSome v -> join ll lk v lr
                        | ValueNone -> merge ll lr
                    elif c > 0 then
                        joinV (choose2VOpt comparer f lo ro ll r) lk (lo.Invoke(lk, lv)) (chooseiOptV lo lr)
                    else
                        joinV (chooseiOptV lo ll) lk (lo.Invoke(lk, lv)) (choose2VOpt comparer f lo ro lr r)
                        

                | MapOne(lk, lv), MapNode(rk, rv, rl, rr, rh, rc) ->
                    let c = comparer.Compare(lk, rk)
                    if c = 0 then
                        let rl = chooseiOptV ro rl
                        let rr = chooseiOptV ro rr
                        match f.Invoke(rk, ValueSome lv, ValueSome rv) with
                        | ValueSome v -> join rl rk v rr
                        | ValueNone -> merge rl rr
                    elif c > 0 then
                        joinV (chooseiOptV ro rl) rk (ro.Invoke(rk, rv)) (choose2VOpt comparer f lo ro l rr)
                    else
                        joinV (choose2VOpt comparer f lo ro l rl) rk (ro.Invoke(rk, rv)) (chooseiOptV ro rr)
                    
                | MapNode(lk, lv, ll, lr, lh, lc), MapNode(rk, rv, rl, rr, rh, rc) ->
                    let c = comparer.Compare(lk, rk)
                    if c = 0 then
                        joinV (choose2VOpt comparer f lo ro ll rl) lk (f.Invoke(lk, ValueSome lv, ValueSome rv)) (choose2VOpt comparer f lo ro  lr rr)
                    elif lh > rh then
                        let struct (rl, _lmax, rv, _rmin, rr) = splitV comparer lk r
                        joinV (choose2VOpt comparer f lo ro ll rl) lk (f.Invoke(lk, ValueSome lv, rv)) (choose2VOpt comparer f lo ro  lr rr)
                    else
                        let struct (ll, _lmax, lv, _rmin, lr) = splitV comparer rk l
                        joinV (choose2VOpt comparer f lo ro ll rl) rk (f.Invoke(rk, lv, ValueSome rv)) (choose2VOpt comparer f lo ro  lr rr)


        let choose2V 
            (comparer: IComparer<'Key>) 
            (mapping : 'Key -> voption<'T1> -> voption<'T2> -> voption<'T3>) 
            l r =
                
            let mapping = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt mapping
            let lo = OptimizedClosures.FSharpFunc<_,_,_>.Adapt (fun k l -> mapping.Invoke(k, ValueSome l, ValueNone))
            let ro = OptimizedClosures.FSharpFunc<_,_,_>.Adapt (fun k r -> mapping.Invoke(k, ValueNone, ValueSome r))

            choose2VOpt comparer mapping lo ro l r



        let rec applyDelta
            (comparer : IComparer<'Key>)
            (apply : OptimizedClosures.FSharpFunc<'Key, voption<'Value>, 'Delta, struct (voption<'Value> * voption<'DeltaOut>)>) 
            (onlyDelta : OptimizedClosures.FSharpFunc<'Key, 'Delta, struct (voption<'Value> * voption<'DeltaOut>)>)
            (l : MapTree<'Key, 'Value>) 
            (r : MapTree<'Key, 'Delta>) =
                
            match struct(l, r) with
            | struct (value, MapEmpty) ->
                struct (value, MapEmpty)

            | struct (MapEmpty, r) -> 
                chooseiOptV2 onlyDelta r

            | struct (MapOne(lk, lv), MapOne(rk, rv)) ->
                let c = comparer.Compare(lk, rk)
                if c = 0 then
                    let struct (s, d) = apply.Invoke(lk, ValueSome lv, rv)
                    let s = match s with | ValueSome v -> MapOne(lk, v) | ValueNone -> MapEmpty
                    let d = match d with | ValueSome v -> MapOne(rk, v) | ValueNone -> MapEmpty
                    struct(s, d)
                else
                    let struct (s, d) = onlyDelta.Invoke(rk, rv)
                    let d = match d with | ValueSome v -> MapOne(rk, v) | ValueNone -> MapEmpty
                    match s with
                    | ValueSome s ->
                        //let state = merge (MapOne(lk, lv)) (MapOne(rk, s))
                        let state =
                            if c > 0 then MapNode(rk, s, MapEmpty, MapOne(lk, lv), 2, 2)
                            else MapNode(lk, lv, MapEmpty, MapOne(rk, s), 2, 2)
                        struct(state, d)
                    | ValueNone ->
                        struct (l, d)

            | struct (MapOne(lk, lv), MapNode(rk, rv, rl, rr, rh, rc)) ->
                let c = comparer.Compare(lk, rk)
                 
                //match tryRemove comparer lk r with
                //| Some (rv, r) ->
                //    let struct (s, d) = chooseiOptV2 onlyDelta r
                //    let struct (sv, dv) = apply.Invoke(lk, ValueSome lv, rv)
                //    let s = match sv with | ValueNone -> s | ValueSome v -> add comparer lk v s
                //    let d = match dv with | ValueNone -> d | ValueSome v -> add comparer lk v d
                //    struct (s, d)
                //| None ->
                //    let struct (s, d) = chooseiOptV2 onlyDelta r
                //    struct(add comparer lk lv s, d)

                if c = 0 then
                    let struct (s, d) = apply.Invoke(rk, ValueSome lv, rv)
                    let struct (rls, rld) = chooseiOptV2 onlyDelta rl
                    let struct (rrs, rrd) = chooseiOptV2 onlyDelta rr

                    let s = joinV rls rk s rrs 
                    let d = joinV rld rk d rrd 
                    struct (s, d)

                elif c < 0 then
                    let struct (s, d) = onlyDelta.Invoke(rk, rv)
                    let struct (rls, rld) = applyDelta comparer apply onlyDelta l rl
                    let struct (rrs, rrd) = chooseiOptV2 onlyDelta rr
                    
                    let s = joinV rls rk s rrs 
                    let d = joinV rld rk d rrd
                    struct (s, d)

                else
                    let struct (s, d) = onlyDelta.Invoke(rk, rv)
                    let struct (rls, rld) = chooseiOptV2 onlyDelta rl
                    let struct (rrs, rrd) = applyDelta comparer apply onlyDelta l rr
                    
                    let s = joinV rls rk s rrs 
                    let d = joinV rld rk d rrd
                    struct (s, d)

            | struct(MapNode(lk, lv, ll, lr, _, _), MapOne(rk, rv)) ->
                //match tryRemove comparer rk l with
                //| Some (lv, l) ->
                //    let struct (s, d) = apply.Invoke(rk, ValueSome lv, rv)
                //    let s = match s with | ValueSome s -> (add comparer rk s l) | ValueNone -> l
                //    let d = match d with | ValueSome v -> MapOne(rk, v) | ValueNone -> MapEmpty
                //    struct (s, d)
                //| None ->
                //    let struct (s, d) = apply.Invoke(rk, ValueNone, rv)
                //    let s = match s with | ValueSome s -> (add comparer rk s l) | ValueNone -> l
                //    let d = match d with | ValueSome v -> MapOne(rk, v) | ValueNone -> MapEmpty
                //    struct (s, d)
                    
                let c = comparer.Compare(lk, rk)
                
                if c = 0 then
                    let struct (s, d) = apply.Invoke(lk, ValueSome lv, rv)
                    let s = joinV ll lk s lr
                    let d = match d with | ValueSome v -> MapOne(lk, v) | ValueNone -> MapEmpty
                    struct (s, d)
                    
                elif c < 0 then
                    // delta in right
                    let struct (lr, d) = applyDelta comparer apply onlyDelta lr r
                    let s = join ll lk lv lr
                    struct (s, d)
                else
                    // delta in left
                    let struct (ll, d) = applyDelta comparer apply onlyDelta ll r
                    let s = join ll lk lv lr
                    struct (s, d)
                    
            | struct(MapNode(lk, lv, ll, lr, lh, _), MapNode(rk, rv, rl, rr, rh, _)) ->
                let c = comparer.Compare(lk, rk)
                if c = 0 then
                    let struct (s, d) = apply.Invoke(lk, ValueSome lv, rv)
                    let struct (ls, ld) = applyDelta comparer apply onlyDelta ll rl
                    let struct (rs, rd) = applyDelta comparer apply onlyDelta lr rr
                    let s = joinV ls lk s rs 
                    let d = joinV ld lk d rd 
                    struct (s, d)
                elif lh > rh then
                    let struct (rl, _lmax, rv, _rmin, rr) = splitV comparer lk r
                    let key = lk
                    let rk = ()
                    let lk = ()
                    match rv with
                    | ValueSome rv ->
                        let struct (s, d) = apply.Invoke(key, ValueSome lv, rv)
                        let struct (ls, ld) = applyDelta comparer apply onlyDelta ll rl
                        let struct (rs, rd) = applyDelta comparer apply onlyDelta lr rr
                        let s = joinV ls key s rs 
                        let d = joinV ld key d rd 
                        struct (s, d)
                    | ValueNone ->
                        let struct (ls, ld) = applyDelta comparer apply onlyDelta ll rl
                        let struct (rs, rd) = applyDelta comparer apply onlyDelta lr rr
                        let s = join ls key lv rs 
                        let d = merge ld rd 
                        struct (s, d)
                else
                    let struct (ll, _lmax, lv, _rmin, lr) = splitV comparer rk l
                    let key = rk
                    let rk = ()
                    let lk = ()
                    match lv with
                    | ValueSome lv ->
                        let struct (s, d) = apply.Invoke(key, ValueSome lv, rv)
                        let struct (ls, ld) = applyDelta comparer apply onlyDelta ll rl
                        let struct (rs, rd) = applyDelta comparer apply onlyDelta lr rr
                        let s = joinV ls key s rs 
                        let d = joinV ld key d rd 
                        struct (s, d)
                    | ValueNone ->
                        let struct (s, d) = onlyDelta.Invoke(key, rv)
                        let struct (ls, ld) = applyDelta comparer apply onlyDelta ll rl
                        let struct (rs, rd) = applyDelta comparer apply onlyDelta lr rr
                        let s = joinV ls key s rs 
                        let d = joinV ld key d rd 
                        struct (s, d)
            //| _ ->
            //    let mutable state = l

            //    let set = System.Collections.Generic.HashSet<_>()

            //    let delta = 
            //        r |> choosei (fun i op ->
            //            if not (set.Add i) then failwith "superbad"
            //            let l = match tryFind comparer i l with | Some l -> ValueSome l | None -> ValueNone
            //            let struct (s, d) = apply.Invoke(i, l, op)
            //            match s with
            //            | ValueSome s -> state <- add comparer i s state
            //            | ValueNone -> state <- remove comparer i state
            //            match d with
            //            | ValueSome d -> Some d
            //            | ValueNone -> None
            //        )
            //    struct(state, delta)


        let rec computeDelta 
            (comparer: IComparer<'Key>) 
            (set : OptimizedClosures.FSharpFunc<'Key, 'Value, 'OP>) 
            (update : OptimizedClosures.FSharpFunc<'Key, 'Value, 'Value, voption<'OP>>) 
            (remove : OptimizedClosures.FSharpFunc<'Key, 'Value, 'OP>) 
            (l : MapTree<'Key, 'Value>) 
            (r : MapTree<'Key, 'Value>) =
            match struct (l, r) with
                | struct (MapEmpty, MapEmpty) -> MapEmpty
                | struct (MapEmpty, r) -> mapiOpt set r
                | struct (l, MapEmpty) -> mapiOpt remove l

                | struct (MapOne(lk, lv), MapOne(rk, rv)) ->
                    if Object.ReferenceEquals(l, r) then
                        MapEmpty
                    else
                        let c = comparer.Compare(lk, rk)
                        if c = 0 then
                            match update.Invoke(lk, lv, rv) with
                            | ValueSome r -> MapOne(lk, r)
                            | ValueNone -> MapEmpty
                        elif c < 0 then
                            MapNode(lk, remove.Invoke(lk, lv), MapEmpty, MapOne(rk, set.Invoke(rk, rv)), 2, 2)
                        else
                            MapNode(lk, remove.Invoke(lk, lv), MapOne(rk, set.Invoke(rk, rv)), MapEmpty, 2, 2)
                            
                | struct (MapOne(lk, lv), MapNode(nk, nv, nl, nr, _, _)) ->
                    let c = comparer.Compare(lk, nk)
                    if c = 0 then
                        let ll = mapiOpt set nl
                        let rr = mapiOpt set nr
                        match update.Invoke(lk, lv, nv) with
                        | ValueSome op ->
                            join ll lk op rr
                        | ValueNone ->
                            merge ll rr
                    elif c > 0 then
                        let rr = computeDelta comparer set update remove l nr
                        join (mapiOpt set nl) nk (set.Invoke(nk, nv)) rr
                    else
                        let ll = computeDelta comparer set update remove l nl
                        join ll nk (set.Invoke(nk, nv)) (mapiOpt set nr)

                | struct(MapNode(nk, nv, nl, nr, _, _), MapOne(rk, rv)) ->
                    let c = comparer.Compare(rk, nk)
                    if c = 0 then
                        let ll = mapiOpt remove nl
                        let rr = mapiOpt remove nr
                        match update.Invoke(rk, nv, rv) with
                        | ValueSome op ->
                            join ll nk op rr
                        | ValueNone ->
                            merge ll rr
                    elif c > 0 then
                        let rr = computeDelta comparer set update remove nr r
                        join (mapiOpt remove nl) nk (remove.Invoke(nk, nv)) rr
                    else
                        let ll = computeDelta comparer set update remove nl r
                        join ll nk (remove.Invoke(nk, nv)) (mapiOpt remove nr)

                | struct(MapNode(lka, lva, lla, lra, lha, _), MapNode(rka, rva, rla, rra, rha, _rc)) ->
                    if Object.ReferenceEquals(l, r) then
                        MapEmpty
                    else
                        let c = comparer.Compare(lka, rka)
                        if c = 0 then
                            let l = computeDelta comparer set update remove lla rla
                            let r = computeDelta comparer set update remove lra rra
                            match update.Invoke(lka, lva, rva) with
                            | ValueSome op ->
                                join l lka op r
                            | ValueNone ->
                                merge l r

                        elif rha < lha then
                            let struct (rl, _lmax, rv, _rmin, rr) = splitV comparer lka r
                            let l = computeDelta comparer set update remove lla rl
                            let r = computeDelta comparer set update remove lra rr
                            match rv with
                            | ValueSome rv -> 
                                match update.Invoke(lka, lva, rv) with
                                | ValueSome op ->
                                    join l lka op r
                                | ValueNone ->
                                    merge l r
                            | ValueNone ->
                                let k = lka
                                let v = remove.Invoke(lka, lva)
                                join l k v r
                        else
                            let struct (ll, _lmax, lv, _rmin, lr) = splitV comparer rka l
                            let l = computeDelta comparer set update remove ll rla
                            let r = computeDelta comparer set update remove lr rra

                            match lv with
                            | ValueSome lv -> 
                                match update.Invoke(rka, lv, rva) with
                                | ValueSome op ->
                                    join l rka op r
                                | ValueNone ->
                                    merge l r
                            | ValueNone ->
                                let k = rka
                                let v = set.Invoke(rka, rva)
                                join l k v r
                                

        let rec intersectWithAux (f:OptimizedClosures.FSharpFunc<'Key,'T1,'T2>) (comparer: IComparer<'k>) (l : MapTree<'k, 'Key>) (r : MapTree<'k, 'T1>) : MapTree<'k, 'T2> =
            match l with
            | MapEmpty -> 
                MapEmpty

            | MapOne(k,lv) ->
                match tryFind comparer k r with
                | Some rv -> MapOne(k, f.Invoke(lv, rv))
                | None -> MapEmpty

            | MapNode(k,v,l1,r1,_,_) ->
                let a, s, b = split comparer k r
                match s with
                | Some s ->
                    let v = f.Invoke(v,s)
                    rebalance (intersectWithAux f comparer l1 a) k v (intersectWithAux f comparer r1 b)
                | None ->
                    let l = intersectWithAux f comparer l1 a
                    let r = intersectWithAux f comparer r1 b
                    match l with
                    | MapEmpty -> r
                    | _ ->
                        match r with
                        | MapEmpty -> l
                        | _ ->
                            let k,v,r' = spliceOutSuccessor r
                            rebalance l k v r'

        let intersectWith (f : 'Key -> 'T1 -> 'T2) (comparer : IComparer<'k>) (l : MapTree<'k, 'Key>) (r : MapTree<'k, 'T1>) =
            let lc = size l
            let rc = size r
            if lc <= rc then
                intersectWithAux (OptimizedClosures.FSharpFunc<_,_,_>.Adapt f) comparer l r
            else
                intersectWithAux (OptimizedClosures.FSharpFunc<_,_,_>.Adapt(fun a b -> f b a)) comparer r l


                      
        let rec foldBackOpt (f:OptimizedClosures.FSharpFunc<_,_,_,_>) m x = 
            match m with 
            | MapEmpty -> x
            | MapOne(k,v) -> f.Invoke(k,v,x)
            | MapNode(k,v,l,r,_,_) -> 
                let x = foldBackOpt f r x
                let x = f.Invoke(k,v,x)
                foldBackOpt f l x

        let foldBack f m x = foldBackOpt (OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)) m x

        let rec foldOpt (f:OptimizedClosures.FSharpFunc<_,_,_,_>) x m  = 
            match m with 
            | MapEmpty -> x
            | MapOne(k,v) -> f.Invoke(x,k,v)
            | MapNode(k,v,l,r,_,_) -> 
                let x = foldOpt f x l
                let x = f.Invoke(x,k,v)
                foldOpt f x r

        let fold f x m = foldOpt (OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)) x m
        
        let rec foldValueOpt (f:OptimizedClosures.FSharpFunc<_,_,_>) x m  = 
            match m with 
            | MapEmpty -> x
            | MapOne(_,v) -> f.Invoke(x,v)
            | MapNode(_,v,l,r,_,_) -> 
                let x = foldValueOpt f x l
                let x = f.Invoke(x,v)
                foldValueOpt f x r

        let foldValue f x m = foldValueOpt (OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)) x m
        
        let foldSectionOpt (comparer: IComparer<'Value>) lo hi (f:OptimizedClosures.FSharpFunc<_,_,_,_>) m x =
            let rec foldFromTo (f:OptimizedClosures.FSharpFunc<_,_,_,_>) m x = 
                match m with 
                | MapEmpty -> x
                | MapOne(k,v) ->
                    let cLoKey = comparer.Compare(lo,k)
                    let cKeyHi = comparer.Compare(k,hi)
                    let x = if cLoKey <= 0 && cKeyHi <= 0 then f.Invoke(k, v, x) else x
                    x
                | MapNode(k,v,l,r,_,_) ->
                    let cLoKey = comparer.Compare(lo,k)
                    let cKeyHi = comparer.Compare(k,hi)
                    let x = if cLoKey < 0                 then foldFromTo f l x else x
                    let x = if cLoKey <= 0 && cKeyHi <= 0 then f.Invoke(k, v, x) else x
                    let x = if cKeyHi < 0                 then foldFromTo f r x else x
                    x
           
            if comparer.Compare(lo,hi) = 1 then x else foldFromTo f m x

        let foldSection (comparer: IComparer<'Value>) lo hi f m x =
            foldSectionOpt comparer lo hi (OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)) m x

        let toList m = 
            let rec loop m acc = 
                match m with 
                | MapEmpty -> acc
                | MapOne(k,v) -> (k,v)::acc
                | MapNode(k,v,l,r,_,_) -> loop l ((k,v)::loop r acc)
            loop m []

        let toListV m =
            let rec loop m acc = 
                match m with 
                | MapEmpty -> acc
                | MapOne(k,v) -> struct(k,v)::acc
                | MapNode(k,v,l,r,_,_) -> loop l (struct(k,v)::loop r acc)
            loop m []

        let rec private copyTo (array : array<'k * 'v>) (index : ref<int>) m =
            match m with
            | MapEmpty -> 
                ()
            | MapOne(k,v) -> 
                array.[!index] <- (k, v)
                index := !index + 1
            | MapNode(k, v, l, r, _, _) ->
                copyTo array index l
                array.[!index] <- (k, v)
                index := !index + 1
                copyTo array index r

        let rec private copyToV (array : array<struct('k * 'v)>) (index : ref<int>) m =
            match m with
            | MapEmpty -> 
                ()
            | MapOne(k,v) -> 
                array.[!index] <- struct(k, v)
                index := !index + 1
            | MapNode(k, v, l, r, _, _) ->
                copyToV array index l
                array.[!index] <- struct(k, v)
                index := !index + 1
                copyToV array index r

        let rec private copyValuesTo (array : array<'v>) (index : ref<int>) m =
            match m with
            | MapEmpty -> 
                ()
            | MapOne(k,v) -> 
                array.[!index] <- v
                index := !index + 1
            | MapNode(k, v, l, r, _, _) ->
                copyValuesTo array index l
                array.[!index] <- v
                index := !index + 1
                copyValuesTo array index r

        let toArrayV m =
            let cnt = size m
            let arr = Array.zeroCreate cnt
            let index = ref 0 
            copyToV arr index m
            arr

        let toArray m =
            let cnt = size m
            let arr = Array.zeroCreate cnt
            let index = ref 0 
            copyTo arr index m
            arr
            

        let valueList m =
            let rec loop m acc = 
                match m with 
                | MapEmpty -> acc
                | MapOne(k,v) -> v::acc
                | MapNode(k,v,l,r,_,_) -> loop l (v::loop r acc)
            loop m []
            
        let valueArray m =
            let cnt = size m
            let arr = Array.zeroCreate cnt
            let index = ref 0 
            copyValuesTo arr index m
            arr
            


        let ofList comparer l = List.fold (fun acc (k,v) -> add comparer k v acc) empty l

        let rec mkFromEnumerator comparer acc (e : IEnumerator<_>) = 
            if e.MoveNext() then 
                let (x,y) = e.Current 
                mkFromEnumerator comparer (add comparer x y acc) e
            else acc
          
        let ofArray comparer (arr : array<_>) =
            let mutable res = empty
            for (x,y) in arr do
                res <- add comparer x y res 
            res

        let ofSeq comparer (c : seq<'Key * 'T>) =
            match c with 
            | :? array<'Key * 'T> as xs -> ofArray comparer xs
            | :? list<'Key * 'T> as xs -> ofList comparer xs
            | _ -> 
                use ie = c.GetEnumerator()
                mkFromEnumerator comparer empty ie 

          
        let copyToArray s (arr: _[]) i =
            let j = ref i 
            s |> iter (fun x y -> arr.[!j] <- KeyValuePair(x,y); j := !j + 1)
            
        let copyValuesToArray s (arr: _[]) i =
            let j = ref i 
            s |> iter (fun x y -> arr.[!j] <- y; j := !j + 1)


        /// Imperative left-to-right iterators.
        [<NoEquality; NoComparison>]
        type MapIterator<'Key,'Value when 'Key : comparison > = 
                { /// invariant: always collapseLHS result 
                mutable stack: MapTree<'Key,'Value> list;  
                /// true when MoveNext has been called   
                mutable started : bool }

        // collapseLHS:
        // a) Always returns either [] or a list starting with MapOne.
        // b) The "fringe" of the set stack is unchanged. 
        let rec collapseLHS stack =
            match stack with
            | []                           -> []
            | MapEmpty             :: rest -> collapseLHS rest
            | MapOne _         :: _ -> stack
            | (MapNode(k,v,l,r,_,_)) :: rest -> collapseLHS (l :: MapOne (k,v) :: r :: rest)
          
        let mkIterator s = { stack = collapseLHS [s]; started = false }

        let notStarted() = raise (InvalidOperationException("enumeration not started"))
        let alreadyFinished() = raise (InvalidOperationException("enumeration finished"))

        let current i =
            if i.started then
                match i.stack with
                    | MapOne (k,v) :: _ -> new KeyValuePair<_,_>(k,v)
                    | []            -> alreadyFinished()
                    | _             -> failwith "Please report error: MapExt iterator, unexpected stack for current"
            else
                notStarted()

        let rec moveNext i =
            if i.started then
                match i.stack with
                    | MapOne _ :: rest -> 
                        i.stack <- collapseLHS rest
                        not i.stack.IsEmpty
                    | [] -> false
                    | _ -> failwith "Please report error: MapExt iterator, unexpected stack for moveNext"
            else
                i.started <- true  (* The first call to MoveNext "starts" the enumeration. *)
                not i.stack.IsEmpty

        let mkIEnumerator s = 
            let i = ref (mkIterator s) 
            { new IEnumerator<_> with 
                member __.Current = current !i
            interface System.Collections.IEnumerator with
                member __.Current = box (current !i)
                member __.MoveNext() = moveNext !i
                member __.Reset() = i :=  mkIterator s
            interface System.IDisposable with 
                member __.Dispose() = ()}


        type MapTreeEnumerator<'k, 'v when 'k : comparison>(m : MapTree<'k, 'v>) =
            let mutable stack = [m]
            let mutable current = Unchecked.defaultof<'k * 'v>
            
            let rec move () =
                match stack with
                    | [] ->
                        false
                    | MapEmpty :: rest ->
                        stack <- rest
                        move()

                    | MapOne(key,value) :: rest ->
                        stack <- rest
                        current <- (key, value)
                        true

                    | MapNode(k,v,l,r,_,_) :: rest ->
                        stack <- l :: (MapOne(k,v)) :: r :: rest
                        move()
                        
            interface System.Collections.IEnumerator with
                member x.MoveNext() = move()
                member x.Reset() =
                    stack <- [m]
                    current <- Unchecked.defaultof<'k * 'v>

                member x.Current = current :> obj
                
            interface IEnumerator<'k * 'v> with
                member x.Dispose() =
                    stack <- []
                    current <- Unchecked.defaultof<'k * 'v>
                member x.Current = current

        type MapTreeBackwardEnumerator<'k, 'v when 'k : comparison>(m : MapTree<'k, 'v>) =
            let mutable stack = [m]
            let mutable current = Unchecked.defaultof<'k * 'v>
            
            let rec move () =
                match stack with
                    | [] ->
                        false
                    | MapEmpty :: rest ->
                        stack <- rest
                        move()

                    | MapOne(key,value) :: rest ->
                        stack <- rest
                        current <- (key, value)
                        true

                    | MapNode(k,v,l,r,_,_) :: rest ->
                        stack <- r :: (MapOne(k,v)) :: l :: rest
                        move()
                        
            interface System.Collections.IEnumerator with
                member x.MoveNext() = move()
                member x.Reset() =
                    stack <- [m]
                    current <- Unchecked.defaultof<'k * 'v>

                member x.Current = current :> obj
                
            interface IEnumerator<'k * 'v> with
                member x.Dispose() =
                    stack <- []
                    current <- Unchecked.defaultof<'k * 'v>
                member x.Current = current

        type ValueMapTreeEnumerator<'k, 'v when 'k : comparison>(m : MapTree<'k, 'v>) =
            let mutable stack = [m]
            let mutable current = Unchecked.defaultof<struct('k * 'v)>
            
            let rec move () =
                match stack with
                    | [] ->
                        false
                    | MapEmpty :: rest ->
                        stack <- rest
                        move()

                    | MapOne(key,value) :: rest ->
                        stack <- rest
                        current <- struct(key, value)
                        true

                    | MapNode(k,v,l,r,_,_) :: rest ->
                        stack <- l :: (MapOne(k,v)) :: r :: rest
                        move()
                        
            interface System.Collections.IEnumerator with
                member x.MoveNext() = move()
                member x.Reset() =
                    stack <- [m]
                    current <- Unchecked.defaultof<_>

                member x.Current = current :> obj
                
            interface IEnumerator<struct('k * 'v)> with
                member x.Dispose() =
                    stack <- []
                    current <- Unchecked.defaultof<_>
                member x.Current = current

        type ValueMapTreeBackwardEnumerator<'k, 'v when 'k : comparison>(m : MapTree<'k, 'v>) =
            let mutable stack = [m]
            let mutable current = Unchecked.defaultof<struct('k * 'v)>
            
            let rec move () =
                match stack with
                    | [] ->
                        false
                    | MapEmpty :: rest ->
                        stack <- rest
                        move()

                    | MapOne(key,value) :: rest ->
                        stack <- rest
                        current <- struct(key, value)
                        true

                    | MapNode(k,v,l,r,_,_) :: rest ->
                        stack <- r :: (MapOne(k,v)) :: l :: rest
                        move()
                        
            interface System.Collections.IEnumerator with
                member x.MoveNext() = move()
                member x.Reset() =
                    stack <- [m]
                    current <- Unchecked.defaultof<_>

                member x.Current = current :> obj
                
            interface IEnumerator<struct('k * 'v)> with
                member x.Dispose() =
                    stack <- []
                    current <- Unchecked.defaultof<_>
                member x.Current = current




open MapExtImplementation
open System.Runtime.CompilerServices


[<System.Diagnostics.DebuggerTypeProxy(typedefof<MapDebugView<_,_>>)>]
[<System.Diagnostics.DebuggerDisplay("Count = {Count}")>]
[<Sealed>]
[<StructuredFormatDisplay("{AsString}")>]
type internal MapExt<[<EqualityConditionalOn>]'Key,[<EqualityConditionalOn;ComparisonConditionalOn>]'Value when 'Key : comparison >(comparer: IComparer<'Key>, tree: MapTree<'Key,'Value>) =

    static let defaultComparer = LanguagePrimitives.FastGenericComparer<'Key> 
    // We use .NET generics per-instantiation static fields to avoid allocating a new object for each empty
    // set (it is just a lookup into a .NET table of type-instantiation-indexed static fields).
    static let empty = new MapExt<'Key,'Value>(defaultComparer, MapTree<_,_>.MapEmpty)

    static member Empty : MapExt<'Key,'Value> = empty

    static member Create(ie : IEnumerable<_>) : MapExt<'Key,'Value> = 
        let comparer = LanguagePrimitives.FastGenericComparer<'Key> 
        new MapExt<_,_>(comparer,MapTree.ofSeq comparer ie)
    
    static member Create() : MapExt<'Key,'Value> = empty

    new(ie : seq<_>) = 
        let comparer = LanguagePrimitives.FastGenericComparer<'Key> 
        new MapExt<_,_>(comparer,MapTree.ofSeq comparer ie)

    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member internal m.Comparer = comparer
    //[<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member internal m.Tree = tree
    member m.Add(k,v) : MapExt<'Key,'Value> = 
        new MapExt<'Key,'Value>(comparer,MapTree.add comparer k v tree)

    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member m.IsEmpty = MapTree.isEmpty tree
    member m.Item 
        with get(k : 'Key) = MapTree.find comparer k tree


    member x.Keys = 
        let mutable s = Set.empty
        for (KeyValue(k,_)) in x do s <- s.Add k
        s

    member x.Values =
        x |> Seq.map (fun (KeyValue(_,v)) -> v)

    member x.TryAt i = MapTree.tryAt i tree
    member x.TryAtV i = MapTree.tryAtV i tree
    member x.Neighbours k = MapTree.neighbours comparer k tree
    member x.NeighboursAt i = MapTree.neighboursi i tree
    member x.NeighboursV k = MapTree.neighboursV comparer k tree
    member x.NeighboursAtV i = MapTree.neighboursVi i tree
    member x.TryGetIndex k = MapTree.tryGetIndex comparer k tree
    member x.TryGetIndexV k = MapTree.tryGetIndexV comparer k tree

    member m.TryPick(f) = MapTree.tryPick f tree 
    member m.TryPickBack(f) = MapTree.tryPickBack f tree 
    member m.Exists(f) = MapTree.exists f tree 
    member m.Filter(f)  : MapExt<'Key,'Value> = new MapExt<'Key,'Value>(comparer ,MapTree.filter comparer f tree)
    member m.ForAll(f) = MapTree.forall f tree 
    member m.Fold f acc = MapTree.foldBack f tree acc

    member m.FoldSection (lo:'Key) (hi:'Key) f (acc:'z) = MapTree.foldSection comparer lo hi f tree acc 

    member m.Iterate f = MapTree.iter f tree
    
    member m.IterateValue f = MapTree.iterValue f tree

    member m.MapRange f  = new MapExt<'Key,'T2>(comparer,MapTree.map f tree)

    member m.Map f  = new MapExt<'Key,'T2>(comparer,MapTree.mapi f tree)
    
    member m.MapMonotonic<'Key2, 'Value2 when 'Key2 : comparison> (f : 'Key -> 'Value -> 'Key2 * 'Value2) : MapExt<'Key2,'Value2> = new MapExt<'Key2,'Value2>(LanguagePrimitives.FastGenericComparer<'Key2>, MapTree.mapiMonotonic f tree)
   
    member m.ChooseMonotonic<'Key2, 'Value2 when 'Key2 : comparison> (f : 'Key -> 'Value -> option<'Key2 * 'Value2>) : MapExt<'Key2,'Value2> = 
        new MapExt<'Key2,'Value2>(LanguagePrimitives.FastGenericComparer<'Key2>, MapTree.chooseiMonotonic f tree)
    
    member x.GetReference key =
        MapTree.getReference comparer 0 key tree
        
    member x.TryIndexOf key =
        match MapTree.getReference comparer 0 key tree with
            | Existing(i,_) -> Some i
            | _ -> None

    member x.TryRemoveMin() = 
        match MapTree.tryRemoveMin comparer tree with
        | Some (k,v,t) -> Some(k,v, MapExt(comparer, t))
        | None -> None

    member x.TryRemoveMax() = 
        match MapTree.tryRemoveMax comparer tree with
        | Some (k,v,t) -> Some(k,v, MapExt(comparer, t))
        | None -> None
        
    member x.TryRemoveMinV() = 
        match MapTree.tryRemoveMinV comparer ValueNone tree with
        | ValueSome (struct(k,v,n,t)) -> ValueSome(struct(k, v, n, MapExt(comparer, t)))
        | ValueNone -> ValueNone

    member x.TryRemoveMaxV() = 
        match MapTree.tryRemoveMaxV comparer ValueNone tree with
        | ValueSome (struct(k,v,p,t)) -> ValueSome(struct(k, v, p, MapExt(comparer, t)))
        | ValueNone -> ValueNone

    member m.Map2(other:MapExt<'Key,'Value2>, f)  = 
        new MapExt<'Key,'Result>(comparer, MapTree.map2 comparer f tree other.Tree)
        
    member m.Choose2(other:MapExt<'Key,'Value2>, f)  = 
        let inline v o = match o with | Some v -> ValueSome v | None -> ValueNone
        let inline o o = match o with | ValueSome v -> Some v | ValueNone -> None

        let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt f
        let mapping k l r = f.Invoke(k, o l, o r) |> v
        new MapExt<'Key,'Result>(comparer, MapTree.choose2V comparer mapping tree other.Tree)
        
    member m.Choose2V(other:MapExt<'Key,'Value2>, f)  = 
        new MapExt<'Key,'Result>(comparer, MapTree.choose2V comparer f tree other.Tree)
        
    member m.ComputeDelta(other:MapExt<'Key,'Value>, add, update, remove)  = 
        let add = OptimizedClosures.FSharpFunc<_,_,_>.Adapt add
        let update = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt update
        let remove = OptimizedClosures.FSharpFunc<_,_,_>.Adapt remove
        new MapExt<'Key,_>(comparer, MapTree.computeDelta comparer add update remove tree other.Tree)
        
    member m.ApplyDelta(other:MapExt<'Key,'Delta>, apply)  = 
        let apply = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt apply
        let onlyDelta = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(fun k d -> apply.Invoke(k, ValueNone, d))
        let struct (s, d) = MapTree.applyDelta comparer apply onlyDelta tree other.Tree
        new MapExt<'Key,_>(comparer, s), new MapExt<'Key,_>(comparer, d)

    member m.Choose(f) =
        new MapExt<'Key, 'Value2>(comparer, MapTree.choosei f tree)

    member m.Alter(k, f) = new MapExt<'Key, 'Value>(comparer, MapTree.alter comparer k f tree)

    member m.Partition(f)  : MapExt<'Key,'Value> * MapExt<'Key,'Value> = 
        let r1,r2 = MapTree.partition comparer f tree  in 
        new MapExt<'Key,'Value>(comparer,r1), new MapExt<'Key,'Value>(comparer,r2)

    member m.Count = MapTree.size tree
    
    member x.TryMin = MapTree.tryMin tree
    member x.TryMax = MapTree.tryMax tree

    member x.TryMinKey = MapTree.tryMin tree |> Option.map fst
    member x.TryMaxKey = MapTree.tryMax tree |> Option.map fst
    
    member x.TryMinKeyV = MapTree.tryMinV tree |> ValueOption.map (fun struct(k,_) -> k)
    member x.TryMaxKeyV = MapTree.tryMaxV tree |> ValueOption.map (fun struct(k,_) -> k)
    
    member x.TryMinValue = MapTree.tryMin tree |> Option.map snd
    member x.TryMaxValue = MapTree.tryMax tree |> Option.map snd
    
    member x.TryMinValueV = MapTree.tryMinV tree |> ValueOption.map (fun struct(_,v) -> v)
    member x.TryMaxValueV = MapTree.tryMaxV tree |> ValueOption.map (fun struct(_,v) -> v)

    member x.Split (k) =
        let l, self, r = MapTree.split comparer k tree
        MapExt<'Key, 'Value>(comparer, l), self, MapExt<'Key, 'Value>(comparer, r)
        
    member x.ReplaceRange (k0, k1, replacement) =
        let repl = OptimizedClosures.FSharpFunc<_,_,_,_,_,_>.Adapt replacement
        let res = MapTree.replaceRange comparer repl k0 k1 ValueNone ValueNone tree
        MapExt<'Key, 'Value>(comparer, res)
        
    member x.SplitV (k) =
        let struct(l, lmax, self, rmin, r) = MapTree.splitV comparer k tree
        struct(MapExt<'Key, 'Value>(comparer, l), lmax, self, rmin, MapExt<'Key, 'Value>(comparer, r))
        
    member x.UnionWith (other : MapExt<_,_>, resolve) =
        if x.IsEmpty then other
        elif other.IsEmpty then x
        else new MapExt<'Key, 'Value>(comparer, MapTree.unionWith comparer resolve tree other.Tree)
        
    member x.Union(other : MapExt<_,_>) =
        new MapExt<'Key, 'Value>(comparer, MapTree.union comparer tree other.Tree)

    member x.IntersectWith(other : MapExt<_,_>, resolve) =
        if x.IsEmpty || other.IsEmpty then MapExt<_,_>.Empty
        else new MapExt<'Key, _>(comparer, MapTree.intersectWith resolve comparer tree other.Tree)
        
    member x.Intersect(other : MapExt<_,_>) =
        if x.IsEmpty || other.IsEmpty then MapExt<_,_>.Empty
        else new MapExt<'Key, _>(comparer, MapTree.intersectWith (fun l r -> (l,r)) comparer tree other.Tree)

    member x.Validate() =
        MapTree.validate comparer tree


    member m.ContainsKey(k) = 
        MapTree.mem comparer k tree


    member m.Remove(k)  : MapExt<'Key,'Value> = 
        new MapExt<'Key,'Value>(comparer,MapTree.remove comparer k tree)
        
    member m.TryRemove(k) : option<'Value * MapExt<'Key,'Value>> = 
        match MapTree.tryRemove comparer k tree with
        | Some (v, t) -> 
            Some(v, new MapExt<'Key,'Value>(comparer, t))
        | None ->
            None
            
    member m.TryRemoveV(k) : voption<struct('Value * MapExt<'Key,'Value>)> = 
        match MapTree.tryRemoveV comparer k tree with
        | ValueSome struct (v, t) -> 
            ValueSome(struct(v, new MapExt<'Key,'Value>(comparer, t)))
        | ValueNone ->
            ValueNone

    member m.TryFind(k) = 
        MapTree.tryFind comparer k tree
        
    member m.TryFindV(k) = 
        MapTree.tryFindV comparer k tree

    member m.ToList() = MapTree.toList tree

    member m.ToArray() = MapTree.toArray tree

    member x.ToListV() = MapTree.toListV tree
    member x.ToArrayV() = MapTree.toArrayV tree
    
    member x.ToValueList() = MapTree.valueList tree
    member x.ToValueArray() = MapTree.valueArray tree

    static member ofList(l) : MapExt<'Key,'Value> = 
        let comparer = LanguagePrimitives.FastGenericComparer<'Key> 
        new MapExt<_,_>(comparer,MapTree.ofList comparer l)
           
    member this.ComputeHashCode() = 
        let combineHash x y = (x <<< 1) + y + 631 
        let mutable res = 0
        for (KeyValue(x,y)) in this do
            res <- combineHash res (hash x)
            res <- combineHash res (DefaultEquality.hash y)
        abs res

    override this.Equals(that) = 
        if System.Object.ReferenceEquals(this, that) then
            true
        else
            match that with 
            | :? MapExt<'Key,'Value> as that -> 
                use e1 = (this :> seq<_>).GetEnumerator() 
                use e2 = (that :> seq<_>).GetEnumerator() 
                let rec loop () = 
                    let m1 = e1.MoveNext() 
                    let m2 = e2.MoveNext()
                    (m1 = m2) && (not m1 || let e1c, e2c = e1.Current, e2.Current in ((e1c.Key = e2c.Key) && (DefaultEquality.equals e1c.Value e2c.Value) && loop()))
                loop()
            | _ -> false

    override this.GetHashCode() = this.ComputeHashCode()

    member x.GetForwardEnumerator() = new MapTree.MapTreeEnumerator<'Key, 'Value>(tree) :> IEnumerator<_> 
    member x.GetBackwardEnumerator() = new MapTree.MapTreeBackwardEnumerator<'Key, 'Value>(tree) :> IEnumerator<_> 
    
    member x.GetForwardEnumeratorV() = new MapTree.ValueMapTreeEnumerator<'Key, 'Value>(tree) :> IEnumerator<_> 
    member x.GetBackwardEnumeratorV() = new MapTree.ValueMapTreeBackwardEnumerator<'Key, 'Value>(tree) :> IEnumerator<_> 

    member x.GetEnumerator() = new MapExtEnumerator<_,_>(tree)

    interface IEnumerable<KeyValuePair<'Key, 'Value>> with
        member x.GetEnumerator() = x.GetEnumerator() :> _

    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = x.GetEnumerator() :> _

    //interface IDictionary<'Key, 'Value> with 
    //    member m.Item 
    //        with get x = m.[x]            
    //        and  set x v = ignore(x,v); raise (NotSupportedException("SR.GetString(SR.mapCannotBeMutated)"))

    //    // REVIEW: this implementation could avoid copying the Values to an array    
    //    member s.Keys = ([| for kvp in s -> kvp.Key |] :> ICollection<'Key>)

    //    // REVIEW: this implementation could avoid copying the Values to an array    
    //    member s.Values = ([| for kvp in s -> kvp.Value |] :> ICollection<'Value>)

    //    member s.Add(k,v) = ignore(k,v); raise (NotSupportedException("SR.GetString(SR.mapCannotBeMutated)"))
    //    member s.ContainsKey(k) = s.ContainsKey(k)
    //    member s.TryGetValue(k,r) = if s.ContainsKey(k) then (r <- s.[k]; true) else false
    //    member s.Remove(k : 'Key) = ignore(k); (raise (NotSupportedException("SR.GetString(SR.mapCannotBeMutated)")) : bool)

    //interface ICollection<KeyValuePair<'Key, 'Value>> with 
    //    member __.Add(x) = ignore(x); raise (NotSupportedException("SR.GetString(SR.mapCannotBeMutated)"));
    //    member __.Clear() = raise (NotSupportedException("SR.GetString(SR.mapCannotBeMutated)"));
    //    member __.Remove(x) = ignore(x); raise (NotSupportedException("SR.GetString(SR.mapCannotBeMutated)"));
    //    member s.Contains(x) = s.ContainsKey(x.Key) && DefaultEquality.equals s.[x.Key] x.Value
    //    member __.CopyTo(arr,i) = MapTree.copyToArray tree arr i
    //    member s.IsReadOnly = true
    //    member s.Count = s.Count

    interface System.IComparable with 
        member m.CompareTo(obj: obj) = 
            match obj with 
            | :? MapExt<'Key,'Value>  as m2->
                Seq.compareWith 
                    (fun (kvp1 : KeyValuePair<_,_>) (kvp2 : KeyValuePair<_,_>)-> 
                        let c = comparer.Compare(kvp1.Key,kvp2.Key) in 
                        if c <> 0 then c else Unchecked.compare kvp1.Value kvp2.Value)
                    m m2 
            | _ -> 
                invalidArg "obj" ("SR.GetString(SR.notComparable)")
    override x.ToString() = 
        let suffix = if x.Count > 4 then "; ..." else ""
        let content = Seq.truncate 4 x |> Seq.map (fun (KeyValue t) -> sprintf "%A" t) |> String.concat "; "
        "map [" + content + suffix + "]"

    member private x.AsString = x.ToString()

and [<Sealed>] internal MapExtEnumerator<'k, 'v when 'k : comparison>(m : MapTree<'k, 'v>) =
    
    let mutable e0 = m
    let mutable e1 = MapExtImplementation.MapTree<'k, 'v>.MapEmpty
    let mutable e2 = MapExtImplementation.MapTree<'k, 'v>.MapEmpty
    let mutable e3 = MapExtImplementation.MapTree<'k, 'v>.MapEmpty
    let mutable e4 = MapExtImplementation.MapTree<'k, 'v>.MapEmpty
    let mutable e5 = MapExtImplementation.MapTree<'k, 'v>.MapEmpty
    let mutable e6 = MapExtImplementation.MapTree<'k, 'v>.MapEmpty
    let mutable e7 = MapExtImplementation.MapTree<'k, 'v>.MapEmpty

    let mutable stack = []
    let mutable cnt = 1

    let mutable current = Unchecked.defaultof<KeyValuePair<'k, 'v>>
            
    

    let rec move (n : MapExtImplementation.MapTree<'k, 'v>) =
        let inline push (n : MapExtImplementation.MapTree<'k, 'v>) =
            match n with
            | MapExtImplementation.MapEmpty -> ()
            | _ -> 
                match cnt with
                | 0 -> e0 <- n
                | 1 -> e1 <- n
                | 2 -> e2 <- n
                | 3 -> e3 <- n
                | 4 -> e4 <- n
                | 5 -> e5 <- n
                | 6 -> e6 <- n
                | 7 -> e7 <- n
                | _ -> stack <- n :: stack
                    
                cnt <- cnt + 1

        let inline pop() =
            let id = cnt
            cnt <- cnt - 1
            match id with
            | 0 -> failwith "bad"
            | 1 -> e0
            | 2 -> e1
            | 3 -> e2
            | 4 -> e3
            | 5 -> e4
            | 6 -> e5
            | 7 -> e6
            | 8 -> e7
            | _ -> 
                match stack with
                | h :: t -> 
                    stack <- t
                    h
                | _ ->
                    failwith "bad"



        match n with
        | MapExtImplementation.MapEmpty ->
            if cnt = 0 then
                false
            else
                let h = pop()
                move h

        | MapExtImplementation.MapOne(key,value) ->
            current <- KeyValuePair(key, value)
            true

        | MapExtImplementation.MapNode(k, v, l, r, _, _) ->
            match l with
            | MapExtImplementation.MapEmpty ->
                push r
                current <- KeyValuePair(k, v)
                true
            | MapExtImplementation.MapOne(lk, lv) ->
                current <- KeyValuePair(lk, lv)
                push r
                push (MapExtImplementation.MapOne(k, v))
                true
            | l ->
                push r
                push (MapExtImplementation.MapOne(k, v))
                move l


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.MoveNext() = 
        let inline pop() =
            let id = cnt
            cnt <- cnt - 1
            match id with
            | 0 -> failwith "bad"
            | 1 -> e0
            | 2 -> e1
            | 3 -> e2
            | 4 -> e3
            | 5 -> e4
            | 6 -> e5
            | 7 -> e6
            | 8 -> e7
            | _ -> 
                match stack with
                | h :: t -> 
                    stack <- t
                    h
                | _ ->
                    failwith "bad"


        if cnt = 0 then
            false
        else
            let h = pop()
            move h
    
    
    member x.Current 
        with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = current
              
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Dispose() = 
        stack <- Unchecked.defaultof<_>
        current <- Unchecked.defaultof<_>
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Reset() =
        e0 <- m
        e1 <- MapExtImplementation.MapTree<'k, 'v>.MapEmpty
        e2 <- MapExtImplementation.MapTree<'k, 'v>.MapEmpty
        e3 <- MapExtImplementation.MapTree<'k, 'v>.MapEmpty
        e4 <- MapExtImplementation.MapTree<'k, 'v>.MapEmpty
        e5 <- MapExtImplementation.MapTree<'k, 'v>.MapEmpty
        e6 <- MapExtImplementation.MapTree<'k, 'v>.MapEmpty
        e7 <- MapExtImplementation.MapTree<'k, 'v>.MapEmpty
        stack <- []
        cnt <- 1
        current <- Unchecked.defaultof<_>

    interface System.Collections.IEnumerator with
        member x.Current = x.Current :> obj
        member x.MoveNext() = x.MoveNext()
        member x.Reset() = x.Reset()
                    
    interface IEnumerator<KeyValuePair<'k, 'v>> with
        member x.Current = x.Current
        member x.Dispose() = x.Dispose()

and 
    [<Sealed>]
    internal MapDebugView<'Key,'Value when 'Key : comparison>(v: MapExt<'Key,'Value>)  =  

        [<DebuggerBrowsable(DebuggerBrowsableState.RootHidden)>]
        member x.Items = v |> Seq.truncate 10000 |> Seq.toArray


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module internal MapExt = 

    [<CompiledName("IsEmpty")>]
    let isEmpty (m:MapExt<_,_>) = m.IsEmpty
    
    [<CompiledName("Keys")>]
    let keys (m:MapExt<_,_>) = m.Keys
    
    [<CompiledName("Values")>]
    let values (m:MapExt<_,_>) = m.Values

    [<CompiledName("Add")>]
    let add k v (m:MapExt<_,_>) = m.Add(k,v)

    [<CompiledName("Find")>]
    let find k (m:MapExt<_,_>) = m.[k]

    [<CompiledName("TryFind")>]
    let tryFind k (m:MapExt<_,_>) = m.TryFind(k)
    
    [<CompiledName("TryFindV")>]
    let tryFindV k (m:MapExt<_,_>) = m.TryFindV(k)

    [<CompiledName("Remove")>]
    let remove k (m:MapExt<_,_>) = m.Remove(k)
    
    [<CompiledName("TryRemove")>]
    let tryRemove k (m:MapExt<_,_>) = m.TryRemove(k)
    
    [<CompiledName("TryRemoveValue")>]
    let tryRemoveV k (m:MapExt<_,_>) = m.TryRemoveV(k)
    
    [<CompiledName("TryRemoveMin")>]
    let tryRemoveMin (m:MapExt<_,_>) = m.TryRemoveMin()

    [<CompiledName("TryRemoveMax")>]
    let tryRemoveMax (m:MapExt<_,_>) = m.TryRemoveMax()
    
    [<CompiledName("TryRemoveMinValue")>]
    let tryRemoveMinV (m:MapExt<_,_>) = m.TryRemoveMinV()

    [<CompiledName("TryRemoveMaxValue")>]
    let tryRemoveMaxV (m:MapExt<_,_>) = m.TryRemoveMaxV()

    [<CompiledName("ContainsKey")>]
    let containsKey k (m:MapExt<_,_>) = m.ContainsKey(k)

    [<CompiledName("Iterate")>]
    let iter f (m:MapExt<_,_>) = m.Iterate(f)
    
    [<CompiledName("IterateValue")>]
    let iterValue f (m:MapExt<_,_>) = m.IterateValue(f)

    [<CompiledName("TryPick")>]
    let tryPick f (m:MapExt<_,_>) = m.TryPick(f)

    [<CompiledName("TryPickBack")>]
    let tryPickBack f (m:MapExt<_,_>) = m.TryPickBack(f)

    [<CompiledName("Pick")>]
    let pick f (m:MapExt<_,_>) = match tryPick f m with None -> raise (KeyNotFoundException()) | Some res -> res

    [<CompiledName("Exists")>]
    let exists f (m:MapExt<_,_>) = m.Exists(f)

    [<CompiledName("Filter")>]
    let filter f (m:MapExt<_,_>) = m.Filter(f)

    [<CompiledName("Partition")>]
    let partition f (m:MapExt<_,_>) = m.Partition(f)

    [<CompiledName("ForAll")>]
    let forall f (m:MapExt<_,_>) = m.ForAll(f)
    
    [<CompiledName("MapRange")>]
    let mapRange f (m:MapExt<_,_>) = m.MapRange(f)

    [<CompiledName("Map")>]
    let map f (m:MapExt<_,_>) = m.Map(f)

    [<CompiledName("Fold")>]
    let fold<'Key,'T,'State when 'Key : comparison> f (z:'State) (m:MapExt<'Key,'T>) = MapTree.fold f z m.Tree
    
    [<CompiledName("FoldValue")>]
    let foldValue<'Key,'T,'State when 'Key : comparison> f (z:'State) (m:MapExt<'Key,'T>) = MapTree.foldValue f z m.Tree

    [<CompiledName("FoldBack")>]
    let foldBack<'Key,'T,'State  when 'Key : comparison> f (m:MapExt<'Key,'T>) (z:'State) =  MapTree.foldBack  f m.Tree z
        
    [<CompiledName("ToSeq")>]
    let toSeq (m:MapExt<_,_>) = m |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
    
    [<CompiledName("ToSeqV")>]
    let toSeqV (m:MapExt<_,_>) = m |> Seq.map (fun kvp -> struct(kvp.Key, kvp.Value))
    
    [<CompiledName("ToValueSeq")>]
    let toValueSeq (m:MapExt<_,_>) = m |> Seq.map (fun kvp -> kvp.Value)
    
    [<CompiledName("ToSeqBack")>]
    let toSeqBack (m : MapExt<_,_>) = new EnumeratorEnumerable<_>(m.GetBackwardEnumerator) :> seq<_> 
    
    [<CompiledName("ToSeqBackV")>]
    let toSeqBackV (m : MapExt<_,_>) = new EnumeratorEnumerable<_>(m.GetBackwardEnumeratorV) :> seq<_> 

    [<CompiledName("FindKey")>]
    let findKey f (m : MapExt<_,_>) = m |> toSeq |> Seq.pick (fun (k,v) -> if f k v then Some(k) else None)

    [<CompiledName("TryFindKey")>]
    let tryFindKey f (m : MapExt<_,_>) = m |> toSeq |> Seq.tryPick (fun (k,v) -> if f k v then Some(k) else None)

    [<CompiledName("OfList")>]
    let ofList (l: ('Key * 'Value) list) = MapExt<_,_>.ofList(l)

    [<CompiledName("OfSeq")>]
    let ofSeq l = MapExt<_,_>.Create(l)
    
    [<CompiledName("Singleton")>]
    let singleton k v = MapExt<_,_>(LanguagePrimitives.FastGenericComparer<_>,MapOne(k,v))

    [<CompiledName("OfArray")>]
    let ofArray (array: ('Key * 'Value) array) = 
        let comparer = LanguagePrimitives.FastGenericComparer<'Key> 
        new MapExt<_,_>(comparer,MapTree.ofArray comparer array)

    [<CompiledName("ToList")>]
    let toList (m:MapExt<_,_>) = m.ToList()

    [<CompiledName("ToArray")>]
    let toArray (m:MapExt<_,_>) = m.ToArray()
    
    [<CompiledName("ToListV")>]
    let toListV (m:MapExt<_,_>) = m.ToListV()

    [<CompiledName("ToArrayV")>]
    let toArrayV (m:MapExt<_,_>) = m.ToArrayV()
    
    [<CompiledName("ToValueList")>]
    let toValueList (m:MapExt<_,_>) = m.ToValueList()

    [<CompiledName("ToValueArray")>]
    let toValueArray (m:MapExt<_,_>) = m.ToValueArray()

    [<CompiledName("Empty")>]
    let empty<'Key,'Value  when 'Key : comparison> = MapExt<'Key,'Value>.Empty

    [<CompiledName("Count")>]
    let count (m:MapExt<_,_>) = m.Count
    
    [<CompiledName("TryMin")>]
    let tryMin (m:MapExt<_,_>) = m.TryMinKey
    
    [<CompiledName("Min")>]
    let min (m:MapExt<_,_>) = 
        match m.TryMinKey with
            | Some min -> min
            | None -> raise <| ArgumentException("The input sequence was empty.")

    [<CompiledName("TryMax")>]
    let tryMax (m:MapExt<_,_>) = m.TryMaxKey

    [<CompiledName("Max")>]
    let max (m:MapExt<_,_>) = 
        match m.TryMaxKey with
        | Some min -> min
        | None -> raise <| ArgumentException("The input sequence was empty.")

    
    [<CompiledName("TryItem")>]
    let tryItem i (m:MapExt<_,_>) = m.TryAt i
    
    [<CompiledName("TryItemV")>]
    let tryItemV i (m:MapExt<_,_>) = m.TryAtV i

    [<CompiledName("Item")>]
    let item i (m:MapExt<_,_>) = 
        match m.TryAt i with
            | Some t -> t
            | None -> raise <| IndexOutOfRangeException()

    [<CompiledName("Alter")>]
    let alter k f (m:MapExt<_,_>) = m.Alter(k, f)
    
    [<CompiledName("MapMonotonic")>]
    let mapMonotonic f (m:MapExt<_,_>) = m.MapMonotonic(f)
    
    [<CompiledName("ChooseMonotonic")>]
    let chooseMonotonic f (m:MapExt<_,_>) = m.ChooseMonotonic(f)

    [<CompiledName("Split")>]
    let split k (m:MapExt<_,_>) = m.Split k

    [<CompiledName("TryIndexOf")>]
    let tryIndexOf i (m:MapExt<_,_>) = m.TryIndexOf i

    [<CompiledName("GetReference")>]
    let reference i (m:MapExt<_,_>) = m.GetReference i


    [<CompiledName("Union")>]
    let union (l:MapExt<_,_>) r = l.Union r

    [<CompiledName("UnionWith")>]
    let unionWith f (l:MapExt<_,_>) r = l.UnionWith (r, f)
    
    [<CompiledName("IntersectWith")>]
    let intersectWith f (l:MapExt<_,_>) r = l.IntersectWith (r, f)
    
    [<CompiledName("Intersect")>]
    let intersect (l:MapExt<_,_>) r = l.Intersect r

    [<CompiledName("Map2")>]
    let map2 f (l:MapExt<_,_>) r = l.Map2 (r, f)

    [<CompiledName("Choose")>]
    let choose f (l:MapExt<_,_>) = l.Choose (f)
    
    [<CompiledName("Choose2")>]
    let choose2 f (l:MapExt<_,_>) r = l.Choose2 (r, f)
    
    [<CompiledName("Choose2Value")>]
    let choose2V f (l:MapExt<_,_>) r = l.Choose2V (r, f)
    
    [<CompiledName("Neighbours")>]
    let neighbours k (m:MapExt<_,_>) = m.Neighbours k
    
    [<CompiledName("NeighboursAt")>]
    let neighboursAt i (m:MapExt<_,_>) = m.NeighboursAt i
    
    [<CompiledName("NeighboursV")>]
    let neighboursV k (m:MapExt<_,_>) = m.NeighboursV k
    
    [<CompiledName("NeighboursAtV")>]
    let neighboursAtV i (m:MapExt<_,_>) = m.NeighboursAtV i

    [<CompiledName("TryGetIndex")>]
    let tryGetIndex k (m:MapExt<_,_>) = m.TryGetIndex k
    
    [<CompiledName("TryGetIndexV")>]
    let tryGetIndexV k (m:MapExt<_,_>) = m.TryGetIndexV k