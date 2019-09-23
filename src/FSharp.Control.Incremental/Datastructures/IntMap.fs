// Ported from http://hackage.haskell.org/package/containers-0.6.2.1/docs/src/Data.IntMap.Internal.html
//---------------------------------------------------------------------------
// Module      :  Data.IntMap.Internal
// Copyright   :  (c) Daan Leijen 2002
//                (c) Andriy Palamarchuk 2008
//                (c) wren romano 2016
// License     :  BSD-style (http://hackage.haskell.org/package/containers-0.6.2.1/src/LICENSE)
// Maintainer  :  libraries@haskell.org
// Portability :  portable
//---------------------------------------------------------------------------
namespace FSharp.Control.Incremental

open System.Collections
open System.Collections.Generic
open System

#nowarn "25" // incomplete pattern match
#nowarn "61"

type internal intmap<'T> =
    | Nil
    | Tip of int * 'T
    | Bin of int * int * intmap<'T> * intmap<'T>

    member x.FoldBackWithKey f z =
        let rec go z =
            function
            | Nil -> z
            | Tip(kx, x) -> f kx x z
            | Bin(_, _, l, r) -> go (go z r) l
        match x with
        | Bin(_, m, l, r) -> 
            if m < 0 then go (go z l) r  // put negative numbers before.
            else go (go z r) l
        | _ -> go z x
    
    member x.ToList() = x.FoldBackWithKey (fun k x xs -> (k, x) :: xs) []

    member x.ToSeq() =
        match x with
            | Nil -> Seq.empty
            | Tip(k,v) -> Seq.singleton (k,v)
            | Bin(_,_,l,r) -> Seq.append (l.ToSeq()) (Seq.delay r.ToSeq)
        
    member x.Count =
        match x with
            | Nil -> 0
            | Tip _ -> 1
            | Bin(_,_,l,r) -> l.Count + r.Count

    interface IEnumerable<int * 'T> with
        member x.GetEnumerator() =
            new IntMapEnumerator<_>(x) :> _
        
    interface System.Collections.IEnumerable with
        member x.GetEnumerator() =
            new IntMapEnumerator<_>(x) :> _

and private IntMapEnumerator<'a>(m : intmap<'a>) =
    let mutable stack = [m]
    let mutable current = Unchecked.defaultof<_>

    let rec moveNext() =
        match stack with
            | [] -> false
            | h :: rest ->
                stack <- rest
                match h with
                    | Nil -> 
                        moveNext()

                    | Tip(k,v) -> 
                        current <- (k,v)
                        true
                    | Bin(_,_,l,r) -> 
                        stack <- l :: r :: stack
                        moveNext()

    interface IEnumerator with
        member x.MoveNext() = moveNext()
        member x.Current = current :> obj
        member x.Reset() =
            stack <- [m]
            current <- Unchecked.defaultof<_>
    interface IEnumerator<int * 'a> with
        member x.Current = current
        member x.Dispose() =
            stack <- []
            current <- Unchecked.defaultof<_>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal IntMap =

    let inline private maskW i m = int (i &&& (~~~ (m - 1ul) ^^^ m))
    let inline private mask i m = maskW (uint32 i) (uint32 m)
    let inline private match' i p m = mask i m = p
    let inline private nomatch i p m = mask i m <> p
    let inline private zero i m = (uint32 i) &&& (uint32 m) = 0ul
    let inline private shorter m1 m2 = (uint32 m1) > (uint32 m2)

    let inline private highestBitMask x0 =
        let x1 = x0 ||| (x0 >>> 1)
        let x2 = x1 ||| (x1 >>> 2)
        let x3 = x2 ||| (x2 >>> 4)
        let x4 = x3 ||| (x3 >>> 8)
        let x5 = x4 ||| (x4 >>> 16)
        let x6 = x5 ||| (x5 >>> 32)   // for 64 bit platforms
        x6 ^^^ (x6 >>> 1)

    let inline private branchMask p1 p2 = int (highestBitMask (uint32 p1 ^^^ uint32 p2))

    let inline private join p1 t1 p2 t2 =
        let m = branchMask p1 p2
        let p = mask p1 m
        if zero p1 m then Bin(p, m, t1, t2)
        else Bin(p, m, t2, t1)

    let inline private bin p m l r =
        match l, r with
        | (l, Nil) -> l
        | (Nil, r) -> r
        | (l, r) -> Bin(p, m, l, r)

    ///O(1). Map is empty.  Credit: Haskell.org
    let isEmpty =
        function
        | Nil -> true
        | _ -> false


    ///O(min(n,W)). Lookup the value at a key in the map. Returns 'T option. Credit: Haskell.org
    let rec tryFind k =
        function
        | Bin(p, m, l, r) ->
            if nomatch k p m then None
            elif zero k m then tryFind k l
            else tryFind k r
        | Tip(kx, x) ->
            if k = kx then Some x
            else None
        | _ -> None

    let rec tryRemove k n =
        match n with
        | Bin(p, m, l, r) ->
            if nomatch k p m then 
                None
            elif zero k m then 
                match tryRemove k l with
                | Some (v, l') ->
                    Some (v, bin p m l' r)
                | None ->
                    None
            else    
                match tryRemove k r with
                | Some (v, r') ->
                    Some (v, bin p m l r')
                | None ->
                    None
        | Tip(kx, x) ->
            if kx = k then Some (x, Nil)
            else None
        | _ ->
            None

    ///O(min(n,W)). Is the key a member of the map? Credit: Haskell.org
    let rec exists k =
        function
        | Bin(p, m, l, r) ->
            if nomatch k p m then false
            elif zero k m then exists k l
            else exists k r
        | Tip(kx, _) -> k = kx
        | _ -> false

    ///O(log n). Is the key not a member of the map? Credit: Haskell.org
    let notExists k m = not <| exists k m

    ///O(min(n,W)). Lookup the value at a key in the map. Credit: Haskell.org
    let rec find k m =
        let notFound() = failwith <| sprintf "intmap.find: key %d is not an element of the map" k
        match m with
        | Bin(p, m, l, r) -> 
            if nomatch k p m then notFound()
            else if zero k m then find k l
            else find k r
        | Tip(kx, x) when k = kx -> x
        | _ -> notFound()

    ///O(min(n,W)). The expression (findWithDefault def k map) returns the value at key k or returns def when the key is not an element of the map.  Credit: Haskell.org
    let rec findWithDefault def k =
        function
        | Bin(p, m, l, r) -> 
            if nomatch k p m then def
            elif zero k m then findWithDefault def k l
            else findWithDefault def k r
        | Tip(kx, x) when k = kx -> x
        | _ -> def

    let rec private unsafeFindMax =
        function
        | Nil -> None
        | Tip(ky, y) -> Some(ky, y)
        | Bin(_, _, _, r) -> unsafeFindMax r

    ///O(log n). Find largest key smaller than the given one and return the corresponding (key, value) pair.  Credit: Haskell.org
    let tryFindLT k t =
        let rec go def =
            function
            | Bin(p, m, l, r) ->
                if nomatch k p m then 
                    if k < p then unsafeFindMax def else unsafeFindMax r
                elif zero k m then go def l
                else go l r
            | Tip(ky, y) -> 
                if k <= ky then unsafeFindMax def
                else Some(ky, y)
            | _ -> unsafeFindMax def
        match t with
        | Bin(_, m, l, r) when m < 0 -> if k >= 0 then go r l else go Nil r
        | _ -> go Nil t

    let rec private unsafeFindMin =
        function
        | Nil -> None
        | Tip(ky, y) -> Some(ky, y)
        | Bin(_, _, l, _) -> unsafeFindMin l

    ///O(log n). Find smallest key greater than the given one and return the corresponding (key, value) pair. Credit: Haskell.org
    let tryFindGT k t =
        let rec go def =
            function
            | Bin(p, m, l, r) ->
                if nomatch k p m then
                    if k < p then unsafeFindMin l else unsafeFindMin def
                elif zero k m then go r l
                else go def r
            | Tip(ky, y) ->
                if k >= ky then unsafeFindMin def
                else Some(ky, y)
            | _ -> unsafeFindMin def
        match t with
        | Bin(_, m, l, r) when m < 0 -> if k >= 0 then go Nil l else go l r
        | _ -> go Nil t

    ///O(log n). Find largest key smaller or equal to the given one and return the corresponding (key, value) pair. Credit: Haskell.org
    let tryFindLE k t =
        let rec go def =
            function
            | Bin(p, m, l, r) -> 
                if nomatch k p m then
                    if k < p then unsafeFindMax def else unsafeFindMax r
                elif zero k m then go def l
                else go l r
            | Tip(ky, y) ->
                if k < ky then unsafeFindMax def
                else Some(ky, y)
            | _ -> unsafeFindMax def
        match t with
        | Bin(_, m, l, r) when m < 0 -> if k >= 0 then go r l else go Nil r
        | _ -> go Nil t

    ///O(log n). Find smallest key greater or equal to the given one and return the corresponding (key, value) pair Credit: Haskell.org
    let tryFindGE k t =
        let rec go def =
            function
            | Bin(p, m, l, r) ->
                if nomatch k p m then
                    if k < p then unsafeFindMin l else unsafeFindMin def
                elif zero k m then go r l
                else go def r
            | Tip(ky, y) -> 
                if k > ky then unsafeFindMin def
                else Some(ky, y)
            | _ -> unsafeFindMin def
        match t with
        | Bin(_, m, l, r) when m < 0 -> if k >= 0 then go Nil l else go l r
        | _ -> go Nil t

    ///O(1). The empty map. Credit: Haskell.org
    let empty = Nil

    ///O(1). A map of one element. Credit: Haskell.org
    let inline singleton k x = Tip(k, x)

    ///O(min(n,W)). Insert a new key/value pair in the map. If the key is already present in the map, the associated value is replaced with the supplied value, i.e. insert is equivalent to insertWith const. Credit: Haskell.org
    let rec insert k x t =
        match t with
        | Bin(p, m, l, r) ->
            if nomatch k p m then join k (Tip(k, x)) p t
            elif zero k m then Bin(p, m, insert k x l, r)
            else Bin(p, m, l, insert k x r)
        | Tip(ky, _) ->
            if k = ky then Tip(k, x)
            else join k (Tip(k, x)) ky t
        | _ -> Tip(k, x)

    ///O(min(n,W)). Insert with a combining function. insertWithKey f key value mp will insert the pair (key, value) into mp if key does not exist in the map. If the key does exist, the function will insert f key new_value old_value. Credit: Haskell.org
    let rec insertWithKey f k x t =
        match t with
        | Bin(p, m, l, r) ->
            if nomatch k p m then join k (Tip(k, x)) p t
            elif zero k m then Bin(p, m, insertWithKey f k x l, r)
            else Bin(p, m, l, insertWithKey f k x r)
        | Tip(ky, y) ->
            if k = ky then Tip(k, f k x y)
            else join k (Tip(k, x)) ky t
        | _ -> Tip(k, x)

    ///O(min(n,W)). Insert with a combining function. insertWith f key value mp will insert the pair (key, value) into mp if key does not exist in the map. If the key does exist, the function will insert f new_value old_value. Credit: Haskell.org
    let insertWith f k x t = insertWithKey (fun _ x' y' -> f x' y') k x t

    ///O(min(n,W)). The expression (insertLookupWithKey f k x map) is a pair where the first element is equal to (lookup k map) and the second element equal to (insertWithKey f k x map). Credit: Haskell.org
    let rec insertTryFindWithKey f k x t =
        match t with
        | Bin(p, m, l, r) ->
            if nomatch k p m then (None, join k (Tip(k, x)) p t)
            elif zero k m then
                let found, l = insertTryFindWithKey f k x l
                (found, Bin(p, m, l, r))
            else
                let found, r = insertTryFindWithKey f k x r
                (found, Bin(p, m, l, r))
        | Tip(ky, y) ->
            if k = ky then (Some y, Tip(k, f k x y))
            else (None, join k (Tip(k, x)) ky t)
        | _ -> (None, Tip(k, x))

    ///O(min(n,W)). Delete a key and its value from the map. When the key is not a member of the map, the original map is returned. Credit: Haskell.org
    let rec delete k t =
        match t with
        | Bin(p, m, l, r) ->
            if nomatch k p m then t
            elif zero k m then bin p m (delete k l) r
            else bin p m l (delete k r)
        | Tip(ky, _) ->
            if k = ky then Nil
            else t
        | _ -> Nil

    ///O(min(n,W)). The expression (update f k map) updates the value x at k (if it is in the map). If (f k x) is Nothing, the element is deleted. If it is (Just y), the key k is bound to the new value y. Credit: Haskell.org
    let rec updateWithKey f k t =
        match t with
        | Bin(p, m, l, r) ->
            if nomatch k p m then t
            elif zero k m then bin p m (updateWithKey f k l) r
            else bin p m l (updateWithKey f k r)
        | Tip(ky, y) ->
            if k = ky then
                match f k y with
                | Some y -> Tip(ky, y)
                | None -> Nil
            else
                t
        | _ -> Nil

    ///O(min(n,W)). The expression (update f k map) updates the value x at k (if it is in the map). If (f x) is Nothing, the element is deleted. If it is (Just y), the key k is bound to the new value y. Credit: Haskell.org
    let update f k m = updateWithKey (fun _ x -> f x) k m

    ///O(min(n,W)). Adjust a value at a specific key. When the key is not a member of the map, the original map is returned. Credit: Haskell.org
    let adjustWithKey f k m = updateWithKey (fun k' x -> Some (f k' x)) k m

    ///O(min(n,W)). Adjust a value at a specific key. When the key is not a member of the map, the original map is returned. Credit: Haskell.org
    let adjust f k m = adjustWithKey (fun _ x -> f x) k m

    ///O(min(n,W)). Lookup and update. Credit: Haskell.org
    let rec updateTryFindWithKey f k t =
        match t with
        | Bin(p, m, l, r) ->
            if nomatch k p m then
                (None, t)
            elif zero k m then
                let (found, l) = updateTryFindWithKey f k l
                (found, bin p m l r)   
            else
                let (found, r) = updateTryFindWithKey f k r
                (found, bin p m l r)
        | Tip(ky, y) ->
            if k = ky then
                match f k y with
                | Some y' -> (Some y, Tip(ky, y'))
                | None -> (Some y, Nil)
            else
                (None, t)
        | _ -> (None, Nil)

    ///O(log n). The expression (alter f k map) alters the value x at k, or absence thereof. alter can be used to insert, delete, or update a value in an intmap. Credit: Haskell.org
    let rec alter f k t =
        match t with
        | Bin(p, m, l, r) ->
            if nomatch k p m then
                match f None with
                | None -> t
                | Some x -> join k (Tip(k, x)) p t
            elif zero k m then
                bin p m (alter f k l) r
            else
                bin p m l (alter f k r)
        | Tip(ky, y) -> 
            if k = ky then
                match f (Some y) with
                | Some x -> Tip(ky, x)
                | None -> Nil
            else
                match f None with
                | Some x -> join k (Tip(k, x)) ky t
                | None -> t
        | _ ->
            match f None with
            | Some x -> Tip(k, x)
            | None -> Nil

    let inline private mergeWithKey' bin' f g1 g2 =

        let inline maybe_join p1 t1 p2 t2  =
            match t1, t2 with
            | Nil, t2 -> t2
            | t1, Nil -> t1
            | _ ->  join p1 t1 p2 t2
     
        let rec merge1 p1 m1 t1 l1 r1 p2 m2 t2 =
            if nomatch p2 p1 m1 then maybe_join p1 (g1 t1) p2 (g2 t2)
            elif zero p2 m1 then bin' p1 m1 (go l1 t2) (g1 r1)
            else bin' p1 m1 (g1 l1) (go r1 t2)

        and merge2 p1 m1 t1 p2 m2 t2 l2 r2 =
            if nomatch p1 p2 m2 then maybe_join p1 (g1 t1) p2 (g2 t2)
            elif zero p1 m2 then bin' p2 m2 (go t1 l2) (g2 r2)
            else bin' p2 m2 (g2 l2) (go t1 r2)

        and go t1 t2 =
            match t1 with
            | Bin(p1, m1, l1, r1) ->
                match t2 with
                | Bin(p2, m2, l2, r2) -> 
                    if shorter m1 m2 then merge1 p1 m1 t1 l1 r1 p2 m2 t2
                    elif shorter m2 m1 then merge2 p1 m1 t1 p2 m2 t2 l2 r2
                    elif p1 = p2 then bin' p1 m1 (go l1 l2) (go r1 r2)
                    else maybe_join p1 (g1 t1) p2 (g2 t2)
                | Tip (k2', _) ->
                    let rec merge t2 k2 t1 =
                        match t1 with
                        | Bin(p1, m1, l1, r1) ->
                            if nomatch k2 p1 m1 then maybe_join p1 (g1 t1) k2 (g2 t2)
                            else if zero k2 m1 then bin' p1 m1 (merge t2 k2 l1) (g1 r1)
                            else bin' p1 m1 (g1 l1) (merge t2 k2 r1)
                        | Tip(k1, _) -> 
                            if k1 = k2 then f t1 t2
                            else maybe_join k1 (g1 t1) k2 (g2 t2)
                        | _ -> g2 t2
                    merge t2 k2' t1
                | _ -> g1 t1
            | Tip(k1', _) -> 
                let rec merge t1 k1 t2 =
                    match t2 with
                    | Bin(p2, m2, l2, r2) ->
                        if nomatch k1 p2 m2 then maybe_join k1 (g1 t1) p2 (g2 t2)
                        elif zero k1 m2 then bin' p2 m2 (merge t1 k1 l2) (g2 r2)
                        else bin' p2 m2 (g2 l2) (merge t1 k1 r2)
                    | Tip(k2, _) ->
                        if k1 = k2 then f t1 t2
                        else maybe_join k1 (g1 t1) k2 (g2 t2)
                    | _ -> g1 t1
                merge t1 k1' t2
            | _ -> g2 t2
        go

    ///Refer to Haskell documentation. Unexpected code growth or corruption of the data structure can occure from wrong use. Credit: Haskell.org
    let mergeWithKey f g1 g2 =
        let combine =
            fun (Tip(k1, x1)) (Tip(_, x2)) ->
                match f k1 x1 x2 with
                | None -> Nil
                | Some x -> Tip(k1, x)
        mergeWithKey' bin combine g1 g2

    let inline konst a _ = a

    let append m1 m2 = mergeWithKey' (fun x y m1' m2' -> Bin(x, y, m1', m2')) konst id id m1 m2

    let appendWithKey f m1 m2 =
        mergeWithKey' (fun x y m1' m2' -> Bin(x, y, m1', m2')) (fun (Tip(k1, x1)) (Tip(_, x2)) -> Tip(k1, f k1 x1 x2)) id id m1 m2

    let appendWith f m1 m2 = appendWithKey (fun _ x y -> f x y) m1 m2

    let concat xs = List.fold append empty xs

    let concatWith f xs = List.fold (appendWith f) empty xs

    ///O(n+m). Difference between two maps (based on keys). Credit: Haskell.org
    let difference m1 m2 = mergeWithKey (fun _ _ _ -> None) id (konst Nil) m1 m2

    ///O(n+m). Difference with a combining function. When two equal keys are encountered, the combining function is applied to the key and both values. If it returns Nothing, the element is discarded (proper set difference). If it returns (Just y), the element is updated with a new value y. Credit: Haskell.org
    let differenceWithKey f m1 m2 = mergeWithKey f id (konst Nil) m1 m2

    ///O(n+m). Difference with a combining function. Credit: Haskell.org
    let differenceWith f m1 m2 = differenceWithKey (fun _ x y -> f x y) m1 m2

    ///O(n+m). The (left-biased) intersection of two maps (based on keys). Credit: Haskell.org
    let intersection m1 m2 = mergeWithKey' bin konst (konst Nil) (konst Nil) m1 m2

    ///O(n+m). The intersection with a combining function. Credit: Haskell.org
    let intersectionWithKey f m1 m2 =
        mergeWithKey' bin (fun (Tip(k1, x1)) (Tip(_, x2)) -> Tip(k1, f k1 x1 x2)) (konst Nil) (konst Nil) m1 m2

    ///O(n+m). The intersection with a combining function. Credit: Haskell.org
    let intersectionWith f m1 m2 = intersectionWithKey (fun _ x y -> f x y) m1 m2

    ///O(log n). Update the value at the minimal key. Credit: Haskell.org
    let updateMinWithKey f t =
        let rec go f =
            function
            | Bin(p, m, l, r) -> bin p m (go f l) r
            | Tip(k, y) ->
                match f k y with
                | Some y -> Tip(k, y)
                | None -> Nil
            | Nil -> failwith "updateMinWithKey Nil"
        match t with
        | Bin(p, m, l, r) when m < 0 -> bin p m l (go f r)
        | _ -> go f t

    ///O(log n). Update the value at the maximal key. Credit: Haskell.org
    let updateMaxWithKey f t =
        let rec go f =
            function
            | Bin(p, m, l, r) -> bin p m l (go f r)
            | Tip(k, y) ->
                match f k y with
                | Some y -> Tip(k, y)
                | None -> Nil
            | Nil -> failwith "updateMaxWithKey Nil"
        match t with
        | Bin(p, m, l, r) when m < 0 -> bin p m (go f l) r
        | _ -> go f t

    ///O(log n). Retrieves the maximal (key,value) couple of the map, and the map stripped from that element. fails (in the monad) when passed an empty map. Credit: Haskell.org
    let maxViewWithKey t =
        let rec go =
            function
            | Bin(p, m, l, r) -> let (result, r) = go r in (result, bin p m l r)
            | Tip(k, y) -> ((k, y), Nil)
            | Nil -> failwith "maxViewWithKey Nil"
        match t with
        | Nil -> None
        | Bin(p, m, l, r) when m < 0 -> let (result, l) = go l in Some(result, bin p m l r)
        | _ -> Some(go t)

    ///O(log n). Retrieves the minimal (key,value) couple of the map, and the map stripped from that element. fails (in the monad) when passed an empty map. Credit: Haskell.org
    let minViewWithKey t =
        let rec go =
            function
            | Bin(p, m, l, r) -> let (result, l) = go l in (result, bin p m l r)
            | Tip(k, y) -> ((k,y), Nil)
            | Nil -> failwith "minViewWithKey Nil"
        match t with
        | Nil -> None
        | Bin(p, m, l, r) when m < 0 -> let (result, r) = go r in Some(result, bin p m l r)
        | _ -> Some(go t)

    ///O(log n). Update the value at the maximal key. Credit: Haskell.org
    let updateMax f = updateMaxWithKey (konst f)

    ///O(log n). Update the value at the minimal key. Credit: Haskell.org
    let updateMin f = updateMinWithKey (konst f)

    let private first f (x, y) = (f x, y)

    ///O(min(n,W)). Retrieves the maximal key of the map, and the map stripped of that element, or Nothing if passed an empty map. Credit: Haskell.org
    let maxView t = Option.map (first snd) (maxViewWithKey t)

    ///O(min(n,W)). Retrieves the minimal key of the map, and the map stripped of that element, or Nothing if passed an empty map. Credit: Haskell.org
    let minView t = Option.map (first snd) (minViewWithKey t)

    ///O(log n). Retrieves the maximal key of the map, and the map stripped from that element. Credit: Haskell.org
    let deleteFindMax t =
        match maxViewWithKey <| t with
        | Some x -> x
        | _ -> failwith "deleteFindMax: empty map has no maximal element"

    ///O(log n). Retrieves the minimal key of the map, and the map stripped from that element. Credit: Haskell.org
    let deleteFindMin t =
        match minViewWithKey <| t with
        | Some x -> x
        | _ -> failwith "deleteFindMin: empty map has no minimal element"

    ///O(log n). The minimal key of the map. Credit: Haskell.org
    let findMin t =
        let rec go =
            function
            | Tip(k, v) -> (k, v)
            | Bin(_, _, l, _) -> go l
            | _ -> failwith "findMin Nil"
        match t with
        | Bin(_, m, l, r) -> if m < 0 then go r else go l
        | Tip(k, v) -> (k, v)
        | _ -> failwith "findMin: empty map has no minimal element"

    ///O(log n). The maximal key of the map. Credit: Haskell.org
    let findMax t =
        let rec go =
            function
            | Tip(k, v) -> (k, v)
            | Bin(_, _, _, r) -> go r
            | _ -> failwith "findMax Nil"
        match t with
        | Bin(_, m, l, r) -> if m < 0 then go l else go r
        | Tip(k, v) -> (k, v)
        | _ -> failwith "findMax: empty map has no maximal element"

    ///O(log n). Delete the minimal key. Credit: Haskell.org
    let deleteMin t = 
        match minView <| t with
        | Some x -> snd x
        | _ -> Nil

    ///O(log n). Delete the maximal key. Credit: Haskell.org
    let deleteMax t =
        match maxView <| t with
        | Some x -> snd x
        | _ -> Nil

    ///O(n). Map a function over all values in the map. Credit: Haskell.org
    let rec mapWithKey f =
        function
        | Bin(p, m, l, r) -> Bin(p, m, mapWithKey f l, mapWithKey f r)
        | Tip(k, x) -> Tip(k, f k x)
        | Nil -> Nil

    ///O(n). Map a function over all values in the map. Credit: Haskell.org
    let rec map<'a, 'b> (f : 'a -> 'b) (m : intmap<'a>) : intmap<'b> =
        match m with
        | Bin(p, m, l, r) -> Bin(p, m, map f l, map f r)
        | Tip(k, x) -> Tip(k, f x)
        | Nil -> Nil


    let rec private mapAccumL f a =
        function
        | Bin(p, m, l, r) ->
            let (a1,l) = mapAccumL f a l
            let (a2,r) = mapAccumL f a1 r
            (a2, Bin(p, m, l, r))
        | Tip(k, x) -> let (a,x) = f a k x in (a,Tip(k, x))
        | Nil -> (a, Nil)

    ///O(n). The function mapAccum threads an accumulating argument through the map in ascending order of keys. Credit: Haskell.org
    let mapAccumWithKey f a t = mapAccumL f a t

    ///O(n). The function mapAccumWithKey threads an accumulating argument through the map in ascending order of keys. Credit: Haskell.org
    let mapAccum f = mapAccumWithKey (fun a' _ x -> f a' x)

    ///O(n). Filter all keys/values that satisfy some predicate. Credit: Haskell.org
    let rec filterWithKey predicate =
        function
        | Bin(p, m, l, r) -> bin p m (filterWithKey predicate l) (filterWithKey predicate r)
        | Tip(k, x) -> if predicate k x then Tip(k, x) else Nil
        | _ -> Nil

    ///O(n). Filter all values that satisfy some predicate. Credit: Haskell.org
    let filter p m = filterWithKey (fun _ x -> p x) m

    ///O(n). partition the map according to some predicate. The first map contains all elements that satisfy the predicate, the second all elements that fail the predicate. See also split. Credit: Haskell.org
    let rec partitionWithKey predicate t =
        match t with
        | Bin(p, m, l, r)  ->
            let (l1, l2) = partitionWithKey predicate l
            let (r1, r2) = partitionWithKey predicate r
            (bin p m l1 r1, bin p m l2 r2)
        | Tip(k, x) -> if predicate k x then (t, Nil) else (Nil, t)
        | _ -> (Nil, Nil)

    ///O(n). partition the map according to some predicate. The first map contains all elements that satisfy the predicate, the second all elements that fail the predicate. See also split. Credit: Haskell.org
    let partition p m = partitionWithKey (fun _ x -> p x) m

    ///O(n). Map keys/values and collect the Just results. Credit: Haskell.org
    let rec mapOptionWithKey f =
        function
        | Bin(p, m, l, r) -> bin p m (mapOptionWithKey f l) (mapOptionWithKey f r)
        | Tip(k, x) ->
            match f k x with
            | Some y -> Tip(k, y)
            | None -> Nil
        | Nil -> Nil
        
    ///O(n). Map keys/values and collect the Just results. Credit: Haskell.org
    let rec mapOptionWithKey2 (f : int -> 'a -> option<'b * 'c>) : intmap<'a> -> intmap<'b> * intmap<'c>  =
        function
        | Bin(p, m, l, r) -> 
            let la, lb = mapOptionWithKey2 f l
            let ra, rb = mapOptionWithKey2 f r
            bin p m la ra,
            bin p m lb rb
        | Tip(k, x) ->
            match f k x with
            | Some (a,b) -> Tip(k, a), Tip(k, b)
            | None -> Nil, Nil
        | Nil -> Nil, Nil

    ///O(n). Map values and collect the Just results. Credit: Haskell.org
    let mapOption f = mapOptionWithKey (fun _ x -> f x)

    ///O(n). Map keys/values and separate the Left and Right results. Credit: Haskell.org
    let rec mapChoiceWithKey f =
        function
        | Bin(p, m, l, r) ->
            let (l1, l2) = mapChoiceWithKey f l
            let (r1, r2) = mapChoiceWithKey f r
            (bin p m l1 r1, bin p m l2 r2)
        | Tip(k, x) ->
            match f k x with
            | Choice1Of2 y  -> (Tip(k, y), Nil)
            | Choice2Of2 z -> (Nil, Tip(k, z))
        | Nil -> (Nil, Nil)

    ///O(n). Map values and separate the Left and Right results. Credit: Haskell.org
    let mapChoice f = mapChoiceWithKey (fun _ x -> f x)

    ///O(log n). The expression (split k map) is a pair (map1,map2) where all keys in map1 are lower than k and all keys in map2 larger than k. Any key equal to k is found in neither map1 nor map2. Credit: Haskell.org
    let split k t =
        let rec go k t =
            match t with
            | Bin(p, m, l, r) ->
                if nomatch k p m then
                    if k > p then (t, Nil) else (Nil, t)
                elif zero k m then
                    let (lt, gt) = go k l
                    (lt, append gt r)
                else
                    let (lt, gt) = go k r
                    (append l lt, gt)
            | Tip(ky, _) ->
                if k > ky then (t, Nil)
                else if k < ky then (Nil, t)
                else (Nil, Nil)
            | _ -> (Nil, Nil)
        match t with
        | Bin(_, m, l, r) when  m < 0 ->
            if k >= 0 // handle negative numbers.
                then let (lt, gt) = go k l in let lt = append r lt in (lt, gt)
            else let (lt, gt) = go k r in let gt = append gt l in (lt, gt)
        | _ -> go k t

    ///O(log n). Performs a split but also returns whether the pivot key was found in the original map. Credit: Haskell.org
    let splitTryFind k t =
        let rec go k t =
            match t with
            | Bin(p, m, l, r) ->
                if nomatch k p m then
                    if k > p then (t, None, Nil) else (Nil, None, t)
                elif zero k m then
                    let (lt, fnd, gt) = go k l
                    let gt = append gt r
                    (lt, fnd, gt)
                else
                    let (lt, fnd, gt) = go k r
                    let lt = append l lt
                    (lt, fnd, gt)
            | Tip(ky, y) ->
                if k > ky then (t, None, Nil)
                elif k < ky then (Nil, None, t)
                else (Nil, Some y, Nil)
            | _ -> (Nil, None, Nil)
        match t with
        | Bin(_, m, l, r) when  m < 0 ->
            if k >= 0 // handle negative numbers.
                then let (lt, fnd, gt) = go k l in let lt = append r lt in (lt, fnd, gt)
            else let (lt, fnd, gt) = go k r in let gt = append gt l in (lt, fnd, gt)
        | _ -> go k t

    ///O(n). FoldBack the values in the map, such that fold f z == Prelude.foldr f z . elems. Credit: Haskell.org
    let foldBack f z =
        let rec go z =
            function
            | Tip(_, x) -> f x z
            | Bin(_, _, l, r) -> go (go z r) l
            | _ -> z
        fun t ->
            match t with
            | Bin(_, m, l, r) ->
                if m < 0 then go (go z l) r  // put negative numbers before.
                else go (go z r) l
            | _ -> go z t

    ///O(n). Fold the values in the map, such that fold f z == Prelude.foldr f z . elems. Credit: Haskell.org
    let fold f z =
        let rec go z =
            function
            | Tip(_, x) -> f z x
            | Bin(_, _, l, r) -> go (go z l) r
            | _ -> z
        fun t ->
            match t with
            | Bin(_, m, l, r) ->
                if m < 0 then go (go z r) l  // put negative numbers before.
                else go (go z l) r
            | _ -> go z t

    ///O(n). FoldBack the keys and values in the map, such that foldWithKey f z == Prelude.foldr (uncurry f) z . toAscList. Credit: Haskell.org
    let inline foldBackWithKey f z = fun (t: _ intmap) -> t.FoldBackWithKey f z

    ///O(n). Fold the keys and values in the map, such that foldWithKey f z == Prelude.foldr (uncurry f) z . toAscList. Credit: Haskell.org
    let foldWithKey f z =
        let rec go z =
            function
            | Tip(kx, x) -> f z kx x
            | Bin(_, _, l, r) -> go (go z l) r
            | Nil -> z
        fun t ->
            match t with
            | Bin(_, m, l, r) ->
                if m < 0 then go (go z r) l  // put negative numbers before.
                else go (go z l) r
            | _ -> go z t
    
    ///O(n). Return all elements of the map in the ascending order of their keys. Credit: Haskell.org
    let values m = foldBack (fun a b -> List.Cons (a, b)) [] m

    ///O(n). Return all keys of the map in ascending order. Credit: Haskell.org
    let keys m = foldBackWithKey (fun k _ ks -> k :: ks) [] m

    ///O(n). Convert the map to a list of key/value pairs. Credit: Haskell.org
    let toList (m: _ intmap) = m.ToList()

    ///O(n). Convert the map to a seq of key/value pairs. Credit: Haskell.org
    let toSeq (m : intmap<'a>) = m.ToSeq()

    ///O(n). Convert the map to an array of key/value pairs. Credit: Haskell.org
    let toArray m = m |> toList |> List.toArray

    ///O(n*min(n,W)). Create a map from a list of key/value pairs. Credit: Haskell.org
    let ofList xs =
        let ins t (k, x) = insert k x t
        List.fold ins empty xs

    ///O(n*min(n,W)). Build a map from a list of key/value pairs with a combining function. See also fromAscListWithKey'. Credit: Haskell.org
    let ofListWithKey f xs =
        let ins t (k, x) = insertWithKey f k x t
        List.fold ins empty xs

    ///O(n*min(n,W)). Create a map from a list of key/value pairs with a combining function. See also fromAscListWith. Credit: Haskell.org
    let ofListWith f xs = ofListWithKey (fun _ x y -> f x y) xs

    ///O(1). Create a map from a single key/value pair.
    let single k x = Tip(k, x)

    ///O(n*min(n,W)). Create a map from a seq of key/value pairs. Credit: Haskell.org
    let ofSeq xs = xs |> List.ofSeq |> ofList

    ///O(n*min(n,W)). Build a map from a seq of key/value pairs with a combining function. See also fromAscListWithKey'. Credit: Haskell.org
    let ofSeqWithKey f xs = xs |> List.ofSeq |> ofListWithKey f

    ///O(n*min(n,W)). Create a map from a seq of key/value pairs with a combining function. See also fromAscListWith. Credit: Haskell.org
    let ofSeqWith f xs = xs |> List.ofSeq |> ofListWith f

    ///O(n*min(n,W)). Create a map from an array of key/value pairs. Credit: Haskell.org
    let ofArray xs = xs |> List.ofArray |> ofList

    ///O(n*min(n,W)). Build a map from an array of key/value pairs with a combining function. See also fromAscListWithKey'. Credit: Haskell.org
    let ofArrayWithKey f xs = xs |> List.ofArray |> ofListWithKey f

    ///O(n*min(n,W)). Create a map from an array of key/value pairs with a combining function. See also fromAscListWith. Credit: Haskell.org
    let ofArrayWith f xs = xs |> List.ofArray |> ofListWith f

    ///O(n*min(n,W)). mapKeys f s is the map obtained by applying f to each key of s. The size of the result may be smaller if f maps two or more distinct keys to the same new key. In this case the value at the greatest of the original keys is retained. Credit: Haskell.org
    let mapKeys f = ofList << foldBackWithKey (fun k x xs -> (f k, x) :: xs) []

    ///O(n*log n). mapKeysWith c f s is the map obtained by applying f to each key of s. The size of the result may be smaller if f maps two or more distinct keys to the same new key. In this case the associated values will be combined using c. Credit: Haskell.org
    let mapKeysWith c f = ofListWith c << foldBackWithKey (fun k x xs -> (f k, x) :: xs) []

    ///O(n+m). The expression (isSubmapOfBy f m1 m2) returns True if all keys in m1 are in m2, and when f returns True when applied to their respective values. Credit: Haskell.org
    let rec isSubmapOfBy predicate t1 t2 =
        match t1 with
        | Bin(p1, m1, l1, r1) ->
            match t2 with 
            | Bin(p2, m2, l2, r2) -> 
                if shorter m1 m2 then false
                elif shorter m2 m1 then 
                    match' p1 p2 m2 &&
                    (if zero p1 m2 then isSubmapOfBy predicate t1 l2
                        else isSubmapOfBy predicate t1 r2)
                else
                    p1 = p2 && isSubmapOfBy predicate l1 l2 && isSubmapOfBy predicate r1 r2
            | _ -> false
        | Tip(k, x) ->
            match tryFind k t2 with
                | Some y  -> predicate x y
                | None -> false
        | _ -> true

    ///O(n+m). Is this a submap? Defined as (isSubmapOf = isSubmapOfBy (==)). Credit: Haskell.org
    let isSubmapOf m1 m2 = isSubmapOfBy (=) m1 m2

    type private Ordering =
        | GT
        | LT
        | EQ

    let rec private submapCmp predicate t1 t2 =

        let submapCmpLt p1 r1 t1 p2 m2 l2 r2  =
            if nomatch p1 p2 m2 then GT
            elif zero p1 m2 then submapCmp predicate t1 l2
            else submapCmp predicate t1 r2

        let submapCmpEq l1 r1 l2 r2 =
            match (submapCmp predicate l1 l2, submapCmp predicate r1 r2) with
            | (GT,_ ) -> GT
            | (_ ,GT) -> GT
            | (EQ,EQ) -> EQ
            | _ -> LT
        match t1 with
        | Bin(p1, m1, l1, r1) ->
            match t2 with 
            | Bin(p2, m2, l2, r2) ->
                if shorter m1 m2 then GT
                elif shorter m2 m1 then submapCmpLt p1 r1 t1 p2 m2 l2 r2
                elif p1 = p2 then submapCmpEq l1 r1 l2 r2
                else GT // disjoint
            | _ -> GT
        | Tip(kx, x) ->
            match t2 with
            | Tip(ky, y) ->
                if (kx = ky) && predicate x y then EQ
                else GT// disjoint
            | _ ->
                match tryFind kx t2 with
                | Some y when predicate x y -> LT
                | _ -> GT // disjoint
        | _ ->
            match t2 with 
            | Nil -> EQ
            | _ -> LT

    ///O(n+m). Is this a proper submap? (ie. a submap but not equal). The expression (isProperSubmapOfBy f m1 m2) returns True when m1 and m2 are not equal, all keys in m1 are in m2, and when f returns True when applied to their respective values.  Credit: Haskell.org
    let isProperSubmapOfBy predicate t1 t2 =
        match submapCmp predicate t1 t2 with
        | LT -> true
        | _ -> false

    ///O(n+m). Is this a proper submap? (ie. a submap but not equal). Defined as (isProperSubmapOf = isProperSubmapOfBy (==)). Credit: Haskell.org
    let isProperSubmapOf m1 m2 = isProperSubmapOfBy (=) m1 m2


    ///Compares two UIntMaps and calls back:
    ///del for any key-value-pair that is in m1 and not in m2, and
    ///add for any key-value-pair that is in m2 and not in m1, and
    ///mod for any key-value-pair is in both, but has changed.
    ///Untouched sub-trees that are reference-equal are not touched.
    let computeDelta (change : int -> 'a -> 'a -> option<'b>) (del : intmap<'a> -> intmap<'b>) (add : intmap<'a> -> intmap<'b>) =
        
        let inline ifChanged (Tip(k1, x1)) (Tip(_, x2)) =
            match change k1 x1 x2 with
            | None -> Nil
            | Some x -> Tip(k1, x)

        let inline maybe_join p1 t1 p2 t2  =
            match t1, t2 with
            | Nil, t2 -> t2
            | t1, Nil -> t1
            | _ ->  join p1 t1 p2 t2
     
        let rec merge1 p1 m1 t1 l1 r1 p2 m2 t2 =
            if nomatch p2 p1 m1 then maybe_join p1 (del t1) p2 (add t2)
            elif zero p2 m1 then bin p1 m1 (go l1 t2) (del r1)
            else bin p1 m1 (del l1) (go r1 t2)

        and merge2 p1 m1 t1 p2 m2 t2 l2 r2 =
            if nomatch p1 p2 m2 then maybe_join p1 (del t1) p2 (add t2)
            elif zero p1 m2 then bin p2 m2 (go t1 l2) (add r2)
            else bin p2 m2 (add l2) (go t1 r2)

        and go t1 t2 =
            if Object.ReferenceEquals (t1, t2) then
                Nil
            else
                match t1 with
                | Bin(p1, m1, l1, r1) ->
                    match t2 with
                    | Bin(p2, m2, l2, r2) ->
                        if shorter m1 m2 then merge1 p1 m1 t1 l1 r1 p2 m2 t2
                        elif shorter m2 m1 then merge2 p1 m1 t1 p2 m2 t2 l2 r2
                        elif p1 = p2 then bin p1 m1 (go l1 l2) (go r1 r2)
                        else maybe_join p1 (del t1) p2 (add t2)
                    | Tip (k2', _) ->
                        let rec merge t2 k2 t1 =
                            match t1 with
                            | Bin(p1, m1, l1, r1) ->
                                if nomatch k2 p1 m1 then maybe_join p1 (del t1) k2 (add t2)
                                elif zero k2 m1 then bin p1 m1 (merge t2 k2 l1) (del r1)
                                else bin p1 m1 (del l1) (merge t2 k2 r1)
                            | Tip(k1, _) ->
                                if k1 = k2 then ifChanged t1 t2
                                else maybe_join k1 (del t1) k2 (add t2)
                            | _ -> add t2
                        merge t2 k2' t1
                    | _ -> del t1
                | Tip(k1', _) ->
                    let rec merge t1 k1 t2 =
                        match t2 with
                        | Bin(p2, m2, l2, r2) ->
                            if nomatch k1 p2 m2 then maybe_join k1 (del t1) p2 (add t2)
                            elif zero k1 m2 then bin p2 m2 (merge t1 k1 l2) (add r2)
                            else bin p2 m2 (add l2) (merge t1 k1 r2)
                        | Tip(k2, _) ->
                            if k1 = k2 then ifChanged t1 t2
                            else maybe_join k1 (del t1) k2 (add t2)
                        | _ -> del t1
                    merge t1 k1' t2
                | _ -> add t2
        go

    

    let rec equals (valueEqual : 'a -> 'a -> bool) (l : intmap<'a>) (r : intmap<'a>) =
        if System.Object.ReferenceEquals(l, r) then
            true
        else
            match l, r with
                | Nil, Nil -> 
                    true

                | Tip(lh,l), Tip(rh,r) -> 
                    lh = rh && valueEqual l r

                | Bin(lp, lm, ll, lr), Bin(rp, rm, rl, rr) ->
                    lp = rp && lm = rm && equals valueEqual ll rl && equals valueEqual lr rr

                | _ ->
                    false