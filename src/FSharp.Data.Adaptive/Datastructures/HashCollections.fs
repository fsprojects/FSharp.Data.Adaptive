namespace FSharp.Data.Adaptive

open FSharp.Data.Adaptive
open System.Runtime.CompilerServices
open System.Collections.Generic
open System.Diagnostics

module private HashNumberCrunching =
    type Mask = uint32
    
    let inline combineHash (a: int) (b: int) =
        uint32 a ^^^ uint32 b + 0x9e3779b9u + ((uint32 a) <<< 6) + ((uint32 a) >>> 2) |> int

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

module internal HashImplementation = 
    open HashNumberCrunching

    [<AllowNullLiteral>]
    type SetLinked<'K> =
        val mutable public SetNext : SetLinked<'K>
        val mutable public Key : 'K
        new(k, n) = { Key = k; SetNext = n }
            
    [<AllowNullLiteral>]
    type MapLinked<'K, 'V> =
        inherit SetLinked<'K>
        val mutable public Value : 'V

        member x.MapNext
            with inline get() : MapLinked<'K, 'V> = downcast x.SetNext
            and inline set (v : MapLinked<'K, 'V>) = x.SetNext <- v

        new(k : 'K, v : 'V, n : MapLinked<'K, 'V>) = { inherit SetLinked<'K>(k, n :> SetLinked<'K>); Value = v }

    type NodeKind =
        | Leaf = 0uy
        | Inner = 1uy
        
    [<AllowNullLiteral; AbstractClass>]
    type SetNode<'K> =
        val mutable public Store : uint32
            
        member inline x.IsLeaf = 
            x.Store &&& 1u = 0u

        member x.Data
            with inline get() = x.Store >>> 1
            and inline set v = x.Store <- (v <<< 1) ||| (x.Store &&& 1u)

        new(k : NodeKind, data : uint32) = { Store = (data <<< 1) ||| uint32 k }

    type SetLeaf<'K> =
        inherit SetNode<'K>
        val mutable public Key : 'K
        val mutable public SetNext : SetLinked<'K>

        member x.Hash
            with inline get() = x.Data
            and inline set v = x.Data <- v

        new(hash : uint32, key : 'K, next : SetLinked<'K>) =
            { inherit SetNode<'K>(NodeKind.Leaf, hash); Key = key; SetNext = next }

    type MapLeaf<'K, 'V> =
        inherit SetLeaf<'K>
        val mutable public Value : 'V

        member x.MapNext
            with inline get() : MapLinked<'K, 'V> = downcast x.SetNext
            and inline set (v : MapLinked<'K, 'V>) = x.SetNext <- v

        new(hash : uint32, key : 'K, value : 'V, next : MapLinked<'K, 'V>) =
            { inherit SetLeaf<'K>(hash, key, next); Value = value }

    type Inner<'K> =
        inherit SetNode<'K>
        val mutable public Mask : uint32
        val mutable public Count : int
        val mutable public Left : SetNode<'K>
        val mutable public Right : SetNode<'K>
            
        static member GetCount(node : SetNode<'K>) =
            if isNull node then 0
            elif node.IsLeaf then 
                let node = node :?> SetLeaf<'K>
                if isNull node.SetNext then 1
                else
                    let mutable c = node.SetNext
                    let mutable cnt = 1
                    while not (isNull c) do 
                        cnt <- cnt + 1
                        c <- c.SetNext
                    cnt
            else
                let inner = node :?> Inner<'K>
                inner.Count
                

        member x.Prefix
            with inline get() = x.Data
            and inline set v = x.Data <- v

        new(prefix : uint32, mask : uint32, left : SetNode<'K>, right : SetNode<'K>) =
            let cnt = Inner.GetCount left + Inner.GetCount right
            { inherit SetNode<'K>(NodeKind.Inner, prefix); Mask = mask; Count = cnt; Left = left; Right = right }

    let size (node : SetNode<'K>) =
        Inner.GetCount node

    module SetLinked =
        let rec add (cmp : IEqualityComparer<'K>) (key : 'K) (n : SetLinked<'K>) =
            if isNull n then
                SetLinked(key, null)
            elif cmp.Equals(n.Key, key) then
                n
            else
                SetLinked(n.Key, add cmp key n.SetNext)
         
        let rec alter (cmp : IEqualityComparer<'K>) (key : 'K) (update : bool -> bool) (n : SetLinked<'K>) =
            if isNull n then
                if update false then 
                    SetLinked(key, null)
                else
                    null
            elif cmp.Equals(key, n.Key) then
                if update true then
                    n
                else
                    n.SetNext
            else
                let next = alter cmp key update n.SetNext
                SetLinked(n.Key, next)

        let rec addInPlace (cmp : IEqualityComparer<'K>) (key : 'K) (n : byref<SetLinked<'K>>) =
            if isNull n then
                n <- SetLinked(key, null)
                true
            elif cmp.Equals(n.Key, key) then
                false
            else
                addInPlace cmp key &n.SetNext
         
        let rec contains (cmp : IEqualityComparer<'K>) (key : 'K) (n : SetLinked<'K>) =
            if isNull n then
                false
            elif cmp.Equals(n.Key, key) then
                true
            else
                contains cmp key n.SetNext


        let rec filter (predicate : 'K -> bool) (n : SetLinked<'K>) =
            if isNull n then 
                null
            elif predicate n.Key then   
                SetLinked(n.Key, filter predicate n.SetNext)
            else
                filter predicate n.SetNext

        let rec tryRemove (cmp : IEqualityComparer<'K>) (key : 'K) (n : byref<SetLinked<'K>>) =
            if isNull n then
                false
            elif cmp.Equals(key, n.Key) then
                n <- n.SetNext
                true
            else
                let mutable next = n.SetNext
                if tryRemove cmp key &next then
                    n <- SetLinked(n.Key, next)
                    true
                else
                    false

        let rec equals (cmp : IEqualityComparer<'K>) (a : SetLinked<'K>) (b : SetLinked<'K>) =
            if isNull a then isNull b
            elif isNull b then false
            else
                let mutable b = b
                if tryRemove cmp a.Key &b then
                    equals cmp a.SetNext b
                else
                    false
        let rec toList (acc : list<'K>) (n : SetLinked<'K>) =
            if isNull n then acc
            else n.Key :: toList acc n.SetNext
                
        let rec copyTo (dst : 'K[]) (index : int) (n : SetLinked<'K>) =
            if isNull n then    
                index
            else
                dst.[index] <- n.Key
                copyTo dst (index + 1) n.SetNext
                    
        let rec mapToMap (mapping : 'K -> 'V) (n : SetLinked<'K>) =
            if isNull n then null
            else MapLinked(n.Key, mapping n.Key, mapToMap mapping n.SetNext)
                 
        let rec chooseToMapV (mapping : 'K -> voption<'V>) (n : SetLinked<'K>) =
            if isNull n then null
            else 
                match mapping n.Key with
                | ValueSome value ->
                    MapLinked(n.Key, value, chooseToMapV mapping n.SetNext)
                | ValueNone ->
                    chooseToMapV mapping n.SetNext
                    
        let rec overlaps 
            (cmp : IEqualityComparer<'K>)
            (a : SetLinked<'K>) (b : SetLinked<'K>) =
            if isNull a then false
            elif isNull b then false
            else
                contains cmp a.Key b ||
                contains cmp b.Key a ||
                overlaps cmp a.SetNext b.SetNext
                  
        let rec subset 
            (cmp : IEqualityComparer<'K>)
            (a : SetLinked<'K>) (b : SetLinked<'K>) =
            if isNull a then true
            elif isNull b then false
            else
                contains cmp a.Key b &&
                subset cmp a.SetNext b

        let rec union 
            (cmp : IEqualityComparer<'K>)
            (a : SetLinked<'K>) (b : SetLinked<'K>) =
            if isNull a then b
            elif isNull b then a
            else
                let mutable b = b
                tryRemove cmp a.Key &b |> ignore
                SetLinked(a.Key, union cmp a.SetNext b)
                
        let rec xor 
            (cmp : IEqualityComparer<'K>)
            (a : SetLinked<'K>) (b : SetLinked<'K>) =
            if isNull a then b
            elif isNull b then a
            else
                let mutable b = b
                if tryRemove cmp a.Key &b then
                    xor cmp a.SetNext b
                else
                    SetLinked(a.Key, xor cmp a.SetNext b)
                
        let rec difference 
            (cmp : IEqualityComparer<'K>)
            (a : SetLinked<'K>) (b : SetLinked<'K>) =
            if isNull a then null
            elif isNull b then a
            else
                let mutable a = a
                tryRemove cmp b.Key &a |> ignore
                difference cmp a b.SetNext
                
        let rec intersect 
            (cmp : IEqualityComparer<'K>)
            (a : SetLinked<'K>) (b : SetLinked<'K>) =
            if isNull a || isNull b then null
            else
                let mutable b = b
                if tryRemove cmp a.Key &b then
                    SetLinked(a.Key, intersect cmp a.SetNext b)
                else
                    intersect cmp a.SetNext b

        let rec computeDelta 
            (cmp : IEqualityComparer<'K>)
            (onlyLeft : 'K -> voption<'OP>) 
            (onlyRight : 'K -> voption<'OP>) 
            (a : SetLinked<'K>) (b : SetLinked<'K>) =

            if isNull a then 
                chooseToMapV onlyRight b

            elif isNull b then 
                chooseToMapV onlyLeft a

            else
                let mutable b = b
                if tryRemove cmp a.Key &b then
                    computeDelta cmp onlyLeft onlyRight a.SetNext b
                else
                    match onlyLeft a.Key with
                    | ValueNone ->  
                        computeDelta cmp onlyLeft onlyRight a.SetNext b
                    | ValueSome v ->
                        MapLinked(a.Key, v, computeDelta cmp onlyLeft onlyRight a.SetNext b)
                   
        let rec applyDeltaNoState
            (apply : OptimizedClosures.FSharpFunc<'K, bool, 'D, struct(bool * voption<'DOut>)>)
            (delta : MapLinked<'K, 'D>) 
            (state : byref<SetLinked<'K>>) =
                
            if isNull delta then
                state <- null
                null
            else
                let struct (exists, op) = apply.Invoke(delta.Key, false, delta.Value)

                let mutable restState = null
                let restDelta = applyDeltaNoState apply delta.MapNext &restState


                if exists then 
                    state <- SetLinked(delta.Key, restState)

                match op with
                | ValueSome op -> 
                    MapLinked(delta.Key, op, restDelta)
                | _ -> restDelta
                     
        let rec applyDelta
            (cmp : IEqualityComparer<'K>)
            (apply : OptimizedClosures.FSharpFunc<'K, bool, 'D, struct(bool * voption<'DOut>)>)
            (delta : MapLinked<'K, 'D>) 
            (state : byref<SetLinked<'K>>) =

            if isNull state then
                applyDeltaNoState apply delta &state
            elif isNull delta then
                null
            else
                let wasExisting = tryRemove cmp delta.Key &state
                let struct(exists, op) = apply.Invoke(delta.Key, wasExisting, delta.Value)
                let restDelta = applyDelta cmp apply delta.MapNext &state
                if exists then 
                    state <- SetLinked(delta.Key, state)

                match op with
                | ValueSome op -> 
                    MapLinked(delta.Key, op, restDelta)
                | ValueNone -> 
                    restDelta

    module MapLinked =
        let rec add (cmp : IEqualityComparer<'K>) (key : 'K) (value : 'V) (n : MapLinked<'K, 'V>) =
            if isNull n then
                MapLinked(key, value, null)
            elif cmp.Equals(n.Key, key) then
                MapLinked(key, value, n.MapNext)
            else
                MapLinked(n.Key, n.Value, add cmp key value n.MapNext)
                   
        let rec alter (cmp : IEqualityComparer<'K>) (key : 'K) (update : option<'V> -> option<'V>) (n : MapLinked<'K, 'V>) =
            if isNull n then
                match update None with
                | Some value -> MapLinked(key, value, null)
                | None -> null
            elif cmp.Equals(n.Key, key) then
                match update (Some n.Value) with
                | Some value -> MapLinked(key, value, n.MapNext)
                | None -> n.MapNext
            else
                MapLinked(n.Key, n.Value, alter cmp key update n.MapNext)
                   
        let rec alterV (cmp : IEqualityComparer<'K>) (key : 'K) (update : voption<'V> -> voption<'V>) (n : MapLinked<'K, 'V>) =
            if isNull n then
                match update ValueNone with
                | ValueSome value -> MapLinked(key, value, null)
                | ValueNone -> null
            elif cmp.Equals(n.Key, key) then
                match update (ValueSome n.Value) with
                | ValueSome value -> MapLinked(key, value, n.MapNext)
                | ValueNone -> n.MapNext
            else
                MapLinked(n.Key, n.Value, alterV cmp key update n.MapNext)
                   
        let rec addInPlace (cmp : IEqualityComparer<'K>) (key : 'K) (value : 'V) (n : byref<SetLinked<'K>>) =
            if isNull n then
                n <- MapLinked(key, value, null)
                true
            elif cmp.Equals(n.Key, key) then
                let n = n :?> MapLinked<'K, 'V>
                n.Key <- key
                n.Value <- value
                false
            else
                addInPlace cmp key value &n.SetNext
                
                
        let rec tryRemove (cmp : IEqualityComparer<'K>) (key : 'K) (n : byref<MapLinked<'K, 'V>>) =
            if isNull n then
                ValueNone
            elif cmp.Equals(key, n.Key) then
                let v = n.Value
                n <- n.MapNext
                ValueSome v
            else
                let mutable next = n.MapNext
                match tryRemove cmp key &next with
                | ValueNone ->
                    ValueNone
                | result ->
                    n <- MapLinked(n.Key, n.Value, next)
                    result

        let rec tryFindV (cmp : IEqualityComparer<'K>) (key : 'K) (n : MapLinked<'K, 'V>) =
            if isNull n then
                ValueNone
            elif cmp.Equals(key, n.Key) then
                ValueSome n.Value
            else
                tryFindV cmp key n.MapNext
                
        let rec tryFind (cmp : IEqualityComparer<'K>) (key : 'K) (n : MapLinked<'K, 'V>) =
            if isNull n then
                None
            elif cmp.Equals(key, n.Key) then
                Some n.Value
            else
                tryFind cmp key n.MapNext
                
        let rec containsKey (cmp : IEqualityComparer<'K>) (key : 'K) (n : MapLinked<'K, 'V>) =
            if isNull n then
                false
            elif cmp.Equals(key, n.Key) then
                true
            else
                containsKey cmp key n.MapNext

        let rec toList (acc : list<_>) (n : MapLinked<'K, 'V>) =
            if isNull n then acc
            else (n.Key,n.Value) :: toList acc n.MapNext
                          
        let rec toListV (acc : list<_>) (n : MapLinked<'K, 'V>) =
            if isNull n then acc
            else struct(n.Key,n.Value) :: toListV acc n.MapNext
                     
        let rec toValueList (acc : list<_>) (n : MapLinked<'K, 'V>) =
            if isNull n then acc
            else n.Value :: toValueList acc n.MapNext
                    
        let rec toListMap (mapping : OptimizedClosures.FSharpFunc<'K, 'V, 'T>) (acc : list<'T>) (n : MapLinked<'K, 'V>) =
            if isNull n then acc
            else mapping.Invoke(n.Key, n.Value) :: toListMap mapping acc n.MapNext
                
        let rec copyTo (dst : ('K * 'V)[]) (index : int) (n : MapLinked<'K, 'V>) =
            if isNull n then    
                index
            else
                dst.[index] <- (n.Key, n.Value)
                copyTo dst (index + 1) n.MapNext
                    
        let rec copyToV (dst : struct('K * 'V)[]) (index : int) (n : MapLinked<'K, 'V>) =
            if isNull n then    
                index
            else
                dst.[index] <- struct(n.Key, n.Value)
                copyToV dst (index + 1) n.MapNext
                    
        let rec copyValuesTo (dst : 'V[]) (index : int) (n : MapLinked<'K, 'V>) =
            if isNull n then    
                index
            else
                dst.[index] <- n.Value
                copyValuesTo dst (index + 1) n.MapNext
                      
        let rec fold (folder : OptimizedClosures.FSharpFunc<'S, 'K, 'V, 'S>) (state : 'S) (node : MapLinked<'K, 'V>) =
            if isNull node then
                state
            else
                let s1 = folder.Invoke(state, node.Key, node.Value)
                fold folder s1 node.MapNext
                       
        let rec exists (predicate : OptimizedClosures.FSharpFunc<'K, 'V, bool>) (node : MapLinked<'K, 'V>) =
            if isNull node then
                false
            else
                predicate.Invoke(node.Key, node.Value) ||
                exists predicate node.MapNext
                
        let rec forall (predicate : OptimizedClosures.FSharpFunc<'K, 'V, bool>) (node : MapLinked<'K, 'V>) =
            if isNull node then
                true
            else
                predicate.Invoke(node.Key, node.Value) &&
                forall predicate node.MapNext

        let rec equals (cmp : IEqualityComparer<'K>) (a : MapLinked<'K, 'V>) (b : MapLinked<'K, 'V>) =
            if isNull a then isNull b
            elif isNull b then false
            else
                let mutable b = b
                match tryRemove cmp a.Key &b with
                | ValueSome vb ->
                    DefaultEquality.equals a.Value vb &&
                    equals cmp a.MapNext b
                | ValueNone ->
                    false 
        let rec map (mapping : OptimizedClosures.FSharpFunc<'K, 'V, 'T>) (node : MapLinked<'K, 'V>) =
            if isNull node then
                null
            else
                MapLinked(node.Key, mapping.Invoke(node.Key, node.Value), map mapping node.MapNext)
                 
        let rec filter (predicate : OptimizedClosures.FSharpFunc<'K, 'V, bool>) (node : MapLinked<'K, 'V>) =
            if isNull node then
                null
            elif predicate.Invoke(node.Key, node.Value) then
                MapLinked(node.Key, node.Value, filter predicate node.MapNext)
                
            else
                filter predicate node.MapNext

        let rec choose (mapping : OptimizedClosures.FSharpFunc<'K, 'V, option<'T>>) (node : MapLinked<'K, 'V>) =
            if isNull node then
                null
            else
                match mapping.Invoke(node.Key, node.Value) with
                | Some v ->
                    MapLinked(node.Key, v, choose mapping node.MapNext)
                | None ->
                    choose mapping node.MapNext

        let rec chooseV (mapping : OptimizedClosures.FSharpFunc<'K, 'V, voption<'T>>) (node : MapLinked<'K, 'V>) =
            if isNull node then
                null
            else
                match mapping.Invoke(node.Key, node.Value) with
                | ValueSome v ->
                    MapLinked(node.Key, v, chooseV mapping node.MapNext)
                | ValueNone ->
                    chooseV mapping node.MapNext
                    
        let rec choose2VLeft
            (mapping : OptimizedClosures.FSharpFunc<'K, voption<'A>, voption<'B>, voption<'C>>)
            (a : MapLinked<'K, 'A>)  =
            if isNull a then
                null
            else
                match mapping.Invoke(a.Key, ValueSome a.Value, ValueNone) with
                | ValueSome c ->
                    MapLinked(a.Key, c, choose2VLeft mapping a.MapNext)
                | ValueNone ->
                    choose2VLeft mapping a.MapNext
                      
        let rec choose2VRight
            (mapping : OptimizedClosures.FSharpFunc<'K, voption<'A>, voption<'B>, voption<'C>>)
            (b : MapLinked<'K, 'B>)  =
            if isNull b then
                null
            else
                match mapping.Invoke(b.Key, ValueNone, ValueSome b.Value) with
                | ValueSome c ->
                    MapLinked(b.Key, c, choose2VRight mapping b.MapNext)
                | ValueNone ->
                    choose2VRight mapping b.MapNext

        let rec choose2V
            (cmp : IEqualityComparer<'K>)
            (mapping : OptimizedClosures.FSharpFunc<'K, voption<'A>, voption<'B>, voption<'C>>)
            (a : MapLinked<'K, 'A>) (b : MapLinked<'K, 'B>)  =
            if isNull a then
                choose2VRight mapping b
            elif isNull b then
                choose2VLeft mapping a
            else
                let mutable b = b
                match tryRemove cmp a.Key &b with
                | ValueSome vb ->
                    match mapping.Invoke(a.Key, ValueSome a.Value, ValueSome vb) with
                    | ValueSome c ->
                        MapLinked(a.Key, c, choose2V cmp mapping a.MapNext b)
                    | ValueNone ->
                        choose2V cmp mapping a.MapNext b
                | ValueNone ->
                    match mapping.Invoke(a.Key, ValueSome a.Value, ValueNone) with
                    | ValueSome c ->
                        MapLinked(a.Key, c, choose2V cmp mapping a.MapNext b)
                    | ValueNone ->
                        choose2V cmp mapping a.MapNext b
                        
                    
        let rec union 
            (cmp : IEqualityComparer<'K>)
            (a : MapLinked<'K, 'V>) (b : MapLinked<'K, 'V>) =
            if isNull a then b
            elif isNull b then a
            else
                let mutable a = a
                tryRemove cmp b.Key &a |> ignore
                MapLinked(b.Key, b.Value, union cmp a b.MapNext)
                    
                    
                    
        let rec unionWithSelf (mapping : OptimizedClosures.FSharpFunc<'K, 'V, 'V, 'V>) (node : MapLinked<'K, 'V>) =
            if isNull node then
                null
            else
                MapLinked(node.Key, mapping.Invoke(node.Key, node.Value, node.Value), unionWithSelf mapping node.MapNext)
                 
        let rec unionWith
            (cmp : IEqualityComparer<'K>)
            (resolve : OptimizedClosures.FSharpFunc<'K, 'V, 'V, 'V>)
            (a : MapLinked<'K, 'V>) (b : MapLinked<'K, 'V>) =
            if isNull a then b
            elif isNull b then a
            else
                let mutable a = a
                match tryRemove cmp b.Key &a with
                | ValueSome aValue ->
                    let v = resolve.Invoke(b.Key, aValue, b.Value)
                    MapLinked(b.Key, v, unionWith cmp resolve a b.MapNext)
                | ValueNone -> 
                    MapLinked(b.Key, b.Value, unionWith cmp resolve a b.MapNext)
                
        let rec unionWithSelfV (mapping : OptimizedClosures.FSharpFunc<'K, 'V, 'V, voption<'V>>) (node : MapLinked<'K, 'V>) =
            if isNull node then
                null
            else
                match mapping.Invoke(node.Key, node.Value, node.Value) with
                | ValueSome v ->
                    MapLinked(node.Key, v, unionWithSelfV mapping node.MapNext)
                | ValueNone ->
                    unionWithSelfV mapping node.MapNext
                       
        let rec unionWithV
            (cmp : IEqualityComparer<'K>)
            (resolve : OptimizedClosures.FSharpFunc<'K, 'V, 'V, voption<'V>>)
            (a : MapLinked<'K, 'V>) (b : MapLinked<'K, 'V>) =
            if isNull a then b
            elif isNull b then a
            else
                let mutable a = a
                match tryRemove cmp b.Key &a with
                | ValueSome aValue ->
                    match resolve.Invoke(b.Key, aValue, b.Value) with
                    | ValueSome v ->
                        MapLinked(b.Key, v, unionWithV cmp resolve a b.MapNext)
                    | ValueNone ->
                        unionWithV cmp resolve a b.MapNext
                | ValueNone -> 
                    MapLinked(b.Key, b.Value, unionWithV cmp resolve a b.MapNext)
                
        let rec computeDelta 
            (cmp : IEqualityComparer<'K>)
            (onlyLeft : OptimizedClosures.FSharpFunc<'K, 'V, voption<'OP>>) 
            (onlyRight : OptimizedClosures.FSharpFunc<'K, 'V, voption<'OP>>) 
            (both : OptimizedClosures.FSharpFunc<'K, 'V, 'V, voption<'OP>>) 
            (a : MapLinked<'K, 'V>) (b : MapLinked<'K, 'V>) =

            if isNull a then 
                chooseV onlyRight b

            elif isNull b then 
                chooseV onlyLeft a

            else
                let mutable b = b
                match tryRemove cmp a.Key &b with
                | ValueSome bv ->
                    match both.Invoke(a.Key, a.Value, bv) with
                    | ValueNone ->  
                        computeDelta cmp onlyLeft onlyRight both a.MapNext b
                    | ValueSome v ->
                        MapLinked(a.Key, v, computeDelta cmp onlyLeft onlyRight both a.MapNext b)
                | ValueNone ->
                    match onlyLeft.Invoke(a.Key, a.Value) with
                    | ValueNone ->  
                        computeDelta cmp onlyLeft onlyRight both a.MapNext b
                    | ValueSome v ->
                        MapLinked(a.Key, v, computeDelta cmp onlyLeft onlyRight both a.MapNext b)
                    
        let rec applyDeltaNoState
            (apply : OptimizedClosures.FSharpFunc<'K, voption<'V>, 'D, struct(voption<'V> * voption<'DOut>)>)
            (delta : MapLinked<'K, 'D>) 
            (state : byref<MapLinked<'K, 'V>>) =
                
            if isNull delta then
                state <- null
                null
            else
                let struct (newValue, op) = apply.Invoke(delta.Key, ValueNone, delta.Value)

                let mutable restState = null
                let restDelta = applyDeltaNoState apply delta.MapNext &restState


                match newValue with
                | ValueSome newValue ->
                    state <- MapLinked(delta.Key, newValue, restState)
                | ValueNone ->
                    ()

                match op with
                | ValueSome op -> 
                    MapLinked(delta.Key, op, restDelta)
                | _ -> restDelta
                     
        let rec applyDelta
            (cmp : IEqualityComparer<'K>)
            (apply : OptimizedClosures.FSharpFunc<'K, voption<'V>, 'D, struct(voption<'V> * voption<'DOut>)>)
            (delta : MapLinked<'K, 'D>) 
            (state : byref<MapLinked<'K, 'V>>) =

            if isNull state then
                applyDeltaNoState apply delta &state
            elif isNull delta then
                null
            else
                let wasExisting = tryRemove cmp delta.Key &state
                let struct(exists, op) = apply.Invoke(delta.Key, wasExisting, delta.Value)
                let restDelta = applyDelta cmp apply delta.MapNext &state
                match exists with
                | ValueSome newValue ->
                    state <- MapLinked(delta.Key, newValue, state)
                | _ ->
                    ()

                match op with
                | ValueSome op -> 
                    MapLinked(delta.Key, op, restDelta)
                | ValueNone -> 
                    restDelta
          
          
    module SetNode =

        let inline join (p0 : uint32) (t0 : SetNode<'K>) (p1 : uint32) (t1 : SetNode<'K>) =
            if isNull t0 then t1
            elif isNull t1 then t0
            else 
                let mask = getMask p0 p1
                let prefix = getPrefix p0 mask
                if zeroBit p0 mask = 0u then Inner(prefix, mask, t0, t1) :> SetNode<_>
                else Inner(prefix, mask, t1, t0) :> SetNode<_>

        let inline newInner (prefix : uint32) (mask : uint32) (l : SetNode<'K>) (r : SetNode<'K>) =
            if isNull l then r
            elif isNull r then l
            else Inner(prefix, mask, l, r) :> SetNode<_>

        let rec add (cmp : IEqualityComparer<'K>) (hash : uint32) (key : 'K) (node : SetNode<'K>) =
            if isNull node then
                SetLeaf(hash, key, null) :> SetNode<_>

            elif node.IsLeaf then
                let node : SetLeaf<'K> = downcast node
                if node.Hash = hash then
                    if cmp.Equals(node.Key, key) then
                        node :> SetNode<_>
                    else
                        SetLeaf(node.Hash, node.Key, SetLinked.add cmp key node.SetNext) :> SetNode<_>
                else
                    join node.Hash node hash (SetLeaf(hash, key, null))
            else
                let node : Inner<'K> = downcast node
                match matchPrefixAndGetBit hash node.Prefix node.Mask with
                | 0u ->
                    newInner 
                        node.Prefix node.Mask 
                        (add cmp hash key node.Left) 
                        node.Right
                | 1u ->
                    newInner 
                        node.Prefix node.Mask 
                        node.Left
                        (add cmp hash key node.Right) 
                | _ ->
                    join node.Prefix node hash (SetLeaf(hash, key, null))
             
        let rec alter (cmp : IEqualityComparer<'K>) (hash : uint32) (key : 'K) (update : bool -> bool) (node : SetNode<'K>) =
            if isNull node then
                if update false then
                    SetLeaf(hash, key, null) :> SetNode<_>
                else
                    null

            elif node.IsLeaf then
                let node : SetLeaf<'K> = downcast node
                if node.Hash = hash then
                    if cmp.Equals(node.Key, key) then
                        if update true then node :> SetNode<_>
                        else 
                            let next = node.SetNext
                            if isNull next then null
                            else SetLeaf(node.Hash, next.Key,  next.SetNext) :> SetNode<_>
                    else
                        SetLeaf(node.Hash, node.Key, SetLinked.alter cmp key update node.SetNext) :> SetNode<_>
                else
                    if update false then
                        join node.Hash node hash (SetLeaf(hash, key, null))
                    else
                        node :> SetNode<_>
            else
                let node : Inner<'K> = downcast node
                match matchPrefixAndGetBit hash node.Prefix node.Mask with
                | 0u ->
                    newInner 
                        node.Prefix node.Mask 
                        (alter cmp hash key update node.Left) 
                        node.Right
                | 1u ->
                    newInner 
                        node.Prefix node.Mask 
                        node.Left
                        (alter cmp hash key update node.Right) 
                | _ ->
                    if update false then
                        join node.Prefix node hash (SetLeaf(hash, key, null))
                    else
                        node :> SetNode<_>

                       
        let rec addInPlace (cmp : IEqualityComparer<'K>) (hash : uint32) (key : 'K) (node : byref<SetNode<'K>>) =
            if isNull node then
                node <- SetLeaf(hash, key, null)
                true

            elif node.IsLeaf then
                let leaf : SetLeaf<'K> = downcast node
                if leaf.Hash = hash then
                    if cmp.Equals(leaf.Key, key) then
                        false
                    else
                        SetLinked.addInPlace cmp key &leaf.SetNext
                else
                    node <- join leaf.Hash leaf hash (SetLeaf(hash, key, null))
                    true
            else
                let inner : Inner<'K> = downcast node
                match matchPrefixAndGetBit hash inner.Prefix inner.Mask with
                | 0u ->
                    if addInPlace cmp hash key &inner.Left then
                        inner.Count <- inner.Count + 1
                        true
                    else
                        false
                | 1u ->
                    if addInPlace cmp hash key &inner.Right then
                        inner.Count <- inner.Count + 1
                        true
                    else
                        false
                | _ ->
                    node <- join inner.Prefix inner hash (SetLeaf(hash, key, null))
                    true
                 
        let rec tryRemove (cmp : IEqualityComparer<'K>) (hash : uint32) (key : 'K) (node : byref<SetNode<'K>>) =
            if isNull node then
                false
            elif node.IsLeaf then
                let n = node :?> SetLeaf<'K>
                if hash = n.Hash then
                    if cmp.Equals(key, n.Key) then
                        let next = n.SetNext
                        if isNull next then node <- null
                        else node <- SetLeaf(n.Hash, next.Key, next.SetNext)
                        true
                    else
                        let mutable next = n.SetNext
                        if SetLinked.tryRemove cmp key &next then
                            node <- SetLeaf(n.Hash, n.Key, next)
                            true
                        else
                            false
                else
                    false
            else
                let n = node :?> Inner<'K>
                match matchPrefixAndGetBit hash n.Prefix n.Mask with
                | 0u ->
                    let mutable l = n.Left
                    if tryRemove cmp hash key &l then
                        node <- newInner n.Prefix n.Mask l n.Right
                        true
                    else
                        false
                | 1u ->
                    let mutable r = n.Right
                    if tryRemove cmp hash key &r then
                        node <- newInner n.Prefix n.Mask n.Left r
                        true
                    else
                        false
                | _ ->
                    false

        let rec contains (cmp : IEqualityComparer<'K>) (hash : uint32) (key : 'K) (node : SetNode<'K>) =
            if isNull node then
                false
            elif node.IsLeaf then   
                let node = node :?> SetLeaf<'K>
                if node.Hash = hash then
                    cmp.Equals(node.Key, key) ||
                    SetLinked.contains cmp key node.SetNext
                else
                    false
            else
                let node = node :?> Inner<'K>
                match matchPrefixAndGetBit hash node.Prefix node.Mask with
                | 0u -> contains cmp hash key node.Left
                | 1u -> contains cmp hash key node.Right
                | _ -> false

        let rec equals (cmp : IEqualityComparer<'K>) (a : SetNode<'K>) (b : SetNode<'K>) =
            if isNull a then isNull b
            elif isNull b then isNull a
            elif System.Object.ReferenceEquals(a,b) then true
            elif a.IsLeaf then  
                if b.IsLeaf then
                    let a = a :?> SetLeaf<'K>
                    let b = b :?> SetLeaf<'K>
                    if a.Hash = b.Hash then
                        let la = SetLinked(a.Key, a.SetNext)
                        let lb = SetLinked(b.Key, b.SetNext) 
                        SetLinked.equals cmp la lb
                    else
                        false
                else
                    false   
            elif b.IsLeaf then false
            else
                let a = a :?> Inner<'K>
                let b = b :?> Inner<'K>
                if a.Prefix = b.Prefix && a.Mask = b.Mask then
                    equals cmp a.Left b.Left &&
                    equals cmp a.Right b.Right
                else
                    false

        let rec hash (acc : int) (a : SetNode<'K>) =
            if isNull a then
                acc
            elif a.IsLeaf then
                let a = a :?> SetLeaf<'K>
                let cnt =
                    let mutable c = 1
                    let mutable n = a.SetNext
                    while not (isNull n) do c <- c + 1; n <- n.SetNext
                    c
                combineHash acc (combineHash (int a.Hash) cnt)
            else
                let a = a :?> Inner<'K>
                let lh = hash acc a.Left
                let nh = combineHash lh (combineHash (int a.Prefix) (int a.Mask))
                hash nh a.Right
                
        let rec iter (action : 'K -> unit) (node : SetNode<'K>) =
            if isNull node then
                ()
            elif node.IsLeaf then
                let node = node :?> SetLeaf<'K>
                action node.Key
                let mutable c = node.SetNext
                while not (isNull c) do
                    action c.Key
                    c <- c.SetNext
            else
                let node = node :?> Inner<'K>
                iter action node.Left
                iter action node.Right
                
        let rec fold (folder : OptimizedClosures.FSharpFunc<'S, 'K, 'S>) (state : 'S) (node : SetNode<'K>) =
            if isNull node then
                state
            elif node.IsLeaf then
                let node = node :?> SetLeaf<'K>
                let mutable state = state
                state <- folder.Invoke(state, node.Key)
                let mutable c = node.SetNext
                while not (isNull c) do
                    state <- folder.Invoke(state, c.Key)
                    c <- c.SetNext
                state
            else
                let node = node :?> Inner<'K>
                let state = fold folder state node.Left
                fold folder state node.Right
                
        let rec exists (predicate : 'K -> bool) (node : SetNode<'K>) =
            if isNull node then
                false
            elif node.IsLeaf then
                let node = node :?> SetLeaf<'K>
                if predicate node.Key then
                    true
                else
                    let rec run (predicate : 'K -> bool) (l : SetLinked<'K>) =
                        if isNull l then false
                        elif predicate l.Key then true
                        else run predicate l.SetNext
                    run predicate node.SetNext
            else
                let node = node :?> Inner<'K>
                exists predicate node.Left ||
                exists predicate node.Right
               
        let rec forall (predicate : 'K -> bool) (node : SetNode<'K>) =
            if isNull node then
                true
            elif node.IsLeaf then
                let node = node :?> SetLeaf<'K>
                if predicate node.Key then
                    let rec run (predicate : 'K -> bool) (l : SetLinked<'K>) =
                        if isNull l then true
                        elif not (predicate l.Key) then false
                        else run predicate l.SetNext
                    run predicate node.SetNext
                else
                    false
            else
                let node = node :?> Inner<'K>
                forall predicate node.Left &&
                forall predicate node.Right
                    
    
        let rec filter (predicate : 'K -> bool) (node : SetNode<'K>) =
            if isNull node then
                null
            elif node.IsLeaf then
                let node = node :?> SetLeaf<'K>
                if predicate node.Key then
                    SetLeaf(node.Hash, node.Key, SetLinked.filter predicate node.SetNext) :> SetNode<_>
                else
                    let n = SetLinked.filter predicate node.SetNext
                    if isNull n then null
                    else SetLeaf(node.Hash, n.Key, n.SetNext) :> SetNode<_>
            else
                let node = node :?> Inner<'K>
                let l = filter predicate node.Left
                let r = filter predicate node.Right
                newInner node.Prefix node.Mask l r

        let rec toList (acc : list<'K>) (node : SetNode<'K>) =
            if isNull node then
                acc
            elif node.IsLeaf then
                let node = node :?> SetLeaf<'K>
                node.Key :: SetLinked.toList acc node.SetNext
            else
                let node = node :?> Inner<'K>
                toList (toList acc node.Right) node.Left
                    
        let rec copyTo (dst : 'K[]) (index : int) (node : SetNode<'K>) =
            if isNull node then
                index
            elif node.IsLeaf then
                let node = node :?> SetLeaf<'K>
                dst.[index] <- node.Key
                SetLinked.copyTo dst (index + 1) node.SetNext
            else
                let node = node :?> Inner<'K>
                let i0 = copyTo dst index node.Left
                copyTo dst i0 node.Right
                
        let rec mapToMap (mapping : 'K -> 'V) (node : SetNode<'K>) =
            if isNull node then
                null
            elif node.IsLeaf then
                let node = node :?> SetLeaf<'K>
                let v = mapping node.Key
                MapLeaf(node.Hash, node.Key, v, SetLinked.mapToMap mapping node.SetNext) :> SetNode<_>
            else
                let node = node :?> Inner<'K>
                let l = mapToMap mapping node.Left
                let r = mapToMap mapping node.Right
                Inner(node.Prefix, node.Mask, l, r) :> SetNode<_>

        let rec chooseToMapV (mapping : 'K -> voption<'T>) (node : SetNode<'K>) =
            if isNull node then
                null
            elif node.IsLeaf then
                let node = node :?> SetLeaf<'K>
                match mapping node.Key with
                | ValueNone ->
                    let next = SetLinked.chooseToMapV mapping node.SetNext
                    if isNull next then null
                    else MapLeaf(node.Hash, next.Key, next.Value, next.MapNext) :> SetNode<_>
                | ValueSome v ->
                    MapLeaf(node.Hash, node.Key, v, SetLinked.chooseToMapV mapping node.SetNext) :> SetNode<_>
            else
                let node = node :?> Inner<'K>
                let l = chooseToMapV mapping node.Left
                let r = chooseToMapV mapping node.Right
                newInner node.Prefix node.Mask l r

                
        let rec subset 
            (cmp : IEqualityComparer<'K>)
            (na : SetNode<'K>) (nb : SetNode<'K>) =
            if isNull na then 
                // empty contained everywhere
                true
            elif isNull nb then 
                // non-empty cannot be contained in empty
                false
            elif System.Object.ReferenceEquals(na, nb) then true
            elif na.IsLeaf then
                let a = na :?> SetLeaf<'K>
                if nb.IsLeaf then
                    let b = nb :?> SetLeaf<'K>
                    if a.Hash = b.Hash then
                        let la = SetLinked(a.Key, a.SetNext)
                        let lb = SetLinked(b.Key, b.SetNext)
                        SetLinked.subset cmp la lb
                    else
                        false
                else
                    let b = nb :?> Inner<'K>
                    match matchPrefixAndGetBit a.Hash b.Prefix b.Mask with
                    | 0u -> subset cmp na b.Left
                    | 1u -> subset cmp na b.Right
                    | _ -> false
            elif nb.IsLeaf then
                // Inner not contained in Leaf
                false
            else
                let a = na :?> Inner<'K>
                let b = nb :?> Inner<'K>
                let cc = compareMasks a.Mask b.Mask
                if cc > 0 then
                    // a in b
                    match matchPrefixAndGetBit a.Prefix b.Prefix b.Mask with
                    | 0u -> subset cmp na b.Left
                    | 1u -> subset cmp na b.Right
                    | _ -> false
                elif cc < 0 then
                    // b in a => not a subset
                    false
                elif a.Prefix = b.Prefix then
                    subset cmp a.Left b.Left &&
                    subset cmp a.Right b.Right
                else
                    false


        let rec overlaps 
            (cmp : IEqualityComparer<'K>)
            (na : SetNode<'K>) (nb : SetNode<'K>) =
            if isNull na then false
            elif isNull nb then false
            elif na.IsLeaf then
                let a = na :?> SetLeaf<'K>
                if nb.IsLeaf then
                    let b = nb :?> SetLeaf<'K>
                    if a.Hash = b.Hash then
                        let la = SetLinked(a.Key, a.SetNext)
                        let lb = SetLinked(b.Key, b.SetNext)
                        SetLinked.overlaps cmp la lb
                    else
                        false
                else
                    let b = nb :?> Inner<'K>
                    match matchPrefixAndGetBit a.Hash b.Prefix b.Mask with
                    | 0u -> overlaps cmp na b.Left
                    | 1u -> overlaps cmp na b.Right
                    | _ -> false
            elif nb.IsLeaf then
                let a = na :?> Inner<'K>
                let b = nb :?> SetLeaf<'K>
                match matchPrefixAndGetBit b.Hash a.Prefix a.Mask with
                | 0u -> overlaps cmp a.Left nb
                | 1u -> overlaps cmp a.Right nb
                | _ -> false
            else
                let a = na :?> Inner<'K>
                let b = nb :?> Inner<'K>
                let cc = compareMasks a.Mask b.Mask
                if cc > 0 then
                    // a in b
                    match matchPrefixAndGetBit a.Prefix b.Prefix b.Mask with
                    | 0u -> overlaps cmp na b.Left
                    | 1u -> overlaps cmp na b.Right
                    | _ -> false
                elif cc < 0 then
                    // b in a
                    match matchPrefixAndGetBit b.Prefix a.Prefix a.Mask with
                    | 0u -> overlaps cmp a.Left nb
                    | 1u -> overlaps cmp a.Right nb
                    | _ -> false
                elif a.Prefix = b.Prefix then
                    overlaps cmp a.Left b.Left ||
                    overlaps cmp a.Right b.Right
                else
                    false

        let rec union
            (cmp : IEqualityComparer<'K>)
            (na : SetNode<'K>) (nb : SetNode<'K>) =
            
            if isNull na then nb
            elif isNull nb then na
            elif System.Object.ReferenceEquals(na, nb) then na
            elif na.IsLeaf then
                let a = na :?> SetLeaf<'K>
                if nb.IsLeaf then
                    let b = nb :?> SetLeaf<'K>
                    if a.Hash = b.Hash then
                        // TODO: avoid allocating SetLinkeds
                        let la = SetLinked(a.Key, a.SetNext)
                        let lb = SetLinked(b.Key, b.SetNext)
                        let res = SetLinked.union cmp la lb
                        if isNull res then null
                        else SetLeaf(a.Hash, res.Key, res.SetNext) :> SetNode<_>
                    else
                        join a.Hash na b.Hash nb
                else
                    let b = nb :?> Inner<'K>
                    match matchPrefixAndGetBit a.Hash b.Prefix b.Mask with
                    | 0u -> newInner b.Prefix b.Mask (union cmp na b.Left) b.Right
                    | 1u -> newInner b.Prefix b.Mask b.Left (union cmp na b.Right)
                    | _ -> join a.Hash na b.Prefix nb
            elif nb.IsLeaf then
                let a = na :?> Inner<'K>
                let b = nb :?> SetLeaf<'K>
                match matchPrefixAndGetBit b.Hash a.Prefix a.Mask with
                | 0u -> newInner a.Prefix a.Mask (union cmp a.Left nb) a.Right
                | 1u -> newInner a.Prefix a.Mask a.Left (union cmp a.Right nb)
                | _ -> join a.Prefix na b.Hash nb
            else    
                let a = na :?> Inner<'K>
                let b = nb :?> Inner<'K>

                let cc = compareMasks a.Mask b.Mask
                if cc > 0 then 
                    // a in b
                    match matchPrefixAndGetBit a.Prefix b.Prefix b.Mask with
                    | 0u -> newInner b.Prefix b.Mask (union cmp na b.Left) b.Right
                    | 1u -> newInner b.Prefix b.Mask b.Left (union cmp na b.Right)
                    | _ -> join a.Prefix na b.Prefix nb
                elif cc < 0 then
                    // b in a
                    match matchPrefixAndGetBit b.Prefix a.Prefix a.Mask with
                    | 0u -> newInner a.Prefix a.Mask (union cmp a.Left nb) a.Right
                    | 1u -> newInner a.Prefix a.Mask a.Left (union cmp a.Right nb)
                    | _ -> join a.Prefix na b.Prefix nb
                elif a.Prefix = b.Prefix then
                    newInner a.Prefix a.Mask (union cmp a.Left b.Left) (union cmp a.Right b.Right)
                else
                    join a.Prefix na b.Prefix nb

        let rec intersect
            (cmp : IEqualityComparer<'K>)
            (na : SetNode<'K>) (nb : SetNode<'K>) =
            
            if isNull na || isNull nb then null
            elif System.Object.ReferenceEquals(na, nb) then na
            elif na.IsLeaf then
                let a = na :?> SetLeaf<'K>
                if nb.IsLeaf then
                    let b = nb :?> SetLeaf<'K>
                    if a.Hash = b.Hash then
                        // TODO: avoid allocating SetLinkeds
                        let la = SetLinked(a.Key, a.SetNext)
                        let lb = SetLinked(b.Key, b.SetNext)
                        let res = SetLinked.intersect cmp la lb
                        if isNull res then null
                        else SetLeaf(a.Hash, res.Key, res.SetNext) :> SetNode<_>
                    else
                        null
                else
                    let b = nb :?> Inner<'K>
                    match matchPrefixAndGetBit a.Hash b.Prefix b.Mask with
                    | 0u -> intersect cmp na b.Left
                    | 1u -> intersect cmp na b.Right
                    | _ -> null
            elif nb.IsLeaf then
                let a = na :?> Inner<'K>
                let b = nb :?> SetLeaf<'K>
                match matchPrefixAndGetBit b.Hash a.Prefix a.Mask with
                | 0u -> intersect cmp a.Left nb
                | 1u -> intersect cmp a.Right nb
                | _ -> null
            else    
                let a = na :?> Inner<'K>
                let b = nb :?> Inner<'K>

                let cc = compareMasks a.Mask b.Mask
                if cc > 0 then 
                    // a in b
                    match matchPrefixAndGetBit a.Prefix b.Prefix b.Mask with
                    | 0u -> intersect cmp na b.Left
                    | 1u -> intersect cmp na b.Right
                    | _ -> null
                elif cc < 0 then
                    // b in a
                    match matchPrefixAndGetBit b.Prefix a.Prefix a.Mask with
                    | 0u -> intersect cmp a.Left nb
                    | 1u -> intersect cmp a.Right nb
                    | _ -> null
                elif a.Prefix = b.Prefix then
                    newInner a.Prefix a.Mask (intersect cmp a.Left b.Left) (intersect cmp a.Right b.Right)
                else
                    null

        let rec xor
            (cmp : IEqualityComparer<'K>)
            (na : SetNode<'K>) (nb : SetNode<'K>) =
            
            if isNull na then nb
            elif isNull nb then na
            elif System.Object.ReferenceEquals(na, nb) then null
            elif na.IsLeaf then
                let a = na :?> SetLeaf<'K>
                if nb.IsLeaf then
                    let b = nb :?> SetLeaf<'K>
                    if a.Hash = b.Hash then
                        // TODO: avoid allocating SetLinkeds
                        let la = SetLinked(a.Key, a.SetNext)
                        let lb = SetLinked(b.Key, b.SetNext)
                        let res = SetLinked.xor cmp la lb
                        if isNull res then null
                        else SetLeaf(a.Hash, res.Key, res.SetNext) :> SetNode<_>
                    else
                        join a.Hash na b.Hash nb
                else
                    let b = nb :?> Inner<'K>
                    match matchPrefixAndGetBit a.Hash b.Prefix b.Mask with
                    | 0u -> newInner b.Prefix b.Mask (xor cmp na b.Left) b.Right
                    | 1u -> newInner b.Prefix b.Mask b.Left (xor cmp na b.Right)
                    | _ -> join a.Hash na b.Prefix nb
            elif nb.IsLeaf then
                let a = na :?> Inner<'K>
                let b = nb :?> SetLeaf<'K>
                match matchPrefixAndGetBit b.Hash a.Prefix a.Mask with
                | 0u -> newInner a.Prefix a.Mask (xor cmp a.Left nb) a.Right
                | 1u -> newInner a.Prefix a.Mask a.Left (xor cmp a.Right nb)
                | _ -> join a.Prefix na b.Hash nb
            else    
                let a = na :?> Inner<'K>
                let b = nb :?> Inner<'K>

                let cc = compareMasks a.Mask b.Mask
                if cc > 0 then 
                    // a in b
                    match matchPrefixAndGetBit a.Prefix b.Prefix b.Mask with
                    | 0u -> newInner b.Prefix b.Mask (xor cmp na b.Left) b.Right
                    | 1u -> newInner b.Prefix b.Mask b.Left (xor cmp na b.Right)
                    | _ -> join a.Prefix na b.Prefix nb
                elif cc < 0 then
                    // b in a
                    match matchPrefixAndGetBit b.Prefix a.Prefix a.Mask with
                    | 0u -> newInner a.Prefix a.Mask (xor cmp a.Left nb) a.Right
                    | 1u -> newInner a.Prefix a.Mask a.Left (xor cmp a.Right nb)
                    | _ -> join a.Prefix na b.Prefix nb
                elif a.Prefix = b.Prefix then
                    newInner a.Prefix a.Mask (xor cmp a.Left b.Left) (xor cmp a.Right b.Right)
                else
                    join a.Prefix na b.Prefix nb

        let rec difference
            (cmp : IEqualityComparer<'K>)
            (na : SetNode<'K>) (nb : SetNode<'K>) =
            
            if isNull na then null
            elif isNull nb then na
            elif System.Object.ReferenceEquals(na, nb) then null
            elif na.IsLeaf then
                let a = na :?> SetLeaf<'K>
                if nb.IsLeaf then
                    let b = nb :?> SetLeaf<'K>
                    if a.Hash = b.Hash then
                        // TODO: avoid allocating SetLinkeds
                        let la = SetLinked(a.Key, a.SetNext)
                        let lb = SetLinked(b.Key, b.SetNext)
                        let res = SetLinked.difference cmp la lb
                        if isNull res then null
                        else SetLeaf(a.Hash, res.Key, res.SetNext) :> SetNode<_>
                    else
                        na
                else
                    let b = nb :?> Inner<'K>
                    match matchPrefixAndGetBit a.Hash b.Prefix b.Mask with
                    | 0u -> difference cmp na b.Left
                    | 1u -> difference cmp na b.Right
                    | _ -> na

            elif nb.IsLeaf then
                let a = na :?> Inner<'K>
                let b = nb :?> SetLeaf<'K>
                match matchPrefixAndGetBit b.Hash a.Prefix a.Mask with
                | 0u -> newInner a.Prefix a.Mask (difference cmp a.Left nb) a.Right
                | 1u -> newInner a.Prefix a.Mask a.Left (difference cmp a.Right nb)
                | _ -> na
            else    
                let a = na :?> Inner<'K>
                let b = nb :?> Inner<'K>

                let cc = compareMasks a.Mask b.Mask
                if cc > 0 then 
                    // a in b
                    match matchPrefixAndGetBit a.Prefix b.Prefix b.Mask with
                    | 0u -> difference cmp na b.Left
                    | 1u -> difference cmp na b.Right
                    | _ -> na
                elif cc < 0 then
                    // b in a
                    match matchPrefixAndGetBit b.Prefix a.Prefix a.Mask with
                    | 0u -> newInner a.Prefix a.Mask (difference cmp a.Left nb) a.Right
                    | 1u -> newInner a.Prefix a.Mask a.Left (difference cmp a.Right nb)
                    | _ -> na
                elif a.Prefix = b.Prefix then
                    newInner a.Prefix a.Mask (difference cmp a.Left b.Left) (difference cmp a.Right b.Right)
                else
                    na


        let rec computeDelta
            (cmp : IEqualityComparer<'K>)
            (onlyLeft : 'K -> voption<'OP>) 
            (onlyRight : 'K -> voption<'OP>) 
            (na : SetNode<'K>) (nb : SetNode<'K>) =

            if isNull na then 
                chooseToMapV onlyRight nb

            elif isNull nb then 
                chooseToMapV onlyLeft na

            elif System.Object.ReferenceEquals(na, nb) then
                null

            elif na.IsLeaf then
                let a = na :?> SetLeaf<'K>
                if nb.IsLeaf then
                    let b = nb :?> SetLeaf<'K>
                    if a.Hash = b.Hash then
                        // TODO: avoid allocating SetLinkeds here
                        let la = SetLinked(a.Key, a.SetNext)
                        let lb = SetLinked(b.Key, b.SetNext)

                        let ops = SetLinked.computeDelta cmp onlyLeft onlyRight la lb
                        if isNull ops then null
                        else MapLeaf(a.Hash, ops.Key, ops.Value, ops.MapNext) :> SetNode<_>
                    else
                        let da = chooseToMapV onlyLeft na
                        let db = chooseToMapV onlyRight nb
                        join a.Hash da b.Hash db
                else
                    let b = nb :?> Inner<'K>
                    match matchPrefixAndGetBit a.Hash b.Prefix b.Mask with
                    | 0u ->
                        newInner 
                            b.Prefix b.Mask 
                            (computeDelta cmp onlyLeft onlyRight na b.Left)
                            (chooseToMapV onlyRight b.Right)
                    | 1u ->
                        newInner
                            b.Prefix b.Mask
                            (chooseToMapV onlyRight b.Left)
                            (computeDelta cmp onlyLeft onlyRight na b.Right)
                    | _ ->
                        join b.Prefix (chooseToMapV onlyRight nb) a.Hash (chooseToMapV onlyLeft na)

            elif nb.IsLeaf then
                let a = na :?> Inner<'K>
                let b = nb :?> SetLeaf<'K>

                match matchPrefixAndGetBit b.Hash a.Prefix a.Mask with
                | 0u ->
                    newInner
                        a.Prefix a.Mask
                        (computeDelta cmp onlyLeft onlyRight a.Left nb)
                        (chooseToMapV onlyLeft a.Right)
                | 1u ->
                    newInner
                        a.Prefix a.Mask
                        (chooseToMapV onlyLeft a.Left)
                        (computeDelta cmp onlyLeft onlyRight a.Right nb)
                | _ ->
                    join a.Prefix (chooseToMapV onlyLeft na) b.Hash (chooseToMapV onlyRight nb)

            else
                // both inner
                let a = na :?> Inner<'K>
                let b = nb :?> Inner<'K>

                let cc = compareMasks a.Mask b.Mask
                if cc > 0 then
                    // a in b
                    match matchPrefixAndGetBit a.Prefix b.Prefix b.Mask with
                    | 0u ->
                        newInner 
                            b.Prefix b.Mask 
                            (computeDelta cmp onlyLeft onlyRight na b.Left)
                            (chooseToMapV onlyRight b.Right)
                    | 1u ->
                        newInner
                            b.Prefix b.Mask
                            (chooseToMapV onlyRight b.Left)
                            (computeDelta cmp onlyLeft onlyRight na b.Right)
                    | _ ->
                        join b.Prefix (chooseToMapV onlyRight nb) a.Prefix (chooseToMapV onlyLeft na)

                elif cc < 0 then
                    // b in a
                    match matchPrefixAndGetBit b.Prefix a.Prefix a.Mask with
                    | 0u ->
                        newInner
                            a.Prefix a.Mask
                            (computeDelta cmp onlyLeft onlyRight a.Left nb)
                            (chooseToMapV onlyLeft a.Right)
                    | 1u ->
                        newInner
                            a.Prefix a.Mask
                            (chooseToMapV onlyLeft a.Left)
                            (computeDelta cmp onlyLeft onlyRight a.Right nb)
                    | _ ->
                        join a.Prefix (chooseToMapV onlyLeft na) b.Prefix (chooseToMapV onlyRight nb)

                elif a.Prefix = b.Prefix then
                    newInner 
                        a.Prefix a.Mask
                        (computeDelta cmp onlyLeft onlyRight a.Left b.Left)
                        (computeDelta cmp onlyLeft onlyRight a.Right b.Right)
                else
                    join a.Prefix (chooseToMapV onlyLeft na) b.Prefix (chooseToMapV onlyRight nb)


        let rec applyDeltaNoState
            (apply : OptimizedClosures.FSharpFunc<'K, bool, 'D, struct(bool * voption<'DOut>)>)
            (delta : SetNode<'K>) 
            (state : byref<SetNode<'K>>) =

            if isNull delta then
                state <- null
                null

            elif delta.IsLeaf then
                let delta = delta :?> MapLeaf<'K, 'D>
                let struct(exists, op) = apply.Invoke(delta.Key, false, delta.Value)
                match op with
                | ValueSome op ->
                    let mutable ls = null
                    let rest = SetLinked.applyDeltaNoState apply delta.MapNext &ls
                    if exists then state <- SetLeaf(delta.Hash, delta.Key, ls)
                    elif isNull ls then state <- null
                    else state <- SetLeaf(delta.Hash, ls.Key, ls.SetNext)
                    MapLeaf(delta.Hash, delta.Key, op, rest) :> SetNode<_>

                | ValueNone ->
                    let mutable ls = null
                    let rest = SetLinked.applyDeltaNoState apply delta.MapNext &ls
                    if exists then state <- SetLeaf(delta.Hash, delta.Key, ls)
                    elif isNull ls then state <- null
                    else state <- SetLeaf(delta.Hash, ls.Key, ls.SetNext)
                    if isNull rest then null
                    else MapLeaf(delta.Hash, rest.Key, rest.Value, rest.MapNext) :> SetNode<_>


            else
                let delta = delta :?> Inner<'K>
                let mutable ls = null
                let mutable rs = null
                let l = applyDeltaNoState apply delta.Left &ls
                let r = applyDeltaNoState apply delta.Right &rs
                state <- newInner delta.Prefix delta.Mask ls rs
                newInner delta.Prefix delta.Mask l r


        let rec applyDelta
            (cmp : IEqualityComparer<'K>)
            (apply : OptimizedClosures.FSharpFunc<'K, bool, 'D, struct(bool * voption<'DOut>)>)
            (state : byref<SetNode<'K>>) (delta : SetNode<'K>) =
            if isNull delta then
                null

            elif isNull state then
                applyDeltaNoState apply delta &state

            elif delta.IsLeaf then  
                let d = delta :?> MapLeaf<'K, 'D>
                if state.IsLeaf then
                    let s = state :?> SetLeaf<'K>
                    if s.Hash = d.Hash then
                        // TODO: avoid allocating Linkeds here
                        let mutable lstate = SetLinked(s.Key, s.SetNext)
                        let ldelta = MapLinked(d.Key, d.Value, d.MapNext)
                        let ldelta = SetLinked.applyDelta cmp apply ldelta &lstate

                        if isNull lstate then state <- null
                        else state <- SetLeaf(s.Hash, lstate.Key, lstate.SetNext)

                        if isNull ldelta then null
                        else MapLeaf(d.Hash, ldelta.Key, ldelta.Value, ldelta.MapNext) :> SetNode<_>
                    else
                        let mutable ls = null
                        let ld = applyDeltaNoState apply delta &ls
                        state <- join s.Hash s d.Hash ls
                        ld

                else
                    // delta in state
                    let s = state :?> Inner<'K>
                    match matchPrefixAndGetBit d.Hash s.Prefix s.Mask with
                    | 0u ->
                        let mutable l = s.Left
                        let delta = applyDelta cmp apply &l delta
                        state <- newInner s.Prefix s.Mask l s.Right
                        delta
                    | 1u ->
                        let mutable r = s.Right
                        let delta = applyDelta cmp apply &r delta
                        state <- newInner s.Prefix s.Mask s.Left r
                        delta
                    | _ ->
                        let mutable ls = null
                        let ld = applyDeltaNoState apply delta &ls
                        state <- join s.Prefix s d.Hash ls
                        ld

            elif state.IsLeaf then
                // state in delta
                let s = state :?> SetLeaf<'K>
                let d = delta :?> Inner<'K>

                match matchPrefixAndGetBit s.Hash d.Prefix d.Mask with
                | 0u ->
                    let mutable ls = state
                    let mutable rs = null
                    let ld = applyDelta cmp apply &ls d.Left
                    let rd = applyDelta cmp apply &rs d.Right
                    state <- newInner d.Prefix d.Mask ls rs
                    newInner d.Prefix d.Mask ld rd
                | 1u -> 
                    let mutable ls = null
                    let mutable rs = state
                    let ld = applyDelta cmp apply &ls d.Left
                    let rd = applyDelta cmp apply &rs d.Right
                    state <- newInner d.Prefix d.Mask ls rs
                    newInner d.Prefix d.Mask ld rd
                | _ ->
                    let mutable ls = null
                    let ld = applyDeltaNoState apply delta &ls
                    state <- join s.Hash state d.Prefix ls
                    ld
                        

            else
                let d = delta :?> Inner<'K>
                let s = state :?> Inner<'K>

                let cc = compareMasks d.Mask s.Mask
                if cc > 0 then
                    // delta in state
                    match matchPrefixAndGetBit d.Prefix s.Prefix s.Mask with
                    | 0u ->
                        let mutable l = s.Left
                        let delta = applyDelta cmp apply &l delta
                        state <- newInner s.Prefix s.Mask l s.Right
                        delta
                    | 1u ->
                        let mutable r = s.Right
                        let delta = applyDelta cmp apply &r delta
                        state <- newInner s.Prefix s.Mask s.Left r
                        delta
                    | _ ->
                        let mutable ls = null
                        let ld = applyDeltaNoState apply delta &ls
                        state <- join s.Prefix s d.Prefix ls
                        ld

                elif cc < 0 then
                    // state in delta
                    match matchPrefixAndGetBit s.Prefix d.Prefix d.Mask with
                    | 0u ->
                        let mutable ls = state
                        let mutable rs = null
                        let ld = applyDelta cmp apply &ls d.Left
                        let rd = applyDelta cmp apply &rs d.Right
                        state <- newInner d.Prefix d.Mask ls rs
                        newInner d.Prefix d.Mask ld rd
                    | 1u -> 
                        let mutable ls = null
                        let mutable rs = state
                        let ld = applyDelta cmp apply &ls d.Left
                        let rd = applyDelta cmp apply &rs d.Right
                        state <- newInner d.Prefix d.Mask ls rs
                        newInner d.Prefix d.Mask ld rd
                    | _ ->
                        let mutable ls = null
                        let ld = applyDeltaNoState apply delta &ls
                        state <- join s.Prefix state d.Prefix ls
                        ld

                elif s.Prefix = d.Prefix then
                    let mutable ls = s.Left
                    let mutable rs = s.Right
                    let ld = applyDelta cmp apply &ls d.Left
                    let rd = applyDelta cmp apply &rs d.Right
                    state <- newInner s.Prefix s.Mask ls rs
                    newInner d.Prefix d.Mask ld rd

                else
                    let mutable ls = null
                    let ld = applyDeltaNoState apply delta &ls
                    state <- join s.Prefix state d.Prefix ls
                    ld
 
    module MapNode =    

        let inline join (p0 : uint32) (t0 : SetNode<'K>) (p1 : uint32) (t1 : SetNode<'K>) =
            SetNode.join p0 t0 p1 t1

        let inline newInner (prefix : uint32) (mask : uint32) (l : SetNode<'K>) (r : SetNode<'K>) =
            SetNode.newInner prefix mask l r
                
        let rec alterV (cmp : IEqualityComparer<'K>) (hash : uint32) (key : 'K) (update : voption<'V> -> voption<'V>) (node : SetNode<'K>) =
            if isNull node then
                match update ValueNone with
                | ValueSome value -> MapLeaf(hash, key, value, null) :> SetNode<_>
                | ValueNone -> null

            elif node.IsLeaf then
                let node = node :?> MapLeaf<'K, 'V>
                if node.Hash = hash then
                    if cmp.Equals(node.Key, key) then
                        match update (ValueSome node.Value) with
                        | ValueSome value -> MapLeaf(node.Hash, key, value, node.MapNext) :> SetNode<_>
                        | ValueNone ->
                            let next = node.MapNext
                            if isNull next then null
                            else MapLeaf(node.Hash, next.Key, next.Value, next.MapNext) :> SetNode<_>
                    else
                        MapLeaf(node.Hash, node.Key, node.Value, MapLinked.alterV cmp key update node.MapNext) :> SetNode<_>

                else
                    match update ValueNone with
                    | ValueSome value -> join node.Hash node hash (MapLeaf(hash, key, value, null))
                    | ValueNone -> node :> SetNode<_>
            else
                let node = node :?> Inner<'K>
                match matchPrefixAndGetBit hash node.Prefix node.Mask with
                | 0u ->
                    newInner 
                        node.Prefix node.Mask 
                        (alterV cmp hash key update node.Left) 
                        node.Right
                | 1u ->
                    newInner 
                        node.Prefix node.Mask 
                        node.Left
                        (alterV cmp hash key update node.Right) 
                | _ ->
                    match update ValueNone with
                    | ValueSome value -> join node.Prefix node hash (MapLeaf(hash, key, value, null))
                    | ValueNone -> node :> SetNode<_>
                 
        let rec alter (cmp : IEqualityComparer<'K>) (hash : uint32) (key : 'K) (update : option<'V> -> option<'V>) (node : SetNode<'K>) =
            if isNull node then
                match update None with
                | Some value -> MapLeaf(hash, key, value, null) :> SetNode<_>
                | None -> null

            elif node.IsLeaf then
                let node = node :?> MapLeaf<'K, 'V>
                if node.Hash = hash then
                    if cmp.Equals(node.Key, key) then
                        match update (Some node.Value) with
                        | Some value -> MapLeaf(node.Hash, key, value, node.MapNext) :> SetNode<_>
                        | None ->
                            let next = node.MapNext
                            if isNull next then null
                            else MapLeaf(node.Hash, next.Key, next.Value, next.MapNext) :> SetNode<_>
                    else
                        MapLeaf(node.Hash, node.Key, node.Value, MapLinked.alter cmp key update node.MapNext) :> SetNode<_>

                else
                    match update None with
                    | Some value -> join node.Hash node hash (MapLeaf(hash, key, value, null))
                    | None -> node :> SetNode<_>
            else
                let node = node :?> Inner<'K>
                match matchPrefixAndGetBit hash node.Prefix node.Mask with
                | 0u ->
                    newInner 
                        node.Prefix node.Mask 
                        (alter cmp hash key update node.Left) 
                        node.Right
                | 1u ->
                    newInner 
                        node.Prefix node.Mask 
                        node.Left
                        (alter cmp hash key update node.Right) 
                | _ ->
                    match update None with
                    | Some value -> join node.Prefix node hash (MapLeaf(hash, key, value, null))
                    | None -> node :> SetNode<_>
                 
        let rec add (cmp : IEqualityComparer<'K>) (hash : uint32) (key : 'K) (value : 'V) (node : SetNode<'K>) =
            if isNull node then
                MapLeaf(hash, key, value, null) :> SetNode<_>

            elif node.IsLeaf then
                let node = node :?> MapLeaf<'K, 'V>
                if node.Hash = hash then
                    if cmp.Equals(node.Key, key) then
                        MapLeaf(node.Hash, key, value, node.MapNext) :> SetNode<_>
                    else
                        MapLeaf(node.Hash, node.Key, node.Value, MapLinked.add cmp key value node.MapNext) :> SetNode<_>
                else
                    join node.Hash node hash (MapLeaf(hash, key, value, null))
            else
                let node = node :?> Inner<'K>
                match matchPrefixAndGetBit hash node.Prefix node.Mask with
                | 0u ->
                    newInner 
                        node.Prefix node.Mask 
                        (add cmp hash key value node.Left) 
                        node.Right
                | 1u ->
                    newInner 
                        node.Prefix node.Mask 
                        node.Left
                        (add cmp hash key value node.Right) 
                | _ ->
                    join node.Prefix node hash (MapLeaf(hash, key, value, null))
                 
        let rec tryRemove<'K, 'V> (cmp : IEqualityComparer<'K>) (hash : uint32) (key : 'K) (node : byref<SetNode<'K>>) =
            if isNull node then
                ValueNone

            elif node.IsLeaf then
                let n = node :?> MapLeaf<'K, 'V>
                if n.Hash = hash then
                    if cmp.Equals(n.Key, key) then
                        let next = n.MapNext
                        if isNull next then node <- null
                        else node <- MapLeaf(n.Hash, next.Key, next.Value, next.MapNext)
                        ValueSome n.Value
                    else
                        let mutable next = n.MapNext
                        match MapLinked.tryRemove cmp key &next with
                        | ValueNone -> 
                            ValueNone
                        | res -> 
                            node <- MapLeaf(n.Hash, n.Key, n.Value, next)
                            res
                else
                    ValueNone
            else
                let n = node :?> Inner<'K>
                match matchPrefixAndGetBit hash n.Prefix n.Mask with
                | 0u ->
                    let mutable l = n.Left
                    match tryRemove cmp hash key &l with
                    | ValueNone ->
                        ValueNone
                    | res ->
                        node <- newInner n.Prefix n.Mask l n.Right
                        res
                | 1u ->
                    let mutable r = n.Right
                    match tryRemove cmp hash key &r with
                    | ValueNone ->
                        ValueNone
                    | res ->
                        node <- newInner n.Prefix n.Mask n.Left r
                        res
                | _ ->
                    ValueNone
                   
        let rec tryFindV<'K, 'V>  (cmp : IEqualityComparer<'K>) (hash : uint32) (key : 'K) (node : SetNode<'K>) =
            if isNull node then
                ValueNone

            elif node.IsLeaf then
                let n = node :?> MapLeaf<'K, 'V>
                if n.Hash = hash then
                    if cmp.Equals(n.Key, key) then  ValueSome n.Value
                    else MapLinked.tryFindV cmp key n.MapNext
                       
                else
                    ValueNone
            else
                let n = node :?> Inner<'K>
                match matchPrefixAndGetBit hash n.Prefix n.Mask with
                | 0u -> tryFindV<'K, 'V> cmp hash key n.Left
                | 1u -> tryFindV<'K, 'V> cmp hash key n.Right
                | _ ->
                    ValueNone
  
        let rec tryFind<'K, 'V>  (cmp : IEqualityComparer<'K>) (hash : uint32) (key : 'K) (node : SetNode<'K>) =
            if isNull node then
                None

            elif node.IsLeaf then
                let n = node :?> MapLeaf<'K, 'V>
                if n.Hash = hash then
                    if cmp.Equals(n.Key, key) then Some n.Value
                    else MapLinked.tryFind cmp key n.MapNext
                       
                else
                    None
            else
                let n = node :?> Inner<'K>
                match matchPrefixAndGetBit hash n.Prefix n.Mask with
                | 0u -> tryFind<'K, 'V> cmp hash key n.Left
                | 1u -> tryFind<'K, 'V> cmp hash key n.Right
                | _ ->
                    None
  
        let rec containsKey<'K, 'V>  (cmp : IEqualityComparer<'K>) (hash : uint32) (key : 'K) (node : SetNode<'K>) =
            if isNull node then
                false

            elif node.IsLeaf then
                let n = node :?> MapLeaf<'K, 'V>
                if n.Hash = hash then
                    if cmp.Equals(n.Key, key) then true
                    else MapLinked.containsKey cmp key n.MapNext
                       
                else
                    false
            else
                let n = node :?> Inner<'K>
                match matchPrefixAndGetBit hash n.Prefix n.Mask with
                | 0u -> containsKey<'K, 'V> cmp hash key n.Left
                | 1u -> containsKey<'K, 'V> cmp hash key n.Right
                | _ ->
                    false

        let rec addInPlace (cmp : IEqualityComparer<'K>) (hash : uint32) (key : 'K) (value : 'V) (node : byref<SetNode<'K>>) =
            if isNull node then
                node <- MapLeaf(hash, key, value, null)
                true

            elif node.IsLeaf then
                let leaf = node :?> MapLeaf<'K, 'V>
                if leaf.Hash = hash then
                    if cmp.Equals(leaf.Key, key) then
                        leaf.Key <- key
                        leaf.Value <- value
                        false
                    else
                        MapLinked.addInPlace cmp key value &leaf.SetNext
                else
                    node <- join leaf.Hash leaf hash (MapLeaf(hash, key, value, null))
                    true
            else
                let inner = node :?> Inner<'K>
                match matchPrefixAndGetBit hash inner.Prefix inner.Mask with
                | 0u -> 
                    if addInPlace cmp hash key value &inner.Left then
                        inner.Count <- inner.Count + 1
                        true
                    else
                        false
                | 1u -> 
                    if addInPlace cmp hash key value &inner.Right then
                        inner.Count <- inner.Count + 1
                        true
                    else
                        false
                | _ ->
                    node <- join inner.Prefix inner hash (MapLeaf(hash, key, value, null))
                    true
                
                
        let rec toValueList (acc : list<'V>) (node : SetNode<'K>) =
            if isNull node then acc
            elif node.IsLeaf then
                let leaf = node :?> MapLeaf<'K, 'V>
                if isNull leaf.SetNext then
                    leaf.Value :: acc
                else
                    leaf.Value :: MapLinked.toValueList acc leaf.MapNext
            else
                let node = node :?> Inner<'K>
                toValueList (toValueList acc node.Right) node.Left
                   
        let rec toList (acc : list<'K * 'V>) (node : SetNode<'K>) =
            if isNull node then acc
            elif node.IsLeaf then
                let leaf = node :?> MapLeaf<'K, 'V>
                if isNull leaf.SetNext then
                    (leaf.Key, leaf.Value) :: acc
                else
                    (leaf.Key, leaf.Value) :: MapLinked.toList acc leaf.MapNext
            else
                let node = node :?> Inner<'K>
                toList (toList acc node.Right) node.Left
                   
        let rec toListV (acc : list<struct('K * 'V)>) (node : SetNode<'K>) =
            if isNull node then acc
            elif node.IsLeaf then
                let leaf = node :?> MapLeaf<'K, 'V>
                if isNull leaf.SetNext then
                    struct(leaf.Key, leaf.Value) :: acc
                else
                    struct(leaf.Key, leaf.Value) :: MapLinked.toListV acc leaf.MapNext
            else
                let node = node :?> Inner<'K>
                toListV (toListV acc node.Right) node.Left

        let rec toListMap (mapping : OptimizedClosures.FSharpFunc<'K, 'V, 'T>) (acc : list<'T>) (node : SetNode<'K>) =  
            if isNull node then
                acc
            elif node.IsLeaf then
                let node = node :?> MapLeaf<'K, 'V>
                mapping.Invoke(node.Key, node.Value) :: MapLinked.toListMap mapping acc node.MapNext
            else
                let node = node :?> Inner<'K>
                toListMap mapping (toListMap mapping acc node.Right) node.Left

            
        let rec copyTo (dst : ('K * 'V)[]) (index : int) (node : SetNode<'K>) =
            if isNull node then
                index
            elif node.IsLeaf then
                let node = node :?> MapLeaf<'K, 'V>
                dst.[index] <- (node.Key, node.Value)
                MapLinked.copyTo dst (index + 1) node.MapNext
            else
                let node = node :?> Inner<'K>
                let i0 = copyTo dst index node.Left
                copyTo dst i0 node.Right
                    
        let rec copyValuesTo (dst : 'V[]) (index : int) (node : SetNode<'K>) =
            if isNull node then
                index
            elif node.IsLeaf then
                let node = node :?> MapLeaf<'K, 'V>
                dst.[index] <- node.Value
                MapLinked.copyValuesTo dst (index + 1) node.MapNext
            else
                let node = node :?> Inner<'K>
                let i0 = copyValuesTo dst index node.Left
                copyValuesTo dst i0 node.Right
                
        let rec copyToV (dst : struct('K * 'V)[]) (index : int) (node : SetNode<'K>) =
            if isNull node then
                index
            elif node.IsLeaf then
                let node = node :?> MapLeaf<'K, 'V>
                dst.[index] <- struct(node.Key, node.Value)
                MapLinked.copyToV dst (index + 1) node.MapNext
            else
                let node = node :?> Inner<'K>
                let i0 = copyToV dst index node.Left
                copyToV dst i0 node.Right
                
        let rec exists (predicate : OptimizedClosures.FSharpFunc<'K, 'V, bool>) (node : SetNode<'K>) =
            if isNull node then
                false
            elif node.IsLeaf then
                let node = node :?> MapLeaf<'K, 'V>
                predicate.Invoke(node.Key, node.Value) || 
                MapLinked.exists predicate node.MapNext
            else
                let node = node :?> Inner<'K>
                exists predicate node.Left ||
                exists predicate node.Right
                
        let rec forall (predicate : OptimizedClosures.FSharpFunc<'K, 'V, bool>) (node : SetNode<'K>) =
            if isNull node then
                true
            elif node.IsLeaf then
                let node = node :?> MapLeaf<'K, 'V>
                predicate.Invoke(node.Key, node.Value) &&
                MapLinked.forall predicate node.MapNext
            else
                let node = node :?> Inner<'K>
                forall predicate node.Left &&
                forall predicate node.Right

        let rec fold (folder : OptimizedClosures.FSharpFunc<'S, 'K, 'V, 'S>) (state : 'S) (node : SetNode<'K>) =
            if isNull node then
                state
            elif node.IsLeaf then
                let node = node :?> MapLeaf<'K, 'V>
                let state = folder.Invoke(state, node.Key, node.Value)
                MapLinked.fold folder state node.MapNext
            else
                let node = node :?> Inner<'K>
                let state = fold folder state node.Left
                fold folder state node.Right

        let rec iter (action : OptimizedClosures.FSharpFunc<'K, 'V, unit>) (node : SetNode<'K>) =
            if isNull node then
                ()
            elif node.IsLeaf then
                let node = node :?> MapLeaf<'K, 'V>
                action.Invoke(node.Key, node.Value)
                let mutable c = node.MapNext
                while not (isNull c) do 
                    action.Invoke(c.Key, c.Value)
                    c <- c.MapNext
            else
                let node = node :?> Inner<'K>
                iter action node.Left
                iter action node.Right
                

        let rec map (mapping : OptimizedClosures.FSharpFunc<'K, 'V, 'T>) (node : SetNode<'K>) =
            if isNull node then
                null
            elif node.IsLeaf then
                let node = node :?> MapLeaf<'K, 'V>
                MapLeaf(node.Hash, node.Key, mapping.Invoke(node.Key, node.Value), MapLinked.map mapping node.MapNext) :> SetNode<_>
            else
                let node = node :?> Inner<'K>
                Inner(node.Prefix, node.Mask, map mapping node.Left, map mapping node.Right) :> SetNode<_>

        let rec filter (predicate : OptimizedClosures.FSharpFunc<'K, 'V, bool>) (node : SetNode<'K>) =
            if isNull node then
                null
            elif node.IsLeaf then
                let node = node :?> MapLeaf<'K, 'V>
                if predicate.Invoke(node.Key, node.Value) then
                    MapLeaf(node.Hash, node.Key, node.Value, MapLinked.filter predicate node.MapNext) :> SetNode<_>
                else
                    let l = MapLinked.filter predicate node.MapNext
                    if isNull l then null
                    else MapLeaf(node.Hash, l.Key, l.Value, l.MapNext) :> SetNode<_>
            else
                let node = node :?> Inner<'K>
                newInner
                    node.Prefix node.Mask
                    (filter predicate node.Left)
                    (filter predicate node.Right)
                   
        let rec choose (mapping : OptimizedClosures.FSharpFunc<'K, 'V, option<'T>>) (node : SetNode<'K>) =
            if isNull node then
                null
            elif node.IsLeaf then
                let node = node :?> MapLeaf<'K, 'V>
                match mapping.Invoke(node.Key, node.Value) with
                | Some value ->
                    MapLeaf(node.Hash, node.Key, value, MapLinked.choose mapping node.MapNext) :> SetNode<_>
                | None ->
                    let next = MapLinked.choose mapping node.MapNext
                    if isNull next then null
                    else MapLeaf(node.Hash, next.Key, next.Value, next.MapNext) :> SetNode<_>
            else
                let node = node :?> Inner<'K>
                let l = choose mapping node.Left
                let r = choose mapping node.Right
                newInner node.Prefix node.Mask l r

        let rec chooseV (mapping : OptimizedClosures.FSharpFunc<'K, 'V, voption<'OP>>) (node : SetNode<'K>) =
            if isNull node then
                null
            elif node.IsLeaf then
                let node = node :?> MapLeaf<'K, 'V>
                match mapping.Invoke(node.Key, node.Value) with
                | ValueSome value ->
                    MapLeaf(node.Hash, node.Key, value, MapLinked.chooseV mapping node.MapNext) :> SetNode<_>
                | ValueNone ->
                    let next = MapLinked.chooseV mapping node.MapNext
                    if isNull next then null
                    else MapLeaf(node.Hash, next.Key, next.Value, next.MapNext) :> SetNode<_>
            else
                let node = node :?> Inner<'K>
                let l = chooseV mapping node.Left
                let r = chooseV mapping node.Right
                newInner node.Prefix node.Mask l r

        let rec hash<'K, 'V> (acc : int) (a : SetNode<'K>) =
            if isNull a then
                acc
            elif a.IsLeaf then
                let a = a :?> MapLeaf<'K, 'V>
                let cnt =
                    let mutable c = 1
                    let mutable n = a.SetNext
                    while not (isNull n) do c <- c + 1; n <- n.SetNext
                    c
                combineHash acc (combineHash (int a.Hash) cnt)
            else
                let a = a :?> Inner<'K>
                let lh = hash<'K, 'V> acc a.Left
                let nh = combineHash lh (combineHash (int a.Prefix) (int a.Mask))
                hash<'K, 'V> nh a.Right
                
        let rec equals<'K, 'V>
            (cmp : IEqualityComparer<'K>)
            (na : SetNode<'K>) (nb : SetNode<'K>) =

            if isNull na then isNull nb
            elif isNull nb then false
            elif System.Object.ReferenceEquals(na, nb) then true
            elif na.IsLeaf then
                let a = na :?> MapLeaf<'K, 'V>
                if nb.IsLeaf then
                    let b = nb :?> MapLeaf<'K, 'V>
                    if a.Hash = b.Hash then
                        let la = MapLinked(a.Key, a.Value, a.MapNext)
                        let lb = MapLinked(b.Key, b.Value, b.MapNext)
                        MapLinked.equals cmp la lb
                    else
                        false
                else
                    false
            elif nb.IsLeaf then
                false
            else
                let a = na :?> Inner<'K>
                let b = nb :?> Inner<'K>
                if a.Prefix = b.Prefix && a.Mask = b.Mask then
                    equals<'K, 'V> cmp a.Left b.Left &&
                    equals<'K, 'V> cmp a.Right b.Right
                else
                    false
                
        let rec private choose2VRight
            (mapping : OptimizedClosures.FSharpFunc<'K, voption<'A>, voption<'B>, voption<'C>>)
            (na : SetNode<'K>) = 

            if isNull na then 
                null
            elif na.IsLeaf then
                let a = na :?> MapLeaf<'K, 'B>
                match mapping.Invoke(a.Key, ValueNone, ValueSome a.Value) with
                | ValueSome c ->
                    let next = MapLinked.choose2VRight mapping a.MapNext
                    MapLeaf(a.Hash, a.Key, c, next) :> SetNode<_>
                | ValueNone ->
                    let next = MapLinked.choose2VRight mapping a.MapNext
                    if isNull next then null
                    else MapLeaf(a.Hash, next.Key, next.Value, next.MapNext) :> SetNode<_>
            else
                let a = na :?> Inner<'K>
                newInner
                    a.Prefix a.Mask
                    (choose2VRight mapping a.Left)
                    (choose2VRight mapping a.Right)
                 
        let rec private choose2VLeft
            (mapping : OptimizedClosures.FSharpFunc<'K, voption<'A>, voption<'B>, voption<'C>>)
            (na : SetNode<'K>) = 

            if isNull na then 
                null
            elif na.IsLeaf then
                let a = na :?> MapLeaf<'K, 'A>
                match mapping.Invoke(a.Key, ValueSome a.Value, ValueNone) with
                | ValueSome c ->
                    let next = MapLinked.choose2VLeft mapping a.MapNext
                    MapLeaf(a.Hash, a.Key, c, next) :> SetNode<_>
                | ValueNone ->
                    let next = MapLinked.choose2VLeft mapping a.MapNext
                    if isNull next then null
                    else MapLeaf(a.Hash, next.Key, next.Value, next.MapNext) :> SetNode<_>
            else
                let a = na :?> Inner<'K>
                newInner
                    a.Prefix a.Mask
                    (choose2VLeft mapping a.Left)
                    (choose2VLeft mapping a.Right)
                 
        let rec choose2V
            (cmp : IEqualityComparer<'K>)
            (mapping : OptimizedClosures.FSharpFunc<'K, voption<'A>, voption<'B>, voption<'C>>)
            (na : SetNode<'K>) (nb : SetNode<'K>) = 
            if isNull na then choose2VRight mapping nb
            elif isNull nb then choose2VLeft mapping na
            elif na.IsLeaf then
                let a = na :?> MapLeaf<'K, 'A>
                if nb.IsLeaf then
                    let b = nb :?> MapLeaf<'K, 'B>
                    if a.Hash  = b.Hash then
                        let la = MapLinked(a.Key, a.Value, a.MapNext)
                        let lb = MapLinked(b.Key, b.Value, b.MapNext)
                        let res = MapLinked.choose2V cmp mapping la lb
                        if isNull res then null
                        else MapLeaf(a.Hash, res.Key, res.Value, res.MapNext) :> SetNode<_>
                    else
                        let va = choose2VLeft mapping na
                        let vb = choose2VRight mapping nb
                        join a.Hash va b.Hash vb
                else
                    let b = nb :?> Inner<'K>
                    match matchPrefixAndGetBit a.Hash b.Prefix b.Mask with
                    | 0u ->
                        newInner
                            b.Prefix b.Mask
                            (choose2V cmp mapping na b.Left)
                            (choose2VRight mapping b.Right)
                    | 1u ->
                        newInner
                            b.Prefix b.Mask
                            (choose2VRight mapping b.Left)
                            (choose2V cmp mapping na b.Right)
                    | _ ->
                        let va = choose2VLeft mapping na
                        let vb = choose2VRight mapping nb
                        join a.Hash va b.Prefix vb

            elif nb.IsLeaf then
                let a = na :?> Inner<'K>
                let b = nb :?> MapLeaf<'K, 'B>
                match matchPrefixAndGetBit b.Hash a.Prefix a.Mask with
                | 0u ->
                    newInner
                        a.Prefix a.Mask
                        (choose2V cmp mapping a.Left nb)
                        (choose2VLeft mapping a.Right)
                | 1u ->
                    newInner
                        a.Prefix a.Mask
                        (choose2VLeft mapping a.Left)
                        (choose2V cmp mapping a.Right nb)
                | _ -> 
                    let va = choose2VLeft mapping na
                    let vb = choose2VRight mapping nb
                    join a.Prefix va b.Hash vb
            else
                let a = na :?> Inner<'K>
                let b = nb :?> Inner<'K>

                let cc = compareMasks a.Mask b.Mask
                if cc > 0 then 
                    // a in b
                    match matchPrefixAndGetBit a.Prefix b.Prefix b.Mask with
                    | 0u ->
                        newInner
                            b.Prefix b.Mask
                            (choose2V cmp mapping na b.Left)
                            (choose2VRight mapping b.Right)
                    | 1u ->
                        newInner
                            b.Prefix b.Mask
                            (choose2VRight mapping b.Left)
                            (choose2V cmp mapping na b.Right)
                    | _ ->
                        let va = choose2VLeft mapping na
                        let vb = choose2VRight mapping nb
                        join a.Prefix va b.Prefix vb

                elif cc < 0 then
                    match matchPrefixAndGetBit b.Prefix a.Prefix a.Mask with
                    | 0u ->
                        newInner
                            a.Prefix a.Mask
                            (choose2V cmp mapping a.Left nb)
                            (choose2VLeft mapping a.Right)
                    | 1u ->
                        newInner
                            a.Prefix a.Mask
                            (choose2VLeft mapping a.Left)
                            (choose2V cmp mapping a.Right nb)
                    | _ -> 
                        let va = choose2VLeft mapping na
                        let vb = choose2VRight mapping nb
                        join a.Prefix va b.Prefix vb

                elif a.Prefix = b.Prefix then
                    newInner
                        a.Prefix a.Mask
                        (choose2V cmp mapping a.Left b.Left)
                        (choose2V cmp mapping a.Right b.Right)

                else
                    let va = choose2VLeft mapping na
                    let vb = choose2VRight mapping nb
                    join a.Prefix va b.Prefix vb
             

             
        let rec unionWithSelfV<'K, 'V>
            (mapping : OptimizedClosures.FSharpFunc<'K, 'V, 'V, voption<'V>>)
            (node : SetNode<'K>) =
            if isNull node then
                null
            elif node.IsLeaf then
                let node = node :?> MapLeaf<'K, 'V>
                match mapping.Invoke(node.Key, node.Value, node.Value) with
                | ValueSome value ->
                    MapLeaf(node.Hash, node.Key, value, MapLinked.unionWithSelfV mapping node.MapNext) :> SetNode<_>
                | ValueNone ->
                    let next = MapLinked.unionWithSelfV mapping node.MapNext
                    if isNull next then null
                    else MapLeaf(node.Hash, next.Key, next.Value, next.MapNext) :> SetNode<_>
            else
                let node = node :?> Inner<'K>
                let l = unionWithSelfV mapping node.Left
                let r = unionWithSelfV mapping node.Right
                newInner node.Prefix node.Mask l r

        let rec unionWithV<'K, 'V>
            (cmp : IEqualityComparer<'K>)
            (resolve : OptimizedClosures.FSharpFunc<'K, 'V, 'V, voption<'V>>)
            (na : SetNode<'K>) (nb : SetNode<'K>) =
            
            if isNull na then nb
            elif isNull nb then na
            elif System.Object.ReferenceEquals(na, nb) then unionWithSelfV resolve na
            elif na.IsLeaf then
                let a = na :?> MapLeaf<'K, 'V>
                if nb.IsLeaf then
                    let b = nb :?> MapLeaf<'K, 'V>
                    if a.Hash = b.Hash then
                        // TODO: avoid allocating SetLinkeds
                        let la = MapLinked(a.Key, a.Value, a.MapNext)
                        let lb = MapLinked(b.Key, b.Value, b.MapNext)
                        let res = MapLinked.unionWithV cmp resolve la lb
                        if isNull res then null
                        else MapLeaf(a.Hash, res.Key, res.Value, res.MapNext) :> SetNode<_>
                    else
                        join a.Hash na b.Hash nb
                else
                    let b = nb :?> Inner<'K>
                    match matchPrefixAndGetBit a.Hash b.Prefix b.Mask with
                    | 0u -> newInner b.Prefix b.Mask (unionWithV cmp resolve na b.Left) b.Right
                    | 1u -> newInner b.Prefix b.Mask b.Left (unionWithV cmp resolve na b.Right)
                    | _ -> join a.Hash na b.Prefix nb
            elif nb.IsLeaf then
                let a = na :?> Inner<'K>
                let b = nb :?> MapLeaf<'K, 'V>
                match matchPrefixAndGetBit b.Hash a.Prefix a.Mask with
                | 0u -> newInner a.Prefix a.Mask (unionWithV cmp resolve a.Left nb) a.Right
                | 1u -> newInner a.Prefix a.Mask a.Left (unionWithV cmp resolve a.Right nb)
                | _ -> join a.Prefix na b.Hash nb
            else    
                let a = na :?> Inner<'K>
                let b = nb :?> Inner<'K>

                let cc = compareMasks a.Mask b.Mask
                if cc > 0 then 
                    // a in b
                    match matchPrefixAndGetBit a.Prefix b.Prefix b.Mask with
                    | 0u -> newInner b.Prefix b.Mask (unionWithV cmp resolve na b.Left) b.Right
                    | 1u -> newInner b.Prefix b.Mask b.Left (unionWithV cmp resolve na b.Right)
                    | _ -> join a.Prefix na b.Prefix nb
                elif cc < 0 then
                    // b in a
                    match matchPrefixAndGetBit b.Prefix a.Prefix a.Mask with
                    | 0u -> newInner a.Prefix a.Mask (unionWithV cmp resolve a.Left nb) a.Right
                    | 1u -> newInner a.Prefix a.Mask a.Left (unionWithV cmp resolve a.Right nb)
                    | _ -> join a.Prefix na b.Prefix nb
                elif a.Prefix = b.Prefix then
                    newInner a.Prefix a.Mask (unionWithV cmp resolve a.Left b.Left) (unionWithV cmp resolve a.Right b.Right)
                else
                    join a.Prefix na b.Prefix nb
                    
        let rec unionWithSelf (mapping : OptimizedClosures.FSharpFunc<'K, 'V, 'V, 'V>) (node : SetNode<'K>) =
            if isNull node then
                null
            elif node.IsLeaf then
                let node = node :?> MapLeaf<'K, 'V>
                MapLeaf(node.Hash, node.Key, mapping.Invoke(node.Key, node.Value, node.Value), MapLinked.unionWithSelf mapping node.MapNext) :> SetNode<_>
            else
                let node = node :?> Inner<'K>
                Inner(node.Prefix, node.Mask, unionWithSelf mapping node.Left, unionWithSelf mapping node.Right) :> SetNode<_>

        let rec unionWith<'K, 'V>
            (cmp : IEqualityComparer<'K>)
            (resolve : OptimizedClosures.FSharpFunc<'K, 'V, 'V, 'V>)
            (na : SetNode<'K>) (nb : SetNode<'K>) =
            
            if isNull na then nb
            elif isNull nb then na
            elif System.Object.ReferenceEquals(na, nb) then unionWithSelf resolve na
            elif na.IsLeaf then
                let a = na :?> MapLeaf<'K, 'V>
                if nb.IsLeaf then
                    let b = nb :?> MapLeaf<'K, 'V>
                    if a.Hash = b.Hash then
                        // TODO: avoid allocating SetLinkeds
                        let la = MapLinked(a.Key, a.Value, a.MapNext)
                        let lb = MapLinked(b.Key, b.Value, b.MapNext)
                        let res = MapLinked.unionWith cmp resolve la lb
                        if isNull res then null
                        else MapLeaf(a.Hash, res.Key, res.Value, res.MapNext) :> SetNode<_>
                    else
                        join a.Hash na b.Hash nb
                else
                    let b = nb :?> Inner<'K>
                    match matchPrefixAndGetBit a.Hash b.Prefix b.Mask with
                    | 0u -> newInner b.Prefix b.Mask (unionWith cmp resolve na b.Left) b.Right
                    | 1u -> newInner b.Prefix b.Mask b.Left (unionWith cmp resolve na b.Right)
                    | _ -> join a.Hash na b.Prefix nb
            elif nb.IsLeaf then
                let a = na :?> Inner<'K>
                let b = nb :?> MapLeaf<'K, 'V>
                match matchPrefixAndGetBit b.Hash a.Prefix a.Mask with
                | 0u -> newInner a.Prefix a.Mask (unionWith cmp resolve a.Left nb) a.Right
                | 1u -> newInner a.Prefix a.Mask a.Left (unionWith cmp resolve a.Right nb)
                | _ -> join a.Prefix na b.Hash nb
            else    
                let a = na :?> Inner<'K>
                let b = nb :?> Inner<'K>

                let cc = compareMasks a.Mask b.Mask
                if cc > 0 then 
                    // a in b
                    match matchPrefixAndGetBit a.Prefix b.Prefix b.Mask with
                    | 0u -> newInner b.Prefix b.Mask (unionWith cmp resolve na b.Left) b.Right
                    | 1u -> newInner b.Prefix b.Mask b.Left (unionWith cmp resolve na b.Right)
                    | _ -> join a.Prefix na b.Prefix nb
                elif cc < 0 then
                    // b in a
                    match matchPrefixAndGetBit b.Prefix a.Prefix a.Mask with
                    | 0u -> newInner a.Prefix a.Mask (unionWith cmp resolve a.Left nb) a.Right
                    | 1u -> newInner a.Prefix a.Mask a.Left (unionWith cmp resolve a.Right nb)
                    | _ -> join a.Prefix na b.Prefix nb
                elif a.Prefix = b.Prefix then
                    newInner a.Prefix a.Mask (unionWith cmp resolve a.Left b.Left) (unionWith cmp resolve a.Right b.Right)
                else
                    join a.Prefix na b.Prefix nb
  

        let rec union<'K, 'V>
            (cmp : IEqualityComparer<'K>)
            (na : SetNode<'K>) (nb : SetNode<'K>) =
            
            if isNull na then nb
            elif isNull nb then na
            elif System.Object.ReferenceEquals(na, nb) then na
            elif na.IsLeaf then
                let a = na :?> MapLeaf<'K, 'V>
                if nb.IsLeaf then
                    let b = nb :?> MapLeaf<'K, 'V>
                    if a.Hash = b.Hash then
                        // TODO: avoid allocating SetLinkeds
                        let la = MapLinked(a.Key, a.Value, a.MapNext)
                        let lb = MapLinked(b.Key, b.Value, b.MapNext)
                        let res = MapLinked.union cmp la lb
                        if isNull res then null
                        else MapLeaf(a.Hash, res.Key, res.Value, res.MapNext) :> SetNode<_>
                    else
                        join a.Hash na b.Hash nb
                else
                    let b = nb :?> Inner<'K>
                    match matchPrefixAndGetBit a.Hash b.Prefix b.Mask with
                    | 0u -> newInner b.Prefix b.Mask (union<'K, 'V> cmp na b.Left) b.Right
                    | 1u -> newInner b.Prefix b.Mask b.Left (union<'K, 'V> cmp na b.Right)
                    | _ -> join a.Hash na b.Prefix nb
            elif nb.IsLeaf then
                let a = na :?> Inner<'K>
                let b = nb :?> MapLeaf<'K, 'V>
                match matchPrefixAndGetBit b.Hash a.Prefix a.Mask with
                | 0u -> newInner a.Prefix a.Mask (union<'K, 'V> cmp a.Left nb) a.Right
                | 1u -> newInner a.Prefix a.Mask a.Left (union<'K, 'V> cmp a.Right nb)
                | _ -> join a.Prefix na b.Hash nb
            else    
                let a = na :?> Inner<'K>
                let b = nb :?> Inner<'K>

                let cc = compareMasks a.Mask b.Mask
                if cc > 0 then 
                    // a in b
                    match matchPrefixAndGetBit a.Prefix b.Prefix b.Mask with
                    | 0u -> newInner b.Prefix b.Mask (union<'K, 'V> cmp na b.Left) b.Right
                    | 1u -> newInner b.Prefix b.Mask b.Left (union<'K, 'V> cmp na b.Right)
                    | _ -> join a.Prefix na b.Prefix nb
                elif cc < 0 then
                    // b in a
                    match matchPrefixAndGetBit b.Prefix a.Prefix a.Mask with
                    | 0u -> newInner a.Prefix a.Mask (union<'K, 'V> cmp a.Left nb) a.Right
                    | 1u -> newInner a.Prefix a.Mask a.Left (union<'K, 'V> cmp a.Right nb)
                    | _ -> join a.Prefix na b.Prefix nb
                elif a.Prefix = b.Prefix then
                    newInner a.Prefix a.Mask (union<'K, 'V> cmp a.Left b.Left) (union<'K, 'V> cmp a.Right b.Right)
                else
                    join a.Prefix na b.Prefix nb

        let rec computeDelta
            (cmp : IEqualityComparer<'K>)
            (onlyLeft : OptimizedClosures.FSharpFunc<'K, 'V, voption<'OP>>)
            (onlyRight : OptimizedClosures.FSharpFunc<'K, 'V, voption<'OP>>)
            (both : OptimizedClosures.FSharpFunc<'K, 'V, 'V, voption<'OP>>) 
            (na : SetNode<'K>) (nb : SetNode<'K>) =

            if isNull na then 
                chooseV onlyRight nb

            elif isNull nb then 
                chooseV onlyLeft na

            elif System.Object.ReferenceEquals(na, nb) then 
                null

            elif na.IsLeaf then
                let a = na :?> MapLeaf<'K, 'V>
                if nb.IsLeaf then
                    let b = nb :?> MapLeaf<'K, 'V>
                    if a.Hash = b.Hash then
                        let la = MapLinked(a.Key, a.Value, a.MapNext)
                        let lb = MapLinked(b.Key, b.Value, b.MapNext)
                        let delta = MapLinked.computeDelta cmp onlyLeft onlyRight both la lb
                        if isNull delta then null
                        else MapLeaf(a.Hash, delta.Key, delta.Value, delta.MapNext) :> SetNode<_>
                    else
                        let da = chooseV onlyLeft na
                        let db = chooseV onlyRight nb
                        join a.Hash da b.Hash db
                else
                    let b = nb :?> Inner<'K>
                    // a in b
                    match matchPrefixAndGetBit a.Hash b.Prefix b.Mask with
                    | 0u ->
                        newInner
                            b.Prefix b.Mask
                            (computeDelta cmp onlyLeft onlyRight both na b.Left)
                            (chooseV onlyRight b.Right)
                    | 1u ->
                        newInner
                            b.Prefix b.Mask
                            (chooseV onlyRight b.Left)
                            (computeDelta cmp onlyLeft onlyRight both na b.Right)
                    | _ ->
                        join a.Hash (chooseV onlyLeft na) b.Prefix (chooseV onlyRight nb)


            elif nb.IsLeaf then
                let a = na :?> Inner<'K>
                let b = nb :?> MapLeaf<'K, 'V>
                // b in a
                match matchPrefixAndGetBit b.Hash a.Prefix a.Mask with
                | 0u ->
                    newInner
                        a.Prefix a.Mask
                        (computeDelta cmp onlyLeft onlyRight both a.Left nb)
                        (chooseV onlyLeft a.Right)
                | 1u ->
                    newInner
                        a.Prefix a.Mask
                        (chooseV onlyLeft a.Left)
                        (computeDelta cmp onlyLeft onlyRight both a.Right nb)
                | _ ->
                    join a.Prefix (chooseV onlyLeft na) b.Hash (chooseV onlyRight nb)
            else
                let a = na :?> Inner<'K>
                let b = nb :?> Inner<'K>
                    
                let cc = compareMasks a.Mask b.Mask 
                if cc > 0 then
                    // a in b
                    match matchPrefixAndGetBit a.Prefix b.Prefix b.Mask with
                    | 0u ->
                        newInner
                            b.Prefix b.Mask
                            (computeDelta cmp onlyLeft onlyRight both na b.Left)
                            (chooseV onlyRight b.Right)
                    | 1u ->
                        newInner
                            b.Prefix b.Mask
                            (chooseV onlyRight b.Left)
                            (computeDelta cmp onlyLeft onlyRight both na b.Right)
                    | _ ->
                        join a.Prefix (chooseV onlyLeft na) b.Prefix (chooseV onlyRight nb)
                elif cc < 0 then
                    // b in a
                    match matchPrefixAndGetBit b.Prefix a.Prefix a.Mask with
                    | 0u ->
                        newInner
                            a.Prefix a.Mask
                            (computeDelta cmp onlyLeft onlyRight both a.Left nb)
                            (chooseV onlyLeft a.Right)
                    | 1u ->
                        newInner
                            a.Prefix a.Mask
                            (chooseV onlyLeft a.Left)
                            (computeDelta cmp onlyLeft onlyRight both a.Right nb)
                    | _ ->
                        join a.Prefix (chooseV onlyLeft na) b.Prefix (chooseV onlyRight nb)
                elif a.Prefix = b.Prefix then
                    newInner
                        a.Prefix a.Mask
                        (computeDelta cmp onlyLeft onlyRight both a.Left b.Left)
                        (computeDelta cmp onlyLeft onlyRight both a.Right b.Right)
                else
                    join a.Prefix (chooseV onlyLeft na) b.Prefix (chooseV onlyRight nb)
                        

        let rec applyDeltaNoState
            (apply : OptimizedClosures.FSharpFunc<'K, voption<'V>, 'D, struct(voption<'V> * voption<'DOut>)>)
            (delta : SetNode<'K>) 
            (state : byref<SetNode<'K>>) =

            if isNull delta then
                state <- null
                null

            elif delta.IsLeaf then
                let delta = delta :?> MapLeaf<'K, 'D>
                let struct(exists, op) = apply.Invoke(delta.Key, ValueNone, delta.Value)
                match op with
                | ValueSome op ->
                    let mutable ls = null
                    let rest = MapLinked.applyDeltaNoState apply delta.MapNext &ls

                    match exists with
                    | ValueSome newValue -> state <- MapLeaf(delta.Hash, delta.Key, newValue, ls)
                    | ValueNone ->
                        if isNull ls then state <- null
                        else state <- MapLeaf(delta.Hash, ls.Key, ls.Value, ls.MapNext)

                    MapLeaf(delta.Hash, delta.Key, op, rest) :> SetNode<_>

                | ValueNone ->
                    let mutable ls = null
                    let rest = MapLinked.applyDeltaNoState apply delta.MapNext &ls
                        
                    match exists with
                    | ValueSome newValue -> state <- MapLeaf(delta.Hash, delta.Key, newValue, ls)
                    | ValueNone ->
                        if isNull ls then state <- null
                        else state <- MapLeaf(delta.Hash, ls.Key, ls.Value, ls.MapNext)

                    if isNull rest then null
                    else MapLeaf(delta.Hash, rest.Key, rest.Value, rest.MapNext) :> SetNode<_>


            else
                let delta = delta :?> Inner<'K>
                let mutable ls = null
                let mutable rs = null
                let l = applyDeltaNoState apply delta.Left &ls
                let r = applyDeltaNoState apply delta.Right &rs
                state <- newInner delta.Prefix delta.Mask ls rs
                newInner delta.Prefix delta.Mask l r


        let rec applyDelta
            (cmp : IEqualityComparer<'K>)
            (apply : OptimizedClosures.FSharpFunc<'K, voption<'V>, 'D, struct(voption<'V> * voption<'DOut>)>)
            (state : byref<SetNode<'K>>) (delta : SetNode<'K>) =
            if isNull delta then
                null

            elif isNull state then
                applyDeltaNoState apply delta &state

            elif delta.IsLeaf then  
                let d = delta :?> MapLeaf<'K, 'D>
                if state.IsLeaf then
                    let s = state :?> MapLeaf<'K, 'V>
                    if s.Hash = d.Hash then
                        // TODO: avoid allocating Linkeds here
                        let mutable lstate = MapLinked(s.Key, s.Value, s.MapNext)
                        let ldelta = MapLinked(d.Key, d.Value, d.MapNext)
                        let ldelta = MapLinked.applyDelta cmp apply ldelta &lstate

                        if isNull lstate then state <- null
                        else state <- MapLeaf(s.Hash, lstate.Key, lstate.Value, lstate.MapNext)

                        if isNull ldelta then null
                        else MapLeaf(d.Hash, ldelta.Key, ldelta.Value, ldelta.MapNext) :> SetNode<_>
                    else
                        let mutable ls = null
                        let ld = applyDeltaNoState apply delta &ls
                        state <- join s.Hash s d.Hash ls
                        ld

                else
                    // delta in state
                    let s = state :?> Inner<'K>
                    match matchPrefixAndGetBit d.Hash s.Prefix s.Mask with
                    | 0u ->
                        let mutable l = s.Left
                        let delta = applyDelta cmp apply &l delta
                        state <- newInner s.Prefix s.Mask l s.Right
                        delta
                    | 1u ->
                        let mutable r = s.Right
                        let delta = applyDelta cmp apply &r delta
                        state <- newInner s.Prefix s.Mask s.Left r
                        delta
                    | _ ->
                        let mutable ls = null
                        let ld = applyDeltaNoState apply delta &ls
                        state <- join s.Prefix s d.Hash ls
                        ld

            elif state.IsLeaf then
                // state in delta
                let s = state :?> MapLeaf<'K, 'V>
                let d = delta :?> Inner<'K>

                match matchPrefixAndGetBit s.Hash d.Prefix d.Mask with
                | 0u ->
                    let mutable ls = state
                    let mutable rs = null
                    let ld = applyDelta cmp apply &ls d.Left
                    let rd = applyDelta cmp apply &rs d.Right
                    state <- newInner d.Prefix d.Mask ls rs
                    newInner d.Prefix d.Mask ld rd
                | 1u -> 
                    let mutable ls = null
                    let mutable rs = state
                    let ld = applyDelta cmp apply &ls d.Left
                    let rd = applyDelta cmp apply &rs d.Right
                    state <- newInner d.Prefix d.Mask ls rs
                    newInner d.Prefix d.Mask ld rd
                | _ ->
                    let mutable ls = null
                    let ld = applyDeltaNoState apply delta &ls
                    state <- join s.Hash state d.Prefix ls
                    ld
                        

            else
                let d = delta :?> Inner<'K>
                let s = state :?> Inner<'K>

                let cc = compareMasks d.Mask s.Mask
                if cc > 0 then
                    // delta in state
                    match matchPrefixAndGetBit d.Prefix s.Prefix s.Mask with
                    | 0u ->
                        let mutable l = s.Left
                        let delta = applyDelta cmp apply &l delta
                        state <- newInner s.Prefix s.Mask l s.Right
                        delta
                    | 1u ->
                        let mutable r = s.Right
                        let delta = applyDelta cmp apply &r delta
                        state <- newInner s.Prefix s.Mask s.Left r
                        delta
                    | _ ->
                        let mutable ls = null
                        let ld = applyDeltaNoState apply delta &ls
                        state <- join s.Prefix s d.Prefix ls
                        ld

                elif cc < 0 then
                    // state in delta
                    match matchPrefixAndGetBit s.Prefix d.Prefix d.Mask with
                    | 0u ->
                        let mutable ls = state
                        let mutable rs = null
                        let ld = applyDelta cmp apply &ls d.Left
                        let rd = applyDelta cmp apply &rs d.Right
                        state <- newInner d.Prefix d.Mask ls rs
                        newInner d.Prefix d.Mask ld rd
                    | 1u -> 
                        let mutable ls = null
                        let mutable rs = state
                        let ld = applyDelta cmp apply &ls d.Left
                        let rd = applyDelta cmp apply &rs d.Right
                        state <- newInner d.Prefix d.Mask ls rs
                        newInner d.Prefix d.Mask ld rd
                    | _ ->
                        let mutable ls = null
                        let ld = applyDeltaNoState apply delta &ls
                        state <- join s.Prefix state d.Prefix ls
                        ld

                elif s.Prefix = d.Prefix then
                    let mutable ls = s.Left
                    let mutable rs = s.Right
                    let ld = applyDelta cmp apply &ls d.Left
                    let rd = applyDelta cmp apply &rs d.Right
                    state <- newInner s.Prefix s.Mask ls rs
                    newInner d.Prefix d.Mask ld rd

                else
                    let mutable ls = null
                    let ld = applyDeltaNoState apply delta &ls
                    state <- join s.Prefix state d.Prefix ls
                    ld
       
open HashImplementation

[<Struct>]
type HashSetEnumerator<'K> =
    val mutable internal Root : SetNode<'K>
    val mutable internal Head : SetNode<'K>
    val mutable internal Tail : list<SetNode<'K>>
    val mutable internal Next : SetLinked<'K>
    val mutable internal Value : 'K

    member x.MoveNext() =
        if not (isNull x.Next) then
            x.Value <- x.Next.Key
            x.Next <- x.Next.SetNext
            true
        elif isNull x.Head then
            false

        elif x.Head.IsLeaf then
            let node = x.Head :?> SetLeaf<'K>
            x.Value <- node.Key
            x.Next <- node.SetNext

            if List.isEmpty x.Tail then
                x.Head <- null
            else
                x.Head <- List.head x.Tail
                x.Tail <- List.tail x.Tail
            true
        else
            let node = x.Head :?> Inner<'K>
            x.Head <- node.Left
            x.Tail <- node.Right :: x.Tail

            x.MoveNext()

    member x.Current = x.Value

    member x.Reset() =
        x.Head <- x.Root
        x.Tail <- []
        x.Next <- null
        x.Value <- Unchecked.defaultof<_>

    member x.Dispose() =
        x.Root <- null
        x.Head <- null
        x.Tail <- []
        x.Next <- null
        x.Value <- Unchecked.defaultof<_>

    interface System.Collections.IEnumerator with
        member x.MoveNext() = x.MoveNext()
        member x.Reset() = x.Reset()
        member x.Current = x.Current :> obj

    interface System.Collections.Generic.IEnumerator<'K> with
        member x.Current = x.Current
        member x.Dispose() = x.Dispose()


    internal new(root : SetNode<'K>) =
        {
            Root = root
            Head = root
            Tail = []
            Next = null
            Value = Unchecked.defaultof<_>
        }

[<Struct>]
type HashMapEnumerator<'K, 'V, 'T> =
    val mutable internal Root : SetNode<'K>
    val mutable internal Head : SetNode<'K>
    val mutable internal Tail : list<SetNode<'K>>
    val mutable internal Next : MapLinked<'K, 'V>
    val mutable internal Mapping : OptimizedClosures.FSharpFunc<'K, 'V, 'T>
    val mutable internal Value : 'T


    member x.MoveNext() =
        if not (isNull x.Next) then
            x.Value <- x.Mapping.Invoke(x.Next.Key, x.Next.Value)
            x.Next <- x.Next.MapNext
            true
        elif isNull x.Head then
            false

        elif x.Head.IsLeaf then
            let node = x.Head :?> MapLeaf<'K, 'V>
            x.Value <- x.Mapping.Invoke(node.Key, node.Value)
            x.Next <- node.MapNext

            if List.isEmpty x.Tail then
                x.Head <- null
            else
                x.Head <- List.head x.Tail
                x.Tail <- List.tail x.Tail
            true
        else
            let node = x.Head :?> Inner<'K>
            x.Head <- node.Left
            x.Tail <- node.Right :: x.Tail

            x.MoveNext()

    member x.Current = x.Value

    member x.Reset() =
        x.Head <- x.Root
        x.Tail <- []
        x.Next <- null
        x.Value <- Unchecked.defaultof<_>

    member x.Dispose() =
        x.Root <- null
        x.Head <- null
        x.Tail <- []
        x.Next <- null
        x.Value <- Unchecked.defaultof<_>

    interface System.Collections.IEnumerator with
        member x.MoveNext() = x.MoveNext()
        member x.Reset() = x.Reset()
        member x.Current = x.Current :> obj

    interface System.Collections.Generic.IEnumerator<'T> with
        member x.Current = x.Current
        member x.Dispose() = x.Dispose()


    internal new(root : SetNode<'K>, mapping : OptimizedClosures.FSharpFunc<'K, 'V, 'T>) =
        {
            Root = root
            Mapping = mapping
            Head = root
            Tail = []
            Next = null
            Value = Unchecked.defaultof<_>
        }

type internal HashMapEnumerable<'K, 'V, 'T>(root : SetNode<'K>, mapping : OptimizedClosures.FSharpFunc<'K, 'V, 'T>) =
    member x.GetEnumerator() = new HashMapEnumerator<_,_,_>(root, mapping)
    interface System.Collections.IEnumerable with member x.GetEnumerator() = x.GetEnumerator() :> _
    interface System.Collections.Generic.IEnumerable<'T> with member x.GetEnumerator() = x.GetEnumerator() :> _
        

[<Struct; DebuggerDisplay("Count = {Count}"); DebuggerTypeProxy(typedefof<HashSetProxy<_>>); CustomEquality; NoComparison; StructuredFormatDisplay("{AsString}"); CompiledName("FSharpHashSet`1");>]
type HashSet<'K> internal(comparer : IEqualityComparer<'K>, root : SetNode<'K>) =
        
    static let addOp = fun (v : 'K) -> ValueSome 1
    static let remOp = fun (v : 'K) -> ValueSome -1
    static let applyOp = 
        OptimizedClosures.FSharpFunc<'K, bool, int, struct(bool * voption<int>)>.Adapt(
            fun (v : 'K) (o : bool) (d : int) ->
                if o then
                    if d < 0 then struct(false, ValueSome -1)
                    else struct(true, ValueNone)
                else
                    if d > 0 then struct(true, ValueSome 1)
                    else struct(false, ValueNone)
        )

    new(elements : seq<'K>) =
        let cmp = DefaultEqualityComparer<'K>.Instance
        let mutable root = null
        let e = elements.GetEnumerator()
        while e.MoveNext() do
            let item = e.Current
            let hash = uint32 (cmp.GetHashCode item) &&& 0x7FFFFFFFu
            SetNode.addInPlace cmp hash item &root |> ignore
        e.Dispose()
        HashSet(cmp, root)

    // ====================================================================================
    // Properties: Count/IsEmpty/etc.
    // ====================================================================================
    member internal x.Root = root
    member internal x.Comparer = comparer
    member x.Count = size root
    member x.IsEmpty = isNull root

    member private x.AsString = x.ToString()

    override x.GetHashCode() =
        SetNode.hash 0 root

    override x.Equals(o : obj) =
        match o with
        | :? HashSet<'K> as o -> SetNode.equals comparer root o.Root
        | _ -> false

    override x.ToString() =
        if x.Count > 8 then
            x |> Seq.take 8 |> Seq.map (sprintf "%A") |> String.concat "; " |> sprintf "HashSet [%s; ...]"
        else
            x |> Seq.map (sprintf "%A") |> String.concat "; " |> sprintf "HashSet [%s]"

    // ====================================================================================
    // Queries: contains/etc.
    // ====================================================================================
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Contains(key : 'K) =
        if isNull root then
            false
        else
            let hash = uint32 (comparer.GetHashCode key) &&& 0x7FFFFFFFu
            SetNode.contains comparer hash key root
        
    // ====================================================================================
    // Modifications: add/remove/etc.
    // ====================================================================================
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Add(key) = 
        let hash = uint32 (comparer.GetHashCode key) &&& 0x7FFFFFFFu
        HashSet<'K>(comparer, SetNode.add comparer hash key root)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Alter(key : 'K, update : bool -> bool) = 
        let hash = uint32 (comparer.GetHashCode key) &&& 0x7FFFFFFFu
        HashSet<'K>(comparer, SetNode.alter comparer hash key update root)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Remove(key) = 
        let hash = uint32 (comparer.GetHashCode key) &&& 0x7FFFFFFFu
        let mutable root = root
        if SetNode.tryRemove comparer hash key &root then
            HashSet<'K>(comparer, root)
        else
            x
            
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.TryRemove(key) = 
        let hash = uint32 (comparer.GetHashCode key) &&& 0x7FFFFFFFu
        let mutable root = root
        if SetNode.tryRemove comparer hash key &root then
            HashSet<'K>(comparer, root) |> Some
        else
            None
             
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.TryRemoveV(key) = 
        let hash = uint32 (comparer.GetHashCode key) &&& 0x7FFFFFFFu
        let mutable root = root
        if SetNode.tryRemove comparer hash key &root then
            HashSet<'K>(comparer, root) |> ValueSome
        else
            ValueNone

    // ====================================================================================
    // Unary Operations: map/choose/filter/etc.
    // ====================================================================================
    
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Iter(action : 'K -> unit) =
        SetNode.iter action root
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Fold(folder : 'S -> 'K -> 'S, state : 'S) =
        if isNull root then
            state
        else
            let folder = OptimizedClosures.FSharpFunc<_,_,_>.Adapt folder
            SetNode.fold folder state root
            
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Exists(predicate : 'K -> bool) =
        SetNode.exists predicate root
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Forall(predicate : 'K -> bool) =
        SetNode.forall predicate root
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Map(mapping : 'K -> 'T) =
        let cmp = DefaultEqualityComparer<'T>.Instance
        let mutable root = null
        for e in x do
            let n = mapping e
            let hash = uint32 (cmp.GetHashCode n) &&& 0x7FFFFFFFu
            SetNode.addInPlace cmp hash n &root |> ignore

        HashSet<'T>(cmp, root)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.MapToMap(mapping : 'K -> 'V) =
        let root = SetNode.mapToMap mapping root
        HashMap<'K, 'V>(comparer, root)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ChooseV(mapping : 'K -> voption<'T>) =
        let cmp = DefaultEqualityComparer<'T>.Instance
        let mutable root = null
        for e in x do
            match mapping e with
            | ValueSome n ->
                let hash = uint32 (cmp.GetHashCode n) &&& 0x7FFFFFFFu
                SetNode.addInPlace cmp hash n &root |> ignore
            | ValueNone ->
                ()
        HashSet<'T>(cmp, root)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Choose(mapping : 'K -> option<'T>) =
        let cmp = DefaultEqualityComparer<'T>.Instance
        let mutable root = null
        let mutable cnt = 0
        for e in x do
            match mapping e with
            | Some n ->
                let hash = uint32 (cmp.GetHashCode n) &&& 0x7FFFFFFFu
                SetNode.addInPlace cmp hash n &root |> ignore
            | None ->
                ()
        HashSet<'T>(cmp, root)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Filter(predicate : 'K -> bool) =
        HashSet(comparer, SetNode.filter predicate root)

    // ====================================================================================
    // Binary Operations: overlaps/union/computeDelta/etc.
    // ====================================================================================
    
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ApplyDelta(a : HashSet<'K>, b : HashMap<'K, 'D>, apply : 'K -> bool -> 'D -> struct(bool * voption<'DOut>)) =
        let mutable state = a.Root
        let delta = SetNode.applyDelta a.Comparer (OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt apply) &state b.Root
        let state = HashSet<'K>(a.Comparer, state)
        let delta = HashMap<'K, 'DOut>(a.Comparer, delta)
        state, delta
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Overlaps(other : HashSet<'K>) =
        SetNode.overlaps comparer root other.Root
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.SetEquals(other : HashSet<'K>) =
        x.Count = other.Count &&
        SetNode.equals comparer root other.Root
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.IsSubsetOf(other : HashSet<'K>) =
        x.Count <= other.Count &&
        SetNode.subset comparer root other.Root
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.IsSupersetOf(other : HashSet<'K>) =
        x.Count >= other.Count &&
        SetNode.subset comparer other.Root root
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.IsProperSubsetOf(other : HashSet<'K>) =
        x.Count < other.Count &&
        SetNode.subset comparer root other.Root
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.IsProperSupersetOf(other : HashSet<'K>) =
        other.Count < x.Count &&
        SetNode.subset comparer other.Root root
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Overlaps(other : seq<'K>) =
        match other with
        | :? HashSet<'K> as o -> x.Overlaps o
        | :? ISet<'K> as o -> x.Exists o.Contains 
        | :? array<'K> as o -> o |> Array.exists x.Contains
        | :? list<'K> as o -> o |> List.exists x.Contains
        | o -> o |> Seq.exists x.Contains
            
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.SetEquals(other : seq<'K>) =
        match other with
        | :? HashSet<'K> as o -> x.SetEquals o
        | :? array<'K> as o -> x.SetEquals (HashSet.OfArray o)
        | :? list<'K> as o -> x.SetEquals (HashSet.OfList o)
        | o -> x.SetEquals (HashSet.OfSeq o)
            
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.IsSubsetOf (other : seq<'K>) =
        match other with
        | :? HashSet<'K> as o -> x.IsSubsetOf o
        | :? ISet<'K> as o -> x.Forall o.Contains
        | :? array<'K> as o -> x.IsSubsetOf (HashSet.OfArray o)
        | :? list<'K> as o -> x.IsSubsetOf (HashSet.OfList o)
        | o -> x.IsSubsetOf (HashSet.OfSeq o)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.IsProperSubsetOf (other : seq<'K>) =
        match other with
        | :? HashSet<'K> as o -> x.IsProperSubsetOf o
        | :? array<'K> as o -> x.IsProperSubsetOf (HashSet.OfArray o)
        | :? list<'K> as o -> x.IsProperSubsetOf (HashSet.OfList o)
        | o -> x.IsProperSubsetOf (HashSet.OfSeq o)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.IsSupersetOf (other : seq<'K>) =
        match other with
        | :? HashSet<'K> as o -> x.IsSupersetOf o
        | :? array<'K> as o -> o |> Array.forall x.Contains
        | :? list<'K> as o -> o |> List.forall x.Contains
        | o -> o |> Seq.forall x.Contains
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.IsProperSupersetOf (other : seq<'K>) =
        match other with
        | :? HashSet<'K> as o -> x.IsProperSupersetOf o
        | :? array<'K> as o -> x.IsProperSupersetOf (HashSet.OfArray o)
        | :? list<'K> as o -> x.IsProperSupersetOf (HashSet.OfList o)
        | o -> x.IsProperSupersetOf (HashSet.OfSeq o)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.UnionWith(other : HashSet<'K>) =
        HashSet<'K>(comparer, SetNode.union comparer root other.Root)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.SymmetricExceptWith(other : HashSet<'K>) =
        HashSet<'K>(comparer, SetNode.xor comparer root other.Root)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ExceptWith(other : HashSet<'K>) =
        HashSet<'K>(comparer, SetNode.difference comparer root other.Root)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.IntersectWith(other : HashSet<'K>) =
        HashSet<'K>(comparer, SetNode.intersect comparer root other.Root)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ComputeDeltaAsHashMap(other : HashSet<'K>) =
        let delta = SetNode.computeDelta comparer remOp addOp root other.Root
        HashMap<'K, int>(comparer, delta)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ApplyDeltaAsHashMap(delta : HashMap<'K, int>) =
        let mutable state = root
        let delta = SetNode.applyDelta comparer applyOp &state delta.Root
        HashSet<'K>(comparer, state), HashMap<'K, int>(comparer, delta)

    // ====================================================================================
    // Creators: Empty/Singleton/OfList/OfSeq/etc.
    // ====================================================================================
    static member Empty = HashSet<'K>(DefaultEqualityComparer<'K>.Instance, null)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Single(key : 'K) =
        let cmp = DefaultEqualityComparer<'K>.Instance
        let hash = uint32 (cmp.GetHashCode key) &&& 0x7FFFFFFFu
        HashSet(cmp, SetLeaf(hash, key, null))

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfSeq(elements : seq<'K>) =
        let cmp = DefaultEqualityComparer<'K>.Instance
        let mutable root = null
        for e in elements do 
            let hash = uint32 (cmp.GetHashCode e) &&& 0x7FFFFFFFu
            SetNode.addInPlace cmp hash e &root |> ignore
        HashSet(cmp, root)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfList(elements : list<'K>) =
        let cmp = DefaultEqualityComparer<'K>.Instance
        let mutable root = null
        for e in elements do 
            let hash = uint32 (cmp.GetHashCode e) &&& 0x7FFFFFFFu
            SetNode.addInPlace cmp hash e &root |> ignore
        HashSet(cmp, root)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfArray(elements : 'K[]) =
        let cmp = DefaultEqualityComparer<'K>.Instance
        let mutable root = null
        for e in elements do 
            let hash = uint32 (cmp.GetHashCode e) &&& 0x7FFFFFFFu
            SetNode.addInPlace cmp hash e &root |> ignore
        HashSet(cmp, root)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfArrayRange(elements: array<'K>, offset: int, length: int) =  
        let cmp = DefaultEqualityComparer<'K>.Instance
        let mutable i = offset
        let mutable root = null
        let ee = offset + length
        while i < ee do
            let e = elements.[i]
            let hash = uint32 (cmp.GetHashCode e) &&& 0x7FFFFFFFu
            SetNode.addInPlace cmp hash e &root |> ignore
            i <- i + 1
        HashSet(cmp, root)
        
    // ====================================================================================
    // Accessors: CopyTo/ToList/etc.
    // ====================================================================================
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ToList() = SetNode.toList [] root
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.CopyTo(array : 'K[], startIndex : int) =
        SetNode.copyTo array startIndex root |> ignore
            
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ToArray() =
        let arr = Array.zeroCreate (size root)
        x.CopyTo(arr, 0)
        arr
            
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.GetEnumerator() = new HashSetEnumerator<'K>(root)

    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = x.GetEnumerator() :> _
            
    interface System.Collections.Generic.IEnumerable<'K> with
        member x.GetEnumerator() = x.GetEnumerator() :> _

    interface System.Collections.Generic.IReadOnlyCollection<'K> with
        member x.Count = x.Count
        
    interface System.Collections.Generic.ICollection<'K> with
        member x.Count = x.Count
        member x.IsReadOnly = true
        member x.Add _ = failwith "readonly"
        member x.Remove _ = failwith "readonly"
        member x.Clear() = failwith "readonly"
        member x.Contains(item : 'K) = x.Contains item
        member x.CopyTo(arr : 'K[], index : int) = x.CopyTo(arr, index)

    interface System.Collections.Generic.ISet<'K> with
        member x.Add(_) = failwith "readonly"
        member x.ExceptWith(_) = failwith "readonly"
        member x.UnionWith(_) = failwith "readonly"
        member x.IntersectWith(_) = failwith "readonly"
        member x.SymmetricExceptWith(_) = failwith "readonly"
        member x.Overlaps(other : seq<'K>) = x.Overlaps other
        member x.SetEquals(other : seq<'K>) = x.SetEquals other
        member x.IsSubsetOf (other : seq<'K>) = x.IsSubsetOf other
        member x.IsProperSubsetOf (other : seq<'K>) = x.IsProperSubsetOf other
        member x.IsSupersetOf (other : seq<'K>) = x.IsSupersetOf other
        member x.IsProperSupersetOf (other : seq<'K>) = x.IsProperSupersetOf other


and internal HashSetProxy<'K>(set : HashSet<'K>) =
    let items = set |> Seq.truncate 10000 |> Seq.toArray

    [<DebuggerBrowsable(DebuggerBrowsableState.RootHidden)>]
    member x.Items = items

and [<Struct; DebuggerDisplay("Count = {Count}"); DebuggerTypeProxy(typedefof<HashMapProxy<_, _>>); CustomEquality; NoComparison; StructuredFormatDisplay("{AsString}"); CompiledName("FSharpHashMap`2")>] 
    HashMap<'K, [<EqualityConditionalOn>] 'V> internal(comparer : IEqualityComparer<'K>, root : SetNode<'K>) =
    static let tupleGetter = OptimizedClosures.FSharpFunc<'K, 'V, _>.Adapt(fun k v -> (k,v))
    static let valueTupleGetter = OptimizedClosures.FSharpFunc<'K, 'V, _>.Adapt(fun k v -> struct(k,v))
    static let keyGetter = OptimizedClosures.FSharpFunc<'K, 'V, _>.Adapt(fun k _ -> k)
    static let valueGetter = OptimizedClosures.FSharpFunc<'K, 'V, _>.Adapt(fun _ v -> v)
    static let kvpGetter = OptimizedClosures.FSharpFunc<'K, 'V, _>.Adapt(fun k v -> KeyValuePair(k, v))



    // ====================================================================================
    // Properties: Count/IsEmpty/etc.
    // ====================================================================================
    member internal x.Root : SetNode<'K> = root
    member internal x.Comparer = comparer
    member x.Count = size root
    member x.IsEmpty = isNull root
        
    override x.GetHashCode() =
        MapNode.hash<'K, 'V> 0 root

    override x.Equals(o : obj) =
        match o with
        | :? HashMap<'K, 'V> as o -> MapNode.equals<'K, 'V> comparer root o.Root
        | _ -> false
        
    member x.Equals(o : HashMap<'K, 'V>) =
        MapNode.equals<'K, 'V> comparer root o.Root
        
    override x.ToString() =
        if x.Count > 8 then
            x |> Seq.take 8 |> Seq.map (sprintf "%A") |> String.concat "; " |> sprintf "HashMap [%s; ...]"
        else
            x |> Seq.map (sprintf "%A") |> String.concat "; " |> sprintf "HashMap [%s]"

    // ====================================================================================
    // Modifications: add/remove/etc.
    // ====================================================================================
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Add(key : 'K, value : 'V) = 
        let hash = uint32 (comparer.GetHashCode key) &&& 0x7FFFFFFFu
        HashMap<'K, 'V>(comparer, MapNode.add comparer hash key value root)
            
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Alter(key : 'K, update : option<'V> -> option<'V>) = 
        let hash = uint32 (comparer.GetHashCode key) &&& 0x7FFFFFFFu
        HashMap<'K, 'V>(comparer, MapNode.alter comparer hash key update root)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.AlterV(key : 'K, update : voption<'V> -> voption<'V>) = 
        let hash = uint32 (comparer.GetHashCode key) &&& 0x7FFFFFFFu
        HashMap<'K, 'V>(comparer, MapNode.alterV comparer hash key update root)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Remove(key : 'K) = 
        let hash = uint32 (comparer.GetHashCode key) &&& 0x7FFFFFFFu
        let mutable root = root
        match MapNode.tryRemove<'K, 'V> comparer hash key &root with
        | ValueNone -> x
        | ValueSome _ -> HashMap<'K, 'V>(comparer, root)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.TryRemove(key : 'K) = 
        let hash = uint32 (comparer.GetHashCode key) &&& 0x7FFFFFFFu
        let mutable root = root
        match MapNode.tryRemove<'K, 'V> comparer hash key &root with
        | ValueNone -> None
        | ValueSome v -> Some (v, HashMap<'K, 'V>(comparer, root))
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.TryRemoveV(key : 'K) = 
        let hash = uint32 (comparer.GetHashCode key) &&& 0x7FFFFFFFu
        let mutable root = root
        match MapNode.tryRemove<'K, 'V> comparer hash key &root with
        | ValueNone -> ValueNone
        | ValueSome v -> ValueSome struct (v, HashMap<'K, 'V>(comparer, root))


    // ====================================================================================
    // Unary Operations: map/choose/filter/etc.
    // ====================================================================================
    
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ContainsKey(key : 'K) = 
        let hash = uint32 (comparer.GetHashCode key) &&& 0x7FFFFFFFu
        MapNode.containsKey<'K, 'V> comparer hash key root

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.TryFindV(key : 'K) = 
        let hash = uint32 (comparer.GetHashCode key) &&& 0x7FFFFFFFu
        MapNode.tryFindV<'K, 'V> comparer hash key root
            
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.TryFind(key : 'K) = 
        let hash = uint32 (comparer.GetHashCode key) &&& 0x7FFFFFFFu
        MapNode.tryFind<'K, 'V> comparer hash key root

    member x.Item
        with get(key : 'K) : 'V = 
            let hash = uint32 (comparer.GetHashCode key) &&& 0x7FFFFFFFu
            match MapNode.tryFindV<'K, 'V> comparer hash key root with
            | ValueSome v -> v
            | ValueNone -> raise <| KeyNotFoundException()

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Fold(folder : 'S -> 'K -> 'V -> 'S, state : 'S) =
        let folder = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt folder
        MapNode.fold folder state root
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Exists(predicate : 'K -> 'V -> bool) =
        let predicate = OptimizedClosures.FSharpFunc<_,_,_>.Adapt predicate
        MapNode.exists predicate root

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Forall(predicate : 'K -> 'V -> bool) =
        let predicate = OptimizedClosures.FSharpFunc<_,_,_>.Adapt predicate
        MapNode.forall predicate root
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Iter(action : 'K -> 'V -> unit) =
        let action = OptimizedClosures.FSharpFunc<_,_,_>.Adapt action
        MapNode.iter action root

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Map(mapping : 'K -> 'V -> 'T) =
        let mapping = OptimizedClosures.FSharpFunc<_,_,_>.Adapt mapping
        HashMap<'K, 'T>(comparer, MapNode.map mapping root)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Choose(mapping : 'K -> 'V -> option<'T>) =
        let mapping = OptimizedClosures.FSharpFunc<_,_,_>.Adapt mapping
        HashMap<'K, 'T>(comparer, MapNode.choose mapping root)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ChooseV(mapping : 'K -> 'V -> voption<'T>) =
        let mapping = OptimizedClosures.FSharpFunc<_,_,_>.Adapt mapping
        HashMap<'K, 'T>(comparer, MapNode.chooseV mapping root)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Filter(predicate : 'K -> 'V -> bool) =
        let predicate = OptimizedClosures.FSharpFunc<_,_,_>.Adapt predicate
        HashMap<'K, 'V>(comparer, MapNode.filter predicate root)

    // ====================================================================================
    // Binary Operations: computeDelta/etc.
    // ====================================================================================
    
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member ApplyDelta(a : HashMap<'K, 'V>, b : HashMap<'K, 'T>, apply : 'K -> voption<'V> -> 'T -> struct(voption<'V> * voption<'U>)) =
        let mutable state = a.Root
        let delta = MapNode.applyDelta a.Comparer (OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt apply) &state b.Root
        let state = HashMap<'K, 'V>(a.Comparer, state)
        let delta = HashMap<'K, 'U>(a.Comparer, delta)
        state, delta
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Choose2V(other : HashMap<'K, 'T>, mapping : 'K -> voption<'V> -> voption<'T> -> voption<'U>) =
        let mapping = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt mapping
        HashMap<'K, 'U>(comparer, MapNode.choose2V comparer mapping root other.Root)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Choose2(other : HashMap<'K, 'T>, mapping : 'K -> option<'V> -> option<'T> -> option<'U>) =
        let mapping = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt mapping
        let inline o v = match v with | ValueSome v -> Some v | ValueNone -> None
        let inline vo v = match v with | Some v -> ValueSome v | None -> ValueNone
        let realMapping = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(fun k l r -> mapping.Invoke(k, o l, o r) |> vo)
        HashMap<'K, 'U>(comparer, MapNode.choose2V comparer realMapping root other.Root)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Map2V(other : HashMap<'K, 'T>, mapping : 'K -> voption<'V> -> voption<'T> -> 'U) =
        let mapping = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt mapping
        let realMapping = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(fun k l r -> mapping.Invoke(k, l, r) |> ValueSome)
        HashMap<'K, 'U>(comparer, MapNode.choose2V comparer realMapping root other.Root)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Map2(other : HashMap<'K, 'T>, mapping : 'K -> option<'V> -> option<'T> -> 'U) =
        let mapping = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt mapping
        let inline o v = match v with | ValueSome v -> Some v | ValueNone -> None
        let inline vo v = match v with | Some v -> ValueSome v | None -> ValueNone
        let realMapping = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(fun k l r -> mapping.Invoke(k, o l, o r) |> ValueSome)
        HashMap<'K, 'U>(comparer, MapNode.choose2V comparer realMapping root other.Root)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.UnionWith(other : HashMap<'K, 'V>) =
        HashMap<'K, 'V>(comparer, MapNode.union<'K, 'V> comparer root other.Root)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.UnionWith(other : HashMap<'K, 'V>, resolve : 'K -> 'V -> 'V -> 'V) =
        let resolve = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt resolve
        HashMap<'K, 'V>(comparer, MapNode.unionWith<'K, 'V> comparer resolve root other.Root)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.UnionWithV(other : HashMap<'K, 'V>, resolve : 'K -> 'V -> 'V -> voption<'V>) =
        let resolve = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt resolve
        HashMap<'K, 'V>(comparer, MapNode.unionWithV<'K, 'V> comparer resolve root other.Root)

    // ====================================================================================
    // Creators: Empty/Singleton/OfList/OfSeq/etc.
    // ====================================================================================
    static member Empty = HashMap<'K, 'V>(DefaultEqualityComparer<'K>.Instance, null)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Single(key : 'K, value : 'V) : HashMap<'K, 'V> =
        let cmp = DefaultEqualityComparer<'K>.Instance
        let hash = uint32 (cmp.GetHashCode key) &&& 0x7FFFFFFFu
        HashMap(cmp, MapLeaf(hash, key, value, null))

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfSeq(elements : seq<'K * 'V>) =
        let cmp = DefaultEqualityComparer<'K>.Instance
        let mutable root = null
        for (k, v) in elements do 
            let hash = uint32 (cmp.GetHashCode k) &&& 0x7FFFFFFFu
            MapNode.addInPlace cmp hash k v &root |> ignore
        HashMap<'K, 'V>(cmp, root)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfList(elements : list<'K * 'V>) =
        let cmp = DefaultEqualityComparer<'K>.Instance
        let mutable root = null
        for (k, v) in elements do 
            let hash = uint32 (cmp.GetHashCode k) &&& 0x7FFFFFFFu
            MapNode.addInPlace cmp hash k v &root |> ignore
        HashMap<'K, 'V>(cmp, root)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfArray(elements : ('K * 'V)[]) =
        let cmp = DefaultEqualityComparer<'K>.Instance
        let mutable root = null
        for (k, v) in elements do 
            let hash = uint32 (cmp.GetHashCode k) &&& 0x7FFFFFFFu
            MapNode.addInPlace cmp hash k v &root |> ignore
        HashMap<'K, 'V>(cmp, root)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfArrayRange(elements: array<'K * 'V>, offset: int, length: int) =  
        let cmp = DefaultEqualityComparer<'K>.Instance
        let mutable i = offset
        let mutable root = null
        let ee = offset + length
        while i < ee do
            let (k, v) = elements.[i]
            let hash = uint32 (cmp.GetHashCode k) &&& 0x7FFFFFFFu
            MapNode.addInPlace cmp hash k v &root |> ignore
            i <- i + 1
        HashMap<'K, 'V>(cmp, root)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfArrayRange(elements: array<struct('K * 'V)>, offset: int, length: int) =  
        let cmp = DefaultEqualityComparer<'K>.Instance
        let mutable i = offset
        let mutable root = null
        let ee = offset + length
        while i < ee do
            let struct(k, v) = elements.[i]
            let hash = uint32 (cmp.GetHashCode k) &&& 0x7FFFFFFFu
            MapNode.addInPlace cmp hash k v &root |> ignore
            i <- i + 1
        HashMap<'K, 'V>(cmp, root)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfSeq(elements : seq<struct('K * 'V)>) =
        let cmp = DefaultEqualityComparer<'K>.Instance
        let mutable root = null
        for struct(k, v) in elements do 
            let hash = uint32 (cmp.GetHashCode k) &&& 0x7FFFFFFFu
            MapNode.addInPlace cmp hash k v &root |> ignore
        HashMap<'K, 'V>(cmp, root)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfList(elements : list<struct('K * 'V)>) =
        let cmp = DefaultEqualityComparer<'K>.Instance
        let mutable root = null
        for struct(k, v) in elements do 
            let hash = uint32 (cmp.GetHashCode k) &&& 0x7FFFFFFFu
            MapNode.addInPlace cmp hash k v &root |> ignore
        HashMap<'K, 'V>(cmp, root)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member OfArray(elements : struct('K * 'V)[]) =
        let cmp = DefaultEqualityComparer<'K>.Instance
        let mutable root = null
        for struct(k, v) in elements do 
            let hash = uint32 (cmp.GetHashCode k) &&& 0x7FFFFFFFu
            MapNode.addInPlace cmp hash k v &root |> ignore
        HashMap<'K, 'V>(cmp, root)
        

    // ====================================================================================
    // Accessors: GetKeys/CopyTo/ToList/etc.
    // ====================================================================================
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.GetKeys() = HashSet<'K>(comparer, root)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ToList() : list<'K * 'V> = MapNode.toList [] root

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ToListV() : list<struct('K * 'V)> = MapNode.toListV [] root

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ToKeyList() = SetNode.toList [] root

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ToValueList() : list<'V> = MapNode.toValueList [] root

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.CopyTo(array : ('K * 'V)[], startIndex : int) =
        MapNode.copyTo array startIndex root |> ignore
            
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.CopyTo(array : struct('K * 'V)[], startIndex : int) =
        MapNode.copyToV array startIndex root |> ignore
            
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.CopyKeysTo(array : 'K[], startIndex : int) =
        SetNode.copyTo array startIndex root |> ignore
            
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.CopyValuesTo(array : 'V[], startIndex : int) =
        MapNode.copyValuesTo array startIndex root |> ignore

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ToArray() =
        let array = Array.zeroCreate<'K * 'V> (size root)
        MapNode.copyTo array 0 root |> ignore
        array
            
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ToArrayV() =
        let array = Array.zeroCreate<struct('K * 'V)> (size root)
        MapNode.copyToV array 0 root |> ignore
        array

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ToValueArray() =
        let array = Array.zeroCreate<'V> (size root)
        MapNode.copyValuesTo array 0 root |> ignore
        array
            
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ToKeyArray() =
        let array = Array.zeroCreate<'K> (size root)
        SetNode.copyTo array 0 root |> ignore
        array
            
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ToSeq() =
        HashMapEnumerable(root, tupleGetter) :> seq<_>
            
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ToSeqV() =
        HashMapEnumerable(root, valueTupleGetter) :> seq<_>
            
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ToValueSeq() =
        HashMapEnumerable(root, valueGetter) :> seq<_>
            
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ToKeySeq() =
        HashMapEnumerable(root, keyGetter) :> seq<_>
            
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ToKeyValueSeq() =
        HashMapEnumerable(root, kvpGetter) :> seq<_>
         
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.GetEnumerator() = new HashMapEnumerator<'K, 'V, _>(root, tupleGetter)
    
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.GetStructEnumerator() = new HashMapEnumerator<'K, 'V, _>(root, valueTupleGetter)

    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = x.GetEnumerator() :> _
            
    interface System.Collections.Generic.IEnumerable<'K * 'V> with
        member x.GetEnumerator() = x.GetEnumerator() :> _
        
    interface System.Collections.Generic.IReadOnlyCollection<'K * 'V> with
        member x.Count = x.Count
        
    interface System.Collections.Generic.ICollection<'K * 'V> with
        member x.IsReadOnly = true
        member x.Add(_) = failwith "readonly"
        member x.Remove(_) = failwith "readonly"
        member x.Clear() = failwith "readonly"
        member x.Contains((k, v)) =
            match x.TryFindV k with
            | ValueSome vv -> DefaultEquality.equals vv v
            | ValueNone -> false
        member x.Count = x.Count
        member x.CopyTo(dst : ('K * 'V)[], index : int) = x.CopyTo(dst, index)

and [<Sealed>] internal HashMapProxy<'K, 'V>(map : HashMap<'K, 'V>) =
    [<DebuggerBrowsable(DebuggerBrowsableState.RootHidden)>]
    member x.Items = 
        map.ToSeqV() 
        |> Seq.map (fun struct(k,v) -> KeyValuePairDebugFriendly(KeyValuePair(k, v))) 
        |> Seq.truncate 10000 
        |> Seq.toArray

and 
    [<Sealed; DebuggerDisplay("{keyValue.Value}", Name = "[{keyValue.Key}]", Type = "")>]
    internal KeyValuePairDebugFriendly<'K, 'V>(keyValue : KeyValuePair<'K, 'V>) =

        [<DebuggerBrowsable(DebuggerBrowsableState.RootHidden)>]
        member x.KeyValue = keyValue

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module HashSet =
    
    /// The empty set.
    [<GeneralizableValue>]
    let empty<'T> = HashSet<'T>.Empty

    /// The number of elements in the set `O(1)`
    let inline count (set : HashSet<'T>) = set.Count
    
    /// Is the set empty? `O(1)`
    let inline isEmpty (set : HashSet<'T>) = set.IsEmpty
    
    /// Creates a set with a single entry.
    /// `O(1)`
    let inline single (element : 'T) = HashSet.Single(element)
    
    /// Creates a set with all entries from the seq.
    /// `O(N)`
    let inline ofSeq (elements : seq<'T>) = HashSet.OfSeq elements
    
    /// Creates a set with all entries from the Set.
    /// `O(N)`
    let inline ofSet (set: Set<'T>) = HashSet.OfSeq set
    
    /// Creates a set with all entries from the list.
    /// `O(N)`
    let inline ofList (elements : list<'T>) = HashSet.OfList elements
    
    /// Creates a set with all entries from the array.
    /// `O(N)`
    let inline ofArray (elements : 'T[]) = HashSet.OfArray elements
    
    /// Adds the given value. `O(1)`
    let inline add (value : 'T) (set : HashSet<'T>) = set.Add(value)
    
    /// Removes the given value. `O(1)`
    let inline remove (value : 'T) (set : HashSet<'T>) = set.Remove value
    
    /// Tries to remove the given value from the set and returns the rest of the set.
    /// `O(1)`       
    let inline tryRemove (value : 'T) (set : HashSet<'T>) = set.TryRemove value
    let inline tryRemoveV (value : 'T) (set : HashSet<'T>) = set.TryRemoveV value
    let inline alter (value : 'T) (update : bool -> bool) (m : HashSet<'T>) = m.Alter(value, update)
        
        
    /// Applies the iter function to all entries of the set.
    /// `O(N)`
    let inline iter (action : 'T -> unit) (set : HashSet<'T>) = set.Iter action

    /// Folds over all entries of the set.
    /// Note that the order for elements is undefined.
    /// `O(N)`
    let inline fold (folder : 'S -> 'T -> 'S) (state : 'S) (set : HashSet<'T>) = set.Fold(folder, state)
    
    /// Tests whether an entry making the predicate true exists.
    /// `O(N)`
    let inline exists (predicate : 'T -> bool) (set : HashSet<'T>) = set.Exists predicate

    /// Tests whether all entries fulfil the given predicate.
    /// `O(N)`
    let inline forall (predicate : 'T -> bool) (set : HashSet<'T>) = set.Forall predicate
    
    /// Creates a new set by applying the given function to all entries.
    /// `O(N)`
    let inline map (mapping : 'T -> 'U) (set : HashSet<'T>) = set.Map mapping

    /// Creates a new set by applying the given function to all entries.
    /// `O(N)`
    let inline choose (mapping : 'T -> option<'U>) (set : HashSet<'T>) = set.Choose mapping
    
    /// Creates a new set by applying the given function to all entries.
    /// `O(N)`
    let inline chooseV (mapping : 'T -> voption<'U>) (set : HashSet<'T>) = set.ChooseV mapping
    
    /// Creates a new set that contains all entries for which predicate was true.
    /// `O(N)`
    let inline filter (predicate : 'T -> bool) (set : HashSet<'T>) = set.Filter predicate

    /// Creates a new set by applying the given function to all entries and unions the results.
    let collect (mapping : 'T -> HashSet<'U>) (set : HashSet<'T>) =
        let mutable e = set.GetEnumerator()
        if e.MoveNext() then
            let mutable res = mapping e.Current
            while e.MoveNext() do res <- res.UnionWith(mapping e.Current)
            res
        else
            empty

    /// Tests if an entry for the given key exists. `O(1)`
    let inline contains (value : 'T) (set : HashSet<'T>) = set.Contains value
            
    /// Creates a seq holding all values.
    /// `O(N)`
    let inline toSeq (set : HashSet<'K>) = set :> seq<_>
    
    /// Creates a list holding all values.
    /// `O(N)`
    let inline toList (set : HashSet<'K>) = set.ToList()
    
    /// Creates an array holding all values.
    /// `O(N)`
    let inline toArray (set : HashSet<'K>) = set.ToArray()
        
    /// Creates a Set holding all entries contained in the HashSet.
    /// `O(N)`
    let inline toSet (set: HashSet<'T>) =
        set |> Set.ofSeq

    /// Creates a new set containing all elements from set1 and set2.
    /// `O(N + M)`  
    let inline union (set1 : HashSet<'T>) (set2 : HashSet<'T>) = set1.UnionWith set2
 
    /// Creates a new set containing all elements that are in set1 AND set2.
    /// `O(N + M)`  
    let inline intersect (set1 : HashSet<'T>) (set2 : HashSet<'T>) = set1.IntersectWith set2
    
    /// Creates a new set containing all elements that are either in set1 or set2 (but not in both)
    /// `O(N + M)`  
    let inline xor (set1 : HashSet<'T>) (set2 : HashSet<'T>) = set1.SymmetricExceptWith set2
    
    /// Creates a new set containing all elements from set1 that are not int set2.
    /// `O(N + M)`  
    let inline difference (set1 : HashSet<'T>) (set2 : HashSet<'T>) = set1.ExceptWith set2
    
    /// Creates a new set containing all elements that are in at least one of the given sets.
    let unionMany (sets : #seq<HashSet<'T>>) =
        use e = sets.GetEnumerator()
        if e.MoveNext() then
            let mutable s = e.Current
            while e.MoveNext() do s <- union s e.Current
            s
        else
            empty
            
    /// Creates a new set containing all elements that are in all the given sets.
    let intersectMany (sets : #seq<HashSet<'T>>) =
        use e = sets.GetEnumerator()
        if e.MoveNext() then
            let mutable s = e.Current
            while e.MoveNext() do s <- intersect s e.Current
            s
        else
            empty

    /// Checks if the two sets are equal. `O(N)`
    let inline equals (set1 : HashSet<'T>) (set2 : HashSet<'T>) = set1.SetEquals set2
    
    /// Checks if the two sets have at least one element in common. `O(N)`
    let inline overlaps (set1 : HashSet<'T>) (set2 : HashSet<'T>) = set1.Overlaps set2

    /// Checks if all elements from `set1` are in `set2`. `O(N)`
    let inline isSubset (set1 : HashSet<'T>) (set2 : HashSet<'T>) = set1.IsSubsetOf set2

    /// Checks if all elements from `set1` are in `set2` and `set2` contains at least one element that is not in `set1`. `O(N)`
    let inline isProperSubset (set1 : HashSet<'T>) (set2 : HashSet<'T>) = set1.IsProperSubsetOf set2

    /// Checks if all elements from `set2` are in `set1`. `O(N)`
    let inline isSuperset (set1 : HashSet<'T>) (set2 : HashSet<'T>) = set1.IsSupersetOf set2

    /// Checks if all elements from `set2` are in `set1` and `set1` contains at least one element that is not in `set3`. `O(N)`
    let inline isProperSuperset (set1 : HashSet<'T>) (set2 : HashSet<'T>) = set1.IsProperSupersetOf set2

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module HashMap =
    /// the empty map.
    [<GeneralizableValue>]
    let empty<'K, 'V> = HashMap<'K, 'V>.Empty

    /// The number of elements in the map `O(1)`
    let inline count (map : HashMap<'K, 'V>) = map.Count
    
    /// Is the map empty? `O(1)`
    let inline isEmpty (map : HashMap<'K, 'V>) = map.IsEmpty

    /// Are the two maps equal? `O(min(N, M))`
    let inline equals (map1 : HashMap<'K, 'V>) (map2 : HashMap<'K, 'V>) = map1.Equals map2
    
    /// Creates a map with a single entry. `O(1)`
    let inline single (key : 'K) (value : 'V) : HashMap<'K, 'V> = HashMap.Single(key, value)
    
    /// Creates a map with all entries from the seq. `O(N)`
    let inline ofSeq (elements : seq<'K * 'V>) = HashMap.OfSeq elements
    
    /// Creates a map with all entries from the seq. `O(N)`
    let inline ofSeqV (elements : seq<struct('K * 'V)>) = HashMap.OfSeq elements
    
    /// Creates a map with all entries from the list. `O(N)`
    let inline ofList (elements : list<'K * 'V>) = HashMap.OfList elements
    
    /// Creates a map with all entries from the list. `O(N)`
    let inline ofListV (elements : list<struct('K * 'V)>) = HashMap.OfList elements
    
    /// Creates a map with all entries from the array. `O(N)`
    let inline ofArray (elements : ('K * 'V)[]) = HashMap.OfArray elements
    
    /// Creates a map with all entries from the array. `O(N)`
    let inline ofArrayV (elements : struct('K * 'V)[]) = HashMap.OfArray elements
    
    /// Creates a map with all entries from the map. `O(N)`
    let inline ofMap (elements : Map<'K, 'V>) = elements |> Map.toSeq |> HashMap.OfSeq

    /// Adds or updates the entry for the given key. `O(1)`
    let inline add k v (m : HashMap<'K, 'V>) = m.Add(k, v)

    /// Removes the entry for the given key. `O(1)`
    let inline remove k (m : HashMap<'K, 'V>) = m.Remove k
 
    /// Tries to remove the entry for the given key from the map and returns its value and the rest of the map.
    /// `O(1)`
    let inline tryRemove k (m : HashMap<'K, 'V>) = m.TryRemove k

    /// Tries to remove the entry for the given key from the map and returns its value and the rest of the map.
    /// `O(1)`
    let inline tryRemoveV k (m : HashMap<'K, 'V>) = m.TryRemoveV k

    
    /// Adds, deletes or updates the entry for the given key.
    /// The update functions gets the optional old value and may optionally return
    /// A new value (or None for deleting the entry).
    /// `O(1)`
    let inline alter k update (m : HashMap<'K, 'V>) = m.Alter(k, update)
    
    /// Adds, deletes or updates the entry for the given key.
    /// The update functions gets the optional old value and may optionally return
    /// A new value (or None for deleting the entry).
    /// `O(1)`
    let inline alterV k update (m : HashMap<'K, 'V>) = m.AlterV(k, update)
    
    /// Adds or updates the entry for the given key.
    /// The update functions gets the optional old value and returns a new value for the key.
    /// `O(1)`
    let inline update k update (m : HashMap<'K, 'V>) = m.Alter(k, update >> Some)
    
    /// Adds or updates the entry for the given key.
    /// The update functions gets the optional old value and returns a new value for the key.
    /// `O(1)`
    let inline updateV k update (m : HashMap<'K, 'V>) = m.AlterV(k, update >> ValueSome)
    
    /// Tests if an entry for the given key exists. `O(1)`
    let inline containsKey k (m : HashMap<'K, 'V>) = m.ContainsKey k

    /// Tries to find the value for the given key.
    /// `O(1)`
    let inline tryFind k (m : HashMap<'K, 'V>) = m.TryFind k

    /// Tries to find the value for the given key.
    /// `O(1)`
    let inline tryFindV k (m : HashMap<'K, 'V>) = m.TryFindV k
    
    /// Finds the value for the given key and raises KeyNotFoundException on failure.
    /// `O(1)`
    let inline find k (m : HashMap<'K, 'V>) = 
        match m.TryFindV k with
        | ValueSome v -> v
        | ValueNone -> raise <| KeyNotFoundException()

    /// Tests whether all entries fulfil the given predicate.
    /// `O(N)`
    let inline forall predicate (m : HashMap<'K, 'V>) = m.Forall predicate
    
    /// Tests whether an entry making the predicate true exists.
    /// `O(N)`
    let inline exists predicate (m : HashMap<'K, 'V>) = m.Exists predicate
    
    /// Creates a new map (with the same keys) by applying the given function to all entries.
    /// `O(N)`
    let inline map mapping (m : HashMap<'K, 'V>) = m.Map mapping
    
    /// Creates a new map (with the same keys) by applying the given function to all entries.
    /// `O(N)`
    let inline choose mapping (m : HashMap<'K, 'V>) = m.Choose mapping
    
    /// Creates a new map (with the same keys) by applying the given function to all entries.
    /// `O(N)`
    let inline chooseV mapping (m : HashMap<'K, 'V>) = m.ChooseV mapping
    
    /// Creates a new map (with the same keys) that contains all entries for which predicate was true.
    /// `O(N)`
    let inline filter predicate (m : HashMap<'K, 'V>) = m.Filter predicate
    
    /// Applies the iter function to all entries of the map.
    /// `O(N)`
    let inline iter (action : 'K -> 'V -> unit) (m : HashMap<'K, 'V>) = m.Iter(action)
    
    /// Folds over all entries of the map.
    /// Note that the order for elements is undefined.
    /// `O(N)`
    let inline fold (folder : 'S -> 'K -> 'V -> 'S) (state : 'S) (m : HashMap<'K, 'V>) = m.Fold(folder, state)

    /// Creates a list holding all tuples contained in the map. `O(N)`
    let inline toList (m : HashMap<'K, 'V>) = m.ToList()
    
    /// Creates a list holding all tuples contained in the map. `O(N)`
    let inline toListV (m : HashMap<'K, 'V>) = m.ToListV()

    /// Creates a list holding all keys contained in the map. `O(N)`
    let inline toKeyList (m : HashMap<'K, 'V>) = m.ToKeyList()    
    
    /// Creates a list holding all values contained in the map. `O(N)`
    let inline toValueList (m : HashMap<'K, 'V>) = m.ToValueList()

    /// Creates an array holding all tuples contained in the map. `O(N)`
    let inline toArray (m : HashMap<'K, 'V>) = m.ToArray()

    /// Creates an array holding all tuples contained in the map. `O(N)`
    let inline toArrayV (m : HashMap<'K, 'V>) = m.ToArrayV()

    /// Creates an array holding all keys contained in the map. `O(N)`
    let inline toKeyArray (m : HashMap<'K, 'V>) = m.ToKeyArray()

    /// Creates an array holding all values contained in the map. `O(N)`
    let inline toValueArray (m : HashMap<'K, 'V>) = m.ToValueArray()

    /// Creates a seq holding all tuples contained in the map. `O(N)`
    let inline toSeq (m : HashMap<'K, 'V>) = m.ToSeq()

    /// Creates a seq holding all tuples contained in the map. `O(N)`
    let inline toSeqV (m : HashMap<'K, 'V>) = m.ToSeqV()

    /// Creates a seq holding all keys contained in the map. `O(N)`
    let inline toKeySeq (m : HashMap<'K, 'V>) = m.ToKeySeq()

    /// Creates a seq holding all values contained in the map. `O(N)`
    let inline toValueSeq (m : HashMap<'K, 'V>) = m.ToValueSeq()

    /// Creates a Map holding all tuples contained in the map. `O(N * log N)`
    let inline toMap (m : HashMap<'K, 'V>) = m.ToSeq() |> Map.ofSeq
    
    /// Creates a HashSet holding all keys contained in the map. `O(1)`
    let inline keys (m : HashMap<'K, 'V>) = m.GetKeys()

    /// Applies the given mapping function to all elements of the two maps. `O(N + M)`
    let inline choose2V (mapping : 'K -> voption<'T1> -> voption<'T2> -> voption<'R>) (l : HashMap<'K, 'T1>) (r : HashMap<'K, 'T2>) = l.Choose2V(r, mapping)
    
    /// Applies the given mapping function to all elements of the two maps. `O(N + M)`
    let inline choose2 (mapping : 'K -> option<'T1> -> option<'T2> -> option<'R>) (l : HashMap<'K, 'T1>) (r : HashMap<'K, 'T2>) = l.Choose2(r, mapping)
    
    /// Applies the given mapping function to all elements of the two maps. `O(N + M)`
    let inline map2V (mapping : 'K -> voption<'T1> -> voption<'T2> -> 'R) (l : HashMap<'K, 'T1>) (r : HashMap<'K, 'T2>) = l.Map2V(r, mapping)
    
    /// Applies the given mapping function to all elements of the two maps. `O(N + M)`
    let inline map2 (mapping : 'K -> option<'T1> -> option<'T2> -> 'R) (l : HashMap<'K, 'T1>) (r : HashMap<'K, 'T2>) = l.Map2(r, mapping)

    
    /// Creates a new map containing all elements from l and r.
    /// Colliding entries are taken from r.
    /// `O(N + M)`  
    let inline union (map1 : HashMap<'K, 'V>) (map2 : HashMap<'K, 'V>) = map1.UnionWith(map2)
    
    /// Creates a new map containing all elements from l and r.
    /// Colliding entries are resolved using the given function.
    /// `O(N + M)`  
    let inline unionWith (resolve : 'K -> 'V -> 'V -> 'V) (map1 : HashMap<'K, 'V>) (map2 : HashMap<'K, 'V>) = map1.UnionWith(map2, resolve)
    
    /// Creates a new map by unioning all the given maps.
    let unionMany (maps : #seq<HashMap<'K, 'V>>) = 
        use e = maps.GetEnumerator()
        if e.MoveNext() then
            let mutable res = e.Current
            while e.MoveNext() do res <- union res e.Current
            res
        else
            empty


    //let inline computeDelta (a : HashMap<'K, 'V>) (b : HashMap<'K, 'V>) = a.ComputeDeltaTo b
    //let inline applyDelta (state : HashMap<'K, 'V>) (delta : HashMap<'K, ElementOperation<'V>>) = state.ApplyDelta delta

