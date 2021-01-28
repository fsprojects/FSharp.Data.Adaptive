namespace FSharp.Data.Adaptive

open FSharp.Data.Adaptive
open System.Runtime.CompilerServices
open System.Collections.Generic

[<AutoOpen>]
module private HAMTNumberCrunching =
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

[<AutoOpen>]
module internal Implementation = 
    
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
        val mutable public SetLeft : SetNode<'K>
        val mutable public SetRight : SetNode<'K>
            
        static member GetCount(node : SetNode<'K>) =
            if isNull node then 0
            elif node.IsLeaf then 
                let node = node :?> SetLeaf<'K>
                if isNull node.SetNext then 1
                else
                    let mutable c = node.SetNext
                    let mutable cnt = 1
                    while not (isNull c) do 
                        c <- c.SetNext
                        cnt <- cnt + 1
                    cnt
            else
                let inner = node :?> Inner<'K>
                inner.Count
                

        member x.Prefix
            with inline get() = x.Data
            and inline set v = x.Data <- v

        new(prefix : uint32, mask : uint32, left : SetNode<'K>, right : SetNode<'K>) =
            let cnt = Inner.GetCount(left) + Inner.GetCount(right)
            { inherit SetNode<'K>(NodeKind.Inner, prefix); Mask = mask; Count = cnt; SetLeft = left; SetRight = right }

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
                        (add cmp hash key node.SetLeft) 
                        node.SetRight
                | 1u ->
                    newInner 
                        node.Prefix node.Mask 
                        node.SetLeft
                        (add cmp hash key node.SetRight) 
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
                        (alter cmp hash key update node.SetLeft) 
                        node.SetRight
                | 1u ->
                    newInner 
                        node.Prefix node.Mask 
                        node.SetLeft
                        (alter cmp hash key update node.SetRight) 
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
                    addInPlace cmp hash key &inner.SetLeft
                | 1u ->
                    addInPlace cmp hash key &inner.SetRight 
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
                    let mutable l = n.SetLeft
                    if tryRemove cmp hash key &l then
                        node <- newInner n.Prefix n.Mask l n.SetRight
                        true
                    else
                        false
                | 1u ->
                    let mutable r = n.SetRight
                    if tryRemove cmp hash key &r then
                        node <- newInner n.Prefix n.Mask n.SetLeft r
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
                | 0u -> contains cmp hash key node.SetLeft
                | 1u -> contains cmp hash key node.SetRight
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
                    equals cmp a.SetLeft b.SetLeft &&
                    equals cmp a.SetRight b.SetRight
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
                let lh = hash acc a.SetLeft
                let nh = combineHash lh (combineHash (int a.Prefix) (int a.Mask))
                hash nh a.SetRight
                
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
                iter action node.SetLeft
                iter action node.SetRight
                
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
                let state = fold folder state node.SetLeft
                fold folder state node.SetRight
                
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
                exists predicate node.SetLeft ||
                exists predicate node.SetRight
               
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
                forall predicate node.SetLeft &&
                forall predicate node.SetRight
                    
    
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
                let l = filter predicate node.SetLeft
                let r = filter predicate node.SetRight
                newInner node.Prefix node.Mask l r

        let rec toList (acc : list<'K>) (node : SetNode<'K>) =
            if isNull node then
                acc
            elif node.IsLeaf then
                let node = node :?> SetLeaf<'K>
                node.Key :: SetLinked.toList acc node.SetNext
            else
                let node = node :?> Inner<'K>
                toList (toList acc node.SetRight) node.SetLeft
                    
        let rec copyTo (dst : 'K[]) (index : int) (node : SetNode<'K>) =
            if isNull node then
                index
            elif node.IsLeaf then
                let node = node :?> SetLeaf<'K>
                dst.[index] <- node.Key
                SetLinked.copyTo dst (index + 1) node.SetNext
            else
                let node = node :?> Inner<'K>
                let i0 = copyTo dst index node.SetLeft
                copyTo dst i0 node.SetRight
                
        let rec mapToMap (mapping : 'K -> 'V) (node : SetNode<'K>) =
            if isNull node then
                null
            elif node.IsLeaf then
                let node = node :?> SetLeaf<'K>
                let v = mapping node.Key
                MapLeaf(node.Hash, node.Key, v, SetLinked.mapToMap mapping node.SetNext) :> SetNode<_>
            else
                let node = node :?> Inner<'K>
                let l = mapToMap mapping node.SetLeft
                let r = mapToMap mapping node.SetRight
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
                let l = chooseToMapV mapping node.SetLeft
                let r = chooseToMapV mapping node.SetRight
                newInner node.Prefix node.Mask l r

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
                    | 0u -> overlaps cmp na b.SetLeft
                    | 1u -> overlaps cmp na b.SetRight
                    | _ -> false
            elif nb.IsLeaf then
                let a = na :?> Inner<'K>
                let b = nb :?> SetLeaf<'K>
                match matchPrefixAndGetBit b.Hash a.Prefix a.Mask with
                | 0u -> overlaps cmp a.SetLeft nb
                | 1u -> overlaps cmp a.SetRight nb
                | _ -> false
            else
                let a = na :?> Inner<'K>
                let b = nb :?> Inner<'K>
                let cc = compareMasks a.Mask b.Mask
                if cc > 0 then
                    // a in b
                    match matchPrefixAndGetBit a.Prefix b.Prefix b.Mask with
                    | 0u -> overlaps cmp na b.SetLeft
                    | 1u -> overlaps cmp na b.SetRight
                    | _ -> false
                elif cc < 0 then
                    // b in a
                    match matchPrefixAndGetBit b.Prefix a.Prefix a.Mask with
                    | 0u -> overlaps cmp a.SetLeft nb
                    | 1u -> overlaps cmp a.SetRight nb
                    | _ -> false
                elif a.Prefix = b.Prefix then
                    overlaps cmp a.SetLeft b.SetLeft ||
                    overlaps cmp a.SetRight b.SetRight
                else
                    false

        let rec subset 
            (cmp : IEqualityComparer<'K>)
            (na : SetNode<'K>) (nb : SetNode<'K>) =
            if isNull na then true
            elif isNull nb then false
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
                    | 0u -> subset cmp na b.SetLeft
                    | 1u -> subset cmp na b.SetRight
                    | _ -> false
            elif nb.IsLeaf then
                false
            else
                let a = na :?> Inner<'K>
                let b = nb :?> Inner<'K>
                let cc = compareMasks a.Mask b.Mask
                if cc > 0 then
                    // a in b
                    match matchPrefixAndGetBit a.Prefix b.Prefix b.Mask with
                    | 0u -> subset cmp na b.SetLeft
                    | 1u -> subset cmp na b.SetRight
                    | _ -> false
                elif cc < 0 then
                    // b in a
                    false
                elif a.Prefix = b.Prefix then
                    subset cmp a.SetLeft b.SetLeft ||
                    subset cmp a.SetRight b.SetRight
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
                    | 0u -> newInner b.Prefix b.Mask (union cmp na b.SetLeft) b.SetRight
                    | 1u -> newInner b.Prefix b.Mask b.SetLeft (union cmp na b.SetRight)
                    | _ -> join a.Hash na b.Prefix nb
            elif nb.IsLeaf then
                let a = na :?> Inner<'K>
                let b = nb :?> SetLeaf<'K>
                match matchPrefixAndGetBit b.Hash a.Prefix a.Mask with
                | 0u -> newInner a.Prefix a.Mask (union cmp a.SetLeft nb) a.SetRight
                | 1u -> newInner a.Prefix a.Mask a.SetLeft (union cmp a.SetRight nb)
                | _ -> join a.Prefix na b.Hash nb
            else    
                let a = na :?> Inner<'K>
                let b = nb :?> Inner<'K>

                let cc = compareMasks a.Mask b.Mask
                if cc > 0 then 
                    // a in b
                    match matchPrefixAndGetBit a.Prefix b.Prefix b.Mask with
                    | 0u -> newInner b.Prefix b.Mask (union cmp na b.SetLeft) b.SetRight
                    | 1u -> newInner b.Prefix b.Mask b.SetLeft (union cmp na b.SetRight)
                    | _ -> join a.Prefix na b.Prefix nb
                elif cc < 0 then
                    // b in a
                    match matchPrefixAndGetBit b.Prefix a.Prefix a.Mask with
                    | 0u -> newInner a.Prefix a.Mask (union cmp a.SetLeft nb) a.SetRight
                    | 1u -> newInner a.Prefix a.Mask a.SetLeft (union cmp a.SetRight nb)
                    | _ -> join a.Prefix na b.Prefix nb
                elif a.Prefix = b.Prefix then
                    newInner a.Prefix a.Mask (union cmp a.SetLeft b.SetLeft) (union cmp a.SetRight b.SetRight)
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
                    | 0u -> intersect cmp na b.SetLeft
                    | 1u -> intersect cmp na b.SetRight
                    | _ -> null
            elif nb.IsLeaf then
                let a = na :?> Inner<'K>
                let b = nb :?> SetLeaf<'K>
                match matchPrefixAndGetBit b.Hash a.Prefix a.Mask with
                | 0u -> intersect cmp a.SetLeft nb
                | 1u -> intersect cmp a.SetRight nb
                | _ -> null
            else    
                let a = na :?> Inner<'K>
                let b = nb :?> Inner<'K>

                let cc = compareMasks a.Mask b.Mask
                if cc > 0 then 
                    // a in b
                    match matchPrefixAndGetBit a.Prefix b.Prefix b.Mask with
                    | 0u -> intersect cmp na b.SetLeft
                    | 1u -> intersect cmp na b.SetRight
                    | _ -> null
                elif cc < 0 then
                    // b in a
                    match matchPrefixAndGetBit b.Prefix a.Prefix a.Mask with
                    | 0u -> intersect cmp a.SetLeft nb
                    | 1u -> intersect cmp a.SetRight nb
                    | _ -> null
                elif a.Prefix = b.Prefix then
                    newInner a.Prefix a.Mask (intersect cmp a.SetLeft b.SetLeft) (intersect cmp a.SetRight b.SetRight)
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
                    | 0u -> newInner b.Prefix b.Mask (xor cmp na b.SetLeft) b.SetRight
                    | 1u -> newInner b.Prefix b.Mask b.SetLeft (xor cmp na b.SetRight)
                    | _ -> join a.Hash na b.Prefix nb
            elif nb.IsLeaf then
                let a = na :?> Inner<'K>
                let b = nb :?> SetLeaf<'K>
                match matchPrefixAndGetBit b.Hash a.Prefix a.Mask with
                | 0u -> newInner a.Prefix a.Mask (xor cmp a.SetLeft nb) a.SetRight
                | 1u -> newInner a.Prefix a.Mask a.SetLeft (xor cmp a.SetRight nb)
                | _ -> join a.Prefix na b.Hash nb
            else    
                let a = na :?> Inner<'K>
                let b = nb :?> Inner<'K>

                let cc = compareMasks a.Mask b.Mask
                if cc > 0 then 
                    // a in b
                    match matchPrefixAndGetBit a.Prefix b.Prefix b.Mask with
                    | 0u -> newInner b.Prefix b.Mask (xor cmp na b.SetLeft) b.SetRight
                    | 1u -> newInner b.Prefix b.Mask b.SetLeft (xor cmp na b.SetRight)
                    | _ -> join a.Prefix na b.Prefix nb
                elif cc < 0 then
                    // b in a
                    match matchPrefixAndGetBit b.Prefix a.Prefix a.Mask with
                    | 0u -> newInner a.Prefix a.Mask (xor cmp a.SetLeft nb) a.SetRight
                    | 1u -> newInner a.Prefix a.Mask a.SetLeft (xor cmp a.SetRight nb)
                    | _ -> join a.Prefix na b.Prefix nb
                elif a.Prefix = b.Prefix then
                    newInner a.Prefix a.Mask (xor cmp a.SetLeft b.SetLeft) (xor cmp a.SetRight b.SetRight)
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
                    | 0u -> difference cmp na b.SetLeft
                    | 1u -> difference cmp na b.SetRight
                    | _ -> na

            elif nb.IsLeaf then
                let a = na :?> Inner<'K>
                let b = nb :?> SetLeaf<'K>
                match matchPrefixAndGetBit b.Hash a.Prefix a.Mask with
                | 0u -> newInner a.Prefix a.Mask (difference cmp a.SetLeft nb) a.SetRight
                | 1u -> newInner a.Prefix a.Mask a.SetLeft (difference cmp a.SetRight nb)
                | _ -> na
            else    
                let a = na :?> Inner<'K>
                let b = nb :?> Inner<'K>

                let cc = compareMasks a.Mask b.Mask
                if cc > 0 then 
                    // a in b
                    match matchPrefixAndGetBit a.Prefix b.Prefix b.Mask with
                    | 0u -> difference cmp na b.SetLeft
                    | 1u -> difference cmp na b.SetRight
                    | _ -> na
                elif cc < 0 then
                    // b in a
                    match matchPrefixAndGetBit b.Prefix a.Prefix a.Mask with
                    | 0u -> newInner a.Prefix a.Mask (difference cmp a.SetLeft nb) a.SetRight
                    | 1u -> newInner a.Prefix a.Mask a.SetLeft (difference cmp a.SetRight nb)
                    | _ -> na
                elif a.Prefix = b.Prefix then
                    newInner a.Prefix a.Mask (difference cmp a.SetLeft b.SetLeft) (difference cmp a.SetRight b.SetRight)
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
                            (computeDelta cmp onlyLeft onlyRight na b.SetLeft)
                            (chooseToMapV onlyRight b.SetRight)
                    | 1u ->
                        newInner
                            b.Prefix b.Mask
                            (chooseToMapV onlyRight b.SetLeft)
                            (computeDelta cmp onlyLeft onlyRight na b.SetRight)
                    | _ ->
                        join b.Prefix (chooseToMapV onlyRight nb) a.Hash (chooseToMapV onlyLeft na)

            elif nb.IsLeaf then
                let a = na :?> Inner<'K>
                let b = nb :?> SetLeaf<'K>

                match matchPrefixAndGetBit b.Hash a.Prefix a.Mask with
                | 0u ->
                    newInner
                        a.Prefix a.Mask
                        (computeDelta cmp onlyLeft onlyRight a.SetLeft nb)
                        (chooseToMapV onlyLeft a.SetRight)
                | 1u ->
                    newInner
                        a.Prefix a.Mask
                        (chooseToMapV onlyLeft a.SetLeft)
                        (computeDelta cmp onlyLeft onlyRight a.SetRight nb)
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
                            (computeDelta cmp onlyLeft onlyRight na b.SetLeft)
                            (chooseToMapV onlyRight b.SetRight)
                    | 1u ->
                        newInner
                            b.Prefix b.Mask
                            (chooseToMapV onlyRight b.SetLeft)
                            (computeDelta cmp onlyLeft onlyRight na b.SetRight)
                    | _ ->
                        join b.Prefix (chooseToMapV onlyRight nb) a.Prefix (chooseToMapV onlyLeft na)

                elif cc < 0 then
                    // b in a
                    match matchPrefixAndGetBit b.Prefix a.Prefix a.Mask with
                    | 0u ->
                        newInner
                            a.Prefix a.Mask
                            (computeDelta cmp onlyLeft onlyRight a.SetLeft nb)
                            (chooseToMapV onlyLeft a.SetRight)
                    | 1u ->
                        newInner
                            a.Prefix a.Mask
                            (chooseToMapV onlyLeft a.SetLeft)
                            (computeDelta cmp onlyLeft onlyRight a.SetRight nb)
                    | _ ->
                        join a.Prefix (chooseToMapV onlyLeft na) b.Prefix (chooseToMapV onlyRight nb)

                elif a.Prefix = b.Prefix then
                    newInner 
                        a.Prefix a.Mask
                        (computeDelta cmp onlyLeft onlyRight a.SetLeft b.SetLeft)
                        (computeDelta cmp onlyLeft onlyRight a.SetRight b.SetRight)
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
                let l = applyDeltaNoState apply delta.SetLeft &ls
                let r = applyDeltaNoState apply delta.SetRight &rs
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
                        let mutable l = s.SetLeft
                        let delta = applyDelta cmp apply &l delta
                        state <- newInner s.Prefix s.Mask l s.SetRight
                        delta
                    | 1u ->
                        let mutable r = s.SetRight
                        let delta = applyDelta cmp apply &r delta
                        state <- newInner s.Prefix s.Mask s.SetLeft r
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
                    let ld = applyDelta cmp apply &ls d.SetLeft
                    let rd = applyDelta cmp apply &rs d.SetRight
                    state <- newInner d.Prefix d.Mask ls rs
                    newInner d.Prefix d.Mask ld rd
                | 1u -> 
                    let mutable ls = null
                    let mutable rs = state
                    let ld = applyDelta cmp apply &ls d.SetLeft
                    let rd = applyDelta cmp apply &rs d.SetRight
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
                        let mutable l = s.SetLeft
                        let delta = applyDelta cmp apply &l delta
                        state <- newInner s.Prefix s.Mask l s.SetRight
                        delta
                    | 1u ->
                        let mutable r = s.SetRight
                        let delta = applyDelta cmp apply &r delta
                        state <- newInner s.Prefix s.Mask s.SetLeft r
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
                        let ld = applyDelta cmp apply &ls d.SetLeft
                        let rd = applyDelta cmp apply &rs d.SetRight
                        state <- newInner d.Prefix d.Mask ls rs
                        newInner d.Prefix d.Mask ld rd
                    | 1u -> 
                        let mutable ls = null
                        let mutable rs = state
                        let ld = applyDelta cmp apply &ls d.SetLeft
                        let rd = applyDelta cmp apply &rs d.SetRight
                        state <- newInner d.Prefix d.Mask ls rs
                        newInner d.Prefix d.Mask ld rd
                    | _ ->
                        let mutable ls = null
                        let ld = applyDeltaNoState apply delta &ls
                        state <- join s.Prefix state d.Prefix ls
                        ld

                elif s.Prefix = d.Prefix then
                    let mutable ls = s.SetLeft
                    let mutable rs = s.SetRight
                    let ld = applyDelta cmp apply &ls d.SetLeft
                    let rd = applyDelta cmp apply &rs d.SetRight
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
                        (add cmp hash key value node.SetLeft) 
                        node.SetRight
                | 1u ->
                    newInner 
                        node.Prefix node.Mask 
                        node.SetLeft
                        (add cmp hash key value node.SetRight) 
                | _ ->
                    join node.Prefix node hash (MapLeaf(hash, key, value, null))
                    
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
                | 0u -> addInPlace cmp hash key value &inner.SetLeft
                | 1u -> addInPlace cmp hash key value &inner.SetRight
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
                toValueList (toValueList acc node.SetRight) node.SetLeft
                   
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
                toList (toList acc node.SetRight) node.SetLeft
                   
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
                toListV (toListV acc node.SetRight) node.SetLeft

        let rec toListMap (mapping : OptimizedClosures.FSharpFunc<'K, 'V, 'T>) (acc : list<'T>) (node : SetNode<'K>) =  
            if isNull node then
                acc
            elif node.IsLeaf then
                let node = node :?> MapLeaf<'K, 'V>
                mapping.Invoke(node.Key, node.Value) :: MapLinked.toListMap mapping acc node.MapNext
            else
                let node = node :?> Inner<'K>
                toListMap mapping (toListMap mapping acc node.SetRight) node.SetLeft

            
        let rec copyTo (dst : ('K * 'V)[]) (index : int) (node : SetNode<'K>) =
            if isNull node then
                index
            elif node.IsLeaf then
                let node = node :?> MapLeaf<'K, 'V>
                dst.[index] <- (node.Key, node.Value)
                MapLinked.copyTo dst (index + 1) node.MapNext
            else
                let node = node :?> Inner<'K>
                let i0 = copyTo dst index node.SetLeft
                copyTo dst i0 node.SetRight
                    
        let rec copyValuesTo (dst : 'V[]) (index : int) (node : SetNode<'K>) =
            if isNull node then
                index
            elif node.IsLeaf then
                let node = node :?> MapLeaf<'K, 'V>
                dst.[index] <- node.Value
                MapLinked.copyValuesTo dst (index + 1) node.MapNext
            else
                let node = node :?> Inner<'K>
                let i0 = copyValuesTo dst index node.SetLeft
                copyValuesTo dst i0 node.SetRight
                
        let rec copyToV (dst : struct('K * 'V)[]) (index : int) (node : SetNode<'K>) =
            if isNull node then
                index
            elif node.IsLeaf then
                let node = node :?> MapLeaf<'K, 'V>
                dst.[index] <- struct(node.Key, node.Value)
                MapLinked.copyToV dst (index + 1) node.MapNext
            else
                let node = node :?> Inner<'K>
                let i0 = copyToV dst index node.SetLeft
                copyToV dst i0 node.SetRight
                

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
                let l = chooseV mapping node.SetLeft
                let r = chooseV mapping node.SetRight
                newInner node.Prefix node.Mask l r

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
                            (computeDelta cmp onlyLeft onlyRight both na b.SetLeft)
                            (chooseV onlyRight b.SetRight)
                    | 1u ->
                        newInner
                            b.Prefix b.Mask
                            (chooseV onlyRight b.SetLeft)
                            (computeDelta cmp onlyLeft onlyRight both na b.SetRight)
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
                        (computeDelta cmp onlyLeft onlyRight both a.SetLeft nb)
                        (chooseV onlyLeft a.SetRight)
                | 1u ->
                    newInner
                        a.Prefix a.Mask
                        (chooseV onlyLeft a.SetLeft)
                        (computeDelta cmp onlyLeft onlyRight both a.SetRight nb)
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
                            (computeDelta cmp onlyLeft onlyRight both na b.SetLeft)
                            (chooseV onlyRight b.SetRight)
                    | 1u ->
                        newInner
                            b.Prefix b.Mask
                            (chooseV onlyRight b.SetLeft)
                            (computeDelta cmp onlyLeft onlyRight both na b.SetRight)
                    | _ ->
                        join a.Prefix (chooseV onlyLeft na) b.Prefix (chooseV onlyRight nb)
                elif cc < 0 then
                    // b in a
                    match matchPrefixAndGetBit b.Prefix a.Prefix a.Mask with
                    | 0u ->
                        newInner
                            a.Prefix a.Mask
                            (computeDelta cmp onlyLeft onlyRight both a.SetLeft nb)
                            (chooseV onlyLeft a.SetRight)
                    | 1u ->
                        newInner
                            a.Prefix a.Mask
                            (chooseV onlyLeft a.SetLeft)
                            (computeDelta cmp onlyLeft onlyRight both a.SetRight nb)
                    | _ ->
                        join a.Prefix (chooseV onlyLeft na) b.Prefix (chooseV onlyRight nb)
                elif a.Prefix = b.Prefix then
                    newInner
                        a.Prefix a.Mask
                        (computeDelta cmp onlyLeft onlyRight both a.SetLeft b.SetLeft)
                        (computeDelta cmp onlyLeft onlyRight both a.SetRight b.SetRight)
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
                let l = applyDeltaNoState apply delta.SetLeft &ls
                let r = applyDeltaNoState apply delta.SetRight &rs
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
                        let mutable l = s.SetLeft
                        let delta = applyDelta cmp apply &l delta
                        state <- newInner s.Prefix s.Mask l s.SetRight
                        delta
                    | 1u ->
                        let mutable r = s.SetRight
                        let delta = applyDelta cmp apply &r delta
                        state <- newInner s.Prefix s.Mask s.SetLeft r
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
                    let ld = applyDelta cmp apply &ls d.SetLeft
                    let rd = applyDelta cmp apply &rs d.SetRight
                    state <- newInner d.Prefix d.Mask ls rs
                    newInner d.Prefix d.Mask ld rd
                | 1u -> 
                    let mutable ls = null
                    let mutable rs = state
                    let ld = applyDelta cmp apply &ls d.SetLeft
                    let rd = applyDelta cmp apply &rs d.SetRight
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
                        let mutable l = s.SetLeft
                        let delta = applyDelta cmp apply &l delta
                        state <- newInner s.Prefix s.Mask l s.SetRight
                        delta
                    | 1u ->
                        let mutable r = s.SetRight
                        let delta = applyDelta cmp apply &r delta
                        state <- newInner s.Prefix s.Mask s.SetLeft r
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
                        let ld = applyDelta cmp apply &ls d.SetLeft
                        let rd = applyDelta cmp apply &rs d.SetRight
                        state <- newInner d.Prefix d.Mask ls rs
                        newInner d.Prefix d.Mask ld rd
                    | 1u -> 
                        let mutable ls = null
                        let mutable rs = state
                        let ld = applyDelta cmp apply &ls d.SetLeft
                        let rd = applyDelta cmp apply &rs d.SetRight
                        state <- newInner d.Prefix d.Mask ls rs
                        newInner d.Prefix d.Mask ld rd
                    | _ ->
                        let mutable ls = null
                        let ld = applyDeltaNoState apply delta &ls
                        state <- join s.Prefix state d.Prefix ls
                        ld

                elif s.Prefix = d.Prefix then
                    let mutable ls = s.SetLeft
                    let mutable rs = s.SetRight
                    let ld = applyDelta cmp apply &ls d.SetLeft
                    let rd = applyDelta cmp apply &rs d.SetRight
                    state <- newInner s.Prefix s.Mask ls rs
                    newInner d.Prefix d.Mask ld rd

                else
                    let mutable ls = null
                    let ld = applyDeltaNoState apply delta &ls
                    state <- join s.Prefix state d.Prefix ls
                    ld
                
                    


       

[<Struct>]
type HAMTSetEnumerator<'K> =
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
            x.Head <- node.SetLeft
            x.Tail <- node.SetRight :: x.Tail

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
type HAMTEnumerator<'K, 'V, 'T> =
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
            x.Head <- node.SetLeft
            x.Tail <- node.SetRight :: x.Tail

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

type internal HAMTEnumerable<'K, 'V, 'T>(root : SetNode<'K>, mapping : OptimizedClosures.FSharpFunc<'K, 'V, 'T>) =
    member x.GetEnumerator() = new HAMTEnumerator<_,_,_>(root, mapping)
    interface System.Collections.IEnumerable with member x.GetEnumerator() = x.GetEnumerator() :> _
    interface System.Collections.Generic.IEnumerable<'T> with member x.GetEnumerator() = x.GetEnumerator() :> _
        

[<Struct; CustomEquality; NoComparison; StructuredFormatDisplay("{AsString}")>]
type HAMTSet<'K> internal(comparer : IEqualityComparer<'K>, root : SetNode<'K>) =
        
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

    static let empty = HAMTSet<'K>(DefaultEqualityComparer<'K>.Instance, null)
        
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
        | :? HAMTSet<'K> as o -> SetNode.equals comparer root o.Root
        | _ -> false

    override x.ToString() =
        if x.Count > 8 then
            x |> Seq.take 8 |> Seq.map (sprintf "%A") |> String.concat "; " |> sprintf "HashSet [%s; ...]"
        else
            x |> Seq.map (sprintf "%A") |> String.concat "; " |> sprintf "HashSet [%s]"

    // ====================================================================================
    // Queries: contains/etc.
    // ====================================================================================
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
        HAMTSet<'K>(comparer, SetNode.add comparer hash key root)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Alter(key : 'K, update : bool -> bool) = 
        let hash = uint32 (comparer.GetHashCode key) &&& 0x7FFFFFFFu
        HAMTSet<'K>(comparer, SetNode.alter comparer hash key update root)


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Remove(key) = 
        let hash = uint32 (comparer.GetHashCode key) &&& 0x7FFFFFFFu
        let mutable root = root
        if SetNode.tryRemove comparer hash key &root then
            HAMTSet<'K>(comparer, root)
        else
            x
            
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.TryRemove(key) = 
        let hash = uint32 (comparer.GetHashCode key) &&& 0x7FFFFFFFu
        let mutable root = root
        if SetNode.tryRemove comparer hash key &root then
            HAMTSet<'K>(comparer, root) |> Some
        else
            None
             
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.TryRemoveV(key) = 
        let hash = uint32 (comparer.GetHashCode key) &&& 0x7FFFFFFFu
        let mutable root = root
        if SetNode.tryRemove comparer hash key &root then
            HAMTSet<'K>(comparer, root) |> ValueSome
        else
            ValueNone

    // ====================================================================================
    // Unary Operations: map/choose/filter/etc.
    // ====================================================================================

    member x.Iter(action : 'K -> unit) =
        SetNode.iter action root

    member x.Fold(folder : 'S -> 'K -> 'S, state : 'S) =
        if isNull root then
            state
        else
            let folder = OptimizedClosures.FSharpFunc<_,_,_>.Adapt folder
            SetNode.fold folder state root

    member x.Exists(predicate : 'K -> bool) =
        SetNode.exists predicate root

    member x.Forall(predicate : 'K -> bool) =
        SetNode.forall predicate root

    member x.Map(mapping : 'K -> 'T) =
        let cmp = DefaultEqualityComparer<'T>.Instance
        let mutable root = null
        for e in x do
            let n = mapping e
            let hash = uint32 (cmp.GetHashCode n) &&& 0x7FFFFFFFu
            SetNode.addInPlace cmp hash n &root |> ignore

        HAMTSet<'T>(cmp, root)
        
    member x.MapToMap(mapping : 'K -> 'V) =
        let root = SetNode.mapToMap mapping root
        HAMT<'K, 'V>(comparer, root)

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
        HAMTSet<'T>(cmp, root)

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
        HAMTSet<'T>(cmp, root)

    member x.Filter(predicate : 'K -> bool) =
        HAMTSet(comparer, SetNode.filter predicate root)

    // ====================================================================================
    // Binary Operations: union/computeDelta/etc.
    // ====================================================================================

    member x.Overlaps(other : HAMTSet<'K>) =
        SetNode.overlaps comparer root other.Root

    member x.SetEquals(other : HAMTSet<'K>) =
        SetNode.equals comparer root other.Root

    member x.IsSubsetOf(other : HAMTSet<'K>) =
        SetNode.subset comparer root other.Root

    member x.IsSupersetOf(other : HAMTSet<'K>) =
        SetNode.subset comparer other.Root root

    member x.IsProperSubsetOf(other : HAMTSet<'K>) =
        x.Count < other.Count &&
        SetNode.subset comparer root other.Root

    member x.IsProperSupersetOf(other : HAMTSet<'K>) =
        other.Count < x.Count &&
        SetNode.subset comparer other.Root root
        
    member x.Overlaps(other : seq<'K>) =
        match other with
        | :? HAMTSet<'K> as o -> x.Overlaps o
        | :? ISet<'K> as o -> x.Exists o.Contains 
        | :? array<'K> as o -> o |> Array.exists x.Contains
        | :? list<'K> as o -> o |> List.exists x.Contains
        | o -> o |> Seq.exists x.Contains
            
    member x.SetEquals(other : seq<'K>) =
        match other with
        | :? HAMTSet<'K> as o -> x.SetEquals o
        | :? array<'K> as o -> x.SetEquals (HAMTSet.FromArray o)
        | :? list<'K> as o -> x.SetEquals (HAMTSet.FromList o)
        | o -> x.SetEquals (HAMTSet.FromSeq o)
            
    member x.IsSubsetOf (other : seq<'K>) =
        match other with
        | :? HAMTSet<'K> as o -> x.IsSubsetOf o
        | :? ISet<'K> as o -> x.Forall o.Contains
        | :? array<'K> as o -> x.IsSubsetOf (HAMTSet.FromArray o)
        | :? list<'K> as o -> x.IsSubsetOf (HAMTSet.FromList o)
        | o -> x.IsSubsetOf (HAMTSet.FromSeq o)

    member x.IsProperSubsetOf (other : seq<'K>) =
        match other with
        | :? HAMTSet<'K> as o -> x.IsProperSubsetOf o
        | :? array<'K> as o -> x.IsProperSubsetOf (HAMTSet.FromArray o)
        | :? list<'K> as o -> x.IsProperSubsetOf (HAMTSet.FromList o)
        | o -> x.IsProperSubsetOf (HAMTSet.FromSeq o)

    member x.IsSupersetOf (other : seq<'K>) =
        match other with
        | :? HAMTSet<'K> as o -> x.IsSupersetOf o
        | :? array<'K> as o -> o |> Array.forall x.Contains
        | :? list<'K> as o -> o |> List.forall x.Contains
        | o -> o |> Seq.forall x.Contains

    member x.IsProperSupersetOf (other : seq<'K>) =
        match other with
        | :? HAMTSet<'K> as o -> x.IsProperSupersetOf o
        | :? array<'K> as o -> x.IsProperSupersetOf (HAMTSet.FromArray o)
        | :? list<'K> as o -> x.IsProperSupersetOf (HAMTSet.FromList o)
        | o -> x.IsProperSupersetOf (HAMTSet.FromSeq o)

    member x.UnionWith(other : HAMTSet<'K>) =
        HAMTSet<'K>(comparer, SetNode.union comparer root other.Root)
        
    member x.SymmetricExceptWith(other : HAMTSet<'K>) =
        HAMTSet<'K>(comparer, SetNode.xor comparer root other.Root)
        
    member x.ExceptWith(other : HAMTSet<'K>) =
        HAMTSet<'K>(comparer, SetNode.difference comparer root other.Root)
        
    member x.IntersectWith(other : HAMTSet<'K>) =
        HAMTSet<'K>(comparer, SetNode.intersect comparer root other.Root)

    member x.ComputeDeltaTo(other : HAMTSet<'K>) =
        let delta = SetNode.computeDelta comparer remOp addOp root other.Root
        HAMT<'K, int>(comparer, delta)

    member x.ApplyDelta(delta : HAMT<'K, int>) =
        let mutable state = root
        let delta = SetNode.applyDelta comparer applyOp &state delta.Root
        HAMTSet<'K>(comparer, state), HAMT<'K, int>(comparer, delta)

    // ====================================================================================
    // Creators: Empty/Singleton/FromList/FromSeq/etc.
    // ====================================================================================
    static member Empty = empty
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Single(key : 'K) =
        let cmp = DefaultEqualityComparer<'K>.Instance
        let hash = uint32 (cmp.GetHashCode key) &&& 0x7FFFFFFFu
        HAMTSet(cmp, SetLeaf(hash, key, null))

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member FromSeq(elements : seq<'K>) =
        let cmp = DefaultEqualityComparer<'K>.Instance
        let mutable root = null
        for e in elements do 
            let hash = uint32 (cmp.GetHashCode e) &&& 0x7FFFFFFFu
            SetNode.addInPlace cmp hash e &root |> ignore
        HAMTSet(cmp, root)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member FromList(elements : list<'K>) =
        let cmp = DefaultEqualityComparer<'K>.Instance
        let mutable root = null
        for e in elements do 
            let hash = uint32 (cmp.GetHashCode e) &&& 0x7FFFFFFFu
            SetNode.addInPlace cmp hash e &root |> ignore
        HAMTSet(cmp, root)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member FromArray(elements : 'K[]) =
        let cmp = DefaultEqualityComparer<'K>.Instance
        let mutable root = null
        for e in elements do 
            let hash = uint32 (cmp.GetHashCode e) &&& 0x7FFFFFFFu
            SetNode.addInPlace cmp hash e &root |> ignore
        HAMTSet(cmp, root)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member FromArrayRange(elements: array<'K>, offset: int, length: int) =  
        let cmp = DefaultEqualityComparer<'K>.Instance
        let mutable i = offset
        let mutable root = null
        let ee = offset + length
        while i < ee do
            let e = elements.[i]
            let hash = uint32 (cmp.GetHashCode e) &&& 0x7FFFFFFFu
            SetNode.addInPlace cmp hash e &root |> ignore
            i <- i + 1
        HAMTSet(cmp, root)
        
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
    member x.GetEnumerator() = new HAMTSetEnumerator<'K>(root)

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

and [<Sealed>] 
    HAMT<'K, 'V> internal(comparer : IEqualityComparer<'K>, root : SetNode<'K>) =
    static let empty = HAMT<'K, 'V>(DefaultEqualityComparer<'K>.Instance, null)

    static let tupleGetter = OptimizedClosures.FSharpFunc<'K, 'V, _>.Adapt(fun k v -> (k,v))
    static let valueTupleGetter = OptimizedClosures.FSharpFunc<'K, 'V, _>.Adapt(fun k v -> struct(k,v))
    static let keyGetter = OptimizedClosures.FSharpFunc<'K, 'V, _>.Adapt(fun k _ -> k)
    static let valueGetter = OptimizedClosures.FSharpFunc<'K, 'V, _>.Adapt(fun _ v -> v)

    static let addOp = OptimizedClosures.FSharpFunc<'K, 'V, voption<ElementOperation<'V>>>.Adapt(fun k v -> ValueSome (Set v))
    static let remOp = OptimizedClosures.FSharpFunc<'K, 'V, voption<ElementOperation<'V>>>.Adapt(fun k v -> ValueSome Remove)
    static let updateOp = OptimizedClosures.FSharpFunc<'K, 'V, 'V, voption<ElementOperation<'V>>>.Adapt(fun k v0 v1 -> if DefaultEquality.equals v0 v1 then ValueNone else ValueSome (Set v1))
    static let applyOp = 
        OptimizedClosures.FSharpFunc<'K, voption<'V>, ElementOperation<'V>, struct(voption<'V> * voption<ElementOperation<'V>>)>.Adapt(
            fun k s d ->
                match s with
                | ValueSome o ->
                    match d with
                    | Set n -> 
                        if DefaultEquality.equals o n then struct(ValueSome n, ValueNone)
                        else struct(ValueSome n, ValueSome (Set n))
                    | Remove ->
                        struct(ValueNone, ValueSome Remove)
                | ValueNone ->
                    match d with
                    | Set n -> struct(ValueSome n, ValueSome d)
                    | Remove -> struct(ValueNone, ValueNone)
        )

    // ====================================================================================
    // Properties: Count/IsEmpty/etc.
    // ====================================================================================
    member internal x.Root : SetNode<'K> = root
    member internal x.Comparer = comparer
    member x.Count = size root
    member x.IsEmpty = isNull root
        
        
    // ====================================================================================
    // Modifications: add/remove/etc.
    // ====================================================================================

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.Add(key : 'K, value : 'V) = 
        let hash = uint32 (comparer.GetHashCode key) &&& 0x7FFFFFFFu
        HAMT<'K, 'V>(comparer, MapNode.add comparer hash key value root)
            
            
    // ====================================================================================
    // Binary Operations: computeDelta/etc.
    // ====================================================================================
    member x.ComputeDeltaTo(other : HAMT<'K, 'V>) =
        let delta = MapNode.computeDelta comparer remOp addOp updateOp root other.Root
        HAMT<'K, ElementOperation<'V>>(comparer, delta)

    member x.ApplyDelta(delta : HAMT<'K, ElementOperation<'V>>) =
        let mutable state = root
        let delta = MapNode.applyDelta comparer applyOp &state delta.Root
        HAMT<'K, 'V>(comparer, state), HAMT<'K, ElementOperation<'V>>(comparer, delta)
        
    // ====================================================================================
    // Creators: Empty/Singleton/FromList/FromSeq/etc.
    // ====================================================================================
    static member Empty = empty
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Singleton(key : 'K, value : 'V) =
        let cmp = DefaultEqualityComparer<'K>.Instance
        let hash = uint32 (cmp.GetHashCode key) &&& 0x7FFFFFFFu
        HAMT(cmp, MapLeaf(hash, key, value, null))

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member FromSeq(elements : seq<'K * 'V>) =
        let cmp = DefaultEqualityComparer<'K>.Instance
        let mutable root = null
        for (k, v) in elements do 
            let hash = uint32 (cmp.GetHashCode k) &&& 0x7FFFFFFFu
            MapNode.addInPlace cmp hash k v &root |> ignore
        HAMT<'K, 'V>(cmp, root)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member FromList(elements : list<'K * 'V>) =
        let cmp = DefaultEqualityComparer<'K>.Instance
        let mutable root = null
        for (k, v) in elements do 
            let hash = uint32 (cmp.GetHashCode k) &&& 0x7FFFFFFFu
            MapNode.addInPlace cmp hash k v &root |> ignore
        HAMT<'K, 'V>(cmp, root)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member FromArray(elements : ('K * 'V)[]) =
        let cmp = DefaultEqualityComparer<'K>.Instance
        let mutable root = null
        for (k, v) in elements do 
            let hash = uint32 (cmp.GetHashCode k) &&& 0x7FFFFFFFu
            MapNode.addInPlace cmp hash k v &root |> ignore
        HAMT<'K, 'V>(cmp, root)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member FromSeq(elements : seq<struct('K * 'V)>) =
        let cmp = DefaultEqualityComparer<'K>.Instance
        let mutable root = null
        for struct(k, v) in elements do 
            let hash = uint32 (cmp.GetHashCode k) &&& 0x7FFFFFFFu
            MapNode.addInPlace cmp hash k v &root |> ignore
        HAMT<'K, 'V>(cmp, root)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member FromList(elements : list<struct('K * 'V)>) =
        let cmp = DefaultEqualityComparer<'K>.Instance
        let mutable root = null
        for struct(k, v) in elements do 
            let hash = uint32 (cmp.GetHashCode k) &&& 0x7FFFFFFFu
            MapNode.addInPlace cmp hash k v &root |> ignore
        HAMT<'K, 'V>(cmp, root)
        
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member FromArray(elements : struct('K * 'V)[]) =
        let cmp = DefaultEqualityComparer<'K>.Instance
        let mutable root = null
        for struct(k, v) in elements do 
            let hash = uint32 (cmp.GetHashCode k) &&& 0x7FFFFFFFu
            MapNode.addInPlace cmp hash k v &root |> ignore
        HAMT<'K, 'V>(cmp, root)
        

    // ====================================================================================
    // Accessors: GetKeys/CopyTo/ToList/etc.
    // ====================================================================================
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.GetKeys() = HAMTSet<'K>(comparer, root)
        
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
        x :> seq<_>
            
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ToSeqV() =
        HAMTEnumerable(root, valueTupleGetter) :> seq<_>
            
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ToValueSeq() =
        HAMTEnumerable(root, valueGetter) :> seq<_>
            
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.ToKeySeq() =
        HAMTEnumerable(root, keyGetter) :> seq<_>
            
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member x.GetEnumerator() = new HAMTEnumerator<'K, 'V, _>(root, tupleGetter)

    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = x.GetEnumerator() :> _
            
    interface System.Collections.Generic.IEnumerable<'K * 'V> with
        member x.GetEnumerator() = x.GetEnumerator() :> _

module HAMTSet =
    [<GeneralizableValue>]
    let empty<'T> = HAMTSet<'T>.Empty

    let inline isEmpty (m : HAMTSet<'T>) = m.IsEmpty
    let inline count (m : HAMTSet<'T>) = m.Count

    let inline single (value : 'T) = HAMTSet.Single(value)
    let inline ofSeq (elements : seq<'T>) = HAMTSet.FromSeq elements
    let inline ofList (elements : list<'T>) = HAMTSet.FromList elements
    let inline ofArray (elements : 'T[]) = HAMTSet.FromArray elements

    let inline add k (m : HAMTSet<'T>) = m.Add(k)
    let inline remove k (m : HAMTSet<'T>) = m.Remove k
    let inline tryRemove k (m : HAMTSet<'T>) = m.TryRemove k
    let inline tryRemoveV k (m : HAMTSet<'T>) = m.TryRemoveV k
    let inline alter k update (m : HAMTSet<'T>) = m.Alter(k, update)
        
    let inline iter (action : 'T -> unit) (m : HAMTSet<'T>) = m.Iter action
    let inline fold (folder : 'S -> 'T -> 'S) (state : 'S) (m : HAMTSet<'T>) = m.Fold(folder, state)
    let inline exists (predicate : 'T -> bool) (m : HAMTSet<'T>) = m.Exists predicate
    let inline forall (predicate : 'T -> bool) (m : HAMTSet<'T>) = m.Forall predicate
    let inline map (mapping : 'T -> 'U) (m : HAMTSet<'T>) = m.Map mapping
    let inline choose (mapping : 'T -> option<'U>) (m : HAMTSet<'T>) = m.Choose mapping
    let inline chooseV (mapping : 'T -> voption<'U>) (m : HAMTSet<'T>) = m.ChooseV mapping
    let inline filter (predicate : 'T -> bool) (m : HAMTSet<'T>) = m.Filter predicate
    let collect (mapping : 'T -> HAMTSet<'U>) (m : HAMTSet<'T>) =
        let mutable e = m.GetEnumerator()
        if e.MoveNext() then
            let mutable res = mapping e.Current
            while e.MoveNext() do res <- res.UnionWith(mapping e.Current)
            res
        else
            empty

    let inline toList (m : HAMTSet<'K>) = m.ToList()
    let inline toArray (m : HAMTSet<'K>) = m.ToArray()
    let inline toSeq (m : HAMTSet<'K>) = m :> seq<_>
        
    let inline union (a : HAMTSet<'T>) (b : HAMTSet<'T>) = a.UnionWith b
    let inline intersect (a : HAMTSet<'T>) (b : HAMTSet<'T>) = a.IntersectWith b
    let inline xor (a : HAMTSet<'T>) (b : HAMTSet<'T>) = a.SymmetricExceptWith b
    let inline difference (a : HAMTSet<'T>) (b : HAMTSet<'T>) = a.ExceptWith b

    let unionMany (maps : #seq<HAMTSet<'T>>) =
        use e = maps.GetEnumerator()
        if e.MoveNext() then
            let mutable s = e.Current
            while e.MoveNext() do s <- union s e.Current
            s
        else
            empty
            
    let intersectMany (maps : #seq<HAMTSet<'T>>) =
        use e = maps.GetEnumerator()
        if e.MoveNext() then
            let mutable s = e.Current
            while e.MoveNext() do s <- intersect s e.Current
            s
        else
            empty

    let inline computeDelta (a : HAMTSet<'K>) (b : HAMTSet<'K>) = a.ComputeDeltaTo b
    let inline applyDelta (state : HAMTSet<'K>) (delta : HAMT<'K, int>) = state.ApplyDelta delta

module HAMT =
    [<GeneralizableValue>]
    let empty<'K, 'V> = HAMT<'K, 'V>.Empty

    let inline singleton (key : 'K) (value : 'V) = HAMT.Singleton(key, value)

    let inline ofSeq (elements : seq<'K * 'V>) = HAMT.FromSeq elements
    let inline ofSeqV (elements : seq<struct('K * 'V)>) = HAMT.FromSeq elements
    let inline ofList (elements : list<'K * 'V>) = HAMT.FromList elements
    let inline ofListV (elements : list<struct('K * 'V)>) = HAMT.FromList elements
    let inline ofArray (elements : ('K * 'V)[]) = HAMT.FromArray elements
    let inline ofArrayV (elements : struct('K * 'V)[]) = HAMT.FromArray elements

    let inline add k v (m : HAMT<'K, 'V>) = m.Add(k, v)

    let inline toList (m : HAMT<'K, 'V>) = m.ToList()
    let inline toListV (m : HAMT<'K, 'V>) = m.ToListV()
    let inline toKeyList (m : HAMT<'K, 'V>) = m.ToKeyList()
    let inline toValueList (m : HAMT<'K, 'V>) = m.ToValueList()

    let inline toArray (m : HAMT<'K, 'V>) = m.ToArray()
    let inline toArrayV (m : HAMT<'K, 'V>) = m.ToArrayV()
    let inline toKeyArray (m : HAMT<'K, 'V>) = m.ToKeyArray()
    let inline toValueArray (m : HAMT<'K, 'V>) = m.ToValueArray()

    let inline toSeq (m : HAMT<'K, 'V>) = m.ToSeq()
    let inline toSeqV (m : HAMT<'K, 'V>) = m.ToSeqV()
    let inline toKeySeq (m : HAMT<'K, 'V>) = m.ToKeySeq()
    let inline toValueSeq (m : HAMT<'K, 'V>) = m.ToValueSeq()

    let inline keys (m : HAMT<'K, 'V>) = m.GetKeys()

    let inline computeDelta (a : HAMT<'K, 'V>) (b : HAMT<'K, 'V>) = a.ComputeDeltaTo b
    let inline applyDelta (state : HAMT<'K, 'V>) (delta : HAMT<'K, ElementOperation<'V>>) = state.ApplyDelta delta

