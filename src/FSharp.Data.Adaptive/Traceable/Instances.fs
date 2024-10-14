﻿namespace FSharp.Data.Traceable

open FSharp.Data.Adaptive

/// Functional operators for HashSetDelta.
module HashSetDelta =
    type private Monoid<'T> private() =
        /// The monoid instance for HashSetDelta
        static let monoid  =
            {
                mempty = HashSetDelta<'T>(HashMap.empty)
                mappend = fun l r -> l.Combine r
                misEmpty = fun s -> s.IsEmpty
            } 
        static member Instance = monoid

    /// The monoid instance for HashSetDelta
    [<GeneralizableValue>]
    let monoid<'T> = Monoid<'T>.Instance
 
/// Functional operators for IndexListDelta.
module IndexListDelta =
    type private Monoid<'T> private() =
        /// The monoid instance for HashSetDelta
        static let monoid  =
            {
                mempty = IndexListDelta<'T>.Empty
                mappend = fun l r -> l.Combine r
                misEmpty = fun s -> s.IsEmpty
            } 
        static member Instance = monoid

    /// The monoid instance for HashSetDelta
    [<GeneralizableValue>]
    let monoid<'T> = Monoid<'T>.Instance
     
/// Functional operators for HashMapDelta.
module HashMapDelta =
    type private Monoid<'K, 'V> private() =
        /// The monoid instance for HashMapDelta
        static let monoid =
            {
                mempty = HashMapDelta.empty<'K, 'V>
                mappend = fun l r -> l.Combine r
                misEmpty = fun s -> s.Store.IsEmpty
            }
        static member Instance = monoid

    /// The monoid instance for HashMapDelta
    [<GeneralizableValue>]
    let monoid<'K, 'V> = Monoid<'K, 'V>.Instance
    
/// Functional operators for HashSet.
module HashSet =

    /// Type for caching the Traceable<_> instance for HashSet<_>
    type private Traceable<'T> private() =
        static let trace : Traceable<HashSet<'T>, HashSetDelta<'T>> =
            {
                tempty = HashSet.empty
                tcomputeDelta = HashSet.computeDelta
                tapplyDelta = HashSet.applyDelta
                tmonoid = HashSetDelta.monoid
                tprune = None
                tsize = fun s -> s.Count
            }
        static member Instance = trace

    /// The traceable instance for HashSet.
    let trace<'T> = Traceable<'T>.Instance
 
/// Functional operators for HashMap.
module HashMap =

    /// Type for caching the Traceable<_> instance for HashMap<_,_>
    type private TraceableInstance<'K, 'V> private() =
        static let trace : Traceable<HashMap<'K, 'V>, HashMapDelta<'K, 'V>> =
            {
                tempty = HashMap.empty
                tcomputeDelta = HashMap.computeDelta
                tapplyDelta = HashMap.applyDelta
                tmonoid = HashMapDelta.monoid
                tprune = None
                tsize = fun s -> s.Store.Count
            }
        static member Instance = trace

    /// The traceable instance for HashSet.
    let trace<'K, 'V> = TraceableInstance<'K, 'V>.Instance
   
/// Functional operators for IndexList.
module IndexList =

    /// Type for caching the Traceable<_> instance for IndexList<_>
    type private Traceable<'T> private() =
        static let trace : Traceable<IndexList<'T>, IndexListDelta<'T>> =
            {
                tempty = IndexList.empty
                tcomputeDelta = IndexList.computeDelta
                tapplyDelta = IndexList.applyDelta
                tmonoid = IndexListDelta.monoid
                tprune = None
                tsize = fun s -> s.Count
            }
        static member Instance = trace

    /// The traceable instance for HashSet.
    let trace<'T> = Traceable<'T>.Instance
 

module ArrDelta =
    [<GeneralizableValue>]
    let monoid<'a> : Monoid<arrdelta<'a>> =
        {
            mempty = ArrDelta.empty
            misEmpty = ArrDelta.isEmpty
            mappend = ArrDelta.combine 
        }

    
module Arr =
    /// Type for caching the Traceable<_> instance for IndexList<_>
    type private Traceable<'T> private() =
        static let trace : Traceable<arr<'T>, arrdelta<'T>> =
            {
                tempty =  Arr.empty
                tcomputeDelta = Arr.computeDelta DefaultEqualityComparer.Instance
                tapplyDelta = Arr.applyDeltaAndGetEffective DefaultEqualityComparer.Instance
                tmonoid = ArrDelta.monoid
                tsize = fun _ -> 0
                tprune = None
            }
        static member Instance = trace

    
    [<GeneralizableValue>]
    let trace<'a> = Traceable<'a>.Instance

    let private traceCache = System.Collections.Generic.Dictionary<obj, obj>()
    
    let getTrace (cmp : System.Collections.Generic.IEqualityComparer<'T>) =
        lock traceCache (fun () ->
            match traceCache.TryGetValue cmp with
            | (true, (:? Traceable<arr<'T>, arrdelta<'T>> as traceable)) ->
                traceable
            | _ ->
                let traceable =
                    {
                        tempty =  Arr.empty
                        tcomputeDelta = Arr.computeDelta cmp
                        tapplyDelta = fun m d -> Arr.applyDeltaAndGetEffective cmp m d
                        tmonoid = ArrDelta.monoid
                        tsize = fun _ -> 0
                        tprune = None
                    }
                traceCache.[cmp] <- traceable
                traceable
        )
        
    



