namespace FSharp.Data.Traceable

open FSharp.Data.Adaptive

/// functional operators for HashSetDelta.
module HashSetDelta =
    type private Monoid<'T> private() =
        /// the monoid instance for HashSetDelta
        static let monoid  =
            {
                mempty = HashSetDelta<'T>(HashMap.empty)
                mappend = fun l r -> l.Combine r
                misEmpty = fun s -> s.IsEmpty
            } 
        static member Instance = monoid

    /// the monoid instance for HashSetDelta
    [<GeneralizableValue>]
    let monoid<'T> = Monoid<'T>.Instance
   
/// functional operators for HashMapDelta.
module HashMapDelta =
    type private Monoid<'K, 'V> private() =
        /// the monoid instance for HashMapDelta
        static let monoid =
            {
                mempty = HashMapDelta.empty<'K, 'V>
                mappend = fun l r -> l.Combine r
                misEmpty = fun s -> s.Store.IsEmpty
            }
        static member Instance = monoid

    /// the monoid instance for HashMapDelta
    [<GeneralizableValue>]
    let monoid<'K, 'V> = Monoid<'K, 'V>.Instance
    
/// functional operators for HashSet.
module HashSet =

    /// type for caching the Traceable<_> instance for HashSet<_>
    type private Traceable<'T> private() =
        static let trace : Traceable<HashSet<'T>, HashSetDelta<'T>> =
            {
                tempty = HashSet.empty
                tdifferentiate = HashSet.differentiate
                tintegrate = HashSet.integrate
                tmonoid = HashSetDelta.monoid
                tprune = None
                tsize = fun s -> s.Count
            }
        static member Instance = trace

    /// the traceable instance for HashSet.
    let trace<'T> = Traceable<'T>.Instance
 
/// functional operators for HashMap.
module HashMap =

    /// type for caching the Traceable<_> instance for HashMap<_,_>
    type private TraceableInstance<'K, 'V> private() =
        static let trace : Traceable<HashMap<'K, 'V>, HashMapDelta<'K, 'V>> =
            {
                tempty = HashMap.empty
                tdifferentiate = HashMap.differentiate
                tintegrate = HashMap.integrate
                tmonoid = HashMapDelta.monoid
                tprune = None
                tsize = fun s -> s.Store.Count
            }
        static member Instance = trace

    /// the traceable instance for HashSet.
    let trace<'K, 'V> = TraceableInstance<'K, 'V>.Instance
     
