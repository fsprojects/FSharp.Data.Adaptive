namespace FSharp.Control.Traceable

open FSharp.Control.Incremental


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DHashSet =
    type private Monoid<'a> private() =
        /// the monoid instance for DHashSet
        static let monoid =
            {
                mempty = DHashSet<'a>(HashMap.empty)
                mappend = fun l r -> l.Combine r
                misEmpty = fun s -> s.IsEmpty
            }
        static member Instance = monoid
    /// the monoid instance for DHashSet
    [<GeneralizableValue>]
    let monoid<'a> = Monoid<'a>.Instance
    
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DHashMap =
    type private Monoid<'k, 'v> private() =
        /// the monoid instance for DHashMap
        static let monoid =
            {
                mempty = DHashMap.empty<'k, 'v>
                mappend = fun l r -> l.Combine r
                misEmpty = fun s -> s.IsEmpty
            }
        static member Instance = monoid

    /// the monoid instance for DHashMap
    [<GeneralizableValue>]
    let monoid<'k, 'v> = Monoid<'k, 'v>.Instance
    

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HashSet =

    /// type for caching the Traceable<_> instance for HashSet<_>
    type private Traceable<'a> private() =
        static let trace : Traceable<HashSet<'a>, DHashSet<'a>> =
            {
                tempty = HashSet.empty
                tdifferentiate = HashSet.differentiate
                tintegrate = HashSet.integrate
                tmonoid = DHashSet.monoid
                tprune = None
                tsize = fun s -> s.Count
            }
        static member Instance = trace

    /// the traceable instance for HashSet.
    let trace<'a> = Traceable<'a>.Instance
 
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HashMap =

    /// type for caching the Traceable<_> instance for HashMap<_,_>
    type private TraceableInstance<'k, 'v> private() =
        static let trace : Traceable<HashMap<'k, 'v>, DHashMap<'k, 'v>> =
            {
                tempty = HashMap.empty
                tdifferentiate = HashMap.differentiate
                tintegrate = HashMap.integrate
                tmonoid = DHashMap.monoid
                tprune = None
                tsize = fun s -> s.Count
            }
        static member Instance = trace

    /// the traceable instance for HashSet.
    let trace<'k, 'v> = TraceableInstance<'k, 'v>.Instance
     
