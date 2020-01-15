namespace FSharp.Data.Adaptive
open System.Collections.Generic
open System.Threading

type IEqualityProvider =
    abstract member GetEqualityComparer<'T> : unit -> IEqualityComparer<'T>

type DefaultEqualityComparer private() =
    static let mutable created = 0

    static let unchecked = 
        { new IEqualityProvider with
            member x.GetEqualityComparer<'T>() =
                { new IEqualityComparer<'T> with
                    member __.GetHashCode(o : 'T) = Unchecked.hash o
                    member __.Equals(l : 'T, r : 'T) = Unchecked.equals l r
                }
        }
        
    static let system = 
        { new IEqualityProvider with
            member x.GetEqualityComparer<'T>() = 
                EqualityComparer<'T>.Default :> IEqualityComparer<_>
        }
     
    static let shallow = 
        { new IEqualityProvider with
            member x.GetEqualityComparer<'T>() = 
                ShallowEqualityComparer<'T>.Instance
        }

    static let mutable defaultCreator = unchecked
    static member Unchecked = unchecked
    static member System = system
    static member Shallow = shallow

    /// handle with care!!
    static member SetProvider(creator : IEqualityProvider) =
        if not (System.Object.ReferenceEquals(defaultCreator, creator)) then
            if created > 0 then failwith "cannot only set default equality before first use"
            defaultCreator <- creator

    static member internal GetEqualityComparer<'T>() = 
        created <- 1
        defaultCreator.GetEqualityComparer<'T>()

type DefaultEqualityComparer<'T> private() =
    static let cmp = DefaultEqualityComparer.GetEqualityComparer<'T>()
    static member Instance = cmp

module DefaultDictionary =
    let inline create<'Key, 'Value> () =
        System.Collections.Generic.Dictionary<'Key, 'Value>(DefaultEqualityComparer<'Key>.Instance)

module DefaultHashSet =
    let inline create<'T> () =
        System.Collections.Generic.HashSet<'T>(DefaultEqualityComparer<'T>.Instance)

module DefaultEquality =
    let inline hash (value : 'T) = 
        DefaultEqualityComparer<'T>.Instance.GetHashCode(value)

    let inline equals (a : 'T) (b : 'T) = 
        DefaultEqualityComparer<'T>.Instance.Equals(a, b)


type ReferenceEqualityComparer<'T when 'T : not struct> private() =
    static let cmp =
        { new System.Collections.Generic.IEqualityComparer<'T> with
            member __.GetHashCode(o : 'T) = System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode o
            member __.Equals(l : 'T, r : 'T) = System.Object.ReferenceEquals(l, r)
        }
    static member Instance = cmp

module ReferenceHashSet =
    let inline create<'T when 'T : not struct> () =
        System.Collections.Generic.HashSet<'T>(ReferenceEqualityComparer<'T>.Instance)
