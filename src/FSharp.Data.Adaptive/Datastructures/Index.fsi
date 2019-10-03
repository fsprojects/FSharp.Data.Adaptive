namespace FSharp.Data.Adaptive

open System

/// datastructure representing an abstract index.
/// this is a 'simple' solution to the order-maintenance problem that has insert in O(log N), delete in O(1) and compare in O(1).
/// Note that the implementation is quite obfuscated due to concurrency.
[<Sealed>]
type Index =
    interface IComparable
    #if !FABLE_COMPILER
    interface IComparable<Index>
    #endif

/// functional operators for the Index datastructure.
/// supported operations are: Index.zero, Index.after(index), Index.before(index), Index.between(l, r).
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Index =
    /// the root index.
    val zero : Index

    /// gets an index after the given one.
    val after : index : Index -> Index

    /// gets an index before the given one.
    val before : index : Index -> Index

    /// gets an index between the given ones.
    val between : a : Index -> b : Index -> Index