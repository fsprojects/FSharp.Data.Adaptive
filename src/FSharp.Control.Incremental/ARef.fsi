namespace FSharp.Control.Incremental


/// aref<'T> represents a dependency-aware reference that may change as changes are fed into the system.
/// note that it cannot be changed directly but gets updated by the dependency graph. 
/// for changeable input-references see cref<'T>
[<Interface>]
type aref<'T> =
    inherit IAdaptiveObject

    /// evaluates the aref<'T> using the given token and returns the current value.
    /// dependencies will be tracked automatically when the token is correctly passed to all inner evaluation-calls.
    abstract member GetValue : token : AdaptiveToken -> 'T

/// cref<'T> represents a reference-cell that can be changed by application code.
/// since it implements aref<'T> it can also be used in dependency aware computations.
[<Sealed; Class>]
type cref<'T> =
    inherit AdaptiveObject
    interface aref<'T>

    /// gets or sets the current value of the reference.
    /// note that setting the value requires a Transaction to be active `transact (fun () -> x.Value <- //...)`
    member Value : 'T with get, set

    /// gets the current value of the reference and adds a dependency to the caller (if any)
    member GetValue : token : AdaptiveToken -> 'T

    /// creates a new cref<'T> intially holding the given value
    new : value : 'T -> cref<'T>

/// operators related to the aref<_> type 
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ARef =
    /// evaluates the given aref<'T> and returns its current value.
    /// note that force shall not be used inside the adaptive evaluation
    /// of other AdaptiveObjects since it does not track dependencies.
    val inline force : ref : aref<'T> -> 'T

    /// creates a new cref<'T> intially holding the given value
    val inline init : value : 'T -> cref<'T>

    /// creates an unchangable aref<'T> always holding the given value.
    /// note that the system internally propagates constants.
    val constant : value : 'T -> aref<'T>

    /// adaptively applies the mapping function to the given aref<'A> and returns
    /// a new aref<'B> holding the result of the computation.
    val map : mapping : ('A -> 'B) -> ref : aref<'A> -> aref<'B>

    /// adaptively applies the mapping function to the given arefs and returns
    /// a new aref<'C> holding the result of the computation.
    val map2 : mapping : ('A -> 'B -> 'C) -> ref1 : aref<'A> -> ref2 : aref<'B> -> aref<'C>
    
    /// adaptively applies the mapping function to the given arefs and returns
    /// a new aref<'C> holding the result of the computation.
    val map3 : mapping : ('A -> 'B -> 'C -> 'D) -> ref1 : aref<'A> -> ref2 : aref<'B> -> ref3 : aref<'C> -> aref<'D>


    /// adaptively applies the mapping function to the given aref<'A> and also
    /// adaptively depends on the aref<'B> returned by mapping.
    /// the resulting aref<'B> will hold the latest value of the aref<'B> returned by mapping.
    val bind : mapping : ('A -> aref<'B>) -> ref : aref<'A> -> aref<'B>
