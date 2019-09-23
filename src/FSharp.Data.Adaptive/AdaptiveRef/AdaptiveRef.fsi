namespace FSharp.Control.Incremental

/// Represents a dependency-aware reference that may change as changes are fed into the system.
/// An AdaptiveRef cannot be changed directly but gets updated by the dependency graph. 
/// For changeable input-references see cref<'T>
type AdaptiveRef<'T> =
    inherit IAdaptiveObject

    /// evaluates the aref<'T> using the given token and returns the current value.
    /// dependencies will be tracked automatically when the token is correctly passed to all inner evaluation-calls.
    abstract member GetValue : token : AdaptiveToken -> 'T

/// An abbreviation for AdaptiveRef
and aref<'T> = AdaptiveRef<'T>

/// Represents a value (a reference-cell) that can be changed by application code and
/// used in dependency-aware computations.
[<Sealed; Class>]
type ChangeableRef<'T> =
    inherit AdaptiveObject
    interface aref<'T>

    /// Gets or sets the current value of the reference.
    /// Setting the value requires a Transaction to be active using `transact`.
    member Value : 'T with get, set

    /// Gets the current value of the reference and adds a dependency to the caller (if any)
    member GetValue : token : AdaptiveToken -> 'T

    /// Creates a new changeable value, intially holding the given value
    new : value : 'T -> cref<'T>

and cref<'T> = ChangeableRef<'T>

/// Operators related to the aref<_> type 
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ARef =

    /// Evaluates the given adaptive value and returns its current value.
    /// This should not be used inside the adaptive evaluation
    /// of other AdaptiveObjects since it does not track dependencies.
    val inline force : ref : aref<'T> -> 'T

    /// Creates a changeable adaptive value intially holding the given value
    val inline init : value : 'T -> cref<'T>

    /// Creates a constant adaptive value always holding the given value.
    /// The system internally propagates constants.
    val constant : value : 'T -> aref<'T>
    
    /// Creates a constant adaptive value using the given create function.
    /// The system internally propagates constants.
    val delay : create : (unit -> 'T) -> aref<'T>

    /// Returns a new adaptive value that adaptively applies the mapping function to the given 
    /// adaptive inputs.
    val map : mapping : ('T1 -> 'T2) -> ref : aref<'T1> -> aref<'T2>

    /// Returns a new adaptive value that adaptively applies the mapping function to the given 
    /// adaptive inputs.
    val map2 : mapping : ('T1 -> 'T2 -> 'T3) -> ref1 : aref<'T1> -> ref2 : aref<'T2> -> aref<'T3>
    
    /// Returns a new adaptive value that adaptively applies the mapping function to the given 
    /// adaptive inputs.
    val map3 : mapping : ('T1 -> 'T2 -> 'T3 -> 'T4) -> ref1 : aref<'T1> -> ref2 : aref<'T2> -> ref3 : aref<'T3> -> aref<'T4>

    /// Returns a new adaptive value that adaptively applies the mapping function to the given 
    /// input and adaptively depends on the resulting adaptive value.
    /// The resulting adaptive value  will hold the latest value of the aref returned by mapping.
    val bind : mapping : ('T1 -> aref<'T2>) -> ref : aref<'T1> -> aref<'T2>

    /// Adaptively applies the mapping function to the given adaptive values and
    /// adaptively depends on the adaptive value returned by mapping.
    /// The resulting aref<'T3> will hold the latest value of the aref returned by mapping.
    val bind2 : mapping : ('T1 -> 'T2 -> aref<'T3>) -> ref1 : aref<'T1> -> ref2 : aref<'T2> -> aref<'T3>

    /// Creates a custom adaptive value using the given computation.
    /// Callers are responsible for removing inputs that are no longer needed.
    val custom : compute : (AdaptiveToken -> 'T) -> aref<'T>
