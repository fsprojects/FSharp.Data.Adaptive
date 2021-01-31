namespace FSharp.Data.Adaptive

/// Represents a dependency-aware value that may change as changes are fed into the system.
/// An AdaptiveValue cannot be changed directly but gets updated by the dependency graph. 
[<Interface>]
type IAdaptiveValue =
    inherit IAdaptiveObject
    
    /// Evaluates the AdaptiveValue using the given token and returns the current value.
    /// Dependencies will be tracked automatically when the token is correctly passed to all inner evaluation-calls.
    abstract member GetValueUntyped: AdaptiveToken -> obj

    /// The (statically known) type of the returned value.
    abstract member ContentType : System.Type

    /// Visits the IAdaptiveValue using the given visitor.
    /// This is useful for "magically" summoning the generic argument in an existential way.
    abstract member Accept : IAdaptiveValueVisitor<'R> -> 'R

/// Represents a dependency-aware value that may change as changes are fed into the system.
/// An AdaptiveValue cannot be changed directly but gets updated by the dependency graph. 
/// For changeable inputs see cval<'T>
and [<Interface>] IAdaptiveValue<'T> =
    inherit IAdaptiveValue

    /// Evaluates the AdaptiveValue<'T> using the given token and returns the current value.
    /// Dependencies will be tracked automatically when the token is correctly passed to all inner evaluation-calls.
    abstract member GetValue : token : AdaptiveToken -> 'T

/// Visitor for "magically" summoning a type-argument.
and [<Interface>] IAdaptiveValueVisitor<'R> =
    abstract member Visit<'T> : aval<'T> -> 'R

/// An abbreviation for AdaptiveValue
and aval<'T> = IAdaptiveValue<'T>

/// Represents an adaptive value that can be changed by application code and
/// Used in dependency-aware computations.
[<Sealed; Class>]
type ChangeableValue<'T> =
    inherit AdaptiveObject
    interface IAdaptiveValue<'T>

    /// Gets or sets the current value.
    /// Setting the value requires a Transaction to be active using `transact`.
    member Value : 'T with get, set

    /// Gets the current value and adds a dependency to the caller (if any)
    member GetValue : token : AdaptiveToken -> 'T
    
    /// Sets the current state of the cval.
    member UpdateTo : 'T -> bool

    /// Creates a new changeable value, intially holding the given value
    new : value : 'T -> cval<'T>

/// Represents an adaptive value that can be changed by application code and
/// Used in dependency-aware computations.
and cval<'T> = ChangeableValue<'T>

/// Operators related to the aval<_> type 
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AVal =

    [<AbstractClass>]
    type AbstractVal<'T> =
        inherit AdaptiveObject
        interface IAdaptiveValue
        interface IAdaptiveValue<'T>
        member GetValue : AdaptiveToken -> 'T
        abstract member Compute : AdaptiveToken -> 'T
        new : unit -> AbstractVal<'T>

    /// Evaluates the given adaptive value and returns its current value.
    /// This should not be used inside the adaptive evaluation
    /// of other AdaptiveObjects since it does not track dependencies.
    val inline force : value : aval<'T> -> 'T

    /// Creates a changeable adaptive value intially holding the given value
    val inline init : value : 'T -> cval<'T>

    /// Creates a constant adaptive value always holding the given value.
    /// The system internally propagates constants.
    val constant : value : 'T -> aval<'T>
    
    /// Creates a constant adaptive value using the given create function.
    /// The system internally propagates constants.
    val delay : create : (unit -> 'T) -> aval<'T>

    /// Returns a new adaptive value that adaptively applies the mapping function to the given 
    /// adaptive inputs.
    val map : mapping : ('T1 -> 'T2) -> value : aval<'T1> -> aval<'T2>
    
    /// Returns a new adaptive value that applies the mapping function whenever a value is demanded. 
    /// This is useful when applying very cheap mapping functions (like unbox, fst, etc.)
    /// WARNING: the mapping function will also be called for unchanged inputs.
    val mapNonAdaptive : mapping : ('T1 -> 'T2) -> value : aval<'T1> -> aval<'T2>

    /// Casts the given adaptive value to the specified type. Raises InvalidCastException *immediately*
    /// when the specified cast is not valid (similar to Seq.cast).
    val cast<'T> : value : IAdaptiveValue -> aval<'T>

    /// Returns a new adaptive value that adaptively applies the mapping function to the given 
    /// adaptive inputs.
    val map2 : mapping : ('T1 -> 'T2 -> 'T3) -> value1 : aval<'T1> -> value2 : aval<'T2> -> aval<'T3>
    
    /// Returns a new adaptive value that adaptively applies the mapping function to the given 
    /// adaptive inputs.
    val map3 : mapping : ('T1 -> 'T2 -> 'T3 -> 'T4) -> value1 : aval<'T1> -> value2 : aval<'T2> -> value3 : aval<'T3> -> aval<'T4>

    /// Returns a new adaptive value that adaptively applies the mapping function to the given 
    /// input and adaptively depends on the resulting adaptive value.
    /// The resulting adaptive value  will hold the latest value of the aval<_> returned by mapping.
    val bind : mapping : ('T1 -> aval<'T2>) -> value : aval<'T1> -> aval<'T2>

    /// Adaptively applies the mapping function to the given adaptive values and
    /// adaptively depends on the adaptive value returned by mapping.
    /// The resulting aval<'T3> will hold the latest value of the aval<_> returned by mapping.
    val bind2 : mapping : ('T1 -> 'T2 -> aval<'T3>) -> value1 : aval<'T1> -> value2 : aval<'T2> -> aval<'T3>
    
    /// Adaptively applies the mapping function to the given adaptive values and
    /// adaptively depends on the adaptive value returned by mapping.
    /// The resulting aval<'T4> will hold the latest value of the aval<_> returned by mapping.
    val bind3 : mapping : ('T1 -> 'T2 -> 'T3 -> aval<'T4>) -> value1 : aval<'T1> -> value2 : aval<'T2> -> value3 : aval<'T3> -> aval<'T4>

    /// Creates a custom adaptive value using the given computation.
    /// Callers are responsible for removing inputs that are no longer needed.
    val custom : compute : (AdaptiveToken -> 'T) -> aval<'T>
