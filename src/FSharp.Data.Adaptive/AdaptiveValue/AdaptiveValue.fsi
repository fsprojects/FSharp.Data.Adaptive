namespace FSharp.Data.Adaptive

/// Represents a dependency-aware value that may change as changes are fed into the system.
/// An AdaptiveValue cannot be changed directly but gets updated by the dependency graph. 
[<Interface>]
type AdaptiveValue =
    inherit IAdaptiveObject
    
    /// Evaluates the AdaptiveValue using the given token and returns the current value.
    /// Dependencies will be tracked automatically when the token is correctly passed to all inner evaluation-calls.
    abstract member GetValueUntyped: AdaptiveToken -> obj

    /// The (statically known) type of the returned value.
    abstract member ContentType : System.Type


/// Represents a dependency-aware value that may change as changes are fed into the system.
/// An AdaptiveValue cannot be changed directly but gets updated by the dependency graph. 
/// For changeable inputs see cval<'T>
[<Interface>]
type AdaptiveValue<'T> =
    inherit AdaptiveValue

    /// Evaluates the AdaptiveValue<'T> using the given token and returns the current value.
    /// Dependencies will be tracked automatically when the token is correctly passed to all inner evaluation-calls.
    abstract member GetValue : token : AdaptiveToken -> 'T

/// An abbreviation for AdaptiveValue
and aval<'T> = AdaptiveValue<'T>

/// Represents an adaptive value that can be changed by application code and
/// Used in dependency-aware computations.
[<Sealed; Class>]
type ChangeableValue<'T> =
    inherit AdaptiveObject
    interface AdaptiveValue<'T>

    /// Gets or sets the current value.
    /// Setting the value requires a Transaction to be active using `transact`.
    member Value : 'T with get, set

    /// Gets the current value and adds a dependency to the caller (if any)
    member GetValue : token : AdaptiveToken -> 'T

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
        interface AdaptiveValue
        interface AdaptiveValue<'T>
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
    val bind3 : mapping : ('T1 -> 'T2 -> 'T3 -> aval<'T4>) -> value1 : aval<'T1> -> value2 : aval<'T2> -> value2 : aval<'T3> -> aval<'T4>

    /// Creates a custom adaptive value using the given computation.
    /// Callers are responsible for removing inputs that are no longer needed.
    val custom : compute : (AdaptiveToken -> 'T) -> aval<'T>
