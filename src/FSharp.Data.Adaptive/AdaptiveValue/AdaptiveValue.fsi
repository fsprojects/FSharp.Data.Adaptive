namespace FSharp.Data.Adaptive

/// Represents a dependency-aware value that may change as changes are fed into the system.
/// An AdaptiveValue cannot be changed directly but gets updated by the dependency graph.
/// Use cval<'T> for values that can be modified directly.
///
/// Adaptive values support:
/// - Automatic dependency tracking
/// - Lazy evaluation (computed only when needed)
/// - Caching (recomputed only when dependencies change)
/// - Weak references (unused parts can be garbage collected)
[<Interface>]
type IAdaptiveValue =
    inherit IAdaptiveObject

    /// Evaluates the AdaptiveValue using the given token and returns the current value.
    /// Dependencies will be tracked automatically when the token is correctly passed to all inner evaluation-calls.
    /// The token is used to track which adaptive objects are accessed during evaluation,
    /// building the dependency graph for automatic change propagation.
    abstract member GetValueUntyped: AdaptiveToken -> obj

    /// The (statically known) type of the returned value.
    /// This is useful for reflection-based scenarios and generic programming.
    abstract member ContentType : System.Type

    /// Visits the IAdaptiveValue using the given visitor.
    /// This is useful for "magically" summoning the generic argument in an existential way.
    /// Enables type-safe operations on untyped IAdaptiveValue instances.
    abstract member Accept : IAdaptiveValueVisitor<'R> -> 'R

/// Represents a dependency-aware value that may change as changes are fed into the system.
/// An AdaptiveValue cannot be changed directly but gets updated by the dependency graph.
/// For changeable inputs see cval<'T>.
///
/// Use adaptive values to build reactive computations that automatically update when inputs change.
/// All operations (map, bind, etc.) create new adaptive values that form a dependency graph.
and [<Interface>] IAdaptiveValue<'T> =
    inherit IAdaptiveValue

    /// Evaluates the AdaptiveValue<'T> using the given token and returns the current value.
    /// Dependencies will be tracked automatically when the token is correctly passed to all inner evaluation-calls.
    /// The returned value is cached until any dependency changes.
    abstract member GetValue : token : AdaptiveToken -> 'T

/// Visitor for "magically" summoning a type-argument.
and [<Interface>] IAdaptiveValueVisitor<'R> =
    abstract member Visit<'T> : aval<'T> -> 'R

/// Type abbreviation for AdaptiveValue.
/// This is the primary type used in most application code.
and aval<'T> = IAdaptiveValue<'T>

/// Represents a changeable adaptive value that can be modified directly by application code.
/// Changes must be made within a transaction using `transact`.
/// Other adaptive values can depend on changeable values and will update automatically when they change.
///
/// Example:
///     let input = cval 10
///     transact (fun () -> input.Value <- 20)
[<Sealed; Class>]
type ChangeableValue<'T> =
    inherit AdaptiveObject
    interface IAdaptiveValue<'T>

    /// Gets or sets the current value.
    /// Setting the value requires a Transaction to be active using `transact`.
    /// Reading the value outside of an adaptive evaluation does not create a dependency.
    member Value : 'T with get, set

    /// Gets the current value and adds a dependency to the caller (if any).
    /// When called during adaptive evaluation, the caller will be notified when this value changes.
    member GetValue : token : AdaptiveToken -> 'T

    /// Sets the current state of the cval and returns true if the value actually changed.
    /// Uses equality comparison to determine if the value changed.
    /// Must be called within a transaction.
    member UpdateTo : 'T -> bool

    /// Creates a new changeable value, initially holding the given value.
    /// The value can be modified later using transact.
    new : value : 'T -> cval<'T>

/// Type abbreviation for ChangeableValue.
/// Represents a changeable adaptive value that serves as an input to the adaptive system.
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
    /// This forces immediate evaluation and should NOT be used inside adaptive evaluation
    /// of other AdaptiveObjects since it does not track dependencies.
    ///
    /// Use this to "exit" the adaptive world and get concrete values.
    /// Time complexity: O(1) if cached, O(n) where n = computation cost if invalidated
    ///
    /// Example:
    ///     let x = cval 5
    ///     let doubled = x |> AVal.map (fun v -> v * 2)
    ///     let value = AVal.force doubled  // => 10
    val inline force : value : aval<'T> -> 'T

    /// Creates a changeable adaptive value initially holding the given value.
    /// Shorthand for: new cval<'T>(value)
    ///
    /// Example:
    ///     let counter = AVal.init 0
    ///     transact (fun () -> counter.Value <- counter.Value + 1)
    val inline init : value : 'T -> cval<'T>

    /// Creates a constant adaptive value always holding the given value.
    /// The system internally propagates constants for optimization.
    /// Constant values never change and have minimal overhead.
    ///
    /// Use this instead of cval when the value will never change.
    /// Time complexity: O(1)
    val constant : value : 'T -> aval<'T>

    /// Creates a constant adaptive value using the given create function.
    /// The function is called once during creation.
    /// The system internally propagates constants for optimization.
    ///
    /// Use this for lazy initialization of constant values.
    val delay : create : (unit -> 'T) -> aval<'T>

    /// Returns a new adaptive value that adaptively applies the mapping function to the given
    /// adaptive input. The mapping is only recomputed when the input changes.
    ///
    /// Time complexity: O(cost of mapping) when input changes, O(1) when cached
    ///
    /// Example:
    ///     let x = cval 5
    ///     let squared = x |> AVal.map (fun v -> v * v)
    ///     AVal.force squared  // => 25
    val map : mapping : ('T1 -> 'T2) -> value : aval<'T1> -> aval<'T2>

    /// Returns a new adaptive value that applies the mapping function whenever a value is demanded.
    /// Unlike `map`, this does NOT cache the result and recomputes on every access.
    ///
    /// Use this ONLY for very cheap operations (like unbox, fst, snd, type casts).
    /// WARNING: The mapping function will be called even for unchanged inputs.
    ///
    /// Time complexity: O(cost of mapping) on every evaluation
    val mapNonAdaptive : mapping : ('T1 -> 'T2) -> value : aval<'T1> -> aval<'T2>

    /// Casts the given adaptive value to the specified type.
    /// Raises InvalidCastException *immediately* if the cast is invalid (similar to Seq.cast).
    ///
    /// Example:
    ///     let untyped : IAdaptiveValue = ...
    ///     let typed = AVal.cast<int> untyped
    val cast<'T> : value : IAdaptiveValue -> aval<'T>

    /// Returns a new adaptive value that adaptively applies the mapping function to two
    /// adaptive inputs. The mapping is only recomputed when either input changes.
    ///
    /// Time complexity: O(cost of mapping) when any input changes, O(1) when cached
    ///
    /// Example:
    ///     let x = cval 3
    ///     let y = cval 4
    ///     let sum = AVal.map2 (+) x y
    ///     AVal.force sum  // => 7
    val map2 : mapping : ('T1 -> 'T2 -> 'T3) -> value1 : aval<'T1> -> value2 : aval<'T2> -> aval<'T3>

    /// Returns a new adaptive value that adaptively applies the mapping function to three
    /// adaptive inputs. The mapping is only recomputed when any input changes.
    ///
    /// Time complexity: O(cost of mapping) when any input changes, O(1) when cached
    val map3 : mapping : ('T1 -> 'T2 -> 'T3 -> 'T4) -> value1 : aval<'T1> -> value2 : aval<'T2> -> value3 : aval<'T3> -> aval<'T4>

    /// Returns a new adaptive value with dynamic dependencies.
    /// The mapping function returns an aval, and the result tracks the current aval's value.
    /// When the input changes, the mapping is recomputed and dependencies switch to the new aval.
    ///
    /// This enables conditional dependency graphs where different branches are followed based on values.
    /// Inactive branches do NOT trigger recomputation and can be garbage collected.
    ///
    /// Time complexity: O(cost of mapping) when input changes, O(cost of inner aval) when inner changes
    ///
    /// Example:
    ///     let useA = cval true
    ///     let a = cval "A"
    ///     let b = cval "B"
    ///     let result = useA |> AVal.bind (fun use -> if use then a else b)
    ///     // Only depends on 'a' when useA is true, otherwise only on 'b'
    val bind : mapping : ('T1 -> aval<'T2>) -> value : aval<'T1> -> aval<'T2>

    /// Adaptively applies the mapping function to two adaptive values with dynamic dependencies.
    /// The resulting aval will hold the latest value of the aval returned by mapping.
    /// See `bind` for details on dynamic dependencies.
    val bind2 : mapping : ('T1 -> 'T2 -> aval<'T3>) -> value1 : aval<'T1> -> value2 : aval<'T2> -> aval<'T3>

    /// Adaptively applies the mapping function to three adaptive values with dynamic dependencies.
    /// The resulting aval will hold the latest value of the aval returned by mapping.
    /// See `bind` for details on dynamic dependencies.
    val bind3 : mapping : ('T1 -> 'T2 -> 'T3 -> aval<'T4>) -> value1 : aval<'T1> -> value2 : aval<'T2> -> value3 : aval<'T3> -> aval<'T4>

    /// Creates a custom adaptive value using the given computation function.
    /// The compute function receives an AdaptiveToken and should call GetValue on dependencies.
    ///
    /// WARNING: Advanced API. Callers are responsible for proper dependency tracking.
    /// Use map/bind for most scenarios instead.
    ///
    /// The compute function should:
    /// - Use the token to evaluate dependencies
    /// - Return the computed value
    /// - Be deterministic for the same input values
    val custom : compute : (AdaptiveToken -> 'T) -> aval<'T>
