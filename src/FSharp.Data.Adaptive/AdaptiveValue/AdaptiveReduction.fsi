namespace FSharp.Data.Adaptive

/// AdaptiveReduction holds operators for reducing collections adaptively.
/// Its main purpose is to simplify reductions on ASet/AMap/AList.
[<Struct>]
type AdaptiveReduction<'a, 's, 'v> =
    {
        /// The seed value for the reduction
        seed    : 's

        /// Addition function used to add new elements
        add     : 's -> 'a -> 's

        /// Subtraction function used when elements are removed.
        /// Note that it may return ValueNone in cases where the value can't be subtracted.
        /// However when doing so, the reduce implementations will fall back to recomputing the 
        /// result from scratch.
        sub     : 's -> 'a -> ValueOption<'s>

        /// The view function allows the result to be mapped to some other type.
        /// Note that reduce implementations internally assume that this is a very  cheap conversion.
        view    : 's -> 'v
    }

/// Functional operators for AdaptiveReduction.
module AdaptiveReduction =

    /// Creates a new AdaptiveReduction that simultaniously (parallel) applies two
    /// reductions by tupling the state- and result-type.
    val par: 
        left: AdaptiveReduction<'a, 's, 'v> -> 
        right: AdaptiveReduction<'a, 't, 'w> -> 
        AdaptiveReduction<'a, 's * 't, 'v * 'w>
        
    /// Creates a new AdaptiveReduction that simultaniously (parallel) applies two
    /// reductions by (struct) tupling the state- and result-type.
    val structpar: 
        left: AdaptiveReduction<'a, 's, 'v> -> 
        right: AdaptiveReduction<'a, 't, 'w> -> 
        AdaptiveReduction<'a, struct ('s * 't), struct ('v * 'w)>

    /// Applies a mapping function on the input side of the given AdaptiveReduction.
    /// Note that the supplied mapping function is assumed to be very cheap.
    val mapIn: 
        mapping: ('a -> 'b) -> 
        reduction: AdaptiveReduction<'b, 's, 'v> -> 
        AdaptiveReduction<'a, 's, 'v>
        
    /// Applies a mapping function on the output side of the given AdaptiveReduction.
    /// Note that the supplied mapping function is assumed to be very cheap.
    val mapOut: 
        mapping: ('v -> 'w) -> 
        reduction: AdaptiveReduction<'a, 's, 'v> -> 
        AdaptiveReduction<'a, 's, 'w>
        
    /// Creates a new AdaptiveReduction using the given (group-like) functions.
    /// Note that the functions need to fulfill the following laws (and maybe more)
    /// * `sub (add x a) a = x` (inverse)
    /// * `add (sub x a) a = x` (inverse)
    /// * `add (add x a) b = add (add x b) a` (commutativity)
    val group:
        zero: 's ->
        add: ('s -> 'a -> 's) ->
        sub: ('s -> 'a -> 's) -> 
        AdaptiveReduction<'a, 's, 's>
        
    /// Creates a new AdaptiveReduction using the given (partial-group-like) functions.
    /// Note that the functions need to fulfill the following laws when sub claims success (and maybe more)
    /// * `sub (add x a) a = x` (inverse)
    /// * `add (sub x a) a = x` (inverse)
    /// * `add (add x a) b = add (add x b) a` (commutativity)
    val halfGroup:
        zero: 's ->
        add: ('s -> 'a -> 's) ->
        sub: ('s -> 'a -> ValueOption<'s>) -> 
        AdaptiveReduction<'a, 's, 's>
        
    /// Creates a new AdaptiveReduction using the given *classical* fold function.
    /// Note the these *classical* folds are inefficient in presence of adaptivity and 
    /// should be replaced by *group-like* constructs whenever possible.
    val fold:
        zero: 's ->
        add: ('s -> 'a -> 's) ->
        AdaptiveReduction<'a, 's, 's>


    /// A simple reduction counting all elements.
    [<GeneralizableValue>]
    val count<'a> : AdaptiveReduction<'a, int, int>
    
    /// A simple reduction counting all elements that are true.
    val countPositive: AdaptiveReduction<bool, int, int>
    
    /// A simple reduction counting all elements that are false.
    val countNegative: AdaptiveReduction<bool, int, int>
    
    /// A reduction getting the smallest element (if any)
    val tryMin<'a when 'a : comparison> : AdaptiveReduction<'a, voption<'a>, voption<'a>> 
    
    /// A reduction getting the largest element (if any)
    val tryMax<'a when 'a : comparison> : AdaptiveReduction<'a, voption<'a>, voption<'a>> 

    /// A reduction returning the sum of all elements.
    val inline sum<'a, 's> : unit -> AdaptiveReduction<'a, 's, 's> 
        when 's : (static member Zero : 's)
        and ('s or 'a) : (static member (+) : 's -> 'a -> 's)
        and ('s or 'a) : (static member (-) : 's -> 'a -> 's)
        
    /// A reduction returning the product of all elements.
    val inline product<'a, 's> : unit -> AdaptiveReduction<'a, 's, 's> 
        when 'a : equality
        and 's : (static member One : 's)
        and 'a : (static member Zero : 'a)
        and ('s or 'a) : (static member (*) : 's -> 'a -> 's)
        and ('s or 'a) : (static member (/) : 's -> 'a -> 's)

    /// A reduction returning the average of all elements.
    val inline average<'a, 's> : unit -> AdaptiveReduction<'a, struct(int * 's), 's> 
        when 's : (static member Zero : 's)
        and ('s or 'a) : (static member (+) : 's -> 'a -> 's)
        and ('s or 'a) : (static member (-) : 's -> 'a -> 's)
        and 's : (static member DivideByInt : 's -> int -> 's)
        


