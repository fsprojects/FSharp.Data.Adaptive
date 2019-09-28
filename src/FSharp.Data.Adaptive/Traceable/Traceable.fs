namespace FSharp.Data.Traceable

/// Function table for a monoid instance
type Monoid<'T> =
    {
        /// Determines whether the given value is empty
        misEmpty: 'T -> bool

        /// The empty element
        mempty: 'T

        /// Appends to values
        mappend: 'T -> 'T -> 'T
    }

/// Function table for a traceable instance
type Traceable<'State, 'Delta> =
    {
        /// The monoid instance for 'Delta
        tmonoid: Monoid<'Delta>

        /// The empty state
        tempty: 'State

        /// Applies the given operations to the state and 
        /// Returns the new state accompanied by (possibly) reduced ops (removing useless ops)
        tapplyDelta: 'State -> 'Delta -> 'State * 'Delta

        /// Differentiates two states and returns the needed ops
        tcomputeDelta: 'State -> 'State -> 'Delta

        /// Determines the size of an operation
        tsize: 'Delta -> int

        /// Determines whether or not a history should be pruned although it is still referentiable.
        /// The first argument is the base-state for that history and the second argument is the size of the operaton that would need to be applied.
        /// When returning true the history implementation will discard the history and reproduce it on demand using tcomputeDelta.
        /// WARNING: current implementation is quite costly.
        tprune: option<'State -> int -> bool>
    }