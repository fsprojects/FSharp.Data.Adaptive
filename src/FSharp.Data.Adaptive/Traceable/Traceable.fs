namespace FSharp.Data.Traceable

/// function table for a monoid instance
type Monoid<'T> =
    {
        /// determines whether the given value is empty
        misEmpty: 'T -> bool

        /// the empty element
        mempty: 'T

        /// appends to values
        mappend: 'T -> 'T -> 'T
    }

/// function table for a traceable instance
type Traceable<'State, 'Delta> =
    {
        /// the monoid instance for 'Delta
        tmonoid: Monoid<'Delta>

        /// the empty state
        tempty: 'State

        /// applies the given operations to the state and 
        /// returns the new state accompanied by (possibly) reduced ops (removing useless ops)
        tintegrate: 'State -> 'Delta -> 'State * 'Delta

        /// differentiates two states and returns the needed ops
        tdifferentiate: 'State -> 'State -> 'Delta

        /// Determines the size of an operation
        tsize: 'Delta -> int

        /// Determines whether or not a history should be pruned although it is still referentiable.
        /// the first argument is the base-state for that history and the second argument is the size of the operaton that would need to be applied.
        /// when returning true the history implementation will discard the history and reproduce it on demand using tdifferentiate.
        /// WARNING: current implementation is quite costly.
        tprune: option<'State -> int -> bool>
    }