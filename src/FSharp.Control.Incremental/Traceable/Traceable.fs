namespace FSharp.Control.Traceable

/// function table for a monoid instance
type Monoid<'a> =
    {
        /// determines whether the given value is empty
        misEmpty : 'a -> bool

        /// the empty element
        mempty : 'a

        /// appends to values
        mappend : 'a -> 'a -> 'a
    }

/// function table for a traceable instance
type Traceable<'s, 'ops> =
    {
        /// the monoid instance for 'ops
        tmonoid : Monoid<'ops>

        /// the empty state
        tempty : 's

        /// applies the given operations to the state and 
        /// returns the new state accompanied by (possibly) reduced ops (removing useless ops)
        tintegrate : 's -> 'ops -> 's * 'ops

        /// differentiates two states and returns the needed ops
        tdifferentiate : 's -> 's -> 'ops

        /// determines the size of an operation
        tsize : 'ops -> int

        /// determines whether or not a history should be pruned although it is still referentiable.
        /// the first argument is the base-state for that history and the second argument is the size of the operaton that would need to be applied.
        /// when returning true the history implementation will discard the history and reproduce it on demand using tdifferentiate.
        /// WARNING: current implementation is quite costly.
        tprune    : Option<'s -> int -> bool>
    }