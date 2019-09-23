namespace FSharp.Data.Adaptive.Reference

/// reference implementation for AdaptiveToken.
type AdaptiveToken private() =
    static let top = AdaptiveToken()
    static member Top = top

/// reference implementations for aval.
type aval<'T> =
    abstract member GetValue : AdaptiveToken -> 'T

/// reference implementation for cval.
type cval<'T>(value : 'T) =
    let mutable value = value

    /// gets the current value for the cval.
    member x.GetValue(_t : AdaptiveToken) =
        value

    /// gets or sets the current value for the cval.
    member x.Value 
        with get() = value
        and set v = value <- v

    interface aval<'T> with
        member x.GetValue(t) = x.GetValue(t)

/// functional operators for the aval reference-implementation.
module AVal =
    /// gets the current value for the given aval.
    let force (value : aval<'T>) = 
        value.GetValue(AdaptiveToken.Top)

    /// creates a new cval initially holding the given value.
    let init (value : 'T) =
        cval value

    /// creates an aval always holding the given value.
    let constant (value : 'T) =
        { new aval<'T> with
            member x.GetValue(_) = value
        }

    /// adaptively maps over the given aval and returns the resulting aval.
    let map (mapping : 'A -> 'B) (input : aval<'A>) =
        { new aval<'B> with
            member x.GetValue(t) = input.GetValue(t) |> mapping
        }
        
    /// adaptively maps over the given aval and returns the resulting aval.
    let map2 (mapping : 'A -> 'B -> 'C) (value1 : aval<'A>) (value2 : aval<'B>) =
        { new aval<'C> with
            member x.GetValue(t) = mapping (value1.GetValue(t)) (value2.GetValue(t))
        }
        
    /// adaptively maps over the given avals and returns the resulting aval.
    let map3 (mapping : 'A -> 'B -> 'C -> 'D) (value1 : aval<'A>) (value2 : aval<'B>) (value3 : aval<'C>) =
        { new aval<'D> with
            member x.GetValue(t) = mapping (value1.GetValue(t)) (value2.GetValue(t)) (value3.GetValue(t))
        }
        
    /// adaptively applies mapping to the given aval and also depends on the inner aval.
    let bind (mapping : 'A -> aval<'B>) (input : aval<'A>) =
        { new aval<'B> with
            member x.GetValue(t) =
                input.GetValue(t) |> mapping |> force
        }
    
    /// adaptively applies mapping to the given avals and also depends on the inner aval.
    let bind2 (mapping : 'A -> 'B -> aval<'C>) (ref1 : aval<'A>) (ref2 : aval<'B>) =
        { new aval<'C> with
            member x.GetValue(_) =
                mapping (force ref1) (force ref2) |> force
        }
    
