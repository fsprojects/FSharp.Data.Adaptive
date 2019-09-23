namespace FSharp.Data.Adaptive.Reference

/// reference implementation for AdaptiveToken.
type AdaptiveToken private() =
    static let top = AdaptiveToken()
    static member Top = top

/// reference implementations for aref.
type aref<'T> =
    abstract member GetValue : AdaptiveToken -> 'T

/// reference implementation for cref.
type cref<'T>(value : 'T) =
    let mutable value = value

    /// gets the current value for the cref.
    member x.GetValue(_t : AdaptiveToken) =
        value

    /// gets or sets the current value for the cref.
    member x.Value 
        with get() = value
        and set v = value <- v

    interface aref<'T> with
        member x.GetValue(t) = x.GetValue(t)

/// functional operators for the aref reference-implementation.
module ARef =
    /// gets the current value for the given aref.
    let force (ref : aref<'T>) = 
        ref.GetValue(AdaptiveToken.Top)

    /// creates a new cref initially holding the given value.
    let init (value : 'T) =
        cref value

    /// creates an aref always holding the given value.
    let constant (value : 'T) =
        { new aref<'T> with
            member x.GetValue(_) = value
        }

    /// adaptively maps over the given aref and returns the resulting aref.
    let map (mapping : 'A -> 'B) (input : aref<'A>) =
        { new aref<'B> with
            member x.GetValue(t) = input.GetValue(t) |> mapping
        }
        
    /// adaptively maps over the given arefs and returns the resulting aref.
    let map2 (mapping : 'A -> 'B -> 'C) (ref1 : aref<'A>) (ref2 : aref<'B>) =
        { new aref<'C> with
            member x.GetValue(t) = mapping (ref1.GetValue(t)) (ref2.GetValue(t))
        }
        
    /// adaptively maps over the given arefs and returns the resulting aref.
    let map3 (mapping : 'A -> 'B -> 'C -> 'D) (ref1 : aref<'A>) (ref2 : aref<'B>) (ref3 : aref<'C>) =
        { new aref<'D> with
            member x.GetValue(t) = mapping (ref1.GetValue(t)) (ref2.GetValue(t)) (ref3.GetValue(t))
        }
        
    /// adaptively applies mapping to the given aref and also depends on the inner aref.
    let bind (mapping : 'A -> aref<'B>) (input : aref<'A>) =
        { new aref<'B> with
            member x.GetValue(t) =
                input.GetValue(t) |> mapping |> force
        }
    
    /// adaptively applies mapping to the given arefs and also depends on the inner aref.
    let bind2 (mapping : 'A -> 'B -> aref<'C>) (ref1 : aref<'A>) (ref2 : aref<'B>) =
        { new aref<'C> with
            member x.GetValue(_) =
                mapping (force ref1) (force ref2) |> force
        }
    
