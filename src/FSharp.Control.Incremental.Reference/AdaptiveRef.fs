namespace FSharp.Control.Incremental.Reference

/// reference implementation for AdaptiveToken.
type AdaptiveToken private() =
    static let top = AdaptiveToken()
    static member Top = top

/// reference implementations for aref.
type aref<'a> =
    abstract member GetValue : AdaptiveToken -> 'a

/// reference implementation for cref.
type cref<'a>(value : 'a) =
    let mutable value = value

    /// gets the current value for the cref.
    member x.GetValue(_t : AdaptiveToken) =
        value

    /// gets or sets the current value for the cref.
    member x.Value 
        with get() = value
        and set v = value <- v

    interface aref<'a> with
        member x.GetValue(t) = x.GetValue(t)

/// functional operators for the aref reference-implementation.
module ARef =
    /// gets the current value for the given aref.
    let force (ref : aref<'a>) = 
        ref.GetValue(AdaptiveToken.Top)

    /// creates a new cref initially holding the given value.
    let init (value : 'a) =
        cref value

    /// creates an aref always holding the given value.
    let constant (value : 'a) =
        { new aref<'a> with
            member x.GetValue(_) = value
        }

    /// adaptively maps over the given aref and returns the resulting aref.
    let map (mapping : 'a -> 'b) (input : aref<'a>) =
        { new aref<'b> with
            member x.GetValue(t) = input.GetValue(t) |> mapping
        }
        
    /// adaptively maps over the given arefs and returns the resulting aref.
    let map2 (mapping : 'a -> 'b -> 'c) (ref1 : aref<'a>) (ref2 : aref<'b>) =
        { new aref<'c> with
            member x.GetValue(t) = mapping (ref1.GetValue(t)) (ref2.GetValue(t))
        }
        
    /// adaptively maps over the given arefs and returns the resulting aref.
    let map3 (mapping : 'a -> 'b -> 'c -> 'd) (ref1 : aref<'a>) (ref2 : aref<'b>) (ref3 : aref<'c>) =
        { new aref<'d> with
            member x.GetValue(t) = mapping (ref1.GetValue(t)) (ref2.GetValue(t)) (ref3.GetValue(t))
        }
        
    /// adaptively applies mapping to the given aref and also depends on the inner aref.
    let bind (mapping : 'a -> aref<'b>) (input : aref<'a>) =
        { new aref<'b> with
            member x.GetValue(t) =
                input.GetValue(t) |> mapping |> force
        }
    
    /// adaptively applies mapping to the given arefs and also depends on the inner aref.
    let bind2 (mapping : 'a -> 'b -> aref<'c>) (ref1 : aref<'a>) (ref2 : aref<'b>) =
        { new aref<'c> with
            member x.GetValue(_) =
                mapping (force ref1) (force ref2) |> force
        }
    
