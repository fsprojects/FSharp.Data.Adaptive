namespace FSharp.Data.Adaptive

/// Represents a set operation (Add/Remove) using a reference count.
/// Note that internally SetOperations may have reference counts > 1 and < -1.
[<Struct>]
type SetOperation<'T>(value : 'T, cnt : int) =
    /// The added/removed value
    member x.Value = value

    /// The reference count delta.
    member x.Count = cnt

    /// The inverse SetOperation to this one.
    member x.Inverse = SetOperation(value, -cnt)

    override x.ToString() =
        if cnt = 1 then sprintf "Add(%A)" value
        elif cnt = -1 then sprintf "Rem(%A)" value
        elif cnt > 0 then sprintf "Add%d(%A)" cnt value
        elif cnt < 0 then sprintf "Rem%d(%A)" -cnt value
        else "Nop"

    static member Add(value: 'T) = SetOperation(value, 1)
    static member Rem(value: 'T) = SetOperation(value, -1)


/// Functional operators for SetOperation<_>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SetOperation =
    /// Creates a new SetOperation
    let inline create (cnt : int) (v : 'T) = SetOperation(v, cnt)

    /// Creates an add operation (reference delta +1)
    let inline add (v : 'T) = SetOperation(v, 1)   

    /// Creates a remove operation (reference delta -1)
    let inline rem (v : 'T) = SetOperation(v, -1)    
    
    /// Inverts the operation.
    let inline inverse (d : SetOperation<'T>) = d.Inverse

    /// Applies a mapping function to the operation's value.
    let map (mapping : 'A -> 'B) (op : SetOperation<'A>) = 
        SetOperation<'B>(mapping op.Value, op.Count)

/// SetOperation extensions making them look like a union-type.
[<AutoOpen>]
module SetDeltaExtensions =
    /// Creates an add operation (reference delta +1)
    let inline Add(v : 'T) = SetOperation(v, 1)

    /// Creates a remove operation (reference delta -1)
    let inline Rem(v : 'T) = SetOperation(v, -1)

    type SetOperation<'T> with
        /// Creates an add operation (reference delta +1)
        static member inline Add v = SetOperation<'T>(v, 1)

        /// Creates a remove operation (reference delta -1)
        static member inline Rem v = SetOperation<'T>(v, -1)

    /// Active pattern for SetOperation.
    /// Note that the patterns also returns the reference delta.
    let inline (|Add|Rem|) (d : SetOperation<'T>) =
        if d.Count > 0 then Add(d.Count, d.Value)
        else Rem(-d.Count, d.Value)

/// Reprensents a element operation (Set/Remove) without its key.
/// Typically datastructures will hold (key * ElementOperation) tuples.
[<Struct>]
type ElementOperation<'T> =
    /// Set the associated key to a specific value.
    | Set of 'T
    /// Remove the associated key.
    | Remove