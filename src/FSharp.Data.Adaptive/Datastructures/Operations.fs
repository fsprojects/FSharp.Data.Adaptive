namespace FSharp.Data.Adaptive

/// represents a set operation (Add/Remove) using a reference count.
/// note that internally SetOperations may have reference counts > 1 and < -1.
[<Struct>]
type SetOperation<'T>(value : 'T, cnt : int) =
    /// the added/removed value
    member x.Value = value

    /// the reference count delta.
    member x.Count = cnt

    /// the inverse SetOperation to this one.
    member x.Inverse = SetOperation(value, -cnt)

    override x.ToString() =
        if cnt = 1 then sprintf "Add(%A)" value
        elif cnt = -1 then sprintf "Rem(%A)" value
        elif cnt > 0 then sprintf "Add%d(%A)" cnt value
        elif cnt < 0 then sprintf "Rem%d(%A)" -cnt value
        else "Nop"

/// functional operators for SetOperation<_>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SetOperation =
    /// creates a new SetOperation
    let inline create (cnt : int) (v : 'T) = SetOperation(v, cnt)

    /// creates an add operation (reference delta +1)
    let inline add (v : 'T) = SetOperation(v, 1)   

    /// creates a remove operation (reference delta -1)
    let inline rem (v : 'T) = SetOperation(v, -1)    
    
    /// inverts the operation.
    let inline inverse (d : SetOperation<'T>) = d.Inverse

    /// applies a mapping function to the operation's value.
    let map (mapping : 'A -> 'B) (op : SetOperation<'A>) = 
        SetOperation<'B>(mapping op.Value, op.Count)

/// SetOperation extensions making them look like a union-type.
[<AutoOpen>]
module SetDeltaExtensions =
    /// creates an add operation (reference delta +1)
    let inline Add(v : 'T) = SetOperation(v, 1)

    // creates a remove operation (reference delta -1)
    let inline Rem(v : 'T) = SetOperation(v, -1)

    type SetOperation<'T> with
        /// creates an add operation (reference delta +1)
        static member inline Add v = SetOperation<'T>(v, 1)

        // creates a remove operation (reference delta -1)
        static member inline Rem v = SetOperation<'T>(v, -1)

    /// active pattern for SetOperation.
    /// note that the patterns also returns the reference delta.
    let inline (|Add|Rem|) (d : SetOperation<'T>) =
        if d.Count > 0 then Add(d.Count, d.Value)
        else Rem(-d.Count, d.Value)

/// reprensents a element operation (Set/Remove) without its key.
/// typically datastructures will hold (key * ElementOperation) tuples.
[<Struct>]
type ElementOperation<'T> =
    | Set of 'T
    | Remove