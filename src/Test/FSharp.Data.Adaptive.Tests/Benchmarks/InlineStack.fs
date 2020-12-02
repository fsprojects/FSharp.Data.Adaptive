namespace Benchmarks

open System
open System.Collections
open System.Collections.Generic
open BenchmarkDotNet.Attributes
open Microsoft.FSharp.NativeInterop
open System.Runtime.InteropServices
open FSharp.Data.Adaptive



module private InlineStackUtilities =
    
    let inline get (ref : byref<'T>) =
        let v = ref
        ref <- Unchecked.defaultof<'T>
        v

    let inline size (t : MapExtImplementation.MapTree<_,_>) =
        match t with
        | MapExtImplementation.MapEmpty -> 0
        | MapExtImplementation.MapOne _ -> 1
        | MapExtImplementation.MapNode(_,_,_,_,_,s) -> s


[<NoComparison; NoEquality>]
type InlineStack16<'T> =
    struct
        [<DefaultValue(false)>]
        val mutable public Rest : list<'T>
        [<DefaultValue(false)>]
        val mutable public E0 : 'T
        [<DefaultValue(false)>]
        val mutable public E1 : 'T
        [<DefaultValue(false)>]
        val mutable public E2 : 'T
        [<DefaultValue(false)>]
        val mutable public E3 : 'T
        [<DefaultValue(false)>]
        val mutable public E4 : 'T
        [<DefaultValue(false)>]
        val mutable public E5 : 'T
        [<DefaultValue(false)>]
        val mutable public E6 : 'T
        [<DefaultValue(false)>]
        val mutable public E7 : 'T
        [<DefaultValue(false)>]
        val mutable public E8 : 'T
        [<DefaultValue(false)>]
        val mutable public E9 : 'T
        [<DefaultValue(false)>]
        val mutable public E10 : 'T
        [<DefaultValue(false)>]
        val mutable public E11 : 'T
        [<DefaultValue(false)>]
        val mutable public E12 : 'T
        [<DefaultValue(false)>]
        val mutable public E13 : 'T
        [<DefaultValue(false)>]
        val mutable public E14 : 'T
        [<DefaultValue(false)>]
        val mutable public E15 : 'T
        [<DefaultValue(false)>]
        val mutable public Count : int

        member inline x.Push(value : 'T) =
            match x.Count with
            | 0 -> x.E0 <- value
            | 1 -> x.E1 <- value
            | 2 -> x.E2 <- value
            | 3 -> x.E3 <- value
            | 4 -> x.E4 <- value
            | 5 -> x.E5 <- value
            | 6 -> x.E6 <- value
            | 7 -> x.E7 <- value
            | 8 -> x.E8 <- value
            | 9 -> x.E9 <- value
            | 10 -> x.E10 <- value
            | 11 -> x.E11 <- value
            | 12 -> x.E12 <- value
            | 13 -> x.E13 <- value
            | 14 -> x.E14 <- value
            | 15 -> x.E15 <- value
            | _ -> 
                if isNull (x.Rest :> obj) then x.Rest <- [value]
                else x.Rest <- value :: x.Rest

            x.Count <- x.Count + 1

        member inline x.Pop() =
            let c = x.Count
            if c <= 0 then raise <| System.IndexOutOfRangeException()
            x.Count <- c - 1

            match c with
            | 1 -> InlineStackUtilities.get &x.E0
            | 2 -> InlineStackUtilities.get &x.E1
            | 3 -> InlineStackUtilities.get &x.E2
            | 4 -> InlineStackUtilities.get &x.E3
            | 5 -> InlineStackUtilities.get &x.E4
            | 6 -> InlineStackUtilities.get &x.E5
            | 7 -> InlineStackUtilities.get &x.E6
            | 8 -> InlineStackUtilities.get &x.E7
            | 9 -> InlineStackUtilities.get &x.E8
            | 10 -> InlineStackUtilities.get &x.E9
            | 11 -> InlineStackUtilities.get &x.E10
            | 12 -> InlineStackUtilities.get &x.E11
            | 13 -> InlineStackUtilities.get &x.E12
            | 14 -> InlineStackUtilities.get &x.E13
            | 15 -> InlineStackUtilities.get &x.E14
            | 16 -> InlineStackUtilities.get &x.E15
            | _ ->
                if isNull (x.Rest :> obj) then failwith "invalid state"
                let h = List.head x.Rest
                x.Rest <- List.tail x.Rest
                h
    end

[<NoComparison; NoEquality>]
type InlineStack12<'T> =
    struct
        [<DefaultValue(false)>]
        val mutable public Rest : list<'T>
        [<DefaultValue(false)>]
        val mutable public E0 : 'T
        [<DefaultValue(false)>]
        val mutable public E1 : 'T
        [<DefaultValue(false)>]
        val mutable public E2 : 'T
        [<DefaultValue(false)>]
        val mutable public E3 : 'T
        [<DefaultValue(false)>]
        val mutable public E4 : 'T
        [<DefaultValue(false)>]
        val mutable public E5 : 'T
        [<DefaultValue(false)>]
        val mutable public E6 : 'T
        [<DefaultValue(false)>]
        val mutable public E7 : 'T
        [<DefaultValue(false)>]
        val mutable public E8 : 'T
        [<DefaultValue(false)>]
        val mutable public E9 : 'T
        [<DefaultValue(false)>]
        val mutable public E10 : 'T
        [<DefaultValue(false)>]
        val mutable public E11 : 'T
        [<DefaultValue(false)>]
        val mutable public Count : int

        member inline x.Push(value : 'T) =
            match x.Count with
            | 0 -> x.E0 <- value
            | 1 -> x.E1 <- value
            | 2 -> x.E2 <- value
            | 3 -> x.E3 <- value
            | 4 -> x.E4 <- value
            | 5 -> x.E5 <- value
            | 6 -> x.E6 <- value
            | 7 -> x.E7 <- value
            | 8 -> x.E8 <- value
            | 9 -> x.E9 <- value
            | 10 -> x.E10 <- value
            | 11 -> x.E11 <- value
            | _ -> 
                if isNull (x.Rest :> obj) then x.Rest <- [value]
                else x.Rest <- value :: x.Rest
            x.Count <- x.Count + 1

        member inline x.Pop() =
            let c = x.Count
            if c <= 0 then raise <| System.IndexOutOfRangeException()
            x.Count <- c - 1
            match c with
            | 1 -> InlineStackUtilities.get &x.E0
            | 2 -> InlineStackUtilities.get &x.E1
            | 3 -> InlineStackUtilities.get &x.E2
            | 4 -> InlineStackUtilities.get &x.E3
            | 5 -> InlineStackUtilities.get &x.E4
            | 6 -> InlineStackUtilities.get &x.E5
            | 7 -> InlineStackUtilities.get &x.E6
            | 8 -> InlineStackUtilities.get &x.E7
            | 9 -> InlineStackUtilities.get &x.E8
            | 10 -> InlineStackUtilities.get &x.E9
            | 11 -> InlineStackUtilities.get &x.E10
            | 12 -> InlineStackUtilities.get &x.E11
            | _ ->
                if isNull (x.Rest :> obj) then failwith "invalid state"
                let h = List.head x.Rest
                x.Rest <- List.tail x.Rest
                h
    end


[<NoComparison; NoEquality>]
type InlineStack8<'T> =
    struct
        [<DefaultValue(false)>]
        val mutable public Rest : list<'T>
        [<DefaultValue(false)>]
        val mutable public E0 : 'T
        [<DefaultValue(false)>]
        val mutable public E1 : 'T
        [<DefaultValue(false)>]
        val mutable public E2 : 'T
        [<DefaultValue(false)>]
        val mutable public E3 : 'T
        [<DefaultValue(false)>]
        val mutable public E4 : 'T
        [<DefaultValue(false)>]
        val mutable public E5 : 'T
        [<DefaultValue(false)>]
        val mutable public E6 : 'T
        [<DefaultValue(false)>]
        val mutable public E7 : 'T
        [<DefaultValue(false)>]
        val mutable public Count : int

        member inline x.Push(value : 'T) =
            match x.Count with
            | 0 -> x.E0 <- value
            | 1 -> x.E1 <- value
            | 2 -> x.E2 <- value
            | 3 -> x.E3 <- value
            | 4 -> x.E4 <- value
            | 5 -> x.E5 <- value
            | 6 -> x.E6 <- value
            | 7 -> x.E7 <- value
            | _ -> 
                if isNull (x.Rest :> obj) then x.Rest <- [value]
                else x.Rest <- value :: x.Rest
            x.Count <- x.Count + 1

        member inline x.Pop() =
            let c = x.Count
            if c <= 0 then raise <| System.IndexOutOfRangeException()
            x.Count <- c - 1
            match c with
            | 1 -> InlineStackUtilities.get &x.E0
            | 2 -> InlineStackUtilities.get &x.E1
            | 3 -> InlineStackUtilities.get &x.E2
            | 4 -> InlineStackUtilities.get &x.E3
            | 5 -> InlineStackUtilities.get &x.E4
            | 6 -> InlineStackUtilities.get &x.E5
            | 7 -> InlineStackUtilities.get &x.E6
            | 8 -> InlineStackUtilities.get &x.E7
            | _ ->
                if isNull (x.Rest :> obj) then failwith "invalid state"
                let h = List.head x.Rest
                x.Rest <- List.tail x.Rest
                h
    end
    
[<NoComparison; NoEquality>]
type InlineStack4<'T> =
    struct
        [<DefaultValue(false)>]
        val mutable public Rest : list<'T>
        [<DefaultValue(false)>]
        val mutable public E0 : 'T
        [<DefaultValue(false)>]
        val mutable public E1 : 'T
        [<DefaultValue(false)>]
        val mutable public E2 : 'T
        [<DefaultValue(false)>]
        val mutable public E3 : 'T
        [<DefaultValue(false)>]
        val mutable public Count : int

        member inline x.Push(value : 'T) =
            match x.Count with
            | 0 -> x.E0 <- value
            | 1 -> x.E1 <- value
            | 2 -> x.E2 <- value
            | 3 -> x.E3 <- value
            | _ -> 
                if isNull (x.Rest :> obj) then x.Rest <- [value]
                else x.Rest <- value :: x.Rest
            x.Count <- x.Count + 1

        member inline x.Pop() =
            let c = x.Count
            if c <= 0 then raise <| System.IndexOutOfRangeException()
            x.Count <- c - 1
            match c with
            | 1 -> InlineStackUtilities.get &x.E0
            | 2 -> InlineStackUtilities.get &x.E1
            | 3 -> InlineStackUtilities.get &x.E2
            | 4 -> InlineStackUtilities.get &x.E3
            | _ ->
                if isNull (x.Rest :> obj) then failwith "invalid state"
                let h = List.head x.Rest
                x.Rest <- List.tail x.Rest
                h
    end
    
[<NoComparison; NoEquality>]
type InlineStack2<'T> =
    struct
        [<DefaultValue(false)>]
        val mutable public Rest : list<'T>
        [<DefaultValue(false)>]
        val mutable public E0 : 'T
        [<DefaultValue(false)>]
        val mutable public E1 : 'T
        [<DefaultValue(false)>]
        val mutable public Count : int

        member inline x.Push(value : 'T) =
            match x.Count with
            | 0 -> x.E0 <- value
            | 1 -> x.E1 <- value
            | _ -> 
                if isNull (x.Rest :> obj) then x.Rest <- [value]
                else x.Rest <- value :: x.Rest
            x.Count <- x.Count + 1

        member inline x.Pop() =
            let c = x.Count
            if c <= 0 then raise <| System.IndexOutOfRangeException()
            x.Count <- c - 1
            match c with
            | 1 -> InlineStackUtilities.get &x.E0
            | 2 -> InlineStackUtilities.get &x.E1
            | _ ->
                if isNull (x.Rest :> obj) then failwith "invalid state"
                let h = List.head x.Rest
                x.Rest <- List.tail x.Rest
                h
    end



type internal MapExtEnumerator0<'Key, 'Value when 'Key : comparison> =
    struct
        val mutable public _Stack : list<MapExtImplementation.MapTree<'Key, 'Value>>
        val mutable public _Root : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _Current : KeyValuePair<'Key, 'Value>
        
        member inline x.MoveNext(top : MapExtImplementation.MapTree<_,_>) =
            let mutable top = top
            let mutable run = true
            while run do
                match top with
                | MapExtImplementation.MapEmpty ->
                    failwith "invalid state"
                | MapExtImplementation.MapOne(k, v) ->
                    x._Current <- KeyValuePair(k, v)
                    run <- false
                | MapExtImplementation.MapNode(k, v, l, r, _, _) ->
                    match l with
                    | MapExtImplementation.MapEmpty ->
                        x._Current <- KeyValuePair(k,v)
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x._Stack <- r :: x._Stack
                        run <- false
                    | MapExtImplementation.MapOne(lk, lv) ->
                        x._Current <- KeyValuePair(lk, lv)
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x._Stack <- r :: x._Stack
                        x._Stack <- MapExtImplementation.MapOne(k, v) :: x._Stack
                        run <- false
                    | l ->
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x._Stack <- r :: x._Stack
                        x._Stack <- MapExtImplementation.MapOne(k, v) :: x._Stack
                        top <- l //x.MoveNext l
            true

        member inline x.MoveNext() =
            match x._Stack with
            | [] ->
                false
            | h :: t ->
                x._Stack <- t
                x.MoveNext h

        member inline x.Current = x._Current

        member inline x.Reset() = 
            x._Current <- Unchecked.defaultof<_>
            x._Stack <- []
            match x._Root with
            | MapExtImplementation.MapEmpty -> ()
            | r -> x._Stack <- r :: x._Stack

        member inline x.Dispose() =
            x._Current <- Unchecked.defaultof<_>
            x._Stack <- []
            x._Root <- MapExtImplementation.MapEmpty
            
        interface IEnumerator with
            member x.MoveNext() = x.MoveNext()
            member x.Reset() = x.Reset()
            member x.Current = x.Current :> obj

        interface IEnumerator<KeyValuePair<'Key, 'Value>> with
            member x.Current = x.Current
            member x.Dispose() = x.Dispose()

        new(map : MapExt<'Key, 'Value>) =
            {
                _Current = Unchecked.defaultof<_>
                _Stack = (match map.Tree with | MapExtImplementation.MapEmpty -> [] | _ -> [map.Tree])
                _Root = map.Tree
            }


    end

type internal MapExtEnumeratorSystemStack<'Key, 'Value when 'Key : comparison> =
    struct
        val mutable public _Stack : System.Collections.Generic.Stack<MapExtImplementation.MapTree<'Key, 'Value>>
        val mutable public _Root : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _Current : KeyValuePair<'Key, 'Value>


        member inline x.MoveNext(top : MapExtImplementation.MapTree<_,_>) =
            let mutable top = top
            let mutable run = true
            while run do
                match top with
                | MapExtImplementation.MapEmpty ->
                    failwith "invalid state"
                | MapExtImplementation.MapOne(k, v) ->
                    x._Current <- KeyValuePair(k, v)
                    run <- false
                | MapExtImplementation.MapNode(k, v, l, r, _, _) ->
                    match l with
                    | MapExtImplementation.MapEmpty ->
                        x._Current <- KeyValuePair(k,v)
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x._Stack.Push r
                        run <- false
                    | MapExtImplementation.MapOne(lk, lv) ->
                        x._Current <- KeyValuePair(lk, lv)
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x._Stack.Push r
                        x._Stack.Push(MapExtImplementation.MapOne(k, v))
                        run <- false
                    | l ->
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x._Stack.Push r
                        x._Stack.Push(MapExtImplementation.MapOne(k, v))
                        top <- l //x.MoveNext l
            true

        member inline x.MoveNext() =
            if x._Stack.Count > 0 then
                let e = x._Stack.Pop()
                x.MoveNext e
            else
                false

        member inline x.Current = x._Current

        member inline x.Reset() = 
            x._Current <- Unchecked.defaultof<_>
            x._Stack.Clear()
            match x._Root with
            | MapExtImplementation.MapEmpty -> ()
            | r -> x._Stack.Push r

        member inline x.Dispose() =
            x._Current <- Unchecked.defaultof<_>
            x._Stack.Clear()
            x._Root <- MapExtImplementation.MapEmpty
            
        interface IEnumerator with
            member x.MoveNext() = x.MoveNext()
            member x.Reset() = x.Reset()
            member x.Current = x.Current :> obj

        interface IEnumerator<KeyValuePair<'Key, 'Value>> with
            member x.Current = x.Current
            member x.Dispose() = x.Dispose()

        new(map : MapExt<'Key, 'Value>) =
            let stack = System.Collections.Generic.Stack()
            match map.Tree with
            | MapExtImplementation.MapEmpty -> ()
            | r -> stack.Push r
            {
                _Current = Unchecked.defaultof<_>
                _Stack = stack
                _Root = map.Tree
            }


    end

type internal MapExtEnumerator2<'Key, 'Value when 'Key : comparison> =
    struct
        val mutable public _Stack : InlineStack2<MapExtImplementation.MapTree<'Key, 'Value>>
        val mutable public _Root : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _Current : KeyValuePair<'Key, 'Value>


        member inline x.MoveNext(top : MapExtImplementation.MapTree<_,_>) =
            let mutable top = top
            let mutable run = true
            while run do
                match top with
                | MapExtImplementation.MapEmpty ->
                    failwith "invalid state"
                | MapExtImplementation.MapOne(k, v) ->
                    x._Current <- KeyValuePair(k, v)
                    run <- false
                | MapExtImplementation.MapNode(k, v, l, r, _, _) ->
                    match l with
                    | MapExtImplementation.MapEmpty ->
                        x._Current <- KeyValuePair(k,v)
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x._Stack.Push r
                        run <- false
                    | MapExtImplementation.MapOne(lk, lv) ->
                        x._Current <- KeyValuePair(lk, lv)
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x._Stack.Push r
                        x._Stack.Push(MapExtImplementation.MapOne(k, v))
                        run <- false
                    | l ->
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x._Stack.Push r
                        x._Stack.Push(MapExtImplementation.MapOne(k, v))
                        top <- l //x.MoveNext l
            true

        member inline x.MoveNext() =
            if x._Stack.Count > 0 then
                let e = x._Stack.Pop()
                x.MoveNext e
            else
                false

        member inline x.Current = x._Current

        member inline x.Reset() = 
            x._Current <- Unchecked.defaultof<_>
            x._Stack <- InlineStack2()
            match x._Root with
            | MapExtImplementation.MapEmpty -> ()
            | r -> x._Stack.Push r

        member inline x.Dispose() =
            x._Current <- Unchecked.defaultof<_>
            x._Stack <- InlineStack2()
            x._Root <- MapExtImplementation.MapEmpty
            
        interface IEnumerator with
            member x.MoveNext() = x.MoveNext()
            member x.Reset() = x.Reset()
            member x.Current = x.Current :> obj

        interface IEnumerator<KeyValuePair<'Key, 'Value>> with
            member x.Current = x.Current
            member x.Dispose() = x.Dispose()

        new(map : MapExt<'Key, 'Value>) =
            let mutable stack = Unchecked.defaultof<InlineStack2<_>>
            match map.Tree with
            | MapExtImplementation.MapEmpty -> ()
            | r -> stack.Push r
            {
                _Current = Unchecked.defaultof<_>
                _Stack = stack
                _Root = map.Tree
            }


    end

    
type internal MapExtEnumerator4<'Key, 'Value when 'Key : comparison> =
    struct
        val mutable public _Stack : InlineStack4<MapExtImplementation.MapTree<'Key, 'Value>>
        val mutable public _Root : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _Current : KeyValuePair<'Key, 'Value>

        
        

        member inline x.MoveNext(top : MapExtImplementation.MapTree<_,_>) =
            let mutable top = top
            let mutable run = true
            while run do
                match top with
                | MapExtImplementation.MapEmpty ->
                    failwith "invalid state"
                | MapExtImplementation.MapOne(k, v) ->
                    x._Current <- KeyValuePair(k, v)
                    run <- false
                | MapExtImplementation.MapNode(k, v, l, r, _, _) ->
                    match l with
                    | MapExtImplementation.MapEmpty ->
                        x._Current <- KeyValuePair(k,v)
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x._Stack.Push r
                        run <- false
                    | MapExtImplementation.MapOne(lk, lv) ->
                        x._Current <- KeyValuePair(lk, lv)
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x._Stack.Push r
                        x._Stack.Push(MapExtImplementation.MapOne(k, v))
                        run <- false
                    | l ->
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x._Stack.Push r
                        x._Stack.Push(MapExtImplementation.MapOne(k, v))
                        top <- l //x.MoveNext l
            true

        member inline x.MoveNext() =
            if x._Stack.Count > 0 then
                let e = x._Stack.Pop()
                x.MoveNext e
            else
                false

        member inline x.Current = x._Current

        member inline x.Reset() = 
            x._Current <- Unchecked.defaultof<_>
            x._Stack <- InlineStack4()
            match x._Root with
            | MapExtImplementation.MapEmpty -> ()
            | r -> x._Stack.Push r

        member inline x.Dispose() =
            x._Current <- Unchecked.defaultof<_>
            x._Stack <- InlineStack4()
            x._Root <- MapExtImplementation.MapEmpty
            
        interface IEnumerator with
            member x.MoveNext() = x.MoveNext()
            member x.Reset() = x.Reset()
            member x.Current = x.Current :> obj

        interface IEnumerator<KeyValuePair<'Key, 'Value>> with
            member x.Current = x.Current
            member x.Dispose() = x.Dispose()

        new(map : MapExt<'Key, 'Value>) =
            let mutable stack = Unchecked.defaultof<InlineStack4<_>>
            match map.Tree with
            | MapExtImplementation.MapEmpty -> ()
            | r -> stack.Push r
            {
                _Current = Unchecked.defaultof<_>
                _Stack = stack
                _Root = map.Tree
            }

    end
    
    
type internal MapExtEnumerator8<'Key, 'Value when 'Key : comparison> =
    struct
        val mutable public _Stack : InlineStack8<MapExtImplementation.MapTree<'Key, 'Value>>
        val mutable public _Root : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _Current : KeyValuePair<'Key, 'Value>


        

        member inline x.MoveNext(top : MapExtImplementation.MapTree<_,_>) =
            let mutable top = top
            let mutable run = true
            while run do
                match top with
                | MapExtImplementation.MapEmpty ->
                    failwith "invalid state"
                | MapExtImplementation.MapOne(k, v) ->
                    x._Current <- KeyValuePair(k, v)
                    run <- false
                | MapExtImplementation.MapNode(k, v, l, r, _, _) ->
                    match l with
                    | MapExtImplementation.MapEmpty ->
                        x._Current <- KeyValuePair(k,v)
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x._Stack.Push r
                        run <- false
                    | MapExtImplementation.MapOne(lk, lv) ->
                        x._Current <- KeyValuePair(lk, lv)
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x._Stack.Push r
                        x._Stack.Push(MapExtImplementation.MapOne(k, v))
                        run <- false
                    | l ->
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x._Stack.Push r
                        x._Stack.Push(MapExtImplementation.MapOne(k, v))
                        top <- l //x.MoveNext l
            true

        member inline x.MoveNext() =
            if x._Stack.Count > 0 then
                let e = x._Stack.Pop()
                x.MoveNext e
            else
                false

        member inline x.Current = x._Current

        member inline x.Reset() = 
            x._Current <- Unchecked.defaultof<_>
            x._Stack <- InlineStack8()
            match x._Root with
            | MapExtImplementation.MapEmpty -> ()
            | r -> x._Stack.Push r

        member inline x.Dispose() =
            x._Current <- Unchecked.defaultof<_>
            x._Stack <- InlineStack8()
            x._Root <- MapExtImplementation.MapEmpty
            
        interface IEnumerator with
            member x.MoveNext() = x.MoveNext()
            member x.Reset() = x.Reset()
            member x.Current = x.Current :> obj

        interface IEnumerator<KeyValuePair<'Key, 'Value>> with
            member x.Current = x.Current
            member x.Dispose() = x.Dispose()

        new(map : MapExt<'Key, 'Value>) =
            let mutable stack = Unchecked.defaultof<InlineStack8<_>>
            match map.Tree with
            | MapExtImplementation.MapEmpty -> ()
            | r -> stack.Push r
            {
                _Current = Unchecked.defaultof<_>
                _Stack = stack
                _Root = map.Tree
            }

    end
    
    
    
type internal MapExtEnumerator12<'Key, 'Value when 'Key : comparison> =
    struct
        val mutable public _Stack : InlineStack12<MapExtImplementation.MapTree<'Key, 'Value>>
        val mutable public _Root : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _Current : KeyValuePair<'Key, 'Value>



        

        member inline x.MoveNext(top : MapExtImplementation.MapTree<_,_>) =
            let mutable top = top
            let mutable run = true
            while run do
                match top with
                | MapExtImplementation.MapEmpty ->
                    failwith "invalid state"
                | MapExtImplementation.MapOne(k, v) ->
                    x._Current <- KeyValuePair(k, v)
                    run <- false
                | MapExtImplementation.MapNode(k, v, l, r, _, _) ->
                    match l with
                    | MapExtImplementation.MapEmpty ->
                        x._Current <- KeyValuePair(k,v)
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x._Stack.Push r
                        run <- false
                    | MapExtImplementation.MapOne(lk, lv) ->
                        x._Current <- KeyValuePair(lk, lv)
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x._Stack.Push r
                        x._Stack.Push(MapExtImplementation.MapOne(k, v))
                        run <- false
                    | l ->
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x._Stack.Push r
                        x._Stack.Push(MapExtImplementation.MapOne(k, v))
                        top <- l //x.MoveNext l
            true

        member inline x.MoveNext() =
            if x._Stack.Count > 0 then
                let e = x._Stack.Pop()
                x.MoveNext e
            else
                false

        member inline x.Current = x._Current

        member inline x.Reset() = 
            x._Current <- Unchecked.defaultof<_>
            x._Stack <- InlineStack12()
            match x._Root with
            | MapExtImplementation.MapEmpty -> ()
            | r -> x._Stack.Push r

        member inline x.Dispose() =
            x._Current <- Unchecked.defaultof<_>
            x._Stack <- InlineStack12()
            x._Root <- MapExtImplementation.MapEmpty
            
        interface IEnumerator with
            member x.MoveNext() = x.MoveNext()
            member x.Reset() = x.Reset()
            member x.Current = x.Current :> obj

        interface IEnumerator<KeyValuePair<'Key, 'Value>> with
            member x.Current = x.Current
            member x.Dispose() = x.Dispose()

        new(map : MapExt<'Key, 'Value>) =
            let mutable stack = Unchecked.defaultof<InlineStack12<_>>
            match map.Tree with
            | MapExtImplementation.MapEmpty -> ()
            | r -> stack.Push r
            {
                _Current = Unchecked.defaultof<_>
                _Stack = stack
                _Root = map.Tree
            }

    end
        
    
type internal MapExtEnumerator16<'Key, 'Value when 'Key : comparison> =
    struct
        val mutable public _Stack : InlineStack16<MapExtImplementation.MapTree<'Key, 'Value>>
        val mutable public _Root : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _Current : KeyValuePair<'Key, 'Value>



        

        member inline x.MoveNext(top : MapExtImplementation.MapTree<_,_>) =
            let mutable top = top
            let mutable run = true
            while run do
                match top with
                | MapExtImplementation.MapEmpty ->
                    failwith "invalid state"
                | MapExtImplementation.MapOne(k, v) ->
                    x._Current <- KeyValuePair(k, v)
                    run <- false
                | MapExtImplementation.MapNode(k, v, l, r, _, _) ->
                    match l with
                    | MapExtImplementation.MapEmpty ->
                        x._Current <- KeyValuePair(k,v)
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x._Stack.Push r
                        run <- false
                    | MapExtImplementation.MapOne(lk, lv) ->
                        x._Current <- KeyValuePair(lk, lv)
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x._Stack.Push r
                        x._Stack.Push(MapExtImplementation.MapOne(k, v))
                        run <- false
                    | l ->
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x._Stack.Push r
                        x._Stack.Push(MapExtImplementation.MapOne(k, v))
                        top <- l //x.MoveNext l
            true

        member inline x.MoveNext() =
            if x._Stack.Count > 0 then
                let e = x._Stack.Pop()
                x.MoveNext e
            else
                false

        member inline x.Current = x._Current

        member inline x.Reset() = 
            x._Current <- Unchecked.defaultof<_>
            x._Stack <- InlineStack16()
            match x._Root with
            | MapExtImplementation.MapEmpty -> ()
            | r -> x._Stack.Push r

        member inline x.Dispose() =
            x._Current <- Unchecked.defaultof<_>
            x._Stack <- InlineStack16()
            x._Root <- MapExtImplementation.MapEmpty
            
        interface IEnumerator with
            member x.MoveNext() = x.MoveNext()
            member x.Reset() = x.Reset()
            member x.Current = x.Current :> obj

        interface IEnumerator<KeyValuePair<'Key, 'Value>> with
            member x.Current = x.Current
            member x.Dispose() = x.Dispose()

        new(map : MapExt<'Key, 'Value>) =
            let mutable stack = Unchecked.defaultof<InlineStack16<_>>
            match map.Tree with
            | MapExtImplementation.MapEmpty -> ()
            | r -> stack.Push r
            {
                _Current = Unchecked.defaultof<_>
                _Stack = stack
                _Root = map.Tree
            }

    end



type internal MapExtEnumerator2Opt<'Key, 'Value when 'Key : comparison> =
    struct
        val mutable public _E0 : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _E1 : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _Rest : list<MapExtImplementation.MapTree<'Key, 'Value>>
        val mutable public _Root : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _Current : KeyValuePair<'Key, 'Value>
        val mutable public _Count : int

        member inline x.Push(value : MapExtImplementation.MapTree<_,_>) =
            match x._Count with
            | 0 -> x._E0 <- value
            | 1 -> x._E1 <- value
            | _ -> x._Rest <- value :: x._Rest
            x._Count <- x._Count + 1

        member inline x.Pop() =
            let c = x._Count
            if c <= 0 then raise <| System.IndexOutOfRangeException()
            x._Count <- c - 1
            match c with
            | 1 -> InlineStackUtilities.get &x._E0
            | 2 -> InlineStackUtilities.get &x._E1
            | _ ->
                let h = List.head x._Rest
                x._Rest <- List.tail x._Rest
                h

        member inline x.MoveNext(top : MapExtImplementation.MapTree<_,_>) =
            let mutable top = top
            let mutable run = true
            while run do
                match top with
                | MapExtImplementation.MapEmpty ->
                    failwith "invalid state"
                | MapExtImplementation.MapOne(k, v) ->
                    x._Current <- KeyValuePair(k, v)
                    run <- false
                | MapExtImplementation.MapNode(k, v, l, r, _, _) ->
                    match l with
                    | MapExtImplementation.MapEmpty ->
                        x._Current <- KeyValuePair(k,v)
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x.Push r
                        run <- false
                    | MapExtImplementation.MapOne(lk, lv) ->
                        x._Current <- KeyValuePair(lk, lv)
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x.Push r
                        x.Push(MapExtImplementation.MapOne(k, v))
                        run <- false
                    | l ->
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x.Push r
                        x.Push(MapExtImplementation.MapOne(k, v))
                        top <- l //x.MoveNext l
            true

        member inline x.MoveNext() =
            if x._Count > 0 then
                let fst = x.Pop()
                x.MoveNext fst
            else
                false

        member inline x.Current = x._Current

        member inline x.Reset() = 
            x._Current <- Unchecked.defaultof<_>
            x._Rest <- []
            x._E1 <- MapExtImplementation.MapEmpty
            x._Count <- 0
            match x._Root with
            | MapExtImplementation.MapEmpty -> 
                x._E0 <- MapExtImplementation.MapEmpty
                x._Count <- 0
            | r ->
                x._E0 <- r
                x._Count <- 1

        member inline x.Dispose() =
            x._Current <- Unchecked.defaultof<_>
            x._Rest <- []
            x._E0 <- MapExtImplementation.MapEmpty
            x._E1 <- MapExtImplementation.MapEmpty
            x._Count <- 0
            
        interface IEnumerator with
            member x.MoveNext() = x.MoveNext()
            member x.Reset() = x.Reset()
            member x.Current = x.Current :> obj

        interface IEnumerator<KeyValuePair<'Key, 'Value>> with
            member x.Current = x.Current
            member x.Dispose() = x.Dispose()

        new(map : MapExt<'Key, 'Value>) =
            {
                _Current = Unchecked.defaultof<_>
                _E0 = map.Tree
                _E1 = MapExtImplementation.MapEmpty
                _Rest = []
                _Root = map.Tree
                _Count = 
                    match map.Tree with
                    | MapExtImplementation.MapEmpty -> 0
                    | _ -> 1
            }


    end


type internal MapExtEnumerator4Opt<'Key, 'Value when 'Key : comparison> =
    struct
        val mutable public _E0 : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _E1 : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _E2 : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _E3 : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _Rest : list<MapExtImplementation.MapTree<'Key, 'Value>>
        val mutable public _Root : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _Current : KeyValuePair<'Key, 'Value>
        val mutable public _Count : int

        member inline x.Push(value : MapExtImplementation.MapTree<_,_>) =
            match x._Count with
            | 0 -> x._E0 <- value
            | 1 -> x._E1 <- value
            | 2 -> x._E2 <- value
            | 3 -> x._E3 <- value
            | _ -> x._Rest <- value :: x._Rest
            x._Count <- x._Count + 1

        member inline x.Pop() =
            let c = x._Count
            if c <= 0 then raise <| System.IndexOutOfRangeException()
            x._Count <- c - 1
            match c with
            | 1 -> InlineStackUtilities.get &x._E0
            | 2 -> InlineStackUtilities.get &x._E1
            | 3 -> InlineStackUtilities.get &x._E2
            | 4 -> InlineStackUtilities.get &x._E3
            | _ ->
                let h = List.head x._Rest
                x._Rest <- List.tail x._Rest
                h

        member inline x.MoveNext(top : MapExtImplementation.MapTree<_,_>) =
            let mutable top = top
            let mutable run = true
            while run do
                match top with
                | MapExtImplementation.MapEmpty ->
                    failwith "invalid state"
                | MapExtImplementation.MapOne(k, v) ->
                    x._Current <- KeyValuePair(k, v)
                    run <- false
                | MapExtImplementation.MapNode(k, v, l, r, _, _) ->
                    match l with
                    | MapExtImplementation.MapEmpty ->
                        x._Current <- KeyValuePair(k,v)
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x.Push r
                        run <- false
                    | MapExtImplementation.MapOne(lk, lv) ->
                        x._Current <- KeyValuePair(lk, lv)
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x.Push r
                        x.Push(MapExtImplementation.MapOne(k, v))
                        run <- false
                    | l ->
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x.Push r
                        x.Push(MapExtImplementation.MapOne(k, v))
                        top <- l //x.MoveNext l
            true

        member inline x.MoveNext() =
            if x._Count > 0 then
                let fst = x.Pop()
                x.MoveNext fst
            else
                false

        member inline x.Current = x._Current

        member inline x.Reset() = 
            x._Current <- Unchecked.defaultof<_>
            x._Rest <- []
            x._E1 <- MapExtImplementation.MapEmpty
            x._Count <- 0
            match x._Root with
            | MapExtImplementation.MapEmpty -> 
                x._E0 <- MapExtImplementation.MapEmpty
                x._Count <- 0
            | r ->
                x._E0 <- r
                x._Count <- 1

        member inline x.Dispose() =
            x._Current <- Unchecked.defaultof<_>
            x._Rest <- []
            x._E0 <- MapExtImplementation.MapEmpty
            x._E1 <- MapExtImplementation.MapEmpty
            x._Count <- 0
            
        interface IEnumerator with
            member x.MoveNext() = x.MoveNext()
            member x.Reset() = x.Reset()
            member x.Current = x.Current :> obj

        interface IEnumerator<KeyValuePair<'Key, 'Value>> with
            member x.Current = x.Current
            member x.Dispose() = x.Dispose()

        new(map : MapExt<'Key, 'Value>) =
            {
                _Current = Unchecked.defaultof<_>
                _E0 = map.Tree
                _E1 = MapExtImplementation.MapEmpty
                _E2 = MapExtImplementation.MapEmpty
                _E3 = MapExtImplementation.MapEmpty
                _Rest = []
                _Root = map.Tree
                _Count = 
                    match map.Tree with
                    | MapExtImplementation.MapEmpty -> 0
                    | _ -> 1
            }


    end

type internal MapExtEnumerator6Opt<'Key, 'Value when 'Key : comparison> =
    struct
        val mutable public _E0 : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _E1 : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _E2 : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _E3 : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _E4 : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _E5 : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _Rest : list<MapExtImplementation.MapTree<'Key, 'Value>>
        val mutable public _Root : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _Current : KeyValuePair<'Key, 'Value>
        val mutable public _Count : int

        member inline x.Push(value : MapExtImplementation.MapTree<_,_>) =
            match x._Count with
            | 0 -> x._E0 <- value
            | 1 -> x._E1 <- value
            | 2 -> x._E2 <- value
            | 3 -> x._E3 <- value
            | 4 -> x._E4 <- value
            | 5 -> x._E5 <- value
            | _ -> x._Rest <- value :: x._Rest
            x._Count <- x._Count + 1

        member inline x.Pop() =
            let c = x._Count
            if c <= 0 then raise <| System.IndexOutOfRangeException()
            x._Count <- c - 1
            match c with
            | 1 -> InlineStackUtilities.get &x._E0
            | 2 -> InlineStackUtilities.get &x._E1
            | 3 -> InlineStackUtilities.get &x._E2
            | 4 -> InlineStackUtilities.get &x._E3
            | 5 -> InlineStackUtilities.get &x._E4
            | 6 -> InlineStackUtilities.get &x._E5
            | _ ->
                let h = List.head x._Rest
                x._Rest <- List.tail x._Rest
                h

        member inline x.MoveNext(top : MapExtImplementation.MapTree<_,_>) =
            let mutable top = top
            let mutable run = true
            while run do
                match top with
                | MapExtImplementation.MapEmpty ->
                    failwith "invalid state"
                | MapExtImplementation.MapOne(k, v) ->
                    x._Current <- KeyValuePair(k, v)
                    run <- false
                | MapExtImplementation.MapNode(k, v, l, r, _, _) ->
                    match l with
                    | MapExtImplementation.MapEmpty ->
                        x._Current <- KeyValuePair(k,v)
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x.Push r
                        run <- false
                    | MapExtImplementation.MapOne(lk, lv) ->
                        x._Current <- KeyValuePair(lk, lv)
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x.Push r
                        x.Push(MapExtImplementation.MapOne(k, v))
                        run <- false
                    | l ->
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x.Push r
                        x.Push(MapExtImplementation.MapOne(k, v))
                        top <- l //x.MoveNext l
            true

        member inline x.MoveNext() =
            if x._Count > 0 then
                let fst = x.Pop()
                x.MoveNext fst
            else
                false

        member inline x.Current = x._Current

        member inline x.Reset() = 
            x._Current <- Unchecked.defaultof<_>
            x._Rest <- []
            x._E1 <- MapExtImplementation.MapEmpty
            x._Count <- 0
            match x._Root with
            | MapExtImplementation.MapEmpty -> 
                x._E0 <- MapExtImplementation.MapEmpty
                x._Count <- 0
            | r ->
                x._E0 <- r
                x._Count <- 1

        member inline x.Dispose() =
            x._Current <- Unchecked.defaultof<_>
            x._Rest <- []
            x._E0 <- MapExtImplementation.MapEmpty
            x._E1 <- MapExtImplementation.MapEmpty
            x._Count <- 0
            
        interface IEnumerator with
            member x.MoveNext() = x.MoveNext()
            member x.Reset() = x.Reset()
            member x.Current = x.Current :> obj

        interface IEnumerator<KeyValuePair<'Key, 'Value>> with
            member x.Current = x.Current
            member x.Dispose() = x.Dispose()

        new(map : MapExt<'Key, 'Value>) =
            {
                _Current = Unchecked.defaultof<_>
                _E0 = map.Tree
                _E1 = MapExtImplementation.MapEmpty
                _E2 = MapExtImplementation.MapEmpty
                _E3 = MapExtImplementation.MapEmpty
                _E4 = MapExtImplementation.MapEmpty
                _E5 = MapExtImplementation.MapEmpty
                _Rest = []
                _Root = map.Tree
                _Count = 
                    match map.Tree with
                    | MapExtImplementation.MapEmpty -> 0
                    | _ -> 1
            }


    end

type internal MapExtEnumerator8Opt<'Key, 'Value when 'Key : comparison> =
    struct
        val mutable public _E0 : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _E1 : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _E2 : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _E3 : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _E4 : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _E5 : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _E6 : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _E7 : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _Rest : list<MapExtImplementation.MapTree<'Key, 'Value>>
        val mutable public _Root : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _Current : KeyValuePair<'Key, 'Value>
        val mutable public _Count : int

        member inline x.Push(value : MapExtImplementation.MapTree<_,_>) =
            match x._Count with
            | 0 -> x._E0 <- value
            | 1 -> x._E1 <- value
            | 2 -> x._E2 <- value
            | 3 -> x._E3 <- value
            | 4 -> x._E4 <- value
            | 5 -> x._E5 <- value
            | 6 -> x._E6 <- value
            | 7 -> x._E7 <- value
            | _ -> x._Rest <- value :: x._Rest
            x._Count <- x._Count + 1

        member inline x.Pop() =
            let c = x._Count
            if c <= 0 then raise <| System.IndexOutOfRangeException()
            x._Count <- c - 1
            match c with
            | 1 -> InlineStackUtilities.get &x._E0
            | 2 -> InlineStackUtilities.get &x._E1
            | 3 -> InlineStackUtilities.get &x._E2
            | 4 -> InlineStackUtilities.get &x._E3
            | 5 -> InlineStackUtilities.get &x._E4
            | 6 -> InlineStackUtilities.get &x._E5
            | 7 -> InlineStackUtilities.get &x._E6
            | 8 -> InlineStackUtilities.get &x._E7
            | _ ->
                let h = List.head x._Rest
                x._Rest <- List.tail x._Rest
                h

        member inline x.MoveNext(top : MapExtImplementation.MapTree<_,_>) =
            let mutable top = top
            let mutable run = true
            while run do
                match top with
                | MapExtImplementation.MapEmpty ->
                    failwith "invalid state"
                | MapExtImplementation.MapOne(k, v) ->
                    x._Current <- KeyValuePair(k, v)
                    run <- false
                | MapExtImplementation.MapNode(k, v, l, r, _, _) ->
                    match l with
                    | MapExtImplementation.MapEmpty ->
                        x._Current <- KeyValuePair(k,v)
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x.Push r
                        run <- false
                    | MapExtImplementation.MapOne(lk, lv) ->
                        x._Current <- KeyValuePair(lk, lv)
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x.Push r
                        x.Push(MapExtImplementation.MapOne(k, v))
                        run <- false
                    | l ->
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x.Push r
                        x.Push(MapExtImplementation.MapOne(k, v))
                        top <- l //x.MoveNext l
            true

        member inline x.MoveNext() =
            if x._Count > 0 then
                let fst = x.Pop()
                x.MoveNext fst
            else
                false

        member inline x.Current = x._Current

        member inline x.Reset() = 
            x._Current <- Unchecked.defaultof<_>
            x._Rest <- []
            x._E1 <- MapExtImplementation.MapEmpty
            x._Count <- 0
            match x._Root with
            | MapExtImplementation.MapEmpty -> 
                x._E0 <- MapExtImplementation.MapEmpty
                x._Count <- 0
            | r ->
                x._E0 <- r
                x._Count <- 1

        member inline x.Dispose() =
            x._Current <- Unchecked.defaultof<_>
            x._Rest <- []
            x._E0 <- MapExtImplementation.MapEmpty
            x._E1 <- MapExtImplementation.MapEmpty
            x._Count <- 0
            
        interface IEnumerator with
            member x.MoveNext() = x.MoveNext()
            member x.Reset() = x.Reset()
            member x.Current = x.Current :> obj

        interface IEnumerator<KeyValuePair<'Key, 'Value>> with
            member x.Current = x.Current
            member x.Dispose() = x.Dispose()

        new(map : MapExt<'Key, 'Value>) =
            {
                _Current = Unchecked.defaultof<_>
                _E0 = map.Tree
                _E1 = MapExtImplementation.MapEmpty
                _E2 = MapExtImplementation.MapEmpty
                _E3 = MapExtImplementation.MapEmpty
                _E4 = MapExtImplementation.MapEmpty
                _E5 = MapExtImplementation.MapEmpty
                _E6 = MapExtImplementation.MapEmpty
                _E7 = MapExtImplementation.MapEmpty
                _Rest = []
                _Root = map.Tree
                _Count = 
                    match map.Tree with
                    | MapExtImplementation.MapEmpty -> 0
                    | _ -> 1
            }


    end

type internal MapExtEnumeratorArr<'Key, 'Value when 'Key : comparison> =
    struct
        val mutable public _Pending : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _Array : KeyValuePair<'Key, 'Value>[]
        val mutable public _ArrayCount : int
        val mutable public _Index : int

        member x.Flatten(node : MapExtImplementation.MapTree<'Key, 'Value>) =
            match node with
            | MapExtImplementation.MapEmpty ->()
            | MapExtImplementation.MapOne(k,v) -> 
                x._Array.[x._ArrayCount] <- KeyValuePair(k,v)
                x._ArrayCount <- x._ArrayCount + 1
                x._Pending <- MapExtImplementation.MapEmpty
            | MapExtImplementation.MapNode(k, v, l, r, _, _) ->
                x.Flatten l
                x._Array.[x._ArrayCount] <- KeyValuePair(k,v)
                x._ArrayCount <- x._ArrayCount + 1
                x.Flatten r

        member x.MoveNext() =
            x._Index <- x._Index + 1
            if x._Index < x._ArrayCount then
                true
            else
                match x._Pending with
                | MapExtImplementation.MapEmpty ->
                    false
                | MapExtImplementation.MapOne(k,v) ->
                    x._Array.[x._ArrayCount] <- KeyValuePair(k, v)
                    x._ArrayCount <- x._ArrayCount + 1
                    x._Pending <- MapExtImplementation.MapEmpty
                    true
                | MapExtImplementation.MapNode(k,v,l,r,_,_) ->
                    x.Flatten l
                    x._Array.[x._ArrayCount] <- KeyValuePair(k, v)
                    x._ArrayCount <- x._ArrayCount + 1
                    x._Pending <- r
                    true

        member x.Reset() =
            x._Index <- -1

        member x.Dispose() =
            x._Pending <- MapExtImplementation.MapEmpty
            x._Array <- null
            x._ArrayCount <- 0
            x._Index <- -1

        member x.Current = x._Array.[x._Index]
        
        interface IEnumerator with
            member x.MoveNext() = x.MoveNext()
            member x.Reset() = x.Reset()
            member x.Current = x.Current :> obj

        interface IEnumerator<KeyValuePair<'Key, 'Value>> with
            member x.Current = x.Current
            member x.Dispose() = x.Dispose()

        new(map : MapExt<'Key, 'Value>) =
            {
                _Array = Array.zeroCreate (MapExt.count map)
                _ArrayCount = 0
                _Index = -1
                _Pending = map.Tree
            }

    end

type internal Entry<'Key, 'Value when 'Key : comparison> =
    struct
        val mutable public Tree : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public Item : KeyValuePair<'Key, 'Value>

        new(k, v) = { Tree = MapExtImplementation.MapEmpty; Item = KeyValuePair(k,v) }
        new(t) = { Tree = t; Item = Unchecked.defaultof<_> }

    end
    

[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type internal KeyedList<'a> =
    | Cons of value : 'a * tail : KeyedList<'a> * key : int
    | Nil

type private MapExtEnumeratorLazyArray<'Key, 'Value when 'Key : comparison> =
    struct
        val mutable public Root : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public Stack : KeyedList<MapExtImplementation.MapTree<'Key, 'Value>>
        val mutable public Array : KeyValuePair<'Key, 'Value>[]
        val mutable public Index : int

        member x.Flatten(start : int, node : MapExtImplementation.MapTree<_,_>) =
            match node with
            | MapExtImplementation.MapEmpty ->
                ()
            | MapExtImplementation.MapOne(k, v) ->
                x.Array.[start] <- KeyValuePair(k,v)
            | MapExtImplementation.MapNode(k, v, l, r, _, _) ->
                let sl = InlineStackUtilities.size l
                let si = start + sl
                x.Flatten(start, l)
                x.Array.[si] <- KeyValuePair(k,v)
                x.Flatten(si + 1, r)

        member x.MoveNext (start : int, node : MapExtImplementation.MapTree<_,_>) =
            match node with
            | MapExtImplementation.MapEmpty ->
                ()
            | MapExtImplementation.MapOne(k, v) ->
                x.Array.[start] <- KeyValuePair(k,v)
            | MapExtImplementation.MapNode(k, v, l, r, _, _) ->
                let sl = InlineStackUtilities.size l
                x.Array.[start + sl] <- KeyValuePair(k,v)
                match r with
                | MapExtImplementation.MapEmpty -> ()
                | r -> x.Stack <- Cons(r, x.Stack, start + 1 + sl)
                x.MoveNext(start, l)


        member inline x.MoveNext() =
            x.Index <- x.Index + 1
            if x.Index >= x.Array.Length then
                false
            else
                match x.Stack with
                | Cons(h, t, i) ->
                    if i = x.Index then
                        x.Stack <- t
                        match h with
                        | MapExtImplementation.MapEmpty -> 
                            failwith "bad"

                        | MapExtImplementation.MapOne(k,v) ->
                            x.Array.[i] <- KeyValuePair(k,v)

                        | MapExtImplementation.MapNode(k,v,l,r,_,c) ->
                            if c <= 16 then
                                x.Flatten(i, h)
                            else
                                match l with
                                | MapExtImplementation.MapEmpty  ->
                                    x.Array.[i] <- KeyValuePair(k,v)
                                | MapExtImplementation.MapOne(lk, lv) ->
                                    x.Array.[i] <- KeyValuePair(lk,lv)
                                    x.Array.[i+1] <- KeyValuePair(k,v)
                                    match r with
                                    | MapExtImplementation.MapEmpty -> ()
                                    | r -> x.Stack <- Cons(r, x.Stack, i + 2)
                                | MapExtImplementation.MapNode(_,_,_,_,_,lc) ->
                                    x.Array.[i + lc] <- KeyValuePair(k,v)
                                    match r with
                                    | MapExtImplementation.MapEmpty -> ()
                                    | r -> x.Stack <- Cons(r, x.Stack, i + lc + 1)
                                    x.MoveNext(i, l)

                | Nil ->
                    ()
           
                true
           
        member inline x.Reset() =
            x.Stack <- (match x.Root with | MapExtImplementation.MapEmpty -> Nil | r -> Cons(r, Nil, 0))
            x.Index <- -1
            
           
        member inline x.Dispose() =
            x.Root <- Unchecked.defaultof<_>
            x.Array <- null
            x.Stack <- Nil
            x.Index <- -1

        member inline x.Current = x.Array.[x.Index]
        
        interface IEnumerator with
            member x.MoveNext() = x.MoveNext()
            member x.Reset() = x.Reset()
            member x.Current = x.Current :> obj

        interface IEnumerator<KeyValuePair<'Key, 'Value>> with
            member x.Current = x.Current
            member x.Dispose() = x.Dispose()

        static member Flatten(m : MapExtImplementation.MapTree<'Key, 'Value>, dst : KeyValuePair<'Key, 'Value>[], offset : byref<int>) =
            match m with
            | MapExtImplementation.MapEmpty ->
                ()
            | MapExtImplementation.MapOne(k, v) ->
                dst.[offset] <- KeyValuePair(k, v)
                offset <- offset + 1

            | MapExtImplementation.MapNode(k, v, l, r, _, _) ->
                MapExtEnumeratorLazyArray<'Key, 'Value>.Flatten(l, dst, &offset)
                
                dst.[offset] <- KeyValuePair(k, v)
                offset <- offset + 1
                
                MapExtEnumeratorLazyArray<'Key, 'Value>.Flatten(r, dst, &offset)


        new(map : MapExt<'Key, 'Value>) =

            let root = map.Tree
            match root with
            | MapExtImplementation.MapEmpty ->
                {
                    Root = root
                    Index = -1
                    Array = [||]
                    Stack = Nil
                }
            | MapExtImplementation.MapOne(k,v) ->
                {
                    Root = root
                    Index = -1
                    Array = [|KeyValuePair(k, v)|]
                    Stack = Nil
                }
            | MapExtImplementation.MapNode(_,_,_,_,_,cnt) ->
                let s = Array.zeroCreate cnt
                if cnt <= 16 then
                    let mutable i = 0
                    MapExtEnumeratorLazyArray<'Key, 'Value>.Flatten(map.Tree, s, &i)
                    {
                        Root = map.Tree
                        Index = -1
                        Array = s
                        Stack = Nil
                    }
                else
                    let stack = 
                        match map.Tree with
                        | MapExtImplementation.MapEmpty -> Nil
                        | r -> Cons(r, Nil, 0)

                    {
                        Root = map.Tree
                        Index = -1
                        Array = s
                        Stack = stack
                    }


    end

type internal MapExtEnumeratorNew<'Key, 'Value when 'Key : comparison> =
    struct
        val mutable public _Root : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public _Stack : Entry<'Key, 'Value>[]
        val mutable public _StackCount : int
        val mutable public _Current : KeyValuePair<'Key, 'Value>
        
        member inline x.Push(e : Entry<_,_>) =
            x._Stack.[x._StackCount] <- e
            x._StackCount <- x._StackCount + 1
            
        member inline x.Pop() =
            x._StackCount <- x._StackCount - 1
            InlineStackUtilities.get &x._Stack.[x._StackCount]

        member inline x.MoveNext(top : MapExtImplementation.MapTree<_,_>) =
            let mutable top = top
            let mutable run = true
            while run do
                match top with
                | MapExtImplementation.MapEmpty ->
                    failwith "invalid state"
                | MapExtImplementation.MapOne(k, v) ->
                    x._Current <- KeyValuePair(k, v)
                    run <- false
                | MapExtImplementation.MapNode(k, v, l, r, _, _) ->
                    match l with
                    | MapExtImplementation.MapEmpty ->
                        x._Current <- KeyValuePair(k,v)
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x.Push(Entry r)
                        run <- false
                    | MapExtImplementation.MapOne(lk, lv) ->
                        x._Current <- KeyValuePair(lk, lv)
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x.Push(Entry r)
                        x.Push(Entry(k, v))
                        run <- false
                    | l ->
                        match r with
                        | MapExtImplementation.MapEmpty -> ()
                        | _ -> x.Push(Entry r)
                        x.Push(Entry(k, v))
                        top <- l //x.MoveNext l
            true

        member inline x.MoveNext() =
            if x._StackCount > 0 then
                let h = x.Pop()

                match h.Tree with
                | MapExtImplementation.MapEmpty ->
                    x._Current <- h.Item
                    true
                | MapExtImplementation.MapOne(k, v) ->
                    x._Current <- KeyValuePair(k,v)
                    true
                | MapExtImplementation.MapNode(k,v,l,r,_,_) ->
                    match r with
                    | MapExtImplementation.MapEmpty -> ()
                    | r ->  x.Push(Entry r)

                    x.Push(Entry(k,v))
                    x.MoveNext l

            else
                false

        member x.Reset() =
            x._StackCount <- 0
            match x._Root with
            | MapExtImplementation.MapEmpty -> ()
            | r -> x.Push(Entry r)
            x._Current <- Unchecked.defaultof<_>

        member x.Dispose() =
            x._Root <- MapExtImplementation.MapEmpty
            x._Stack <- Unchecked.defaultof<_>
            x._Current <- Unchecked.defaultof<_>

        member x.Current = x._Current
        
        interface IEnumerator with
            member x.MoveNext() = x.MoveNext()
            member x.Reset() = x.Reset()
            member x.Current = x.Current :> obj

        interface IEnumerator<KeyValuePair<'Key, 'Value>> with
            member x.Current = x.Current
            member x.Dispose() = x.Dispose()

        new(map : MapExt<'Key, 'Value>) =
            let s = Array.zeroCreate map.Count
            let count = 
                match map.Tree with
                | MapExtImplementation.MapEmpty -> 0
                | r -> 
                    s.[0] <- Entry r
                    1

            {
                _Root = map.Tree
                _Current = Unchecked.defaultof<_>
                _Stack = s
                _StackCount = count
            }

    end



type private MapExtEnumeratorPartialArray<'Key, 'Value when 'Key : comparison> =
    struct
        val mutable public Root : MapExtImplementation.MapTree<'Key, 'Value>
        val mutable public Stack : list<MapExtImplementation.MapTree<'Key, 'Value>>
        val mutable public Values : KeyValuePair<'Key, 'Value>[]
        val mutable public Index : int

        member x.MoveNext (top : MapExtImplementation.MapTree<_,_>) =
            let mutable top = top
            let mutable run = true
            while run do
                match top with
                | MapExtImplementation.MapEmpty ->
                    failwith "invalid state"
                | MapExtImplementation.MapOne(k, v) ->
                    x.Values <- [| KeyValuePair(k, v) |]
                    x.Index <- 0
                    run <- false
                | MapExtImplementation.MapNode(k, v, l, r, _, c) ->
                    if c <= 16 then
                        x.Values <- MapExtImplementation.MapTree.toArrayKVP top
                        x.Index <- 0
                        run <- false
                    else
                        match l with
                        | MapExtImplementation.MapEmpty ->
                            x.Values <- [| KeyValuePair(k,v) |]
                            x.Index <- 0
                            match r with
                            | MapExtImplementation.MapEmpty -> ()
                            | _ -> x.Stack <- r :: x.Stack
                            run <- false
                        | MapExtImplementation.MapOne(lk, lv) ->
                            x.Values <- [| KeyValuePair(lk, lv); KeyValuePair(k, v) |]
                            x.Index <- 0
                            match r with
                            | MapExtImplementation.MapEmpty -> ()
                            | _ -> x.Stack <- r :: x.Stack
                            run <- false
                        | l ->
                            match r with
                            | MapExtImplementation.MapEmpty -> ()
                            | _ -> x.Stack <- r :: x.Stack
                            x.Stack <- MapExtImplementation.MapOne(k, v) :: x.Stack
                            top <- l //x.MoveNext l
            true
            
        member inline x.MoveNext() =
            x.Index <- x.Index + 1
            if isNull x.Values || x.Index >= x.Values.Length then
                match x.Stack with
                | h :: t ->
                    x.Stack <- t
                    x.MoveNext h
                | [] ->
                    false
            else
                true
           
        member x.Reset() =
            match x.Root with
            | MapExtImplementation.MapEmpty ->
                x.Index <- -1
                x.Values <- [||]
                x.Stack <- []
            | MapExtImplementation.MapOne(k,v) ->
                x.Index <- -1
                x.Values <- [|KeyValuePair(k, v)|]
                x.Stack <- []
            | MapExtImplementation.MapNode(_,_,_,_,_,cnt) ->
                if cnt <= 16 then
                    x.Index <- -1
                    x.Values <- MapExtImplementation.MapTree.toArrayKVP x.Root
                    x.Stack <- []
                else
                    let stack = 
                        match x.Root with
                        | MapExtImplementation.MapEmpty -> []
                        | r -> [r]
                    x.Index <- -1
                    x.Values <- null
                    x.Stack <- stack
           
        member inline x.Dispose() =
            x.Root <- MapExtImplementation.MapEmpty
            x.Index <- -1
            x.Values <- null
            x.Stack <- []

        member inline x.Current = x.Values.[x.Index]
        
        interface IEnumerator with
            member x.MoveNext() = x.MoveNext()
            member x.Reset() = x.Reset()
            member x.Current = x.Current :> obj

        interface IEnumerator<KeyValuePair<'Key, 'Value>> with
            member x.Current = x.Current
            member x.Dispose() = x.Dispose()

        static member Flatten(m : MapExtImplementation.MapTree<'Key, 'Value>, dst : KeyValuePair<'Key, 'Value>[], offset : byref<int>) =
            match m with
            | MapExtImplementation.MapEmpty ->
                ()
            | MapExtImplementation.MapOne(k, v) ->
                dst.[offset] <- KeyValuePair(k, v)
                offset <- offset + 1

            | MapExtImplementation.MapNode(k, v, l, r, _, _) ->
                MapExtEnumeratorLazyArray<'Key, 'Value>.Flatten(l, dst, &offset)
                
                dst.[offset] <- KeyValuePair(k, v)
                offset <- offset + 1
                
                MapExtEnumeratorLazyArray<'Key, 'Value>.Flatten(r, dst, &offset)


        new(map : MapExt<'Key, 'Value>) =

            let root = map.Tree
            match root with
            | MapExtImplementation.MapEmpty ->
                {
                    Root = root
                    Index = -1
                    Values = [||]
                    Stack = []
                }
            | MapExtImplementation.MapOne(k,v) ->
                {
                    Root = root
                    Index = -1
                    Values = [|KeyValuePair(k, v)|]
                    Stack = []
                }
            | MapExtImplementation.MapNode(_,_,_,_,_,cnt) ->
                if cnt <= 16 then
                    {
                        Root = map.Tree
                        Index = -1
                        Values = MapExtImplementation.MapTree.toArrayKVP map.Tree
                        Stack = []
                    }
                else
                    let stack = 
                        match map.Tree with
                        | MapExtImplementation.MapEmpty -> []
                        | r -> [r]

                    {
                        Root = map.Tree
                        Index = -1
                        Values = null
                        Stack = stack
                    }


    end


[<PlainExporter; MemoryDiagnoser>]
type InlineStackBenchmark() =

    let mutable map : MapExt<Index, int> = MapExt.empty
    let mutable list : IndexList<int> = IndexList.empty
    let mutable array : KeyValuePair<Index, int>[] = [||]
    
    [<DefaultValue; Params(0, 1, 10, 100, 1000)>]
    val mutable public Count : int

    // |               Method | Count |         Mean |      Error |     StdDev |  Gen 0 | Gen 1 | Gen 2 | Allocated |
    // |--------------------- |------ |-------------:|-----------:|-----------:|-------:|------:|------:|----------:|
    // |    MapExtEnumerator0 |     1 |     24.55 ns |   0.518 ns |   0.554 ns | 0.0051 |     - |     - |      32 B |
    // | MapExtEnumerator2Opt |     1 |     20.80 ns |   0.324 ns |   0.304 ns |      - |     - |     - |         - |
    // | MapExtEnumerator4Opt |     1 |     20.95 ns |   0.440 ns |   0.556 ns |      - |     - |     - |         - |
    // |    MapExtEnumerator0 |    10 |    166.08 ns |   3.319 ns |   4.198 ns | 0.0701 |     - |     - |     440 B |
    // | MapExtEnumerator2Opt |    10 |    125.09 ns |   2.522 ns |   3.927 ns | 0.0293 |     - |     - |     184 B |
    // | MapExtEnumerator4Opt |    10 |    108.04 ns |   2.078 ns |   2.041 ns | 0.0191 |     - |     - |     120 B |
    // |    MapExtEnumerator0 |   100 |  1,632.39 ns |  32.582 ns |  45.675 ns | 0.7000 |     - |     - |    4400 B |
    // | MapExtEnumerator2Opt |   100 |  1,939.34 ns |  37.562 ns |  47.504 ns | 0.6294 |     - |     - |    3952 B |
    // | MapExtEnumerator4Opt |   100 |  1,529.47 ns |  30.376 ns |  29.833 ns | 0.5074 |     - |     - |    3184 B |
    // |    MapExtEnumerator0 |  1000 | 14,558.08 ns | 283.050 ns | 264.766 ns | 7.0038 |     - |     - |   44000 B |
    // | MapExtEnumerator2Opt |  1000 | 19,977.72 ns | 352.837 ns | 330.044 ns | 6.8970 |     - |     - |   43360 B |
    // | MapExtEnumerator4Opt |  1000 | 18,125.02 ns | 371.355 ns | 412.760 ns | 6.5613 |     - |     - |   41184 B |



    static member Check () =
        let l = List.init 100 id |> IndexList.ofList
        let m = l.Content

        let res = System.Collections.Generic.List<_>()
        let mutable e = new MapExtEnumerator0<_,_>(m)
        while e.MoveNext() do res.Add e.Current
        if Seq.toList res <> Seq.toList m then failwith "MapExtEnumerator0 bad"

        res.Clear()
        let mutable e = new MapExtEnumerator2<_,_>(m)
        while e.MoveNext() do res.Add e.Current
        if Seq.toList res <> Seq.toList m then failwith "MapExtEnumerator2 bad"
    
        res.Clear()
        let mutable e = new MapExtEnumerator4<_,_>(m)
        while e.MoveNext() do res.Add e.Current
        if Seq.toList res <> Seq.toList m then failwith "MapExtEnumerator4 bad"
    
        res.Clear()
        let mutable e = new MapExtEnumerator8<_,_>(m)
        while e.MoveNext() do res.Add e.Current
        if Seq.toList res <> Seq.toList m then failwith "MapExtEnumerator8 bad"
    
        res.Clear()
        let mutable e = new MapExtEnumerator12<_,_>(m)
        while e.MoveNext() do res.Add e.Current
        if Seq.toList res <> Seq.toList m then failwith "MapExtEnumerator12 bad"
    
        res.Clear()
        let mutable e = new MapExtEnumerator16<_,_>(m)
        while e.MoveNext() do res.Add e.Current
        if Seq.toList res <> Seq.toList m then failwith "MapExtEnumerator16 bad"
        
        res.Clear()
        let mutable e = new MapExtEnumeratorSystemStack<_,_>(m)
        while e.MoveNext() do res.Add e.Current
        if Seq.toList res <> Seq.toList m then failwith "MapExtEnumeratorSystemStack bad"
        
        res.Clear()
        let mutable e = new MapExtEnumerator2Opt<_,_>(m)
        while e.MoveNext() do res.Add e.Current
        if Seq.toList res <> Seq.toList m then failwith "MapExtEnumerator2Opt bad"
    
        res.Clear()
        let mutable e = new MapExtEnumerator4Opt<_,_>(m)
        while e.MoveNext() do res.Add e.Current
        if Seq.toList res <> Seq.toList m then failwith "MapExtEnumerator4Opt bad"
 
        res.Clear()
        let mutable e = new MapExtEnumerator6Opt<_,_>(m)
        while e.MoveNext() do res.Add e.Current
        if Seq.toList res <> Seq.toList m then failwith "MapExtEnumerator6Opt bad"
 
        res.Clear()
        let mutable e = new MapExtEnumerator8Opt<_,_>(m)
        while e.MoveNext() do res.Add e.Current
        if Seq.toList res <> Seq.toList m then failwith "MapExtEnumerator8Opt bad"
 
        res.Clear()
        let mutable e = new MapExtEnumeratorArr<_,_>(m)
        while e.MoveNext() do res.Add e.Current
        if Seq.toList res <> Seq.toList m then failwith "MapExtEnumeratorArr bad"
 
        res.Clear()
        let mutable e = new MapExtEnumeratorNew<_,_>(m)
        while e.MoveNext() do res.Add e.Current
        if Seq.toList res <> Seq.toList m then failwith "MapExtEnumeratorNew bad"
 
        res.Clear()
        let mutable e = new MapExtEnumeratorLazyArray<_,_>(m)
        while e.MoveNext() do res.Add e.Current
        if Seq.toList res <> Seq.toList m then failwith "MapExtEnumeratorLazyArray bad"
 
        res.Clear()
        let mutable e = new MapExtEnumeratorPartialArray<_,_>(m)
        while e.MoveNext() do res.Add e.Current
        if Seq.toList res <> Seq.toList m then failwith "MapExtEnumeratorPartialArray bad"
 
    [<GlobalSetup>]
    member x.Setup() =
        InlineStackBenchmark.Check()
        let l = IndexList.ofList [1.. x.Count] 
        map <- l.Content
        list <- l
        array <- Array.zeroCreate x.Count
        
         
    [<Benchmark>]
    member x.MapExtEnumeratorPartialArray() =
        let mutable sum = 0
        let mutable e = new MapExtEnumeratorPartialArray<_,_>(map)
        try
            while e.MoveNext() do 
                sum <- sum + e.Current.Value
        finally
            e.Dispose()
        sum
        
        
        
    [<Benchmark>]
    member x.CopyToArraySum() =
        let mutable o = 0
        MapExtEnumeratorLazyArray<_,_>.Flatten(map.Tree, array, &o)
        let mutable sum = 0
        for kvp in array do
            sum <- sum + kvp.Value
        sum

    //[<Benchmark>]
    //member x.MapExtEnumerator0() =
    //    let mutable sum = 0
    //    let mutable e = new MapExtEnumerator0<_,_>(map)
    //    try
    //        while e.MoveNext() do 
    //            sum <- sum + e.Current.Value
    //    finally
    //        e.Dispose()
    //    sum

    //[<Benchmark>]
    //member x.MapExtEnumerator2Opt() =
    //    let mutable sum = 0
    //    let mutable e = new MapExtEnumerator2Opt<_,_>(map)
    //    try
    //        while e.MoveNext() do 
    //            sum <- sum + e.Current.Value
    //    finally
    //        e.Dispose()
    //    sum

    //[<Benchmark>]
    //member x.MapExtEnumerator4Opt() =
    //    let mutable sum = 0
    //    let mutable e = new MapExtEnumerator4Opt<_,_>(map)
    //    try
    //        while e.MoveNext() do 
    //            sum <- sum + e.Current.Value
    //    finally
    //        e.Dispose()
    //    sum

    //[<Benchmark>]
    //member x.MapExtEnumerator6Opt() =
    //    let mutable sum = 0
    //    let mutable e = new MapExtEnumerator6Opt<_,_>(map)
    //    try
    //        while e.MoveNext() do 
    //            sum <- sum + e.Current.Value
    //    finally
    //        e.Dispose()
    //    sum

    //[<Benchmark>]
    //member x.MapExtEnumerator8Opt() =
    //    let mutable sum = 0
    //    let mutable e = new MapExtEnumerator8Opt<_,_>(map)
    //    try
    //        while e.MoveNext() do 
    //            sum <- sum + e.Current.Value
    //    finally
    //        e.Dispose()
    //    sum
         
    [<Benchmark>]
    member x.MapExtEnumeratorLazyArray() =
        let mutable sum = 0
        let mutable e = new MapExtEnumeratorLazyArray<_,_>(map)
        try
            while e.MoveNext() do 
                sum <- sum + e.Current.Value
        finally
            e.Dispose()
        sum
    //[<Benchmark>]
    //member x.MapExtEnumeratorArr() =
    //    let mutable sum = 0
    //    let mutable e = new MapExtEnumeratorArr<_,_>(map)
    //    try
    //        while e.MoveNext() do 
    //            sum <- sum + e.Current.Value
    //    finally
    //        e.Dispose()
    //    sum

    //[<Benchmark>]
    //member x.CurrentImpl() =
    //    let mutable sum = 0
    //    for kvp in map do
    //        sum <- sum + kvp.Value
    //    sum

    [<Benchmark>]
    member x.IndexList() =
        let mutable sum = 0
        for e in list do
            sum <- sum + e
        sum