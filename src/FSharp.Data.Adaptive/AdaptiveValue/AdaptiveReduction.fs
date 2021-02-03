namespace FSharp.Data.Adaptive

[<Struct>]
type AdaptiveReduction<'a, 's, 'v> =
    {
        seed    : 's
        add     : 's -> 'a -> 's
        sub     : 's -> 'a -> ValueOption<'s>
        view    : 's -> 'v
    }

module AdaptiveReduction =

    let par (left : AdaptiveReduction<'a, 's, 'v>) (right : AdaptiveReduction<'a, 't, 'w>) =
        {
            seed = (left.seed, right.seed)
            add = fun (s, t) a -> (left.add s a, right.add t a)
            sub = fun (s, t) a ->
                match left.sub s a with
                | ValueSome s ->
                    match right.sub t a with
                    | ValueSome t -> ValueSome(s,t)
                    | _ -> ValueNone
                | ValueNone ->
                    ValueNone
            view = fun (s,t) -> (left.view s, right.view t)
        }

    let structpar (left : AdaptiveReduction<'a, 's, 'v>) (right : AdaptiveReduction<'a, 't, 'w>) =
        {
            seed = struct(left.seed, right.seed)
            add = fun struct (s, t) a -> struct(left.add s a, right.add t a)
            sub = fun struct (s, t) a ->
                match left.sub s a with
                | ValueSome s ->
                    match right.sub t a with
                    | ValueSome t -> ValueSome(struct (s,t))
                    | _ -> ValueNone
                | ValueNone ->
                    ValueNone
            view = fun struct (s,t) -> struct (left.view s, right.view t)
        }

    let mapIn (mapping : 'a -> 'b) (reduction : AdaptiveReduction<'b, 's, 'v>) =
        {
            seed = reduction.seed
            add = fun s a -> reduction.add s (mapping a)
            sub = fun s a -> reduction.sub s (mapping a)
            view = reduction.view
        }
        
    let mapOut (mapping : 'v -> 'w) (reduction : AdaptiveReduction<'b, 's, 'v>) =
        {
            seed = reduction.seed
            add = reduction.add
            sub = reduction.sub
            view = reduction.view >> mapping
        }

    [<GeneralizableValue>]
    let count<'a> : AdaptiveReduction<'a, int, int> =
        {
            seed = LanguagePrimitives.GenericZero
            add = fun s a -> s + 1
            sub = fun s a -> ValueSome (s - 1)
            view = id
        }

    let group (zero : 's) (add : 's -> 'a -> 's) (sub : 's -> 'a -> 's) =
        {
            seed = zero
            add = add
            sub = fun s a -> ValueSome(sub s a)
            view = id
        }

    let halfGroup (zero : 's) (add : 's -> 'a -> 's) (sub : 's -> 'a -> ValueOption<'s>) =
        {
            seed = zero
            add = add
            sub = sub
            view = id
        }

    let fold (zero : 's) (add : 's -> 'a -> 's) =
        {
            seed = zero
            add = add
            sub = fun _ _ -> ValueNone
            view = id
        }


    let countPositive : AdaptiveReduction<bool, int, int> =
        let inline convert (v : bool) =
            if v then 1
            else 0
        {
            seed = LanguagePrimitives.GenericZero
            add = fun s a -> s + convert a
            sub = fun s a -> ValueSome (s - convert a)
            view = id
        }

    let countNegative : AdaptiveReduction<bool, int, int> =
        let inline convert (v : bool) =
            if v then 0
            else 1
        {
            seed = LanguagePrimitives.GenericZero
            add = fun s a -> s + convert a
            sub = fun s a -> ValueSome (s - convert a)
            view = id
        }

    let tryMin<'a when 'a : comparison> =
        let add (o : ValueOption<'a>) (v : 'a) =
            match o with
            | ValueSome o -> ValueSome (min o v)
            | ValueNone -> ValueSome v
            
        let trySub (o : ValueOption<'a>) (v : 'a) =
            match o with
            | ValueSome o ->
                if v > o then ValueSome (ValueSome o)
                else ValueNone
            | ValueNone -> 
                ValueSome ValueNone

        {
            seed = ValueNone
            add = add
            sub = trySub
            view = id
        }

    let tryMax<'a when 'a : comparison> =
        let add (o : ValueOption<'a>) (v : 'a) =
            match o with
            | ValueSome o -> ValueSome (max o v)
            | ValueNone -> ValueSome v
            
        let trySub (o : ValueOption<'a>) (v : 'a) =
            match o with
            | ValueSome o ->
                if v < o then ValueSome (ValueSome o)
                else ValueNone
            | ValueNone -> 
                ValueSome ValueNone

        {
            seed = ValueNone
            add = add
            sub = trySub
            view = id
        }
        

    let inline sum() =
        {
            seed = LanguagePrimitives.GenericZero
            add = fun s a -> s + a
            sub = fun s a -> ValueSome (s - a)
            view = id
        }
        
    let inline average() =
        {
            seed = struct(0, LanguagePrimitives.GenericZero)
            add = fun struct(c, s) a -> struct(c + 1, s + a)
            sub = fun struct(c, s) a -> ValueSome (struct (c - 1, s - a))
            view = fun struct(c, s) -> LanguagePrimitives.DivideByInt s c
        }

    let inline product() =
        {
            seed = LanguagePrimitives.GenericOne
            add = fun s a -> s * a
            sub = fun s a -> if a <> LanguagePrimitives.GenericZero then ValueSome (s / a) else ValueNone
            view = id
        }