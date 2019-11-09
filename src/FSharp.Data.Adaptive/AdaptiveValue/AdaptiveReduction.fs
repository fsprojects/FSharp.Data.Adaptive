namespace FSharp.Data.Adaptive


type AdaptiveReduction<'a, 's, 'v> =
    {
        seed    : 's
        add     : 's -> 'a -> 's
        sub     : 's -> 'a -> ValueOption<'s>
        view    : 's -> 'v
    }

module AdaptiveReduction =

    let par (l : AdaptiveReduction<'a, 's, 'v>) (r : AdaptiveReduction<'a, 't, 'w>) =
        {
            seed = (l.seed, r.seed)
            add = fun (s, t) a -> (l.add s a, r.add t a)
            sub = fun (s, t) a ->
                match l.sub s a with
                | ValueSome s ->
                    match r.sub t a with
                    | ValueSome t -> ValueSome(s,t)
                    | _ -> ValueNone
                | ValueNone ->
                    ValueNone
            view = fun (s,t) -> (l.view s, r.view t)
        }

    let structpar (l : AdaptiveReduction<'a, 's, 'v>) (r : AdaptiveReduction<'a, 't, 'w>) =
        {
            seed = struct(l.seed, r.seed)
            add = fun struct (s, t) a -> struct(l.add s a, r.add t a)
            sub = fun struct (s, t) a ->
                match l.sub s a with
                | ValueSome s ->
                    match r.sub t a with
                    | ValueSome t -> ValueSome(struct (s,t))
                    | _ -> ValueNone
                | ValueNone ->
                    ValueNone
            view = fun struct (s,t) -> struct (l.view s, r.view t)
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

    let inline sum() =
        {
            seed = LanguagePrimitives.GenericZero
            add = fun s a -> s + a
            sub = fun s a -> ValueSome (s - a)
            view = id
        }

    let inline product() =
        {
            seed = LanguagePrimitives.GenericOne
            add = fun s a -> s * a
            sub = fun s a -> if a <> LanguagePrimitives.GenericZero then ValueSome (s / a) else ValueNone
            view = id
        }