namespace FSharp.Data.Adaptive


[<AutoOpen>]
module ComputationExpressions =

    type AValBuilder() =
        static let delayed = AVal.custom ignore

        member inline x.Bind(value: aval<'T1>, mapping: 'T1 -> aval<'T2>) =
            AVal.bind mapping value

        member inline x.Return(value: 'T) =
            AVal.constant value

        member inline x.ReturnFrom(value: aval<'T>) =
            value

        member inline x.Zero() =
            AVal.constant ()

        member x.Delay(value: unit -> aval<'T>) =
            delayed |> AVal.bind value

        member inline x.Combine(left: aval<unit>, right: aval<'T>) =
            AVal.map2 (fun () r -> r) left right

        // TODO: should we add these? 
        //member inline x.For(elements: seq<'T>, action: 'T -> aval<unit>) =
        //    let all = elements |> Seq.map action |> Seq.toArray
        //    AVal.custom (fun token ->
        //        all |> Array.iter (fun v -> v.GetValue token)
        //    )

    type ASetBuilder() =
        member inline x.Yield(value: 'T) = ASet.single value
        member inline x.YieldFrom(values: aset<'T>) = values
        member inline x.YieldFrom(values: seq<'T>) = ASet.ofSeq values
        member inline x.YieldFrom(values: list<'T>) = ASet.ofList values
        member inline x.YieldFrom(values: 'T[]) = ASet.ofArray values
        member inline x.YieldFrom(values: HashSet<'T>) = ASet.ofHashSet values

        member inline x.Bind(value: aval<'T1>, mapping: 'T1 -> aset<'T2>) =
            ASet.bind mapping value

        member inline x.For(elements: aset<'T1>, mapping: 'T1 -> aset<'T2>) =
            ASet.collect mapping elements
            
        member inline x.For(elements: seq<'T1>, mapping: 'T1 -> aset<'T2>) =
            elements |> Seq.map mapping |> ASet.ofSeq |> ASet.unionMany
            
        member inline x.For(elements: list<'T1>, mapping: 'T1 -> aset<'T2>) =
            elements |> List.map mapping |> ASet.ofList |> ASet.unionMany
            
        member inline x.For(elements: 'T1[], mapping: 'T1 -> aset<'T2>) =
            elements |> Array.map mapping |> ASet.ofArray |> ASet.unionMany
            
        member inline x.For(elements: HashSet<'T1>, mapping: 'T1 -> aset<'T2>) =
            elements |> HashSet.map mapping |> ASet.ofHashSet |> ASet.unionMany

        member inline x.Zero() = ASet.empty
        member inline x.Combine(l: aset<'T>, r: aset<'T>) = ASet.union l r
        member inline x.Delay(value: unit -> aset<'T>) = value()

        
    type AListBuilder() =
        member inline x.Yield(value: 'T) = AList.single value
        member inline x.YieldFrom(values: alist<'T>) = values
        member inline x.YieldFrom(values: seq<'T>) = AList.ofSeq values
        member inline x.YieldFrom(values: list<'T>) = AList.ofList values
        member inline x.YieldFrom(values: 'T[]) = AList.ofArray values
        member inline x.YieldFrom(values: IndexList<'T>) = AList.ofIndexList values

        member inline x.Bind(value: aval<'T1>, mapping: 'T1 -> alist<'T2>) =
            AList.bind mapping value

        member inline x.For(elements: alist<'T1>, mapping: 'T1 -> alist<'T2>) =
            AList.collect mapping elements
            
        member inline x.For(elements: seq<'T1>, mapping: 'T1 -> alist<'T2>) =
            elements |> Seq.map mapping |> AList.concat
            
        member inline x.For(elements: list<'T1>, mapping: 'T1 -> alist<'T2>) =
            elements |> List.map mapping |> AList.concat
            
        member inline x.For(elements: 'T1[], mapping: 'T1 -> alist<'T2>) =
            elements |> Array.map mapping |> AList.concat
            
        member inline x.For(elements: IndexList<'T1>, mapping: 'T1 -> alist<'T2>) =
            elements |> IndexList.map mapping |> AList.concat

        member inline x.Zero() = AList.empty
        member inline x.Combine(l: alist<'T>, r: alist<'T>) = AList.append l r
        member inline x.Delay(value: unit -> alist<'T>) = value()

            
    let aval = AValBuilder()
    let aset = ASetBuilder()
    let alist = AListBuilder()
    let adaptive = aval

