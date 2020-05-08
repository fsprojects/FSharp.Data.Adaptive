namespace FSharp.Data.Adaptive


/// ComputationExpression builders for aval/aset/alist/amap.
[<AutoOpen>]
module ComputationExpressions =

    /// ComputationExpression builder for aval.
    type AValBuilder() =
        
        member inline x.MergeSources(v1 : aval<'T1>, v2 : aval<'T2>) =
            AVal.map2 (fun a b -> a,b) v1 v2
            
        member inline x.MergeSources3(v1 : aval<'T1>, v2 : aval<'T2>, v3 : aval<'T3>) =
            AVal.map3 (fun a b c -> a,b,c) v1 v2 v3

        member inline x.BindReturn(value : aval<'T1>, mapping: 'T1 -> 'T2) =
            AVal.map mapping value
            
        member inline x.Bind2Return(v1 : aval<'T1>, v2 : aval<'T2>, mapping: 'T1 * 'T2 -> 'T3) =
            AVal.map2 (fun a b -> mapping(a,b)) v1 v2

        member inline x.Bind3Return(v1 : aval<'T1>, v2: aval<'T2>, v3: aval<'T3>, mapping: 'T1 * 'T2 * 'T3 -> 'T4) =
            AVal.map3 (fun a b c -> mapping(a, b, c)) v1 v2 v3

        member inline x.Bind(value: aval<'T1>, mapping: 'T1 -> aval<'T2>) =
            AVal.bind mapping value
            
        member inline x.Bind2(v1: aval<'T1>, v2: aval<'T2>, mapping: 'T1 * 'T2 -> aval<'T3>) =
            AVal.bind2 (fun a b -> mapping(a,b)) v1 v2
            
        member inline x.Bind3(v1: aval<'T1>, v2: aval<'T2>, v3: aval<'T3>, mapping: 'T1 * 'T2 * 'T3 -> aval<'T4>) =
            AVal.bind3 (fun a b c -> mapping(a, b, c)) v1 v2 v3

        member inline x.Return(value: 'T) =
            AVal.constant value

        member inline x.ReturnFrom(value: aval<'T>) =
            value
        
    /// ComputationExpression builder for aset.
    type ASetBuilder() =
        member inline x.Yield(value: 'T) = ASet.single value
        member inline x.YieldFrom(values: aset<'T>) = values
        member inline x.YieldFrom(values: cset<'T>) = values :> aset<_>
        member inline x.YieldFrom(values: seq<'T>) = ASet.ofSeq values
        member inline x.YieldFrom(values: list<'T>) = ASet.ofList values
        member inline x.YieldFrom(values: 'T[]) = ASet.ofArray values
        member inline x.YieldFrom(values: HashSet<'T>) = ASet.ofHashSet values
        
        member inline x.YieldFrom(values : alist<'T>) =
            ASet.ofAList values

        member inline x.YieldFrom(values : amap<'K, 'V>) =
            ASet.ofAMap values

        member inline x.YieldFrom(values: aval<HashSet<'T>>) = 
            ASet.ofAVal values
            
        member inline x.YieldFrom(values: aval<seq<'T>>) = 
            ASet.ofAVal (AVal.map HashSet.ofSeq values)
            
        member inline x.YieldFrom(values: aval<list<'T>>) = 
            ASet.ofAVal (AVal.map HashSet.ofList values)
            
        member inline x.YieldFrom(values: aval<array<'T>>) = 
            ASet.ofAVal (AVal.map HashSet.ofArray values)
            
        member inline x.Bind(value: aval<'T1>, mapping: 'T1 -> aset<'T2>) =
            ASet.bind mapping value
            
        member inline x.For(elements: aval<#seq<'T1>>, mapping: 'T1 -> aset<'T2>) =
            elements 
            |> AVal.map HashSet.ofSeq
            |> ASet.ofAVal
            |> ASet.collect mapping

        member inline x.For(elements: aval<HashSet<'T1>>, mapping: 'T1 -> aset<'T2>) =
            elements 
            |> ASet.ofAVal
            |> ASet.collect mapping

        member inline x.For(elements: aset<'T1>, mapping: 'T1 -> aset<'T2>) =
            ASet.collect mapping elements
            
        member inline x.For(elements: cset<'T1>, mapping: 'T1 -> aset<'T2>) =
            ASet.collect mapping elements

        member inline x.For(elements: alist<'T1>, mapping: 'T1 -> aset<'T2>) =
            elements |> AList.map mapping |> ASet.ofAList |> ASet.unionMany
            
        member inline x.For(elements: amap<'K, 'V>, mapping: ('K * 'V) -> aset<'T>) =
            elements |> AMap.map (fun k v -> mapping(k,v)) |> ASet.ofAMapValues |> ASet.unionMany

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
        
    /// ComputationExpression builder for alist.
    type AListBuilder() =
        member inline x.Yield(value: 'T) = AList.single value
        member inline x.YieldFrom(values: alist<'T>) = values
        member inline x.YieldFrom(values: clist<'T>) = values :> alist<_>
        member inline x.YieldFrom(values: seq<'T>) = AList.ofSeq values
        member inline x.YieldFrom(values: list<'T>) = AList.ofList values
        member inline x.YieldFrom(values: 'T[]) = AList.ofArray values
        member inline x.YieldFrom(values: IndexList<'T>) = AList.ofIndexList values
        
        member inline x.YieldFrom(values: aval<IndexList<'T>>) = 
            AList.ofAVal values
            
        member inline x.YieldFrom(values: aval<seq<'T>>) = 
            AList.ofAVal (AVal.map IndexList.ofSeq values)
            
        member inline x.YieldFrom(values: aval<list<'T>>) = 
            AList.ofAVal (AVal.map IndexList.ofList values)
            
        member inline x.YieldFrom(values: aval<array<'T>>) = 
            AList.ofAVal (AVal.map IndexList.ofArray values)
        
    
        member inline x.Bind(value: aval<'T1>, mapping: 'T1 -> alist<'T2>) =
            AList.bind mapping value

        member inline x.For(elements: alist<'T1>, mapping: 'T1 -> alist<'T2>) =
            AList.collect mapping elements
            
        member inline x.For(elements: clist<'T1>, mapping: 'T1 -> alist<'T2>) =
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
        
    /// ComputationExpression builder for amap.
    type AMapBuilder() =
        member inline x.Zero() = AMap.empty

        member inline x.Yield(key : 'Key, value : 'Value) = 
            AMap.single key value

        member inline x.YieldFrom(value: aval<'Key * 'Value>) =
            value |> AVal.map (fun (k,v) -> HashMap.single k v) |> AMap.ofAVal
            
        //member inline x.YieldFrom((key: aval<'Key>, value: aval<'Value>)) =
        //    (key, value) ||> AVal.map2 HashMap.single |> AMap.ofAVal
            
        member inline x.YieldFrom((key: aval<'Key>, value: 'Value)) =
            key |> AVal.map (fun k -> HashMap.single k value) |> AMap.ofAVal

        member inline x.YieldFrom((key: 'Key, value: aval<'Value>)) =
            value |> AVal.map (fun v -> HashMap.single key v) |> AMap.ofAVal

        member inline x.YieldFrom(value: aval<HashMap<'Key, 'Value>>) = AMap.ofAVal value
        member inline x.YieldFrom(map: amap<'Key, 'Value>) = map
        member inline x.YieldFrom(map: cmap<'Key, 'Value>) = map :> amap<_,_>
        member inline x.YieldFrom(map: HashMap<'Key, 'Value>) = AMap.ofHashMap map
        member inline x.YieldFrom(map: ('Key * 'Value) seq) = AMap.ofSeq map
        member inline x.YieldFrom(map: ('Key * 'Value) list) = AMap.ofList map
        member inline x.YieldFrom(map: ('Key * 'Value) array) = AMap.ofArray map
        member inline x.Bind(value: aval<'T>, mapping: 'T -> amap<'Key, 'Value>) = AMap.bind mapping value
        member inline x.Delay(value: unit -> amap<'Key, 'Value>) = value()
        member inline x.Combine(l: amap<'Key, 'Value>, r: amap<'Key, 'Value>) = AMap.union l r
            
    /// ComputationExpression builder for aval.
    let aval = AValBuilder()

    /// ComputationExpression builder for aset.
    let aset = ASetBuilder()

    /// ComputationExpression builder for alist.
    let alist = AListBuilder()

    /// ComputationExpression builder for amap.
    let amap = AMapBuilder()

    /// ComputationExpression builder for aval.
    let adaptive = aval

    /// tests if some ComputationExpressions compile
    module private Test =
        
        let set (e : aval<int>) (some : aval<list<int>>) (set : aset<int>) (c : cset<int>) (m : amap<int, string>) =
            aset {
                yield 1337

                yield! c
                yield! AMap.keys m
                
                yield! some
                yield! (AVal.map Seq.singleton e)
                yield! (AVal.map List.singleton e)
                yield! (AVal.map Array.singleton e)
                yield! (AVal.map HashSet.single e)
                
                for v in c do
                    yield v

                for v in set do
                    yield v

                for i in 1 .. 100 do
                    yield i

                for e in some do
                    yield e.GetHashCode()


                let! _test = e
                ()
            }

        let list (e : aval<int>) (some : aval<list<int>>) (list : alist<int>) (c : clist<int>) =
            alist {
                yield 1

                yield! c
                yield! list
                yield! some

                for e in c do
                    yield 2*e

                for e in list do
                    yield! [2*e; 2*e+1]

                let! v = e
                ()
            }

        let map (k : aval<int>) (v : aval<string>) (c : cmap<int, string>) =
            amap {
                // yield constant key and value
                yield (1, "one")

                yield! c

                // yield adaptive tuple
                yield! (k |> AVal.map (fun v -> v, string v))

                // yield adaptive key and constant value
                yield! (k, "value")

                // yield constant key and adaptive value
                yield! (1, v)

                // yield sequences
                yield! [2,"two"; 3,"three"]
                yield! [|2,"two"; 3,"three"|]
                yield! HashMap.ofArray [|2,"two"; 3,"three"|]
                yield! Seq.empty

                // bind works
                let! _foo = v

                // zero works
                ()
                
            }


