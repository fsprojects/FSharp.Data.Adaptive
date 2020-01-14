module Generators

open System
open FsCheck
open FSharp.Data
open FSharp.Reflection
open System.Reflection
open System.Text.RegularExpressions
open FSharp.Data.Adaptive
open FsUnit

/// reflection helpers for existential types.
[<AutoOpen>]
module Helpers =

    type TypeVisitor<'r> =
        abstract member Accept<'a> : unit -> 'r
        
    type TypeVisitor2<'r> =
        abstract member Accept<'a, 'b> : unit -> 'r
        
    type TypeVisitor3<'r> =
        abstract member Accept<'a, 'b, 'c> : unit -> 'r

    let private def1 = typedefof<TypeVisitor<_>>
    let private def2 = typedefof<TypeVisitor2<_>>
    let private def3 = typedefof<TypeVisitor3<_>>

    open System.Reflection.Emit

    let private invoker (mi : MethodInfo) =
        let dargs = [|typeof<obj>|]
        let m = DynamicMethod("invoker", MethodAttributes.Static ||| MethodAttributes.Public, CallingConventions.Standard, typeof<obj>, dargs, typeof<obj>, true)

        let il = m.GetILGenerator()
        il.Emit(OpCodes.Ldarg, 0)
        il.EmitCall(OpCodes.Callvirt, mi, null)
        if mi.ReturnType.IsValueType then il.Emit(OpCodes.Box, mi.ReturnType)
        il.Emit(OpCodes.Ret)

        let invoke = 
            m.CreateDelegate(typeof<Func<obj, obj>>)
            |> unbox<Func<obj, obj>>

        invoke.Invoke

    let private getMethod1 =
        let cache = System.Collections.Concurrent.ConcurrentDictionary<Type * Type, obj -> obj>()
        fun (t : Type) (tr : Type) ->
            cache.GetOrAdd((t, tr), Func<_,_>(fun (t, tr) -> 
                let tv = def1.MakeGenericType [|tr|]
                let mi = tv.GetMethod("Accept").MakeGenericMethod [| t |]
                invoker mi
            ))
            
    let private getMethod2 =
        let cache = System.Collections.Concurrent.ConcurrentDictionary<Type * Type * Type, obj -> obj>()
        fun (t1 : Type) (t2 : Type) (tr : Type) ->
            cache.GetOrAdd((t1, t2, tr), Func<_,_>(fun (t1, t2, tr) -> 
                let tv = def2.MakeGenericType [|tr|]
                let mi = tv.GetMethod("Accept").MakeGenericMethod [| t1; t2 |]
                invoker mi
            ))
            
    let private getMethod3 =
        let cache = System.Collections.Concurrent.ConcurrentDictionary<Type * Type * Type * Type, obj -> obj>()
        fun (t1 : Type) (t2 : Type) (t3 : Type) (tr : Type) ->
            cache.GetOrAdd((t1, t2, t3, tr), Func<_,_>(fun (t1, t2, t3, tr) -> 
                let tv = def3.MakeGenericType [|tr|]
                let mi = tv.GetMethod("Accept").MakeGenericMethod [| t1; t2; t3 |]
                invoker mi
            ))

    let visit (v : TypeVisitor<'r>) (t0 : Type) =
        let mi = getMethod1 t0 typeof<'r>
        mi v |> unbox<'r>
        
    let visit2 (v : TypeVisitor2<'r>) (t0 : Type) (t1 : Type) =
        let mi = getMethod2 t0 t1 typeof<'r>
        mi v |> unbox<'r>
        
    let visit3 (v : TypeVisitor3<'r>) (t0 : Type) (t1 : Type) (t2 : Type) =
        let mi = getMethod3 t0 t1 t2 typeof<'r>
        mi v |> unbox<'r>

    type Type with
        member x.Visit(v : TypeVisitor<'r>) =
            let mi = getMethod1 x typeof<'r>
            mi v |> unbox<'r>
   
type refval<'a> = Reference.aval<'a>
type realval<'a> = Adaptive.aval<'a>
type refset<'a> = Reference.aset<'a>
type realset<'a> = Adaptive.aset<'a>
type refmap<'a, 'b> = Reference.amap<'a, 'b>
type realmap<'a, 'b> = Adaptive.amap<'a, 'b>
type reflist<'a> = Reference.alist<'a>
type reallist<'a> = Adaptive.alist<'a>


type ChangeGen = 
    {
        change : Gen<unit -> string>
        cell    : obj
    }

type VVal<'a> =
    {
        real : realval<'a>
        ref : refval<'a>
        expression : string
        changes : unit -> list<ChangeGen>
    }

type VSet<'a> =
    {
        sreal : realset<'a>
        sref : refset<'a>
        sexpression : bool -> Map<string, string> * string
        schanges : unit -> list<ChangeGen>
    }
    
type VMap<'a, 'b> =
    {
        mreal : realmap<'a, 'b>
        mref : refmap<'a, 'b>
        mexpression : bool -> Map<string, string> * string
        mchanges : unit -> list<ChangeGen>
    }

type VList<'a> =
    {
        lreal : reallist<'a>
        lref : reflist<'a>
        lexpression : bool -> Map<string, string> * string
        lchanges : unit -> list<ChangeGen>
    }
    
module Generators =
    let rand = Random()
    
    module Map =
        let union (a: Map<'K, 'V>) (b: Map<'K, 'V>) =
            let mutable res = a 
            for kvp in b do
                res <- Map.add kvp.Key kvp.Value res
            res


    let internal randomFunction<'a, 'b>(size : int) =
        let cache = 
            Cache<'a, 'b>(fun _ -> 
                let a = rand.Next()
                let b = rand.Next()
                let res = Arb.generate<'b> |> Gen.eval size (Random.StdGen(a, b))

                res
            )
        cache, cache.Invoke

    let internal randomFunction2<'a, 'b, 'c>() =
        let cache = 
            Cache<'a * 'b, 'c>(fun _ -> 
                let a = rand.Next()
                let b = rand.Next()
                Arb.generate<'c> |> Gen.eval 30 (Random.StdGen(a, b))
            )
        cache, fun a b -> cache.Invoke(a,b)
        
    let internal randomFunction3<'a, 'b, 'c, 'd>() =
        let cache = 
            Cache<'a * 'b * 'c, 'd>(fun _ -> 
                let a = rand.Next()
                let b = rand.Next()
                Arb.generate<'d> |> Gen.eval 30 (Random.StdGen(a, b))
            )
        cache, fun a b c -> cache.Invoke(a,b,c)

    let indent (str : string) =
        str.Split("\r\n") |> Array.map (fun s -> "  " + s) |> String.concat "\r\n"

    module Val = 
        let create a b s c = 
            {
                real = a
                ref = b
                expression = s
                changes = c
            }

        let init<'a>() =
            gen {
                let! id = Gen.choose(1, 128)
                let! value = Arb.generate<'a>

                let real = Adaptive.AVal.init value
                let ref = Reference.AVal.init value

                let change =    
                    { 
                        cell = (real, id) :> obj
                        change = 
                            gen {
                                let! newValue = Arb.generate<'a>
                                return fun () ->    
                                    real.Value <- newValue
                                    ref.Value <- newValue
                                    sprintf "C%d <- %A" id newValue

                            }
                    }

                return 
                    create 
                        (real :> Adaptive.aval<_>)
                        (ref :> Reference.aval<_>)
                        (sprintf "c%d" id)
                        (fun () -> [change])
            }

        let constant<'a>() =
            gen {
                let! value = Arb.generate<'a>

                let asString = 
                    match value :> obj with
                    | :? string as str -> System.Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes str)
                    | _ -> sprintf "%A" value
                return 
                    create
                        (Adaptive.AVal.constant value)
                        (Reference.AVal.constant value)
                        (sprintf "v (%s)" asString)
                        (fun () -> [])
            }

        let map<'a, 'b>() =
            gen {
                let! value = Arb.generate<_> |> Gen.scaleSize (fun s -> s - 1)
                let _table, f = randomFunction<'a, 'b> 40
                return 
                    create 
                        (Adaptive.AVal.map f value.real)
                        (Reference.AVal.map f value.ref)
                        (sprintf "map (\r\n%s\r\n)" (indent value.expression))
                        value.changes
            }

        let map2<'a, 'b, 'c>() =
            gen {
                let! value0 = Arb.generate<_> |> Gen.scaleSize (fun s -> s / 2)
                let! value1 = Arb.generate<_> |> Gen.scaleSize (fun s -> s / 2)
                let _table, f = randomFunction2<'a, 'b, 'c>() //let! f = Arb.generate<'a -> 'b -> 'c> |> Gen.scaleSize (fun _ -> 50)
                return 
                    create 
                        (Adaptive.AVal.map2 f value0.real value1.real)
                        (Reference.AVal.map2 f value0.ref value1.ref)
                        (sprintf "map2 (\r\n%s\r\n%s\r\n)" (indent value0.expression) (indent value1.expression))
                        (fun () -> List.append (value0.changes()) (value1.changes()))
            }

        let map3<'a, 'b, 'c, 'd>() =
            gen {
                let! value0 = Arb.generate<_> |> Gen.scaleSize (fun s -> s / 3)
                let! value1 = Arb.generate<_> |> Gen.scaleSize (fun s -> s / 3)
                let! value2 = Arb.generate<_> |> Gen.scaleSize (fun s -> s / 3)
                let _table, f = randomFunction3<'a, 'b, 'c, 'd>() //let! f = Arb.generate<'a -> 'b -> 'c> |> Gen.scaleSize (fun _ -> 50)
                return 
                    create 
                        (Adaptive.AVal.map3 f value0.real value1.real value2.real)
                        (Reference.AVal.map3 f value0.ref value1.ref value2.ref)
                        (sprintf "map3 (\r\n%s\r\n%s\r\n%s\r\n)" (indent value0.expression) (indent value1.expression) (indent value2.expression))
                        (fun () -> value0.changes() @ value1.changes() @ value2.changes())
            }

        let bind<'a, 'b>() =
            gen {
                let mutable mySize = ref 0
                let! value = Arb.generate<VVal<'a>> |> Gen.scaleSize (fun s -> mySize := s; s - 1)
                //let! mapping = Arb.generate<'a -> Val<'b>> |> Gen.scaleSize (fun _ -> 50)
                let _table, mapping = randomFunction<'a, VVal<'b>>(!mySize / 2)

                let changes = value.changes

                let mutable latest = None

                let getChanges() =
                    match latest with
                    | Some l -> List.append (l.changes()) (changes())
                    | None -> changes()

                let mapping (input : 'a) =
                    let res = mapping input
                    latest <- Some res
                    res

                return 
                    create 
                        (Adaptive.AVal.bind (fun v -> (mapping v).real) value.real)
                        (Reference.AVal.bind (fun v -> (mapping v).ref) value.ref)
                        (sprintf "bind (\r\n%s\r\n)" (indent value.expression))
                        getChanges
            }

        let bind2<'a, 'b, 'c>() =
            gen {
                let mutable mySize = ref 0
                let! v1 = Arb.generate<VVal<'a>> |> Gen.scaleSize (fun s -> mySize := s; s / 2)
                let! v2 = Arb.generate<VVal<'b>> |> Gen.scaleSize (fun s -> s / 2)

                let _table, mapping = randomFunction<'a * 'b, VVal<'c>>(!mySize / 2)

                let changes() = 
                    v1.changes() @ v2.changes()

                let mutable latest = None

                let getChanges() =
                    match latest with
                    | Some l -> List.append (l.changes()) (changes())
                    | None -> changes()

                let mapping (a : 'a) (b : 'b) =
                    let res = mapping(a,b)
                    latest <- Some res
                    res

                return 
                    create 
                        (Adaptive.AVal.bind2 (fun a b -> (mapping a b).real) v1.real v2.real)
                        (Reference.AVal.bind2 (fun a b -> (mapping a b).ref) v1.ref v2.ref)
                        (sprintf "bind2 (\r\n%s\r\n%s\r\n)" (indent v1.expression) (indent v2.expression))
                        getChanges
            }

        let ofASet<'i, 'a>() =
            let transform =
                if typeof<'a> = typeof<HashSet<'i>> then
                    fun (v : HashSet<'i>) -> unbox v
                elif typeof<'a> = typeof<string> then 
                    fun (v : HashSet<'i>) -> string v |> unbox<'a>
                elif typeof<'a> = typeof<obj> then  
                    fun (v : HashSet<'i>) -> v :> obj |> unbox<'a>
                elif typeof<'a> = typeof<int> then 
                    fun (v : HashSet<'i>) -> v.GetHashCode() |> unbox<'a>
                else
                    fun (v : HashSet<'i>) -> Arb.generate<'a> |> Gen.eval 1 (Random.newSeed())

            gen {
                let mutable mySize = ref 0
                let! set = Arb.generate<VSet<'i>> |> Gen.scaleSize (fun _s -> 0)

                let cache = 
                    System.Collections.Concurrent.ConcurrentDictionary<HashSet<'i>, 'a>()


                let reduce (set : HashSet<'i>) =
                    cache.GetOrAdd(set, fun set ->
                        transform set
                    )

                let real = set.sreal |> Adaptive.ASet.toAVal |> Adaptive.AVal.map reduce
                let ref = set.sref |> Reference.ASet.toAVal |> Reference.AVal.map reduce


                return
                    create
                        real
                        ref
                        (sprintf "ofASet\r\n%s" (indent (set.sexpression false |> snd)))
                        (fun () -> set.schanges())
            }
            
        let ofAMap<'k, 'v, 'a>() =
            let transform =
                if typeof<'a> = typeof<HashMap<'k, 'v>> then
                    fun (v : HashMap<'k, 'v>) -> unbox v
                elif typeof<'a> = typeof<string> then 
                    fun (v : HashMap<'k, 'v>) -> string (Unchecked.hash v) |> unbox<'a>
                elif typeof<'a> = typeof<obj> then  
                    fun (v : HashMap<'k, 'v>) -> v :> obj |> unbox<'a>
                elif typeof<'a> = typeof<int> then 
                    fun (v : HashMap<'k, 'v>) -> Unchecked.hash v |> unbox<'a>
                else
                    fun (v : HashMap<'k, 'v>) -> Arb.generate<'a> |> Gen.eval 1 (Random.newSeed())

            gen {
                let mutable mySize = ref 0
                let! set = Arb.generate<VMap<'k, 'v>> |> Gen.scaleSize (fun _s -> 0)

                let cache = 
                    System.Collections.Concurrent.ConcurrentDictionary<HashMap<'k, 'v>, 'a>()


                let reduce (set : HashMap<'k, 'v>) =
                    cache.GetOrAdd(set, fun set ->
                        transform set
                    )

                let real = set.mreal |> Adaptive.AMap.toAVal |> Adaptive.AVal.map reduce
                let ref = set.mref |> Reference.AMap.toAVal |> Reference.AVal.map reduce


                return
                    create
                        real
                        ref
                        (sprintf "ofAMap\r\n%s" (indent (set.mexpression false |> snd)))
                        (fun () -> set.mchanges())
            }

    module Set =
        let mutable cid = 0

        let create a b s c = 
            {
                sreal = a
                sref = b
                sexpression = s
                schanges = c
            }

        let init<'a>() =
            gen {
                let id = System.Threading.Interlocked.Increment(&cid)
                let! value = Arb.generate<HashSet<'a>>

                let real = Adaptive.cset value
                let ref = Reference.cset value

                let change =    
                    { 
                        cell = (real, id) :> obj
                        change = 
                            gen {
                                let! newValue = Arb.generate<HashSet<'a>>
                                return fun () ->    
                                    real.Value <- newValue
                                    ref.Value <- newValue
                                    sprintf "C%d <- %A" id newValue

                            }
                    }

                return 
                    create 
                        (real :> Adaptive.aset<_>)
                        (ref :> Reference.aset<_>)
                        (function 
                            | false -> 
                                Map.empty, sprintf "c%d" id
                            | true -> 
                                let c = real.Value |> Seq.map (sprintf "%A") |> String.concat "; "
                                let m = Map.ofList [sprintf "c%d" id, sprintf "cset [%s]" c]
                                m, sprintf "c%d" id
                        )
                        (fun () -> [change])
            }

        let constant<'a>() =
            gen {
                let! value = Arb.generate<HashSet<'a>>
                let id = System.Threading.Interlocked.Increment(&cid)

                return 
                    create
                        (Adaptive.ASet.ofHashSet value)
                        (Reference.ASet.ofHashSet value)
                        (function 
                            | false -> Map.empty, sprintf "%A" value
                            | true -> 
                                let m = Map.ofList [sprintf "v%d" id, sprintf "ASet.ofList [%s]" (value |> Seq.map (sprintf "%A") |> String.concat "; ")]
                                m, sprintf "v%d" id
                        )
                        (fun () -> [])
            }

        let map<'a, 'b>() =
            gen {
                let mySize = ref 0
                let! value = Arb.generate<_> |> Gen.scaleSize (fun s -> mySize := s; s - 2)
                //let! f = Arb.generate<'a -> 'b> |> Gen.scaleSize (fun _ -> 50)
                let table, f = randomFunction<'a, 'b> (!mySize / 2)
                return 
                    create 
                        (Adaptive.ASet.map f value.sreal)
                        (Reference.ASet.map f value.sref)
                        (function
                            | false -> 
                                let m, v = value.sexpression false
                                m, sprintf "map (\r\n%s\r\n)" (indent v)
                            | true ->
                                let realContent = value.sref.Content |> Reference.AVal.force
                                let mi, input = value.sexpression true

                                let table =
                                    realContent 
                                    |> Seq.map (fun v -> sprintf "| %A -> %A" v (f v))
                                    |> String.concat "\r\n"

                                mi, sprintf "%s\r\n|> ASet.map (\r\n  function\r\n%s\r\n)" (indent input) (indent table)
                        )
                        value.schanges
            }
            
        let choose<'a, 'b>() =
            gen {
                let mySize = ref 0
                let! value = Arb.generate<_> |> Gen.scaleSize (fun s -> mySize := s; s - 2)
                //let! f = Arb.generate<'a -> 'b> |> Gen.scaleSize (fun _ -> 50)
                let table, f = randomFunction<'a, Option<'b>> (!mySize / 2)
                return 
                    create 
                        (Adaptive.ASet.choose f value.sreal)
                        (Reference.ASet.choose f value.sref)
                        (function
                            | false -> 
                                let m, v = value.sexpression false
                                m, sprintf "choose (\r\n%s\r\n)" (indent v)
                            | true ->
                                let realContent = value.sref.Content |> Reference.AVal.force
                                let mi, input = value.sexpression true

                                let table =
                                    realContent 
                                    |> Seq.map (fun v -> sprintf "| %A -> %A" v (f v))
                                    |> String.concat "\r\n"

                                mi, sprintf "%s\r\n|> ASet.choose (\r\n  function\r\n%s\r\n)" (indent input) (indent table)
                        )
                        value.schanges
            }
                 
        let filter<'a>() =
            gen {
                let mySize = ref 0
                let! value = Arb.generate<_> |> Gen.scaleSize (fun s -> mySize := s; s - 2)
                //let! f = Arb.generate<'a -> 'b> |> Gen.scaleSize (fun _ -> 50)
                let table, f = randomFunction<'a, bool> (!mySize / 2)
                return 
                    create 
                        (Adaptive.ASet.filter f value.sreal)
                        (Reference.ASet.filter f value.sref)
                        (function
                            | false -> 
                                let m, v = value.sexpression false
                                m, sprintf "filter (\r\n%s\r\n)" (indent v)
                            | true ->
                                let realContent = value.sref.Content |> Reference.AVal.force
                                let mi, input = value.sexpression true

                                let table =
                                    realContent 
                                    |> Seq.map (fun v -> sprintf "| %A -> %A" v (f v))
                                    |> String.concat "\r\n"

                                mi, sprintf "%s\r\n|> ASet.filter (\r\n  function\r\n%s\r\n)" (indent input) (indent table)
                        )
                        value.schanges
            }
                
        let ofAList<'a>() =
            gen {
                let! value = Arb.generate<VList<'a>> |> Gen.scaleSize (fun s -> 0)

                let reference =
                    value.lref.Content |> Reference.AVal.map HashSet.ofSeq |> Reference.ASet.ofRef

                return 
                    create 
                        (Adaptive.CollectionExtensions.ASet.ofAList value.lreal)
                        reference
                        (function
                            | false -> 
                                let m, v = value.lexpression false
                                m, sprintf "ofAList (\r\n%s\r\n)" (indent v)
                            | true ->
                                let realContent = value.lref.Content |> Reference.AVal.force
                                let mi, input = value.lexpression true

                                mi, sprintf "%s\r\n|> ASet.ofAList" (indent input)
                        )
                        value.lchanges
            }

        let union<'a> () =
            gen {
                let! a = Arb.generate<VSet<'a>> |> Gen.scaleSize (fun v -> v / 2)
                let! b = Arb.generate<VSet<'a>> |> Gen.scaleSize (fun v -> v / 2)
                return 
                    create 
                        (Adaptive.ASet.union a.sreal b.sreal)
                        (Reference.ASet.union a.sref b.sref)
                        (fun verbose ->
                            let ma, a = a.sexpression verbose
                            let mb, b = b.sexpression verbose
                            let m = Map.union ma mb

                            m, sprintf "union\r\n%s\r\n%s" (indent a) (indent b)
                        )
                        (fun () -> a.schanges() @ b.schanges())

            }

        let unionMany<'a> () =
            gen {
                let! a = 
                    Arb.generate<VSet<VSet<'a>>> 
                    |> Gen.scaleSize (fun v -> 
                        if v <= 1 then 0
                        else int (sqrt (float v))
                    )

                let real = a.sreal |> Adaptive.ASet.map (fun v -> v.sreal)
                let ref = a.sref |> Reference.ASet.map (fun v -> v.sref)

                return 
                    create 
                        (Adaptive.ASet.unionMany real)
                        (Reference.ASet.unionMany ref)
                        (fun verbose ->
                            let m, e = a.sexpression verbose
                            let current = a.sref.Content |> Reference.AVal.force

                            let current, m = 
                                (([], m), current)
                                ||> Seq.fold (fun (str, map) s -> 
                                    let m, a = s.sexpression verbose
                                    (str @ [a], Map.union map m)
                                )

                            let current =
                                current |> String.concat "\r\n"

                            m, sprintf "unionMany\r\n%s\r\n%s" (indent e) (indent (indent current))
                        )
                        (fun () -> 
                            let current = 
                                a.sref.Content 
                                |> Reference.AVal.force
                                |> Seq.collect (fun s -> s.schanges())
                                |> Seq.toList
                            
                            a.schanges() @ current
                        )

            }

        let ofAVal<'a> () =
            gen {
                let! a = Arb.generate<VVal<HashSet<'a>>> |> Gen.scaleSize (fun v -> 0)
                return 
                    create
                        (a.real |> Adaptive.ASet.ofAVal)
                        (a.ref |> Reference.ASet.ofAVal)
                        (fun _ -> Map.empty, sprintf "ofAVal\r\n%s" (indent a.expression))
                        (fun () -> a.changes())
            }

        let bind<'a, 'b>() =
            gen {
                let mySize = ref 0
                let! value = Arb.generate<VVal<'a>> |> Gen.scaleSize (fun s -> mySize := s; 0)
                //let! f = Arb.generate<'a -> 'b> |> Gen.scaleSize (fun _ -> 50)
                let table, mapping = randomFunction<'a, VSet<'b>> (!mySize - 1)

                
                let mutable latest = None

                let getChanges() =
                    match latest with
                    | Some l -> List.append (l.schanges()) (value.changes())
                    | None -> value.changes()

                let mapping (input : 'a) =
                    let res = mapping input
                    latest <- Some res
                    res


                return 
                    create 
                        (Adaptive.ASet.bind (fun a -> (mapping a).sreal) value.real)
                        (Reference.ASet.bind (fun a -> (mapping a).sref) value.ref)
                        (fun _ -> 
                            let v = value.expression
                            Map.empty, sprintf "bind (\r\n%s\r\n)" (indent v)
                        )
                        getChanges
            }
                
        let collect<'a, 'b>() =
            gen {
                let mySize = ref 0
                let! value = Arb.generate<_> |> Gen.scaleSize (fun s -> mySize := s; s - 1)
                //let! f = Arb.generate<'a -> 'b> |> Gen.scaleSize (fun _ -> 50)
                let innerSize = 
                    let s = !mySize
                    if s > 0 then int (sqrt (float s))
                    else 0
                let table, mapping = randomFunction<'a, VSet<'b>> innerSize

                let cache = Cache<'a, VSet<'b>>(mapping)

                let getChanges() =
                    value.sref.Content 
                    |> Reference.AVal.force 
                    |> Seq.toList 
                    |> List.collect (fun v -> cache.Invoke(v).schanges())
                    |> List.append (value.schanges())

                let mapping (input : 'a) = cache.Invoke input
                return 
                    create 
                        (Adaptive.ASet.collect (fun a -> (mapping a).sreal) value.sreal)
                        (Reference.ASet.collect (fun a -> (mapping a).sref) value.sref)
                        (function
                            | false ->  
                                let m, v = value.sexpression false
                                m, sprintf "collect (\r\n%s\r\n)" (indent v)
                            | true ->
                                let realContent = value.sref.Content |> Reference.AVal.force
                                let it, input = value.sexpression true

                                let maps, kv =
                                    realContent 
                                    |> Seq.toList
                                    |> List.map (fun v -> 
                                        let m, b = (mapping v).sexpression true
                                        m, (v,b)
                                    )
                                    |> List.unzip
                                    //|> String.concat "\r\n"
                                    
                                let table = 
                                    kv 
                                    |> List.map (fun (k,v) -> sprintf "| %A ->\r\n  %s" k v)
                                    |> String.concat "\r\n"

                                let res = 
                                    maps 
                                    |> Seq.map (Map.toSeq >> HashMap.ofSeq) 
                                    |> Seq.fold HashMap.union (HashMap.ofSeq (Map.toSeq it))
                                    |> Map.ofSeq

                                res, sprintf "%s\r\n  |> ASet.collect (\r\n    function\r\n%s\r\n  )" input (indent (indent table))
                        )
                        getChanges
            }
      
        let ofAMap<'a, 'b> () =
            gen {
                let! a = Arb.generate<VMap<'a, 'b>> |> Gen.scaleSize (fun v -> 0)
                return 
                    create 
                        (Adaptive.AMap.toASet a.mreal |> ASet.map fst)
                        (Reference.AMap.toASet a.mref |> Reference.ASet.map fst)
                        (fun verbose ->
                            let ma, a = a.mexpression verbose
                            ma, sprintf "ofAMap\r\n%s" (indent a)
                        )
                        (fun () -> a.mchanges())

            }
       
        let aMapKeys<'a, 'b> () =
            gen {
                let! a = Arb.generate<VMap<'a, 'b>> |> Gen.scaleSize (fun v -> 0)
                return 
                    create 
                        (Adaptive.CollectionExtensions.AMap.keys a.mreal)
                        (Reference.AMap.toASet a.mref |> Reference.ASet.map fst)
                        (fun verbose ->
                            let ma, a = a.mexpression verbose
                            ma, sprintf "aMapKeys\r\n%s" (indent a)
                        )
                        (fun () -> a.mchanges())

            }
     
    module VMap =
        let mutable cid = 0

        let create a b s c = 
            {
                mreal = a
                mref = b
                mexpression = s
                mchanges = c
            }
        
        let constant<'a, 'b>() =
            gen {
                let! value = Arb.generate<HashMap<'a, 'b>>

                let asString = sprintf "%A" value
                return 
                    create
                        (Adaptive.AMap.ofSeq value)
                        (Reference.AMap.ofSeq value)
                        (fun _ -> Map.empty, sprintf "v(%s)" asString)
                        (fun () -> [])
            }

        let init<'a, 'b>() =
            gen {
                let id = System.Threading.Interlocked.Increment(&cid)
                let! value = Arb.generate<HashMap<'a, 'b>>

                let real = Adaptive.cmap value
                let ref = Reference.cmap value

                let change =    
                    { 
                        cell = (real, id) :> obj
                        change = 
                            gen {
                                let! newValue = Arb.generate<HashMap<'a, 'b>>
                                return fun () ->    
                                    real.Value <- newValue
                                    ref.Value <- newValue
                                    sprintf "C%d <- %A" id newValue

                            }
                    }

                return 
                    create 
                        (real :> Adaptive.amap<_,_>)
                        (ref :> Reference.amap<_,_>)
                        (function 
                            | false -> 
                                Map.empty, sprintf "c%d" id
                            | true -> 
                                let c = real.Value |> Seq.map (sprintf "%A") |> String.concat "; "
                                let m = Map.ofList [sprintf "c%d" id, sprintf "cmap [%s]" c]
                                m, sprintf "c%d" id
                        )
                        (fun () -> [change])
            }

        let map<'a, 'b, 'c>() =
            gen {
                let mySize = ref 0
                let! value = Arb.generate<_> |> Gen.scaleSize (fun s -> mySize := s; s - 2)
                //let! f = Arb.generate<'a -> 'b> |> Gen.scaleSize (fun _ -> 50)
                let table, f = randomFunction<'a * 'b, 'c> (!mySize / 2)

                let mapping k v = f(k,v)

                return 
                    create 
                        (Adaptive.AMap.map mapping value.mreal)
                        (Reference.AMap.map mapping value.mref)
                        (function
                            | false -> 
                                let m, v = value.mexpression false
                                m, sprintf "map (\r\n%s\r\n)" (indent v)
                            | true ->
                                let realContent = value.mref.Content |> Reference.AVal.force
                                let mi, input = value.mexpression true

                                let table =
                                    realContent 
                                    |> Seq.map (fun v -> sprintf "| %A -> %A" v (f v))
                                    |> String.concat "\r\n"

                                mi, sprintf "%s\r\n|> AMap.map (\r\n  function\r\n%s\r\n)" (indent input) (indent table)
                        )
                        value.mchanges
            }

        let choose<'a, 'b, 'c>() =
            gen {
                let mySize = ref 0
                let! value = Arb.generate<_> |> Gen.scaleSize (fun s -> mySize := s; s - 2)
                //let! f = Arb.generate<'a -> 'b> |> Gen.scaleSize (fun _ -> 50)
                let table, f = randomFunction<'a * 'b, Option<'c>> (!mySize / 2)

                let mapping k v = f(k,v)

                return 
                    create 
                        (Adaptive.AMap.choose mapping value.mreal)
                        (Reference.AMap.choose mapping value.mref)
                        (function
                            | false -> 
                                let m, v = value.mexpression false
                                m, sprintf "choose (\r\n%s\r\n)" (indent v)
                            | true ->
                                let realContent = value.mref.Content |> Reference.AVal.force
                                let mi, input = value.mexpression true

                                let table =
                                    realContent 
                                    |> Seq.map (fun v -> sprintf "| %A -> %A" v (f v))
                                    |> String.concat "\r\n"

                                mi, sprintf "%s\r\n|> AMap.choose (\r\n  function\r\n%s\r\n)" (indent input) (indent table)
                        )
                        value.mchanges
            }
            
        let filter<'a, 'b>() =
            gen {
                let mySize = ref 0
                let! value = Arb.generate<_> |> Gen.scaleSize (fun s -> mySize := s; s - 2)
                //let! f = Arb.generate<'a -> 'b> |> Gen.scaleSize (fun _ -> 50)
                let table, f = randomFunction<'a * 'b, bool> (!mySize / 2)

                let mapping k v = f(k,v)

                return 
                    create 
                        (Adaptive.AMap.filter mapping value.mreal)
                        (Reference.AMap.filter mapping value.mref)
                        (function
                            | false -> 
                                let m, v = value.mexpression false
                                m, sprintf "filter (\r\n%s\r\n)" (indent v)
                            | true ->
                                let realContent = value.mref.Content |> Reference.AVal.force
                                let mi, input = value.mexpression true

                                let table =
                                    realContent 
                                    |> Seq.map (fun v -> sprintf "| %A -> %A" v (f v))
                                    |> String.concat "\r\n"

                                mi, sprintf "%s\r\n|> AMap.filter (\r\n  function\r\n%s\r\n)" (indent input) (indent table)
                        )
                        value.mchanges
            }

        let map'<'a, 'b, 'c>() : Gen<VMap<'a, 'c>> =
            gen {
                let mySize = ref 0
                let! value = Arb.generate<_> |> Gen.scaleSize (fun s -> mySize := s; s - 2)
                //let! f = Arb.generate<'a -> 'b> |> Gen.scaleSize (fun _ -> 50)
                let table, f = randomFunction<'b, 'c> (!mySize / 2)

                return 
                    create 
                        (Adaptive.AMap.map' f value.mreal)
                        (Reference.AMap.map' f value.mref)
                        (function
                            | false -> 
                                let m, v = value.mexpression false
                                m, sprintf "map' (\r\n%s\r\n)" (indent v)
                            | true ->
                                let realContent = value.mref.Content |> Reference.AVal.force
                                let mi, input = value.mexpression true

                                let table =
                                    realContent 
                                    |> Seq.map (fun (_,v) -> sprintf "| %A -> %A" v (f v))
                                    |> String.concat "\r\n"

                                mi, sprintf "%s\r\n|> AMap.map' (\r\n  function\r\n%s\r\n)" (indent input) (indent table)
                        )
                        value.mchanges
            }

        let choose'<'a, 'b, 'c>() : Gen<VMap<'a, 'c>> =
            gen {
                let mySize = ref 0
                let! value = Arb.generate<_> |> Gen.scaleSize (fun s -> mySize := s; s - 2)
                //let! f = Arb.generate<'a -> 'b> |> Gen.scaleSize (fun _ -> 50)
                let table, f = randomFunction<'b, Option<'c>> (!mySize / 2)


                return 
                    create 
                        (Adaptive.AMap.choose' f value.mreal)
                        (Reference.AMap.choose' f value.mref)
                        (function
                            | false -> 
                                let m, v = value.mexpression false
                                m, sprintf "choose' (\r\n%s\r\n)" (indent v)
                            | true ->
                                let realContent = value.mref.Content |> Reference.AVal.force
                                let mi, input = value.mexpression true

                                let table =
                                    realContent 
                                    |> Seq.map (fun (_,v) -> sprintf "| %A -> %A" v (f v))
                                    |> String.concat "\r\n"

                                mi, sprintf "%s\r\n|> AMap.choose' (\r\n  function\r\n%s\r\n)" (indent input) (indent table)
                        )
                        value.mchanges
            }
            
        let filter'<'a, 'b>() : Gen<VMap<'a, 'b>> =
            gen {
                let mySize = ref 0
                let! value = Arb.generate<_> |> Gen.scaleSize (fun s -> mySize := s; s - 2)
                //let! f = Arb.generate<'a -> 'b> |> Gen.scaleSize (fun _ -> 50)
                let table, f = randomFunction<'b, bool> (!mySize / 2)

                return 
                    create 
                        (Adaptive.AMap.filter' f value.mreal)
                        (Reference.AMap.filter' f value.mref)
                        (function
                            | false -> 
                                let m, v = value.mexpression false
                                m, sprintf "filter' (\r\n%s\r\n)" (indent v)
                            | true ->
                                let realContent = value.mref.Content |> Reference.AVal.force
                                let mi, input = value.mexpression true

                                let table =
                                    realContent 
                                    |> Seq.map (fun (_,v) -> sprintf "| %A -> %A" v (f v))
                                    |> String.concat "\r\n"

                                mi, sprintf "%s\r\n|> AMap.filter' (\r\n  function\r\n%s\r\n)" (indent input) (indent table)
                        )
                        value.mchanges
            }
            
        let union<'a, 'b> () =
            gen {
                let! a = Arb.generate<VMap<'a, 'b>> |> Gen.scaleSize (fun v -> v / 2)
                let! b = Arb.generate<VMap<'a, 'b>> |> Gen.scaleSize (fun v -> v / 2)
                return 
                    create 
                        (Adaptive.AMap.union a.mreal b.mreal)
                        (Reference.AMap.union a.mref b.mref)
                        (fun verbose ->
                            let ma, a = a.mexpression verbose
                            let mb, b = b.mexpression verbose
                            let m = Map.union ma mb

                            m, sprintf "union\r\n%s\r\n%s" (indent a) (indent b)
                        )
                        (fun () -> a.mchanges() @ b.mchanges())

            }

        let mapSet<'a, 'b>() =
            gen {
                let mySize = ref 0
                let! value = Arb.generate<VSet<'a>> |> Gen.scaleSize (fun s -> mySize := s; 0)
                //let! f = Arb.generate<'a -> 'b> |> Gen.scaleSize (fun _ -> 50)
                let table, f = randomFunction<'a, 'b> (!mySize - 1)

                return 
                    create 
                        (Adaptive.AMap.mapSet f value.sreal)
                        (Reference.AMap.mapSet f value.sref)
                        (function
                            | false -> 
                                let m, v = value.sexpression false
                                m, sprintf "mapSet (\r\n%s\r\n)" (indent v)
                            | true ->
                                let realContent = value.sref.Content |> Reference.AVal.force
                                let mi, input = value.sexpression true

                                let table =
                                    realContent 
                                    |> Seq.map (fun v -> sprintf "| %A -> %A" v (f v))
                                    |> String.concat "\r\n"

                                mi, sprintf "%s\r\n|> AMap.mapSet (\r\n  function\r\n%s\r\n)" (indent input) (indent table)
                        )
                        value.schanges
            }

        let ofAVal<'a, 'b> () =
            gen {
                let! a = Arb.generate<VVal<HashMap<'a, 'b>>> |> Gen.scaleSize (fun v -> 0)
                return 
                    create
                        (a.real |> Adaptive.AMap.ofAVal)
                        (a.ref |> Reference.AMap.ofAVal)
                        (fun _ -> Map.empty, sprintf "ofAVal\r\n%s" (indent a.expression))
                        (fun () -> a.changes())
            }

        let bind<'a, 'b, 'c>() =
            gen {
                let mySize = ref 0
                let! value = Arb.generate<VVal<'a>> |> Gen.scaleSize (fun s -> mySize := s; 0)
                //let! f = Arb.generate<'a -> 'b> |> Gen.scaleSize (fun _ -> 50)
                let table, mapping = randomFunction<'a, VMap<'b, 'c>> (!mySize - 1)

                
                let mutable latest = None

                let getChanges() =
                    match latest with
                    | Some l -> List.append (l.mchanges()) (value.changes())
                    | None -> value.changes()

                let mapping (input : 'a) =
                    let res = mapping input
                    latest <- Some res
                    res


                return 
                    create 
                        (Adaptive.AMap.bind (fun a -> (mapping a).mreal) value.real)
                        (Reference.AMap.bind (fun a -> (mapping a).mref) value.ref)
                        (fun _ -> 
                            let v = value.expression
                            Map.empty, sprintf "bind (\r\n%s\r\n)" (indent v)
                        )
                        getChanges
            }

    module List =
        let mutable cid = 0

        let create a b s c = 
            {
                lreal = a
                lref = b
                lexpression = s
                lchanges = c
            }



        let init<'a>() =
            gen {
                let id = System.Threading.Interlocked.Increment(&cid)
                let! value = Arb.generate<IndexList<'a>>

                let real = Adaptive.clist value
                let ref = Reference.clist value

                let change =    
                    { 
                        cell = (real, id) :> obj
                        change = 
                            gen {
                                let! newValue = Arb.generate<IndexList<'a>>
                                return fun () ->    
                                    real.Value <- newValue
                                    ref.Value <- newValue
                                    sprintf "C%d <- %A" id newValue

                            }
                    }

                return 
                    create 
                        (real :> Adaptive.alist<_>)
                        (ref :> Reference.alist<_>)
                        (function 
                            | false -> 
                                Map.empty, sprintf "c%d" id
                            | true -> 
                                let c = real.Value |> Seq.map (sprintf "%A") |> String.concat "; "
                                let m = Map.ofList [sprintf "c%d" id, sprintf "clist [%s]" c]
                                m, sprintf "c%d" id
                        )
                        (fun () -> [change])
            }

        let constant<'a>() =
            gen {
                let! value = Arb.generate<IndexList<'a>>
                let id = System.Threading.Interlocked.Increment(&cid)

                return 
                    create
                        (Adaptive.AList.ofIndexList value)
                        (Reference.AList.ofIndexList value)
                        (function 
                            | false -> Map.empty, sprintf "%A" value
                            | true -> 
                                let m = Map.ofList [sprintf "v%d" id, sprintf "AList.ofList [%s]" (value |> Seq.map (sprintf "%A") |> String.concat "; ")]
                                m, sprintf "v%d" id
                        )
                        (fun () -> [])
            }

        let ofAVal<'a> () =
            gen {
                let! a = Arb.generate<VVal<IndexList<'a>>> |> Gen.scaleSize (fun v -> 0)
                return 
                    create
                        (a.real |> Adaptive.AList.ofAVal)
                        (a.ref |> Reference.AList.ofRef)
                        (fun _ -> Map.empty, sprintf "ofAVal\r\n%s" (indent a.expression))
                        (fun () -> a.changes())
            }


        let map<'a, 'b>() =
            gen {
                let mySize = ref 0
                let! value = Arb.generate<_> |> Gen.scaleSize (fun s -> mySize := s; s - 2)
                //let! f = Arb.generate<'a -> 'b> |> Gen.scaleSize (fun _ -> 50)
                let table, f = randomFunction<'a, 'b> (!mySize / 2)

                let mapping i v =  f(v)

                return 
                    create 
                        (Adaptive.AList.mapi mapping value.lreal)
                        (Reference.AList.mapi mapping value.lref)
                        (function
                            | false -> 
                                let m, v = value.lexpression false
                                m, sprintf "map (\r\n%s\r\n)" (indent v)
                            | true ->
                                let realContent = value.lref.Content |> Reference.AVal.force
                                let mi, input = value.lexpression true

                                let table =
                                    realContent .Content
                                    |> Seq.map (fun (KeyValue(i, v)) -> sprintf "| %A, %A -> %A" i v (mapping i v))
                                    |> String.concat "\r\n"

                                mi, sprintf "%s\r\n|> AList.map (\r\n  function\r\n%s\r\n)" (indent input) (indent table)
                        )
                        value.lchanges
            }

        let choose<'a, 'b>() =
            gen {
                let mySize = ref 0
                let! value = Arb.generate<_> |> Gen.scaleSize (fun s -> mySize := s; s - 2)
                //let! f = Arb.generate<'a -> 'b> |> Gen.scaleSize (fun _ -> 50)
                let table, f = randomFunction<'a, option<'b>> (!mySize / 2)
                let mapping i v = f(v)

                return 
                    create 
                        (Adaptive.AList.choosei mapping value.lreal)
                        (Reference.AList.choosei mapping value.lref)
                        (function
                            | false -> 
                                let m, v = value.lexpression false
                                m, sprintf "choose (\r\n%s\r\n)" (indent v)
                            | true ->
                                let realContent = value.lref.Content |> Reference.AVal.force
                                let mi, input = value.lexpression true

                                let table =
                                    realContent.Content
                                    |> Seq.map (fun (KeyValue(i, v)) -> sprintf "| %A,%A -> %A" i v (mapping i v))
                                    |> String.concat "\r\n"

                                mi, sprintf "%s\r\n|> AList.choose (\r\n  function\r\n%s\r\n)" (indent input) (indent table)
                        )
                        value.lchanges
            }

        let filter<'a>() =
            gen {
                let mySize = ref 0
                let! value = Arb.generate<_> |> Gen.scaleSize (fun s -> mySize := s; s - 2)
                //let! f = Arb.generate<'a -> 'b> |> Gen.scaleSize (fun _ -> 50)
                let table, f = randomFunction<'a, bool> (!mySize / 2)
                let pred i v = f(v)
                return 
                    create 
                        (Adaptive.AList.filteri pred value.lreal)
                        (Reference.AList.filteri pred value.lref)
                        (function
                            | false -> 
                                let m, v = value.lexpression false
                                m, sprintf "filter (\r\n%s\r\n)" (indent v)
                            | true ->
                                let realContent = value.lref.Content |> Reference.AVal.force
                                let mi, input = value.lexpression true

                                let table =
                                    realContent.Content
                                    |> Seq.map (fun (KeyValue(i,v)) -> sprintf "| %A,%A -> %A" i v (pred i v))
                                    |> String.concat "\r\n"

                                mi, sprintf "%s\r\n|> AList.filter (\r\n  function\r\n%s\r\n)" (indent input) (indent table)
                        )
                        value.lchanges
            }
            
        let append<'a>() =
            gen {
                let! a = Arb.generate<VList<'a>> |> Gen.scaleSize (fun v -> v / 2)
                let! b = Arb.generate<VList<'a>> |> Gen.scaleSize (fun v -> v / 2)
                return 
                    create 
                        (Adaptive.AList.append a.lreal b.lreal)
                        (Reference.AList.append a.lref b.lref)
                        (fun verbose ->
                            let ma, a = a.lexpression verbose
                            let mb, b = b.lexpression verbose
                            let m = Map.union ma mb

                            m, sprintf "append\r\n%s\r\n%s" (indent a) (indent b)
                        )
                        (fun () -> a.lchanges() @ b.lchanges())

            }

        let sortBy<'a>() =
            gen {
                let! list = Arb.generate<VList<'a>> |> Gen.scaleSize (fun v -> v - 1)
                
                let mapping = 
                    let _, mapping = randomFunction<'a, int> 20
                    mapping

                return 
                    create 
                        (Adaptive.AList.sortBy mapping list.lreal)
                        (Reference.AList.sortBy mapping list.lref)
                        (fun verbose ->
                            let ma, a = list.lexpression verbose
                            ma, sprintf "sortBy\r\n%s" (indent a)
                        )
                        list.lchanges

            }
            

        let sortWith<'a>() =
            gen {
                let! list = Arb.generate<VList<'a>> |> Gen.scaleSize (fun v -> v - 1)
                
                let mapping = 
                    let _, mapping = randomFunction<'a, int> 20
                    mapping

                let compare (l : 'a) (r : 'a) =
                    compare (mapping l) (mapping r)

                return 
                    create 
                        (Adaptive.AList.sortWith compare list.lreal)
                        (Reference.AList.sortWith compare list.lref)
                        (fun verbose ->
                            let ma, a = list.lexpression verbose
                            ma, sprintf "sortWith\r\n%s" (indent a)
                        )
                        list.lchanges

            }
            

        let setSortBy<'a>() =
            gen {
                let! list = Arb.generate<VSet<'a>> |> Gen.scaleSize (fun v -> 0)
                
                let mapping = unbox
                    //let _, mapping = randomFunction<'a, int> 20
                    //mapping

                let reference =
                    list.sref.Content |> Reference.AVal.map (fun set ->
                        set 
                        |> Seq.indexed 
                        |> Seq.sortBy (fun (i,k) -> mapping k, i) 
                        |> Seq.map snd 
                        |> IndexList.ofSeq

                    ) |> Reference.AList.ofRef

                return 
                    create 
                        (Adaptive.CollectionExtensions.ASet.sortBy mapping list.sreal)
                        (reference)
                        (fun verbose ->
                            let ma, a = list.sexpression verbose
                            ma, sprintf "setSortBy\r\n%s" (indent a)
                        )
                        list.schanges

            }
            

        let setSortWith<'a>() =
            gen {
                let! list = Arb.generate<VSet<'a>> |> Gen.scaleSize (fun v -> 0)
                
                let compare = Unchecked.compare
                    //let _, mapping = randomFunction<'a, int> 20
                    //mapping

                let reference =
                    list.sref.Content |> Reference.AVal.map (fun set ->
                        set 
                        |> Seq.sortWith compare
                        |> IndexList.ofSeq
                    ) |> Reference.AList.ofRef

                return 
                    create 
                        (Adaptive.CollectionExtensions.ASet.sortWith compare list.sreal)
                        (reference)
                        (fun verbose ->
                            let ma, a = list.sexpression verbose
                            ma, sprintf "setSortWith\r\n%s" (indent a)
                        )
                        list.schanges

            }
            


        let collect<'a, 'b>() =
            gen {
                let mySize = ref 0
                let! value = 
                    Arb.generate<_> |> Gen.scaleSize (fun s -> 
                        mySize := 0
                            //if s <= 1 then 0
                            //else int (sqrt (float s))
                        !mySize
                    )
                //let! f = Arb.generate<'a -> 'b> |> Gen.scaleSize (fun _ -> 50)
                let innerSize =  !mySize
                let table, mapping = randomFunction<Index * 'a, VList<'b>> innerSize

                //let cache = Cache<Index * 'a, VList<'b>>(mapping)

                let getChanges() =
                    (Reference.AVal.force  value.lref.Content).Content
                    |> MapExt.toList 
                    |> List.collect (fun v -> table.Invoke(v).lchanges())
                    |> List.append (value.lchanges())

                let mapping (i : Index) (input : 'a) = mapping(i, input)
                return 
                    create 
                        (Adaptive.AList.collecti (fun i a -> (mapping i a).lreal) value.lreal)
                        (Reference.AList.collecti (fun i a -> (mapping i a).lref) value.lref)
                        (function
                            | false ->  
                                let m, v = value.lexpression false
                                m, sprintf "collect (\r\n%s\r\n)" (indent v)
                            | true ->
                                let realContent = value.lref.Content |> Reference.AVal.force
                                let it, input = value.lexpression true

                                let maps, kv =
                                    realContent.Content
                                    |> MapExt.toList
                                    |> List.map (fun (i, v) -> 
                                        let m, b = (mapping i v).lexpression true
                                        m, (v,b)
                                    )
                                    |> List.unzip
                                    //|> String.concat "\r\n"
                                    
                                let table = 
                                    kv 
                                    |> List.map (fun (k,v) -> sprintf "| %A ->\r\n  %s" k v)
                                    |> String.concat "\r\n"

                                let res = 
                                    maps 
                                    |> Seq.map (Map.toSeq >> HashMap.ofSeq) 
                                    |> Seq.fold HashMap.union (HashMap.ofSeq (Map.toSeq it))
                                    |> Map.ofSeq

                                res, sprintf "%s\r\n  |> AList.collect (\r\n    function\r\n%s\r\n  )" input (indent (indent table))
                        )
                        getChanges
            }
    
[<Struct; CustomEquality; NoComparison>]
type StupidHash(v : int) =
    member x.Value = v
    override x.GetHashCode() = v % 2
    override x.Equals o =
        match o with
        | :? StupidHash as o -> v = o.Value
        | _ -> false

    override x.ToString() = string v

type AdaptiveGenerators() =

    static let relevantTypes = 
        [
            typeof<StupidHash>
            typeof<int>
            typeof<obj>
            //typeof<HashSet<int>>
            //typeof<HashSet<obj>>
        ]

    static member StupidHash() =
        { new Arbitrary<StupidHash>() with
            member x.Generator =
                Arb.generate<int> |> Gen.map StupidHash
            member x.Shrinker _ =
                Seq.empty
        }
        

        
    static member IndexList<'a>() =
        { new Arbitrary<IndexList<'a>>() with
            member x.Generator =
                Arb.generate<list<'a>> |> Gen.map IndexList.ofList
            member x.Shrinker _ =
                Seq.empty
        }

    static member HashSet<'a>() =
        { new Arbitrary<HashSet<'a>>() with
            member x.Generator =
                Arb.generate<list<'a>> |> Gen.map HashSet.ofList
            member x.Shrinker _ =
                Seq.empty
        }
       
    static member HashMap<'a, 'b>() =
        { new Arbitrary<HashMap<'a, 'b>>() with
            member x.Generator =
                Arb.generate<list<'a * 'b>> |> Gen.map HashMap.ofList
            member x.Shrinker _ =
                Seq.empty
        }
 
    static member Val<'a>() = 
        { new Arbitrary<VVal<'a>>() with
            member x.Generator =
                Gen.sized (fun size ->
                    gen {
                        let! kind = 
                            if size = 0 then
                                Gen.frequency [
                                    1, Gen.constant "constant"
                                    5, Gen.constant "cval"
                                ]
                            else 
                                Gen.frequency [
                                    1, Gen.constant "constant"
                                    5, Gen.constant "cval"
                                    5, Gen.constant "map"
                                    5, Gen.constant "map2"
                                    5, Gen.constant "map3"
                                    5, Gen.constant "bind"
                                    5, Gen.constant "bind2"
                                    3, Gen.constant "ofASet"
                                    3, Gen.constant "ofAMap"
                                    
                                ]
                        match kind with
                        | "constant" -> return! Generators.Val.constant<'a>()
                        | "cval" -> return! Generators.Val.init<'a>()
                        | "ofASet" -> 
                            let! t = Gen.elements relevantTypes
                            return!
                                t.Visit { new TypeVisitor<_> with member __.Accept<'z>() = Generators.Val.ofASet<'z, 'a>() }
                        | "ofAMap" -> 
                            let! t1 = Gen.elements relevantTypes
                            let! t2 = Gen.elements relevantTypes
                            return!
                                (t1, t2) ||> visit2 { new TypeVisitor2<_> with 
                                    member __.Accept<'t1, 't2>() = Generators.Val.ofAMap<'t1, 't2, 'a>() 
                                }
                        
                        
                        | "map" ->
                            let! t = Gen.elements relevantTypes
                            return!
                                t.Visit { new TypeVisitor<_> with member __.Accept<'z>() = Generators.Val.map<'z, 'a>() }
                        | "map2" ->
                            let! t1 = Gen.elements relevantTypes
                            let! t2 = Gen.elements relevantTypes
                            return!
                                (t1, t2) ||> visit2 { new TypeVisitor2<_> with 
                                    member __.Accept<'t1, 't2>() =
                                        Generators.Val.map2<'t1, 't2, 'a>()
                                }
                                
                        | "map3" ->
                            let! t1 = Gen.elements relevantTypes
                            let! t2 = Gen.elements relevantTypes
                            let! t3 = Gen.elements relevantTypes
                            return!
                                (t1, t2, t3) |||> visit3 { new TypeVisitor3<_> with 
                                    member __.Accept<'t1, 't2, 't3>() =
                                        Generators.Val.map3<'t1, 't2, 't3, 'a>()
                                }

                        | "bind" ->
                            let! t = Gen.elements relevantTypes
                            return!
                                t.Visit { new TypeVisitor<_> with
                                member __.Accept<'z>() =
                                    Generators.Val.bind<'z, 'a>()
                                }

                        | "bind2" ->
                            let! t1 = Gen.elements relevantTypes
                            let! t2 = Gen.elements relevantTypes
                            return!
                                (t1, t2) ||> visit2 { new TypeVisitor2<_> with 
                                    member __.Accept<'t1, 't2>() =
                                        Generators.Val.bind2<'t1, 't2, 'a>()
                                }
                                
                        | _ ->
                            return failwith ""
                    }
                )
            member __.Shrinker _ = 
                Seq.empty
        }

    static member Set<'a>() = 
        { new Arbitrary<VSet<'a>>() with
            member x.Generator =
                Gen.sized (fun size ->
                    gen {
                        let! kind = 
                            if size = 0 then
                                Gen.frequency [
                                    1, Gen.constant "constant"
                                    5, Gen.constant "cset"
                                ]
                            else 
                                Gen.frequency [
                                    1, Gen.constant "constant"
                                    3, Gen.constant "cset"
                                    3, Gen.constant "map"
                                    3, Gen.constant "choose"
                                    3, Gen.constant "filter"
                                    3, Gen.constant "union"
                                    2, Gen.constant "collect"
                                    1, Gen.constant "unionMany"
                                    1, Gen.constant "aval"
                                    1, Gen.constant "bind"
                                    3, Gen.constant "ofAMap"
                                    3, Gen.constant "aMapKeys"
                                    3, Gen.constant "ofAList"
                                ]
                        match kind with
                        | "constant" -> 
                            return! Generators.Set.constant<'a>()
                        | "cset" -> 
                            return! Generators.Set.init<'a>()
                        | "union" ->
                            return! Generators.Set.union<'a>()
                        | "unionMany" ->
                            return! Generators.Set.unionMany<'a>()
                        | "aval" ->
                            return! Generators.Set.ofAVal<'a>() 
                        | "filter" ->
                            return! Generators.Set.filter<'a>()
                        | "ofAList" ->
                            return! Generators.Set.ofAList<'a>()

                        | "aMapKeys" -> 
                            let! t = Gen.elements relevantTypes
                            return!
                                t |> visit { new TypeVisitor<_> with 
                                    member __.Accept<'z>() = Generators.Set.aMapKeys<'a, 'z>() 
                                }

                        | "ofAMap" ->
                            let! t = Gen.elements relevantTypes
                            return!
                                t |> visit { new TypeVisitor<_> with 
                                    member __.Accept<'z>() = Generators.Set.ofAMap<'a, 'z>() 
                                }

                        | "map" -> 
                            let! t = Gen.elements relevantTypes
                            return!
                                t |> visit { new TypeVisitor<_> with 
                                    member __.Accept<'z>() = Generators.Set.map<'z, 'a>() 
                                }
                        | "bind" -> 
                            let! t = Gen.elements relevantTypes
                            return!
                                t |> visit { new TypeVisitor<_> with 
                                    member __.Accept<'z>() = Generators.Set.bind<'z, 'a>() 
                                }
                        
                        | "choose" -> 
                            let! t = Gen.elements relevantTypes
                            return!
                                t |> visit { new TypeVisitor<_> with 
                                    member __.Accept<'z>() = Generators.Set.choose<'z, 'a>() 
                                }
                        | "collect" -> 
                            let! t = Gen.elements relevantTypes
                            return!
                                t |> visit { new TypeVisitor<_> with 
                                    member __.Accept<'z>() = Generators.Set.collect<'z, 'a>() 
                                }
                        | kind ->
                            return failwithf "unknown operation: %s" kind
                    }
                )
            member x.Shrinker _ =
                Seq.empty
        }

    static member List<'a>() = 
        { new Arbitrary<VList<'a>>() with
            member x.Generator =
                Gen.sized (fun size ->
                    gen {
                        let! kind = 
                            if size = 0 then
                                Gen.frequency [
                                    1, Gen.constant "constant"
                                    5, Gen.constant "clist"
                                ]
                            else 
                                Gen.frequency [
                                    yield 1, Gen.constant "constant"
                                    yield 3, Gen.constant "clist"
                                    yield 3, Gen.constant "map"
                                    yield 3, Gen.constant "choose"
                                    yield 3, Gen.constant "filter"
                                    yield 3, Gen.constant "collect"
                                    yield 3, Gen.constant "append"
                                    yield 3, Gen.constant "sortBy"
                                    yield 3, Gen.constant "sortWith"
                                    yield 3, Gen.constant "ofAVal"
                                    if typeof<IComparable>.IsAssignableFrom typeof<'a> then
                                        yield 3, Gen.constant "setSortBy"
                                        yield 3, Gen.constant "setSortWith"
                                ]
                        match kind with
                        | "constant" -> 
                            return! Generators.List.constant<'a>()
                        | "clist" -> 
                            return! Generators.List.init<'a>()
                        | "filter" -> 
                            return! Generators.List.filter<'a>()
                        | "append" ->
                            return! Generators.List.append<'a>()
                        | "ofAVal" ->
                            return! Generators.List.ofAVal<'a>()
                        | "sortBy" ->
                            return! Generators.List.sortBy<'a>()
                        | "sortWith" ->
                            return! Generators.List.sortWith<'a>()
                        | "setSortBy" ->
                            return! Generators.List.setSortBy<'a>()
                        | "setSortWith" ->
                            return! Generators.List.setSortWith<'a>()
                        | "map" -> 
                            let! t = Gen.elements relevantTypes
                            return!
                                t |> visit { new TypeVisitor<_> with 
                                    member __.Accept<'z>() = Generators.List.map<'z, 'a>() 
                                }
                        | "choose" -> 
                            let! t = Gen.elements relevantTypes
                            return!
                                t |> visit { new TypeVisitor<_> with 
                                    member __.Accept<'z>() = Generators.List.choose<'z, 'a>() 
                                }
                        | "collect" -> 
                            let! t = Gen.elements relevantTypes
                            return!
                                t |> visit { new TypeVisitor<_> with 
                                    member __.Accept<'z>() = Generators.List.collect<'z, 'a>() 
                                }
                        | kind ->
                            return failwithf "unknown operation: %s" kind
                    }
                )
            member x.Shrinker _ =
                Seq.empty
        }

    static member Map<'a, 'b>() = 
        { new Arbitrary<VMap<'a, 'b>>() with
            member x.Generator =
                Gen.sized (fun size ->
                    gen {
                        let! kind = 
                            Gen.frequency [
                                yield 1, Gen.constant "constant"
                                yield 3, Gen.constant "cmap"
                                if size > 0 then
                                    yield 3, Gen.constant "map"
                                    yield 3, Gen.constant "choose"
                                    yield 3, Gen.constant "filter"
                                    yield 3, Gen.constant "map'"
                                    yield 3, Gen.constant "choose'"
                                    yield 3, Gen.constant "filter'"
                                    yield 3, Gen.constant "union"
                                    yield 3, Gen.constant "mapSet"
                                    yield 3, Gen.constant "bind"
                                    yield 3, Gen.constant "ofAVal"
                                    
                            ]
                        match kind with
                        | "constant" -> 
                            return! Generators.VMap.constant<'a, 'b>()
                        | "cmap" -> 
                            return! Generators.VMap.init<'a, 'b>()
                        | "filter" -> 
                            return! Generators.VMap.filter<'a, 'b>()
                        | "union" -> 
                            return! Generators.VMap.union<'a, 'b>()
                        | "mapSet" ->
                            return! Generators.VMap.mapSet<'a, 'b>()
                        | "ofAVal" ->
                            return! Generators.VMap.ofAVal<'a, 'b>()
                            
                        | "bind" -> 
                            let! t = Gen.elements relevantTypes
                            return!
                                t |> visit { new TypeVisitor<_> with 
                                    member __.Accept<'z>() = Generators.VMap.bind<'z, 'a, 'b>() 
                                }
                        
                        | "map" -> 
                            let! t = Gen.elements relevantTypes
                            return!
                                t |> visit { new TypeVisitor<_> with 
                                    member __.Accept<'z>() = Generators.VMap.map<'a, 'z, 'b>() 
                                }
                    
                        | "choose" -> 
                            let! t = Gen.elements relevantTypes
                            return!
                                t |> visit { new TypeVisitor<_> with 
                                    member __.Accept<'z>() = Generators.VMap.choose<'a, 'z, 'b>() 
                                }

                        | "filter'" -> 
                            return! Generators.VMap.filter'<'a, 'b>()
                        | "map'" -> 
                            let! t = Gen.elements relevantTypes
                            return!
                                t |> visit { new TypeVisitor<_> with 
                                    member __.Accept<'z>() = Generators.VMap.map'<'a, 'z, 'b>() 
                                }
                    
                        | "choose'" -> 
                            let! t = Gen.elements relevantTypes
                            return!
                                t |> visit { new TypeVisitor<_> with 
                                    member __.Accept<'z>() = Generators.VMap.choose'<'a, 'z, 'b>() 
                                }

                        | kind ->
                            return failwithf "unknown operation: %s" kind
                    }
                )
            member x.Shrinker _ =
                Seq.empty
        }