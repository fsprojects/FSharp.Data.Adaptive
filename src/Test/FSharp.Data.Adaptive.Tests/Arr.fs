module Arr

open System
open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit
open FSharp.Data.Adaptive
open FSharp.Data.Traceable

[<Test>]
let ``[Arr] empty`` () =
    let a = Arr.empty<int>
    a.Length |> should equal 0
    a.IsEmpty |> should equal true
    a |> Seq.toList |> should equal List.empty<int>
    
[<Test>]
let ``[Arr] single`` () =
    let a = Arr.single 5
    a.Length |> should equal 1
    a.IsEmpty |> should equal false
    a |> Seq.toList |> should equal [5]
  

[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] ofSeq`` (input : list<int>) =
    let a = Arr.ofSeq input
    a.Length |> should equal input.Length
    a |> Seq.toList |> should equal input
    
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] ofList`` (input : list<int>) =
    let a = Arr.ofList input
    a.Length |> should equal input.Length
    a |> Seq.toList |> should equal input
    
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] ofArray`` (input : list<int>) =
    let a = Arr.ofArray (List.toArray input)
    a.Length |> should equal input.Length
    a |> Seq.toList |> should equal input
    
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] fromReadonlySpan`` (input : list<int>) =
    let array = List.toArray input
    let a = arr.FromSpan (System.ReadOnlySpan<int>(array))
    a.Length |> should equal input.Length
    a |> Seq.toList |> should equal input
        
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] fromSpan`` (input : list<int>) =
    let array = List.toArray input
    let a = arr.FromSpan (System.Span<int>(array))
    a.Length |> should equal input.Length
    a |> Seq.toList |> should equal input
          
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] fromMemory`` (input : list<int>) =
    let array = List.toArray input
    let a = arr.FromMemory (System.Memory<int>(array))
    a.Length |> should equal input.Length
    a |> Seq.toList |> should equal input

[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] fromReadonlyMemory`` (input : list<int>) =
    let array = List.toArray input
    let a = arr.FromMemory (System.ReadOnlyMemory<int>(array))
    a.Length |> should equal input.Length
    a |> Seq.toList |> should equal input
    
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] insert`` (input : list<int>) (pos : uint32) =
    
    let idx = int (pos % uint32 (input.Length + 1))
    let value = 7654321
    let res = Arr.ofList input |> Arr.insert idx value
    
    let ref =
        if idx <= 0 then
            value :: input
        elif idx >= input.Length then
            List.append input [value]
        else
            List.indexed input |> List.collect (fun (i, v) ->
                if i = idx then [value; v]
                else [v]
            )
    
    
    res.Length |> should equal ref.Length
    res |> Seq.toList |> should equal ref
       
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] set`` (NonEmptyArray (input : int[])) (pos : uint32) =
    
    let idx = int (pos % uint32 input.Length)
    let value = 7654321
    let res = Arr.ofArray input |> Arr.set idx value
    
    let ref =
        Array.indexed input |> Array.map (fun (i, v) ->
            if i = idx then value
            else v
        ) |> Array.toList
    
    
    res.Length |> should equal ref.Length
    res |> Seq.toList |> should equal ref
    
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] add`` (input : list<int>) (value : int) =
    let a = Arr.ofList input
    
    let res = Arr.add value a
    let ref = List.append input [value]
    
    res.Length |> should equal ref.Length
    res |> Seq.toList |> should equal ref
    
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] append`` (input : list<int>) (value : int) =
    let a = Arr.ofList input
    
    let res = Arr.append value a
    let ref = List.append input [value]
    
    res.Length |> should equal ref.Length
    res |> Seq.toList |> should equal ref
    
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] prepend`` (input : list<int>) (value : int) =
    let a = Arr.ofList input
    
    let res = Arr.prepend value a
    let ref = value :: input
    
    res.Length |> should equal ref.Length
    res |> Seq.toList |> should equal ref
    
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] concat`` (input : list<list<int>>) =
    let a = input |> List.map Arr.ofList |> Arr.ofList
    
    let res = Arr.concat a
    let ref = List.concat input
    
    res.Length |> should equal ref.Length
    res |> Seq.toList |> should equal ref
    
    
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] take`` (input : list<int>) (n : int) =
    let a = Arr.ofList input
    let n = abs n
    let res = a |> Arr.take n
    let ref = input |> List.truncate n
    
    res.Length |> should equal ref.Length
    res |> Seq.toList |> should equal ref
    
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] skip`` (input : list<int>) (n : int) =
    let a = Arr.ofList input
    let n = abs n
    let res = a |> Arr.skip n
    let ref =
        try input |> List.skip n
        with _ -> []
    
    res.Length |> should equal ref.Length
    res |> Seq.toList |> should equal ref
    
       
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] sub`` (NonEmptyArray (arr : int[])) (pos : uint32) (cnt : uint32) =
    let input = List.ofArray arr
    let idx = int (pos % uint32 input.Length)
    let cnt = int (cnt % uint32 (input.Length - idx))
    
    let a = Arr.ofList input
    let res = a |> Arr.sub idx cnt
    let ref =
        try input |> List.skip idx |> List.truncate cnt
        with _ -> []
    
    res.Length |> should equal ref.Length
    res |> Seq.toList |> should equal ref
    
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] slice (minmax)`` (NonEmptyArray (arr : int[])) (pos : uint32) (cnt : uint32) =
    let input = List.ofArray arr
    let idx = int (pos % uint32 input.Length)
    let cnt = int (cnt % uint32 (input.Length - idx))
    
    let a = Arr.ofList input
    let res = a.[idx .. idx + cnt - 1]
    let ref =
        try input.[idx .. idx + cnt - 1]
        with _ -> []
    
    res.Length |> should equal ref.Length
    res |> Seq.toList |> should equal ref
    
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] slice (min)`` (NonEmptyArray (arr : int[])) (pos : uint32) =
    let input = List.ofArray arr
    let idx = int (pos % uint32 input.Length)
    
    let a = Arr.ofList input
    let res = a.[idx .. ]
    let ref =
        try input.[idx .. ]
        with _ -> []
    
    res.Length |> should equal ref.Length
    res |> Seq.toList |> should equal ref
    
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] slice (max)`` (NonEmptyArray (arr : int[])) (pos : uint32) =
    let input = List.ofArray arr
    let idx = int (pos % uint32 input.Length)
    
    let a = Arr.ofList input
    let res = a.[ .. idx]
    let ref =
        try input.[ .. idx]
        with _ -> []
    
    res.Length |> should equal ref.Length
    res |> Seq.toList |> should equal ref

[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] slice (empty)`` (NonEmptyArray (arr : int[])) =
    let input = List.ofArray arr
    
    let a = Arr.ofList input
    let res = a.[*]
    let ref =
        try input.[*]
        with _ -> []
    
    res.Length |> should equal ref.Length
    res |> Seq.toList |> should equal ref

[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] split`` (NonEmptyArray (arr : int[])) (pos : uint32) =
    let input = List.ofArray arr
    let idx = int (pos % uint32 input.Length)
    
    let a = Arr.ofList input
    let resa, resb = a |> Arr.split idx
    let refa = List.take idx input
    let refb = List.skip idx input
    
    resa.Length |> should equal refa.Length
    resb.Length |> should equal refb.Length
    resa |> Seq.toList |> should equal refa
    resb |> Seq.toList |> should equal refb
               
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] uncons`` (NonEmptyArray (arr : int[])) =
    let input = Array.toList arr
    let arr = Arr.ofList input
    let (a, rest) = Arr.uncons arr
    a |> should equal (List.head input)
    rest |> Seq.toList |> should equal (List.tail input)
     
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] item`` (NonEmptyArray (input : int[])) (pos : uint32) =
    let input = Array.toList input
    let idx = int (pos % uint32 input.Length)
    let arr = Arr.ofList input
    arr.[idx] |> should equal input.[idx]
    
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] map`` (input : list<int>) =
    let arr = Arr.ofList input
    let res = arr |> Arr.map ((+) 1)
    let ref = input |> List.map ((+) 1)
    res |> Seq.toList |> should equal ref
    
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] mapi`` (input : list<int>) =
    let arr = Arr.ofList input
    let res = arr |> Arr.mapi (fun i v -> i + v)
    let ref = input |> List.mapi (fun i v -> i + v)
    res |> Seq.toList |> should equal ref
        
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] choose`` (input : list<int>) =
    let arr = Arr.ofList input
    let res = arr |> Arr.choose (fun v -> if v % 3 = 0 then Some v else None)
    let ref = input |> List.choose (fun v -> if v % 3 = 0 then Some v else None)
    res |> Seq.toList |> should equal ref


[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] choosei`` (input : list<int>) =
    let arr = Arr.ofList input
    let res = arr |> Arr.choosei (fun i v -> if v % 3 = 0 then Some (i, v) else None)
    let ref = input |> List.indexed |> List.choose (fun (i, v) -> if v % 3 = 0 then Some (i, v) else None)
    res |> Seq.toList |> should equal ref

[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] filter`` (input : list<int>) =
    let arr = Arr.ofList input
    let res = arr |> Arr.filter (fun v -> v % 3 = 0)
    let ref = input |> List.filter (fun v -> v % 3 = 0)
    res |> Seq.toList |> should equal ref
        
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] filteri`` (input : list<int>) =
    let arr = Arr.ofList input
    let res = arr |> Arr.filteri (fun i v -> v % 3 = 0 || i % 2 = 0)
    let ref = input |> List.indexed |> List.filter (fun (i, v) -> v % 3 = 0 || i % 2 = 0) |> List.map snd
    res |> Seq.toList |> should equal ref
   
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] partition`` (input : list<int>) =
    let arr = Arr.ofList input
    let res1, res2 = arr |> Arr.partition (fun v -> v % 3 = 0)
    let ref1, ref2 = input |> List.partition (fun v -> v % 3 = 0)
    res1 |> Seq.toList |> should equal ref1
    res2 |> Seq.toList |> should equal ref2
             
        
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] collect`` (input : list<int>) =
    let arr = Arr.ofList input
    let res = arr |> Arr.collect (fun v -> Arr.ofList [v; v*2])
    let ref = input |> List.collect (fun v -> [v; v*2])
    res |> Seq.toList |> should equal ref
    
      
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] collecti`` (input : list<int>) =
    let arr = Arr.ofList input
    let res = arr |> Arr.collecti (fun i v -> Arr.ofList [i; v; v*2])
    let ref = input |> List.indexed |> List.collect (fun (i, v) -> [i; v; v*2])
    res |> Seq.toList |> should equal ref
    
      
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] iter`` (input : list<int>) =
    let arr = Arr.ofList input
    
    let mutable res = FSharp.Core.CompilerServices.ListCollector()
    
    arr |> Arr.iter (fun v -> res.Add (v*2))
    let ref = input |> List.map (fun v -> v*2)
    res.Close() |> should equal ref
          
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] iteri`` (input : list<int>) =
    let arr = Arr.ofList input
    
    let mutable res = FSharp.Core.CompilerServices.ListCollector()
    
    arr |> Arr.iteri (fun i v -> res.Add (i, v*2))
    let ref = input |> List.mapi (fun i v -> i, v*2)
    res.Close() |> should equal ref
    
    
        
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] removeRange`` (NonEmptyArray (arr : int[])) (pos : uint32) (cnt : uint32) =
    let input = List.ofArray arr
    let idx = int (pos % uint32 input.Length)
    let cnt = int (cnt % uint32 (input.Length - idx))
    
    let arr = Arr.ofList input
    
    let rec removeRange (l : list<'a>) =
        let mutable res = FSharp.Core.CompilerServices.ListCollector()
        let mutable i = 0
        for e in l do
            if i < idx || i >= idx + cnt then
                res.Add e
            i <- i + 1
        res.Close()
    
    arr.RemoveRange(idx, cnt)
    |> Seq.toList
    |> should equal (removeRange input)
    
    
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] replaceRange`` (NonEmptyArray (arr : int[])) (NonEmptyArray (repl : int[])) (pos : uint32) (cnt : uint32) =
    let input = List.ofArray arr
    let idx = int (pos % uint32 input.Length)
    let cnt = int (cnt % uint32 (input.Length - idx))
    
    let arr = Arr.ofList input
    let repl = Array.toList repl
    
    let rec replaceRange (l : list<int>) =
        if idx = l.Length then
            List.append l repl
        else
            let mutable res = FSharp.Core.CompilerServices.ListCollector()
            let mutable i = 0
            for e in l do
                if i = idx then
                    for ee in repl do res.Add ee
                
                if i < idx || i >= idx + cnt then
                    res.Add e
                
                i <- i + 1
            res.Close()
    
    let res = arr.ReplaceRange(idx, cnt, Arr.ofList repl)
    let reff = replaceRange input
    
    res.Length |> should equal (arr.Length - cnt + repl.Length)
    
    let resl = res |> Seq.toList
    
    resl |> should equal reff

    
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] updateRange`` (NonEmptyArray (arr : int[])) (pos : uint32) (cnt : uint32) =
    let input = List.ofArray arr
    let idx = int (pos % uint32 input.Length)
    let cnt = int (cnt % uint32 (input.Length - idx))
    
    let arr = Arr.ofList input
    //let repl = Array.toList repl
    
    let mutable last = []
    
    let mappingList (l : list<int>) =
        last <- l
        [1] @ (List.map ((+) 1) l) @ [2;3]
    
    let mappingArr (l : arr<int>) =
        l |> Arr.toList |> should equal last
        Arr.prepend 1 (Arr.map ((+)1) l) |> Arr.append 2 |> Arr.append 3
    
    let rec updateRange (mapping : list<int> -> list<int>) (l : list<int>) =
        if idx = l.Length then
            List.append l (mapping [])
        else
            let repl = mapping l.[idx .. idx + cnt - 1]
            let mutable res = FSharp.Core.CompilerServices.ListCollector()
            let mutable i = 0
            for e in l do
                if i = idx then
                    for ee in repl do res.Add ee
                
                if i < idx || i >= idx + cnt then
                    res.Add e
                
                i <- i + 1
            res.Close()
    
    let reff = updateRange mappingList input
    let res = arr.UpdateRange(idx, cnt, mappingArr)
    
    let resl = res |> Seq.toList
    resl |> should equal reff

    
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] toList`` (input : list<int>) =
    let mutable res = Arr.empty
    for e in input do res <- res.Add e
    res |> Arr.toList |> should equal input
    
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] toArray`` (input : list<int>) =
    let mutable res = Arr.empty
    for e in input do res <- res.Add e
    res |> Arr.toArray |> Array.toList |> should equal input
    
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] toSeq`` (input : list<int>) =
    let mutable res = Arr.empty
    for e in input do res <- res.Add e
    res |> Arr.toSeq |> Seq.toList |> should equal input
    
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] asSeq`` (input : list<int>) =
    let mutable res = Arr.empty
    for e in input do res <- res.Add e
    res |> Seq.toList |> should equal input
        
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] fold`` (input : list<NonEmptyString>) =
    let input = input |> List.map (function NonEmptyString str -> str)
    let arr = Arr.ofList input
    
    let res = arr |> Arr.fold (+) "123"
    let reff = input |> List.fold (+) "123"
    res |> should equal reff
    
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] foldBack`` (input : list<NonEmptyString>) =
    let input = input |> List.map (function NonEmptyString str -> str)
    let arr = Arr.ofList input
    
    let res = Arr.foldBack (+) arr "123"
    let reff = List.foldBack (+) input "123"
    res |> should equal reff
    
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] init`` (cnt : uint32) =
    let reff = List.init (int cnt % 5000) id
    let res = Arr.init (int cnt % 5000) id
    res |> Arr.toList |> should equal reff
     
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] tryItem`` (cnt : uint32) =
    let count = int (cnt % 5000u)
    let idx = int (cnt % uint32 (max 1 count))
    let res = Arr.init (int cnt % 5000) id
    let reff =
        if idx >= 0 && idx < res.Length then Some idx
        else None
    
    res |> Arr.tryItem idx |> should equal reff
    
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] exists`` (NonEmptyArray list) =
    let list = list |> Array.toList
    let arr = Arr.ofList list
    
    let res = arr |> Arr.exists (fun v -> v % 7 = 0)
    let reff = list |> List.exists (fun v -> v % 7 = 0)
    res |> should equal reff
        
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] forall`` (NonEmptyArray list) =
    let list = list |> Array.toList
    let arr = Arr.ofList list
    
    let res = arr |> Arr.forall (fun v -> v % 3 = 0)
    let reff = list |> List.forall (fun v -> v % 3 = 0)
    res |> should equal reff
    
            
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] pairwise`` (NonEmptyArray (list : array<int>)) =
    let list = list |> Array.toList
    let arr = Arr.ofList list
    
    let res = arr |> Arr.pairwise
    let reff = list |> List.pairwise
    res |> Arr.toList |> should equal reff
                
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] rev`` (NonEmptyArray (list : array<int>)) =
    let list = list |> Array.toList
    let arr = Arr.ofList list
    
    let res = arr |> Arr.rev
    let reff = list |> List.rev
    res |> Arr.toList |> should equal reff
    
    
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] tryPick`` (NonEmptyArray list) =
    let list = list |> Array.toList
    let arr = Arr.ofList list
    
    let res = arr |> Arr.tryPick (fun v -> if v % 7 = 0 then Some v else None)
    let reff = list |> List.tryPick (fun v -> if v % 7 = 0 then Some v else None)
    res |> should equal reff
    
            
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] tryPickV`` (NonEmptyArray list) =
    let list = list |> Array.toList
    let arr = Arr.ofList list
    
    let res = arr |> Arr.tryPickV (fun v -> if v % 7 = 0 then ValueSome v else ValueNone)
    let reff = list |> List.tryPick (fun v -> if v % 7 = 0 then Some v else None)
    let res =
        match res with
        | ValueSome v -> Some v
        | _ -> None
    
    res |> should equal reff
        
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] tryFindIndex`` (NonEmptyArray list) =
    let list = list |> Array.toList
    let arr = Arr.ofList list
    
    let res = arr |> Arr.tryFindIndex (fun v -> v % 7 = 0)
    let reff = list |> List.tryFindIndex (fun v -> v % 7 = 0)
    res |> should equal reff
    
                    
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] min`` (NonEmptyArray (list : int[])) =
    let list = list |> Array.toList
    let arr = Arr.ofList list
    
    let res = arr |> Arr.min
    let reff = list |> List.min
    res |> should equal reff
                        
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] max`` (NonEmptyArray (list : int[])) =
    let list = list |> Array.toList
    let arr = Arr.ofList list
    
    let res = arr |> Arr.max
    let reff = list |> List.max
    res |> should equal reff
              
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] sum`` (NonEmptyArray (list : int[])) =
    let list = list |> Array.toList
    let arr = Arr.ofList list
    
    let res = arr |> Arr.sum
    let reff = list |> List.sum
    res |> should equal reff
            
[<Property(EndSize = 1000, MaxTest = 1000)>]
let ``[Arr] average`` (NonEmptyArray (list : NormalFloat[])) =
    let list = list |> Array.map (function NormalFloat v -> v) |> Array.toList
    let arr = Arr.ofList list
    
    let res = arr |> Arr.average
    let reff = list |> List.average
    res |> should equal reff
    
    
    
[<Property(EndSize = 200, MaxTest = 10000)>]
let ``[ArrOperation] TryMerge`` (NonEmptyArray (a : int[])) (NonEmptyArray (b : int[])) (NonEmptyArray (c : int[])) =
    
    let a = Arr.ofArray a
    let b = Arr.ofArray b
    let c = Arr.ofArray c
    let ab = Arr.computeDelta (=) a b
    let bc = Arr.computeDelta (=) b c
    
    match Seq.tryHead ab with
    | Some ab0 ->
        match Seq.tryHead bc with
        | Some bc0 ->
            match ArrOperation.TryMerge(ab0, bc0) with
            | Some ac0 ->
                let oab = ArrDelta.single ab0 
                let obc = ArrDelta.single bc0
                let oac = ArrDelta.single ac0
                
                
                let reff = Arr.applyDelta (Arr.applyDelta a oab) obc
                let res = Arr.applyDelta a oac
                
                res |> should equal reff
                
            | None ->
                ()
            
        | None ->
            ()
    | None ->
        ()
    
    
let minimize (delta : arrdelta<'a>) =
    let mutable output = Arr.empty
    let mutable pending = None
    
    for op in delta do
        if not op.IsEmpty then
            match pending with
            | Some p ->
                match ArrOperation.TryMerge(p, op) with
                | Some res ->
                    pending <- Some res
                | None ->
                    output <- Arr.append p output
                    pending <- Some op
            | None ->
                pending <- Some op
          
    let res = 
        match pending with 
        | Some p -> Arr.append p output
        | None -> output
    ArrDelta.ofArr res
            
  
[<Property(EndSize = 200, MaxTest = 10000)>]
let ``[ArrDelta] combine minimal`` (NonEmptyArray (a : int[])) (NonEmptyArray (b : int[])) (NonEmptyArray (c : int[]))  =

    let a = Arr.ofArray a
    let b = Arr.ofArray b
    let c = Arr.ofArray c
    
    let ab = Arr.computeDelta (=) a b
    let bc = Arr.computeDelta (=) b c
    
    let abc = ArrDelta.combine ab bc
    let abc1 = minimize abc
    abc |> should equal abc1
      
[<Property(EndSize = 200, MaxTest = 10000)>]
let ``[ArrDelta] combine sorted`` (NonEmptyArray (a : int[])) (NonEmptyArray (b : int[])) (NonEmptyArray (c : int[]))  =
    
    let a = Arr.ofArray a
    let b = Arr.ofArray b
    let c = Arr.ofArray c
    
    let ab = Arr.computeDelta (=) a b
    let bc = Arr.computeDelta (=) b c
    
    let abc = ArrDelta.combine ab bc
    
    use mutable e = abc.GetEnumerator()
    if e.MoveNext() then
        let mutable last = e.Current
        while e.MoveNext() do
            e.Current.Index |> should (be greaterThan) last.Index
            last <- e.Current
        
    
    
[<Property(EndSize = 200, MaxTest = 10000)>]
let ``[ArrDelta] combine correct`` (NonEmptyArray (a : int[])) (NonEmptyArray (b : int[])) (NonEmptyArray (c : int[]))  =
    
    // let sa = System.Collections.Generic.HashSet<int>(a)
    // for i in 0 .. b.Length - 1 do
    //     while sa.Contains b.[i] do b.[i] <- b.[i] + 1
    //         
    // sa.UnionWith b
    // for i in 0 .. c.Length - 1 do
    //     while sa.Contains c.[i] do c.[i] <- c.[i] + 1
    //
    let a = Arr.ofArray a
    let b = Arr.ofArray b
    let c = Arr.ofArray c
    
    let ab = Arr.computeDelta (=) a b
    let bc = Arr.computeDelta (=) b c
    let ac = Arr.computeDelta (=) a c
    
    let abc = ArrDelta.combine ab bc 
    
    let res = Arr.applyDelta a abc
    res |> should equal c
    
[<Property(EndSize = 200, MaxTest = 10000)>]
let ``[Arr] apply/computeDelta`` (NonEmptyArray (a : int[])) (NonEmptyArray (b : int[])) =
    let a = Arr.ofArray a
    let b = Arr.ofArray b
    
    let d = Arr.computeDelta (=) a b
    let b1, d1 = Arr.applyDeltaAndGetEffective (=) a d
    let b2 = Arr.applyDelta a d
    
    b1 |> should equal b
    b2 |> should equal b
    d1 |> should equal d
    
    
[<Property(EndSize = 200, MaxTest = 10000)>]
let ``[Arr] applyDeltaAndGetEffective cancellation`` (NonEmptyArray (a : int[]))  =
    let a = Arr.ofArray a
    
    let op = ArrDelta.single { Index = 0; Count = a.Length; Elements = a }
    let a1, d = Arr.applyDeltaAndGetEffective (=) a op
    
    a1 |> should equal a
    d |> should equal ArrDelta.empty<int>
    
    
      
[<Property(EndSize = 200, MaxTest = 10000)>]
let ``[Arr] applyDeltaAndGetEffective preserves effective`` (NonEmptyArray (a : int[])) (NonEmptyArray (b : int[]))  =
    let a = Arr.ofArray a
    let b = Arr.ofArray b
    
    let op = Arr.computeDelta (=) a b
    let a1, d = Arr.applyDeltaAndGetEffective (=) a op
    
    a1 |> should equal b
    d |> should equal op
    
    
    
    