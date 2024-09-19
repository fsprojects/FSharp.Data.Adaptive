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
  

[<Property(EndSize = 10000)>]
let ``[Arr] ofSeq`` (input : list<int>) =
    let a = Arr.ofSeq input
    a.Length |> should equal input.Length
    a |> Seq.toList |> should equal input
    
[<Property(EndSize = 10000)>]
let ``[Arr] ofList`` (input : list<int>) =
    let a = Arr.ofList input
    a.Length |> should equal input.Length
    a |> Seq.toList |> should equal input
    
[<Property(EndSize = 10000)>]
let ``[Arr] ofArray`` (input : list<int>) =
    let a = Arr.ofArray (List.toArray input)
    a.Length |> should equal input.Length
    a |> Seq.toList |> should equal input
    
[<Property(EndSize = 10000)>]
let ``[Arr] fromReadonlySpan`` (input : list<int>) =
    let array = List.toArray input
    let a = arr.FromSpan (System.ReadOnlySpan<int>(array))
    a.Length |> should equal input.Length
    a |> Seq.toList |> should equal input
        
[<Property(EndSize = 10000)>]
let ``[Arr] fromSpan`` (input : list<int>) =
    let array = List.toArray input
    let a = arr.FromSpan (System.Span<int>(array))
    a.Length |> should equal input.Length
    a |> Seq.toList |> should equal input
          
[<Property(EndSize = 10000)>]
let ``[Arr] fromMemory`` (input : list<int>) =
    let array = List.toArray input
    let a = arr.FromMemory (System.Memory<int>(array))
    a.Length |> should equal input.Length
    a |> Seq.toList |> should equal input

[<Property(EndSize = 10000)>]
let ``[Arr] fromReadonlyMemory`` (input : list<int>) =
    let array = List.toArray input
    let a = arr.FromMemory (System.ReadOnlyMemory<int>(array))
    a.Length |> should equal input.Length
    a |> Seq.toList |> should equal input
    
[<Property(EndSize = 10000)>]
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
       
[<Property(EndSize = 10000)>]
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
    
[<Property(EndSize = 10000)>]
let ``[Arr] add`` (input : list<int>) (value : int) =
    let a = Arr.ofList input
    
    let res = Arr.add value a
    let ref = List.append input [value]
    
    res.Length |> should equal ref.Length
    res |> Seq.toList |> should equal ref
    
[<Property(EndSize = 10000)>]
let ``[Arr] append`` (input : list<int>) (value : int) =
    let a = Arr.ofList input
    
    let res = Arr.append value a
    let ref = List.append input [value]
    
    res.Length |> should equal ref.Length
    res |> Seq.toList |> should equal ref
    
[<Property(EndSize = 10000)>]
let ``[Arr] prepend`` (input : list<int>) (value : int) =
    let a = Arr.ofList input
    
    let res = Arr.prepend value a
    let ref = value :: input
    
    res.Length |> should equal ref.Length
    res |> Seq.toList |> should equal ref
    
[<Property(EndSize = 10000)>]
let ``[Arr] concat`` (input : list<list<int>>) =
    let a = input |> List.map Arr.ofList |> Arr.ofList
    
    let res = Arr.concat a
    let ref = List.concat input
    
    res.Length |> should equal ref.Length
    res |> Seq.toList |> should equal ref
    
    
[<Property(EndSize = 10000)>]
let ``[Arr] take`` (input : list<int>) (n : int) =
    let a = Arr.ofList input
    let n = abs n
    let res = a |> Arr.take n
    let ref = input |> List.truncate n
    
    res.Length |> should equal ref.Length
    res |> Seq.toList |> should equal ref
    
[<Property(EndSize = 10000)>]
let ``[Arr] skip`` (input : list<int>) (n : int) =
    let a = Arr.ofList input
    let n = abs n
    let res = a |> Arr.skip n
    let ref =
        try input |> List.skip n
        with _ -> []
    
    res.Length |> should equal ref.Length
    res |> Seq.toList |> should equal ref
    
       
[<Property(EndSize = 10000)>]
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
    
[<Property(EndSize = 10000)>]
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
    
[<Property(EndSize = 10000)>]
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
    
[<Property(EndSize = 10000)>]
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

[<Property(EndSize = 10000)>]
let ``[Arr] slice (empty)`` (NonEmptyArray (arr : int[])) =
    let input = List.ofArray arr
    
    let a = Arr.ofList input
    let res = a.[*]
    let ref =
        try input.[*]
        with _ -> []
    
    res.Length |> should equal ref.Length
    res |> Seq.toList |> should equal ref

[<Property(EndSize = 10000)>]
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
               
[<Property(EndSize = 10000)>]
let ``[Arr] uncons`` (NonEmptyArray (arr : int[])) =
    let input = Array.toList arr
    let arr = Arr.ofList input
    let (a, rest) = Arr.uncons arr
    a |> should equal (List.head input)
    rest |> Seq.toList |> should equal (List.tail input)
     
[<Property(EndSize = 10000)>]
let ``[Arr] item`` (NonEmptyArray (input : int[])) (pos : uint32) =
    let input = Array.toList input
    let idx = int (pos % uint32 input.Length)
    let arr = Arr.ofList input
    arr.[idx] |> should equal input.[idx]
    
[<Property(EndSize = 10000)>]
let ``[Arr] map`` (input : list<int>) =
    let arr = Arr.ofList input
    let res = arr |> Arr.map ((+) 1)
    let ref = input |> List.map ((+) 1)
    res |> Seq.toList |> should equal ref
        
[<Property(EndSize = 10000)>]
let ``[Arr] choose`` (input : list<int>) =
    let arr = Arr.ofList input
    let res = arr |> Arr.choose (fun v -> if v % 3 = 0 then Some v else None)
    let ref = input |> List.choose (fun v -> if v % 3 = 0 then Some v else None)
    res |> Seq.toList |> should equal ref

[<Property(EndSize = 10000)>]
let ``[Arr] filter`` (input : list<int>) =
    let arr = Arr.ofList input
    let res = arr |> Arr.filter (fun v -> v % 3 = 0)
    let ref = input |> List.filter (fun v -> v % 3 = 0)
    res |> Seq.toList |> should equal ref
        
        
[<Property(EndSize = 10000)>]
let ``[Arr] collect`` (input : list<int>) =
    let arr = Arr.ofList input
    let res = arr |> Arr.collect (fun v -> Arr.ofList [v; v*2])
    let ref = input |> List.collect (fun v -> [v; v*2])
    res |> Seq.toList |> should equal ref
    
    
        
[<Property(EndSize = 10000)>]
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
    
    
[<Property(EndSize = 10000)>]
let ``[Arr] replaceRange`` (NonEmptyArray (arr : int[])) (NonEmptyArray (repl : int[])) (pos : uint32) (cnt : uint32) =
    let input = List.ofArray arr
    let idx = int (pos % uint32 input.Length)
    let cnt = int (cnt % uint32 (input.Length - idx))
    
    let arr = Arr.ofList input
    let repl = Array.toList repl
    
    let rec replaceRange (l : list<int>) =
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
    
    res
    |> Seq.toList
    |> should equal reff

    
[<Property(EndSize = 10000)>]
let ``[Arr] toList`` (input : list<int>) =
    let mutable res = Arr.empty
    for e in input do res <- res.Add e
    res |> Arr.toList |> should equal input
    
[<Property(EndSize = 10000)>]
let ``[Arr] toArray`` (input : list<int>) =
    let mutable res = Arr.empty
    for e in input do res <- res.Add e
    res |> Arr.toArray |> Array.toList |> should equal input
    
[<Property(EndSize = 10000)>]
let ``[Arr] toSeq`` (input : list<int>) =
    let mutable res = Arr.empty
    for e in input do res <- res.Add e
    res |> Arr.toSeq |> Seq.toList |> should equal input
    
[<Property(EndSize = 10000)>]
let ``[Arr] asSeq`` (input : list<int>) =
    let mutable res = Arr.empty
    for e in input do res <- res.Add e
    res |> Seq.toList |> should equal input
    