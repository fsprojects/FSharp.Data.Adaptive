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
    
[<Property(EndSize = 10000)>]
let ``[Arr] insert`` (input : list<int>) (NormalFloat pos) =
    
    let idx = (abs pos % 1.0) * float input.Length |> round |> int
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
let ``[Arr] set`` (NonEmptyArray (input : int[])) (NormalFloat pos) =
    
    let idx = (abs pos % 1.0) * float input.Length |> round |> int |> min (input.Length - 1)
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
let ``[Arr] ofSpan`` (input : list<int>) =
    let array = List.toArray input
    let a = arr.FromSpan (System.ReadOnlySpan<int>(array))
    a.Length |> should equal input.Length
    a |> Seq.toList |> should equal input
    
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
let ``[Arr] sub`` (NonEmptyArray (arr : int[])) (NormalFloat pos) (NormalFloat cnt) =
    let input = List.ofArray arr
    let idx = (abs pos % 1.0) * float input.Length |> round |> int |> min (input.Length - 1)
    let cnt = (abs cnt % 1.0) * float (input.Length - idx) |> round |> int |> min (input.Length - idx)
    
    let a = Arr.ofList input
    let res = a |> Arr.sub idx cnt
    let ref =
        try input |> List.skip idx |> List.truncate cnt
        with _ -> []
    
    res.Length |> should equal ref.Length
    res |> Seq.toList |> should equal ref
    
           
[<Property(EndSize = 10000)>]
let ``[Arr] split`` (NonEmptyArray (arr : int[])) (NormalFloat pos) =
    let input = List.ofArray arr
    let idx = (abs pos % 1.0) * float input.Length |> round |> int |> min (input.Length - 1)
    
    let a = Arr.ofList input
    let resa, resb = a |> Arr.split idx
    let refa = List.take idx input
    let refb = List.skip idx input
    
    resa.Length |> should equal refa.Length
    resb.Length |> should equal refb.Length
    resa |> Seq.toList |> should equal refa
    resb |> Seq.toList |> should equal refb
    
    