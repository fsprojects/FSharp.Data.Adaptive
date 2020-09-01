
[![CI - Windows](https://github.com/fsprojects/FSharp.Data.Adaptive/workflows/CI%20-%20Windows/badge.svg?branch=master)](https://github.com/fsprojects/FSharp.Data.Adaptive/actions?query=workflow%3A%22CI+-+Windows%22)
[![CI - Linux](https://github.com/fsprojects/FSharp.Data.Adaptive/workflows/CI%20-%20Linux/badge.svg?branch=master)](https://github.com/fsprojects/FSharp.Data.Adaptive/actions?query=workflow%3A%22CI+-+Linux%22)
[![CI - MacOS](https://github.com/fsprojects/FSharp.Data.Adaptive/workflows/CI%20-%20MacOS/badge.svg?branch=master)](https://github.com/fsprojects/FSharp.Data.Adaptive/actions?query=workflow%3A%22CI+-+MacOS%22)
[![CI - Fable](https://github.com/fsprojects/FSharp.Data.Adaptive/workflows/CI%20-%20Fable/badge.svg?branch=master)](https://github.com/fsprojects/FSharp.Data.Adaptive/actions?query=workflow%3A%22CI+-+Fable%22)
# FSharp.Data.Adaptive

FSharp.Data.Adaptive provides a clean API for handling changeable ("adaptive") data while sticking to *functional* principles. It allows programmers to treat adaptive values just like immutable data while still maintaining efficient updates. Many adaptive data structures and operators are provided.

# Purpose

Writing a program that adjusts its output according to changes in data or user input is traditionally either:
1. very tedious work, error prone and involves lots of caches with unknown lifetime, cluttered code handling reuse of values, etc.; or
2. rather inefficient when employing functional principles (immutable data). The program state is often effectively replaced on every change and (at least conceptually) the entire program is re-executed.

FSharp.Data.Adaptive tackles this with a somewhat *in-between* solution. It employs a functional-first approach on the surface and abstracts away caching/reuse issues in a clean way underneath.

# Overview

FSharp.Data.Adaptive provides several container types that are used to represent adaptive values and various combinators for operating on these containers. 

`aval<'T>` holds a single changeable value. We distinguish two associated variants of each container cell type:
1. `cval<'T>`: Changeable cells that can be modified directly by user code . These are prefixed with `c` for "changeable".
2. `aval<'T>`: Adaptive cells that represent dependent computations. These depend on changeable cells or other adaptive cells, but cannot be directly modified by user code. They are prefixed with `a` for "adaptive".

Changeable cells are compatible (via inheritance) with their adaptive counterparts.

```fsharp
let changeable = cval 10
let dependent = changeable |> AVal.map (fun a -> 2 * a)

dependent |> AVal.force // => 20
transact (fun () -> changeable.Value <- 1)
dependent |> AVal.force // => 2
```
*`transact` manually changes the value of a changable cell. `force` recovers the current value of an adaptive cell, thus leaving adaptive world.*

Adaptive combinators are efficient. The ones provided for sets `aset<'T>`, lists `alist<'T>` or maps `amap<'Key,'Value>` recompute results *incrementally*. 

```fsharp
let input = clist [1;2;3]
let dependent = 
    input 
    |> AList.map (fun v -> v * v)
    |> AList.fold (+) 0

dependent |> AVal.force // => 14
transact (fun () -> input.Append 4)
dependent |> AVal.force // => 30
```
*The cost of updates stays constant regardless of the length of the list.*

Adaptive depencies are *dynamic*. A dependency may or may not exist depending on another dependency.

```fsharp
let a = cval "some dependency"
let b = cval "other input"
let param = cval 0.5

let result = 
    param 
    |> AVal.bind (fun p -> 
        if p <= 0.33 then a :> aval<_>
        elif p <= 0.66 then b :> aval<_>
        else AVal.constant "invalid"
    )
```
*Thanks to `bind`, dependencies between cells only exist if the condition is met. No recomputations are ever performed as long as the result doesn't demand it via dependencies.*

# Contribute

Communicate via:

[![Discord](https://discordapp.com/api/guilds/611129394764840960/widget.png) Discord](https://discord.gg/UyecnhM)

[Gitter](https://gitter.im/aardvark-platform/Lobby)

[Github issues](https://github.com/fsprojects/FSharp.Data.Adaptive/issues)


about things like bugs, feature requests, use cases or your own implementations of adaptive data structures. We're looking forward to talking to you!

# Documentation

[Find the repository documentation here](https://fsprojects.github.io/FSharp.Data.Adaptive/reference/index.html). It contains a full list of adaptive data structures and operators as well as descriptions of their implementations.

# Projects

* in-depth tutorial: https://fsprojects.github.io/FSharp.Data.Adaptive
* [TodoMVC](https://aardvarkians.com/demo/TodoMVC/) implemented in [Fable.Elmish.Adaptive](https://github.com/krauthaufen/Fable.Elmish.Adaptive)
* [Adaptify.](https://github.com/krauthaufen/Adaptify) Automatically generates adaptive equivalents of regular immutable FSharp data types.
* [Lightning talk slides and 10 line demo script by @dsyme](https://github.com/dsyme/fsharp-presentations/tree/master/2019-09-27-openfsharp)
* [Nuget Package](https://www.nuget.org/packages/FSharp.Data.Adaptive/)

# History

The project started back in 2013 at the [VRVis Research Center](https://www.vrvis.at) and was mainly developed for the [Aardvark.Platform](https://aardvarkians.com). Over time it became more and more apparent that adaptive data has the potential to benefit many different applications, so we decided to move it to this standalone library (outside the Aardvark world). If you're interested in the development history, the last stable aardvark-implementation (and most of its history) can be found in [Aardvark.Base.Incremental](https://github.com/aardvark-platform/aardvark.base/tree/v4/src/Aardvark.Base.Incremental). 
