[![DotNet](https://github.com/fsprojects/FSharp.Data.Adaptive/actions/workflows/dotnet.yml/badge.svg)](https://github.com/fsprojects/FSharp.Data.Adaptive/actions/workflows/dotnet.yml)
[![Fable](https://github.com/fsprojects/FSharp.Data.Adaptive/actions/workflows/fable.yml/badge.svg)](https://github.com/fsprojects/FSharp.Data.Adaptive/actions/workflows/fable.yml)
[![NuGet](https://badgen.net/nuget/v/FSharp.Data.Adaptive)](https://www.nuget.org/packages/FSharp.Data.Adaptive/)
[![NuGet](https://badgen.net/nuget/dt/FSharp.Data.Adaptive)](https://www.nuget.org/packages/FSharp.Data.Adaptive/)
[![Discord](https://badgen.net/discord/online-members/UyecnhM)](https://discord.gg/UyecnhM)

# FSharp.Data.Adaptive

**A functional reactive programming library for F# that provides efficient incremental computation and automatic dependency tracking.**

FSharp.Data.Adaptive provides a clean API for handling changeable ("adaptive") data while sticking to *functional* principles. It allows programmers to treat adaptive values just like immutable data while still maintaining efficient updates through automatic change propagation and incremental computation.

## Table of Contents

- [Installation](#installation)
- [Quick Start](#quick-start)
- [Purpose](#purpose)
- [Core Concepts](#core-concepts)
- [Adaptive Data Structures](#adaptive-data-structures)
- [Key Features](#key-features)
- [Examples](#examples)
- [Performance Characteristics](#performance-characteristics)
- [Documentation](#documentation)
- [Projects and Resources](#projects-and-resources)
- [Contributing](#contributing)
- [History](#history)

## Installation

### Using .NET CLI

```bash
dotnet add package FSharp.Data.Adaptive
```

### Using Paket

```bash
paket add FSharp.Data.Adaptive
```

### Using Package Manager

```powershell
Install-Package FSharp.Data.Adaptive
```

### Supported Platforms

- **.NET Standard 2.0** - Compatible with .NET Framework, .NET Core, and .NET 5+
- **.NET 8.0** - Optimized for latest .NET runtime
- **Fable** - JavaScript/WebAssembly support for browser and Node.js applications

## Quick Start

Here's a minimal example to get you started:

```fsharp
open FSharp.Data.Adaptive

// Create a changeable value
let input = cval 10

// Create a derived adaptive value that doubles the input
let doubled = input |> AVal.map (fun x -> x * 2)

// Read the current value
printfn "Initial: %d" (AVal.force doubled)  // Output: 20

// Change the input within a transaction
transact (fun () -> input.Value <- 15)

// The derived value updates automatically
printfn "After change: %d" (AVal.force doubled)  // Output: 30
```

## Purpose

Writing programs that respond to changing data or user input is traditionally either:

1. **Imperative and Error-Prone**: Requires manual cache management, tracking dependencies, handling update propagation, and dealing with uncertain object lifetimes. Code quickly becomes cluttered and bug-prone.

2. **Functional but Inefficient**: When using pure functional principles with immutable data, the entire program state is often recreated on every change, requiring complete re-computation even when changes are small.

FSharp.Data.Adaptive provides an elegant *in-between* solution:
- **Functional on the surface**: Clean, composable API with immutable-style operations
- **Efficient underneath**: Automatic caching, dependency tracking, and incremental updates
- **Best of both worlds**: Write clean functional code that runs with imperative-level efficiency

## Core Concepts

### Changeable vs Adaptive

FSharp.Data.Adaptive distinguishes between two categories of adaptive values:

1. **`cval<'T>`** (Changeable): Input cells that can be modified directly by your code. These are the "sources" of change in your application. Prefixed with `c` for "changeable".

2. **`aval<'T>`** (Adaptive): Derived values that automatically update based on their dependencies. These cannot be modified directly - they recompute when their inputs change. Prefixed with `a` for "adaptive".

Think of `cval` as your application's inputs and `aval` as computed outputs that stay synchronized automatically.

**Note**: Changeable cells are compatible with their adaptive counterparts (via inheritance), so a `cval<'T>` can be used wherever an `aval<'T>` is expected.

```fsharp
let changeable = cval 10
let dependent = changeable |> AVal.map (fun a -> 2 * a)

dependent |> AVal.force // => 20
transact (fun () -> changeable.Value <- 1)
dependent |> AVal.force // => 2
```

**Key points:**
- `transact` wraps changes to changeable cells in a transaction for efficient batch updates
- `force` evaluates an adaptive value and returns its current result
- Changes propagate automatically through the dependency graph

### Transactions

All modifications to changeable values must occur within a transaction:

```fsharp
let input = cval 5

// Correct: Modify within a transaction
transact (fun () ->
    input.Value <- 10
)

// Multiple changes in one transaction are efficient
transact (fun () ->
    input.Value <- 20
    // More changes...
)
```

Transactions ensure that:
- Changes are batched for efficient propagation
- The dependency graph remains consistent
- Observers see a coherent view of changes

## Adaptive Data Structures

FSharp.Data.Adaptive provides three main collection types with incremental update support:

| Type | Description | Use Case |
|------|-------------|----------|
| **`aval<'T>`** | Single adaptive value | Reactive variables, computed properties |
| **`alist<'T>`** | Ordered list with index tracking | Sequences where order matters, UI lists |
| **`aset<'T>`** | Unordered hash set | Unique collections, set operations |
| **`amap<'K,'V>`** | Key-value dictionary | Lookup tables, indexed data |

Each type has a changeable variant (`cval`, `clist`, `cset`, `cmap`) for direct modification.

### Incremental Collection Updates

Adaptive combinators recompute results *incrementally*. Operations on collections only process the changes (deltas), not the entire collection: 

```fsharp
let input = clist [1; 2; 3]
let dependent =
    input
    |> AList.map (fun v -> v * v)    // Squares each element
    |> AList.fold (+) 0                // Sums the results

dependent |> AVal.force // => 14 (1 + 4 + 9)
transact (fun () -> input.Append 4)
dependent |> AVal.force // => 30 (14 + 16)
```

**The magic**: When appending `4`, only the new element is processed:
- `map` computes `4 * 4 = 16` (doesn't recompute 1, 2, 3)
- `fold` adds `16` to the previous sum of `14`
- **The cost stays constant** regardless of list length

### Dynamic Dependencies

Dependencies can change based on values, allowing conditional computation graphs:

```fsharp
let a = cval "dependency A"
let b = cval "dependency B"
let param = cval 0.5

let result =
    param
    |> AVal.bind (fun p ->
        if p <= 0.33 then a :> aval<_>
        elif p <= 0.66 then b :> aval<_>
        else AVal.constant "constant value"
    )

result |> AVal.force // => "dependency B"

// Changing 'a' has no effect (not a current dependency)
transact (fun () -> a.Value <- "A changed")
result |> AVal.force // => "dependency B" (unchanged)

// Changing 'param' switches dependency to 'a'
transact (fun () -> param.Value <- 0.2)
result |> AVal.force // => "A changed" (now depends on 'a')
```

**Benefits of dynamic dependencies:**
- Only active dependencies trigger recomputation
- Conditional branches are truly lazy
- Memory efficient - unused branches can be garbage collected
- Perfect for conditional UI rendering, state machines, and complex workflows

## Key Features

### Automatic Dependency Tracking

Dependencies are tracked automatically without explicit declarations:

```fsharp
let x = cval 1
let y = cval 2
let sum = AVal.map2 (+) x y  // Automatically tracks both x and y

sum |> AVal.force // => 3
transact (fun () -> x.Value <- 5)
sum |> AVal.force // => 7 (automatically updated)
```

### Composable Operators

Rich set of functional operators for all adaptive types:

```fsharp
// Map, filter, fold on adaptive lists
let numbers = clist [1..10]
let evenSquares =
    numbers
    |> AList.filter (fun x -> x % 2 = 0)
    |> AList.map (fun x -> x * x)
    |> AList.sortBy id

// Set operations
let setA = cset [1; 2; 3]
let setB = cset [3; 4; 5]
let union = ASet.union setA setB
let intersection = ASet.intersect setA setB

// Map transformations
let prices = cmap [("apple", 1.0); ("banana", 0.5)]
let withTax =
    prices
    |> AMap.map (fun key price -> price * 1.1)
```

### Efficient Deltas

Collections track changes as deltas (additions, removals, updates):

```fsharp
let items = clist [1; 2; 3]
let doubled = items |> AList.map (fun x -> x * 2)

// Readers provide incremental access to changes
let reader = doubled.GetReader()

// Initial state: [(0,2); (1,4); (2,6)]
let ops = reader.GetChanges(AdaptiveToken.Top)

transact (fun () -> items.Append 4)

// Delta: only [(3,8)] - the new element
let delta = reader.GetChanges(AdaptiveToken.Top)
```

### Weak References and Memory Management

The dependency graph uses weak references, allowing unused parts to be garbage collected:

```fsharp
let createComputation () =
    let input = cval 42
    input |> AVal.map (fun x -> x * 2)
    // No strong reference kept - can be GC'd when unused
```

## Examples

### Example 1: Reactive UI State

```fsharp
type Model = {
    firstName: cval<string>
    lastName: cval<string>
}

let model = {
    firstName = cval "John"
    lastName = cval "Doe"
}

// Derived state automatically updates
let fullName =
    AVal.map2
        (fun first last -> sprintf "%s %s" first last)
        model.firstName
        model.lastName

let greeting =
    fullName |> AVal.map (sprintf "Hello, %s!")

greeting |> AVal.force // => "Hello, John Doe!"

transact (fun () -> model.firstName.Value <- "Jane")
greeting |> AVal.force // => "Hello, Jane Doe!"
```

### Example 2: Filtered and Sorted List

```fsharp
type Person = { Name: string; Age: int }

let people = clist [
    { Name = "Alice"; Age = 30 }
    { Name = "Bob"; Age = 25 }
    { Name = "Charlie"; Age = 35 }
]

// Filter adults and sort by name
let adults =
    people
    |> AList.filter (fun p -> p.Age >= 30)
    |> AList.sortBy (fun p -> p.Name)

// Add a new person
transact (fun () ->
    people.Append { Name = "Diana"; Age = 32 }
)

// Only Diana is filtered and inserted in correct sorted position
// Alice and Charlie are not reprocessed
```

### Example 3: Incremental Computation

```fsharp
let numbers = clist [1..1000]

// Expensive computation
let result =
    numbers
    |> AList.map (fun x ->
        // Simulate expensive operation
        System.Threading.Thread.Sleep(1)
        x * x
    )
    |> AList.fold (+) 0

// Initial evaluation: computes all 1000 items (takes ~1 second)
let initial = AVal.force result

// Add one number
transact (fun () -> numbers.Append 1001)

// Incremental update: only computes new item (takes ~1ms)
let updated = AVal.force result
```

## Performance Characteristics

### Time Complexity

| Operation | Initial Evaluation | Incremental Update |
|-----------|-------------------|-------------------|
| `AVal.map` | O(1) | O(1) |
| `AList.map` | O(n) | O(k) where k = changes |
| `ASet.map` | O(n) | O(k) where k = changes |
| `AList.fold` | O(n) | O(k) where k = changes |
| `ASet.union` | O(n + m) | O(k) where k = changes |
| `AMap.map` | O(n) | O(k) where k = changes |

### Space Complexity

- **Adaptive Values**: O(1) per value (plus cached result)
- **Adaptive Collections**: O(n) for elements + O(m) for dependency graph
- **Readers**: O(1) per reader (tracks position, not full state)

### Best Practices for Performance

1. **Use `mapNonAdaptive` for cheap operations**: If the mapping is very fast (like a type cast), use `AVal.mapNonAdaptive` to avoid caching overhead.

2. **Batch changes in transactions**: Multiple changes in one transaction are more efficient than separate transactions.

3. **Prefer incremental operations**: Use `AList`, `ASet`, `AMap` operators instead of converting to/from regular collections.

4. **Avoid unnecessary `force` calls**: Only evaluate when you need the actual value.

5. **Use readers for repeated access**: When consuming collection changes repeatedly, use readers instead of converting to lists.

## Documentation

### Official Documentation

- **[Technical API Reference](https://fsprojects.github.io/FSharp.Data.Adaptive/reference/index.html)** - Complete API documentation with all types and functions
- **[In-depth Tutorial](https://fsprojects.github.io/FSharp.Data.Adaptive)** - Step-by-step guide to using adaptive data
- **[NuGet Package](https://www.nuget.org/packages/FSharp.Data.Adaptive/)** - Package repository and version history

### Learning Resources

- **[Lightning Talk by Don Syme](https://github.com/dsyme/fsharp-presentations/tree/master/2019-09-27-openfsharp)** - Quick intro slides and demo script
- **[Functional Data that Adapts to Change (NDC Oslo 2020)](https://www.youtube.com/watch?v=us4dp7Ksly0)** - Video presentation by Don Syme
- **[fsharpConf 2023 Presentation](https://www.youtube.com/watch?v=UThMnaRNvHQ)** - Conference talk on adaptive data
- **[C# Interop Example](https://github.com/aardvark-platform/aardvark.base/blob/master/src/Demo/IncrementalDemo.CSharp/Program.cs)** - Using FSharp.Data.Adaptive from C#

## Projects and Resources

### Related Projects

- **[Fable.Elmish.Adaptive](https://github.com/krauthaufen/Fable.Elmish.Adaptive)** - Elmish integration for web apps
  - [TodoMVC Demo](https://aardvarkians.com/demo/TodoMVC/) - See it in action
- **[Adaptify](https://github.com/krauthaufen/Adaptify)** - Code generator for adaptive equivalents of F# data types
- **[Aardvark.Platform](https://aardvarkians.com)** - Real-time 3D rendering framework using FSharp.Data.Adaptive

## Contributing

We welcome contributions! Here's how you can help:

### Reporting Issues

Found a bug or have a feature request? Please open an issue on [GitHub Issues](https://github.com/fsprojects/FSharp.Data.Adaptive/issues).

When reporting bugs, include:
- Minimal reproducible example
- Expected vs actual behavior
- Library version and platform (.NET version, OS)

### Discussions and Questions

Join the conversation:

- **[Discord](https://discord.gg/UyecnhM)** - Chat with the community and maintainers
- **[GitHub Discussions](https://github.com/fsprojects/FSharp.Data.Adaptive/discussions)** - Long-form discussions about features and use cases

### Pull Requests

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/my-feature`)
3. Make your changes with tests
4. Ensure all tests pass
5. Submit a pull request

### Areas for Contribution

- New adaptive data structures
- Performance optimizations
- Documentation improvements
- Examples and tutorials
- Bug fixes

## History

FSharp.Data.Adaptive has an interesting evolution:

- **2013**: Initial development at [VRVis Research Center](https://www.vrvis.at) for the [Aardvark.Platform](https://aardvarkians.com) 3D rendering framework
- **2019**: Extracted as a standalone library to benefit the broader F# ecosystem
- **Present**: Widely used in UI frameworks, data processing, and reactive applications

The original implementation can be found in [Aardvark.Base.Incremental](https://github.com/aardvark-platform/aardvark.base/tree/v4/src/Aardvark.Base.Incremental).

## License

This project is licensed under the Apache License 2.0 - see the repository for details. 
