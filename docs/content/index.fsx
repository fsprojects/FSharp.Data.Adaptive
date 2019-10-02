(*** hide ***)
#I "../../bin/Release/netstandard2.0"
#r "netstandard"
#r "FSharp.Data.Adaptive.dll"
open FSharp.Data.Adaptive

(**

# FSharp.Data.Adaptive: adaptive data for F#

FSharp.Data.Adaptive aims at providing simple, yet powerful tools for developers to 
write incremental (or as we call it: adaptive) programs.
In contrast to RX or similar libraries, adaptive focuses on values instead of events.
The most basic adaptive container type we provide is `aval<'T>` and here's a little example of what it can do for you:
*)


(** 
## Gravity Example

*)

(**

Consider a ball that falls from 10m height on earth (9.81m/s^2).
*)
let height  = cval 10.0
let gravity = cval 9.81
(** So when will the ball be hitting the floor? *)
let timeToFloor = AVal.map2 (fun h g -> sqrt (2.0 * h / g)) height gravity
printfn "%.3fs" (AVal.force timeToFloor) // => 1.428s

(** How long will it take on the moon? *)
transact (fun () -> gravity.Value <- 1.62)
printfn "%.3fs" (AVal.force timeToFloor) // => 3.514s

(** What about a height of 2000m on Jupiter? *)
transact (fun () ->
    gravity.Value <- 24.79
    height.Value <- 2000.0
)
printfn "%.3fs" (AVal.force timeToFloor) // => 12.703s

(** This example feels a lot like an Excel calcuation, in which dependent cells get updated
whenever their inputs change.  

> * `cval` **Changeable Value**. Adaptive cell whose value can be manually set. A `cval` is also an `aval`.
> * `transact` Set a `cval`'s value within this scope.
> * `aval` **Adaptive Value**. Adaptive cell whose value purely depends on other adaptive cells.
> * `map` `map2` `map3` Create a new `aval` whose value depends on the one (two, three) input `aval`(s). 
> * `force` Read an `aval`'s value.

This is not particularly impressive. The API becomes much more interesting when *structural dependencies* (or *dynamic dependencies* )
come into play. The term *dynamic dependencies* means `aval`s can dynamically decide whether or not to depend on something based on their content. 

Let's calculate the height of our ball at a certain point in time.
*)
let time = cval 0.5

// simple utility calculating the height
let calcHeight (t : float) (h0 : float) (g : float) =
    printf "height after %.3fs: " t
    h0 - 0.5*g*t*t

let currentHeight = AVal.map3 calcHeight time height gravity
printfn "%.3fm" (AVal.force currentHeight) // => height after 0.500s: 1996.901m
printfn "%.3fm" (AVal.force currentHeight) // => 1996.901m
printfn "%.3fm" (AVal.force currentHeight) // => 1996.901m

(** 
Note that we snuck a little "height after" print into the utility calculation. This is for illustrative purposes: observe how the print occurs only when `currentHeight` needs to re-calculate its value. It happens *only* at initial calculation or when one of its inputs changes! 

But, oh no! Someone could enter an invalid time (let's say -100s)! *)
transact (fun () -> time.Value <- -100.0)
printfn "%.3fm" (AVal.force currentHeight) // => height after -100.000s: -121950.000m  
(** We'd like to prevent that!

Let's rewrite the code using a dynamic dependency.
*)
let currentHeightSafe =
    time |> AVal.bind (fun t ->
        if t <= 0.0 then 
            // whenever the time is negative the ball will just report its initial height.
            height :> aval<_>
        else
            // when the time is positive we use our utility to calculate the height at the current time.
            AVal.map2 (calcHeight t) height gravity
    )

printfn "%.3fm" (AVal.force currentHeightSafe) // => 2000.000m
(** 
The content of `currentHeightSafe` is now 2000.0, since our time is still negative and we decide to return the content of `height`. 
Observe the fact that there is no "height after" print! There's no need to run our utility calculation at this time.

What happens if we change the gravity back to earth?
*)
transact (fun () -> gravity.Value <- 9.81)
printfn "%.3fm" (AVal.force currentHeightSafe) // => 2000.000m

(** 
Nothing changed, since `currentHeightSafe` currently *does not depend* on `gravity`. There is no "height after" print!

Okay, set the height to 10m again. *)
transact (fun () -> height.Value <- 10.0)
printfn "%.3fm" (AVal.force currentHeightSafe) // => 10.0m

(** 
Our value of `currentHeightSafe` got updated to represent the new value of `height`. There is **still** no "height after" print!

Finally, let's set the time to a positive value again. *)
transact (fun () -> time.Value <- 1.42785)
printfn "%.3fm" (AVal.force currentHeightSafe) // height after 1.428s: 0.000m

(** 
And there's our "height after" print. We decided to depend on the utility calculation again.

> * `bind` Create a dynamic dependency. The resulting `aval` may dynamically decide on which `aval`s to depend.
> * **Dependency graph**. A chain of dependent `aval`s.
> * **Dynamic dependency graph**. A dependency graph that includes dynamic dependencies.

This simple example demonstrates the *true power* of `aval`s: Their capacity of expressing *dynamic dependency graphs*. In this example, the system *knew exactly* that `currentHeightSafe` didn't depend on `gravity`/`height` as long as `time` was negative. As soon as `time` became positive, the system *precisely* maintained the resulting structure of the dependencies. 

The advantage is immediately clear: If the dependencies contained heavyweight computations, the system ensures only the minimal amount of effort is expended for updates whenever a result is requested. Traditional reactive programming libraries typically struggle to model dynamic dependency graphs, but adaptive embraces them as its core motivation.
*)


(**
## Adaptive Collections

As seen in the examples above `aval<'T>` and `cval<'T>` are containers for single values that may change adaptively.
A natural way of handling collections of values would be `aval<Set<'T>>`. However, this is not always optimal. The mapping function on the set would need to look like:
*)

let map (mapping: 'T1 -> 'T2) (set : aval<Set<'T1>>) =   
    set |> AVal.map (Set.map mapping)

(**
which effectively means that `mapping` will be executed for all elements of the set even if just a single element changes.
`cset<'T>` and `aset<'T>` solve this problem by effectively working on *deltas* instead of *values*.

> * `cset` **Changeable set**. Adaptive set container. Manually add and remove values.
> * `aset` **Adaptive set**. Adaptive set container. Content purely depends on other adaptive cells.
> * **Deltas**. Difference between two sets. A sequence of Additions and Removals to go from one set to the other set.

Here's an example illustrating `aset`s:
*)

let inputSet = cset [1;2;3]
let dependentSet =
    inputSet |> ASet.map (fun v -> printf "map %d, " v; v * 2)

printfn "%A" (AVal.force dependentSet.Content) // => map 1, map 2, map 3, HashSet [2; 4; 6]

(**
We create an `aset` and specify a mapping function on the elements. The mapping is evaluated for each element individually, as illustrated by the three "map" prints.

Let's add an element to the set.
*)
transact (fun () -> inputSet.Add 0)
printfn "%A" (AVal.force dependentSet.Content) // => map 0, HashSet [0; 2; 4; 6]
(**
The mapping function is evaluated only once, for the newly added element! `aset` is an *incremental* data structure on the level of its contained elements.
*)
transact (fun () -> inputSet.Remove 2)
printfn "%A" (AVal.force dependentSet.Content) // => HashSet [0; 2; 6]
(** 
There is no "map" print - the removal did not trigger an evaluation of the mapping function!

In addition to the unordered set, we also have implementations of the ordered list, called `alist`, and the key-value map, called `amap`.
*)

(** 
# Implementation Details

## Changeable, Adaptive and Constant

`cval<'T>` and `cset<'T>` also implement the unifying type `aval<'T>` and `aset<'T>` respectively. The changeable values can be used in places where adaptive value are expected (as seen in the example above). This helps the API to look nice.

The system distinguishes between two different kinds of objects: 

Changeable input values, which are prefixed with the letter c (e.g. `cval<'T>`, `cset<'T>`), and can imperatively be modified by user-code.

And Dependent values, which are prefixed with the letter a (e.g. `aval<'T>`, `aset<'T>`). These cannot directly be modified but instead depend on other adaptive values.

Internally, there is a third kind of adaptive value: The constant value, which can never be changed. 
In our system these are not expressed on the type-level, but instead expose a property `IsConstant`. 
All combinators correctly propagate constants in the dependency graph (when possible) and therefore:
*)

let shouldBeConstant = AVal.constant 5 |> AVal.map (fun v -> v * 10)
printfn "%A" (AVal.force shouldBeConstant)  // ==> 50
printfn "%A" shouldBeConstant.IsConstant // => true

(**
Whenever values are not constant, combinators like `AVal.map` internally build a dependency graph that keeps track of dependencies between cells. This dependency graph is used to find out what needs to be re-computed whenever changes are fed into the system.

Supplying changes to changeable cells can be done using `transact : (unit -> 'T) -> 'T`. The function makes the dependency graph consistent according to the nature and scope of the changes.
*)

(** 
## Evaluation strategy, avoidance of re-execution

Our system uses a *push-pull* evaluation strategy, meaning that evaluation will (by default) be lazy.
This is achieved via eagerly marking all affected values as *outOfDate*, and lazily making them
*upToDate* whenever they're evaluated. 
We opted for lazy evaluation since it causes *unneeded* parts of the dependency graph to impose virtually no runtime overhead. Lazy evaluation also allows us to support (limited) concurrency in the system.

On the downside, the marking process (performed by `transact`) needs to mark *all* potentially affected values as *outOfDate*,
even if their resulting value has not changed. 
However, we did our best to avoid re-execution of unchanged functions.
Here's an example illustrating our system's behaviour in such a circumstance:

*)
let someInput = cval 10
let someOutput = 
    someInput 
    |> AVal.map (fun v -> printf "%d %% 2 => " v; v % 2)
    |> AVal.map (fun v -> printf "%d * 10 => " v; v * 10)

printf "%A: " someOutput.OutOfDate; printfn "%A" (AVal.force someOutput) // => true: 10 % 2 => 0 * 10 => 0
transact (fun () -> someInput.Value <- 12)
printf "%A: " someOutput.OutOfDate; printfn "%A" (AVal.force someOutput) // => true: 12 % 2 => 0

(**
Note that the `v * 10` function doesn't get executed after the change, even though its result has been marked *outOfDate*. The system understood that the output wouldn't have changed either way.

*)