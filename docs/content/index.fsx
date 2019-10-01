(*** hide ***)
#I "../../bin/Release/netstandard2.0"
#r "netstandard"
#r "FSharp.Data.Adaptive.dll"
open FSharp.Data.Adaptive

(**

# FSharp.Data.Adaptive : adaptive data for F#

FSharp.Data.Adaptive aims at providing simple, yet powerful tools for developers to 
write incremental (or as we call it: adaptive) programs.
In contrast to RX or similar libraries adaptive focuses on values instead of events.
The most basic adaptive container-type we provide is `aval<'T>` and here's a little example of what it can do for you:
*)


(** 
### Simple Example

consider a falling ball that falls from `10m` height on earth (`9.81m/s^2`).
*)
let height  = cval 10.0
let gravity = cval 9.81
(** so when will the ball be hitting the floor? *)
let timeToFloor = AVal.map2 (fun h g -> sqrt (2.0 * h / g)) height gravity
printfn "%.3fs" (AVal.force timeToFloor) // => 1.428s

(** so how long will it take on the moon? *)
transact (fun () -> gravity.Value <- 1.62)
printfn "%.3fs" (AVal.force timeToFloor) // => 3.514s

(** what about 2000m height on jupiter? *)
transact (fun () ->
    gravity.Value <- 24.79
    height.Value <- 2000.0
)
printfn "%.3fs" (AVal.force timeToFloor) // => 12.703s


(** so currently this feels a lot like excel calcuations where dependent values get updated
whenever their inputs change which is not really impressive. The API becomes interesting when **structural** / **dynamic** dependencies
are needed.
So let's calculate the height where our ball will be at a certain point in time.
*)

let time = cval 0.5

// simple utility calculating the height
let calcHeight (t : float) (h0 : float) (g : float) =
    printf "height after %.3fs: " t
    h0 - 0.5*g*t*t

let currentHeight = AVal.map3 calcHeight time height gravity
printfn "%.3fm" (AVal.force currentHeight) // height after 0.500s: 1996.901m

(** but what about invalid times (let's say -100s)? *)
transact (fun () -> time.Value <- -100.0)
printfn "%.3fm" (AVal.force currentHeight) // => height after -100.000s: -121950.000m  
(** that's not really what we wanted!
 so let's rewrite the code using a new combinator (namely `bind`)
*)

let currentHeightNew =
    time |> AVal.bind (fun t ->
        if t <= 0.0 then 
            // whenever the time is negative the ball will just report its initial height.
            height :> aval<_>
        else
            // when the time is positive we use our utility to calculate the height.
            AVal.map2 (calcHeight t) height gravity
    )
    
printfn "%.3fm" (AVal.force currentHeightNew) // => 2000.000m
(** that's better! NOTE that there is no `"height after"` print! 
  so what if we change the gravity back to earth?
*)
transact (fun () -> gravity.Value <- 9.81)
printfn "%.3fm" (AVal.force currentHeightNew) // => 2000.000m (still no computation)

(** okay, set the height to 10m again. *)
transact (fun () -> height.Value <- 10.0)
printfn "%.3fm" (AVal.force currentHeightNew) // 10.0m (still no computation)

(** so let's set the time to something positive again and see if the result will be correct. *)
transact (fun () -> time.Value <- 1.42785)
printfn "%.3fm" (AVal.force currentHeightNew) // height after 1.428s: 0.000m


(** What this simple example demonstrates is that `aval<'T>` is capable of expressing **dynamic** dependency graphs.
The system *knows* that the resulting value doesn't depend on gravity/height as long as the current time is negative but still can compute the 
correct result once the time gets positive again. This may seem trivial at a first glance but this kind of dependency 
cannot be expressed in traditional reactive-programming libraries.

*)


(** 
### Changeable, Adaptive and Constant

First of all the system distinguishes between two different kinds of objects: 

1. changeable input values (e.g. `cval<'T>`, `cset<'T>`) that can imperatively be modified by user-code.
2. dependent values (e.g. `aval<'T>`, `aset<'T>`) that cannot directly be modified but instead depend on other values.

However note that `cval<'T>` and `cset<'T>` also implement `aval<'T>` and `aset<'T>` respectively and can therefore
be used in places where these are expected (as seen in the example above).
Actually there is a third kind of objects: the ones which can never be changed. 
In our system these are not expressed on type-level but instead expose a property `IsConstant`. 
All combinators correctly track constants (when possible) and therefore:

*)

let shouldBeConstant = AVal.constant 5 |> AVal.map (fun v -> v * 10)
printfn "%A" (AVal.force shouldBeConstant)  // ==> 50
printfn "%A" shouldBeConstant.IsConstant // => true

(**
Whenever values are not constant combinators like `AVal.map` internally build a dependency-graph that keeps 
track of dependencies between cells and is needed whenever changes are fed into the system.
Supplying changes can be done using `transact : (unit -> 'T) -> 'T` which takes care of making the dependency-graph consistent after a certain change.

*)

(** 
### Evaluation Strategy

Our system uses a *push-pull* evaluation strategy meaning that evaluation will (by default) be lazy.
This is achieved via eagerly marking all affected values as *outOfDate* and lazily making them
*upToDate* whenever they're evaluated. 
We opted for lazy evaluation since *unneeded* parts of the dependency graph impose virtually no runtime overhead 
and it allows us to support (limited) concurrency in the system.
On the downside the marking process (performed by `transact`) needs to mark all *potentially affected* values as *outOfDate*
even though their resulting value may not have changed. 
However we did our best to avoid re-execution of unchanged functions.
Here's an example illustrating this behaviour:

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
Note that the `v * 10` function doesn't get executed after the change although its result has been marked *outOfDate*.

*)

(**
### Adaptive Collections

As seen in the examples above `aval<'T>` and `cval<'T>` are containers for single values that may change adaptively.
A natural way of handling collections of values would be `aval<Set<'T>>`. However this is not optimal since the map function would then
need to look like:

*)

let map (mapping: 'T1 -> 'T2) (set : aval<Set<'T1>>) =   
    set |> AVal.map (Set.map mapping)

(**
which effectively means that `mapping` will be executed for all elements of the set whenever something changes.
`cset<'T>` and `aset<'T>` solve this problem via effectively working on *deltas* instead of *values*.

Here's an example illustrating this:
*)

let inputSet = cset [1;2;3]
let dependentSet =
    inputSet |> ASet.map (fun v -> printf "map %d, " v; v * 2)

printfn "%A" (AVal.force dependentSet.Content) // => map 1, map 2, map 3, HashSet [2; 4; 6]

// so let's add an element to the set
transact (fun () -> inputSet.Add 0)
printfn "%A" (AVal.force dependentSet.Content) // => map 0, HashSet [0; 2; 4; 6]

transact (fun () -> inputSet.Remove 2)
printfn "%A" (AVal.force dependentSet.Content) // => HashSet [0; 2; 6]
// Note that there's no `map 2` print!


