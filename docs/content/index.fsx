(*** hide ***)
#I "../../bin/Release/netstandard2.0"
#r "netstandard"
#r "FSharp.Data.Adaptive.dll"
(**

# FSharp.Data.Adaptive : on demand adaptive/incremental data for F#

FSharp.Data.Adaptive aims at providing simple, yet powerful tools for developers to 
write incremental (or as we call it: adaptive) programs.
In contrast to RX or similar libraries adaptive focuses on values instead of events.
The most basic adaptive container-type we provide is `aval<'T>` and here's a little example of what it can do for you:
*)

open FSharp.Data.Adaptive

(** 
consider a falling ball that falls from `10m` heigh on earth (`9.81m/s^2`).
*)

let height  = AVal.init 10.0
let gravity = AVal.init 9.81
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

let time = AVal.init 0.5

// simple utility calculating the height
let calcHeight (t : float) (h0 : float) (g : float) =
    printf "height after %.3fs: " t
    h0 - 0.5*g*t*t

let currentHeight = AVal.map3 calcHeight time height gravity
printfn "%.3fm" (AVal.force currentHeight) // height after 0.500s: 1996.901m

(** but what about invalid times (let's say -10s)? *)
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

 