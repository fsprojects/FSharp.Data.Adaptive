module ``Basic Set tests``

open FsUnit
open Xunit
open FsCheck
open FsCheck.Xunit
open FsIncremental


[<Fact>]
let testTimesTwo () =
    2 |> Test.timesTwo |> (fun a -> should equal 4 a)


[<Property>]
let addAdds (s : Set<int>) (n : int) =
    [
        Set.add n s |> Set.contains n
    ]    
        
[<Property>]
let remRemoves (s : Set<int>) (n : int) =
    [
        Set.remove n s |> Set.contains n |> not
    ]    

