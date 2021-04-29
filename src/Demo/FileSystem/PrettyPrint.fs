module Pretty

open System
open FSharp.Data.Adaptive

let cprintfn color fmt =
    fmt |> Printf.kprintf (fun str ->
        let o = Console.ForegroundColor
        try
            Console.ForegroundColor <- color
            Console.WriteLine("{0}", str)
        finally
            Console.ForegroundColor <- o
            
    )

let rec print (pos : int) (a : list<Index * string>) (b : list<Index * ElementOperation<string>>) =
    match a with
    | (iha, vha) :: ta ->
        match b with
        | (ihb, vhb) :: tb ->
            if iha < ihb then
                printfn " %3d: %s" pos vha
                print (pos + 1) ta b
            elif iha > ihb then
                match vhb with
                | Set v -> 
                    cprintfn ConsoleColor.Green "    + %s" v
                | Remove ->
                    () // BAD
                print (pos + 1) a tb
            else
                match vhb with
                | Set v ->
                    cprintfn ConsoleColor.Red "    - %s" vha
                    cprintfn ConsoleColor.Green "    + %s"  v
                    print (pos + 1) ta tb
                | Remove ->
                    cprintfn ConsoleColor.Red "    - %s" vha
                    print pos ta tb
        | [] ->
            printfn " %3d: %s" pos vha
            print (pos + 1) ta b
    | [] ->
        match b with
        | (ihb, vhb) :: tb ->
            match vhb with
            | Set v ->
                cprintfn ConsoleColor.Green "    + %s" v
            | Remove ->
                ()
            print (pos + 1) a tb
        | [] ->
            ()
               
