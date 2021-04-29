open System
open System.IO
open FSharp.Data.Adaptive
open System.Threading

// This example uses AdaptiveDirectory and AdaptiveFile to read all lines
// from all files in a directory maintaining a single adaptive list of lines.

[<EntryPoint>]
let main _argv =

    // create a temporary directory
    let dir = Path.Combine(Path.GetTempPath(), Guid.NewGuid() |> string)
    Directory.CreateDirectory dir |> ignore
    
    // get the lines of all files (sorted by name) concatenated in one alist.
    let content = 
        AdaptiveDirectory.GetFiles(dir, true)
        |> ASet.sortBy (fun info -> info.Name)
        |> AList.collect (fun info ->
            AdaptiveFile.ReadAllLinesAList info.FullName
        )
    
    // for demonstration purposes we want to wait until the
    // callback gets executed after each change, therefore
    // we use a Semaphore here.
    let callbackSem = new SemaphoreSlim(0)

    // subscribe to changes and pretty-print the changes.
    let subscription = 
        content.AddCallback (fun oldState delta ->
            // print the change
            Pretty.print 1 (IndexList.toListIndexed oldState) (IndexListDelta.toList delta)
            printfn ""

            // tell the main-thread we're done printing 
            callbackSem.Release() |> ignore
        )

    // some temporary files
    let a = Path.Combine(dir, "a.txt")
    let b = Path.Combine(dir, "b.txt")
    let c = Path.Combine(dir, "c.txt")
    let d = Path.Combine(dir, "d.txt")

    // perform some changes and wait for our print
    printfn "write a.txt"
    File.WriteAllText(a, "This is file a")
    callbackSem.Wait()

    printfn "write c.txt"
    File.WriteAllText(c, "File c has\r\ntwo lines")
    callbackSem.Wait()

    printfn "write b.txt"
    File.WriteAllText(b, "b also exists now!")
    callbackSem.Wait()

    printfn "update b.txt"
    File.WriteAllText(b, "b has a new line!\r\nb also exists now!\r\nand another one")
    callbackSem.Wait()

    printfn "delete a.txt"
    File.Delete a
    callbackSem.Wait()
    
    printfn "write a.txt"
    File.WriteAllText(a, "THIS\r\nIS FILE A")
    callbackSem.Wait()

    printfn "b.txt -> d.txt"
    File.Move(b, d)
    callbackSem.Wait()


    // dispose our subscription and delete the temp-directory
    subscription.Dispose()
    Directory.Delete(dir, true)


    0
