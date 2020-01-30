open System
open System.IO
open System.Threading
open FSharp.Data.Adaptive

// This example outlines one possible way of interoperating with
// event-based systems and imperatively keeping an output up-to-date.
// Please note that this kind of adapter code tends to get dirty and 
// there may be better ways for doing this (depending on the concrete use-case).
// However the pattern outlined below is used in Aardvark.Rendering and Aardvark.UI 
// for keeping mutable datastructures in sync with our system's state.
//
// As a basic example we're gonna build an adapter for adaptively listing a directory
// and writing operations to the console. 
// Of course this could easier be done without adaptive system but that's not the point here.


/// Our entries can either be files with their size or directories without any info.
type FileSystemEntryKind =
    | File of size : int64
    | Directory

/// An entry consists of a kind and a fully qualified path
type FileSystemEntry =
    {
        kind    : FileSystemEntryKind
        path    : string
    }

/// Basic functions for an adaptive FileSystem.
module FileSystem =
    /// Helper function for creating FileSystemEntries
    let tryGetEntry (path: string) =
        try
            if File.Exists path then 
                // get the file info
                let info = System.IO.FileInfo path
                Some {
                    kind = File info.Length
                    path = path
                }

            elif Directory.Exists path then 
                // is a directory
                Some {
                    kind = Directory
                    path = path
                }

            else 
                None
          
        with _ ->
            // handle permission problems
            None

    /// Watches the given directory and returns an amap<string, FileSystemEntry>
    let watchDirectory (path: string) =
        let info = DirectoryInfo path
        if info.Exists then
            /// the changeable map holding all the results
            let entries = cmap<string, FileSystemEntry>()

            /// our adapter for pushing events into the adaptive system.
            let handleChange (e : FileSystemEventArgs) =
                // In this example we use `transact` directly in the callback which
                // causes the resulting value to get marked immediately, which in turn
                // will cause immediate evaluation of our amap.
                // This way we lost the ability of feeding batch-changes into our system.
                // In real-world implementation we often use some for of 'buffering' that
                // just records changes on the input and eventually flushes them.
                match e.ChangeType with
                | WatcherChangeTypes.Created | WatcherChangeTypes.Changed ->
                    match tryGetEntry e.FullPath with
                    | Some entry ->
                        transact (fun () -> entries.[e.Name] <- entry)
                    | None ->
                        ()

                | WatcherChangeTypes.Deleted ->
                    transact (fun () ->
                        entries.Remove e.Name |> ignore
                    )

                | WatcherChangeTypes.Renamed ->
                    let e = unbox<RenamedEventArgs> e
                    match HashMap.tryFind e.OldName entries.Value with
                    | Some old ->
                        transact (fun () ->
                            entries.Remove e.OldName |> ignore
                            entries.[e.Name] <- { old with path = e.FullPath }
                        )
                    | None ->
                        match tryGetEntry e.FullPath with
                        | Some entry -> transact (fun () -> entries.[e.Name] <- entry )
                        | None -> ()
                | change ->
                    printfn "unknown change type: %A" change

            // create the watcher with all needed handlers.
            let watcher = new FileSystemWatcher(info.FullName, "*")
            watcher.NotifyFilter <- NotifyFilters.Attributes ||| NotifyFilters.FileName ||| NotifyFilters.DirectoryName
            watcher.Changed.Add handleChange
            watcher.Created.Add handleChange
            watcher.Deleted.Add handleChange
            watcher.Renamed.Add handleChange
            watcher.EnableRaisingEvents <- true

            // add all currently living entries
            for e in info.GetFileSystemInfos() do
                match tryGetEntry e.FullName with
                | Some entry -> entries.[e.Name] <- entry
                | None -> ()

            entries :> amap<_,_>
        else
            AMap.empty

/// utility for starting a thread
let startThread (run: unit -> unit) =
    let thread = Thread(ThreadStart(run), IsBackground = true)
    thread.Start()
    thread

[<EntryPoint>]
let main argv =

    let set1 = cset [1;2;3;4]
    let set2 = cset [3;4;5;6]

    let a = set1 |> ASet.map (fun v -> v % 2)
    let b = set2 |> ASet.map (fun v -> v % 2)

    let test = ASet.intersect a b
    let r = test.GetReader()

    let eval() =
        let ops = r.GetChanges AdaptiveToken.Top
        let state = r.State

        let state = state |> Seq.map (sprintf "%A") |> String.concat "; " |> sprintf "{ %s }"
        let a = a |> ASet.force |> Seq.map (sprintf "%A") |> String.concat "; " |> sprintf "{ %s }"
        let b = b |> ASet.force |> Seq.map (sprintf "%A") |> String.concat "; " |> sprintf "{ %s }"


        if ops.Count = 0 then
            printfn "%s ^ %s = %s (nop)" a b state
        else
            let ops = ops |> Seq.map (sprintf "%A") |> String.concat "; " |> sprintf "{ %s }"
            printfn "%s ^ %s = %s (%s)" a b state ops

    eval()

    transact (fun () -> set1.UnionWith [6])
    eval()

    transact (fun () -> set2.ExceptWith [6])
    eval()

    transact (fun () -> set2.ExceptWith [4])
    eval()

    transact (fun () -> set1.Clear())
    eval()
    Environment.Exit 0


    // which directory to watch
    let dir = 
        if argv.Length > 0 then argv.[0]
        else Environment.GetFolderPath(Environment.SpecialFolder.Desktop)

    printfn "watching %s" dir

    // list all entries for the given directory
    let entries = FileSystem.watchDirectory dir

    // The following code creates a thread that pulls changes from the
    // entries above whenever something changed in the amap.
    // This pattern can be quite confusing but it illustrates a way of 
    // eagerly feeding changes somewhere (in this case just the console).
    
    // we need some form of synchronization and signaling for our thread
    // and use Monitor for this here.
    let lockObj = obj()
    let reader = entries.GetReader()
    let mutable pending = true
    let mutable running = true

    // whenever the reader gets marked *outOfDate* the callback will get executed.
    // note that it doesn't evaluate anything but instead tells our thread that
    // pending changes may exist.
    let subscription = 
        reader.AddMarkingCallback(fun () ->
            lock lockObj (fun () ->
                pending <- true
                Monitor.PulseAll lockObj
            )   
        )

    let clearLine() =
        Console.Write(System.String(' ', Console.WindowWidth - 1))
        Console.SetCursorPosition(0, Console.CursorTop)

    let writeLastLine (str : string) =
        let missing = Console.WindowWidth - str.Length - 1
        Console.Write("{0}{1}", str, System.String(' ', missing))
        Console.SetCursorPosition(0, Console.CursorTop)

    // here we start the thread that actually pulls changes from out reader
    // and prints the results to the console.
    let thread = 
        startThread (fun () ->
            while running do    
                // wait until running gets false or pending gets true.
                lock lockObj (fun () ->
                    while not pending && running do
                        Monitor.Wait lockObj |> ignore
                    pending <- false
                )

                // if still running pull the changes
                if running then
                    // note that the marking of our reader and the evaluation
                    // may happen on different threads!
                    let old = reader.State
                    let ops = reader.GetChanges AdaptiveToken.Top

                    clearLine()
                    // Here some mutable output-datastructure (like a DOM/Code/etc.) could
                    // be mutated in order to reflect the necessary changes.
                    // However our simple example just prints the operations to the (mutable) console.
                    for (name, op) in ops do
                        let old = HashMap.tryFind name old
                        match old, op with
                        | Some _, Set n ->
                            printfn "> changed %s: %A" name n.kind
                        | None, Set n ->
                            printfn "> added %s: %A" name n.kind
                        | Some o, Remove ->
                            printfn "> removed %s: %A" name o.kind
                        | None, Remove ->
                            ()
                    writeLastLine "press enter to exit..."
        )



    // wait until the user presses enter
    Console.ReadLine() |> ignore
    Console.SetCursorPosition(0, Console.CursorTop - 1)
    clearLine()
    printfn "shutting down"
    // shutdown our thread and wait until it terminates
    lock lockObj (fun () ->
        subscription.Dispose()
        running <- false
        Monitor.PulseAll lockObj
    )
    thread.Join()
    printfn "bye!"


    0
