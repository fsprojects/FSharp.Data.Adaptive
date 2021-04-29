namespace FSharp.Data.Adaptive

open System.IO
open System.Diagnostics
open System.Threading
open FSharp.Data.Traceable
open System
open System.IO

#if !FABLE_COMPILER

/// Helpers for AdaptiveDirectory & AdaptiveFile
[<AutoOpen>]
module private AdaptiveFileSystemUtilities = 

    type FileSystemInfo with
        static member TryGet(path : string) =
            if File.Exists path then FileInfo path :> FileSystemInfo |> Some
            elif Directory.Exists path then DirectoryInfo path :> FileSystemInfo |> Some
            else None

    type Path with
        static member Normalize(path : string) =
            let path = Path.GetFullPath(path.Trim())
            if path.EndsWith(String(Path.DirectorySeparatorChar, 1)) || path.EndsWith (String(Path.AltDirectorySeparatorChar, 1)) then
                path.Substring(0, path.Length-1)
            else
                path

    type File with
        static member ReadAllTextSafe(path : string, ?retires : int) =
            let rec run (cnt : int) (file : string) =
                if cnt <= 0 then
                    File.ReadAllText file
                else
                    try File.ReadAllText file
                    with _ -> Thread.Sleep 16; run (cnt - 1) file

            let retries = defaultArg retires 5
            run retries path

        static member ReadAllBytesSafe(path : string, ?retires : int) =
            let rec run (cnt : int) (file : string) =
                if cnt <= 0 then
                    File.ReadAllBytes file
                else
                    try File.ReadAllBytes file
                    with _ -> Thread.Sleep 16; run (cnt - 1) file

            let retries = defaultArg retires 5
            run retries path

        static member ReadAllLinesSafe(path : string, ?retires : int) =
            let rec run (cnt : int) (file : string) =
                if cnt <= 0 then
                    File.ReadAllLines file
                else
                    try File.ReadAllLines file
                    with _ -> Thread.Sleep 16; run (cnt - 1) file

            let retries = defaultArg retires 5
            run retries path

    let (|Strong|_|) (w : WeakReference<'a>) =
        match w.TryGetTarget() with
        | (true, t) -> Some t
        | _ -> None

    type FsWatcher private(path : string, watcher : FileSystemWatcher, refCount : ref<int>) =
        static let watchers = System.Collections.Generic.Dictionary<string, WeakReference<FileSystemWatcher> * ref<int>>()
        static let destroy = new System.Collections.Concurrent.BlockingCollection<ref<list<IDisposable>>>()

        static do
            let run() =
                for e in destroy.GetConsumingEnumerable() do
                    for e in !e do e.Dispose()
                    e := []
            let thread = Thread(ThreadStart(run), IsBackground = true, Name = "WatcherDestroyThread")
            thread.Start()

        let mutable watcher = watcher
        let mutable disposables : list<IDisposable> = []

        static member Get (path : string) =
            let path = Path.Normalize path

            let watcher, refCount =
                lock watchers (fun () ->
                    match watchers.TryGetValue path with
                    | (true, (Strong w,r)) -> 
                        r := !r + 1
                        w, r
                    | _ ->
                        let w = new FileSystemWatcher(path)
                        w.EnableRaisingEvents <- true
                        let r = ref 1
                        let t = (WeakReference<_> w, r)
                        watchers.[path] <- t
                        w, r
                )

            FsWatcher(path, watcher, refCount)

        override x.Finalize() = 
            let self =
                let watcher = watcher
                let refCount = refCount
                let path = path
                { new IDisposable with
                    member x.Dispose() =
                        lock watchers (fun () ->
                            refCount := !refCount - 1
                            if !refCount <= 0 then
                                watchers.Remove path |> ignore
                                watcher.Dispose()
                        )
                        
                }

            destroy.Add (ref (self :: disposables))

            disposables <- []
            watcher <- Unchecked.defaultof<_>

        member x.Add(callback : FileSystemEventArgs -> unit) =
            disposables <-
                watcher.Changed.Subscribe callback ::
                watcher.Renamed.Subscribe callback ::
                watcher.Created.Subscribe callback ::
                watcher.Deleted.Subscribe callback ::
                disposables


    type DirectoryWatchReader<'a>(directory : string, mapping : FileSystemInfo -> voption<'a>) =
        inherit AbstractReader<HashSetDelta<'a>>(HashSetDelta.empty)

        let mutable watcher : option<FsWatcher> = None
        let pending = ref HashSetDelta.empty<FileSystemInfo>
        let mapping = Cache mapping

        let infoCache = System.Collections.Generic.Dictionary<string, FileSystemInfo>()


        static let getInfo (infoCache : System.Collections.Generic.Dictionary<string, FileSystemInfo>) (path : string) =
            match infoCache.TryGetValue path with
            | (true, i) -> 
                i.Refresh()
                i
            | _ -> 
                let info = 
                    if Directory.Exists path then DirectoryInfo path :> FileSystemInfo
                    else FileInfo path :> FileSystemInfo
                infoCache.[path] <- info
                info

        static let subscribe (w : FsWatcher) (self : WeakReference<IAdaptiveObject>) (pending : ref<_>) (infoCache : System.Collections.Generic.Dictionary<string, FileSystemInfo>) =
            w.Add(fun e ->
                let w = ()
                lock infoCache (fun () ->
                    match e.ChangeType with
                    | WatcherChangeTypes.Created ->
                        let info = getInfo infoCache e.FullPath
                        pending := HashSetDelta.combine !pending (HashSetDelta.single (Add info))
                    
                        match self.TryGetTarget() with
                        | (true, s) -> transact s.MarkOutdated
                        | _ -> ()

                    | WatcherChangeTypes.Deleted ->
                        match infoCache.TryGetValue e.FullPath with
                        | (true, info) ->
                            infoCache.Remove e.FullPath |> ignore
                            pending := HashSetDelta.combine !pending (HashSetDelta.single (Rem info))
                            match self.TryGetTarget() with
                            | (true, s) -> transact s.MarkOutdated
                            | _ -> ()
                        | _ ->  
                            ()
                    | WatcherChangeTypes.Renamed ->
                        let e = unbox<RenamedEventArgs> e
                        let info = getInfo infoCache e.FullPath
                        match infoCache.TryGetValue e.OldFullPath with
                        | (true, oldInfo) ->
                            infoCache.Remove e.OldFullPath |> ignore
                            pending := HashSetDelta.combine !pending (HashSetDelta.ofList [Rem oldInfo; Add info])
                        | _ ->
                            pending := HashSetDelta.combine !pending (HashSetDelta.single (Add info))
                        match self.TryGetTarget() with
                        | (true, s) -> transact s.MarkOutdated
                        | _ -> ()
                            
                    | _ ->
                        ()
                )
            )

        let mapDelta (delta : HashSetDelta<FileSystemInfo>) =
            delta |> HashSetDelta.chooseV (function 
                | Add(_, v) ->
                    match mapping.Invoke v with 
                    | ValueSome r -> ValueSome (Add r)
                    | ValueNone -> ValueNone
                | Rem(_, v) ->
                    match mapping.Revoke v with 
                    | ValueSome r -> ValueSome (Rem r)
                    | ValueNone -> ValueNone
            )

        override x.Compute(_token : AdaptiveToken) =
            match watcher with
            | Some _ ->
                let delta = 
                    lock infoCache (fun () ->
                        let d = !pending
                        pending := HashSetDelta.empty
                        d
                    )
                mapDelta delta
            | None ->
                let initial = Directory.GetFileSystemEntries directory |> Array.map (getInfo infoCache >> Add) |> HashSetDelta.ofArray
                let w = FsWatcher.Get directory

                subscribe w x.Weak pending infoCache

                watcher <- Some w
                mapDelta initial

    [<AbstractClass>]
    type FileWatchVal<'a>(file : string) =
        inherit AVal.AbstractVal<'a>()
        let file = Path.GetFullPath file
        let mutable watcher : option<FsWatcher> = None

        abstract member Read : unit -> 'a 
        
        override x.Compute(token : AdaptiveToken) =
            match watcher with
            | Some _ -> ()
            | None ->
                let w = FsWatcher.Get(Path.GetDirectoryName file)
                let self = x.Weak
                let file = file
                w.Add(fun e ->
                    if e.FullPath = file then 
                        match self.TryGetTarget() with
                        | (true, s) -> transact s.MarkOutdated
                        | _ -> ()
                )
                watcher <- Some w

            x.Read()

    type TryReadAllTextVal(file : string) =
        inherit FileWatchVal<option<string>>(file)

        override x.Read() =
            if File.Exists file then 
                try File.ReadAllTextSafe file |> Some
                with _ -> None
            else 
                None

    type TryReadAllBytesVal(file : string) =
        inherit FileWatchVal<option<byte[]>>(file)

        override x.Read() =
            if File.Exists file then
                try File.ReadAllBytesSafe file |> Some
                with _ -> None
            else 
                None

    type TryReadAllLinesVal(file : string) =
        inherit FileWatchVal<option<string[]>>(file)

        override x.Read() =
            if File.Exists file then
                try File.ReadAllLinesSafe file |> Some
                with _ -> None
            else 
                None

    type ReadAllTextVal(file : string) =
        inherit FileWatchVal<string>(file)
        override x.Read() = File.ReadAllTextSafe file

    type ReadAllBytesVal(file : string) =
        inherit FileWatchVal<byte[]>(file)
        override x.Read() = File.ReadAllBytesSafe file
            
    type ReadAllLinesVal(file : string) =
        inherit FileWatchVal<string[]>(file)
        override x.Read() = 
            if File.Exists file then
                try File.ReadAllLinesSafe file
                with _ -> [||]
            else 
                [||]

/// Adaptive functions for handling directories.
[<AbstractClass; Sealed>]
type AdaptiveDirectory private() =

    /// Adaptively determines whether the specified directory exists.
    static member Exists(path : string) =
        { new FileWatchVal<bool>(path) with
            override x.Read() = Directory.Exists path
        } :> aval<_>

    
    /// Adaptively gets the creation time for the specified directory.
    static member GetCreationTime(path : string) =
        { new FileWatchVal<DateTime>(path) with
            override x.Read() = Directory.GetCreationTime path
        } :> aval<_>
        
    /// Adaptively gets the creation time (in UTC) for the specified directory.
    static member GetCreationTimeUtc(path : string) =
        { new FileWatchVal<DateTime>(path) with
            override x.Read() = Directory.GetCreationTimeUtc path
        } :> aval<_>
        
    /// Adaptively gets last access time for the specified directory.
    static member GetLastAccessTime(path : string) =
        { new FileWatchVal<DateTime>(path) with
            override x.Read() = Directory.GetLastAccessTime path
        } :> aval<_>
        
    /// Adaptively gets last access time (in UTC) for the specified directory.
    static member GetLastAccessTimeUtc(path : string) =
        { new FileWatchVal<DateTime>(path) with
            override x.Read() = Directory.GetLastAccessTimeUtc path
        } :> aval<_>
        
    /// Adaptively gets last write time for the specified directory.
    static member GetLastWriteTime(path : string) =
        { new FileWatchVal<DateTime>(path) with
            override x.Read() = Directory.GetLastWriteTime path
        } :> aval<_>
        
    /// Adaptively gets last write time (in UTC) for the specified directory.
    static member GetLastWriteTimeUtc(path : string) =
        { new FileWatchVal<DateTime>(path) with
            override x.Read() = Directory.GetLastWriteTimeUtc path
        } :> aval<_>

    /// Gets an aset<FileSystemInfo> holding all existing Files/Directories in path
    /// whose name matches the specified Regex (if any).
    static member GetFileSystemEntries(path : string, ?regex : System.Text.RegularExpressions.Regex) =
        let path = Path.Normalize path

        match regex with
        | Some rx ->
            ASet.ofReader (fun () ->
                DirectoryWatchReader<FileSystemInfo>(path, fun e ->
                    if rx.IsMatch e.Name then ValueSome e
                    else ValueNone
                )
            )
        | None ->
            ASet.ofReader (fun () ->
                DirectoryWatchReader<FileSystemInfo>(path, ValueSome)
            )
        
    /// Gets an aset<FileInfo> holding all existing Files in path (recursively)
    /// whose name matches the specified Regex (if any).
    static member GetFiles(path : string, ?regex : System.Text.RegularExpressions.Regex, ?recursive : bool) =
        let path = Path.Normalize path
        let recursive = defaultArg recursive false

        if recursive then
            let all = ASet.ofReader (fun () -> DirectoryWatchReader<_>(path, ValueSome))
            all |> ASet.collect (function
                | :? FileInfo as f -> 
                    match regex with
                    | Some rx when not (rx.IsMatch f.Name) -> ASet.empty
                    | _ -> ASet.single f
                | :? DirectoryInfo as d -> AdaptiveDirectory.GetFiles(d.FullName, ?regex = regex, recursive = true)
                | _ -> ASet.empty
            )
        else
            ASet.ofReader (fun () ->
                DirectoryWatchReader<FileInfo>(path, function
                    | :? FileInfo as f -> 
                        match regex with
                        | Some rx when not (rx.IsMatch f.Name) -> ValueNone
                        | _ -> ValueSome f
                    | _ -> ValueNone
                )
            )
     
    /// Gets an aset<FileInfo> holding all existing Files in path (recursively)
    /// whose name matches the specified Regex-pattern (if any).
    static member GetFiles(path : string, ?pattern : string, ?recursive : bool) =
        let rx = pattern |> Option.map (System.Text.RegularExpressions.Regex)
        AdaptiveDirectory.GetFiles(path, ?regex = rx, ?recursive = recursive)
        
    /// Gets an aset<FileInfo> holding all existing Files in path (recursively).
    static member GetFiles(path : string, recursive : bool) =
        AdaptiveDirectory.GetFiles(path, ?regex = None, recursive = recursive)
        
    /// Gets an aset<FileInfo> holding all existing Files in path.
    static member GetFiles(path : string) =
        AdaptiveDirectory.GetFiles(path, ?regex = None, ?recursive = None)
    
/// Adaptive functions for handling files.
[<AbstractClass; Sealed>]
type AdaptiveFile private() =

    /// Adaptively determines whether the specified file exists.
    static member Exists(path : string) =
        { new FileWatchVal<bool>(path) with
            override x.Read() = File.Exists path
        } :> aval<_>
        
    /// Adaptively get the FileAttributes for the path.
    static member GetAttributes(path : string) =
        { new FileWatchVal<FileAttributes>(path) with
            override x.Read() = File.GetAttributes path
        } :> aval<_>
        
    
    /// Adaptively gets the creation time for the specified file.
    static member GetCreationTime(path : string) =
        { new FileWatchVal<DateTime>(path) with
            override x.Read() = File.GetCreationTime path
        } :> aval<_>
        
    /// Adaptively gets the creation time (in UTC) for the specified file.
    static member GetCreationTimeUtc(path : string) =
        { new FileWatchVal<DateTime>(path) with
            override x.Read() = File.GetCreationTimeUtc path
        } :> aval<_>
        
    /// Adaptively gets last access time for the specified file.
    static member GetLastAccessTime(path : string) =
        { new FileWatchVal<DateTime>(path) with
            override x.Read() = File.GetLastAccessTime path
        } :> aval<_>
        
    /// Adaptively gets last access time (in UTC) for the specified file.
    static member GetLastAccessTimeUtc(path : string) =
        { new FileWatchVal<DateTime>(path) with
            override x.Read() = File.GetLastAccessTimeUtc path
        } :> aval<_>
        
    /// Adaptively gets last write time for the specified file.
    static member GetLastWriteTime(path : string) =
        { new FileWatchVal<DateTime>(path) with
            override x.Read() = File.GetLastWriteTime path
        } :> aval<_>
        
    /// Adaptively gets last write time (in UTC) for the specified file.
    static member GetLastWriteTimeUtc(path : string) =
        { new FileWatchVal<DateTime>(path) with
            override x.Read() = File.GetLastWriteTimeUtc path
        } :> aval<_>
        
    /// Adaptively tries to read the file's content as aval<option<string>>.
    static member TryReadAllText(path : string) = 
        TryReadAllTextVal(Path.Normalize path) :> aval<_>
        
    /// Adaptively reads the file's content as aval<string>.
    static member ReadAllText(path : string) = 
        ReadAllTextVal(Path.Normalize path) :> aval<_>
    
    /// Adaptively tries to read the file's content as aval<option<byte[]>>.
    static member TryReadAllBytes(path : string) = 
        TryReadAllBytesVal(Path.Normalize path) :> aval<_>
        
    /// Adaptively reads the file's content as aval<byte[]>.
    static member ReadAllBytes(path : string) = 
        ReadAllBytesVal(Path.Normalize path) :> aval<_>

    /// Adaptively tries to read the file's lines as aval<option<string[]>>.    
    static member TryReadAllLines(path : string) = 
        TryReadAllLinesVal(Path.Normalize path) :> aval<_>
        
    /// Adaptively reads the file's lines as aval<string[]>.   
    static member ReadAllLines(path : string) = 
        ReadAllLinesVal(Path.Normalize path) :> aval<_>
    
    /// Adaptively reads the file's lines as alist<string>.   
    static member ReadAllLinesAList(path : string) = 
        TryReadAllLinesVal(Path.Normalize path) 
        |> AVal.mapNonAdaptive (function Some l -> l | None -> [||]) 
        |> AList.ofAVal


#endif

