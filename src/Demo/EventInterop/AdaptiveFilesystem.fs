namespace FSharp.Data.Adaptive

open System.IO
open System.Diagnostics
open System.Threading
open FSharp.Data.Traceable
open System
open System.Threading.Tasks
open System.Runtime.CompilerServices

[<AutoOpen>]
module private FileSystemUtilities =
    let dprintfn fmt = Printf.kprintf (fun str -> Debug.WriteLine str) fmt

    let normalizePath (path : string) =
        let path = Path.GetFullPath(path.Trim())
        if path.EndsWith Path.DirectorySeparatorChar || path.EndsWith Path.AltDirectorySeparatorChar then
            path.Substring(0, path.Length-1)
        else
            path

    /// Cache represents a cached function which can be 
    /// invoked and revoked. invoke increments the reference
    /// count for a specific argument (possibly causing the 
    /// function to be executed) whereas revoke decreases the
    /// eeference count and removes the cache entry whenever
    /// the reference count is 0.
    type Cache<'T1, 'T2>(mapping : 'T1 -> 'T2) =  
        /// utility checking for null (if possible)
        static let isNull =
            #if FABLE_COMPILER
            fun (o : 'T1) -> isNull (o :> obj)
            #else
            if typeof<'T1>.IsValueType then fun (_o : 'T1) -> false
            else fun (o : 'T1) -> isNull (o :> obj)
            #endif 

        /// cache for non-null values.
        let cache = DefaultDictionary.create<'T1, 'T2 * ref<int>>()

        /// cache for null values (needed for option, unit, etc.)
        let mutable nullCache = None

        /// Clear removes all entries from the Cache and
        /// executes a function for all removed cache entries.
        /// This function is helpful if the contained values
        /// are (for example) disposable resources.
        member x.Clear(remove : 'T2 -> unit) =
            for (KeyValue(_,(v,_))) in cache do 
                remove v
            cache.Clear()
            match nullCache with
                | Some (v,_) -> 
                    remove v
                    nullCache <- None
                | None -> ()

        /// Invoke returns the function value associated
        /// With the given argument (possibly executing the function)
        /// And increases the associated reference count.
        member x.Invoke (v : 'T1) =
            if isNull v then
                match nullCache with
                    | Some (r, ref) -> 
                        ref := !ref + 1
                        r
                    | None ->
                        let r = mapping v
                        nullCache <- Some(r, ref 1)
                        r
            else
                match cache.TryGetValue v with
                    | (true, (r, ref)) -> 
                        ref := !ref + 1
                        r
                    | _ ->
                        let r = mapping v
                        cache.[v] <- (r, ref 1)
                        r
        /// Revoke returns the function value associated
        /// With the given argument and decreases its reference count.
        member x.RevokeAndGetDeleted (v : 'T1) =
            if isNull v then
                match nullCache with
                    | Some (r, ref) -> 
                        ref := !ref - 1
                        if !ref = 0 then
                            nullCache <- None
                            (true, r)
                        else
                            (false, r)
                    | None -> failwithf "cannot revoke null"
            else
                match cache.TryGetValue v with
                    | (true, (r, ref)) -> 
                        ref := !ref - 1
                        if !ref = 0 then
                            cache.Remove v |> ignore
                            (true, r)
                        else
                            (false, r)
                    | _ -> failwithf "cannot revoke unknown value: %A" v
                
        /// Revoke returns the function value associated
        /// With the given argument and decreases its reference count.
        member x.RevokeAndGetDeletedTotal (v : 'T1) =
            if isNull v then
                match nullCache with
                    | Some (r, ref) -> 
                        ref := !ref - 1
                        if !ref = 0 then
                            nullCache <- None
                            Some (true, r)
                        else
                            Some(false, r)
                    | None -> 
                        None
            else
                match cache.TryGetValue v with
                    | (true, (r, ref)) -> 
                        ref := !ref - 1
                        if !ref = 0 then
                            cache.Remove v |> ignore
                            Some(true, r)
                        else
                            Some(false, r)
                    | _ -> 
                        None

        /// Revoke the value and return its associated cache value.
        member x.Revoke (v : 'T1) =
            x.RevokeAndGetDeleted v |> snd

        /// Enumerate over all cache values.
        member x.Values = 
            cache.Values |> Seq.map fst



    type FsWatcher private(path : string, watcher : FileSystemWatcher, refCount : ref<int>) =
        static let watchers = System.Collections.Generic.Dictionary<string, WeakReference<FileSystemWatcher> * ref<int>>()
     
        static let destroy = new System.Collections.Concurrent.BlockingCollection<ref<list<IDisposable>>>()

        static let (|Strong|_|) (w : WeakReference<'a>) =
            match w.TryGetTarget() with
            | (true, t) -> Some t
            | _ -> None

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
            let path = normalizePath path

            let watcher, refCount =
                lock watchers (fun () ->
                    match watchers.TryGetValue path with
                    | (true, (Strong w,r)) -> 
                        r := !r + 1
                        w, r
                    | _ ->
                        let w = new FileSystemWatcher(path)
                        dprintfn "create watcher %s" path
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
                                dprintfn "dispose watcher %s" path
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

        member x.WhenDisposed(callback : unit -> unit) =
            disposables <- { new IDisposable with member x.Dispose() = callback() } :: disposables

[<AutoOpen>]
module private DirectoryImplementation = 
    module private FileSytemInfo =
        let tryGet (path : string) =
            if File.Exists path then FileInfo path :> FileSystemInfo |> Some
            elif Directory.Exists path then DirectoryInfo path :> FileSystemInfo |> Some
            else None

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

[<AbstractClass; Sealed>]
type AdaptiveDirectory private() =
    static member GetFileSystemEntries(path : string, ?regex : System.Text.RegularExpressions.Regex) =
        let path = normalizePath path

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
        
    static member GetFiles(path : string, ?regex : System.Text.RegularExpressions.Regex, ?recursive : bool) =
        let path = normalizePath path
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
     
    static member GetFiles(path : string, ?pattern : string, ?recursive : bool) =
        let rx = pattern |> Option.map (System.Text.RegularExpressions.Regex)
        AdaptiveDirectory.GetFiles(path, ?regex = rx, ?recursive = recursive)

    static member GetFiles(path : string, recursive : bool) =
        AdaptiveDirectory.GetFiles(path, ?regex = None, recursive = recursive)
        
    static member GetFiles(path : string) =
        AdaptiveDirectory.GetFiles(path, ?regex = None, ?recursive = None)
    

module Directory =



    let getFiles (dir : string) =
        let dir = normalizePath dir
        ASet.ofReader (fun () ->
            DirectoryWatchReader<FileInfo>(dir, function
                | :? FileInfo as f -> ValueSome f
                | _ -> ValueNone
            )
        )
        
    let getDirectories (dir : string) =
        let dir = normalizePath dir
        ASet.ofReader (fun () ->
            DirectoryWatchReader<DirectoryInfo>(dir, function
                | :? DirectoryInfo as f -> ValueSome f
                | _ -> ValueNone
            )
        )
        
    let getFileSystemEntries (dir : string) =
        let dir = normalizePath dir
        ASet.ofReader (fun () ->
            DirectoryWatchReader<FileSystemInfo>(dir, ValueSome)
        )


module File =
    [<AutoOpen>]
    module private Implementation = 
        let rec private readText (cnt : int) (file : string) =
            if cnt <= 0 then
                File.ReadAllText file
            else
                try File.ReadAllText file
                with _ -> Thread.Sleep 16; readText (cnt - 1) file
            
        let rec private readBytes (cnt : int) (file : string) =
            if cnt <= 0 then
                File.ReadAllBytes file
            else
                try File.ReadAllBytes file
                with _ -> Thread.Sleep 16; readBytes (cnt - 1) file
                
        let rec private readLines (cnt : int) (file : string) =
            if cnt <= 0 then
                File.ReadAllLines file
            else
                try File.ReadAllLines file
                with _ -> Thread.Sleep 16; readLines (cnt - 1) file

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
                if File.Exists file then readText 5 file |> Some
                else None

        type TryReadAllBytesVal(file : string) =
            inherit FileWatchVal<option<byte[]>>(file)

            override x.Read() =
                if File.Exists file then readBytes 5 file |> Some
                else None

        type TryReadAllLinesVal(file : string) =
            inherit FileWatchVal<option<string[]>>(file)

            override x.Read() =
                if File.Exists file then readLines 5 file |> Some
                else None

        type ReadAllTextVal(file : string) =
            inherit FileWatchVal<string>(file)
            override x.Read() = readText 5 file

        type ReadAllBytesVal(file : string) =
            inherit FileWatchVal<byte[]>(file)
            override x.Read() = readBytes 5 file
            
        type ReadAllLinesVal(file : string) =
            inherit FileWatchVal<string[]>(file)
            override x.Read() = 
                if File.Exists file then
                    try readLines 5 file
                    with _ -> [||]
                else 
                    [||]
        
    let tryReadAllText (file : string) =
        let file = normalizePath file
        TryReadAllTextVal(file) :> aval<_>

    let tryReadAllBytes (file : string) =
        let file = normalizePath file
        TryReadAllBytesVal(file) :> aval<_>
        
    let tryReadAllLines(file : string) =
        let file = normalizePath file
        TryReadAllLinesVal(file) :> aval<_>
        
    let readAllText (file : string) =
        let file = normalizePath file
        ReadAllTextVal(file) :> aval<_>

    let readAllBytes (file : string) =
        let file = normalizePath file
        ReadAllBytesVal(file) :> aval<_>
        
    let readAllLines(file : string) =
        let file = normalizePath file
        ReadAllLinesVal(file) :> aval<_>
        
    let readAllLinesAList(file : string) =
        let file = normalizePath file
        ReadAllLinesVal(file) :> aval<_> |> AList.ofAVal
        