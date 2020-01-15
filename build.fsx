#r "paket: groupref Build //"
#load ".fake/build.fsx/intellisense.fsx"

open System
open System.IO
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.Tools
open System.Text.RegularExpressions

do Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let notes = ReleaseNotes.load "RELEASE_NOTES.md"

let isWindows =
    Environment.OSVersion.Platform <> PlatformID.Unix && Environment.OSVersion.Platform <> PlatformID.MacOSX



type GitDescription =
    {
        tag             : string
        commitsSince    : int
        dirty           : bool
        objectName      : string
    }

    static member TryParse(description : string) =
        let rx = Regex @"^(.*?)-([0-9]+)-g(.*?)(-dirty)?$"
        let m = rx.Match description
        if m.Success then
            let tag = m.Groups.[1].Value
            let commitsSince = m.Groups.[2].Value |> int
            let objectName = m.Groups.[3].Value
            let dirty = m.Groups.[4].Success
            Some { tag = tag; commitsSince = commitsSince; dirty = dirty; objectName = objectName }
        else
            None

    static member TryGet (repoDir : string) =
        let success, lines, err = Git.CommandHelper.runGitCommand repoDir "describe --tags --long --always --dirty"
        match lines with
        | d :: _ when success ->
            match GitDescription.TryParse d with
            | Some desc -> Ok desc
            | None -> Error err
        | _ ->
            Error err
        
    static member Get (repoDir : string) =
        match GitDescription.TryGet repoDir with
        | Ok desc -> desc
        | Error err ->
            Trace.traceErrorfn "%s" err
            failwithf "git could not get information: %s" err
     
     
type GitRemoteStatus =
    {
        branch    : string
        remote    : string
        local     : string
        mergeBase : string
    }


    member x.remoteAhead = x.mergeBase = x.local
    member x.localAhead = x.mergeBase = x.remote

    static member TryGet (repoDir : string) =
        Git.CommandHelper.directRunGitCommandAndFail repoDir "fetch"
        let branch = Git.Information.getBranchName repoDir

        match Git.CommandHelper.runGitCommand repoDir (sprintf "rev-parse %s" branch) with
        | true, [localName], _ ->
            match Git.CommandHelper.runGitCommand repoDir (sprintf "rev-parse origin/%s" branch) with
            | true, [remoteName], _ ->
                match Git.CommandHelper.runGitCommand repoDir (sprintf "merge-base %s origin/%s" branch branch) with
                | true, [mergeBase], _ ->
                    Ok {
                        branch = branch
                        remote = remoteName
                        local = localName
                        mergeBase = mergeBase
                    }
                | _, _, err ->
                    Error err
            | _, _, _ ->
                Ok {
                    branch = branch
                    remote = "empty"
                    local = localName
                    mergeBase = "empty"
                }
        | _, _, err ->
            Error err

    static member Get(repoDir : string) =
        match GitRemoteStatus.TryGet repoDir with
        | Ok status -> status
        | Error err -> failwithf "git could not get remote status: %s" err





Target.create "Clean" (fun _ ->
    if Directory.Exists "bin/Debug" then
        Trace.trace "deleting bin/Debug"
        Directory.delete "bin/Debug"
    if Directory.Exists "bin/Release" then
        Trace.trace "deleting bin/Release"
        Directory.delete "bin/Release"

    let pkgs = !!"bin/*.nupkg" |> Seq.toList
    if not (List.isEmpty pkgs) then
        Trace.tracefn "deleting packages: %s" (pkgs |> Seq.map Path.GetFileNameWithoutExtension |> String.concat ", ")
        File.deleteAll pkgs


    let dirs = Directory.EnumerateDirectories("src", "obj", SearchOption.AllDirectories) |> Seq.toList

    if not (List.isEmpty dirs) then 
        for d in dirs do    
            let parent = Path.GetDirectoryName d
            let proj = Directory.GetFiles(parent, "*.fsproj") |> Seq.isEmpty |> not
            if proj then
                Trace.tracefn "deleting %s" d
                Directory.delete d
)

Target.create "Compile" (fun _ ->
    let options (o : DotNet.BuildOptions) =
        let v = sprintf "%d.%d.%d.%s" notes.SemVer.Major notes.SemVer.Minor notes.SemVer.Patch (string notes.SemVer.Build)

        { o with 
            Configuration = DotNet.BuildConfiguration.Release
            
            MSBuildParams = 
                { o.MSBuildParams with 
                    Properties = 
                        [
                            "GenerateAssemblyInfo", "true"
                            "AssemblyVersion", v
                            "FileVersion", v
                            "AssemblyFileVersion", v
                            "ProductVersion", v
                            "InformationalVersion", v
                        ] 
                }
        }
    DotNet.build options "FSharp.Data.Adaptive.sln"
)

Target.create "NpmInstall" (fun _ ->
    let modules = "node_modules" |> Path.GetFullPath

    if not (Directory.Exists modules) then
        Trace.trace "running `npm install`"

        let npm =
            if isWindows then CreateProcess.fromRawCommand "cmd" ["/C"; "npm"; "install"; "--dev"]
            else CreateProcess.fromRawCommand "npm" ["install"]

        use s = new MemoryStream()
        npm
        |> CreateProcess.withWorkingDirectory Environment.CurrentDirectory
        |> CreateProcess.withStandardError (StreamSpecification.UseStream(true, s))
        |> CreateProcess.withStandardOutput  (StreamSpecification.UseStream(true, s))
        |> Proc.run
        |> ignore

)

Target.create "CompileFable" (fun _ ->
    let npx = "node_modules/npx/index.js" |> Path.GetFullPath

    CreateProcess.fromRawCommand "node" [npx; "fable-splitter"; "-c"; "splitter-config.js"]
    |> CreateProcess.withWorkingDirectory Environment.CurrentDirectory
    |> CreateProcess.withStandardError StreamSpecification.Inherit
    |> CreateProcess.withStandardOutput StreamSpecification.Inherit
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore
)

Target.create "WatchFable" (fun _ ->
    let wpds = "node_modules/webpack-dev-server/bin/webpack-dev-server.js" |> Path.GetFullPath
    //let proj = "src/FSharp.Data.Adaptive/FSharp.Data.Adaptive.fsproj" |> Path.GetFullPath
    //let old = Environment.CurrentDirectory
    //Environment.CurrentDirectory <- Path.GetDirectoryName proj
    CreateProcess.fromRawCommand "node" [wpds]
    |> CreateProcess.withWorkingDirectory Environment.CurrentDirectory
    |> CreateProcess.withStandardError StreamSpecification.Inherit
    |> CreateProcess.withStandardOutput StreamSpecification.Inherit
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore

)



Target.create "Pack" (fun _ ->
    
    Paket.pack (fun o ->
        { o with
            WorkingDir = Environment.CurrentDirectory
            OutputPath = "bin"
            PinProjectReferences = true
            ProjectUrl = "https://github.com/fsprojects/FSharp.Data.Adaptive"
            Version = notes.NugetVersion
            ReleaseNotes = String.concat "\n" notes.Notes
        }
    )
)

Target.create "CheckPush" (fun _ ->

    let desc = GitDescription.Get "."
    let newTag = notes.NugetVersion

    if desc.dirty then
        Git.Information.showStatus "."
        failwith "repo not clean (please commit your changes)"

    let status = GitRemoteStatus.Get "."

    if status.remoteAhead then
        failwithf "remote branch %s contains new commits (please pull)" status.branch
    elif status.localAhead then
        Trace.traceImportantfn "local branch %s contains new commits. would you like to push it? (y|N)" status.branch
        let answer = Console.ReadLine().Trim().ToLower()
        match answer with
        | "y" -> Git.CommandHelper.directRunGitCommandAndFail "." (sprintf "push origin %s" status.branch)
        | _ -> failwithf "local branch %s contains new commits." status.branch

    if desc.tag = newTag && desc.commitsSince > 0 then
        failwithf "cannot push package version %s since current head is %d commits ahead of the tag" newTag desc.commitsSince 


    
)

Target.create "Push" (fun _ ->
    let packageNameRx = Regex @"^(?<name>[a-zA-Z_0-9\.-]+?)\.(?<version>([0-9]+\.)*[0-9]+.*?)\.nupkg$"
    
    if not (Git.Information.isCleanWorkingCopy ".") then
        Git.Information.showStatus "."
        failwith "repo not clean"

    
    let packages =
        !!"bin/*.nupkg"
        |> Seq.filter (fun path ->
            let name = Path.GetFileName path
            let m = packageNameRx.Match name
            if m.Success then
                m.Groups.["version"].Value = notes.NugetVersion
            else
                false
        )
        |> Seq.toList

    let targetsAndKeys =
        (if File.Exists "deploy.targets" then File.ReadAllLines "deploy.targets" else [||])
        |> Array.map (fun l -> l.Split(' '))
        |> Array.choose (function [|dst; key|] -> Some (dst, key) | _ -> None)
        |> Array.choose (fun (dst, key) ->
            let path = 
                Path.Combine(
                    Environment.GetFolderPath(Environment.SpecialFolder.UserProfile),
                    ".ssh",
                    key
                )
            if File.exists path then
                let key = File.ReadAllText(path).Trim()
                Some (dst, key)
            else
                None
        )
        |> Map.ofArray
            
    if List.isEmpty packages then
        failwith "no packages produced"

    if Map.isEmpty targetsAndKeys then
        failwith "no deploy targets"
            

    Git.CommandHelper.directRunGitCommandAndFail "." "fetch --tags"

    try Git.Branches.tag "." notes.NugetVersion
    with _ -> Trace.tracefn "tag %s already exists" notes.NugetVersion

    let desc = GitDescription.Get "."
    if not desc.dirty && desc.commitsSince = 0 then
        let branch = Git.Information.getBranchName "."

        try Git.Branches.pushBranch "." "origin" branch
        with _ ->
            Trace.traceErrorfn "could not push %s" branch
            reraise()

        for (dst, key) in Map.toSeq targetsAndKeys do
            Trace.tracefn "pushing to %s" dst
            let options (o : Paket.PaketPushParams) =
                { o with 
                    PublishUrl = dst
                    ApiKey = key 
                    WorkingDir = "bin"
                }

            Paket.pushFiles options packages

        try Git.Branches.pushTag "." "origin" notes.NugetVersion
        with _ ->
            Trace.traceErrorfn "could not push tag %s: PLEASE PUSH MANUALLY" notes.NugetVersion
            reraise()
    ()
)

Target.create "RunTest" (fun _ ->
    let options (o : DotNet.TestOptions) =
        { (o.WithRedirectOutput false) with
            NoBuild = true
            NoRestore = true
            Configuration = DotNet.BuildConfiguration.Release
            Logger = Some "console;verbosity=normal"
        }
    DotNet.test options "FSharp.Data.Adaptive.sln"
)

Target.create "Default" ignore

Target.create "Docs" (fun _ ->
    let path = Path.Combine(__SOURCE_DIRECTORY__, "packages/docs/FSharp.Compiler.Tools/tools/fsi.exe")
    let workingDir = "docs/tools"
    let args = "generate.fsx"
    let command, args = 
        if false (* EnvironmentHelper.isMono *) then "mono", sprintf "'%s' %s" path args 
        else path, args

    if Shell.Exec(command, args, workingDir) <> 0 then
        failwith "failed to generate docs"
)
Target.create "GenerateDocs" (fun _ ->
    let path = Path.Combine(__SOURCE_DIRECTORY__, "packages/docs/FSharp.Compiler.Tools/tools/fsi.exe")
    let workingDir = "docs/tools"
    let args = "--define:RELEASE generate.fsx"
    let command, args = 
        if false (* EnvironmentHelper.isMono *) then "mono", sprintf "'%s' %s" path args 
        else path, args

    if Shell.Exec(command, args, workingDir) <> 0 then
        failwith "failed to generate docs"
)


let cleanDir (dir : string) =
    let info = DirectoryInfo(dir)
    if info.Exists then
        let children = info.GetFileSystemInfos()
        for c in children do
            Trace.tracefn "delete %s" c.Name
            match c with
            | :? DirectoryInfo as d -> 
                if d.Name <> ".git" then d.Delete(true)
            | _ -> c.Delete()

let rec copyRecursive (src : string) (dst : string) =
    let info = DirectoryInfo(src)
    if info.Exists && not (info.Name.StartsWith ".") then
        Directory.ensure dst
        let children = info.GetFileSystemInfos()
        for c in children do
            Trace.tracefn "copy %s" c.Name
            match c with
            | :? DirectoryInfo as d -> copyRecursive d.FullName (Path.Combine(dst, d.Name))
            | :? FileInfo as f -> f.CopyTo(Path.Combine(dst, f.Name)) |> ignore
            | _ -> ()

let gitOwner = "fsprojects"
let gitHome = "https://github.com/" + gitOwner
let gitName = "FSharp.Data.Adaptive"

Target.create "ReleaseDocs" (fun _ ->
    let name = Guid.NewGuid() |> string
    let tempDocsDir = "temp/" + name
    let outputDir = "docs/output"

    Git.Repository.cloneSingleBranch "" (gitHome + "/" + gitName + ".git") "gh-pages" tempDocsDir
    cleanDir tempDocsDir
    
    Directory.ensure outputDir
    copyRecursive outputDir tempDocsDir 
    Git.Staging.stageAll tempDocsDir
    let info = Git.Information.getCurrentSHA1 __SOURCE_DIRECTORY__
    Git.Commit.exec tempDocsDir (sprintf "Update generated documentation for version %s/%s" notes.NugetVersion info)
    Git.Branches.push tempDocsDir

    let rec reallyDelete (iter : int) =
        if iter >= 0 then 
            try Directory.Delete(tempDocsDir, true)
            with _ -> reallyDelete (iter - 1)

    reallyDelete 5
)

"NpmInstall" ==> "CompileFable"
"NpmInstall" ==> "WatchFable"

"Compile" ==> 
    "Docs"
    
"Compile" ==> 
    "GenerateDocs" ==> 
    "ReleaseDocs"


"CheckPush" ?=> "Compile"
"Compile" ?=> "RunTest"
"RunTest" ?=> "Pack"

"Compile" ==> "Pack"

"Pack" ==> "Push"
"RunTest" ==> "Push"
"CheckPush" ==> "Push"

"Compile" ==> "Default"
"RunTest" ==> "Default"



Target.create "Sepp" (fun _ ->
    let d = GitDescription.Get "."
    Trace.tracefn "info: %A" d
        
)

Target.runOrDefault "Default"


