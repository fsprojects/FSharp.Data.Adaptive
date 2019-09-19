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
    DotNet.build options "FSharp.Control.Incremental.sln"
)

Target.create "Pack" (fun _ ->
    
    Paket.pack (fun o ->
        { o with
            
            WorkingDir = Environment.CurrentDirectory
            OutputPath = "bin"
            PinProjectReferences = true
            ProjectUrl = "https://github.com/krauthaufen/FSharp.Control.Incremental"
            Version = notes.NugetVersion
            ReleaseNotes = String.concat "\n" notes.Notes
        }
    )
)

Target.create "Push" (fun _ ->
    let packageNameRx = Regex @"^(?<name>[a-zA-Z_0-9\.-]+?)\.(?<version>([0-9]+\.)*[0-9]+.*?)\.nupkg$"
    
    if not (Git.Information.isCleanWorkingCopy ".") then
        Git.Information.showStatus "."
        failwith "repo not clean"

    
    if File.exists "deploy.targets" then
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
            File.ReadAllLines "deploy.targets"
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
            
        
        Git.CommandHelper.directRunGitCommandAndFail "." "fetch --tags"
        Git.Branches.tag "." notes.NugetVersion

        let branch = Git.Information.getBranchName "."
        Git.Branches.pushBranch "." "origin" branch

        if List.isEmpty packages then
            failwith "no packages produced"

        if Map.isEmpty targetsAndKeys then
            failwith "no deploy targets"
            
        for (dst, key) in Map.toSeq targetsAndKeys do
            Trace.tracefn "pushing to %s" dst
            let options (o : Paket.PaketPushParams) =
                { o with 
                    PublishUrl = dst
                    ApiKey = key 
                    WorkingDir = "bin"
                }

            Paket.pushFiles options packages

        Git.Branches.pushTag "." "origin" notes.NugetVersion
    ()
)


Target.create "Test" (fun _ ->
    let options (o : DotNet.TestOptions) =
        { (o.WithRedirectOutput false) with
            Configuration = DotNet.BuildConfiguration.Release
            Logger = Some "console;verbosity=normal"
        }
    DotNet.test options "FSharp.Control.Incremental.sln"
)

Target.create "Default" ignore


"Compile" ==> 
    "Test" ==> 
    "Pack" ==>
    "Push"

"Compile" ==> 
    "Test" ==>
    "Default"


Target.runOrDefault "Default"


