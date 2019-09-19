#r "paket: groupref Build //"
#load ".fake/build.fsx/intellisense.fsx"

open System
open System.IO
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet

do Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let notes = ReleaseNotes.load "RELEASE_NOTES.md"

Target.create "Clean" (fun _ ->
    DotNet.exec id "clean" "" |> ignore
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
                            "AssemblyVersion", v
                        ] 
                }
        }
    DotNet.build options "FsIncremental.sln"
)



Target.create "Pack" (fun _ ->
    
    Paket.pack (fun o ->
        { o with
            
            WorkingDir = Environment.CurrentDirectory
            OutputPath = "bin"
            PinProjectReferences = true
            ProjectUrl = "https://github.com/krauthaufen/FsIncremental"
            Version = notes.NugetVersion
            ReleaseNotes = String.concat "\n" notes.Notes
        }
    )
)

Target.create "Test" (fun _ ->
    let options (o : DotNet.TestOptions) =
        { (o.WithRedirectOutput false) with
            Configuration = DotNet.BuildConfiguration.Release
            Logger = Some "console;verbosity=normal"
        }
    DotNet.test options "FsIncremental.sln"
)

Target.create "Start" (fun _ ->
    let param (p : DotNet.Options) =
        { p with WorkingDirectory = Path.Combine("bin", "Release", "netcoreapp2.0") }

    DotNet.exec param "" "FsIncremental.dll" |> ignore
)

Target.create "Run" (fun _ -> Target.run 1 "Start" [])

Target.create "Default" ignore


"Compile" ==> "Test" ==> "Pack"
"Compile" ==> "Run"
"Compile" ==> 
    "Test" ==>
    "Default"


Target.runOrDefault "Default"


