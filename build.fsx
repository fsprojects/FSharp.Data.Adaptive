#r "paket: groupref Build //"
#load ".fake/build.fsx/intellisense.fsx"
#load @"paket-files/build/aardvark-platform/aardvark.fake/DefaultSetup.fsx"

open System
open System.IO
open System.Diagnostics
open Aardvark.Fake
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet

do Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

DefaultSetup.install ["FsIncremental.sln"]


Target.create "Test" (fun _ ->
    let options (o : DotNet.TestOptions) =
        
        { (o.WithRedirectOutput false) with
            //Common = { o.Common with Verbosity = Some DotNet.Verbosity.Quiet }
            //ListTests = true
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
"Compile" ==> "Run"

entry()