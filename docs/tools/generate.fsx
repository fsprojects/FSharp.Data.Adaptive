// --------------------------------------------------------------------------------------
// Builds the documentation from `.fsx` and `.md` files in the 'docs/content' directory
// (the generated documentation is stored in the 'docs/output' directory)
// --------------------------------------------------------------------------------------

// Binaries that have XML documentation (in a corresponding generated XML file)
let referenceProjects = [ "../../src/FSharp.Data.Adaptive" ]

let githubLink = "http://github.com/fsprojects/FSharp.Data.Adaptive"

// Specify more information about your project
let info =
  [ "project-name", "FSharp.Data.Adaptive"
    "project-author", "Georg Haaser, Don Syme"
    "project-summary", "On-demand adaptive/incremental data for F#"
    "project-github", githubLink
    "project-nuget", "http://www.nuget.org/packages/FSharp.Data.Adaptive" ]

// --------------------------------------------------------------------------------------
// For typical project, no changes are needed below
// --------------------------------------------------------------------------------------

#I "../../packages/docs/FSharp.Compiler.Service/lib/net45"
#I "../../packages/docs/FSharp.Formatting/lib/net461"
#r "../../packages/docs/FAKE/tools/FakeLib.dll"
#r "RazorEngine.dll"
#r "FSharp.Markdown.dll"
#r "FSharp.Literate.dll"
#r "FSharp.CodeFormat.dll"
#r "FSharp.MetadataFormat.dll"
#r "FSharp.Formatting.Common.dll"
#r "FSharp.Formatting.Razor.dll"

open Fake
open System.IO
open FSharp.Formatting.Razor

let website = "https://fsprojects.github.io/FSharp.Data.Adaptive"

// When called from 'build.fsx', use the public project URL as <root>
// otherwise, use the current 'output' directory.
#if RELEASE
let root = website
#else
let root = "file://" + (__SOURCE_DIRECTORY__ @@ "../output")
#endif

// Paths with template/source/output locations
let bin        = __SOURCE_DIRECTORY__ @@ "../../bin"
let content    = __SOURCE_DIRECTORY__ @@ "../content"
let output     = __SOURCE_DIRECTORY__ @@ "../output"
let files      = __SOURCE_DIRECTORY__ @@ "../files"
let templates  = __SOURCE_DIRECTORY__ @@ "templates"
let formatting = __SOURCE_DIRECTORY__ @@ "../../packages/docs/FSharp.Formatting/"
let docTemplate = formatting @@ "templates/docpage.cshtml"

// Where to look for *.csproj templates (in this order)
let layoutRoots =
  [ templates; formatting @@ "templates"
    formatting @@ "templates/reference" ]

// Copy static files and CSS + JS from F# Formatting
let copyFiles () =
  ensureDirectory (output @@ "content")
  CopyRecursive (formatting @@ "styles") (output @@ "content") true 
    |> Log "Copying styles and scripts: "
  CopyRecursive files output true |> Log "Copying file: "


let getReferenceAssembliesForProject (proj : string) =
    let projName = Path.GetFileName proj
    !! ( __SOURCE_DIRECTORY__ @@ "../../bin/Release/netstandard2.0/" + projName + ".dll") |> Seq.head

// Build API reference from XML comments
let buildReference () =
  CleanDir (output @@ "reference")
  let binaries = referenceProjects |> List.map getReferenceAssembliesForProject
  printfn "%A" binaries
  RazorMetadataFormat.Generate
    ( binaries, output @@ "reference", layoutRoots, 
      parameters = ("root", root)::info,
      sourceRepo = githubLink @@ "tree/master",
      sourceFolder = __SOURCE_DIRECTORY__ @@ ".." @@ "..",
      publicOnly = true,
      markDownComments = true )

let references =
    // Workaround compiler errors in Razor-ViewEngine
    let d = RazorEngine.Compilation.ReferenceResolver.UseCurrentAssembliesReferenceResolver()
    let loadedList = d.GetReferences () |> Seq.map (fun r -> r.GetFile()) |> Seq.cache
    // We replace the list and add required items manually as mcs doesn't like duplicates...
    Seq.toList loadedList

// Build documentation from `fsx` and `md` files in `docs/content`
let buildDocumentation () =
  let subdirs = Directory.EnumerateDirectories(content, "*", SearchOption.AllDirectories)
  let binaries = referenceProjects |> List.map getReferenceAssembliesForProject

  let binaries=
    List.concat [
        references
        binaries
    ]

  for dir in Seq.append [content] subdirs do
    let sub = if dir.Length > content.Length then dir.Substring(content.Length + 1) else "."
    RazorLiterate.ProcessDirectory
      ( dir, docTemplate, output @@ sub, replacements = ("root", root)::info,
        layoutRoots = layoutRoots, assemblyReferences = binaries, generateAnchors = true, fsiEvaluator = FSharp.Literate.FsiEvaluator() )

// Generate
ensureDirectory output
CleanDir output
copyFiles()
buildDocumentation()
buildReference()