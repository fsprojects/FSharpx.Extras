#r "paket: groupref fakebuild //"
#load "./.fake/build.fsx/intellisense.fsx"
#if !FAKE
#r "Facades/netstandard"
#r "netstandard"
#endif

open Fake.Api
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.Tools
open System
open System.IO
open System.Xml.Linq

// --------------------------------------------------------------------------------------
// START TODO: Provide project-specific details below
// --------------------------------------------------------------------------------------

// Information about the project are used
//  - for version and project name in generated AssemblyInfo file
//  - by the generated NuGet package
//  - to run tests and to publish documentation on GitHub gh-pages
//  - for documentation, you also need to edit info in "docs/tools/generate.fsx"

// The name of the project
// (used by attributes in AssemblyInfo, name of a NuGet package and directory in 'src')
let project = "FSharpx.Extras"

// Short summary of the project
// (used as description in AssemblyInfo and as a short summary for NuGet package)
let summary = "FSharpx.Extras implements general functional constructs on top of the F# core library. Its main target is F# but it aims to be compatible with all .NET languages wherever possible."

// Longer description of the project
// (used as a description for NuGet package; line breaks are automatically cleaned up)
let description = "FSharpx.Extras implements general functional constructs on top of the F# core library. Its main target is F# but it aims to be compatible with all .NET languages wherever possible."

// List of author names (for NuGet package)
let authors = [ "Steffen Forkmann and others" ]

// Tags for your project (for NuGet package)
let tags = "fsharpx fsharp"

// File system information
let solutionFile  = "FSharpx.Extras.sln"
let srcProjects = "src/**/*.??proj"

// Pattern specifying assemblies to be tested using NUnit
let testProjects = "tests/**/*.??proj"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted
let gitOwner = "fsprojects"
let gitHome = sprintf "%s/%s" "https://github.com" gitOwner

// The name of the project on GitHub
let gitName = "FSharpx.Extras"

// The url for the raw files hosted
let gitRaw = Environment.environVarOrDefault "gitRaw" "https://raw.githubusercontent.com/fsprojects"

// --------------------------------------------------------------------------------------
// END TODO: The rest of the file includes standard build steps
// --------------------------------------------------------------------------------------

// Read additional information from the release notes document
let release = ReleaseNotes.load "RELEASE_NOTES.md"

// Copies binaries from default VS location to expected bin folder
// But keeps a subdirectory structure for each project in the
// src folder to support multiple project outputs
Target.create "CopyBinaries" (fun _ ->
    !! "src/**/*.??proj"
    -- "src/**/*.shproj"
    |>  Seq.map (fun f -> IO.Path.Combine(IO.Path.GetDirectoryName f, "bin", "Release"), IO.Path.Combine("bin", IO.Path.GetFileNameWithoutExtension f))
    |>  Seq.iter (fun (fromDir, toDir) -> Shell.copyDir toDir fromDir (fun _ -> true))
)

// Helper active pattern for project types
// --------------------------------------------------------------------------------------
// Clean build results

Target.create "Clean" (fun _ ->
    DotNet.exec id "clean" "" |> ignore
    Shell.cleanDirs ["bin"; "temp"; "docs/output"]
)

// --------------------------------------------------------------------------------------
// Build library & test project

Target.create "Build" (fun _ ->
    !! srcProjects
    |> Seq.iter (DotNet.build (fun c -> { c with Configuration=DotNet.BuildConfiguration.Release }))
)

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner

Target.create "RunTests" (fun _ ->
    !! testProjects
    |> Seq.iter (DotNet.test (fun c -> { c with Configuration=DotNet.BuildConfiguration.Release }))
)

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target.create "NuGet" (fun _ ->
    !! srcProjects
    |> Seq.iter (DotNet.pack(fun p ->
        { p with OutputPath=Some(IO.Path.Combine(__SOURCE_DIRECTORY__, "bin")) }))
)

Target.create "PublishNuget" (fun _ ->
    Paket.push(fun p -> 
        { p with WorkingDir=IO.Path.Combine(__SOURCE_DIRECTORY__, "bin") })
)


// --------------------------------------------------------------------------------------
// Release Scripts

// Directory.Build.props helpers

let getVersion (versionFile:string) =
    let doc = XElement.Load versionFile
    let version =
        doc.Elements().Elements()
        |> Seq.filter (fun e -> e.Name.LocalName = "VersionPrefix")
        |> Seq.head
    version.Value

let setVersion (versionFile:string) newVersion =
    let doc = XElement.Load versionFile
    let version =
        doc.Elements().Elements()
        |> Seq.filter (fun e -> e.Name.LocalName = "VersionPrefix")
        |> Seq.head
    version.Value <- newVersion
    doc.Save versionFile

let nextTag (version:string) =
    let lastDot = version.LastIndexOf(".")
    let patchNum = version.Substring(lastDot + 1)
    match System.Int32.TryParse(patchNum) with
    | false, _ ->
        invalidArg version (sprintf "version %s is invalid" version)
    | true, num ->
        sprintf "%s.%d"  (version.Substring(0,lastDot)) (num + 1)

Target.create "Release" (fun _ ->
    let user =
        match Environment.environVarOrDefault "github-user" "" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> UserInput.getUserInput "Username: "
    let pw =
        match Environment.environVarOrDefault "github-pw" "" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> UserInput.getUserPassword "Password: "
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
        |> Seq.tryFind (fun (s: string) -> s.Contains(gitOwner + "/" + gitName))
        |> function None -> gitHome + "/" + gitName | Some (s: string) -> s.Split().[0]

    Git.Staging.stageAll ""
    Git.Commit.exec "" (sprintf "Bump version to %s" release.NugetVersion)
    Git.Branches.pushBranch "" remote (Git.Information.getBranchName "")

    Git.Branches.tag "" release.NugetVersion
    Git.Branches.pushTag "" remote release.NugetVersion

    let versionFile = Path.Combine(__SOURCE_DIRECTORY__, "src", "Directory.Build.props")
    setVersion versionFile (nextTag release.NugetVersion)

    // release on github
    GitHub.createClient user pw
    |> GitHub.draftNewRelease gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes
    // TODO: |> uploadFile "PATH_TO_FILE"
    |> GitHub.publishDraft
    |> Async.RunSynchronously
)

Target.create "BuildPackage" ignore

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target.create "All" ignore
Target.create "LinuxCI" ignore

"Build"
  ==> "CopyBinaries"
  ==> "RunTests"
  ==> "LinuxCI"
  ==> "NuGet"
  ==> "BuildPackage"
  ==> "All"

"Clean"
  ==> "Release"

"BuildPackage"
  ==> "PublishNuget"
  ==> "Release"

Target.runOrDefault "All"
