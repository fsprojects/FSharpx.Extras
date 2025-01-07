﻿open Fake.Api
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.Tools
open System
open System.IO
open System.Xml.Linq
open Fake.BuildServer


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

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted
let gitOwner = "fsprojects"
let gitHome = sprintf "%s/%s" "https://github.com" gitOwner

// The name of the project on GitHub
let gitName = "FSharpx.Extras"

// The url for the raw files hosted
let gitRaw = Environment.environVarOrDefault "gitRaw" "https://raw.githubusercontent.com/fsprojects"

let outputDir = __SOURCE_DIRECTORY__ @@ "../bin"

// --------------------------------------------------------------------------------------
// END TODO: The rest of the file includes standard build steps
// --------------------------------------------------------------------------------------

// Read additional information from the release notes document
let release = ReleaseNotes.load "RELEASE_NOTES.md"
let maybeBuildNumber = Environment.environVarOrNone "GITHUB_RUN_ID"

let initTargets () =
    BuildServer.install [
        GitHubActions.Installer
    ]
    Target.create "SetCIVersion" (fun _ ->
        let version =
            let postfix =
                maybeBuildNumber
                |> Option.map ((+) ".")
                |> Option.defaultValue ""
            release.AssemblyVersion + postfix
        Trace.setBuildNumber version
    )

// --------------------------------------------------------------------------------------
// Clean build results

    Target.create "Clean" (fun _ ->
        DotNet.exec id "clean" "" |> ignore
        Shell.cleanDirs ["bin"; "temp"; "output"; "src/**/bin";"test/**/bin";"src/**/obj";"test/**/obj"]
    )

// --------------------------------------------------------------------------------------
// Build library & test project

    Target.create "Build" (fun _ ->
        solutionFile
        |> DotNet.build (fun c -> {
                c with
                    Configuration=DotNet.BuildConfiguration.Release
                    MSBuildParams= {c.MSBuildParams with Properties = c.MSBuildParams.Properties @ [("CopyLocalLockFileAssemblies","true")]}
            }) 
    )

    Target.create "Publish" (fun _ ->
        [
            "src/FSharpx.Extras/FSharpx.Extras.fsproj"
            "src/FSharpx.Text.StructuredFormat/FSharpx.Text.StructuredFormat.fsproj"
        ]
        |> Seq.iter (DotNet.publish (fun p ->
            { p with
                OutputPath=Some outputDir
                Framework=Some"netstandard2.0"

            }))
    )

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner

    Target.create "RunTests" (fun _ ->
        solutionFile
        |> DotNet.test (fun c -> { 
            c with
                Configuration=DotNet.BuildConfiguration.Release
                Logger = if BuildServer.buildServer = GitHubActions then Some "GitHubActions" else None
            })
    )

// --------------------------------------------------------------------------------------
// Build a NuGet package

    Target.create "NuGet" (fun _ ->
        solutionFile
        |> DotNet.pack (fun p ->
            { p with OutputPath=Some outputDir })
    )

    Target.create "PublishNuget" (fun _ ->
        Paket.push(fun p -> 
            { p with WorkingDir= outputDir })
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

    Target.create "GenerateDocs" (fun _ ->
        Shell.cleanDir ".fsdocs"
        DotNet.exec id "fsdocs" "build --clean --properties Configuration=Release TargetFramework=netstandard2.0" |> ignore
    )

    Target.create "WatchDocs" (fun _ ->
        DotNet.exec id "fsdocs" "watch --clean --properties Configuration=Release TargetFramework=netstandard2.0" |> ignore
    )

    Target.create "ReleaseDocs" (fun _ ->
        let tempDocsDir = "temp/gh-pages"
        Shell.cleanDir tempDocsDir
        Git.Repository.cloneSingleBranch "" (gitHome + "/" + gitName + ".git") "gh-pages" tempDocsDir
        
        Shell.copyRecursive "output" tempDocsDir true |> Trace.tracefn "%A"
        Git.Staging.stageAll tempDocsDir
        Git.Commit.exec tempDocsDir (sprintf "Update generated documentation for version %s" release.NugetVersion)
        Git.Branches.push tempDocsDir
    )

    Target.create "BuildPackage" ignore

    // --------------------------------------------------------------------------------------
    // Run all targets by default. Invoke 'build <Target>' to override

    Target.create "All" ignore

    "SetCIVersion"
    ==> "Build"
    ==> "RunTests"
    ==> "NuGet"
    ==> "BuildPackage"
    ==> "All"
    |> ignore

    "Clean"
    ==> "Release"
    |> ignore
    
    "Clean"
    ?=> "Build"
    |> ignore

    "Clean"
    ?=> "Publish"
    |> ignore

    "Clean"
    ?=> "GenerateDocs"
    |> ignore

    "BuildPackage"
    ==> "PublishNuget"
    ==> "Release"
    |> ignore

    "GenerateDocs"
    ==> "ReleaseDocs"
    ==> "Release"
    |> ignore

    // fsdocs requires build for api references and publish for examples compilation
    "Publish"
    ==> "WatchDocs"
    |> ignore

    "Build"
    ==> "WatchDocs"
    |> ignore

    "Build"
    ==> "GenerateDocs"
    |> ignore

    "Publish"
    ==> "GenerateDocs"
    ==> "All"
    |> ignore

//-----------------------------------------------------------------------------
// Target Start
//-----------------------------------------------------------------------------
[<EntryPoint>]
let main argv =
    argv
    |> Array.toList
    |> Context.FakeExecutionContext.Create false "build.fsx"
    |> Context.RuntimeContext.Fake
    |> Context.setExecutionContext

    initTargets ()
    Target.runOrDefaultWithArguments ("All")

    0 // return an integer exit code