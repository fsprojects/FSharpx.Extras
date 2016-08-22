#I @"./packages/FAKE/tools"
#r @"./packages/FAKE/tools/FakeLib.dll"

open Fake 
open Fake.Git
open Fake.ReleaseNotesHelper
open System.IO
open Fake.Testing


let net45 = "v4.5"

// directories
let buildDir = "./bin"
let buildDirVer fxVersion = buildDir @@ fxVersion
let packagesDir = "./packages"

let projectDesc = "FSharpx.Extras implements general functional constructs on top of the F# core library. Its main target is F# but it aims to be compatible with all .NET languages wherever possible."

let rec getPackageDesc = function
| "FSharpx.Text.StructuredFormat" -> projectDesc + "\r\n\r\nThis library provides data structures and functins for pretty printers."
| _ -> projectDesc + "\r\n\r\nIt currently implements:\r\n\r\n" + 
                       "* Several standard monads: State, Reader, Writer, Either, Continuation, Distribution\r\n" +
                       "* Validation applicative functor\r\n" + 
                       "* General functions like flip\r\n" +
                       "* Additional functions around collections\r\n" + 
                       "* Functions to make C# - F# interop easier."

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted 
let gitHome = "https://github.com/fsprojects"

// The name of the project on GitHub
let gitName = "FSharpx.Extras"

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = parseReleaseNotes (File.ReadAllLines "RELEASE_NOTES.md")

let fxVersions = [net45]

let normalizeFrameworkVersion fxVersion =
    let v = ("[^\\d]" >=> "") fxVersion
    v.Substring(0,2)

let buildLibParams fxVersion = 
    ["TargetFrameworkVersion", fxVersion
     "DefineConstants", "NET" + normalizeFrameworkVersion fxVersion
     "TargetFSharpCoreVersion", "4.3.0.0"
     "DefineConstants", "FX_NO_WINDOWSFORMS"
     ]
     
// targets
Target "Clean" (fun _ ->
    for fxVersion in fxVersions do
        !! "*.sln"
        |> MSBuild (buildDirVer fxVersion) "Clean"(["Configuration","Release"] @ buildLibParams fxVersion)
        |> ignore
    CleanDirs [buildDir; packagesDir; "docs/output"]
)


Target "AssemblyInfo" (fun _ ->
    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = release.AssemblyVersion
            AssemblyTitle = "FSharpx.Extras"
            AssemblyDescription = getPackageDesc "FSharpx.Extras"
            Guid = "1e95a279-c2a9-498b-bc72-6e7a0d6854ce"
            OutputFileName = "./src/FSharpx.Extras/AssemblyInfo.fs" })


    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = release.AssemblyVersion
            AssemblyTitle = "FSharpx.Text.StructuredFormat"
            AssemblyDescription = getPackageDesc "FSharpx.Text.StructuredFormat"
            Guid = "65e077ed-f51a-42d7-8004-e90d60af8b8f"
            OutputFileName = "./src/FSharpx.Text.StructuredFormat/AssemblyInfo.fs" })
)


Target "Build" (fun _ ->
    for fxVersion in fxVersions do
        // Only generate tests for net40
        !! "*.sln"
        |> MSBuild (buildDirVer fxVersion) "Build" (["Configuration","Release"] @ buildLibParams fxVersion)
        |> ignore)

Target "Test" (fun _ ->
    ActivateFinalTarget "CloseTestRunner"
    for fxVersion in [net45] do
      printfn "buildDirVer fxVersion = %s" (buildDirVer fxVersion)
      !! (buildDirVer fxVersion @@ "*.Tests.dll")
      |> NUnit3 id)


Target "PaketPack" (fun _ ->
    Paket.Pack (fun p ->
      { p with
          OutputPath = "bin"
          Version = release.AssemblyVersion
          ReleaseNotes = toLines release.Notes
      })
)

Target "PaketPush" (fun _ ->
    Paket.Push id
)

// --------------------------------------------------------------------------------------
// Generate the documentation

Target "GenerateDocs" (fun _ ->
    executeFSIWithArgs "docs/tools" "generate.fsx" ["--define:RELEASE"] [] |> ignore
)

// --------------------------------------------------------------------------------------
// Release Scripts

Target "ReleaseDocs" (fun _ ->
    let tempDocsDir = "bin/gh-pages"
    if not (Directory.Exists tempDocsDir) then 
        Repository.cloneSingleBranch "" (gitHome + "/" + gitName + ".git") "gh-pages" tempDocsDir

    fullclean tempDocsDir
    CopyRecursive "docs/output" tempDocsDir true |> tracefn "%A"
    StageAll tempDocsDir
    Commit tempDocsDir (sprintf "Update generated documentation for version %s" release.NugetVersion)
    Branches.push tempDocsDir
)


FinalTarget "CloseTestRunner" (fun _ ->  
    ProcessHelper.killProcess "nunit-agent.exe"
)

Target "Release" DoNothing

Target "CI" DoNothing

// Build order
"AssemblyInfo"
  ==> "Build" 
  ==> "Test" 

"Build"
  ==> "PaketPack"

"Test" 
  ==> "Release"

"Build" 
  ==> "GenerateDocs"
  ==> "ReleaseDocs"
  ==> "PaketPack"
  ==> "PaketPush"
  ==> "Release"

"Test"
  ==> "GenerateDocs"
  ==> "PaketPack"
  ==> "CI"

let target = getBuildParamOrDefault "target" "Test"

// Start build
Run target
