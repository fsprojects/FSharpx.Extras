#I @"./lib/FAKE/tools"
#r @"./lib/FAKE/tools/FakeLib.dll"

open Fake 
open Fake.Git
open Fake.ReleaseNotesHelper
open System.IO

let nugetPath = ".nuget/NuGet.exe"
let RestorePackage() =
    !! "./**/packages.config"
    |> Seq.iter (RestorePackage (fun p -> { p with ToolPath = nugetPath }))

RestorePackage()

// properties
let currentDate = System.DateTime.UtcNow
let projectName = "FSharpx"

let coreSummary = "FSharpx is a library for the .NET platform implementing general functional constructs on top of the F# core library."
let projectSummary = "FSharpx is a library for the .NET platform implementing general functional constructs on top of the F# core library."
let authors = ["Steffen Forkmann"; "Daniel Mohl"; "Tomas Petricek"; "Ryan Riley"; "Mauricio Scheffer"; "Phil Trelford" ]
let mail = "ryan.riley@panesofglass.org"
let homepage = "http://github.com/fsprojects/fsharpx"

// .NET Frameworks
let net35 = "v3.5"
let net40 = "v4.0"

// directories
let buildDir = "./bin/"
let packagesDir = "./packages/"
let testDir = "./test/"

let targetPlatformDir = getTargetPlatformDir "v4.0.30319"

let nugetDir package = sprintf "./nuget/%s/" package
let nugetLibDir package = nugetDir package @@ "lib"

let packages = ["Core"; "Http"; "Observable"; "Text.StructuredFormat"] 

let projectDesc = "FSharpx is a library for the .NET platform implementing general functional constructs on top of the F# core library. Its main target is F# but it aims to be compatible with all .NET languages wherever possible."

let rec getPackageDesc = function
| "Http" -> projectDesc + "\r\n\r\nThis library provides common features for working with HTTP applications."
| "Observable" -> projectDesc + "\r\n\r\nThis library implements a mini-Reactive Extensions (MiniRx) and was authored by Phil Trelford."
| "Text.StructuredFormat" -> projectDesc + "\r\n\r\nThis library provides data structures and functoins for pretty printers."
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
let gitName = "fsharpx"

// The url for the raw files hosted
let gitRaw = environVarOrDefault "gitRaw" "https://raw.github.com/fsprojects"

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = parseReleaseNotes (File.ReadAllLines "RELEASE_NOTES.md")

let fxVersions = [net35; net40]

let normalizeFrameworkVersion fxVersion =
    let v = ("[^\\d]" >=> "") fxVersion
    v.Substring(0,2)

let buildLibParams fxVersion = 
    ["TargetFrameworkVersion", fxVersion
     "DefineConstants", "NET" + normalizeFrameworkVersion fxVersion
     "TargetFSharpCoreVersion", (if fxVersion = net35 then "2.3.0.0" else "4.3.0.0") ]

// tools
let nunitVersion = GetPackageVersion packagesDir "NUnit.Runners"
let nunitPath = sprintf "%sNUnit.Runners.%s/Tools" packagesDir nunitVersion


// targets
Target "Clean" (fun _ ->       
    CleanDirs [buildDir; testDir]

    packages
    |> Seq.iter (fun x -> CleanDirs [nugetDir x; nugetLibDir x;])
)


Target "AssemblyInfo" (fun _ ->
    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = release.AssemblyVersion
            AssemblyTitle = projectName
            AssemblyDescription = getPackageDesc "Core"
            Guid = "1e95a279-c2a9-498b-bc72-6e7a0d6854ce"
            OutputFileName = "./src/FSharpx.Core/AssemblyInfo.fs" })

    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = release.AssemblyVersion
            AssemblyTitle = "FSharpx.Http"
            AssemblyDescription = getPackageDesc "Http"
            Guid = "60F3BB81-5449-45DD-A217-B6045327680C"
            OutputFileName = "./src/FSharpx.Http/AssemblyInfo.fs" })

    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = release.AssemblyVersion
            AssemblyTitle = "FSharpx.Observable"
            AssemblyDescription = getPackageDesc "Observable"
            Guid = "2E802F54-9CD0-4B0A-B834-5C5979403B50"
            OutputFileName = "./src/FSharpx.Observable/AssemblyInfo.fs" })

    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = release.AssemblyVersion
            AssemblyTitle = "FSharpx.Text.StructuredFormat"
            AssemblyDescription = getPackageDesc "Text.StructuredFormat"
            Guid = "65e077ed-f51a-42d7-8004-e90d60af8b8f"
            OutputFileName = "./src/FSharpx.Text.StructuredFormat/AssemblyInfo.fs" })
            

)


let testTarget = TargetTemplate (fun fxVersion ->
    ActivateFinalTarget "CloseTestRunner"
    !! (testDir + "/*.Tests.dll")
    |> NUnit (fun p ->
        {p with
            ToolPath = nunitPath
            DisableShadowCopy = true
            OutputFile = testDir + sprintf "TestResults.%s.xml" fxVersion })
)


Target "Build" (fun _ ->
    for fxVersion in fxVersions do
        (!! "./src/**/*.*proj")  
        |> MSBuild buildDir "Rebuild" (["Configuration","Release"] @ buildLibParams fxVersion)
        |> ignore)

Target "Test" (fun _ ->
    for fxVersion in fxVersions do
        (!! "./tests/**/*.*proj") 
        |> MSBuild testDir "Rebuild" ["Configuration","Release"]
        |> ignore)

Target "PrepareNuGet" (fun _ ->
    for fxVersion in fxVersions do
      for package in packages do
        let frameworkSubDir = nugetLibDir package @@ normalizeFrameworkVersion fxVersion
        CleanDir frameworkSubDir

        [for ending in ["dll";"pdb";"xml"] do
            yield sprintf "%sFSharpx.%s.%s" buildDir package ending]
        |> Seq.filter File.Exists
        |> CopyTo frameworkSubDir)


Target "NuGet" (fun _ ->

  for package in packages do 
    tracefn "Generating nuget target for package %s" package
    [ "LICENSE.md" ] |> CopyTo (nugetDir package)
    NuGet (fun p -> 
        {p with               
            Authors = authors
            Project = projectName + "." + package
            WorkingDir = nugetDir package
            Description = getPackageDesc package
            Version = release.AssemblyVersion
            OutputPath = nugetDir package
            AccessKey = getBuildParamOrDefault "nugetkey" ""
            Publish = hasBuildParam "nugetkey"
            ToolPath = nugetPath
            Dependencies =
                if package = "Core" then p.Dependencies 
                else
                  [projectName + ".Core", RequireExactly (NormalizeVersion release.AssemblyVersion)] })
        "FSharpx.Core.nuspec")
            


// --------------------------------------------------------------------------------------
// Generate the documentation

Target "GenerateDocs" (fun _ ->
    executeFSIWithArgs "docs/tools" "generate.fsx" ["--define:RELEASE"] [] |> ignore
)

// --------------------------------------------------------------------------------------
// Release Scripts

Target "ReleaseDocs" (fun _ ->
    let tempDocsDir = "temp/gh-pages"
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

// Build order
"Clean"
  ==> "AssemblyInfo"
  ==> "Build" 
  ==> "Test" 

"Build"
  ==> "PrepareNuGet"
  ==> "NuGet"

"Test"
  ==> "GenerateDocs"
  ==> "ReleaseDocs"
  ==> "Release"

"NuGet"
  ==> "Release"

let target = getBuildParamOrDefault "target" "Test"

// Start build
Run target
