#I @"./packages/FAKE/tools"
#r @"./packages/FAKE/tools/FakeLib.dll"

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
let projectName = "FSharpx.Extras"

let coreSummary = "FSharpx.Extras is a library for the .NET platform implementing general functional constructs on top of the F# core library."
let projectSummary = "FSharpx.Extras is a library for the .NET platform implementing general functional constructs on top of the F# core library."
let authors = ["Steffen Forkmann"; "Daniel Mohl"; "Tomas Petricek"; "Ryan Riley"; "Mauricio Scheffer"; "Phil Trelford" ]
let mail = "ryan.riley@panesofglass.org"
let homepage = "http://github.com/fsprojects/FSharpx.Extras"

// .NET Frameworks
let net35 = "v3.5"
let net40 = "v4.0"

// directories
let buildDir = "./bin"
let buildDirVer fxVersion = buildDir @@ fxVersion
let packagesDir = "./packages"

let targetPlatformDir = getTargetPlatformDir "v4.0.30319"

let nugDir = "./nuget"
let nugetDir package = nugDir @@ package
let nugetLibDir package = nugetDir package @@ "lib"

let packages = ["Extras"; "Text.StructuredFormat"] 

let projectDesc = "FSharpx.Extras implements general functional constructs on top of the F# core library. Its main target is F# but it aims to be compatible with all .NET languages wherever possible."

let rec getPackageDesc = function
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
let gitName = "FSharpx.Extras"

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
let nunitPath = packagesDir  @@ sprintf "NUnit.Runners.%s/Tools" nunitVersion


// targets
Target "Clean" (fun _ ->       
    CleanDirs [buildDir; nugDir]
)


Target "AssemblyInfo" (fun _ ->
    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = release.AssemblyVersion
            AssemblyTitle = projectName
            AssemblyDescription = getPackageDesc "Extras"
            Guid = "1e95a279-c2a9-498b-bc72-6e7a0d6854ce"
            OutputFileName = "./src/FSharpx.Extras/AssemblyInfo.fs" })


    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = release.AssemblyVersion
            AssemblyTitle = "FSharpx.Text.StructuredFormat"
            AssemblyDescription = getPackageDesc "Text.StructuredFormat"
            Guid = "65e077ed-f51a-42d7-8004-e90d60af8b8f"
            OutputFileName = "./src/FSharpx.Text.StructuredFormat/AssemblyInfo.fs" })
            

)



Target "Build" (fun _ ->
    for fxVersion in fxVersions do
        // Only generate tests for net40
        (if fxVersion = net35 then !! "src/**/*.fsproj" else !! "*.sln")  
        |> MSBuild (buildDirVer fxVersion) "Rebuild" (["Configuration","Release"] @ buildLibParams fxVersion)
        |> ignore)

Target "Test" (fun _ ->
    ActivateFinalTarget "CloseTestRunner"
    for fxVersion in [net40] do
      printfn "buildDirVer fxVersion = %s" (buildDirVer fxVersion)
      !! (buildDirVer fxVersion @@ "*.Tests.dll")
      |> NUnit (fun p ->
        {p with
            ToolPath = nunitPath
            DisableShadowCopy = true
            OutputFile = buildDirVer fxVersion @@ sprintf "TestResults.%s.xml" fxVersion }))

Target "PrepareNuGet" (fun _ ->
    for fxVersion in fxVersions do
      for package in packages do
        let frameworkSubDir = nugetLibDir package @@ normalizeFrameworkVersion fxVersion
        CleanDir frameworkSubDir

        [for ending in ["dll";"pdb";"xml"] do
            yield buildDirVer fxVersion @@ sprintf "FSharpx.%s.%s" package ending]
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
                if package = "Extras" then p.Dependencies 
                else
                  ["FSharpx.Extras", RequireExactly (NormalizeVersion release.AssemblyVersion)] })
        "FSharpx.Extras.nuspec")
            


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
  ==> "Release"

"Build" 
  ==> "GenerateDocs"
  ==> "ReleaseDocs"
  ==> "Release"

"NuGet"
  ==> "Release"


let target = getBuildParamOrDefault "target" "Test"

// Start build
Run target
