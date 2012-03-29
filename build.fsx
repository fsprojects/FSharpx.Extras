#r "./packages/FAKE.1.64.5/tools/FakeLib.dll"

open Fake 
open System.IO

// properties
let currentDate = System.DateTime.UtcNow
let projectName = "FSharpx"
let version = if isLocalBuild then "1.4." + currentDate.ToString("yMMdd") else buildVersion
let coreSummary = "FSharpx is a library for the .NET platform implementing general functional constructs on top of the F# core library."
let projectSummary = "FSharpx is a library for the .NET platform implementing general functional constructs on top of the F# core library."
let projectDescription = "FSharpx is a library for the .NET platform implementing general functional constructs on top of the F# core library. Its main target is F# but it aims to be compatible with all .NET languages wherever possible.\r\n\r\nIt currently implements:\r\n\r\n* Several standard monads: State, Reader, Writer, Either, Continuation, Distribution\r\n* Iteratee\r\n* Validation applicative functor\r\n* General functions like flip\r\n* Additional functions around collections\r\n* Functions to make C# - F# interop easier."
let authors = ["Steffen Forkmann"; "Daniel Mohl"; "Tomas Petricek"; "Ryan Riley"; "Mauricio Scheffer"; "Phil Trelford" ]
let mail = "ryan.riley@panesofglass.org"
let homepage = "http://github.com/fsharp/fsharpx"

// directories
let buildDir = "./build/"
let packagesDir = "./packages/"
let testDir = "./test/"
let deployDir = "./deploy/"
let docsDir = "./docs/"
let nugetDir = "./nuget/"
let targetPlatformDir = getTargetPlatformDir "4.0.30319"
let nugetLibDir = nugetDir @@ "lib"
let nugetDocsDir = nugetDir @@ "docs"

// params
let target = getBuildParamOrDefault "target" "All"
let frameworkVersion = getBuildParamOrDefault "frameworkVersion" "v4.0"
let frameworkParams = 
    let v = ("[^\\d]" >=> "") frameworkVersion
    let v = v.Substring(0,2)
    ["TargetFrameworkVersion", frameworkVersion; "DefineConstants", "NET" + v]
    

// tools
let fakeVersion = GetPackageVersion packagesDir "FAKE"
let fakePath = sprintf "%sFAKE.%s/tools" packagesDir fakeVersion
let nugetPath = "./lib/Nuget/nuget.exe"
let nunitVersion = GetPackageVersion packagesDir "NUnit"
let nunitPath = sprintf "%sNUnit.%s/Tools" packagesDir nunitVersion

// files
let appReferences =
    let refs = !+ "./src/**/*.*proj"
               -- "./src/**/*.Silverlight.*proj"

    let refs = if frameworkVersion <> "v4.5"
                then refs -- "./src/**/*.TypeProviders.*proj"
                else refs
    let refs = if frameworkVersion = "v3.5"
                then refs -- "./src/FSharpx.Async/FSharpx.Async.fsproj"
                else refs
    refs |> Scan

let testReferences =
    let refs = !+ "./tests/**/*.*proj"
    let refs = if frameworkVersion <> "v4.5"
                then refs -- "./src/**/*.TypeProviders.*proj"
                else refs
    refs |> Scan

let filesToZip =
    !+ (buildDir + "/**/*.*")
        -- "*.zip"
        |> Scan

// targets
Target "Clean" (fun _ ->
    CleanDirs [buildDir; testDir; deployDir; docsDir]
)

Target "BuildApp" (fun _ ->
    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = version
            AssemblyTitle = projectName
            AssemblyDescription = projectSummary
            Guid = "1e95a279-c2a9-498b-bc72-6e7a0d6854ce"
            OutputFileName = "./src/FSharpx.Core/AssemblyInfo.fs" })

    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = version
            AssemblyTitle = "FSharpx.Http"
            AssemblyDescription = "This library provides common features for working with HTTP applications."
            Guid = "60F3BB81-5449-45DD-A217-B6045327680C"
            OutputFileName = "./src/FSharpx.Http/AssemblyInfo.fs" })

    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = version
            AssemblyTitle = "FSharpx.Async"
            AssemblyDescription = "This library implements various extensions for asynchronous programming using F# asynchronous workflows and F# agents."
            Guid = "ede1812b-5a62-410a-9553-02499cf29317"
            OutputFileName = "./src/FSharpx.Async/AssemblyInfo.fs" })

    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = version
            AssemblyTitle = "FSharpx.Observable"
            AssemblyDescription = "This library implements a mini-Reactive Extensions (MiniRx) and was authored by Phil Trelford."
            Guid = "2E802F54-9CD0-4B0A-B834-5C5979403B50"
            OutputFileName = "./src/FSharpx.Observable/AssemblyInfo.fs" })

    MSBuild buildDir "Build" (["Configuration","Release"] @ frameworkParams) appReferences
        |> Log "AppBuild-Output: "
)

Target "BuildTest" (fun _ ->
    MSBuild testDir "Build" ["Configuration","Debug"] testReferences
        |> Log "TestBuild-Output: "
)

Target "Test" (fun _ ->
    !+ (testDir + "/*.Tests.dll")
        |> Scan
        |> NUnit (fun p ->
            {p with
                ToolPath = nunitPath
                DisableShadowCopy = true
                OutputFile = testDir + "TestResults.xml" })
)

Target "GenerateDocumentation" (fun _ ->
    !+ (buildDir + "*.dll")
        |> Scan
        |> Docu (fun p ->
            {p with
                ToolPath = fakePath + "/docu.exe"
                TemplatesPath = "./lib/templates"
                OutputPath = docsDir })
)

Target "CopyLicense" (fun _ ->
    [ "LICENSE.md" ] |> CopyTo buildDir
)

Target "ZipDocumentation" (fun _ ->
    !+ (docsDir + "/**/*.*")
        |> Scan
        |> Zip docsDir (deployDir + sprintf "Documentation-%s.zip" version)
)

Target "BuildNuGet" (fun _ ->
    CleanDirs [nugetDir; nugetLibDir; nugetDocsDir]

    XCopy (docsDir |> FullName) nugetDocsDir
    let libs = ["FSharpx.Core"; "FSharpx.Http"; "FSharpx.Observable"]
    let libs = if frameworkVersion <> "v3.5"
                then "FSharpx.Async"::libs
                else libs
    let libs = [ for l in libs do
                 for e in ["dll";"pdb";"xml"] ->
                    sprintf "%s%s.%s" buildDir l e ]
    libs |> CopyTo nugetLibDir

    NuGet (fun p -> 
        {p with               
            Authors = authors
            Project = projectName + ".Core"
            Description = projectDescription
            Version = version
            OutputPath = nugetDir
            ToolPath = nugetPath
            AccessKey = getBuildParamOrDefault "nugetkey" ""
            Publish = hasBuildParam "nugetkey" })
        "FSharpx.Core.nuspec"

    !! (nugetDir + "FSharpx.Core.*.nupkg")
      |> CopyTo deployDir
)

Target "Deploy" (fun _ ->
    !+ (buildDir + "/**/*.*")
        -- "*.zip"
        |> Scan
        |> Zip buildDir (deployDir + sprintf "%s-%s.zip" projectName version)
)

Target "All" DoNothing

// Build order
"Clean"
  ==> "BuildApp" <=> "BuildTest" <=> "CopyLicense"
  ==> "Test" <=> "GenerateDocumentation"
  ==> "ZipDocumentation"
  ==> "BuildNuGet"
  ==> "Deploy"

"All" <== ["Deploy"]

// Start build
Run target
