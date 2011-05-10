#I "./packages/FAKE.1.54.1.0/tools"
#r "FakeLib.dll"

open Fake 
open System.IO

// properties
let projectName = "FSharp.Monad"
let version = "1.0.1"  
let projectSummary = "A monad library for F# projects."
let projectDescription = "A monad library for F# projects, including Maybe, State, Reader, Writer, Continuation, and MinLinq."
let authors = ["Ryan Riley"]
let mail = "ryan.riley@panesofglass.org"
let homepage = "http://github.com/panesofglass/FSharp.Monad"
let nugetKey = if System.IO.File.Exists "./key.txt" then ReadFileAsString "./key.txt" else ""

// directories
let buildDir = "./build/"
let packagesDir = "./packages/"
let testDir = "./test/"
let deployDir = "./deploy/"
let docsDir = "./docs/"
let nugetDir = "./nuget/"
let targetPlatformDir = getTargetPlatformDir "4.0.30319"
let nugetLibDir = nugetDir + "lib/"

// params
let target = getBuildParamOrDefault "target" "All"

// tools
let fakePath = "./packages/FAKE.1.54.1.0/tools"
let nunitPath = "./packages/NUnit.2.5.9.10348/Tools"

// files
let appReferences =
    !+ "./src/**/FSharp.Monad.fsproj"
        |> Scan

let testReferences =
    !+ "./src/**/FSharp.Monad.Tests.fsproj"
      |> Scan

let filesToZip =
    !+ (buildDir + "/**/*.*")
        -- "*.zip"
        |> Scan

// targets
Target "Clean" (fun _ ->
    CleanDirs [buildDir; testDir; deployDir]
)

Target "BuildApp" (fun _ ->
    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = version
            AssemblyTitle = projectSummary
            AssemblyDescription = projectDescription
            Guid = "1e95a279-c2a9-498b-bc72-6e7a0d6854ce"
            OutputFileName = "./src/FSharp.Monad/AssemblyInfo.fs" })

    MSBuildRelease buildDir "Build" appReferences
        |> Log "AppBuild-Output: "
)

Target "BuildTest" (fun _ ->
    printfn "%A" testReferences
    MSBuildDebug testDir "Build" testReferences
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
                TemplatesPath = fakePath + "/templates"
                OutputPath = docsDir })
)

Target "CopyLicense" (fun _ ->
    [ "LICENSE.txt" ] |> CopyTo buildDir
)

Target "ZipDocumentation" (fun _ ->
    !+ (docsDir + "/**/*.*")
        |> Scan
        |> Zip docsDir (deployDir + sprintf "Documentation-%s.zip" version)
)

Target "BuildNuGet" (fun _ ->
    CleanDirs [nugetDir; nugetLibDir]

    XCopy docsDir 
    [buildDir + "FSharp.Monad.dll"]
        |> CopyTo nugetLibDir

    NuGet (fun p -> 
        {p with               
            Authors = authors
            Project = projectName
            Version = version
            OutputPath = nugetDir
            AccessKey = nugetKey
            Publish = nugetKey <> "" })
        "fsharp.monad.nuspec"

    [nugetDir + sprintf "FSharp.Monad.%s.nupkg" version]
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
//  ==> "BuildNuGet"
  ==> "Deploy"

"All" <== ["Deploy"]

// Start build
Run target

