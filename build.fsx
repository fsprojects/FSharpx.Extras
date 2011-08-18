#I "./packages/FAKE.1.56.6/tools"
#r "FakeLib.dll"

open Fake 
open System.IO

// properties
let currentDate = System.DateTime.UtcNow
let projectName = "FSharp.Monad"
let version = "1.1.6." + currentDate.ToString("yMMdd")
let coreSummary = "Library containing standard operators for building computation expressions (monads)."
let projectSummary = "A monad library for F# projects."
let projectDescription = "A monad library for F# projects."
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
let nugetLibDir = nugetDir @@ "lib"
let nugetDocsDir = nugetDir @@ "docs"

// params
let target = getBuildParamOrDefault "target" "All"

// tools
let fakePath = "./packages/FAKE.1.56.6/tools"
let nugetPath = "./lib/Nuget/nuget.exe"
let nunitPath = "./packages/NUnit.2.5.9.10348/Tools"

// files
let appReferences =
    !+ "./src/**/*.*proj"
        |> Scan

let testReferences =
    !+ "./tests/**/*.*proj"
      |> Scan

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
            AssemblyTitle = projectName + ".Core"
            AssemblyDescription = coreSummary
            Guid = "1653436C-D15F-4E79-920A-FA7BC94306BB"
            OutputFileName = "./src/FSharp.Monad.Core/AssemblyInfo.fs" })

    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = version
            AssemblyTitle = projectName
            AssemblyDescription = projectSummary
            Guid = "1e95a279-c2a9-498b-bc72-6e7a0d6854ce"
            OutputFileName = "./src/FSharp.Monad/AssemblyInfo.fs" })

    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = version
            AssemblyTitle = projectName + ".MinLinq"
            AssemblyDescription = projectSummary
            Guid = "258D97E2-004C-4F9A-913B-A08B3E281544"
            OutputFileName = "./src/FSharp.Monad.MinLinq/AssemblyInfo.fs" })

    MSBuildRelease buildDir "Build" appReferences
        |> Log "AppBuild-Output: "
)

Target "BuildTest" (fun _ ->
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
                TemplatesPath = "./lib/templates"
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
    CleanDirs [nugetDir; nugetLibDir; nugetDocsDir]

    XCopy (docsDir |> FullName) nugetDocsDir
    [ buildDir + "FSharp.Monad.Core.dll"
      buildDir + "FSharp.Monad.Core.pdb"
      buildDir + "FSharp.Monad.Core.xml"
      buildDir + "FSharp.Monad.dll"
      buildDir + "FSharp.Monad.pdb"
      buildDir + "FSharp.Monad.xml"
      buildDir + "FSharp.Monad.Iteratee.dll"
      buildDir + "FSharp.Monad.Iteratee.pdb"
      buildDir + "FSharp.Monad.Iteratee.xml" ]
        |> CopyTo nugetLibDir

    NuGet (fun p -> 
        {p with               
            Authors = authors
            Project = projectName
            Description = projectDescription
            Version = version
            OutputPath = nugetDir
            AccessKey = nugetKey
            ToolPath = nugetPath
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
  ==> "BuildNuGet"
  ==> "Deploy"

"All" <== ["Deploy"]

// Start build
Run target

