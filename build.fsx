#I "./packages/FAKE.1.56.6/tools"
#r "FakeLib.dll"

open Fake 
open System.IO

// properties
let currentDate = System.DateTime.UtcNow
let projectName = "FSharpx"
let version = "1.2." + currentDate.ToString("yMMdd")
let coreSummary = "FSharpx is a library for the .NET platform implementing general functional constructs on top of the F# core library."
let projectSummary = "FSharpx is a library for the .NET platform implementing general functional constructs on top of the F# core library."
let projectDescription = "FSharpx is a library for the .NET platform implementing general functional constructs on top of the F# core library. Its main target is F# but it aims to be compatible with all .NET languages wherever possible.\r\n\r\nIt currently implements:\r\n\r\n* Several standard monads: State, Reader, Writer, Either, Continuation, Distribution\r\n* Iteratee\r\n* Validation applicative functor\r\n* General functions like flip\r\n* Additional functions around collections\r\n* Functions to make C# - F# interop easier."
let authors = ["Ryan Riley"; "Mauricio Scheffer"; "Steffen Forkmann"]
let mail = "ryan.riley@panesofglass.org"
let homepage = "http://github.com/fsharp/fsharpx"
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
let frameworkVersion = getBuildParamOrDefault "frameworkVersion" "v4.0"
let frameworkParams = 
    let v = ("[^\\d]" >=> "") frameworkVersion
    let v = v.Substring(0,2)
    ["TargetFrameworkVersion", frameworkVersion; "DefineConstants", "NET" + v]
    

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
            AssemblyTitle = projectName
            AssemblyDescription = projectSummary
            Guid = "1e95a279-c2a9-498b-bc72-6e7a0d6854ce"
            OutputFileName = "./src/FSharpx.Core/AssemblyInfo.fs" })

    MSBuild buildDir "Build" (["Configuration","Release"] @ frameworkParams) appReferences
        |> Log "AppBuild-Output: "
)

Target "BuildTest" (fun _ ->
    MSBuild testDir "Build" (["Configuration","Debug"] @ frameworkParams) testReferences
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
    [ buildDir + "FSharpx.Core.dll"
      buildDir + "FSharpx.Core.pdb"
      buildDir + "FSharpx.Core.xml" ]
        |> CopyTo nugetLibDir

    NuGet (fun p -> 
        {p with               
            Authors = authors
            Project = projectName + ".Core"
            Description = projectDescription
            Version = version
            OutputPath = nugetDir
            AccessKey = nugetKey
            ToolPath = nugetPath
            Publish = nugetKey <> "" })
        "FSharpx.Core.nuspec"

    [nugetDir + sprintf "FSharpx.Core.%s.nupkg" version]
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
