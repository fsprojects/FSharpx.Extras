#I "./packages/FAKE.1.54.1.0/tools"
#r "FakeLib.dll"

open Fake 

// properties
let projectName = "FSharp.Monad"
let version = "1.0.1"  
let projectSummary = "A monad library for F# projects."
let projectDescription = "A monad library for F# projects, including Maybe, State, Reader, Writer, Continuation, and MinLinq."
let authors = ["Ryan Riley"]
let mail = "ryan.riley@panesofglass.org"
let homepage = "http://github.com/panesofglass/FSharp.Monad"

// directories
let buildDir = "./build/"
let testDir = "./test/"
let deployDir = "./deploy/"
let docsDir = "./docs/"
let nugetDir = "./nuget/"

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
                OutputPath = docsDir })
)

Target "ZipDocumentation" (fun _ ->
    !+ (docsDir + "/**/*.*")
        |> Scan
        |> Zip docsDir (deployDir + sprintf "Documentation-%s.zip" version)
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
  ==> "BuildApp" <=> "BuildTest"
  ==> "Test" <=> "GenerateDocumentation"
  ==> "ZipDocumentation"
  ==> "Deploy"

"All" <== ["Deploy"]

// Start build
Run target

