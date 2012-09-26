#r "./packages/FAKE.1.64.5/tools/FakeLib.dll"

open Fake 
open System.IO

// properties
let currentDate = System.DateTime.UtcNow
let projectName = "FSharpx"
let version = if isLocalBuild then "1.6.77" else buildVersion
let coreSummary = "FSharpx is a library for the .NET platform implementing general functional constructs on top of the F# core library."
let projectSummary = "FSharpx is a library for the .NET platform implementing general functional constructs on top of the F# core library."
let authors = ["Steffen Forkmann"; "Daniel Mohl"; "Tomas Petricek"; "Ryan Riley"; "Mauricio Scheffer"; "Phil Trelford" ]
let mail = "ryan.riley@panesofglass.org"
let homepage = "http://github.com/fsharp/fsharpx"

// .NET Frameworks
let net35 = "v3.5"
let net40 = "v4.0"

// directories
let buildDir = "./build/"
let packagesDir = "./packages/"
let testDir = "./test/"
let deployDir = "./deploy/"
let docsDir = "./docs/"
let nugetMainDir = "./nuget/"

let targetPlatformDir = getTargetPlatformDir "v4.0.30319"

let nugetDir package = sprintf "./nuget/%s/" package
let nugetLibDir package = nugetDir package @@ "lib"
let nugetDocsDir package = nugetDir package @@ "docs"

let typeProvidersPackages = ["TypeProviders.Graph"; "TypeProviders.Documents"; "TypeProviders.Xaml"; "TypeProviders.Math"; "TypeProviders.Excel"; "TypeProviders.Machine"; "TypeProviders.Regex"; "TypeProviders.AppSettings"; "TypeProviders.Freebase"]
let packages = ["Core"; "Http"; "Observable"; "TypeProviders"] @ typeProvidersPackages

let projectDesc = "FSharpx is a library for the .NET platform implementing general functional constructs on top of the F# core library. Its main target is F# but it aims to be compatible with all .NET languages wherever possible."

let rec getPackageDesc = function
| "Http" -> projectDesc + "\r\n\r\nThis library provides common features for working with HTTP applications."
| "Observable" -> projectDesc + "\r\n\r\nThis library implements a mini-Reactive Extensions (MiniRx) and was authored by Phil Trelford."
| "TypeProviders" -> projectDesc + "\r\n\r\nThis library is for the .NET platform implementing common type providers on top of the FSharpx.Core."
| "TypeProviders.Graph" -> projectDesc + "\r\n\r\nThis library is for the .NET platform implementing a state machine type provider."
| "TypeProviders.Documents" -> projectDesc + "\r\n\r\nThis library is for the .NET platform implementing a type provider for JSON, XML and CSV documents."
| "TypeProviders.Xaml" -> projectDesc + "\r\n\r\nThis library is for the .NET platform implementing a type provider for Xaml files."
| "TypeProviders.Math" -> projectDesc + "\r\n\r\nThis library is for the .NET platform implementing a type provider for vectors."
| "TypeProviders.Excel" -> projectDesc + "\r\n\r\nThis library is for the .NET platform implementing a Excel type provider."
| "TypeProviders.Machine" -> projectDesc + "\r\n\r\nThis library is for the .NET platform implementing type providers for the file system and the registry."
| "TypeProviders.Regex" -> projectDesc + "\r\n\r\nThis library is for the .NET platform implementing a type providers for regular expressions."
| "TypeProviders.AppSettings" -> projectDesc + "\r\n\r\nThis library is for the .NET platform implementing an AppSettings type provider."
| "TypeProviders.Freebase" -> projectDesc + "\r\n\r\nThis library is for the .NET platform implementing a Freebase type provider."
| _ -> projectDesc + "\r\n\r\nIt currently implements:\r\n\r\n* Several standard monads: State, Reader, Writer, Either, Continuation, Distribution\r\n* Iteratee\r\n* Purely functional data structures: Queues, double-ended Queues, BottomUpMergeSort, RandomAccessList, Vector\r\n* Validation applicative functor\r\n* General functions like flip\r\n* Additional functions around collections\r\n* Functions to make C# - F# interop easier."

// params
let target = getBuildParamOrDefault "target" "All"

let buildTypeProviders frameworkVersion = frameworkVersion <> net35 && buildServer = BuildServer.LocalBuild

let normalizeFrameworkVersion frameworkVersion =
    let v = ("[^\\d]" >=> "") frameworkVersion
    v.Substring(0,2)

let frameworkParams frameworkVersion = ["TargetFrameworkVersion", frameworkVersion; "DefineConstants", "NET" + normalizeFrameworkVersion frameworkVersion]

// tools
let fakeVersion = GetPackageVersion packagesDir "FAKE"
let fakePath = sprintf "%sFAKE.%s/tools" packagesDir fakeVersion
let nugetPath = "./lib/Nuget/nuget.exe"
let nunitVersion = GetPackageVersion packagesDir "NUnit.Runners"
let nunitPath = sprintf "%sNUnit.Runners.%s/Tools" packagesDir nunitVersion

// files
let appReferences frameworkVersion =    
    { (!+ "./src/**/*.*proj") with 
        Excludes = 
            [yield "./src/**/*.Silverlight.*proj"
             if not (buildTypeProviders frameworkVersion) then                
                yield "./src/**/*.TypeProviders.*.*proj"
                yield "./src/**/*.TypeProviders.*proj"
             if frameworkVersion = net35 then 
                yield "./src/**/*.Async.fsproj"
                yield "./src/**/*.Http.fsproj" // TODO: why is that?
                yield "./src/**/*.Observable.fsproj" // TODO: why is that?
                  ] }
    |> Scan

let testReferences frameworkVersion =
    { (!+ "./tests/**/*.*proj") with 
        Excludes = [if not (buildTypeProviders frameworkVersion) then
                        yield "./tests/**/*.TypeProviders.*proj"
                        yield "./tests/**/*.TypeProviders.*.*proj"] }
    |> Scan

// targets
Target "Clean" (fun _ ->
    CleanDirs [buildDir; testDir; deployDir; docsDir; nugetMainDir]

    packages
    |> Seq.iter (fun x -> CleanDirs [nugetDir x; nugetLibDir x; nugetDocsDir x])
)

Target "AssemblyInfo" (fun _ ->
    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = version
            AssemblyTitle = projectName
            AssemblyDescription = getPackageDesc "Core"
            Guid = "1e95a279-c2a9-498b-bc72-6e7a0d6854ce"
            OutputFileName = "./src/FSharpx.Core/AssemblyInfo.fs" })

    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = version
            AssemblyTitle = "FSharpx.Http"
            AssemblyDescription = getPackageDesc "Http"
            Guid = "60F3BB81-5449-45DD-A217-B6045327680C"
            OutputFileName = "./src/FSharpx.Http/AssemblyInfo.fs" })

    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = version
            AssemblyTitle = "FSharpx.Observable"
            AssemblyDescription = getPackageDesc "Observable"
            Guid = "2E802F54-9CD0-4B0A-B834-5C5979403B50"
            OutputFileName = "./src/FSharpx.Observable/AssemblyInfo.fs" })
            
    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = version
            AssemblyTitle = "FSharpx.TypeProviders"
            AssemblyDescription = getPackageDesc "TypeProviders"
            Guid = "89B6AF94-507D-4BE0-98FA-A5124884DBA8"
            OutputFileName = "./src/FSharpx.TypeProviders/AssemblyInfo.fs" })

    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = version
            AssemblyTitle = "FSharpx.TypeProviders.Graph"
            AssemblyDescription = getPackageDesc "TypeProviders.Graph"
            Guid = "D68BF790-E641-4A40-9BC2-CCD8870D8C4B"
            OutputFileName = "./src/FSharpx.TypeProviders.Graph/AssemblyInfo.fs" })

    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = version
            AssemblyTitle = "FSharpx.TypeProviders.Documents"
            AssemblyDescription = getPackageDesc "TypeProviders.Documents"
            Guid = "39F68CD1-A6CC-4AF8-9734-3C2FE3E3B7D8"
            OutputFileName = "./src/FSharpx.TypeProviders.Documents/AssemblyInfo.fs" })

    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = version
            AssemblyTitle = "FSharpx.TypeProviders.Xaml"
            AssemblyDescription = getPackageDesc "TypeProviders.Xaml"
            Guid = "BF0A0BF6-B215-49F8-A842-C6CB0CB20B21"
            OutputFileName = "./src/FSharpx.TypeProviders.Xaml/AssemblyInfo.fs" })

    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = version
            AssemblyTitle = "FSharpx.TypeProviders.Math"
            AssemblyDescription = getPackageDesc "TypeProviders.Math"
            Guid = "B6D98F36-F327-4ECD-8E29-3C7296117498"
            OutputFileName = "./src/FSharpx.TypeProviders.Math/AssemblyInfo.fs" })

    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = version
            AssemblyTitle = "FSharpx.TypeProviders.Excel"
            AssemblyDescription = getPackageDesc "TypeProviders.Excel"
            Guid = "54AB8A7D-094D-49A7-AB18-AA34E388A43E"
            OutputFileName = "./src/FSharpx.TypeProviders.Excel/AssemblyInfo.fs" })

    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = version
            AssemblyTitle = "FSharpx.TypeProviders.Machine"
            AssemblyDescription = getPackageDesc "TypeProviders.Machine"
            Guid = "63B7CF90-901B-4809-ACBA-F6366B994677"
            OutputFileName = "./src/FSharpx.TypeProviders.Machine/AssemblyInfo.fs" })

    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = version
            AssemblyTitle = "FSharpx.TypeProviders.Regex"
            AssemblyDescription = getPackageDesc "TypeProviders.Regex"
            Guid = "6E8A9AD1-176F-49D9-8E1B-F91BEAB0AFD3"
            OutputFileName = "./src/FSharpx.TypeProviders.Regex/AssemblyInfo.fs" })

    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = version
            AssemblyTitle = "FSharpx.TypeProviders.AppSettings"
            AssemblyDescription = getPackageDesc "TypeProviders.AppSettings"
            Guid = "75A1B454-ED85-4FAB-939C-026891B758DB"
            OutputFileName = "./src/FSharpx.TypeProviders.AppSettings/AssemblyInfo.fs" })

    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = version
            AssemblyTitle = "FSharpx.TypeProviders.Freebase"
            AssemblyDescription = getPackageDesc "TypeProviders.Freebase"
            Guid = "9758C301-CC29-4D89-BE76-6C6BC0353867"
            OutputFileName = "./src/FSharpx.TypeProviders.Freebase/AssemblyInfo.fs" })

    AssemblyInfo (fun p ->
        {p with 
            CodeLanguage = FSharp
            AssemblyVersion = version
            AssemblyTitle = "FSharpx.TypeProviders.Freebase.DesignTime"
            AssemblyDescription = getPackageDesc "TypeProviders.Freebase.DesignTime"
            Guid = "028E461D-AA92-4E5F-8F0D-1C8778FBBD4F"
            OutputFileName = "./src/FSharpx.TypeProviders.Freebase.DesignTime/AssemblyInfo.fs" })
)

let buildAppTarget = TargetTemplate (fun frameworkVersion ->
    CleanDir buildDir

    appReferences frameworkVersion
    |> MSBuild buildDir "Build" (["Configuration","Release"] @ frameworkParams frameworkVersion)
    |> Log "AppBuild-Output: "
)

let buildTestTarget = TargetTemplate (fun frameworkVersion ->
    CleanDir testDir
    testReferences frameworkVersion
    |> MSBuild testDir "Build" ["Configuration","Debug"] 
    |> Log "TestBuild-Output: "
)

let testTarget = TargetTemplate (fun frameworkVersion ->
    ActivateFinalTarget "CloseTestRunner"
    !! (testDir + "/*.Tests.dll")
    |> NUnit (fun p ->
        {p with
            ToolPath = nunitPath
            DisableShadowCopy = true
            OutputFile = testDir + sprintf "TestResults.%s.xml" frameworkVersion })
)

Target "GenerateDocumentation" (fun _ ->
    !! (buildDir + "*.dll")
    |> Docu (fun p ->
        {p with
            ToolPath = fakePath + "/docu.exe"
            TemplatesPath = "./lib/templates"
            OutputPath = docsDir })
)

Target "ZipDocumentation" (fun _ ->
    !! (docsDir + "/**/*.*")
    |> Zip docsDir (deployDir + sprintf "Documentation-%s.zip" version)
)

let prepareNugetTarget = TargetTemplate (fun frameworkVersion ->
    packages
    |> Seq.iter (fun package ->
        let frameworkSubDir = nugetLibDir package @@ normalizeFrameworkVersion frameworkVersion
        if not <| package.StartsWith "TypeProviders" || buildTypeProviders frameworkVersion then
            CleanDir frameworkSubDir

            [for ending in ["dll";"pdb";"xml"] do
                yield sprintf "%sFsharpx.%s.%s" buildDir package ending
                yield sprintf "%sFsharpx.%s.DesignTime.%s" buildDir package ending]
            |> Seq.filter (fun f -> File.Exists f)
            |> CopyTo frameworkSubDir)
)

let buildFrameworkVersionTarget = TargetTemplate (fun frameworkVersion -> ())

let generateTargets() =
    let versions = 
        [if hasBuildParam "v35" then yield net35
         if hasBuildParam "v40" then yield net40]

    if versions = [] then [net40] else versions
    |> Seq.fold
        (fun dependency frameworkVersion -> 
            tracefn "Generating targets for .NET %s" frameworkVersion
            let v = normalizeFrameworkVersion frameworkVersion
            let buildApp = sprintf "BuildApp_%s" v
            let buildTest = sprintf "BuildTest_%s" v
            let test = sprintf "Test_%s" v
            let prepareNuget = sprintf "PrepareNuget_%s" v
            let buildFrameworkVersion = sprintf "Build_%s" v

            buildAppTarget buildApp frameworkVersion
            buildTestTarget buildTest frameworkVersion
            testTarget test frameworkVersion
            prepareNugetTarget prepareNuget frameworkVersion
            buildFrameworkVersionTarget buildFrameworkVersion frameworkVersion

            dependency ==> buildApp ==> buildTest ==> test ==> prepareNuget ==> buildFrameworkVersion)
            "AssemblyInfo"

let nugetTarget = TargetTemplate (fun package ->
    XCopy (docsDir |> FullName) (nugetDocsDir package)
    [ "LICENSE.md" ] |> CopyTo (nugetDir package)
    NuGet (fun p -> 
        {p with               
            Authors = authors
            Project = projectName + "." + package
            Description = getPackageDesc package
            Version = version
            OutputPath = nugetDir package
            ToolPath = nugetPath
            AccessKey = getBuildParamOrDefault "nugetkey" ""
            Dependencies =
                if package = "TypeProviders" then
                  typeProvidersPackages
                  |> List.map (fun p -> "FSharpx." + p, RequireExactly (NormalizeVersion version))
                elif package = "Core" || package.StartsWith "TypeProviders" then p.Dependencies else
                  [projectName + ".Core", RequireExactly (NormalizeVersion version)]
            Publish = hasBuildParam "nugetkey" })
        "FSharpx.Core.nuspec"

    !! (nugetDir package + sprintf "FSharpx.%s.*.nupkg" package)
      |> CopyTo deployDir
)

let generateNugetTargets() =
    packages 
    |> Seq.fold
        (fun dependency package -> 
            tracefn "Generating nuget target for package %s" package
            let buildPackage = sprintf "Nuget_%s" package
            
            nugetTarget buildPackage package

            dependency ==> buildPackage)
            "ZipDocumentation"

Target "DeployZip" (fun _ ->
    !! (buildDir + "/**/*.*")
    |> Zip buildDir (deployDir + sprintf "%s-%s.zip" projectName version)
)

FinalTarget "CloseTestRunner" (fun _ ->  
    ProcessHelper.killProcess "nunit-agent.exe"
)

Target "Deploy" DoNothing
Target "All" DoNothing

// Build order
"Clean"
  ==> "AssemblyInfo"
  ==> (generateTargets())
  ==> "GenerateDocumentation"
  ==> "ZipDocumentation"
  ==> (generateNugetTargets())
  ==> "DeployZip"
  ==> "Deploy"

"All" <== ["Deploy"]

// Start build
Run target