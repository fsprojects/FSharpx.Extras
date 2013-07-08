#I @"./lib/FAKE/tools"
#r @"./lib/FAKE/tools/FakeLib.dll"

open Fake 
open Fake.Git
open System.IO

let nugetPath = ".nuget/NuGet.exe"
let RestorePackage() =
    !! "./**/packages.config"
    |> Seq.iter (RestorePackage (fun p -> { p with ToolPath = nugetPath }))

RestorePackage()

// properties
let currentDate = System.DateTime.UtcNow
let projectName = "FSharpx"

let version =
    if hasBuildParam "version" then getBuildParam "version" else
    if isLocalBuild then getLastTag() else
    buildVersion

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
let buildPortableDir = "./build-portable/"
let packagesDir = "./packages/"
let testDir = "./test/"
let deployDir = "./deploy/"
let docsDir = "./docs/"
let nugetMainDir = "./nuget/"

let targetPlatformDir = getTargetPlatformDir "v4.0.30319"

let nugetDir package = sprintf "./nuget/%s/" package
let nugetLibDir package = nugetDir package @@ "lib"
let nugetDocsDir package = nugetDir package @@ "docs"

let typeProvidersPackages = ["TypeProviders.Graph"; "TypeProviders.Xaml"; "TypeProviders.Math"; "TypeProviders.Excel"; "TypeProviders.Machine"; "TypeProviders.Regex"; "TypeProviders.AppSettings"; "TypeProviders.Management"; "TypeProviders.Xrm"]
let packages = ["Core"; "Http"; "Observable"; "Collections.Experimental"; "TypeProviders"; "Text.StructuredFormat"] @ typeProvidersPackages

let projectDesc = "FSharpx is a library for the .NET platform implementing general functional constructs on top of the F# core library. Its main target is F# but it aims to be compatible with all .NET languages wherever possible."

let rec getPackageDesc = function
| "Http" -> projectDesc + "\r\n\r\nThis library provides common features for working with HTTP applications."
| "Collections.Experimental" -> projectDesc + "\r\n\r\nThis library provides experimental data structures."
| "Observable" -> projectDesc + "\r\n\r\nThis library implements a mini-Reactive Extensions (MiniRx) and was authored by Phil Trelford."
| "Text.StructuredFormat" -> projectDesc + "\r\n\r\nThis library provides data structures and functoins for pretty printers."
| "TypeProviders" -> projectDesc + "\r\n\r\nThis library is for the .NET platform implementing common type providers on top of the FSharpx.Core."
| "TypeProviders.Graph" -> projectDesc + "\r\n\r\nThis library is for the .NET platform implementing a state machine type provider."
| "TypeProviders.Xaml" -> projectDesc + "\r\n\r\nThis library is for the .NET platform implementing a type provider for Xaml files."
| "TypeProviders.Math" -> projectDesc + "\r\n\r\nThis library is for the .NET platform implementing a type provider for vectors."
| "TypeProviders.Excel" -> projectDesc + "\r\n\r\nThis library is for the .NET platform implementing a Excel type provider."
| "TypeProviders.Machine" -> projectDesc + "\r\n\r\nThis library is for the .NET platform implementing type providers for the file system and the registry."
| "TypeProviders.Regex" -> projectDesc + "\r\n\r\nThis library is for the .NET platform implementing a type providers for regular expressions."
| "TypeProviders.AppSettings" -> projectDesc + "\r\n\r\nThis library is for the .NET platform implementing an AppSettings type provider."
| "TypeProviders.Management" -> projectDesc + "\r\n\r\nThis library is for the .NET platform implementing a WMI type provider."
| "TypeProviders.Xrm" -> projectDesc + "\r\n\r\nThis library is for the .NET platform implementing a type provider for Microsoft Dynamics CRM 2011"
| _ -> projectDesc + "\r\n\r\nIt currently implements:\r\n\r\n" + 
                       "* Several standard monads: State, Reader, Writer, Either, Continuation, Distribution\r\n" +
                       "* Iteratee\r\n" +
                       "* Purely functional data structures: Queues, double-ended Queues, BottomUpMergeSort, RandomAccessList, Vector, RoseTree, BKTree\r\n" +
                       "* Validation applicative functor\r\n" + 
                       "* General functions like flip\r\n* Additional functions around collections\r\n* Functions to make C# - F# interop easier."

// params
let target = getBuildParamOrDefault "target" "All"

let buildTypeProviders frameworkVersion = frameworkVersion <> net35 && buildServer = BuildServer.LocalBuild

let normalizeFrameworkVersion frameworkVersion =
    let v = ("[^\\d]" >=> "") frameworkVersion
    v.Substring(0,2)

let frameworkParams portable frameworkVersion = 
    if portable then
        ["TargetFramework", "portable47"
         "TargetFrameworkVersion", frameworkVersion
         "TypeProviderRuntimeFramework", "portable47"
         "DefineConstants", "NET" + normalizeFrameworkVersion frameworkVersion + ";FX_NO_LOCAL_FILESYSTEM;FX_NO_CONCURRENT;NO_SYSTEM_ENVIRONMENT_GETENVIRONMENTVARIABLE;FX_NO_CUSTOMTYPEDESCRIPTOR;FX_NO_CUSTOMATTRIBUTEDATA;FX_NO_SYNC_WEBRESPONSE;FX_NO_WEBREQUEST_CONTENTLENGTH;FX_NO_GETCURRENTMETHOD;FX_NO_WEBHEADERS_ADD;TYPE_PROVIDER_RUNTIME_FX_PORTABLE47;TRACE"]
    else
        ["TargetFrameworkVersion", frameworkVersion
         "DefineConstants", "NET" + normalizeFrameworkVersion frameworkVersion]

// tools
let nunitVersion = GetPackageVersion packagesDir "NUnit.Runners"
let nunitPath = sprintf "%sNUnit.Runners.%s/Tools" packagesDir nunitVersion

// files
let appReferences portable frameworkVersion =
    if portable then Seq.empty else
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
                        yield "./tests/**/*.TypeProviders.*.*proj"
                    if frameworkVersion = net35 then
                        yield "./tests/**/FSharpx.Collections.Tests.fsproj" // FsCheck is no longer available for .NET 3.5
                        yield "./tests/**/FSharpx.Collections.Experimental.Tests.fsproj" // FsCheck is no longer available for .NET 3.5
                    ] }
    |> Scan

// targets
Target "Clean" (fun _ ->       
    CleanDirs [buildDir; buildPortableDir; testDir; deployDir; docsDir; nugetMainDir]

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
            AssemblyTitle = projectName
            AssemblyDescription = getPackageDesc "Collections.Experimental"
            Guid = "4C646C09-6925-47D0-B187-8A5C3D061329"
            OutputFileName = "./src/FSharpx.Collections.Experimental/AssemblyInfo.fs" })

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
            AssemblyTitle = "FSharpx.Text.StructuredFormat"
            AssemblyDescription = getPackageDesc "Text.StructuredFormat"
            Guid = "65e077ed-f51a-42d7-8004-e90d60af8b8f"
            OutputFileName = "./src/FSharpx.Text.StructuredFormat/AssemblyInfo.fs" })
            
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
            AssemblyTitle = "FSharpx.TypeProviders.Xrm"
            AssemblyDescription = getPackageDesc "TypeProviders.Xrm"
            Guid = "D69A0DB8-9D31-4CDD-8470-231A15313DBA"
            OutputFileName = "./src/FSharpx.TypeProviders.Xrm/AssemblyInfo.fs" })

            // TODO: COMVISIBLE is not working with portable
//    AssemblyInfo (fun p ->
//        {p with 
//            CodeLanguage = FSharp
//            AssemblyVersion = version
//            AssemblyTitle = "FSharpx.TypeProviders.Management"
//            AssemblyDescription = getPackageDesc "TypeProviders.Management"
//            Guid = "a19058ba-54bf-498f-bed3-6564d5117842"
//            OutputFileName = "./src/FSharpx.TypeProviders.Management/AssemblyInfo.fs" })
)

let buildAppTarget = TargetTemplate (fun frameworkVersion ->
    CleanDir buildDir
    CleanDir buildPortableDir

    appReferences false frameworkVersion
    |> MSBuild buildDir "Rebuild" (["Configuration","Release"] @ frameworkParams false frameworkVersion)
    |> Log "AppBuild-Output: "

    if frameworkVersion = net40 then
        appReferences true frameworkVersion
        |> MSBuild buildPortableDir "Rebuild" (["Configuration","Release"] @ frameworkParams true frameworkVersion)
        |> Log "AppBuild-Output: "

        !! (buildDir @@ "*DesignTime.*")
            |> CopyTo buildPortableDir
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

let prepareNugetTarget = TargetTemplate (fun frameworkVersion ->
    packages
    |> Seq.iter (fun package ->
        let frameworkSubDir = nugetLibDir package @@ normalizeFrameworkVersion frameworkVersion
        let portableSubDir = nugetLibDir package @@ "portable-net4+sl4+wp71+win8"
        if not <| package.StartsWith "TypeProviders" || buildTypeProviders frameworkVersion then
            CleanDir frameworkSubDir
            CleanDir portableSubDir

            if package = "TypeProviders.Xrm" then
                [for ending in ["dll";"xml"] do
                    yield sprintf "%smicrosoft.xrm.sdk.%s" buildDir ending
                    yield sprintf "%sMicrosoft.Crm.Services.Utility.%s" buildDir ending]
                |> Seq.filter (fun f -> File.Exists f)
                |> CopyTo frameworkSubDir

            [for ending in ["dll";"pdb";"xml"] do
                yield sprintf "%sFSharpx.%s.%s" buildDir package ending
                yield sprintf "%sFSharpx.%s.DesignTime.%s" buildDir package ending]
            |> Seq.filter (fun f -> File.Exists f)
            |> CopyTo frameworkSubDir

            [for ending in ["dll";"pdb";"xml"] do
                yield sprintf "%sFSharpx.%s.%s" buildPortableDir package ending
                yield sprintf "%sFSharpx.%s.DesignTime.%s" buildPortableDir package ending]
            |> Seq.filter (fun f -> File.Exists f)
            |> CopyTo portableSubDir)
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
    CleanDir docsDir
    !! (buildDir @@ (sprintf "FSharpx.%s.dll" package))
    |> Docu (fun p ->
        {p with
            ToolPath = "./lib/FAKE/tools/docu.exe"
            TemplatesPath = "./lib/templates"
            OutputPath = docsDir })

    XCopy (FullName docsDir) (nugetDocsDir package)
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

Target "TestAll" DoNothing

let generateNugetTargets() =
    packages 
    |> Seq.fold
        (fun dependency package -> 
            tracefn "Generating nuget target for package %s" package
            let buildPackage = sprintf "Nuget_%s" package
            
            nugetTarget buildPackage package

            dependency ==> buildPackage)
            "TestAll"

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
  ==> "TestAll"
  ==> (generateNugetTargets())
  ==> "DeployZip"
  ==> "Deploy"

"All" <== ["Deploy"]

// Start build
Run target
