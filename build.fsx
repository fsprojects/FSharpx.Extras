#I "./packages/FAKE.1.52.1.0/tools"
#r "FakeLib.dll"
open Fake 
open Fake.MSBuild

(* properties *)
let projectName = "FSharp.Monad"
let version = "1.0.0"  
let projectSummary = "A monad library for F# projects."
let projectDescription = "A monad library for F# projects, including Maybe, State, Reader, Writer, Continuation, and MinLinq."
let authors = ["Ryan Riley"]
let mail = "ryan.riley@panesofglass.org"
let homepage = "https://github.com/panesofglass/FSharp.Monad"

(* Directories *)
let buildDir = "./build/"
let docsDir = "./docs/" 
let deployDir = "./deploy/"
let testDir = "./test/"
let nugetDir = "./nuget/" 

(* Tools *)
let fakePath = "./packages/FAKE.1.52.1.0/tools"
let nunitPath = "./packages/NUnit.2.5.9.10348/Tools"
let nunitOutput = testDir + "TestResults.xml"
let zipFileName = deployDir + sprintf "%s-%s.zip" projectName version

(* files *)
let appReferences =
    !+ @"src\**\*.csproj" 
      ++ @"src\**\*.fsproj"
      -- "**\*_Spliced*" 
        |> Scan

let filesToZip =
  !+ (buildDir + "/**/*.*")     
      -- "*.zip"
      |> Scan      

(* Targets *)
Target? Clean <-
    fun _ -> CleanDirs [buildDir; testDir; deployDir; docsDir]

Target? BuildApp <-
    fun _ -> 
        if not isLocalBuild then
          AssemblyInfo 
           (fun p -> 
              {p with
                 CodeLanguage = FSharp;
                 AssemblyVersion = buildVersion;
                 AssemblyTitle = projectName;
                 AssemblyDescription = projectDescription;
                 Guid = "1e95a279-c2a9-498b-bc72-6e7a0d6854ce";
                 OutputFileName = "./src/AssemblyInfo.fs"})

        appReferences
          |> MSBuildRelease buildDir "Build"
          |> Log "AppBuild-Output: "

Target? BuildTest <-
    fun _ -> 
        appReferences
          |> MSBuildDebug testDir "Build"
          |> Log "TestBuild-Output: "

Target? Test <-
    fun _ ->
        !+ (testDir + "/*.dll")
          |> Scan
          |> NUnit (fun p -> 
                      {p with 
                         ToolPath = nunitPath; 
                         DisableShadowCopy = true; 
                         OutputFile = nunitOutput}) 

Target? GenerateDocumentation <-
    fun _ ->
      !+ (buildDir + "FSharp.Monad.dll")      
        |> Scan
        |> Docu (fun p ->
            {p with
               ToolPath = fakePath + "/docu.exe"
               TemplatesPath = fakePath + "/templates"
               OutputPath = docsDir })

Target? CopyLicense <-
    fun _ ->
        [ "LICENSE.txt" ] |> CopyTo buildDir

Target? BuildZip <-
    fun _ -> Zip buildDir zipFileName filesToZip

Target? ZipDocumentation <-
    fun _ ->    
        let docFiles = 
          !+ (docsDir + "/**/*.*")
            |> Scan
        let zipFileName = deployDir + sprintf "Documentation-%s.zip" version
        Zip docsDir zipFileName docFiles

Target? CreateNuGet <-
    fun _ ->
        let nugetDocsDir = nugetDir @@ "docs/"
        let nugetToolsDir = nugetDir @@ "tools/"
        
        XCopy docsDir nugetDocsDir
        XCopy buildDir nugetToolsDir
        
        NuGet (fun p ->
            {p with
                Authors = authors
                Project = projectName
                Description = projectDescription
                OutputPath = nugetDir }) "FSharp.Monad.nuspec"

Target? Default <- DoNothing
Target? Deploy <- DoNothing

// Dependencies
For? BuildApp <- Dependency? Clean
For? Test <- Dependency? BuildApp |> And? BuildTest
For? GenerateDocumentation <- Dependency? BuildApp
For? ZipDocumentation <- Dependency? GenerateDocumentation
For? BuildZip <- Dependency? BuildApp |> And? CopyLicense
For? CreateNuGet <- Dependency? Test |> And? BuildZip |> And? ZipDocumentation
For? Deploy <- Dependency? Test |> And? BuildZip |> And? ZipDocumentation
For? Default <- Dependency? Deploy

// start build
Run? Default
