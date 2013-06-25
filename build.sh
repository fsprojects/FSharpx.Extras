#!/bin/sh
mono "./lib/NuGet/NuGet.exe" "install" "FAKE" "-OutputDirectory" "lib" "-ExcludeVersion" "-Prerelease"
mono "./lib/FAKE/tools/FAKE.exe" build.fsx all v40 "NugetKey=6cfcf909-7b5a-417d-a706-d1a1ab9b4b9e"
