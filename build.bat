@echo off
cls
".nuget/NuGet.exe" "install" "FAKE" "-OutputDirectory" "lib" "-ExcludeVersion" "-Prerelease"
"lib\FAKE\tools\FAKE.exe" "build.fsx" %*
pause
