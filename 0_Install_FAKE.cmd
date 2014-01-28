@echo off
cls

".nuget\nuget.exe" "install" "FAKE" "-OutputDirectory" "lib" "-ExcludeVersion" "-Prerelease"
pause