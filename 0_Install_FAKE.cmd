@echo off
cls

"lib\nuget\nuget.exe" "install" "FAKE" "-OutputDirectory" "lib" "-ExcludeVersion"

pause