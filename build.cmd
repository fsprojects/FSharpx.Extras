@echo off

dotnet tool restore
dotnet restore
if not "%*"=="" (
    dotnet fake run build.fsx --parallel 3 --target %*
) else (
    dotnet fake run build.fsx --parallel 3
)
