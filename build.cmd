@echo off

dotnet tool restore
dotnet restore
dotnet run -v:m --project ./build/build.fsproj -- -t %*
