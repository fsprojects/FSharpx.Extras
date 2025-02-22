@echo off

dotnet tool restore
if %ERRORLEVEL% NEQ 0 exit /b %ERRORLEVEL%
dotnet restore
if %ERRORLEVEL% NEQ 0 exit /b %ERRORLEVEL%
dotnet run -v:m --project ./build/build.fsproj -- -t %*
if %ERRORLEVEL% NEQ 0 exit /b %ERRORLEVEL%
