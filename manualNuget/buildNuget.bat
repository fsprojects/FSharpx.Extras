c:\nuget\nuget.exe pack C:\git\fsharpx\manualNuget\FSharpx.TypeProviders.nuspec -b C:\git\fsharpx\manualNuget -o C:\git\fsharpx\manualNuget
md C:\nuget\fsharpx\
copy C:\git\fsharpx\manualNuget\*.nupkg C:\nuget\fsharpx\ /Y
pause																			