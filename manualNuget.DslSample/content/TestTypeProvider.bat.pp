IF EXIST "%ProgramFiles(x86)%" (
    "%ProgramFiles(x86)%\Microsoft SDKs\F#\3.0\Framework\v4.0\fsc.exe" -r:bin\debug\$assemblyname$.dll Test.fsx
) ELSE (
    "%ProgramFiles%\Microsoft SDKs\F#\3.0\Framework\v4.0\fsc.exe" -r:bin\debug\$assemblyname$.dll Test.fsx
)

pause