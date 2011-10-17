#r @"bin\debug\$assemblyname$.dll"

open $rootnamespace$

type sample = MySample<"SampleStaticValue">
printfn "The provided static param value is %s" sample.MySampleFieldName


