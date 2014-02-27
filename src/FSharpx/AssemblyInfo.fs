namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FSharpx")>]
[<assembly: AssemblyProductAttribute("FSharpx")>]
[<assembly: AssemblyDescriptionAttribute("FSharpx is a library for the .NET platform implementing general functional constructs on top of the F# core library.")>]
[<assembly: AssemblyVersionAttribute("0.0.0")>]
[<assembly: AssemblyFileVersionAttribute("0.0.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.0.0"
