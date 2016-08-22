namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FSharpx.Text.StructuredFormat")>]
[<assembly: AssemblyProductAttribute("FSharpx.Extras")>]
[<assembly: AssemblyDescriptionAttribute("FSharpx.Extras implements general functional constructs on top of the F# core library. Its main target is F# but it aims to be compatible with all .NET languages wherever possible.")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
    let [<Literal>] InformationalVersion = "1.0"
