# FSharpx

**FSharpx** is a collection of libraries and tools for use with F#. 

## Use via NuGET

    Install-Package FSharpx.Core 
    Install-Package FSharpx.Http
    Install-Package FSharpx.Observable
    Install-Package FSharpx.TypeProviders
    Install-Package FSharpx.TypeProviders.{Documents,Graph,AppSettings,Excel,Math,Regex,Machine,Xaml,Freebase}
    Install-Package FSharpx.Compatibility.OCaml  

## FSharpx Core

FSharpx.Core is a collection of general functional constructs extending F#. Its main target is F# but
it aims to be compatible with all .NET languages wherever possible.

FSharpx.Core provides:
 * Several standard monads: State, Reader, Writer, Either, Continuation, Distribution
 * Purely functional data structures: Queues, double-ended Queues, BottomUpMergeSort, RandomAccessList, Vector, RoseTree
 * Validation applicative functor
 * General functions like flip
 * Additional functions around collections
 * Functions to make C# - F# interop easier
 * [Async extensions, Reusable agents and Observable extensions](https://github.com/fsharp/fsharpx/wiki/FSharpx-Async-Extensions)


## Type Providers

FSharpx provides some very interesting F# type providers. At the moment we have:

 * FSharpx.TypeProviders.AppSettings which generates setters and getters for application settings files.
 * FSharpx.TypeProviders.Documents which allows strongly typed access to JSON, XML and CSV files.
 * FSharpx.TypeProviders.Excel which provides strongly typed access to Excel spread sheets.
 * FSharpx.TypeProviders.Graph which includes type providers for state machines and graphs.
 * FSharpx.TypeProviders.Math which contains a type provider for vector data structures.
 * FSharpx.TypeProviders.Regex which allows to access regular expressions in a strongly typed manner.
 * FSharpx.TypeProviders.Machine which provides strongly typed access to the Registry and the file system.
 * FSharpx.TypeProviders.Xaml which includes a type provider for XAML files and therefor enables to use Visual Studios’s WPF Designer from F#.
 * FSharpx.TypeProviders.Freebase which allows to access the Freebase database with strong typing.

At the moment all type providers should work with .NET 4.0 and .NET 4.5 and F# 3.0.

In order to build and test the type providers do the following:

 * Open FSharpx.WithTypeProviders.sln with Visual Studio 2012
 * Set FSharpx.TypeProviders as startup project
 * In the properties of FSharpx.TypeProviders:
   * Set the start action to "external program" and "C:\Program Files (x86)\Microsoft Visual Studio 11.0\Common7\IDE\devenv.exe"
   * Set the command line argument to "C:\code\fsharpx\FSharpx.TypeProviders.Tests.sln" (adjust the path to match to your system)
 * Run the project --> A new Visual Studio 2012 instance should opened with FSharpx.TypeProviders.Tests.sln


### Building the project

Read the wiki page about [Build](https://github.com/fsharp/fsharpx/wiki/Build)


