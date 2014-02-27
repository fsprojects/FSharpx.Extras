(*** hide ***)
#I "../../bin"

(**
F# Project Scaffold
===================

Documentation

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The F# ProjectTemplate library can be <a href="https://nuget.org/packages/FSharpx">installed from NuGet</a>:
      <pre>PM> Install-Package FSharpx</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

Example
-------

This example demonstrates using a function defined in this sample library.

*)
#r "FSharpx.dll"
open System
open FSharpx

Double.parse "2.33"

(**
Some more info

Samples & documentation
-----------------------

The library comes with comprehensible documentation. 
It can include a tutorials automatically generated from `*.fsx` files in [the content folder][content]. 
The API reference is automatically generated from Markdown comments in the library implementation.

 * [Tutorial](tutorial.html) contains a further explanation of this sample library.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding new public API, please also 
consider adding [samples][content] that can be turned into a documentation. You might
also want to read [library design notes][readme] to understand how it works.

The library is available under Public Domain license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/fsprojects/FSharpx/tree/master/docs/content
  [gh]: https://github.com/fsprojects/FSharpx
  [issues]: https://github.com/fsprojects/FSharpx/issues
  [readme]: https://github.com/fsprojects/FSharpx/blob/master/README.md
  [license]: https://github.com/fsprojects/FSharpx/blob/master/LICENSE.txt
*)
