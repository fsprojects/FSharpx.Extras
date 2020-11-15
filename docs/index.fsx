(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../bin"

(** 
# FSharpx.Extras

## FSharpx.Extras

FSharpx.Extras implements:

* Several standard monads: State, Reader, Writer, Either, Continuation, Distribution

* Validation applicative functor

* General functions like flip

* Some asynchronous programming utilities

* Functions to make C# - F# interop easier.

* A few features for working with HTTP applications.

FSharpx.Extras is a set of additions on top of FSharpx.Collections and FSharpx.Async


## FSharpx.Text.StructuredFormat

This is a separate component:

* FSharpx.Text.StructuredFormat provides data structures and functions for pretty printers.


*)

#r "FSharpx.Collections.dll"
#r "FSharpx.Extras.dll"

open FSharpx

open FSharpx.Collections

// Access the object that proves that 'List' is a 'monoid'

FSharpx.Collections.List.monoid

(**

## Examples

* [DiningPhilosophers](DiningPhilosophers.html)
* [Santa](Santa.html)
* [StmSample](StmSample.html)
* [StructuredFormatSample](StructuredFormatSample.html)
* [UndoSample](UndoSample.html)
* [WebProxy](WebProxy.html)

*)
