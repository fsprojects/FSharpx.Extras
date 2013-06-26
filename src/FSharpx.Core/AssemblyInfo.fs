module FSharpx.Core.AssemblyInfo
#nowarn "49" // uppercase argument names
#nowarn "67" // this type test or downcast will always hold
#nowarn "66" // tis upast is unnecessary - the types are identical
#nowarn "58" // possible incorrect indentation..
#nowarn "57" // do not use create_DelegateEvent
#nowarn "51" // address-of operator can occur in the code
open System
open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
exception ReturnException183c26a427ae489c8fd92ec21a0c9a59 of obj
exception ReturnNoneException183c26a427ae489c8fd92ec21a0c9a59

[<assembly: ComVisible (false)>]

[<assembly: CLSCompliant (false)>]

[<assembly: Guid ("1e95a279-c2a9-498b-bc72-6e7a0d6854ce")>]

[<assembly: AssemblyTitle ("FSharpx")>]

[<assembly: AssemblyDescription ("FSharpx is a library for the .NET platform implementing general functional constructs on top of the F# core library. Its main target is F# but it aims to be compatible with all .NET languages wherever possible.

It currently implements:

* Several standard monads: State, Reader, Writer, Either, Continuation, Distribution
* Iteratee
* Purely functional data structures: Queues, double-ended Queues, BottomUpMergeSort, RandomAccessList, Vector, RoseTree, BKTree
* Validation applicative functor
* General functions like flip
* Additional functions around collections
* Functions to make C# - F# interop easier.")>]

[<assembly: AssemblyProduct ("FSharpx")>]

[<assembly: AssemblyVersion ("1.8.45.0")>]

[<assembly: AssemblyFileVersion ("1.8.45.0")>]

[<assembly: AssemblyDelaySign (false)>]

()
