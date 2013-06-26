module FSharpx.Observable.AssemblyInfo
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

[<assembly: Guid ("2E802F54-9CD0-4B0A-B834-5C5979403B50")>]

[<assembly: AssemblyTitle ("FSharpx.Observable")>]

[<assembly: AssemblyDescription ("FSharpx is a library for the .NET platform implementing general functional constructs on top of the F# core library. Its main target is F# but it aims to be compatible with all .NET languages wherever possible.

This library implements a mini-Reactive Extensions (MiniRx) and was authored by Phil Trelford.")>]

[<assembly: AssemblyProduct ("FSharpx.Observable")>]

[<assembly: AssemblyVersion ("1.8.45.0")>]

[<assembly: AssemblyFileVersion ("1.8.45.0")>]

[<assembly: AssemblyDelaySign (false)>]

()
