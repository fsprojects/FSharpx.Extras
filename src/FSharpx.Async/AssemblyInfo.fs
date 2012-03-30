module FSharpx.Async.AssemblyInfo
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

[<assembly: Guid ("ede1812b-5a62-410a-9553-02499cf29317")>]

[<assembly: AssemblyTitle ("FSharpx.Async")>]

[<assembly: AssemblyDescription ("This library implements various extensions for asynchronous programming using F# asynchronous workflows and F# agents.")>]

[<assembly: AssemblyProduct ("FSharpx.Async")>]

[<assembly: AssemblyVersion ("1.4.120330")>]

[<assembly: AssemblyFileVersion ("1.4.120330")>]

[<assembly: AssemblyDelaySign (false)>]

()
