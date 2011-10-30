module FSharpx.Stm.AssemblyInfo
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

[<assembly: Guid ("FFDBAF72-7F9F-4DE0-A8F3-83D0D21EBEB5")>]

[<assembly: AssemblyTitle ("FSharpx.Stm")>]

[<assembly: AssemblyDescription ("This library implements the Software Transactional Memory workflow in F#.")>]

[<assembly: AssemblyProduct ("FSharpx.Stm")>]

[<assembly: AssemblyVersion ("1.3.111030")>]

[<assembly: AssemblyFileVersion ("1.3.111030")>]

[<assembly: AssemblyDelaySign (false)>]

()
