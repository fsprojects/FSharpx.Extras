module FSharp.Monad.Core.AssemblyInfo
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

[<assembly: Guid ("1653436C-D15F-4E79-920A-FA7BC94306BB")>]

[<assembly: AssemblyTitle ("FSharp.Monad.Core")>]

[<assembly: AssemblyDescription ("Library containing standard operators for building computation expressions (monads).")>]

[<assembly: AssemblyProduct ("FSharp.Monad.Core")>]

[<assembly: AssemblyVersion ("1.1.4.110810")>]

[<assembly: AssemblyFileVersion ("1.1.4.110810")>]

[<assembly: AssemblyDelaySign (false)>]

()
