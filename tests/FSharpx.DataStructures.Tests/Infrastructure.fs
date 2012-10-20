module FSharpx.DataStructures.Tests.Infrastructure

open FSharpx
open FSharpx.DataStructures
open FSharpx.DataStructures.Interfaces
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open FsUnit

(* quick cut n paste for fsi
#I "C:/Users/Jack\Documents/GitHub/fsharpx/packages"
#I "C:/Users/Jack/Documents/GitHub/fsharpx/packages/FsCheck.0.8.3.0/lib/net40-Client/"
#I "C:/Users/Jack\Documents/GitHub/fsharpx/src/FSharpx.Core/bin/Debug/"
#I "C:/Users/Jack/Documents/GitHub/fsharpx/tests/FSharpx.Tests/bin/Debug/"
#r "nunit.framework.dll"
#r "fsharpx.core.dll"
#r "FSharpx.Tests.dll"
open FSharpx
open FSharpx.DataStructures
open FSharpx.DataStructures.Interfaces
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open FsUnit;;
*)

(*
Recommend a range of size 1 - 12 for lists used to build test data structures:

Reason #1: If you pay attention to the output from classifyCollect, you will notice generated list sizes skew towards the minimum. In fact if you allow
FsCheck to generate empty (0 length) lists it will generate more empty lists than any other size. You probably only ever need one empty list for 
each "empty list" unit test, so build those tests without FsCheck.

Reason #2: Several data structures, especially those where the internal data representation is either binary or skew binary, have "distinct failure modes
across the low range of sizes". What I mean by this is "it is possible to have bugs specific to certain small sizes in these data structures". So it is 
important that every structure size up to a certain value (let us say "8" for arguments sake) needs to be tested every time. By default FsCheck generates 
100 (pseudo-random) lists. The larger the size range you allow for generation, the higher the chance these crucial small sizes will be skipped.
*)

let genListBool min max  = Gen.listOf Arb.generate<bool> |> Gen.suchThat (fun l -> (l.Length >= min) && (l.Length <= max))

let genListInt min max  = Gen.listOf Arb.generate<int> |> Gen.suchThat (fun l -> (l.Length >= min) && (l.Length <= max))

let genListObj min max  = Gen.listOf Arb.generate<obj> |> Gen.suchThat (fun l -> (l.Length >= min) && (l.Length <= max))

let genListString min max  = Gen.listOf Arb.generate<string> |> Gen.suchThat (fun l -> (l.Length >= min) && (l.Length <= max))

(*
Would like to find a way to associate classifyCollect stats with each individual unit test, both in the NUnit runner and within VS. For now only
available in the "Text Output" tab of the runner along with stats from every other unit test of the latest run. Only thing remotely available in the runner
is right clicking the test and selecting "properties", but I don't see a way to get these stats in there.
Can't figure out how to manipulate NUnit.Framework.TestContext.CurrentContext.Test.Properties which is 
System.Collections.Specialized.ListDictionary.NodeKeyValueCollection 
*)
let classifyCollect xs (count : int) (y : bool) =
    y |> Prop.collect count
    |> Prop.classify (xs.GetType().FullName.Contains("System.Int32")) "int"  
    |> Prop.classify (xs.GetType().FullName.Contains("System.String")) "string"
    |> Prop.classify (xs.GetType().FullName.Contains("System.Boolean")) "bool"
    |> Prop.classify (xs.GetType().FullName.Contains("System.Object")) "object"