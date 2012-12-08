#load "../FSharpx.TypeProviders/SetupTesting.fsx"
SetupTesting.setup __SOURCE_DIRECTORY__
#load "__setup__.fsx"

open System.IO
open FSharpx.TypeProviders.Helper
open FSharpx.TypeProviders.MiniCsvProvider
open FSharpx.TypeProviders.JsonTypeProvider
open FSharpx.TypeProviders.XmlTypeProvider

let (++) a b = Path.Combine(a, b)
let resolutionFolder = __SOURCE_DIRECTORY__ ++ ".." ++ ".." ++ "tests" ++ "FSharpx.TypeProviders.Documents.Tests"

let schema = 
        """{  
                 "firstName": "John",
                 "lastName" : "Smith",
                 "age"      : 25,
                 "address"  :
                 {
                     "streetAddress": "21 2nd Street",
                     "city"         : "New York",
                     "state"        : "NY",
                     "postalCode"   : "10021"
                 },
                 "phoneNumber":
                 [
                     {
                       "type"  : "home",
                       "number": "212 555-1234"
                     },
                     {
                       "type"  : "fax",
                       "number": "646 555-4567"
                     }
                 ]
             }"""

generate jsonType resolutionFolder [| "@@@missingValue###"; schema |] |> prettyPrint
generate csvType resolutionFolder [| "SmallTest.csv" |] |> prettyPrint
generate xmlType resolutionFolder [| "Philosophy.xml"; "@@@missingValue###" |] |> prettyPrint
