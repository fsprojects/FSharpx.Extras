#if INTERACTIVE
#load "../FSharpx.TypeProviders/SetupTesting.fsx"
SetupTesting.generateSetupScript __SOURCE_DIRECTORY__
#load "__setup__.fsx"
#endif

open System
open System.IO
open FSharpx.TypeProviders.Helper
open FSharpx.TypeProviders.MiniCsvProvider
open FSharpx.TypeProviders.JsonTypeProvider
open FSharpx.TypeProviders.XmlTypeProvider

let (++) a b = Path.Combine(a, b)
let resolutionFolder = __SOURCE_DIRECTORY__ ++ ".." ++ ".." ++ "tests" ++ "FSharpx.TypeProviders.Documents.Tests"

generate csvType resolutionFolder [| "SmallTest.csv" |] 
|> prettyPrint
|> Console.WriteLine

generate xmlType resolutionFolder [| "Philosophy.xml"; missingValue |] 
|> prettyPrint
|> Console.WriteLine

generate xmlType resolutionFolder [| "projects.xml"; missingValue |] 
|> prettyPrint
|> Console.WriteLine

generate xmlType "" [| missingValue; """<authors><author name="Ludwig" surname="Wittgenstein" age="29" isPhilosopher="True" size="30.3" /></authors>""" |] 
|> prettyPrint
|> Console.WriteLine

generate xmlType "" [| missingValue; """<topics><topic><title>My Topic Title</title></topic><topic><title>Another Topic Title</title></topic></topics>""" |] 
|> prettyPrint
|> Console.WriteLine

generate xmlType "" [| missingValue; "<authors><item name=\"Steffen\" /><item size=\"42.42\" isCool=\"true\" age=\"29\" name=\"Tomas\" /></authors>" |] 
|> prettyPrint
|> Console.WriteLine

generate xmlType "" [| missingValue; """<CellSet><Row key="base64"><Cell timestamp="1349209076311" column="cGw6eG1s">base64</Cell></Row></CellSet>""" |] 
|> prettyPrint
|> Console.WriteLine

generate jsonType resolutionFolder [| "Simple.json"; missingValue |] 
|> prettyPrint 
|> Console.WriteLine

generate jsonType resolutionFolder [| "Nested.json"; missingValue |] 
|> prettyPrint 
|> Console.WriteLine

generate jsonType resolutionFolder [| "DoubleNested.json"; missingValue |] 
|> prettyPrint 
|> Console.WriteLine

generate jsonType resolutionFolder [| "SimpleArray.json"; missingValue |] 
|> prettyPrint 
|> Console.WriteLine

generate jsonType resolutionFolder [| "OptionValues.json"; missingValue |] 
|> prettyPrint 
|> Console.WriteLine

generate jsonType resolutionFolder [| "WikiData.json"; missingValue |] 
|> prettyPrint 
|> Console.WriteLine

generate jsonType resolutionFolder [| "Empty.json"; missingValue |] 
|> prettyPrint 
|> Console.WriteLine

generate jsonType resolutionFolder [| "projects.json"; missingValue |] 
|> prettyPrint 
|> Console.WriteLine

generate jsonType "" [| missingValue; """["Adam","Eve","Bonnie","Clyde","Donald","Daisy","Han","Leia"]""" |] 
|> prettyPrint 
|> Console.WriteLine

generate jsonType "" [| missingValue; """[["Adam","Eve"],["Bonnie","Clyde"],["Donald","Daisy"],["Han","Leia"]]""" |] 
|> prettyPrint 
|> Console.WriteLine

generate jsonType "" [| missingValue; """{ "firstName": "Max" "lastName": "Mustermann" "age": 26 "isCool": true }""" |] 
|> prettyPrint 
|> Console.WriteLine

generate jsonType "" [| missingValue; """{ "authors": [{ "name": "Steffen" }, { "name": "Tomas", "age": 29, "isCool": true, "size":42.42 }]}""" |] 
|> prettyPrint 
|> Console.WriteLine

generate jsonType "" [| missingValue
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
                            }""" |]
|> prettyPrint 
|> Console.WriteLine

generate jsonType "" [| missingValue; """{"Row":[{"key":"base64","Cell":[{"timestamp":1349209076311,"column":"cGw6eG1s","$":"base64"}]}]}""" |] 
|> prettyPrint 
|> Console.WriteLine
