// ----------------------------------------------------------------------------
// F# structured format example. 
// (c) Semyon Grigorev, 2014, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------

// This example demonstrates how to use printer combinator library.

#r @"../../bin/v4.0/FSharpx.Extras.dll"
#r @"..\..\bin\v4.0\FSharpx.Text.StructuredFormat.dll"

open Microsoft.FSharp.Text.StructuredFormat
open Microsoft.FSharp.Text.StructuredFormat.LayoutOps

let stmt i = wordL <| "stmt_" + string i

let stmtBlock k =
    [for i in 0 .. k -> stmt i]
    |> List.reduce (@@)

let layout =
   let condBlock = wordL "if" ^^ bracketL (wordL "cond")
   let thenBlock = wordL "then" @@- (stmtBlock 2)
   let elseBlock = wordL "else" @@- (stmtBlock 3)
   condBlock
   @@ thenBlock
   @@ elseBlock

let str = Display.layout_to_string FormatOptions.Default layout

printfn "%s" str

(*
Expectesd result:

if (cond)
then
 stmt_0
 stmt_1
 stmt_2
else
 stmt_0
 stmt_1
 stmt_2
 stmt_3

*)
