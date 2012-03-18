// Original Xml type provider by Tomas Petricek - tomasP.net
namespace FSharpx.TypeProviders

open System.Xml.Linq
open System.Collections.Generic

// ------------------------------------------------------------------------------------------------
// Runtime objects
// ------------------------------------------------------------------------------------------------

type TypedXElement(element:XElement) =
  member x.Element = element

type TypedXDocument(document:XDocument) =
  member x.Document = document

// ------------------------------------------------------------------------------------------------
// Helper utilities
// ------------------------------------------------------------------------------------------------

module Utils = 
  open System

  let wrapElements els = 
    seq { for e in els ->  TypedXElement(e) }

  // Active patterns & operators for parsing strings
  let (@?) (s:string) i = if i >= s.Length then None else Some s.[i]
  let sat f (c:option<char>) = match c with Some c when f c -> Some c | _ -> None
  let (|EOF|_|) c = match c with Some _ -> None | _ -> Some ()
  let (|LetterDigit|_|) = sat Char.IsLetterOrDigit
  let (|Upper|_|) = sat Char.IsUpper
  let (|Lower|_|) = sat Char.IsLower

  // Turns a string into a nice PascalCase identifier
  let niceName (s:string) = 
    // Starting to parse a new segment 
    let rec restart i = seq {
      match s @? i with 
      | EOF -> ()
      | LetterDigit _ & Upper _ -> yield! upperStart i (i + 1)
      | LetterDigit _ -> yield! consume i false (i + 1)
      | _ -> yield! restart (i + 1) }
    // Parsed first upper case letter, continue either all lower or all upper
    and upperStart from i = seq {
      match s @? i with 
      | Upper _ -> yield! consume from true (i + 1) 
      | Lower _ -> yield! consume from false (i + 1) 
      | _ -> yield! restart (i + 1) }
    // Consume are letters of the same kind (either all lower or all upper)
    and consume from takeUpper i = seq {
      match s @? i with
      | Lower _ when not takeUpper -> yield! consume from takeUpper (i + 1)
      | Upper _ when takeUpper -> yield! consume from takeUpper (i + 1)
      | _ -> 
          yield from, i
          yield! restart i }
    
    // Split string into segments and turn them to PascalCase
    seq { for i1, i2 in restart 0 do 
            let sub = s.Substring(i1, i2 - i1) 
            if Seq.forall Char.IsLetterOrDigit sub then
              yield sub.[0].ToString().ToUpper() + sub.ToLower().Substring(1) }
    |> String.concat ""

  let convertExpr typ expr = 
    if typ = typeof<bool> then 
      <@@ let (s:string) = %%expr
          s.Equals("true", StringComparison.InvariantCultureIgnoreCase) ||
          s.Equals("yes", StringComparison.InvariantCultureIgnoreCase) @@>
    elif typ = typeof<int> then
      <@@ Int32.Parse(%%expr : string) @@>
    elif typ = typeof<float> then
      <@@ Double.Parse(%%expr : string) @@>
    elif typ = typeof<string> then
      expr
    else failwith "Unexpected type in convertExpr"      

  let mkSeqTy = 
    let seqTy = typedefof<seq<_>>
    (fun ty -> seqTy.MakeGenericType[| ty |])

  let mkOptTy =
    let optTy = typedefof<option<_>>
    (fun ty -> optTy.MakeGenericType[| ty |])

  let makeUniqueName =
    let dict = new Dictionary<_, _>()
    (fun name ->
        if dict.ContainsKey(name) then
          dict.[name] <- dict.[name] + 1
          sprintf "%s%d" name (dict.[name])
        else 
          dict.[name] <- 0
          name)

// ------------------------------------------------------------------------------------------------
// Tests for the niceName function
// ------------------------------------------------------------------------------------------------

  let tests () = 
    let (=!=) a b = if a <> b then failwithf "%s <> %s" a b
    niceName "" =!= "" 
    niceName "__hello__" =!= "Hello"
    niceName "abc" =!= "Abc"
    niceName "hello_world" =!= "HelloWorld"
    niceName "HelloWorld" =!= "HelloWorld"
    niceName "helloWorld" =!= "HelloWorld"
    niceName "hello123" =!= "Hello123"
    niceName "Hello123" =!= "Hello123"
    niceName "hello!123" =!= "Hello123"
    niceName "HelloWorld123_hello__@__omg" =!= "HelloWorld123HelloOmg"
