namespace FSharpx

open System
open System.Globalization

module Strings =
    /// Checks whether the given text starts with the given prefix
    let inline startsWith prefix (text:string) = text.StartsWith prefix

    /// Replaces the given "replacement" for every occurence of the pattern in the given text 
    let inline replace (pattern:string) replacement (text:string) = text.Replace(pattern,replacement)

    /// Returns a value indicating whether the specified substring occurs within this string
    let inline contains substr (t: string) = t.Contains substr
    
    /// Splits the given string at the given delimiter
    let inline split (delimiter:char) (text:string) = text.Split [|delimiter|]

    /// Converts a sequence of strings to a single string separated with the delimiters
    [<System.Obsolete("Function 'separatedBy' obsolete. Use 'String.concat' from 'Microsoft.FSharp.Core' instead.")>]
    let inline separatedBy delimiter (items: string seq) = String.Join(delimiter, Array.ofSeq items)

    let inline toCharArray (str:string) = str.ToCharArray()

    let isNewline c = c = '\r' || c = '\n'
            
    /// Returns a sequence of strings split by the predicate    
    let splitBy (isDelimiter:char -> bool) (str:string) = 
        seq{
            let result = new System.Text.StringBuilder()
            for char in str do
                if not (isDelimiter char) then 
                    result.Append char |> ignore
                else if result.Length > 0 then 
                    yield result.ToString()
                    result.Length <- 0

            // yield the last accumulated value if one exists
            if result.Length > 0 then yield result.ToString()
        }

    /// Splits a string based on newlines 
    let toLines (input:string) : string seq = splitBy isNewline input
        
    /// Creates newline seperated string from the string list
    let joinLines (input:string list) : string = (String.concat System.Environment.NewLine input).Trim()

    /// Splits a string based on whitespace (spaces, tabs, and newlines)
    let toWords (input:string) : string seq = splitBy Char.IsWhiteSpace input

    /// Folds the string list by seperating entries with a single space
    let joinWords (input: string list) : string = (String.concat " " input).Trim()

    /// Returns if the string is null or empty
    let inline isNullOrEmpty text = String.IsNullOrEmpty text

    /// Returns the pluralized version of a noun
    let pluralize (noun: string) =        
        if noun.Contains " of " || noun.Contains " Of " then 
            noun
        else
            Pluralizer.toPlural noun

    /// Returns the singularized version of a noun
    let singularize (noun: string) =
        Pluralizer.toSingular noun

    // Active patterns & operators for parsing strings
    let (@?) (s:string) i = if i >= s.Length then None else Some s.[i]

    let inline satisfies predicate (charOption:option<char>) = 
        match charOption with 
        | Some c when predicate c -> charOption 
        | _ -> None

    let (|EOF|_|) = function 
        | Some _ -> None
        | _ -> Some ()

    let (|LetterDigit|_|) = satisfies Char.IsLetterOrDigit
    let (|Upper|_|) = satisfies Char.IsUpper
    let (|Lower|_|) = satisfies Char.IsLower

    /// Turns a string into a nice PascalCase identifier
    let niceName (s:string) = 
        if s = s.ToUpper() then s else
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

    /// Checks whether the string is a boolean value
    let isBool (s:string) =
        let l = s.ToLower()
        l = "true" || l = "false" || l = "yes" || l = "no"

    /// Checks whether the string is an int32
    let isInt (s:string) = Int32.TryParse s |> fst

    /// Checks whether the string is an int64
    let isInt64 (s:string) = Int64.TryParse s |> fst

    /// Checks whether the string is a float
    let isFloat (s:string) =
          Double.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture) 
            |> fst

    /// Checks whether all values of the sequence can be inferred to a special type
    let inferType values =     
        if Seq.forall isBool values then typeof<bool>
        elif Seq.forall isInt values then typeof<int>
        elif Seq.forall isInt64 values then typeof<int64>
        elif Seq.forall isFloat values then typeof<float>
        else typeof<string>