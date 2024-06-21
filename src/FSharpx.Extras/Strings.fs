namespace FSharpx.Text

open System
open System.Globalization

module Strings =
    /// Checks whether the given text starts with the given prefix
    [<CompiledName("StartsWith")>]
    let inline startsWith (prefix : string) (text:string) = text.StartsWith prefix

    /// Replaces the given "replacement" for every occurence of the pattern in the given text 
    [<CompiledName("Replace")>]
    let inline replace (pattern:string) replacement (text:string) = text.Replace(pattern,replacement)

    /// Returns a value indicating whether the specified substring occurs within this string
    [<CompiledName("Contains")>]
    let inline contains substr (t: string) = t.Contains substr
    
    /// Splits the given string at the given delimiter
    [<CompiledName("Split")>]
    let inline split (delimiter:char) (text:string) = text.Split [|delimiter|]

    [<CompiledName("ToCharArray")>]
    let inline toCharArray (str:string) = str.ToCharArray()

    [<CompiledName("IsNewline")>]
    let isNewline c = c = '\r' || c = '\n'
            
    /// Returns a sequence of strings split by the predicate    
    [<CompiledName("SplitBy")>]
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
    [<CompiledName("ToLines")>]
    let toLines (input:string) : string seq = splitBy isNewline input
        
    /// Creates newline seperated string from the string list
    [<CompiledName("JoinLines")>]
    let joinLines (input:string list) : string = (String.concat System.Environment.NewLine input).Trim()

    /// Splits a string based on whitespace (spaces, tabs, and newlines)
    [<CompiledName("ToWords")>]
    let toWords (input:string) : string seq = splitBy Char.IsWhiteSpace input

    /// Folds the string list by seperating entries with a single space
    [<CompiledName("JoinWords")>]
    let joinWords (input: string list) : string = (String.concat " " input).Trim()

    /// Returns if the string is null or empty
    [<CompiledName("IsNullOrEmpty")>]
    let inline isNullOrEmpty text = String.IsNullOrEmpty text

    /// Returns the pluralized version of a noun
    [<CompiledName("Pluralize")>]
    let pluralize (noun: string) =        
        if noun.Contains " of " || noun.Contains " Of " then 
            noun
        else
            Pluralizer.toPlural noun

    /// Returns the singularized version of a noun
    [<CompiledName("Singularize")>]
    let singularize (noun: string) =
        Pluralizer.toSingular noun

    // Active patterns & operators for parsing strings
    let (@?) (s:string) i = if i >= s.Length then ValueNone else ValueSome s.[i]

    let inline satisfies predicate (charOption:voption<char>) = 
        match charOption with 
        | ValueSome c when predicate c -> charOption 
        | _ -> ValueNone

    [<return: Struct>]
    let (|EOF|_|) = function 
        | ValueSome _ -> ValueNone
        | _ -> ValueSome ()
    [<return: Struct>]
    let (|LetterDigit|_|) = satisfies Char.IsLetterOrDigit
    [<return: Struct>]
    let (|Upper|_|) = satisfies Char.IsUpper
    [<return: Struct>]
    let (|Lower|_|) = satisfies Char.IsLower

    /// Turns a string into a nice PascalCase identifier
    [<CompiledName("NiceName")>]
    let niceName (s:string) = 
        if s = s.ToUpper() then s else
        // Starting to parse a new segment 
        let rec restart i = 
          match s @? i with 
          | EOF -> Seq.empty
          | LetterDigit _ & Upper _ -> upperStart i (i + 1)
          | LetterDigit _ -> consume i false (i + 1)
          | _ -> restart (i + 1)

        // Parsed first upper case letter, continue either all lower or all upper
        and upperStart from i = 
          match s @? i with 
          | Upper _ -> consume from true (i + 1) 
          | Lower _ -> consume from false (i + 1) 
          | _ -> restart (i + 1)
        // Consume are letters of the same kind (either all lower or all upper)
        and consume from takeUpper i = 
          match s @? i with
          | Lower _ when not takeUpper -> consume from takeUpper (i + 1)
          | Upper _ when takeUpper -> consume from takeUpper (i + 1)
          | _ -> 
            let r1 = struct(from, i) 
            let r2 = restart i
            seq {
              yield r1
              yield! r2 }
    
        // Split string into segments and turn them to PascalCase
        seq { for i1, i2 in restart 0 do 
                let sub = s.Substring(i1, i2 - i1) 
                if Seq.forall Char.IsLetterOrDigit sub then
                  yield sub.[0].ToString().ToUpper() + sub.ToLower().Substring(1) }
        |> String.concat ""

    /// Checks whether the string is a boolean value
    [<CompiledName("IsBool")>]
    let isBool (s:string) =
        let l = s.ToLower()
        l = "true" || l = "false" || l = "yes" || l = "no"

    /// Checks whether the string is an int32
    [<CompiledName("IsInt")>]
    let isInt (s:string) = Int32.TryParse s |> fst

    /// Checks whether the string is an int64
    [<CompiledName("IsInt64")>]
    let isInt64 (s:string) = Int64.TryParse s |> fst

    /// Checks whether the string is a float
    [<CompiledName("IsFloat")>]
    let isFloat (s:string) =
          Double.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture) 
            |> fst

    /// Checks whether all values of the sequence can be inferred to a special type
    [<CompiledName("InferType")>]
    let inferType values =     
        if Seq.forall isBool values then typeof<bool>
        elif Seq.forall isInt values then typeof<int>
        elif Seq.forall isInt64 values then typeof<int64>
        elif Seq.forall isFloat values then typeof<float>
        else typeof<string>