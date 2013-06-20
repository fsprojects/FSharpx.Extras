namespace FSharpx.PowerPack.Unittests
open NUnit.Framework
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Text.StructuredFormat

module StructuredFormat_MiscBitsAndPiecesToCompiler = 
    let opts = { FormatOptions.Default with 
                   PrintWidth=30; 
                   ShowIEnumerable=true;
                   ShowProperties=true }
