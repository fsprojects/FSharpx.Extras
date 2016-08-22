#### 2.0.0 - 22.08.2016
* BREAKING: Deprecated .NET 4.0 version
* BREAKING: Removed all functions that were marked as obsolete. This includes JSON API

### 1.10.3 
* Added undefined in Prelude - https://github.com/fsprojects/FSharpx.Extras/pull/332

### 1.10.2
* List dependencies properly in nuget package

### 1.10.1
* Fix dependency in nuget package
* Fix #332

### 1.10.0 - Renamed to FSharpx.Extras
* Renamed to FSharpx.Core to FSharpx.Extras

### 1.9.4 - Remove Obsolete method
* Remove the WebClient extension method that has long ago been moved to FSharp.Core

### 1.9.3 - Cleanup namespaces
* FSharpx.* types and operators are now in FSharpx
* FSharpx.Regex are now in FSharpx.Text.Regex
* FSharpx.Strings is now in FSharpx.Text.Strings
* Microsoft.FSharp.Control has been renamed to FSharpx.Control

### 1.9.2 - Remove components now available elsewhere
* FSharpx.JSON support now marked as deprecated in favour of FSharp.Data
* FSharpx.Linq.QuotationEvaluator now marked as deprecated in favour of FSharp.Quotations.Evaluator 

### 1.9.1 - Remove type providers from FSharpx
* All type providers now removed from FSharpx

### 1.9.0 - Remove FSharpx.Collections
* FSharpx.Core now depends on FSharpx.Collections
* Generated docs now available


