### 3.3.0 - 22.02.2025
* Switch `Validation` to `Result` and mark `Choice` based `Validation` as obsolete - https://github.com/fsprojects/FSharpx.Extras/pull/440
* Test on net9 and update infra - https://github.com/fsprojects/FSharpx.Extras/pull/441

### 3.2.1 - 27.07.2024
* Fix FSharpx.Extras v3.2.0 breaks compilation - https://github.com/fsprojects/FSharpx.Extras/issues/436

### 3.2.0 - 09.07.2024
* Remove net452 support and update test targets
* Faster and less-memory-consuming niceName - https://github.com/fsprojects/FSharpx.Extras/pull/434

### 3.1.0 - 10.03.2022
* Restore Task functions by reintroducing Task computation expression as internal component. - https://github.com/fsprojects/FSharpx.Extras/pull/409

### 3.0.0 - 27.02.2021
* Add using and loops for `EitherBuilder` and `ResultBuilder` - https://github.com/fsprojects/FSharpx.Extras/pull/394
* Allow not null implementation of `IDisposable` in CEs - https://github.com/fsprojects/FSharpx.Extras/pull/395
* Add `either`, `defaultValue` and `defaultWith` to `Result` - https://github.com/fsprojects/FSharpx.Extras/pull/396
* Remove `TaskBuilder`. - https://github.com/fsprojects/FSharpx.Extras/pull/397
* Add `Enum.toString` and constrain types in `Enum` module. - https://github.com/fsprojects/FSharpx.Extras/pull/401
* Remove obsolete functions and modules. - https://github.com/fsprojects/FSharpx.Extras/pull/403
* Fix files deviating from repo's license. - https://github.com/fsprojects/FSharpx.Extras/pull/407

### 2.5.0 - 04.06.2020
* Add `Option.someIf` function - https://github.com/fsprojects/FSharpx.Extras/pull/391
* Add `Option.ofUnchecked` function - https://github.com/fsprojects/FSharpx.Extras/pull/392

### 2.4.0 - 14.05.2020
* Add `Enum.isDefined` function - https://github.com/fsprojects/FSharpx.Extras/pull/382
* Deprecate `taskbuilder` in favor of `taskbuilder.fs` - https://github.com/fsprojects/FSharpx.Extras/pull/381
* Add net452 support - https://github.com/fsprojects/FSharpx.Extras/pull/379
* Add `Validation.sequenceIgnore` and `Validation.mapMIgnore` functions - https://github.com/fsprojects/FSharpx.Extras/pull/365

### 2.3.2 - 21.10.2019
* Improve C# compatibility - https://github.com/fsprojects/FSharpx.Extras/pull/376

### 2.3.1 - 21.10.2019
* Update dependency versions for FSharpx.Async and FSharpx.Collections - https://github.com/fsprojects/FSharpx.Extras/issue/370
* Update source linking to use new, dotnet approach - https://github.com/fsprojects/FSharpx.Extras/issue/372
* Added ofOptionF to Result module - https://github.com/fsprojects/FSharpx.Extras/pull/377
 
### 2.3.0 - 4.8.2019
* New netstandard2.0 support - https://github.com/fsprojects/FSharpx.Extras/pull/360
* New split monads into separate files - https://github.com/fsprojects/FSharpx.Extras/pull/364
* New Add IsNone, IsSome and IsChoiceXOfY variants https://github.com/fsprojects/FSharpx.Extras/pull/354
* New add Option.orElseLazy - https://github.com/fsprojects/FSharpx.Extras/pull/352

### 2.2.1 - 29.01.2017
* BGUFIX: Fixed `ParallelWithThrottle` - https://github.com/fsprojects/FSharpx.Extras/pull/350

### 2.2.0 - 31.10.2016
* New getOrFailf function for Option - https://github.com/fsprojects/FSharpx.Extras/pull/345
* New getOrFail and getOrRaise functions for Option - https://github.com/fsprojects/FSharpx.Extras/pull/344
* New String helpers - https://github.com/fsprojects/FSharpx.Extras/pull/343
* BUGFIX: Fixed Try/Finally in TaskBuilder - https://github.com/fsprojects/FSharpx.Extras/pull/349

### 2.1.0 - 23.08.2016
* New Parsers in prelude - https://github.com/fsprojects/FSharpx.Extras/pull/342

### 2.0.0 - 22.08.2016
* Added Add Task related helper functions - https://github.com/fsprojects/FSharpx.Extras/pull/339
* Moved infrastructure to ProjectScaffold
* New Enum convenience methods - https://github.com/fsprojects/FSharpx.Extras/pull/327
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


