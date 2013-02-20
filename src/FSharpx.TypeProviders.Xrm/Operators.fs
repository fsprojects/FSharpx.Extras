// Copyright (c) Microsoft Corporation 2005-2013.
// This sample code is provided "as is" without warranty of any kind. 
// We disclaim all warranties, either express or implied, including the 
// warranties of merchantability and fitness for a particular purpose.

namespace Samples.XrmProvider

open System.Linq
// Dummy operators, these are placeholders that are replaced in the expression tree traversal with special server-side operations such as In, Like
// The operators here are used to force the compiler to statically check against the correct types
[<AutoOpenAttribute>]
module Operators =
    /// In
    let (|=|) (a:'a) (b:'a seq) = false
    // Not In
    let (|<>|) (a:'a) (b:'a seq) = false
    // Like
    let (=%) (a:'a) (b:string) = false
    // Not Like
    let (<>%) (a:'a) (b:string) = false
    // Left join
    let (!!) (a:IQueryable<_>) = a