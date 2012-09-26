// Copyright (c) Microsoft Corporation 2005-2012.
// This sample code is provided "as is" without warranty of any kind. 
// We disclaim all warranties, either express or implied, including the 
// warranties of merchantability and fitness for a particular purpose. 

namespace Utilities.Caching

type internal ICache<'T> = 
    abstract TryRetrieve : string -> 'T option
    abstract Set : string * 'T -> unit

module Cache =
    val CreateCache : unit -> ICache<'T>
    val CreateNonCachingCache : unit -> ICache<'T>

type internal RestDownloadCache =
    new : string -> RestDownloadCache
    member CacheLocation : string
    interface ICache<string>