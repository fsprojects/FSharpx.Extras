// Copyright (c) Microsoft Corporation 2005-2012.
// This sample code is provided "as is" without warranty of any kind. 
// We disclaim all warranties, either express or implied, including the 
// warranties of merchantability and fitness for a particular purpose. 

namespace Utilities.Caching
open System
open System.IO
open System.Collections.Generic
open System.Reflection
open System.Text

type ICache<'T> = 
    abstract TryRetrieve : string -> 'T option
    abstract Set : string * 'T -> unit

module Cache = 
            
    let CreateNonCachingCache() = 
        {new ICache<'T> with
            override __.TryRetrieve(key) = None
            override __.Set(key,value) = () }   
                
       
    let CreateCache() = 
        let cache = Dictionary<_,_>()
        {new ICache<'T> with
            override __.TryRetrieve(key) = 
                match cache.TryGetValue(key) with
                | false,_ -> None
                | true,result -> Some(result)
            override __.Set(key,value) = 
                cache.[key]<-value }   
                
       
#if FX_NO_LOCAL_FILESYSTEM
type RestDownloadCache(_prefix:string) =
    let c = Cache.CreateCache()

    member __.CacheLocation : string = failwith "Invalid operation"

    interface ICache<string> with
        member __.TryRetrieve(key) = c.TryRetrieve(key)
        member __.Set(key,value) = c.Set(key,value)

#else
open System.Security.Cryptography
type RestDownloadCache(prefix) =
    let syncObj = obj()
    // e.g. C:\Users\dsyme\AppData\Local\Microsoft\Windows\Temporary Internet Files
    let cacheFolder = System.Environment.GetFolderPath(System.Environment.SpecialFolder.InternetCache)
    let downloadCache = Path.Combine(cacheFolder,prefix)
    do
        try
            if not(Directory.Exists(downloadCache)) then
                Directory.CreateDirectory(downloadCache) |> ignore
        with e-> assert false //"Could not create cache directory"

    let sha1OfString (plainText:string) = 
        let plainTextBytes = Encoding.UTF8.GetBytes(plainText)

        let hash = new SHA1Managed()
        let hashBytes = hash.ComputeHash(plainTextBytes);
        
        let s = Convert.ToBase64String(hashBytes)
        s.Replace("ab","abab").Replace("\\","ab")
        
    let cacheFileOfQuery query = 
        let sha1 = sha1OfString query
        let encoded = System.Uri.EscapeDataString sha1
        Path.Combine(downloadCache,encoded+".txt")

    // A simple check for now. This is to guard against a corrupted cache file.
    let isWellFormedResult result = not (String.IsNullOrEmpty result)
        
    member fr.CacheLocation = downloadCache         
            

    interface ICache<string> with 
        override __.TryRetrieve(key) = 
          lock syncObj (fun () -> 
            let cacheFile = cacheFileOfQuery key
            try 
                if File.Exists cacheFile then
                    let result = File.ReadAllText cacheFile
                    if isWellFormedResult result then Some result
                    else None
                else 
                    None
             with e -> 
                System.Diagnostics.Debug.Assert(false, sprintf "Failed to read from download cache file %s" cacheFile)
                None)

        override __.Set(key,value) = 
          lock syncObj (fun () -> 
            let mutable cacheFile = String.Empty
            try
                cacheFile <- cacheFileOfQuery key
                File.WriteAllText(cacheFile,value)
            with e->
                System.Diagnostics.Debug.Assert(false, sprintf "Failed to write download cache file to %s %s" cacheFile e.Message))
#endif
