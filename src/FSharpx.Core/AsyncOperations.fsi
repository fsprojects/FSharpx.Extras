// First version copied from the F# Power Pack 
// https://raw.github.com/fsharp/powerpack/master/src/FSharp.PowerPack/AsyncOperations.fsi

// (c) Microsoft Corporation 2005-2009.
namespace FSharpx.Control

    open System
    open Microsoft.FSharp.Control
    
    /// Represents the reified result of an asynchronous computation
    [<NoEquality; NoComparison>]
    type AsyncResult<'T>  =
        | AsyncOk of 'T
        | AsyncException of exn
        | AsyncCanceled of OperationCanceledException

        /// Create an async whose result depends on the value of an AsyncResult.
        static member Commit : AsyncResult<'T> -> Async<'T>

    [<Sealed>]
    /// A helper type to store a single result from an asynchronous computation and asynchronously
    /// access its result.
    type AsyncResultCell<'T> =
        /// Record the result in the AsyncResultCell. Subsequent sets of the result are ignored. 
        ///
        /// This may result in the scheduled resumption of a waiting asynchronous operation  
        member RegisterResult:AsyncResult<'T> * ?reuseThread:bool -> unit

        /// Wait for the result and commit it
        member AsyncResult : Async<'T>
        /// Create a new result cell
        new : unit -> AsyncResultCell<'T>
        
    [<AutoOpen>]
    module StreamReaderExtensions =
        open System.IO

        type System.IO.StreamReader with 
            /// Return an asynchronous computation that will read to the end of a stream via a fresh I/O thread.
            member AsyncReadToEnd: unit -> Async<string>

    [<AutoOpen>]
    module FileExtensions =
        open System.IO
        type System.IO.File with 
            /// Create an async that opens an existing file for reading, via a fresh I/O thread.
            static member AsyncOpenText: path:string -> Async<StreamReader>

            /// Create an async that opens a <c>System.IO.FileStream</c> on the specified path for read/write access, via a fresh I/O thread.
            static member AsyncOpenRead: path:string -> Async<FileStream>

            /// Create an async that opens an existing file writing, via a fresh I/O thread.
            static member AsyncOpenWrite: path:string -> Async<FileStream>

            /// Create an async that returns a <c>System.IO.StreamWriter</c> that appends UTF-8 text to an existing file, via a fresh I/O thread.
            static member AsyncAppendText: path:string -> Async<StreamWriter>

            /// Create an async that opens a <c>System.IO.FileStream</c> on the specified path, via a fresh I/O thread.
            /// Pass <c>options=FileOptions.Asynchronous</c> to enable further asynchronous read/write operations
            /// on the FileStream.
#if FX_NO_FILE_OPTIONS
            static member AsyncOpen: path:string * mode:FileMode * ?access: FileAccess * ?share: FileShare * ?bufferSize: int -> Async<FileStream>
#else
            static member AsyncOpen: path:string * mode:FileMode * ?access: FileAccess * ?share: FileShare * ?bufferSize: int * ?options: FileOptions -> Async<FileStream>

            // Aims to take advantage of IO completion ports using FileStream.AsyncWrite and FileOptions.Asynchronous, so no FX_NO_FILE_OPTIONS version
            static member AsyncWriteAllBytes: path:string * bytes:byte array -> Async<unit>
            static member AsyncWriteAllText: path:string * txt:string * ?encoder:System.Text.Encoding -> Async<unit>
            static member AsyncWriteAllLines: path:string * lines:string array * ?encoder:System.Text.Encoding -> Async<unit>
            static member AsyncAppendAllBytes: path:string * bytes:byte array -> Async<unit>
            static member AsyncAppendAllText: path:string * txt:string * ?encoder:System.Text.Encoding -> Async<unit>
            static member AsyncAppendAllLines: path:string * lines:string array * ?encoder:System.Text.Encoding -> Async<unit>

#endif

#if FX_NO_WEB_REQUESTS
#else
    [<AutoOpen>]
    module WebRequestExtensions =
        type System.Net.WebRequest with 
            /// Return an asynchronous computation that, when run, will wait for a response to the given WebRequest.
            [<System.Obsolete("The extension method now resides in the 'WebExtensions' module in the F# core library. Please add 'open Microsoft.FSharp.Control.WebExtensions' to access this method")>]
            member AsyncGetResponse : unit -> Async<System.Net.WebResponse>
#endif
    
#if FX_NO_WEB_CLIENT
#else
    [<AutoOpen>]
    module WebClientExtensions =
        type System.Net.WebClient with
            [<System.Obsolete("The extension method now resides in the 'WebExtensions' module in the F# core library. Please add 'open Microsoft.FSharp.Control.WebExtensions' to access this method")>]
            member AsyncDownloadString : address:System.Uri -> Async<string>

            /// Returns an asynchronous computation that, when run, will wait for the upload of the data in the specified name/value collection to the resource identified by the specified URI.
            member AsyncUploadValues : address: System.Uri * data: Collections.Specialized.NameValueCollection -> Async<byte[]>
            /// Returns an asynchronous computation that, when run, will wait for the upload of the data in the specified name/value collection to the resource identified by the specified URI, using the specified method.
            member AsyncUploadValues : address: System.Uri * uploadMethod: string * data: Collections.Specialized.NameValueCollection -> Async<byte[]>

            /// Returns an asynchronous computation that, when run, will wait for the upload of the specified string to the specified resource.
            member AsyncUploadString : address: System.Uri * data: string -> Async<string>
            /// Returns an asynchronous computation that, when run, will wait for the upload of the specified string to the specified resource, using the specified method.
            member AsyncUploadString : address: System.Uri * uploadMethod: string * data: string -> Async<string>

            /// Returns an asynchronous computation that, when run, will wait for the upload of the specified local file to the specified resource, using the POST method.
            member AsyncUploadFile : address: System.Uri * fileName: string -> Async<byte[]>
            /// Returns an asynchronous computation that, when run, will wait for the upload of the specified local file to the specified resource, using the specified method.
            member AsyncUploadFile : address: System.Uri * uploadMethod: string * fileName: string -> Async<byte[]>

            /// Returns an asynchronous computation that, when run, will wait for the upload of a data buffer to a resource identified by a URI, using the POST method.
            member AsyncUploadData : address: System.Uri * data: byte[] -> Async<byte[]>
            /// Returns an asynchronous computation that, when run, will wait for the upload of a data buffer to a resource identified by a URI, using the specified method.
            member AsyncUploadData : address: System.Uri * uploadMethod: string * data: byte[] -> Async<byte[]>

            /// Returns an asynchronous computation that, when run, will wait for the opening of a stream for writing data to the specified resource.
            member AsyncOpenWrite : address: System.Uri -> Async<IO.Stream>
            /// Returns an asynchronous computation that, when run, will wait for the opening of a stream for writing data to the specified resource.
            member AsyncOpenWrite : address: System.Uri * uploadMethod: string -> Async<IO.Stream>

            /// Returns an asynchronous computation that, when run, will wait for the opening of a readable stream containing the specified resource.
            member AsyncOpenRead : address: System.Uri -> Async<IO.Stream>

            /// Returns an asynchronous computation that, when run, will wait for the download of a resource with the specified URI to the local file.
            member AsyncDownloadFile : address: System.Uri * fileName: string -> Async<unit>

            /// Returns an asynchronous computation that, when run, will wait for the download of the specified resource as a data buffer.
            member AsyncDownloadData : address: System.Uri -> Async<byte[]>
#endif
