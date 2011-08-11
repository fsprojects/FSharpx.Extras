// ----------------------------------------------------------------------------
// F# async extensions (IO.fs)
// (c) Tomas Petricek, 2011, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------

namespace FSharp.IO
open FSharp.Control
// ----------------------------------------------------------------------------
// Extensions that simplify working with Stream using async sequences

[<AutoOpen>]
module IOExtensions = 
  type System.IO.Stream with
    /// Asynchronously reads the stream in chunks of a specified size
    /// and returns the result as an asynchronous sequence.
    member x.AsyncReadSeq(?bufferSize) = 
      let bufferSize = defaultArg bufferSize 1024
      let buffer = Array.zeroCreate bufferSize
      let rec loop () = asyncSeq {
        let! count = x.AsyncRead(buffer, 0, bufferSize)
        if count > 0 then 
          yield Array.sub buffer 0 count
          yield! loop() }
      loop ()

    /// Asynchronously writes all data specified by the 
    /// given asynchronous sequence to the stream.
    member x.AsyncWriteSeq(input : AsyncSeq<byte[]>) = async {
      for data in input do
        do! x.AsyncWrite(data) }

// ----------------------------------------------------------------------------
// Extensions that simplify working with HttpListener and related types

namespace FSharp.Net
open System.IO
open System.Net
open System.Text
open System.Threading

open FSharp.IO
open FSharp.Control

[<AutoOpen>]
module HttpExtensions = 

  type System.Net.HttpListener with
    /// Asynchronously waits for an incoming request and returns it.
    member x.AsyncGetContext() = 
      Async.FromBeginEnd(x.BeginGetContext, x.EndGetContext)

    /// Starts HttpListener on the specified URL. The 'handler' function is
    /// called (in a new thread pool thread) each time an HTTP request is received.
    static member Start(url, handler, ?cancellationToken) =
      let server = async { 
        use listener = new HttpListener()
        listener.Prefixes.Add(url)
        listener.Start()
        while true do 
          let! context = listener.AsyncGetContext()
          Async.Start
            ( handler (context.Request, context.Response), 
              ?cancellationToken = cancellationToken) }
      Async.Start(server, ?cancellationToken = cancellationToken)

  type System.Net.HttpListenerRequest with
    /// Asynchronously reads the 'InputStream' of the request and converts it to a string
    member request.AsyncInputString = async {
      use tmp = new MemoryStream()
      for data in request.InputStream.AsyncReadSeq(16 * 1024) do
        tmp.Write(data, 0, data.Length) 
      tmp.Seek(0L, SeekOrigin.Begin) |> ignore
      use sr = new StreamReader(tmp)
      return sr.ReadToEnd() }


  type System.Net.HttpListenerResponse with
    /// Sends the specified string as a reply in UTF 8 encoding
    member response.AsyncReply(s:string) = async {
      let buffer = Encoding.UTF8.GetBytes(s)
      response.ContentLength64 <- int64 buffer.Length
      let output = response.OutputStream
      do! output.AsyncWrite(buffer,0,buffer.Length)
      output.Close() }

    /// Sends the specified data as a reply with the specified content type
    member response.AsyncReply(typ, buffer:byte[]) = async {
      response.ContentLength64 <- int64 buffer.Length
      let output = response.OutputStream
      response.ContentType <- typ
      do! output.AsyncWrite(buffer,0,buffer.Length)
      output.Close() }