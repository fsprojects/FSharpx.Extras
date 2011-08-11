// ----------------------------------------------------------------------------
// F# async extensions (AutoCancel.fsx)
// (c) Tomas Petricek, 2011, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------

// This example demonstrates how to implement simple HTTP proxy

#r "..\\bin\\FSharp.AsyncExtensions.dll"
open FSharp.Control
open FSharp.IO
open FSharp.Net

open System
open System.Net
open System.Threading

let root = "http://msdn.microsoft.com"
let proxy = "http://localhost:8082/"

// ----------------------------------------------------------------------------
// Simple web proxy implemented using 'HttpListener'. This version downloads
// the entire web page as a string and then writes it to the response stream.

let cts1 = new CancellationTokenSource()
HttpListener.Start(proxy, (fun (req, resp) -> async {
    // Download the web page
    let url = root + req.Url.PathAndQuery
    let wc = new WebClient()
    let! html = wc.AsyncDownloadString(Uri(url))

    // Replace URLs and send to the response stream
    let html = html.Replace(root, proxy)
    do! resp.AsyncReply(html) }), cancellationToken = cts1.Token)

// Now go to: http://localhost:8082/en-us/fsharp
cts1.Cancel()

// ----------------------------------------------------------------------------
// Better version of a proxy - this time, we read data from the input stream
// in chunks and write them to the response stream as they arive.

let cts2 = new CancellationTokenSource()
HttpListener.Start(proxy, (fun (req, resp) -> async {
    // Initialize the download
    let url = root + req.Url.PathAndQuery
    let targetReq = HttpWebRequest.Create(url)
    use! targetResp = targetReq.AsyncGetResponse()
    use stream = targetResp.GetResponseStream()
  
    // Copy data until we read the entire input
    let count = ref 1
    let buffer = Array.zeroCreate 4096
    while !count > 0 do
      let! read = stream.AsyncRead(buffer, 0, buffer.Length)
      do! resp.OutputStream.AsyncWrite(buffer, 0, read)    
      count := read
    resp.Close() }), cancellationToken = cts2.Token)

cts2.Cancel()

// ----------------------------------------------------------------------------
// Proxy that copies data in chunks can be easily implemented using 
// asynchronous sequences. We read all data as asynchronous sequence and
// write them to the output (Even simpler version could use 'AsyncWriteSeq'
// to write all input buffers to the output stream).

let cts3 = new CancellationTokenSource()
HttpListener.Start(proxy, (fun (req, resp) -> async {
    // Initialize the download
    let url = root + req.Url.PathAndQuery
    let targetReq = HttpWebRequest.Create(url)
    use! targetResp = targetReq.AsyncGetResponse()
    use stream = targetResp.GetResponseStream()
  
    // Iterate over chunks read as an asynchronous sequence
    // and write them to the output stream
    for buffer in stream.AsyncReadSeq(4096) do
      do! resp.OutputStream.AsyncWrite(buffer, 0, buffer.Length)
    resp.Close() }), cancellationToken = cts3.Token)

cts3.Cancel()

// ----------------------------------------------------------------------------
// A more sophisticated version of proxy that caches web 
// pages using a simple agent.

type CacheMessage =
  | TryGet of string * AsyncReplyChannel<option<byte[]>>
  | Add of string * byte[]

// Creates an agent that handles 'CacheMessage' and implements the cache
let cache = Agent.Start(fun agent -> async {
  let pages = new System.Collections.Generic.Dictionary<_, _>()
  while true do
    let! msg = agent.Receive()
    match msg with 
    | TryGet(url, repl) ->
        // Try to get a value from the dictionary
        match pages.TryGetValue(url) with
        | true, data -> repl.Reply(Some(data))
        | _ -> repl.Reply(None)
    | Add(url, data) ->
        // Add byte array to the cache
        pages.[url] <- data })


let cts4 = new CancellationTokenSource()
HttpListener.Start(proxy, (fun (req, resp) -> async {
    // Generate URL and check data from the cache
    let url = root + req.Url.PathAndQuery
    let! cached = cache.PostAndAsyncReply(fun repl -> TryGet(url, repl))
    match cached with 
    | Some data ->
        // Reply using data from the cache
        do! resp.OutputStream.AsyncWrite(data)
        resp.Close() 
    | None ->
        // Initialize the download
        let targetReq = HttpWebRequest.Create(url)
        use! targetResp = targetReq.AsyncGetResponse()
        use stream = targetResp.GetResponseStream()
  
        // Create a cached asynchronous sequence 
        // (that reads the stream only once)
        let cachedData = stream.AsyncReadSeq(4096) |> AsyncSeq.cache

        // Start workflow that reads all data in memory (for caching)
        let! allBytes = 
          cachedData 
          |> AsyncSeq.fold (fun st data -> data::st) []
          |> Async.StartChild
        // Write all data from the async sequence to the output
        for buffer in cachedData do
          do! resp.OutputStream.AsyncWrite(buffer, 0, buffer.Length)
        resp.Close() 

        // Get all data accumulated in background and save
        // them to the cache (for later use)
        let! allData = allBytes
        let data = allData |> List.rev |> Array.concat 
        cache.Post(Add(url, data)) }), cts4.Token)

cts4.Cancel()
