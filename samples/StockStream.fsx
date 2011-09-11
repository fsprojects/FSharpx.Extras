// ----------------------------------------------------------------------------
// F# async extensions (StockStream.fsx)
// (c) Tomas Petricek, 2011, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------


// This example shows how to download stock prices from Yahooo and
// return the result as an asynchronous sequence. The code that returns
// the sequence downloads data from Yahoo in buffers and only downloads
// the next buffer if it is required by the client (if it continues
// iterating over the sequence).

// Also available at: http://fssnip.net/7X

#r "..\\bin\\FSharp.AsyncExtensions.dll"

open System
open System.Net
open System.Text
open FSharp.IO
open FSharp.Control
open Microsoft.FSharp.Control.WebExtensions

// ----------------------------------------------------------------------------
// Asynchronously downloading file from the web line-by-line 

/// Asynchronously download lines of a specified file
/// (content is decuded using ASCII encoding)
let downloadLines (url:string) = asyncSeq {
  // Create HTTP request and get response asynchronously
  let req = HttpWebRequest.Create(url)
  let! resp = req.AsyncGetResponse()
  let stream = resp.GetResponseStream()

  let str = ref ""
  // Download content in 1kB buffers 
  for buffer in stream.AsyncReadSeq(1024) do
    // Decode buffer using ASCII and add to remaining text
    str := str.Value + String(Encoding.ASCII.GetChars(buffer)) + " "

    // Yield all lines except for the (incomplete) last one
    let parts = str.Value.Split([|'\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries)
    for i in 0 .. parts.Length - 2 do
      yield parts.[i]

    // Save the unprocessed rest of text for the next iteration
    let rest = parts.[parts.Length - 1]
    str := rest.Substring(0, rest.Length - 1)

  // Yield the last line if it is not empty
  if str.Value <> "" then yield str.Value }

// ----------------------------------------------------------------------------
// Getting stock prices from Yahoo

// Yahoo URL with historical stock prices
let ystock = "http://ichart.finance.yahoo.com/table.csv?s="

// Download data for MSFT and skip the header line 
downloadLines (ystock + "MSFT")
|> AsyncSeq.skip 1
|> AsyncSeq.map (fun line ->
     // Split line into Open, High, Low, Close values
     let infos = line.Split(',')
     float infos.[1], float infos.[2], float infos.[3], float infos.[4])
// Take first 30 values and start printing asynchronously
|> AsyncSeq.take 30
|> AsyncSeq.iter (printfn "%A")
|> Async.Start

// ----------------------------------------------------------------------------
// Wrap inside a reusable function

/// Reusable function that downloads parsed stock prices
let downloadStockPrices stock =
  downloadLines (ystock + stock)
  |> AsyncSeq.skip 1
  |> AsyncSeq.map (fun line ->
     // Split line into Open, High, Low, Close values
     let infos = line.Split(',')
     DateTime.Parse(infos.[0]),
     (float infos.[1], float infos.[2], float infos.[3], float infos.[4]))
