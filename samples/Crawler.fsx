// ----------------------------------------------------------------------------
// F# async extensions (Crawler.fsx)
// (c) Tomas Petricek, 2011, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------

// This example demonstrates how to use asynchronous sequences and
// blocking agents to implement a web crawler. The sample also uses
// various AsyncSeq combinators to process the resulting async sequence.
//
// The first version performs single-threaded random walk (returned 
// as an asynchronous sequence) and the second version is concurrent.

#r "..\\bin\\FSharp.AsyncExtensions.dll"
#r "..\\bin\\HtmlAgilityPack.dll"

open System
open System.Net
open System.Text.RegularExpressions
open HtmlAgilityPack

open FSharp.Control

// ----------------------------------------------------------------------------
// Helper functions for downloading documents, extracting links etc.

/// Asynchronously download the document and parse the HTML
let downloadDocument url = async {
  try let wc = new WebClient()
      let! html = wc.AsyncDownloadString(Uri(url))
      let doc = new HtmlDocument()
      doc.LoadHtml(html)
      return Some doc 
  with _ -> return None }

/// Extract all links from the document that start with "http://"
let extractLinks (doc:HtmlDocument) = 
  try
    [ for a in doc.DocumentNode.SelectNodes("//a") do
        if a.Attributes.Contains("href") then
          let href = a.Attributes.["href"].Value
          if href.StartsWith("http://") then 
            let endl = href.IndexOf('?')
            yield if endl > 0 then href.Substring(0, endl) else href ]
  with _ -> []

/// Extract the <title> of the web page
let getTitle (doc:HtmlDocument) =
  let title = doc.DocumentNode.SelectSingleNode("//title")
  if title <> null then title.InnerText.Trim() else "Untitled"

// ----------------------------------------------------------------------------
// Basic crawling - crawl web pages and follow just one link from every page

/// Crawl the internet starting from the specified page
/// From each page follow the first not-yet-visited page
let rec randomCrawl url = 
  let visited = new System.Collections.Generic.HashSet<_>()

  // Visits page and then recursively visits all referenced pages
  let rec loop url = asyncSeq {
    if visited.Add(url) then
      let! doc = downloadDocument url
      match doc with 
      | Some doc ->
          // Yield url and title as the next element
          yield url, getTitle doc
          // For every link, yield all referenced pages too
          for link in extractLinks doc do
            yield! loop link 
      | _ -> () }
  loop url

// Use AsyncSeq combinators to print the titles of the first 10
// web sites that are from other domains than bing.com
randomCrawl "http://news.bing.com"
|> AsyncSeq.filter (fun (url, title) -> url.Contains("bing.com") |> not)
|> AsyncSeq.map snd
|> AsyncSeq.take 10
|> AsyncSeq.iter (printfn "%s")
|> Async.Start


// ----------------------------------------------------------------------------
// Better crawler - crawls the web concurrently using the specified number of
// workers, stores results and pending URLS to blocking buffers and returns
// all results as an asynchronous sequence. After caller stops taking elements
// from the asynchronous sequence, the blocking buffers will eventually fill
// up and crawling will stop. 

let concurrentWorkers = 20

let rec concurrentCrawl url = asyncSeq {
  // Number of pending requests is usually very high 
  // (when the queue fills up, the workers will stop, so set this to 10k)
  let requests = BlockingQueueAgent<_>(10000)
  let results = BlockingQueueAgent<_>(40)
  let visited = ConcurrentSetAgent<_>()

  /// Worker repeatedly takes URL from the queue and processes it
  let worker() = async {
    while true do
      let! url = requests.AsyncGet()
      let! doc = downloadDocument url
      match doc with 
      | Some doc ->
          // Yield url and title as the next element
          do! results.AsyncAdd( (url, getTitle doc) )
          // For every link, yield all referenced pages too
          for link in extractLinks doc do
            let! added = visited.AsyncAdd(link)
            if added then
              do! requests.AsyncAdd(link) 
      | _ -> () }

  // Return an asynchronous sequence that sends intial request
  // to the crawler, starts workers and then repeatedly takes
  // results from the results queue.
  do! requests.AsyncAdd(url)
  for i in 0 .. concurrentWorkers do 
    worker () |> Async.Start
  while true do
    let! res = results.AsyncGet()
    yield res }

// ----------------------------------------------------------------------------
// Visualize the results of crawling - show the most common words in titles

// Create user interface with text box for displaying words
open System.Windows.Forms
let frm = new Form(TopMost=true, Visible=true, Width=400, Height=600)
let txt = new TextBox( Multiline = true, Dock = DockStyle.Fill, 
                       Font = new System.Drawing.Font("Cambria", 12.0f),
                       ScrollBars = ScrollBars.Vertical )
frm.Controls.Add(txt)

// Creates an asynchronous sequence that produces values of type 
// Map<string, int> representing words together with their count
// (new version returned after every processing step)
let tables = 
  concurrentCrawl "http://news.bing.com"
  // Split title into lowercase words 
  |> AsyncSeq.map (fun (_, title) -> 
        title.Split( [|' '; '.'; '-'; '|'; ','; ';' |], 
                     StringSplitOptions.RemoveEmptyEntries )
        |> Array.map (fun s -> s.ToLower()) )
  // Create sequence that aggregates words and returns immediate results
  |> AsyncSeq.scan (fun table words ->
      words |> Seq.fold (fun table word ->
        match Map.tryFind word table with
        | Some v -> Map.add word (v + 1) table
        | _ -> Map.add word 1 table) table) Map.empty

// Asynchronous workflow that iterates over the sequence
// and displays the results in the textbox
async { 
  let counter = ref 0
  for table in tables |> AsyncSeq.take 200 do
    frm.Text <- sprintf "Processed %d" (counter := !counter + 1; !counter)
    txt.Text <-
      table 
      |> Seq.sortBy (fun (KeyValue(k, v)) -> -v)
      |> Seq.map (fun (KeyValue(k, v)) -> sprintf "%s (%d)" k v)
      |> String.concat "\r\n" } 
|> Async.StartImmediate
