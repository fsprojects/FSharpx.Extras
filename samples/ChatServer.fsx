#r "System.Xml.Linq.dll"
//#load "Server.fs"

//open System.IO
//open System.Net
//open System.Text
//open System.Threading
open System.Xml.Linq
//open FSharp.Control

type Agent<'T> = MailboxProcessor<'T>

// ----------------------------------------------------------------------------

module First = 
  let agent = Agent.Start(fun agent -> async {
    while true do
      let! msg = agent.Receive()
      printfn "Hello %s!" msg })

  agent.Post("Tomas")

type ChatMessage = 
  | GetContent of AsyncReplyChannel<string>
  | SendMessage of string

module Second =
  let agent = Agent<_>.Start(fun agent -> 
    let rec loop messages = async {

      // Pick next message from the mailbox
      let! msg = agent.Receive()
      match msg with 
      | SendMessage msg -> 
          // Add message to the list & continue
          let msg = XElement(XName.Get("li"), msg)
          return! loop (msg :: messages)

      | GetContent reply -> 
          // Generate HTML with messages
          let html = XElement(XName.Get("ul"), messages)
          // Send it back as the reply
          reply.Reply(html.ToString())
          return! loop messages }
    loop [] )

  agent.Post(SendMessage "Welcome to F# chat implemented using agents!")
  agent.Post(SendMessage "This is my second message to this chat room...")

  agent.PostAndReply(GetContent)

// --------------------------------------------------------------------------------------

type internal ChatMessage = 
  | GetContent of AsyncReplyChannel<string>
  | SendMessage of string


type ChatRoom() = 
  let agent = Agent.Start(fun agent -> 
    let rec loop messages = async {
      // Pick next message from the mailbox
      let! msg = agent.Receive()
      match msg with 
      | SendMessage msg -> 
          // Add message to the list & continue
          let msg = XElement(XName.Get("li"), msg)
          return! loop (msg :: messages)

      | GetContent reply -> 
          // Generate HTML with messages
          let html = XElement(XName.Get("ul"), messages)
          // Send it back as the reply
          reply.Reply(html.ToString())
          return! loop messages }
    loop [] )
  member x.SendMessage(msg) = agent.Post(SendMessage msg)
  member x.AsyncGetContent(?timeout) = agent.PostAndAsyncReply(GetContent, ?timeout=timeout) 
  member x.GetContent() = agent.PostAndReply(GetContent)

  member x.GetContentAsync() = 
    Async.StartAsTask(agent.PostAndAsyncReply(GetContent))

  member x.GetContentAsync(cancellationToken) = 
    Async.StartAsTask
     ( agent.PostAndAsyncReply(GetContent), 
       cancellationToken = cancellationToken )

let room = new ChatRoom()

room.SendMessage("Welcome to F# chat implemented using agents!")
room.SendMessage("This is my second message to this chat room...")

async { 
  while true do
    do! Async.Sleep(10000)
    let! html = room.AsyncGetContent()
    printfn "%A" html }
|> Async.Start     

// --------------------------------------------------------------------------------------

open System.Net
open System.Threading

[<AutoOpen>]
module HttpExtensions = 

  type System.Net.HttpListener with
    member x.AsyncGetContext() = 
      Async.FromBeginEnd(x.BeginGetContext, x.EndGetContext)

type HttpAgent private (url, f) as this =
  let tokenSource = new CancellationTokenSource()
  let agent = Agent.Start((fun _ -> f this), tokenSource.Token)
  let server = async { 
    use listener = new HttpListener()
    listener.Prefixes.Add(url)
    listener.Start()
    while true do 
      let! context = listener.AsyncGetContext()
      agent.Post(context) }
  do Async.Start(server, cancellationToken = tokenSource.Token)

  member x.Receive(?timeout) = agent.Receive(?timeout = timeout)
  member x.Stop() = tokenSource.Cancel()
  static member Start(url, f) = 
    new HttpAgent(url, f)


open System.IO
open System.Text

[<AutoOpen>]
module HttpExtensions2 = 

  type System.Net.HttpListenerRequest with
    member request.InputString =
      use sr = new StreamReader(request.InputStream)
      sr.ReadToEnd()

  type System.Net.HttpListenerResponse with
    member response.Reply(s:string) = 
      let buffer = Encoding.UTF8.GetBytes(s)
      response.ContentLength64 <- int64 buffer.Length
      let output = response.OutputStream
      output.Write(buffer,0,buffer.Length)
      output.Close()
    member response.Reply(typ, buffer:byte[]) = 
      response.ContentLength64 <- int64 buffer.Length
      let output = response.OutputStream
      response.ContentType <- typ
      output.Write(buffer,0,buffer.Length)
      output.Close()

// --------------------------------------------------------------------------------------

open System.Threading

let server = HttpAgent.Start("http://localhost:8082/", fun server -> async {
  while true do 
    let! ctx = server.Receive()
    ctx.Response.Reply("Hello!") })
  
server.Stop()

let root = @"C:\Tomas\Writing\MSDN\code\2 Server Side\Demo.ChatServer\"

let contentTypes = 
  dict [ ".gif", "binary/image"
         ".css", "text/css"
         ".html", "text/html" 
         ".xap", "application/x-silverlight-app" ]

let server = HttpAgent.Start("http://localhost:8082/", fun mbox -> 
  let handleRequest (ctx:HttpListenerContext) = async { 
    match ctx.Request.Url.LocalPath with 
    | "/post" -> 
        // Send message to the chat room
        room.SendMessage(ctx.Request.InputString)
        ctx.Response.Reply("OK")
    | "/chat" -> 
        // Get messages from the chat room (asynchronously!)
        let! text = room.AsyncGetContent()
        ctx.Response.Reply(text)
    | s ->
        // Handle an ordinary file request
        let file = 
          root + (if s = "/" then "chat.html" else s.ToLower())
        if File.Exists(file) then 
          let typ = contentTypes.[Path.GetExtension(file)]
          ctx.Response.Reply(typ, File.ReadAllBytes(file))
        else 
          ctx.Response.Reply(sprintf "File not found: %s" file) }
  async {
    while true do 
      let! ctx = mbox.Receive()
      ctx |> handleRequest |> Async.Start })

server.Stop()
