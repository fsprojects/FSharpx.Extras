// ----------------------------------------------------------------------------
// F# async extensions (AsyncSeq.fs)
// (c) Tomas Petricek, 2011, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------
namespace FSharp.Control

// ----------------------------------------------------------------------------

[<AutoOpen>]
module AsyncExtensions = 

  type Microsoft.FSharp.Control.Async with 

    /// Creates an asynchronous workflow that runs the asynchronous workflow
    /// given as an argument at most once. When the returned workflow is 
    /// started for the second time, it reuses the result of the 
    /// previous execution.
    static member Cache (input:Async<'T>) = 
      let agent = Agent<AsyncReplyChannel<_>>.Start(fun agent -> async {
        let! repl = agent.Receive()
        let! res = input
        repl.Reply(res)
        while true do
          let! repl = agent.Receive()
          repl.Reply(res) })
      async { return! agent.PostAndAsyncReply(id) }
