// ----------------------------------------------------------------------------
// F# async extensions (ConcurrentSetAgent.fs)
// (c) Tomas Petricek, 2011, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------
namespace FSharpx.Control

open System

// ----------------------------------------------------------------------------

/// Agent that implements a simple concurrent set. The agent exposes a 
/// member that adds value to the set and returns whether the value
/// was already present.
type ConcurrentSetAgent<'T>() = 

    let agent = Agent.Start(fun agent -> async {

        let hashSet = new System.Collections.Generic.HashSet<_>(HashIdentity.Structural)
        while true do
            let! value, (repl:AsyncReplyChannel<_>) = agent.Receive()
            repl.Reply(hashSet.Add(value)) })

    /// Adds the specified element to the set and returns 
    /// 'false' when it was already present in the set
    member x.AsyncAdd(v) = agent.PostAndAsyncReply(fun repl -> v, repl)
