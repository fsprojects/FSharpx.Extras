// ----------------------------------------------------------------------------
// F# async extensions (Agent.fs)
// (c) Tomas Petricek, 2011, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------
namespace FSharpx.Control

/// Type alias for F# mailbox processor type
type Agent<'T> = MailboxProcessor<'T>
