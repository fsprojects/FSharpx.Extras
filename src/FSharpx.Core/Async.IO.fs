// ----------------------------------------------------------------------------
// F# async extensions (IO.fs)
// (c) Tomas Petricek and Ryan Riley, 2011-2012, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------

namespace FSharp.IO
open System.IO
open FSharp.Control
// ----------------------------------------------------------------------------
// Extensions that simplify working with Stream using async sequences

[<AutoOpen>]
module IOExtensions = 
  type Stream with
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

open System
#if NET40
open System.Diagnostics.Contracts
#endif

// Loosely based on Stephen Toub's Stream Pipelines article in MSDN.
// See http://msdn.microsoft.com/en-us/magazine/cc163290.aspx
type CircularStream(maxLength) =
    inherit Stream()

    let queue = new CircularQueueAgent<byte>(maxLength)

    override x.CanRead = true
    override x.CanSeek = false
    // We deviate from Toub's implementation in that we
    // never prevent writes.
    override x.CanWrite = true
    
    override x.Flush() = ()
    override x.Length = raise <| new NotSupportedException()
    override x.Position
        with get() = raise <| new NotSupportedException()
        and set(v) = raise <| new NotSupportedException()
    override x.Seek(offset, origin) = raise <| new NotSupportedException()
    override x.SetLength(value) = raise <| new NotSupportedException()

    override x.Read(buffer, offset, count) =
        #if NET40
        Contract.Requires(buffer <> null, "buffer cannot be null")
        Contract.Requires(offset >= 0 && offset < buffer.Length, "offset is out of range")
        Contract.Requires(count >= 0 && offset + count <= buffer.Length, "count is out of range")
        #else
        assert (buffer <> null)
        assert (offset >= 0 && offset < buffer.Length)
        assert (count >= 0 && offset + count <= buffer.Length)
        #endif

        if count = 0 then 0 else
        let chunk = queue.Dequeue(count)
        Buffer.BlockCopy(chunk, 0, buffer, offset, chunk.Length)
        chunk.Length

    override x.Write(buffer, offset, count) =
        #if NET40
        Contract.Requires(buffer <> null, "buffer cannot be null")
        Contract.Requires(offset >= 0 && offset < buffer.Length, "offset is out of range")
        Contract.Requires(count >= 0 && offset + count <= buffer.Length, "count is out of range")
        #else
        assert (buffer <> null)
        assert (offset >= 0 && offset < buffer.Length)
        assert (count >= 0 && offset + count <= buffer.Length)
        #endif

        if count = 0 then () else
        queue.Enqueue(buffer, offset, count)

    member x.AsyncRead(buffer: byte[], offset, count, ?timeout) =
        #if NET40
        Contract.Requires(buffer <> null, "buffer cannot be null")
        Contract.Requires(offset >= 0 && offset < buffer.Length, "offset is out of range")
        Contract.Requires(count >= 0 && offset + count <= buffer.Length, "count is out of range")
        #else
        assert (buffer <> null)
        assert (offset >= 0 && offset < buffer.Length)
        assert (count >= 0 && offset + count <= buffer.Length)
        #endif

        if count = 0 then async.Return(0) else
        async {
            let! chunk = queue.AsyncDequeue(count, ?timeout = timeout)
            Buffer.BlockCopy(chunk, 0, buffer, offset, chunk.Length)
            return chunk.Length }

    override x.Close() =
        base.Close()
        // TODO: Close the queue agent.
