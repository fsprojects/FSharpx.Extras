namespace FSharp.IO

open System
#if NET40
open System.Diagnostics.Contracts
#else
open System.Diagnostics
#endif
open System.IO
open FSharp.Control
open FSharpx

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
        Debug.Assert(buffer <> null, "buffer cannot be null")
        Debug.Assert(offset >= 0 && offset < buffer.Length, "offset is out of range")
        Debug.Assert(count >= 0 && offset + count <= buffer.Length, "count is out of range")
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
        Debug.Assert(buffer <> null, "buffer cannot be null")
        Debug.Assert(offset >= 0 && offset < buffer.Length, "offset is out of range")
        Debug.Assert(count >= 0 && offset + count <= buffer.Length, "count is out of range")
        #endif

        if count = 0 then () else
        queue.Enqueue(buffer, offset, count)

    member x.AsyncRead(buffer: byte[], offset, count, ?timeout) =
        #if NET40
        Contract.Requires(buffer <> null, "buffer cannot be null")
        Contract.Requires(offset >= 0 && offset < buffer.Length, "offset is out of range")
        Contract.Requires(count >= 0 && offset + count <= buffer.Length, "count is out of range")
        #else
        Debug.Assert(buffer <> null, "buffer cannot be null")
        Debug.Assert(offset >= 0 && offset < buffer.Length, "offset is out of range")
        Debug.Assert(count >= 0 && offset + count <= buffer.Length, "count is out of range")
        #endif

        if count = 0 then async.Return(0) else
        async {
            let! chunk = queue.AsyncDequeue(count, ?timeout = timeout)
            Buffer.BlockCopy(chunk, 0, buffer, offset, chunk.Length)
            return chunk.Length }

    override x.Close() =
        base.Close()
        // TODO: Close the queue agent.
