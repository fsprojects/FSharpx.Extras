namespace FSharp.IO

open System
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
        if buffer = null then
            raise <| new ArgumentNullException("buffer")
        if offset < 0 || offset >= buffer.Length then
            raise <| new ArgumentOutOfRangeException("offset")
        if count < 0 || offset + count > buffer.Length then
            raise <| new ArgumentOutOfRangeException("count")

        if count = 0 then 0 else
        let chunk = queue.Dequeue(count)
        Buffer.BlockCopy(chunk, 0, buffer, offset, chunk.Length)
        chunk.Length

    override x.Write(buffer, offset, count) =
        if buffer = null then
            raise <| new ArgumentNullException("buffer")
        if offset < 0 || offset >= buffer.Length then
            raise <| new ArgumentOutOfRangeException("offset")
        if count < 0 || offset + count > buffer.Length then
            raise <| new ArgumentOutOfRangeException("count")

        if count = 0 then () else
        queue.Enqueue(buffer, offset, count)

    member x.AsyncRead(buffer: byte[], offset, count, ?timeout) =
        if buffer = null then
            raise <| new ArgumentNullException("buffer")
        if offset < 0 || offset >= buffer.Length then
            raise <| new ArgumentOutOfRangeException("offset")
        if count < 0 || offset + count > buffer.Length then
            raise <| new ArgumentOutOfRangeException("count")

        if count = 0 then async.Return(0) else
        async {
            let! chunk = queue.AsyncDequeue(count, ?timeout = timeout)
            Buffer.BlockCopy(chunk, 0, buffer, offset, chunk.Length)
            return chunk.Length }

    member x.AsyncWrite(buffer: byte[], offset, count, ?timeout) =
        if buffer = null then
            raise <| new ArgumentNullException("buffer")
        if offset < 0 || offset >= buffer.Length then
            raise <| new ArgumentOutOfRangeException("offset")
        if count < 0 || offset + count > buffer.Length then
            raise <| new ArgumentOutOfRangeException("count")

        if count = 0 then async.Return() else
        queue.AsyncEnqueue(buffer, offset, count, ?timeout = timeout)

    override x.Close() =
        base.Close()
        // TODO: Close the queue agent.
