// First version copied from the F# Power Pack 
// https://raw.github.com/fsharp/powerpack/master/src/FSharp.PowerPack/AsyncStreamReader.fs

namespace FSharpx.Control

open System
open System.Diagnostics
open System.IO
open System.Text

/// <summary>
/// Implements a TextReader-like API that asynchronously reads characters from 
/// a byte stream in a particular encoding.
/// </summary>
[<Sealed>]
type AsyncStreamReader(stream:Stream, encoding:Encoding, detectEncodingFromByteOrderMarks:bool, bufferSize:int) =
    static let defaultBufferSize = 1024;  // Byte buffer size
    static let defaultFileStreamBufferSize = 4096;
    static let minBufferSize = 128; 

    // Creates a new StreamReader for the given stream.  The 
    // character encoding is set by encoding and the buffer size,
    // in number of 16-bit characters, is set by bufferSize. 
    // 
    // Note that detectEncodingFromByteOrderMarks is a very
    // loose attempt at detecting the encoding by looking at the first 
    // 3 bytes of the stream.  It will recognize UTF-8, little endian
    // unicode, and big endian unicode text, but that's it.  If neither
    // of those three match, it will use the Encoding you provided.
    // 

    do  if (stream=null || encoding=null) then 
            raise <| new ArgumentNullException(if (stream=null) then "stream" else "encoding");

        if not stream.CanRead then
            invalidArg "stream" "stream not readable";
#if FX_NO_FILESTREAM_ISASYNC
#else
        match stream with 
        | :? System.IO.FileStream as fs when not fs.IsAsync -> 
            invalidArg "stream" "FileStream not asynchronous. AsyncStreamReader should only be used on FileStream if the IsAsync property returns true. Consider passing 'true' for the async flag in the FileStream constructor"
        | _ -> 
            ()
#endif
        if (bufferSize <= 0) then
            raise <| new ArgumentOutOfRangeException("bufferSize");

    let mutable stream = stream
    let mutable decoder = encoding.GetDecoder();
    let mutable encoding = encoding
    let bufferSize = max bufferSize  minBufferSize; 

    // This is the maximum number of chars we can get from one call to
    // readBuffer.  Used so readBuffer can tell when to copy data into 
    // a user's char[] directly, instead of our internal char[].
    let mutable _maxCharsPerBuffer = encoding.GetMaxCharCount(bufferSize) 
    let mutable byteBuffer = Array.zeroCreate<byte> bufferSize;
    let mutable charBuffer = Array.zeroCreate<char> _maxCharsPerBuffer;
    let preamble = encoding.GetPreamble();   // Encoding's preamble, which identifies this encoding. 
    let mutable charPos = 0
    let mutable charLen = 0
    // Record the number of valid bytes in the byteBuffer, for a few checks. 
    let mutable byteLen = 0
    // This is used only for preamble detection 
    let mutable bytePos = 0

    // We will support looking for byte order marks in the stream and trying
    // to decide what the encoding might be from the byte order marks, IF they 
    // exist.  But that's all we'll do.
    let mutable _detectEncoding = detectEncodingFromByteOrderMarks;

    // Whether we must still check for the encoding's given preamble at the 
    // beginning of this file.
    let mutable _checkPreamble = (preamble.Length > 0); 

    let readerClosed() = invalidOp "reader closed"
    // Trims n bytes from the front of the buffer.
    let compressBuffer(n) =
        Debug.Assert(byteLen >= n, "compressBuffer was called with a number of bytes greater than the current buffer length.  Are two threads using this StreamReader at the same time?");
        Buffer.BlockCopy(byteBuffer, n, byteBuffer, 0, byteLen - n);
        byteLen <- byteLen - n; 

    // Trims the preamble bytes from the byteBuffer. This routine can be called multiple times
    // and we will buffer the bytes read until the preamble is matched or we determine that
    // there is no match. If there is no match, every byte read previously will be available 
    // for further consumption. If there is a match, we will compress the buffer for the
    // leading preamble bytes 
    let isPreamble() = 
        if not _checkPreamble then _checkPreamble else

        Debug.Assert(bytePos <= preamble.Length, "_compressPreamble was called with the current bytePos greater than the preamble buffer length.  Are two threads using this StreamReader at the same time?");
        let len = if (byteLen >= (preamble.Length)) then (preamble.Length - bytePos) else (byteLen  - bytePos); 

        let mutable fin = false
        let mutable i = 0
        while i < len && not fin do
            if (byteBuffer.[bytePos] <> preamble.[bytePos]) then
                bytePos <- 0;
                _checkPreamble <- false; 
                fin <- true
            if not fin then 
                i <- i + 1
                bytePos <- bytePos + 1

        Debug.Assert(bytePos <= preamble.Length, "possible bug in _compressPreamble.  Are two threads using this StreamReader at the same time?");

        if (_checkPreamble) then
            if (bytePos = preamble.Length) then
                // We have a match 
                compressBuffer(preamble.Length);
                bytePos <- 0;
                _checkPreamble <- false;
                _detectEncoding <- false; 

        _checkPreamble;


    let detectEncoding() =
        if (byteLen >= 2) then 
            _detectEncoding <- false;
            let mutable changedEncoding = false;
            if (byteBuffer.[0]=0xFEuy && byteBuffer.[1]=0xFFuy) then
                // Big Endian Unicode

                encoding <- new UnicodeEncoding(true, true); 
                compressBuffer(2);
                changedEncoding <- true; 
#if FX_NO_UTF32ENCODING
#else
            elif (byteBuffer.[0]=0xFFuy && byteBuffer.[1]=0xFEuy) then
                // Little Endian Unicode, or possibly little endian UTF32
                if (byteLen >= 4 && byteBuffer.[2] = 0uy && byteBuffer.[3] = 0uy) then
                    encoding <- new UTF32Encoding(false, true);
                    compressBuffer(4); 
                else 
                    encoding <- new UnicodeEncoding(false, true); 
                    compressBuffer(2);
                changedEncoding <- true;
#endif
            elif (byteLen >= 3 && byteBuffer.[0]=0xEFuy && byteBuffer.[1]=0xBBuy && byteBuffer.[2]=0xBFuy) then
                // UTF-8 
                encoding <- Encoding.UTF8; 
                compressBuffer(3);
                changedEncoding <- true; 
#if FX_NO_UTF32ENCODING
#else
            elif (byteLen >= 4 && byteBuffer.[0] = 0uy && byteBuffer.[1] = 0uy && byteBuffer.[2] = 0xFEuy && byteBuffer.[3] = 0xFFuy) then
                // Big Endian UTF32 
                encoding <- new UTF32Encoding(true, true);
                changedEncoding <- true; 
#endif
            elif (byteLen = 2) then
                _detectEncoding <- true; 
            // Note: in the future, if we change this algorithm significantly,
            // we can support checking for the preamble of the given encoding.

            if (changedEncoding) then 
                decoder <- encoding.GetDecoder();
                _maxCharsPerBuffer <- encoding.GetMaxCharCount(byteBuffer.Length); 
                charBuffer <- Array.zeroCreate<char> _maxCharsPerBuffer; 

    let readBuffer() = async {
        charLen <- 0;
        charPos <- 0; 

        if not _checkPreamble then
            byteLen <- 0; 

        let fin = ref false
        while (charLen = 0 && not !fin) do
            if (_checkPreamble) then 
                Debug.Assert(bytePos <= preamble.Length, "possible bug in _compressPreamble.  Are two threads using this StreamReader at the same time?");
                let! len = stream.AsyncRead(byteBuffer, bytePos, byteBuffer.Length - bytePos);
                Debug.Assert(len >= 0, "Stream.Read returned a negative number!  This is a bug in your stream class.");

                if (len = 0) then
                    // EOF but we might have buffered bytes from previous 
                    // attempts to detecting preamble that needs to decoded now 
                    if (byteLen > 0) then
                        charLen <-  charLen + decoder.GetChars(byteBuffer, 0, byteLen, charBuffer, charLen); 

                    fin := true
                
                byteLen <- byteLen + len;
            else 
                Debug.Assert((bytePos = 0), "bytePos can be non zero only when we are trying to _checkPreamble.  Are two threads using this StreamReader at the same time?");
                let! len = stream.AsyncRead(byteBuffer, 0, byteBuffer.Length); 
                byteLen <- len
                Debug.Assert(byteLen >= 0, "Stream.Read returned a negative number!  This is a bug in your stream class.");

                if (byteLen = 0)  then // We're at EOF
                    fin := true

            // Check for preamble before detect encoding. This is not to override the
            // user suppplied Encoding for the one we implicitly detect. The user could 
            // customize the encoding which we will loose, such as ThrowOnError on UTF8
            if not !fin then 
                if not (isPreamble()) then
                    // If we're supposed to detect the encoding and haven't done so yet, 
                    // do it.  Note this may need to be called more than once.
                    if (_detectEncoding && byteLen >= 2) then
                        detectEncoding();

                    charLen <- charLen + decoder.GetChars(byteBuffer, 0, byteLen, charBuffer, charLen);

            if (charLen <> 0) then 
                fin := true

        return charLen

    } 


    let cleanup() = 
            // Dispose of our resources if this StreamReader is closable.
            // Note that Console.In should not be closable. 
            try 
                // Note that Stream.Close() can potentially throw here. So we need to
                // ensure cleaning up internal resources, inside the finally block.
                if (stream <> null) then
                    stream.Close();
            
            finally 
                if (stream <> null) then
                    stream <- null; 
                    encoding <- null;
                    decoder <- null;
                    byteBuffer <- null;
                    charBuffer <- null; 
                    charPos <- 0;
                    charLen <- 0; 
                    //REMOVED: base.Dispose(disposing); 

    // StreamReader by default will ignore illegal UTF8 characters. We don't want to 
    // throw here because we want to be able to read ill-formed data without choking.
    // The high level goal is to be tolerant of encoding errors when we read and very strict 
    // when we write. Hence, default StreamWriter encoding will throw on error.

    new (stream) = new AsyncStreamReader(stream, true) 

    new (stream, detectEncodingFromByteOrderMarks:bool) = new AsyncStreamReader(stream, Encoding.UTF8, detectEncodingFromByteOrderMarks, defaultBufferSize)

    new (stream, encoding:Encoding) = new AsyncStreamReader(stream, encoding, true, defaultBufferSize) 

    new (stream, encoding, detectEncodingFromByteOrderMarks) = new AsyncStreamReader(stream, encoding, detectEncodingFromByteOrderMarks, defaultBufferSize) 

(*
    new (path:string) = new AsyncStreamReader(path, true)

    new (path: string, detectEncodingFromByteOrderMarks: bool) = new AsyncStreamReader (path, Encoding.UTF8, detectEncodingFromByteOrderMarks, defaultBufferSize) 

    new (path:string, encoding:Encoding) = new AsyncStreamReader(path, encoding, true, defaultBufferSize) 

    new (path: string, encoding:Encoding, detectEncodingFromByteOrderMarks: bool)  = new AsyncStreamReader(path, encoding, detectEncodingFromByteOrderMarks, defaultBufferSize) 

    new (path: string, encoding: Encoding, detectEncodingFromByteOrderMarks: bool, bufferSize: int)  =
        // Don't open a Stream before checking for invalid arguments, 
        // or we'll create a FileStream on disk and we won't close it until 
        // the finalizer runs, causing problems for applications.
        if (path=null || encoding=null) then
            raise <| new ArgumentNullException((path=null ? "path" : "encoding"));
        if (path.Length=0) then
            raise <| new ArgumentException((* Environment.GetResourceString *)("Argument_EmptyPath"));
        if (bufferSize <= 0)  then
            raise <| new ArgumentOutOfRangeException("bufferSize", (* Environment.GetResourceString *)("ArgumentOutOfRange_NeedPosNum"));

        Stream stream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, defaultFileStreamBufferSize, FileOptions.SequentialScan); 
        Init(stream, encoding, detectEncodingFromByteOrderMarks, bufferSize);

*)

    member x.Close() = cleanup()

    interface System.IDisposable with 
        member x.Dispose() = cleanup()

    member x.CurrentEncoding  = encoding
    member x.BaseStream = stream

    // DiscardBufferedData tells StreamReader to throw away its internal 
    // buffer contents.  This is useful if the user needs to seek on the
    // underlying stream to a known location then wants the StreamReader 
    // to start reading from this new point.  This method should be called
    // very sparingly, if ever, since it can lead to very poor performance.
    // However, it may be the only way of handling some scenarios where
    // users need to re-read the contents of a StreamReader a second time. 
    member x.DiscardBufferedData() =
        byteLen <- 0; 
        charLen <- 0; 
        charPos <- 0;
        decoder <- encoding.GetDecoder(); 

    member x.EndOfStream = async {
        if (stream = null) then
            readerClosed(); 

        if (charPos < charLen) then
            return false
        else
            let! numRead = readBuffer(); 
            return numRead = 0;
    }

    member x.Peek() = 
        async {
            let! emp = x.EndOfStream 
            return (if emp then -1 else int charBuffer.[charPos])
        }

    member x.Read() = async {
        if (stream = null) then
            readerClosed();

        if (charPos = charLen) then 
            let! n = readBuffer() 
            if n = 0 then 
                return char -1; 
            else
                let result = charBuffer.[charPos];
                charPos <- charPos + 1; 
                return result;
        else
            let result = charBuffer.[charPos];
            charPos <- charPos + 1; 
            return result;
    }
    
    // Returns only when count characters have been read or the end of the file was reached. 
    member x.ReadExactly(buffer:char[], index, count) = async {
        let i = ref 0
        let n = ref 0 
        let count = ref count
        let first = ref true
        while !first || (!i > 0 && !n < !count) do 
            let! j = x.Read(buffer, index + !n, !count - !n)
            i := j 
            n := !n + j
            first := false
        return !n;
    } 

    member x.Read(buffer:char[], index, count) = async {
        if (stream = null) then
            readerClosed(); 
        if (buffer=null) then
            raise <| new ArgumentNullException("buffer");
        if (index < 0 || count < 0) then
            raise <| new ArgumentOutOfRangeException((if (index < 0) then "index" else "count"), (* Environment.GetResourceString *)("ArgumentOutOfRange_NeedNonNegNum"));
        if (buffer.Length - index < count) then
            raise <| new ArgumentException("index")

        let charsRead = ref 0;
        let charsReqd = ref count;
        let fin = ref false
        while (!charsReqd > 0) && not !fin do
            let! charsAvail = if (charLen = charPos) then readBuffer() else async { return charLen - charPos }
            if (charsAvail = 0) then 
                // We're at EOF
                fin := true 
            else  
                let charsConsumed = min charsAvail !charsReqd
                Buffer.BlockCopy(charBuffer, charPos * 2, buffer, (index + !charsRead) * 2, charsConsumed*2); 
                charPos <- charPos + charsConsumed; 
                charsRead := !charsRead + charsConsumed; 
                charsReqd := !charsReqd - charsConsumed;

        return !charsRead;
    } 

    member x.ReadToEnd() = async {
        if (stream = null) then
            readerClosed();

        // Call readBuffer, then pull data out of charBuffer. 
        let sb = new StringBuilder(charLen - charPos);
        let readNextChunk = 
            async {
                sb.Append(charBuffer, charPos, charLen - charPos) |> ignore;
                charPos <- charLen;  // Note we consumed these characters
                let! _ = readBuffer() 
                return ()
            }
        do! readNextChunk
        while charLen > 0 do 
            do! readNextChunk
        return sb.ToString();
    } 


    // Reads a line. A line is defined as a sequence of characters followed by 
    // a carriage return ('\r'), a line feed ('\n'), or a carriage return
    // immediately followed by a line feed. The resulting string does not 
    // contain the terminating carriage return and/or line feed. The returned 
    // value is null if the end of the input stream has been reached.
    // 
    member x.ReadLine() = async {

        let! emp = x.EndOfStream
        if emp then return null else
        let sb = new StringBuilder()
        let fin1 = ref false
        while not !fin1 do 
            let i = ref charPos;
            let fin2 = ref false
            while (!i < charLen) && not !fin2 do 
                let ch = charBuffer.[!i];
                // Note the following common line feed chars: 
                // \n - UNIX   \r\n - DOS   \r - Mac
                if (ch = '\r' || ch = '\n') then
                    sb.Append(charBuffer, charPos, !i - charPos) |> ignore; 
                    charPos <- !i + 1; 
                    if ch = '\r' then 
                        let! emp = x.EndOfStream
                        if not emp && (charBuffer.[charPos] = '\n') then 
                            charPos <- charPos + 1;
                    // Found end of line, done
                    fin2 := true
                    fin1 := true
                else
                    i := !i + 1;

            if not !fin1 then 
                i := charLen - charPos;
                sb.Append(charBuffer, charPos, !i) |> ignore; 

                let! n = readBuffer() 
                fin1 := (n <= 0)

        return sb.ToString(); 

    }

