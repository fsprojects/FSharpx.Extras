namespace FSharpx.Tests

open Microsoft.FSharp.Control
open NUnit.Framework
open System
open System.IO
open System.Text

#nowarn "40"

[<TestFixture>]
type AsyncStreamReaderTest() =
    
    let utf8Encoding : Encoding = downcast (new UTF8Encoding()).Clone()
#if FX_NO_UTF32ENCODING
#else
    let utf32Encoding : Encoding = downcast (new UTF32Encoding()).Clone()
#endif
#if FX_NO_ASCII_ENCODING
#else
    let ASCIIEncoding : Encoding = downcast (new ASCIIEncoding()).Clone()
    let win1251Encoding : Encoding = downcast (Encoding.GetEncoding("windows-1251")).Clone() // Russian
#endif    
    let encodings : (Encoding*bool) list = 
        [   utf8Encoding, true; 
#if FX_NO_UTF32ENCODING
#else
            utf32Encoding, true; 
#endif
#if FX_NO_ASCII_ENCODING
#else
            ASCIIEncoding,false; 
            win1251Encoding,false
#endif
        ]
#if FX_NO_ENCODER_FALLBACK    
#else
    do encodings |> List.iter (fun (e,_) -> e.EncoderFallback <- new EncoderExceptionFallback())
#endif
        
        
    let testAllEncodings (s:string) tester = 
            for (e,detectable) in encodings do
            let bytes, encodingOk =
                try
                    e.GetBytes(s), true
                with
                | :? EncoderFallbackException -> [| |], false
            if encodingOk then
                let ms = new MemoryStream(bytes)
                let reader =  new AsyncStreamReader(ms, e)
                tester reader (e.ToString())
                if detectable then
                    let preamble = e.GetPreamble()
                    let newBytes = [preamble; bytes] |> Array.concat
                    let ms = new MemoryStream(newBytes)
                    let reader =  new AsyncStreamReader(ms)
                    tester reader (sprintf "detectable %A" e)

    let readToEndAndReadCharTest (s:string) =
        testAllEncodings s
            (fun (reader:AsyncStreamReader) encodingDescription ->
                    let result = reader.ReadToEnd() |> Async.RunSynchronously
                    Assert.AreEqual(s, result, (sprintf "ReadToEnd failure on %s" encodingDescription))
                    Assert.IsTrue(reader.EndOfStream |> Async.RunSynchronously, "End of stream reached after ReadToEnd")
                )
        testAllEncodings s
            (fun (reader:AsyncStreamReader) encodingDescription ->
                let rec readAllChars =
                    let sb = new StringBuilder()
                    async {
                        let! eof = reader.EndOfStream
                        if eof then return sb.ToString()
                        else
                            let! c = reader.Read()
                            sb.Append(c) |> ignore
                            return! readAllChars
                    }
                let result = readAllChars |> Async.RunSynchronously
                Assert.AreEqual(s, result, (sprintf "Read-all-charsToEnd failure on %s" encodingDescription))
                Assert.IsTrue(reader.EndOfStream |> Async.RunSynchronously, "End of stream reached after ReadToEnd")
            )
        let readAllUsingBufferOfLength fdesc f n =
            (fun (reader:AsyncStreamReader) encodingDescription ->
                let rec readAllChars =
                    let buffer = Array.create (2*n) ' '
                    let sb = new StringBuilder()
                    async {
                        let! eof = reader.EndOfStream
                        if eof then return sb.ToString()
                        else
                            let! count = f reader (buffer, 0, n)
                            sb.Append(buffer, 0, count) |> ignore
                            let! count = f reader (buffer, count, n)
                            sb.Append(buffer, n, count) |> ignore
                            return! readAllChars
                    }
                let result = readAllChars |> Async.RunSynchronously
                Assert.AreEqual(s, result, (sprintf "%s buffer of length %d failure on %s" fdesc n encodingDescription))
                Assert.IsTrue(reader.EndOfStream |> Async.RunSynchronously, sprintf "End of stream reached after %s using buffer of length %d" fdesc n)
             )
        for (fdesc,f) in [  "Read",         fun (reader:AsyncStreamReader) (buffer,index,count) -> reader.Read(buffer,index,count)
                            "ReadExactly",  fun (reader:AsyncStreamReader) (buffer,index,count) -> reader.ReadExactly(buffer,index,count) ] do
            testAllEncodings s (readAllUsingBufferOfLength fdesc f 1)
            testAllEncodings s (readAllUsingBufferOfLength fdesc f 1024)
            testAllEncodings s (readAllUsingBufferOfLength fdesc f 1999)
            testAllEncodings s (readAllUsingBufferOfLength fdesc f s.Length)

    let textBuilders =
        [
            yield "default separators",
                fun (ss:string list) lastLine ->
                    let sb = new StringBuilder()
                    ss |> List.iter (fun s -> sb.AppendLine s |> ignore)
                    sb.Append (lastLine:string) |> ignore
                    sb.ToString()
            for separator in ["\r"; "\n"; "\r\n"] do
                yield separator.Replace("\r",@"\r").Replace("\n",@"\n"),
                    fun (ss:string list) lastLine ->
                        let sb = new StringBuilder()
                        ss |> List.iter (fun s -> sb.Append s |> ignore; sb.Append separator |> ignore)
                        sb.Append (lastLine:string) |> ignore
                        sb.ToString()
        ]
   
   
    let readLinesTest (ss:string list) lastLine =
        for (descr,textBuilder) in textBuilders do
            testAllEncodings (textBuilder ss lastLine)
                (fun (reader:AsyncStreamReader) encodingDescription ->
                    let rec r acc = 
                        async {
                            let! eof = reader.EndOfStream
                            if eof then return acc |> List.rev
                            else
                                let! nextLine = reader.ReadLine()
                                return! r (nextLine::acc)
                        }
                    let result = r [] |> Async.RunSynchronously
                    let expectedResult =
                        if String.IsNullOrEmpty lastLine then ss else ss @ [lastLine]
                            
                    if expectedResult <> result then
                        Assert.Fail(sprintf "ReadLinesTest: builder %s string list mismatch on %s" descr encodingDescription)
                )
    
    [<Test>]
    member this.``ReadToEnd, Read and ReadExactly on empty stream``() =
        readToEndAndReadCharTest ""
        
    [<Test>]
    member this.``ReadToEnd, Read and ReadExactly on short stream``() =
        readToEndAndReadCharTest "fooBar"
        
    [<Test>]
    member this.``ReadToEnd, Read and ReadExactly on multiple of buffer size``() =
        let chars =
            [| for i in 1..1024 do
                    yield '0'
                    yield '1'
                    yield '2'
            |] 
        readToEndAndReadCharTest (new String(chars))

    [<Test>]
    member this.``ReadToEnd, Read and ReadExactly on non-multiple of buffer size``() =
        let chars =
            [| for i in 1..1024 do
                    yield '0'
                    yield '1'
                    yield '2'
               yield 'a'
               yield 'b'
            |] 
        readToEndAndReadCharTest (new String(chars))
        
    [<Test>]
    member this.``ReadToEnd, Read and ReadExactly on Russian chars``() =
       readToEndAndReadCharTest "Однажды в студеную зимнюю пору\r\nЯ из лесу вышел\r\nБыл сильный мороз!"
       
    [<Test>]
    member this.``ReadToEnd, Read and ReadExactly with surrogate codepoints``() =
      let chars =
        [| for i in 1..4096 do
            yield 'a'
            // surrogate pair
            yield '\uD800' 
            yield '\uDC00'
        |]
      readToEndAndReadCharTest (new String(chars))
    
    [<Test>]
    member this.``ReadLine on empty file``() =
        readLinesTest [] "" 
    
    [<Test>]
    member this.``ReadLine with end on end of line``() =
        readLinesTest ["a";"b";"c"] ""

    [<Test>]
    member this.``ReadLine with end not on end of line``() =
        readLinesTest ["a";"b";"c"] "d"

    [<Test>]
    member this.``ReadLine with EOL mark starting at end-of-buffer``() =
        readLinesTest
            [ 
                new String([|for i in 1..1024 do yield 'x'|])
                new String([|for i in 1..1024 do yield 'y'|])
                new String([|for i in 1..1024 do yield 'z'|])
            ]
            ""

    [<Test>]
    member this.``ReadLine with EOL mark breaking at end-of-buffer``() =
        readLinesTest
            [ 
                new String([|for i in 1..1023 do yield 'x'|])
                new String([|for i in 1..1022 do yield 'y'|])
                new String([|for i in 1..1022 do yield 'z'|])
            ]
            ""

    [<Test>]
    member this.``ReadLine with EOL mark ending at end-of-buffer``() =
        readLinesTest
            [ 
                new String([|for i in 1..1022 do yield 'x'|])
                new String([|for i in 1..1022 do yield 'y'|])
                new String([|for i in 1..1022 do yield 'z'|])
            ]
            ""

