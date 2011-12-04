#load @"..\src\FSharpx.Core\Prelude.fs"
open FSharpx
#load @"..\src\FSharpx.Core\Collections.fs"
#load @"..\src\FSharpx.Core\Iteratee.fs"

open System
open System.Diagnostics
open System.IO
open FSharpx.Iteratee
open FSharpx.Iteratee.Binary

let httpRequest : byte [] = @"GET /some/uri HTTP/1.1
Accept:text/html,application/xhtml+xml,application/xml
Accept-Charset:ISO-8859-1,utf-8;q=0.7,*;q=0.3
Accept-Encoding:gzip,deflate,sdch
Accept-Language:en-US,en;q=0.8
Cache-Control:max-age=0
Connection:keep-alive
Host:stackoverflow.com
If-Modified-Since:Sun, 25 Sep 2011 20:55:29 GMT
Referer:http://www.bing.com/search?setmkt=en-US&q=don't+use+IEnumerable%3Cbyte%3E
User-Agent:Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/535.4 (KHTML, like Gecko) Chrome/16.0.889.0 Safari/535.4

<!DOCTYPE HTML PUBLIC ""-//W3C//DTD HTML 4.01//EN"" ""http://www.w3.org/TR/html4/strict.dtd"">
<html>
<head>
...
</head>
<body>
...
</body>
</html>"B

let runStream() =
    let rec readConsecutiveLines (reader:System.IO.StreamReader) cont =
        if reader.EndOfStream then cont []
        else let line = reader.ReadLine()
             if System.String.IsNullOrEmpty(line) then cont []
             else readConsecutiveLines reader (fun tail -> cont (line::tail))

    let sw = System.Diagnostics.Stopwatch.StartNew()
    let result =
        [ for _ in 1..10000 do 
            use stream = new System.IO.MemoryStream(httpRequest)
            use reader = new System.IO.StreamReader(stream)
            yield! readConsecutiveLines reader id ]
    sw.Stop()
    printfn "Stream read %d in %d ms" result.Length sw.ElapsedMilliseconds

let runAsyncStream() =
    let readLines stream =
        let bufferSize = 128
        let buffer = Array.zeroCreate<byte> bufferSize
        let rec loop (stream:Stream) cont = async {
            let! bytesRead = stream.AsyncRead(buffer, 0, bufferSize)
            if bytesRead = 0 then return cont []
            else let line = System.Text.Encoding.ASCII.GetString(buffer, 0, bytesRead)
                 let lines = line.Split([|"\r\n"|], StringSplitOptions.None) |> List.ofArray
                 return! loop stream (fun tail -> cont (lines @ tail)) }
        loop stream id

    let readLines10000Times () =
        let rec loop n acc = async {
            if n = 10000 then return acc
            else use stream = new MemoryStream(httpRequest)
                 let! lines = readLines stream
                 return! loop (n+1) (acc @ lines) }
        loop 0 []

    let sw = Stopwatch.StartNew()
    let result = readLines10000Times() |> Async.RunSynchronously
    sw.Stop()
    printfn "Async stream read %d lines in %d ms" result.Length sw.ElapsedMilliseconds

//    Async.StartWithContinuations(
//        readLines10000Times (), 
//        (fun (result:string list) ->
//            sw.Stop()
//            printfn "Async stream read %d lines in %d ms" result.Length sw.ElapsedMilliseconds),
//        (fun e -> ()), (fun c -> ()))

let runIteratee() =
    let sw = Stopwatch.StartNew()
    let result =
        [ for _ in 1..10000 do
            use stream = new MemoryStream(httpRequest)
            yield! match enumStream 128 stream readLines |> run with
                   | Choice1Of2 x -> x
                   | Choice2Of2 y -> y ]
    sw.Stop()
    printfn "Iteratee read %d lines in %d ms" result.Length sw.ElapsedMilliseconds

let runIterateeWithReader() =
    let takeUntilEmptyLine = takeUntil <| ((=) Unchecked.defaultof<byte>)
    let sw = Stopwatch.StartNew()
    let result =
        [ for _ in 1..10000 do
            use stream = new MemoryStream(httpRequest)
            use reader = new StreamReader(stream, true)
            yield! enumStreamReader reader takeUntilEmptyLine |> run ]
    sw.Stop()
    printfn "Iteratee with reader read %d lines in %d ms" result.Length sw.ElapsedMilliseconds

let runEnumerator() =
    let readLines reader =
        let rec loop (reader:StreamReader) cont = FSharpx.Enumerator.iter {
            let line = reader.ReadLine()
            if String.IsNullOrEmpty(line) then yield cont []
            else yield! loop reader (fun rest -> cont(line::rest)) }
        loop reader id
    let sw = Stopwatch.StartNew()
    let result =
        [ for _ in 1..10000 do
            use stream = new MemoryStream(httpRequest)
            use reader = new StreamReader(stream)
            yield! FSharpx.Enumerator.toSeq <| fun () -> readLines reader ] |> List.concat
    sw.Stop()
    printfn "Enumerator read %d lines in %d ms" result.Length sw.ElapsedMilliseconds

let private main args =
    runStream()
    runAsyncStream()
    runIteratee()
    runIterateeWithReader()
    runEnumerator()
    
#if INTERACTIVE
let args = fsi.CommandLineArgs in main args.[1..args.Length - 1]
#else
[<EntryPoint>]
let entryPoint args = main args; 0
#endif
