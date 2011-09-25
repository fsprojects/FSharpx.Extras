#r @"..\packages\FSPowerPack.Community.2.1.1.1\Lib\Net40\FSharp.PowerPack.dll"
#load @"..\src\FSharpx.Core\Prelude.fs"
open FSharpx
#load @"..\src\FSharpx.Core\Collections.fs"
#load @"..\src\FSharpx.Core\Monad.fs"

open System
open System.Diagnostics
open System.IO
open FSharpx.Iteratee
open FSharpx.Iteratee.Binary

let bytes : byte [] = "GET / HTTP/1.1\r\nContent-Type: text/plain\r\n\r\nBody content\r\n"B

let runStream() =
    let sw = Stopwatch.StartNew()
    use stream = new MemoryStream(bytes)
    use reader = new StreamReader(stream)
    let rec readLine (reader:StreamReader) cont =
        if reader.EndOfStream then cont []
        else let line = reader.ReadLine()
             if String.IsNullOrEmpty(line) then cont []
             else readLine reader (fun tail -> cont (line::tail))
    let result = [ for _ in 1..100000 do yield! readLine reader id ]
    sw.Stop()
    printfn "Stream read %d lines in %d ms" result.Length sw.ElapsedMilliseconds

let runAsyncStream() =
    let bufferSize = 4096
    let buffer = Array.zeroCreate<byte> bufferSize
    let sw = Stopwatch.StartNew()
    let stream = new MemoryStream(bytes)

    let rec readLine (stream:Stream) cont = async {
        let! bytesRead = stream.AsyncRead(buffer, 0, bufferSize)
        if bytesRead = 0 then return cont []
        else
            let line = System.Text.Encoding.ASCII.GetString(buffer, 0, bytesRead)
            let lines = line.Split([|"\r\n"|], StringSplitOptions.None) |> List.ofArray
            return! readLine stream (fun tail -> cont (lines @ tail)) }

    let readLines100000Times stream =
        let rec loop n acc = async {
            if n = 100000 then return acc
            else let! lines = readLine stream id
                 return! loop (n+1) (acc @ lines) }
        loop 0 []

    let result = readLines100000Times stream
                 |> Async.RunSynchronously
    sw.Stop()
    printfn "Async stream read %d lines in %d ms" result.Length sw.ElapsedMilliseconds

//    Async.StartWithContinuations(
//        readLines100000Times stream, 
//        (fun (result:string list) ->
//            sw.Stop()
//            printfn "Async stream read %d lines in %d ms" result.Length sw.ElapsedMilliseconds),
//        (fun e -> ()), (fun c -> ()))

let runEnumerator() =
    let readLines (stream:Stream) =
        // Note: not able to automatically dispose of the reader
        let reader = new StreamReader(stream)
        let loop (reader:TextReader) = FSharpx.Enumerator.iter {
            let line = reader.ReadLine()
            yield line }
        loop reader
    let sw = Stopwatch.StartNew()
    use stream = new MemoryStream(bytes)
    let readLinesFromStream() = readLines stream
    let result = [ for _ in 1..100000 do yield! FSharpx.Enumerator.toSeq <| fun () -> readLines stream ]
    sw.Stop()
    printfn "Enumerator read %d lines in %d ms" result.Length sw.ElapsedMilliseconds

let runIteratee() =
    let sw = Stopwatch.StartNew()
    use stream = new MemoryStream(bytes)
    let result = [ for _ in 1..100000 do yield! match enumStream 4096 stream readLines |> run with Choice1Of2 x -> x | Choice2Of2 y -> y ]
    sw.Stop()
    printfn "Iteratee read %d lines in %d ms" result.Length sw.ElapsedMilliseconds

let runIterateeWithReader() =
    let takeUntilEmptyLine = takeUntil <| ((=) Unchecked.defaultof<byte>)
    let sw = Stopwatch.StartNew()
    use stream = new MemoryStream(bytes)
    use reader = new StreamReader(stream, true)
    let result = [ for _ in 1..100000 do yield! (enumStreamReader reader takeUntilEmptyLine |> run) ]
    sw.Stop()
    printfn "Iteratee with reader read %d lines in %d ms" result.Length sw.ElapsedMilliseconds

let private main args =
    runStream()
    runAsyncStream()
    runEnumerator()
    runIteratee()
    runIterateeWithReader()
    
#if INTERACTIVE
let args = fsi.CommandLineArgs in main args.[1..args.Length - 1]
#else
[<EntryPoint>]
let entryPoint args = main args; 0
#endif

