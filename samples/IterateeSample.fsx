#r @"..\packages\FSPowerPack.Community.2.1.1.1\Lib\Net40\FSharp.PowerPack.dll"
#load @"..\src\FSharpx\Prelude.fs"
#load @"..\src\FSharpx\Collections.fs"
#load @"..\src\FSharpx\Monad.fs"

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
    let rec readLine cont =
        if reader.EndOfStream then cont []
        else let line = reader.ReadLine() in readLine (fun tail -> cont (line::tail))
    let result = readLine id
    sw.Stop()
    printfn "Stream read %d lines in %d ms" result.Length sw.ElapsedMilliseconds

let runAsyncStream() =
    let bufferSize = 4096
    let buffer = Array.zeroCreate<byte> bufferSize
    let sw = Stopwatch.StartNew()
    let stream = new MemoryStream(bytes)
    let onError e = ()
    let onDone (result:string list) =
        sw.Stop()
        printfn "Async stream read %d lines in %d ms" result.Length sw.ElapsedMilliseconds
    let rec readLine cont = async {
        let! bytesRead = stream.AsyncRead(buffer, 0, bufferSize)
        if bytesRead = 0 then return cont []
        else
            // parse for lines
            return! readLine (fun tail -> cont (System.Text.Encoding.ASCII.GetString(buffer, 0, bytesRead)::tail)) }
    Async.StartWithContinuations(readLine id, onDone, onError, onError)

let runIteratee() =
    let sw = Stopwatch.StartNew()
    use stream = new MemoryStream(bytes)
    let result = match enumStream 4096 stream readLines |> run_ with
                 | Choice1Of2 x -> x
                 | Choice2Of2 y -> y
    sw.Stop()
    printfn "Iteratee read %d lines in %d ms" result.Length sw.ElapsedMilliseconds

let private main args =
    runStream()
    runAsyncStream()
    runIteratee()
    
#if INTERACTIVE
let args = fsi.CommandLineArgs in main args.[1..args.Length - 1]
#else
[<EntryPoint>]
let entryPoint args = main args; 0
#endif
