

#r @"..\build\FSharpx.Core.dll"


open Microsoft.FSharp.Control


let lines = [| for n in 1..100000 do
               let str = sprintf "line: %d" n
               yield str |]



let asyncWriteA =  System.IO.File.AsyncWriteAllLines(@"c:\tmp\a.txt", lines)
let asyncWriteB =  System.IO.File.AsyncWriteAllLines(@"c:\tmp\b.txt", lines)
let asyncWriteC =  System.IO.File.AsyncWriteAllLines(@"c:\tmp\c.txt", lines)
let asyncWriteD =  System.IO.File.AsyncWriteAllLines(@"c:\tmp\d.txt", lines)
let asyncWriteE =  System.IO.File.AsyncWriteAllText( @"c:\tmp\e.txt", "this is some text\n")

printfn "async write all lines beginning"

[|asyncWriteA; asyncWriteB; asyncWriteC; asyncWriteD; asyncWriteE|]
    |> Async.Parallel 
    |> Async.RunSynchronously 
    |> ignore


let asyncAppendA =  System.IO.File.AsyncAppendAllLines(@"c:\tmp\a.txt", lines)
let asyncAppendB =  System.IO.File.AsyncAppendAllLines(@"c:\tmp\b.txt", lines)
let asyncAppendC =  System.IO.File.AsyncAppendAllLines(@"c:\tmp\c.txt", lines)
let asyncAppendD =  System.IO.File.AsyncAppendAllLines(@"c:\tmp\d.txt", lines)
let asyncAppendE =  System.IO.File.AsyncAppendAllText( @"c:\tmp\e.txt", "this is some more text\n")


[|asyncAppendA;asyncAppendB;asyncAppendC;asyncAppendD; asyncAppendE|]
    |> Async.Parallel 
    |> Async.RunSynchronously 
    |> ignore


printfn "async append all lines complete"