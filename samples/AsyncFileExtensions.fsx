

//#r @"..\build\FSharpx.Core.dll"
#r @"C:\Users\Ian\Documents\GitHub\Ian144\fsharpx\build\debug\FSharpx.Core.dll"

open Microsoft.FSharp.Control


let lines = [| for n in 1..100000 do
               let str = sprintf "line: %d" n
               yield str |]



let asyncWriteA =  System.IO.File.AsyncWriteAllLines(@"c:\tmp\a.txt", lines)
let asyncWriteB =  System.IO.File.AsyncWriteAllLines(@"c:\tmp\b.txt", lines)
let asyncWriteC =  System.IO.File.AsyncWriteAllLines(@"c:\tmp\c.txt", lines)
let asyncWriteD =  System.IO.File.AsyncWriteAllLines(@"c:\tmp\d.txt", lines)
let asyncWriteE =  System.IO.File.AsyncWriteAllText( @"c:\tmp\e.txt", "this is some text")

printfn "async write all lines begining"

[|asyncWriteA; asyncWriteB; asyncWriteC; asyncWriteD; asyncWriteE|] 
    |> Async.Parallel 
    |> Async.RunSynchronously 
    |> ignore



printfn "async write all lines complete"