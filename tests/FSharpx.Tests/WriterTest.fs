module FSharpx.Tests.WriterTest

open FSharpx
open FSharpx.Writer
open NUnit.Framework
open FsUnit
open TestHelpers

let logMsg (message:string) = tell [message]
let processFile file = printfn "%s" file
let processFiles files = writer {
  try
    do! logMsg "Begin processing files"
    for file in files do
      do! logMsg (sprintf "Processing %s" file)
      processFile file

    do! logMsg "End processing files"
  
  with e ->
    do! logMsg (sprintf "An exception occurred %s" (e.ToString())) }

[<Test>]
let ``When processing files, it should log messages``() =
  let processing files = processFiles files ()
  let files = [ "C:\Test1.txt"; "C:\Test2.txt" ]
  processing files |> should equal ((), ["Begin processing files"
                                         "Processing C:\Test1.txt"
                                         "Processing C:\Test2.txt"
                                         "End processing files"])

[<Test>]
let ``use should dispose underlying IDisposable``() =
  let disposeChecker = new DisposeChecker()
  let (r,_) =
     (writer {
       use! x = writer {return disposeChecker}
       return x.Disposed
     })()
  Assert.Multiple
    (fun () ->
      disposeChecker.Disposed |> shouldEqual true
      r |> shouldEqual false
    )