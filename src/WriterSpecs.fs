module WriterTests
open FSharp.Monad.Writer
open NaturalSpec

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

[<Scenario>]
let ``When processing files, it should log messages``() =
  let ``processing the files through the writer`` files =
    printMethod ""
    processFiles files |> runWriter 

  let files = seq { yield "C:\Test1.txt"
                    yield "C:\Test2.txt" }

  let expected = ((),
                  ["Begin processing files"
                   "Processing C:\Test1.txt"
                   "Processing C:\Test2.txt"
                   "End processing files"])
  Given files
  |> When ``processing the files through the writer``
  |> It should equal expected
  |> Verify