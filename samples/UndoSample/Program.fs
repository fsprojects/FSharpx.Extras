open System
open FSharpx.Functional.Undo

module Sample =
    let rec handleInput<'a> = undoable {         
        match Console.ReadLine() with         
        | "undo" -> 
                let! _ = undo
                ()
        | input -> 
                match Double.TryParse input with
                | true,x -> do! combineWithCurrent (+) x
                | _ -> ()

        let! currentVal = getCurrent
        printfn "The current total is %O" currentVal
        return! handleInput }

Sample.handleInput (newHistory 0.) |> ignore