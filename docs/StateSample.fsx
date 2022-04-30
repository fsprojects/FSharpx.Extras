#r @"../bin/FSharpx.Extras.dll"
#r @"../bin/FSharpx.Collections.dll"

open System
open System.Threading
open FSharpx

type MyStateRecord =
    {
        X:int
        Y:string
    }
type MyResultType =
    {
        R: double
    }

let ``nested step without result`` : State.State<unit,MyStateRecord> =
    State.state {
        printfn "Starting ``nested step without result``"
        
        let! s = State.getState
        do! State.putState {s with X = s.X+1}
        printfn "Ending ``nested step without result``"
    }

let ``nested step with same state type, but different result type`` : State.State<char,MyStateRecord> =
    State.state {
        printfn "Starting ``nested step with same state type, but different result type``"
        
        printfn "Getting the state sn1 inside nested step "
        let! sn1 = State.getState
        printfn "sn1: %A" sn1

        printfn "Writing state inside nested step"
        do! State.putState {sn1 with X=0;  Y="nested Step"}
        printfn "Getting the state sn2 inside nested step "
        let! sn2 = State.getState
        
        printfn "Notice that put does not mutate sn1"
        printfn "sn1: %A" sn1
        printfn "sn2: %A" sn2
        
        printfn "Ending ``nested step with same state type, but different result type``"
        return (char sn1.X)
    }

let ``computation using state monad``  : State.State<MyResultType, MyStateRecord> =
    State.state {
        printfn "Starting ``computation using state monad``"
        printfn "Getting state value s1"
        let! s1 = State.getState
        printfn "s1: %A" s1

        printfn "Writing state"
        do! State.putState {s1 with Y = "first put"}
        printfn "Getting state value s2"
        let! s2 = State.getState
        printfn "s2: %A" s2

        printfn "Start nested step resulting in c1"
        let! c1 = ``nested step with same state type, but different result type``
        printfn "End nested step resulting in c1"
        printfn "Getting state value s3"
        let! s3 = State.getState
        printfn "c1: %A, s3: %A" c1 s3

        printfn "Start nested step without result (increases X by one)"
        do! ``nested step without result``
        printfn "End nested step resulting"
        printfn "Getting state value s4"
        let! s4 = State.getState
        printfn "s4: %A" s4
        
        printfn "double c1: %A double s4.X: %A" (double c1) (double s4.X)
        printfn "Ending ``computation using state monad``"
        return {R= 123.0 + double c1 + double s4.X}
    }

let run () =
    let startingState = {X = 42; Y="start"}
    
    printfn "You can get both result and state by providing the State monad with start state"
    printfn "(Notice that computation won't start until you provide `startState`)"
    let (result, endState) = ``computation using state monad`` startingState
    printfn "result: %A, endState: %A" result endState
    printfn ""
    printfn ""
    printfn ""
    printfn "Or result only with `eval`"
    printfn "(Notice recomputation)"
    let resultOnly = State.eval ``computation using state monad`` startingState
    printfn "resultOnly: %A" resultOnly
    printfn ""
    printfn ""
    printfn ""
    printfn "Or endState only with `exec`"
    let onlyEndState = State.exec ``computation using state monad`` startingState
    printfn "onlyEndState: %A" onlyEndState
    ()