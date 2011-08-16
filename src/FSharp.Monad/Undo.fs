module FSharp.Monad.Undo

// UndoMonad on top of StateMonad
open FSharp.Monad.State

let undoable = state

type 'a History =  { 
    Current:'a
    Undos : 'a list
    Redos : 'a list }

let empty x = { Current = x; Undos = []; Redos = [] }
let current history = history.Current

let getHistory = getState

let putToHistory x = undoable {
    let! history = getState
    do! putState 
         { Current = x; 
           Undos = history.Current :: history.Undos
           Redos = [] } }

let exec m s = m s |> snd |> current

let getCurrent<'a> = undoable {
    let! (history:'a History) = getState
    return current history}

let undo<'a> = undoable {
    let! (history:'a History) = getState
    match history.Undos with
    | [] -> return false
    | (x::rest) -> 
        do! putState 
                { Current = x;
                  Undos = rest;
                  Redos = history.Current :: history.Redos }
        return true}


let redo<'a> = undoable {
    let! (history:'a History) = getState
    match history.Redos with
    | [] -> return false
    | (x::rest) -> 
        do! putState 
                { Current = x;
                  Undos = history.Current :: history.Undos;
                  Redos = rest }
        return true}