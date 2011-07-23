/// Extensions to the List module.
module FSharp.Monad.List

let split pred l =
  let rec loop l cont =
    match l with
    | [] -> ([],[])
    | x::[] when not (pred x) -> (cont l, [])
    | x::xs when pred x -> (cont [], l)
    | x::xs when not (pred x) -> loop xs (fun rest -> cont (x::rest))
    | _ -> failwith "List.split: Unrecognized pattern"
  loop l id

let splitAt n l =
  let pred i = i >= n
  let rec loop i l cont =
    match l with
    | [] -> ([],[])
    | x::[] when not (pred i) -> (cont l, [])
    | x::xs when pred i -> (cont [], l)
    | x::xs when not (pred i) -> loop (i+1) xs (fun rest -> cont (x::rest))
    | _ -> failwith "List.splitAt: Unrecognized pattern"
  loop 0 l id
