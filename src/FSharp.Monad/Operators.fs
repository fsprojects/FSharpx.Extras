module FSharp.Monad.Operators

let inline returnM builder x = (^M: (member Return: 'b -> 'c) (builder, x))
let inline bindM builder m f = (^M: (member Bind: 'd -> ('e -> 'c) -> 'c) (builder, m, f))
let inline liftM builder m f =
  let inline ret x = (^M: (member Return: 'b -> 'c) (builder, f x))
  (^M: (member Bind: 'd -> ('e -> 'c) -> 'c) (builder, m, ret))
let inline applyM (builder1:^M1) (builder2:^M2) f m =
  bindM builder1 f <| fun f' ->
    bindM builder2 m <| fun m' ->
      returnM builder2 (f' m') 
