module FSharp.Monad.Async.Operators

open FSharp.Monad.Operators

let inline returnM x = returnM async x
let inline (>>=) m f = bindM async m f
let inline (<*>) f m = applyM async async f m
let inline pipe m f = liftM async f m
let inline pipe2 x y f = returnM f <*> x <*> y
let inline pipe3 x y z f = returnM f <*> x <*> y <*> z
let inline (<!>) f m = pipe m f
let inline ( *>) x y = pipe2 x y (fun _ z -> z)
let inline ( <*) x y = pipe2 x y (fun z _ -> z)
let inline (>>.) m f = bindM async m (fun _ -> f)
