module FSharpx.Tests.MaybeTest

open FSharpx
open FSharpx.Option
open NUnit.Framework
open FsUnit

let divide x y =
  match y with
  | 0.0 -> None
  | _ -> Some(x/y)

let totalResistance r1 r2 r3 =
  maybe {
    let! x = divide 1.0 r1
    let! y = divide 1.0 r2
    let! z = divide 1.0 r3
    return! divide 1.0 (x + y + z) }

let calculating r1 r2 r3 =
  defaultArg (totalResistance r1 r2 r3) 0.0

[<Test>]
let ``When calculating, it should calculate 0.009581881533``() =
  calculating 0.01 0.75 0.33 |> should equal (1.0/(1.0/0.01+1.0/0.75+1.0/0.33))

[<Test>]
let ``When calculating, it should calculate 0.0``() =
  calculating 0.00 0.55 0.75 |> should equal 0.0 

[<Test>]
let ``Desugared else branch should be None``() =
    maybe { 
        if false then 
          return 4 } 
      |> should equal None

open FsCheck
open FsCheck.NUnit

[<Test>]
let ``monad laws``() =
    let ret (x: int) = maybe.Return x
    let n = sprintf "Maybe : monad %s"
    let inline (>>=) m f = maybe.Combine(m,f)
    fsCheck "left identity" <| 
        fun f a -> ret a >>= f = f a
    fsCheck "right identity" <| 
        fun x -> x >>= ret = x
    fsCheck "associativity" <| 
        fun f g v ->
            let a = (v >>= f) >>= g
            let b = v >>= (fun x -> f x >>= g)
            a = b

[<Test>]
let ``monadplus laws``() =
    // http://www.haskell.org/haskellwiki/MonadPlus
    let mzero = maybe.Zero()
    let ret = maybe.Return
    let mplus x = flip orElse x
    let inline (>>=) m f = maybe.Combine(m,f)
    fsCheck "monoid left identity" <|
        fun a -> mplus a mzero = a
    fsCheck "monoid right identity" <|
        fun a -> mplus mzero a = a
    fsCheck "monoid associativity" <|
        fun a b c -> mplus (mplus a b) c = mplus a (mplus b c)
    fsCheck "left zero" <|
        fun a -> mzero >>= a = mzero
    fsCheck "left catch" <|
        fun a b -> mplus (ret a) b = ret a
//    fsCheck "left distribution" <|
//        fun a b f -> (mplus a b >>= f) = (mplus (a >>= f) (b >>= f))
