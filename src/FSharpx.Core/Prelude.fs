namespace FSharpx

open System
open System.Globalization

[<AutoOpen>]
module Prelude =
    let inline flip f a b = f b a
    let inline curry f a b = f(a,b)
    let inline uncurry f (a,b) = f a b
    let inline swap (a,b) = (b,a)
    let inline konst a _ = a
    let inline konst2 a _ _ = a
  
    /// Creates a pair
    let inline tuple2 a b = a,b
    /// Creates a 3-tuple
    let inline tuple3 a b c = a,b,c
    /// Creates a 4-tuple
    let inline tuple4 a b c d = a,b,c,d
    /// Creates a 5-tuple
    let inline tuple5 a b c d e = a,b,c,d,e
    /// Creates a 6-tuple
    let inline tuple6 a b c d e f = a,b,c,d,e,f

    let rec fix f x = f (fix f) x
    let rec fix2 f x y = f (fix2 f) x y
    let rec fix3 f x y z = f (fix3 f) x y z

    type Boolean with
        static member parse x =
            match bool.TryParse(x) with
            | true,v -> Some v
            | _ -> None
            
    type Int16 with
        static member parseWithOptions style provider x =
            match Int16.TryParse(x, style, provider) with
            | true,v -> Some v
            | _ -> None
            
        static member parse x = 
            Int16.parseWithOptions NumberStyles.Integer CultureInfo.InvariantCulture x

    type Int32 with
        static member parseWithOptions style provider x =
            match Int32.TryParse(x, style, provider) with
            | true,v -> Some v
            | _ -> None
            
        static member parse x = 
            Int32.parseWithOptions NumberStyles.Integer CultureInfo.InvariantCulture x

    type Decimal with
        static member parseWithOptions style provider x =
            match Decimal.TryParse(x, style, provider) with
            | true,v -> Some v
            | _ -> None
            
        static member parse x = 
            Decimal.parseWithOptions NumberStyles.Currency CultureInfo.InvariantCulture x

    type Byte with
        static member parseWithOptions style provider x =
            match Byte.TryParse(x, style, provider) with
            | true,v -> Some v
            | _ -> None
            
        static member parse x = 
            Byte.parseWithOptions NumberStyles.Integer CultureInfo.InvariantCulture x

    type Int64 with
        static member parseWithOptions style provider x =
            match Int64.TryParse(x, style, provider) with
            | true,v -> Some v
            | _ -> None
            
        static member parse x = 
            Int64.parseWithOptions NumberStyles.Integer CultureInfo.InvariantCulture x

    type Single with
        static member parseWithOptions style provider x =
            match Single.TryParse(x, style, provider) with
            | true,v -> Some v
            | _ -> None
            
        static member parse x = 
            Single.parseWithOptions NumberStyles.Float CultureInfo.InvariantCulture x

    type Double with
        static member parseWithOptions style provider x =
            match Double.TryParse(x, style, provider) with
            | true,v -> Some v
            | _ -> None
            
        static member parse x = 
            Double.parseWithOptions NumberStyles.Float CultureInfo.InvariantCulture x

    type DateTime with
        static member parseWithOptions style provider x =
            match DateTime.TryParse(x, provider, style) with
            | true,v -> Some v
            | _ -> None
            
        static member parse x = 
            DateTime.parseWithOptions DateTimeStyles.None CultureInfo.InvariantCulture x

        static member parseExactWithOptions style provider (formats: string[]) x =
            match DateTime.TryParseExact(x, formats, provider, style) with
            | true,v -> Some v
            | _ -> None

        static member parseExact formats x =
            DateTime.parseExactWithOptions DateTimeStyles.None CultureInfo.InvariantCulture formats x

    type DateTimeOffset with
        static member parseWithOptions style provider x =
            match DateTimeOffset.TryParse(x, provider, style) with
            | true,v -> Some v
            | _ -> None
            
        static member parse x = 
            DateTimeOffset.parseWithOptions DateTimeStyles.None CultureInfo.InvariantCulture x

        static member parseExactWithOptions style provider (formats: string[]) x =
            match DateTimeOffset.TryParseExact(x, formats, provider, style) with
            | true,v -> Some v
            | _ -> None

        static member parseExact formats x =
            DateTimeOffset.parseExactWithOptions DateTimeStyles.None CultureInfo.InvariantCulture formats x

    // Active patterns
    let (|Boolean|_|) = Boolean.parse
    let (|Int32|_|) = Int32.parse
    let (|Double|_|) = Double.parse


    // Typeclasses

    open System.Collections.Generic

    // Functor
    type Fmap = Fmap with
        static member (?<-) (_, _Functor:Fmap, x:option<_>) = fun f -> Option.map f x
        static member (?<-) (_, _Functor:Fmap, x:list<_>  ) = fun f -> List.map   f x
        static member (?<-) (_, _Functor:Fmap, g:_->_     ) = (>>) g
    let inline fmap f x = (() ? (Fmap) <- x) f

    // Monad
    type Return = Return with
        static member (?<-) (_, _Monad:Return, _:'a option) = fun (x:'a) -> Some x        
        static member (?<-) (_, _Monad:Return, _:'a list  ) = fun (x:'a) -> [x]
        static member (?<-) (_, _Monad:Return, _: _ -> 'a ) = fun (x:'a) -> konst x        
    let inline return' x : ^R = (() ? (Return) <- Unchecked.defaultof< ^R> ) x

    type Bind = Bind with
        static member (?<-) (x:option<_>, _Monad:Bind,_:'b option) = fun f -> Option.bind  f x
        static member (?<-) (x:list<_>  , _Monad:Bind,_:'b list  ) = fun f -> List.collect f x
        static member (?<-) (f:'e->'a   , _Monad:Bind,_:'e->'b   ) = fun (k:'a->'e->'b) r -> k (f r) r
    let inline (>>=) x f : ^R = (x ? (Bind) <- Unchecked.defaultof< ^R> ) f

    type DoNotationBuilder() =
        member inline b.Return(x)    = return' x
        member inline b.Bind(p,rest) = p >>= rest
        member        b.Let (p,rest) = rest p
        member    b.ReturnFrom(expr) = expr
    let do' = new DoNotationBuilder()
    
    // Utility functions for Monad

    let inline sequence ms =
        let k m m' = m >>= fun (x:'a) -> m' >>= fun xs -> (return' :list<'a> -> ^M) (List.Cons(x,xs))
        List.foldBack k ms ((return' :list<'a> -> ^M) [])

    let inline mapM f as' = sequence (List.map f as')

    let inline liftM  f m1    = m1 >>= (return' << f)
    let inline liftM2 f m1 m2 = m1 >>= fun x1 -> m2 >>= fun x2 -> return' (f x1 x2)
    let inline when'  p s     = if p then s else return' ()
    let inline unless p s     = when' (not p) s
    let inline ap     x y     = liftM2 id x y

    let inline (>=>)  f g x   = f x >>= g


    // Monoid
    type Mempty = Mempty with    
        static member        (?<-) (_, _Monoid:Mempty, _:'a list  ) = []    
        static member        (?<-) (_, _Monoid:Mempty, _:'a option) = None
        static member        (?<-) (_, _Monoid:Mempty, _:'a[]     ) = [||]
        static member        (?<-) (_, _Monoid:Mempty, _:string   ) = ""
        static member        (?<-) (_, _Monoid:Mempty, _:unit     ) = ()
        static member inline (?<-) (_, _Monoid:Mempty, _: ^A * ^B ) = (() ? (Mempty) <- Unchecked.defaultof< ^A>) , (() ? (Mempty) <- Unchecked.defaultof< ^B>)

    let inline mempty() : ^R = (() ? (Mempty) <- Unchecked.defaultof< ^R>)


    type Mappend = Mappend with        
        static member        (?<-) (x:list<_>  , _Monoid:Mappend, y      ) = List.append  x y        
        static member inline (?<-) (x:option<_>, _Monoid:Mappend, y      ) = 
            match (x,y) with
            | (Some a,Some b) -> Some (a ? (Mappend) <- b)
            | (Some a,None  ) -> Some a
            | (None  ,Some b) -> Some b
            | _               -> None
        static member        (?<-) (x:_[]      , _Monoid:Mappend, y      ) = Array.append x y
        static member        (?<-) (x:string   , _Monoid:Mappend, y      ) = x + y
        static member        (?<-) (()         , _Monoid:Mappend, _:unit ) = ()    
        static member inline (?<-) ((x1,x2)    , _Monoid:Mappend, (y1,y2)) = (x1 ? (Mappend) <- y1) , (x2 ? (Mappend) <- y2)
    
    let inline mappend (x:'a) (y:'a) : 'a = x ? (Mappend) <- y


    let inline mconcat x =
        let foldR f s lst = List.foldBack f lst s
        foldR mappend (mempty()) x


    // Applicative
    type Pure = Pure with
        member inline this.Base x = return' x
        static member (?<-) (_, _Applicative:Pure, _:'a option) = fun (x:'a) -> Pure.Pure.Base x :'a option        
        static member (?<-) (_, _Applicative:Pure, _:'a list  ) = fun (x:'a) -> Pure.Pure.Base x :'a list
        static member (?<-) (_, _Applicative:Pure, _: _ -> 'a ) = konst
    let inline pure' x : ^R = (() ? (Pure) <- Unchecked.defaultof< ^R> ) x


    type Ap = Ap with
        member inline this.Base f x = ap f x
        static member (?<-) (f:option<_>, _Applicative:Ap, x:option<_>) = Ap.Ap.Base f x
        static member (?<-) (f:list<_>  , _Applicative:Ap, x:list<_>  ) = Ap.Ap.Base f x : list<_>
        static member (?<-) (f:_ -> _   , _Applicative:Ap, g: _ -> _  ) = fun x ->   f x (g x)
    let inline (<*>) x y : ^R = (x ? (Ap) <- y)