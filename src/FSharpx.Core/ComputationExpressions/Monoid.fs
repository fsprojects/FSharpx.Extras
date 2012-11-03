module FSharpx.Monoid
    
/// Monoid (associative binary operation with identity)
/// The monoid implementation comes from Matthew Podwysocki's http://codebetter.com/blogs/matthew.podwysocki/archive/2010/02/01/a-kick-in-the-monads-writer-edition.aspx.
[<AbstractClass>]
type Monoid<'a>() =
    /// <summary>
    /// Identity
    /// </summary>
    abstract member mempty : 'a

    /// <summary>
    /// Associative operation
    /// </summary>
    abstract member mappend : 'a -> 'a -> 'a

    /// <summary>
    /// Fold a list using this monoid
    /// </summary>
    abstract member mconcat : 'a seq -> 'a
    default x.mconcat a = Seq.fold x.mappend x.mempty a

/// List monoid
type ListMonoid<'a>() =
    inherit Monoid<'a list>()
        override this.mempty = []
        override this.mappend a b = a @ b

/// Option wrapper monoid
type OptionMonoid<'a>(m: 'a Monoid) =
    inherit Monoid<'a option>()
        override this.mempty = None
        override this.mappend a b = 
            match a,b with
            | Some a, Some b -> Some (m.mappend a b)
            | Some a, None   -> Some a
            | None, Some a   -> Some a
            | None, None     -> None
            
/// Monoid (int,0,+)
let IntSumMonoid = 
    { new Monoid<int>() with
        override this.mempty = 0
        override this.mappend a b = a + b }

/// Monoid (int,1,*)
let IntProductMonoid =
    { new Monoid<int>() with
        override this.mempty = 1
        override this.mappend a b = a * b }

let StringMonoid =
    { new Monoid<string>() with
        override this.mempty = ""
        override this.mappend a b = a + b }