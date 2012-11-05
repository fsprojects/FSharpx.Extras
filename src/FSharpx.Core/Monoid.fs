namespace FSharpx

type ISemigroup<'a> =
    /// <summary>
    /// Associative operation
    /// </summary>
    abstract Combine : 'a * 'a -> 'a
    
/// Monoid (associative binary operation with identity)
/// The monoid implementation comes from Matthew Podwysocki's http://codebetter.com/blogs/matthew.podwysocki/archive/2010/02/01/a-kick-in-the-monads-writer-edition.aspx.
[<AbstractClass>]
type Monoid<'a>() as m =
    /// <summary>
    /// Identity
    /// </summary>
    abstract Zero : unit -> 'a

    /// <summary>
    /// Associative operation
    /// </summary>
    abstract Combine : 'a * 'a -> 'a

    /// <summary>
    /// Fold a list using this monoid
    /// </summary>
    abstract Concat : 'a seq -> 'a
    default x.Concat a = x.For(a, id)

    abstract For: 'a seq * ('a -> 'a) -> 'a
    default x.For(sequence, body) =
        let combine a b = x.Combine(a, body b)
        Seq.fold combine (x.Zero()) sequence

    member x.Yield a = a
    member x.Delay f = f()

    interface ISemigroup<'a> with
        member x.Combine(a,b) = m.Combine(a,b)

module Monoid =

    /// The dual of a monoid, obtained by swapping the arguments of 'Combine'.
    type DualMonoid<'a>(m: 'a Monoid) =
        inherit Monoid<'a>()
            override this.Zero() = m.Zero()
            override this.Combine(a,b) = m.Combine(b,a)

    type Tuple2Monoid<'a,'b>(a: 'a Monoid, b: 'b Monoid) =
        inherit Monoid<'a * 'b>()
            override this.Zero() = a.Zero(), b.Zero()
            override this.Combine((a1,b1), (a2,b2)) = a.Combine(a1, a2), b.Combine(b1, b2)

    type Tuple3Monoid<'a,'b,'c>(a: 'a Monoid, b: 'b Monoid, c: 'c Monoid) =
        inherit Monoid<'a * 'b * 'c>()
            override this.Zero() = a.Zero(), b.Zero(), c.Zero()
            override this.Combine((a1,b1,c1), (a2,b2,c2)) =
                a.Combine(a1, a2), b.Combine(b1, b2), c.Combine(c1, c2)
            
    /// Monoid (int,0,+)
    let IntSumMonoid = 
        { new Monoid<int>() with
            override this.Zero() = 0
            override this.Combine(a,b) = a + b }

    /// Monoid (int,1,*)
    let IntProductMonoid =
        { new Monoid<int>() with
            override this.Zero() = 1
            override this.Combine(a,b) = a * b }

    let StringMonoid =
        { new Monoid<string>() with
            override this.Zero() = ""
            override this.Combine(a,b) = a + b }

    let AllMonoid =
        { new Monoid<bool>() with
            override this.Zero() = true
            override this.Combine(a,b) = a && b }

    let AnyMonoid = 
        { new Monoid<bool>() with
            override this.Zero() = false
            override this.Combine(a,b) = a || b }

    // doesn't compile due to this F# bug http://stackoverflow.com/questions/4485445/f-interface-inheritance-failure-due-to-unit
    (*
    let UnitMonoid =
        { new Monoid<unit>() with
            override this.Zero() = ()
            override this.Combine(_,_) = () }
    *)

