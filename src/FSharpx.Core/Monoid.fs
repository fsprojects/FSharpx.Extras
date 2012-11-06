namespace FSharpx

type ISemigroup<'a> =
    /// <summary>
    /// Associative operation
    /// </summary>
    abstract Combine : 'a * 'a -> 'a
    
/// Monoid (associative binary operation with identity)
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
    open System

    /// The dual of a monoid, obtained by swapping the arguments of 'Combine'.
    let dual (m: _ Monoid) =
        { new Monoid<_>() with            
            override this.Zero() = m.Zero()
            override this.Combine(a,b) = m.Combine(b,a) }

    let tuple2 (a: _ Monoid) (b: _ Monoid) =
        { new Monoid<_ * _>() with
            override this.Zero() = a.Zero(), b.Zero()
            override this.Combine((a1,b1), (a2,b2)) = a.Combine(a1, a2), b.Combine(b1, b2) }

    let tuple3 (a: 'a Monoid) (b: 'b Monoid) (c: 'c Monoid) =
        { new Monoid<_ * _ * _>() with
            override this.Zero() = a.Zero(), b.Zero(), c.Zero()
            override this.Combine((a1,b1,c1), (a2,b2,c2)) =
                a.Combine(a1, a2), b.Combine(b1, b2), c.Combine(c1, c2) }
            
    /// Monoid (int,0,+)
    let intSum = 
        { new Monoid<int>() with
            override this.Zero() = 0
            override this.Combine(a,b) = a + b }

    /// Monoid (int,1,*)
    let intProduct =
        { new Monoid<int>() with
            override this.Zero() = 1
            override this.Combine(a,b) = a * b }

    let string =
        { new Monoid<string>() with
            override this.Zero() = ""
            override this.Combine(a,b) = a + b }

    let all =
        { new Monoid<bool>() with
            override this.Zero() = true
            override this.Combine(a,b) = a && b }

    let any = 
        { new Monoid<bool>() with
            override this.Zero() = false
            override this.Combine(a,b) = a || b }

    let minInt =
        { new Monoid<_>() with
            override this.Zero() = Int32.MaxValue
            override this.Combine(a,b) = min a b }

    let maxInt =
        { new Monoid<_>() with
            override this.Zero() = Int32.MinValue
            override this.Combine(a,b) = max a b }

    // doesn't compile due to this F# bug http://stackoverflow.com/questions/4485445/f-interface-inheritance-failure-due-to-unit
    (*
    let UnitMonoid =
        { new Monoid<unit>() with
            override this.Zero() = ()
            override this.Combine(_,_) = () }
    *)

