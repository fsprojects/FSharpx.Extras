namespace FSharpx

/// Semigroup (set with associative binary operation)
type ISemigroup<'T> =
    /// <summary>
    /// Associative operation
    /// </summary>
    abstract Combine : 'T * 'T -> 'T
    
/// Monoid (associative binary operation with identity)
[<AbstractClass>]
type Monoid<'T>() as m =
    /// <summary>
    /// Identity
    /// </summary>
    abstract Zero : unit -> 'T

    /// Associative operation
    abstract Combine : 'T * 'T -> 'T

    /// Fold a list using this monoid
    abstract Concat : 'T seq -> 'T
    default x.Concat a = x.For(a, id)

    abstract For: 'T seq * ('T -> 'T) -> 'T
    default x.For(sequence, body) =
        let combine a b = x.Combine(a, body b)
        Seq.fold combine (x.Zero()) sequence

    member x.Yield a = a
    member x.Delay f = f()

    interface ISemigroup<'T> with
        member x.Combine(a,b) = m.Combine(a,b)

module Semigroup =
    let min<'T when 'T : comparison> = 
        { new ISemigroup<'T> with
            member x.Combine(a,b) = min a b }

    let max<'T when 'T : comparison> = 
        { new ISemigroup<'T> with
            member x.Combine(a,b) = max a b }

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

    let tuple3 (a: 'T Monoid) (b: 'b Monoid) (c: 'c Monoid) =
        { new Monoid<_ * _ * _>() with
            override this.Zero() = a.Zero(), b.Zero(), c.Zero()
            override this.Combine((a1,b1,c1), (a2,b2,c2)) =
                a.Combine(a1, a2), b.Combine(b1, b2), c.Combine(c1, c2) }

    /// Monoid (a,0,+)
    let inline sum() = 
        { new Monoid<_>() with
            override this.Zero() = LanguagePrimitives.GenericZero
            override this.Combine(a,b) = a + b }

    /// Monoid (a,1,*)
    let inline product() =
        { new Monoid<_>() with
            override this.Zero() = LanguagePrimitives.GenericOne
            override this.Combine(a,b) = a * b }

    /// Monoid (int,0,+)
    let sumInt : Monoid<int> = sum()

    /// Monoid (int,1,*)
    let productInt : Monoid<int> = product()

    let minInt =
        { new Monoid<_>() with
            override this.Zero() = Int32.MaxValue
            override this.Combine(a,b) = min a b }

    let maxInt =
        { new Monoid<_>() with
            override this.Zero() = Int32.MinValue
            override this.Combine(a,b) = max a b }

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

    let unit = 
        // can't write this as a direct Monoid object expression due to this F# bug http://stackoverflow.com/questions/4485445/f-interface-inheritance-failure-due-to-unit
        let inline create zero combine =
            { new Monoid<_>() with
                override this.Zero() = zero
                override this.Combine(a,b) = combine a b }
        create () (fun _ _ -> ())

    [<GeneralizableValue>]
    let endo<'T> = 
        { new Monoid<'T -> 'T>() with
            override this.Zero() = id
            override this.Combine(f,g) = f << g }