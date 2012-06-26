namespace FSharpx

open System
open System.Collections
open System.Collections.Generic
#if NET40
open System.Diagnostics.Contracts
#endif
open System.Runtime.CompilerServices
            
module Enumerator = 
 
    let singleton a = 
        let state = ref 0
        { new IEnumerator<_> with 
            member self.Current = if (state.Value = 1) then a else invalidOp "!"
          interface System.Collections.IEnumerator with
            member self.Current = if (state.Value = 1) then box a else invalidOp "!"
            member self.MoveNext() = state := state.Value + 1; state.Value = 1
            member self.Reset() = state := 0
          interface System.IDisposable with 
            member self.Dispose() = () }
  
    let bind (f:_ -> IEnumerator<'b>) (a:IEnumerator<'a>) =
        if a.MoveNext() then f (Some(a.Current)) else f(None) 
   
    let combine (a:IEnumerator<_>) b = 
        let current = ref a
        let first = ref true
        { new IEnumerator<_> with 
            member self.Current = current.Value.Current
          interface System.Collections.IEnumerator with
            member self.Current = box current.Value.Current
            member self.MoveNext() = 
                if current.Value.MoveNext() then true 
                elif first.Value then 
                    current := b
                    first := false
                    current.Value.MoveNext()
                else false
            member self.Reset() = 
                a.Reset(); b.Reset()
                first := true; current := a
          interface System.IDisposable with 
            member self.Dispose() = a.Dispose(); b.Dispose() }
    
    let empty<'a> : IEnumerator<'a> = 
        { new IEnumerator<_> with 
            member self.Current = invalidOp "!"
          interface System.Collections.IEnumerator with
            member self.Current = invalidOp "!"
            member self.MoveNext() = false
            member self.Reset() = ()
          interface System.IDisposable with 
            member self.Dispose() = ()}
    
    let delay f = 
        let en : Lazy<IEnumerator<_>> = lazy f()
        { new IEnumerator<_> with 
            member self.Current = en.Value.Current
          interface System.Collections.IEnumerator with
            member self.Current = box en.Value.Current
            member self.MoveNext() = en.Value.MoveNext()
            member self.Reset() = en.Value.Reset()
          interface System.IDisposable with 
            member self.Dispose() = en.Value.Dispose() }
   
    let toSeq gen = 
        { new IEnumerable<'T> with 
              member x.GetEnumerator() = gen()
          interface IEnumerable with 
              member x.GetEnumerator() = (gen() :> IEnumerator) }

    type EnumerableBuilder() = 
        member x.Delay(f) = delay f
        member x.YieldFrom(a) = a
        member x.Yield(a) = singleton a
        member x.Bind(a:IEnumerator<'a>, f:_ -> IEnumerator<'b>) = bind f a
        member x.Combine(a, b) = combine a b
        member x.Zero() = empty<_>
    let iter = new EnumerableBuilder()    

    let head (en:IEnumerator<'a>) =
        if en.MoveNext() then en.Current
        else invalidOp "!"

    let firstOrDefault def (en:IEnumerator<'a>) =
        if en.MoveNext() then en.Current
        else def

    let rec lastOrDefault def (en:IEnumerator<'a>) =
        if en.MoveNext() then
            lastOrDefault en.Current en
        else def

    let last (en:IEnumerator<'a>) =
        if en.MoveNext() then
            lastOrDefault en.Current en
        else invalidOp "!"

    let length (en:IEnumerator<'a>) =
        let rec loop acc =
            if en.MoveNext() then loop (acc+1)
            else acc
        loop 0
  
    let skip n (en:IEnumerator<'a>) =
        if n = 0 then en
        else
            let rec loop acc =
                if not (en.MoveNext()) then empty<_>
                elif n = acc then en
                else loop (acc+1)
            loop 1
  
    let take n (en:IEnumerator<'a>) =
        if n = 0 then empty<_>
        else
            let rec loop acc = iter {
                let! v = en
                match v with
                | None -> ()
                | Some v' ->
                    yield v'
                    if acc = n then ()
                    else yield! loop (acc+1) }
            loop 1

    // Implementing the zip function for enumerators
    let rec zip xs ys = iter {
        let! x = xs
        let! y = ys
        match x, y with 
        | Some(x), Some(y) ->
            yield x, y
            yield! zip xs ys 
        | _ -> () }
   
    let rec map f en = iter {
        let! x = en
        match x with 
        | Some(x) -> yield f x
                     yield! map f en
        | _ -> () }               

    /// Scan progressively folds over the enumerator, returning a new enumerator
    /// that lazily computes each state.
    let rec scan f state (en : IEnumerator<_>) = iter {
        yield state
        if en.MoveNext() then
            let state' = f state en.Current
            yield! scan f state' en }

    let private scanWithPredicate f state pred (en : IEnumerator<_>) = iter {
        yield state
        if en.MoveNext() then
            let state' = f state en.Current
            if pred state' then
                yield! scan f state' en }

    /// Scan progressively folds over the enumerator, returning a new enumerator
    /// that lazily computes each state while the provided predicate is true.
    let scanWhile f state pred en = scanWithPredicate f state pred en

    /// Scan progressively folds over the enumerator, returning a new enumerator
    /// that lazily computes each state until the provided predicate is true.
    let scanUntil f state pred en = scanWithPredicate f state (not << pred) en
