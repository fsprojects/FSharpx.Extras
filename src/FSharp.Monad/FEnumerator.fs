module FSharp.Monad.MinLinq.Imperative

open System.Collections.Generic

/// The Imperative monad, taken from http://tomasp.net/articles/imperative-i-return.aspx.
/// This can also pass as the FEnumerator monad.
type Imperative<'a> = unit -> 'a option

type ImperativeBuilder() =
  // Create computation that returns the given value  
  member x.Return(v) : Imperative<_> = fun () -> Some(v)
  // Create computation that doesn't return any value
  member x.Zero() = fun () -> None

  // Return a computation that will evaluate the provided function  
  // only when the computation is being evaluated
  member x.Delay(f:unit -> Imperative<_>) =  fun () -> f()()

  // Combines two delayed computations (that may return 
  // value imperatively using 'return') into one  
  member x.Combine(a, b) = fun () ->
    // run the first part of the computation
    match a() with 
    // if it returned, we can return the result immediately
    | Some(v) -> Some(v) 
    // otherwise, we need to run the second part
    | _ -> b()

  // Execute the imperative computation 
  // expression given as an argument
  member x.Run(imp) = 
    // run the computation and return the result or 
    // fail when the computation didn't return anything
    match imp() with 
    | Some(v) -> v 
    | None -> failwith "nothing returned!"

  member x.For(inp:seq<_>, f) =
    // Process next element from the sequence
    let rec loop(en:IEnumerator<_>) = 
      // If there are no more elements, return empty computation
      if not(en.MoveNext()) then x.Zero() else
        // Otherwise call body and combine it with a 
        // computation that continues looping
        x.Combine(f(en.Current), x.Delay(fun () -> loop(en)))
    // Start enumerating from the first element
    loop(inp.GetEnumerator())
  
  member x.While(gd, body) = 
    // Perform one step of the 'looping'
    let rec loop() =
      // If the condition is false, return empty computation
      if not(gd()) then x.Zero() else
        // Otherwise, call body and then loop again
        x.Combine(body, x.Delay(fun () -> loop()))
    loop()

let fenumerator = ImperativeBuilder()
