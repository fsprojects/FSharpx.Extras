#r @"../bin/FSharpx.Extras.dll"
#r @"../bin/FSharpx.Collections.dll"

open FSharpx

// Sample monoid implementation
let addition = Monoid.sum()

// Using monoids computation expression capabilities
let x : float =
    addition {
        yield 1.0
        yield 5.0
        yield 6.0
    }

// Getting the zero
let zero : float =
    addition {
        ()
    }