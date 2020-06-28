// First version copied from the F# Power Pack 
// https://raw.github.com/fsharp/powerpack/master/src/FSharp.PowerPack.Linq/Linq.fsi

namespace FSharpx.Linq
    module ExtraHashCompare =
        /// An intrinsic for compiling <c>&lt;@ x <> y @&gt;</c> to expression trees
        val GenericNotEqualIntrinsic : 'T -> 'T -> bool
