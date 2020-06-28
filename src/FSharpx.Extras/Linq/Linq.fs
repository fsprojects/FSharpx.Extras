// First version copied from the F# Power Pack 
// https://raw.github.com/fsharp/powerpack/master/src/FSharp.PowerPack.Linq/Linq.fs

namespace FSharpx.Linq

#nowarn "1204"

module ExtraHashCompare =
    let GenericNotEqualIntrinsic<'T> (x:'T) (y:'T) : bool = not (Microsoft.FSharp.Core.LanguagePrimitives.HashCompare.GenericEqualityIntrinsic<'T> x y)
