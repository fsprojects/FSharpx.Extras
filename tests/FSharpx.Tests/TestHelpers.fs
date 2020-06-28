module FSharpx.Tests.TestHelpers
type internal DisposeChecker() =
  let mutable disposed = false
  member _.Disposed = disposed
  interface System.IDisposable with
    member _.Dispose() =
      disposed <- true
