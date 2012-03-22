FSharpx
=======

**FSharpx** is a library for the .NET platform implementing general functional constructs on top of the F# core library. 
Its main target is F# but it aims to be compatible with all .NET languages wherever possible.

It currently implements:

 * Several standard monads: State, Reader, Writer, Either, Continuation, Distribution
 * Validation applicative functor
 * General functions like flip
 * Additional functions around collections
 * Functions to make C# - F# interop easier
 * Async and Observable extensions


F# Async Extensions
===================

This library implements various extensions for asynchronous programming 
using F# asynchronous workflows and F# agents (the `MailboxProcessor` type
in the standard F# library). It defines _asynchronous sequences_ that represent
asynchronous operations returning multiple values (such as reading data from
a stream in chunks), several reusable F# agents and numerous extensions.

 * Samples that demonstrate how to use most of the extensions can
   be found in the [samples directory][7]

Asynchronous sequences
----------------------

Asynchronous sequences can be used to work with asynchronous computations that return
multiple results. A value of type `AsyncSeq<'T>` can be started (just like an asynchronous 
workflow) and it eventually returns. The result is either a special value representing
the end of the sequence or a value of type `'T` (head) together with the rest of the 
asynchronous sequence (tail) of type `AsyncSeq<'T>`.

Unlike `IObservable<'T>`, asynchronous sequences are not _push-based_. The code that 
generates the next value of the asynchronous sequence starts only after previous elements
have been processed. This makes it possible to easily write computations that return
results as long as some component is using them. 

However, `IObservable<'T>` values can
be converted to asynchronous sequences. The `AsyncSeq.ofObservable` combinator creates an
asynchronous sequence that discards values produced by the observable while the 
asynchronous sequence was blocked. The `AsyncSeq.ofObservableBuffered` combinator stores
all produced values in an unbounded buffer and returns the values from the buffer as soon
as the user of asynchronous sequence requestst the next element.

The library defines an F# computation expression for workfing with asynchronous sequences.
For example, sequence that emits numbers in 1 second intervals can be defined as follows:

    let rec numbers n = asyncSeq {
      yield n
      do! Async.Sleep(1000)
      yield! numbers (n + 1) }

Asynchronous workflows and asynchronous sequences can use the `for` construct to iterate
over all elements of an asynchronous sequence. For example:

    let rec evenNumbers = asyncSeq {
      for n in numbers 0 do
        if n%2=0 then yield n }

The library also provides numerous combinators (similar to functions from the `Seq` module).
The result of operations that aggregate values of an asynchronous sequence is an asynchronous
workflow that returns a single value:

    let rec sumTenEvenSquares = 
      numbers 0
      |> AsyncSeq.filter (fun n -> n%2 = 0)
      |> AsyncSeq.map (fun n -> n*n)
      |> AsyncSeq.fold (+) 0

    let n = 
      sumTenEvenSquares 
      |> Async.RunSynchronously

For some examples that use (earlier versions) of asynchronous sequences, see also the following
two F# snippets: [first][5] and [second][6].

Reusable agents
---------------

The library implements several reusable agents for building concurrent applications:

 * **Agent** is a simple type aliast for `MailboxProcessor` that is more convenient to use

 * **AutoCancelAgent** wraps the standard F# agent and adds support for stopping of the
   agent's body using the `IDisposable` interface (the type automatically creates a 
   cancellation token, uses it to start the underlying agent and cancels it when the agent 
   is disposed). For example, [see this F# snippet][1].

 * **BatchProcessingAgent** can be used to implement batch processing. It creates groups of 
   messages (added using the `Enqueue` method) and emits them using the `BatchProduced` 
   event. A group is produced when it reaches the maximal size or after the timeout elapses.

 * **BlockingQueueAgent** implements an asynchronous queue with blocking put and blocking 
   get operations. It can be used to implement the _producer-consumer_ concurrent pattern. 
   The constructor of the agent takes the maximal size of the buffer.


Observable extensions
---------------------

The library implements extensions for using `IObservable<'T>` type from F# asynchronous 
workflows. An overloaded extension method `Async.AwaitObservable` can be used to wait 
for an occurrence of an event (or other observable action):

    let counter n = async {
      printfn "Counting: %d" n
      let! _ = form.MouseDown |> Async.AwaitObservable
      return! counter (n + 1) }

Overloaded version of the method allows waiting for the first of multiple events. The 
method asynchronously returns `Choice<'T1, 'T2>` value that can be used to determine 
which of the events has occurred.

For examples using this method see Chapter 16 of [Real World Functional Programming][2] 
(some examples are available in a [free excerpt from the chapter][3]). The 
`Async.AwaitObservable` method should be used instead of `Async.AwaitEvent` to avoid 
memory leaks (see also related [StackOverflow discussion][4])


Type Providers
---------------------

FSharpx.TypeProviders is a library for the .NET platform implementing common type providers on top of the FSharpx.Core. 
 main target is F# 3.0 but it aims to be compatible with all .NET languages wherever possible.

It currently implements type safe variants of:

* Regex
* FileSystem
* Csv
* Excel
* JSON
* XML
* Registry
* XAML
* AppSettings.

Building the project
-----------------------

Due to missing fsc.exe and fsi.exe there is no automated build for the type providers project at the moment.
The nuget package is created by the following steps:

* Build the application within Visual Studio 2011 Beta
* Copy the created dlls into /manualNuget/
* Fix the version no. in /manualNuget/FSharpx.TypeProviders.nuspec 
* Run .\lib\NuGet\NuGet.exe pack "manualNuget\FSharpx.TypeProviders.nuspec"

  [1]: http://fssnip.net/64
  [2]: http://manning.com/petricek
  [3]: http://dotnetslackers.com/articles/net/Programming-user-interfaces-using-f-sharp-workflows.aspx
  [4]: http://stackoverflow.com/questions/3701861/wait-for-any-event-of-multiple-events-simultaneously-in-f
  [5]: http://fssnip.net/1k
  [6]: http://fssnip.net/1Y
  [7]: http://github.com/fsharp/fsharpx/tree/master/samples
