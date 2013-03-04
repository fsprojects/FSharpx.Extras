module FSharpx.Tests.TaskBuilderTests

open System
open System.IO
open System.Net
open NUnit.Framework
open FSharpx
open System.Threading

#if NET40
open System.Threading.Tasks
open Microsoft.FSharp.Control.FileExtensions
open Microsoft.FSharp.Control.StreamReaderExtensions
open Microsoft.FSharp.Control.WebExtensions

let checkSuccess (expected: 'a) (t: CancellationToken -> Task<'a>) =
    match Task.run (fun () -> (t CancellationToken.None)) with
    | Task.Canceled -> Assert.Fail("Task should have been successful, but was canceled")
    | Task.Error e -> Assert.Fail("Task should have been successful, but errored with exception {0}", e)
    | Task.Successful a -> Assert.AreEqual(expected, a)     

let checkCancelledWithToken (cts: CancellationTokenSource) (t: CancellationToken -> Task<'a>) =    
    cts.Cancel()
    match Task.run (fun () -> t cts.Token) with
    | Task.Canceled -> ()
    | Task.Error e -> Assert.Fail("Task should have been canceled, but errored with exception {0}", e)
    | Task.Successful a -> Assert.Fail("Task should have been canceled, but succeeded with result {0}", a)

let checkCancelled (t: CancellationToken -> Task<'a>) =
    use cts = new CancellationTokenSource()
    checkCancelledWithToken cts t

[<Test>]
let ``task should return the right value after let!``() =
    let task = Task.TaskBuilderWithToken()
    let t = 
        task {
            let! v = Task.Factory.StartNew(fun () -> 100)
            return v
        }
    checkSuccess 100 t

[<Test>]
let ``task should return the right value after return!``() =
    let task = Task.TaskBuilderWithToken()
    let t =
        task {
            return! Task.Factory.StartNew(fun () -> "hello world")
        }    
    checkSuccess "hello world" t

[<Test>]
let ``exception in task``() =
    let task = Task.TaskBuilderWithToken(continuationOptions = TaskContinuationOptions.ExecuteSynchronously)
    let t = 
        task {
            failwith "error"
        }
    match Task.run (fun () -> t CancellationToken.None) with
    | Task.Error e -> Assert.AreEqual("error", e.InnerException.Message)
    | _ -> Assert.Fail "task should have errored"

[<Test>]
let ``canceled task``() =
    let task = Task.TaskBuilderWithToken()
    let cts = new CancellationTokenSource()
    let t = 
        task {
            cts.Token.ThrowIfCancellationRequested()
        }
    checkCancelledWithToken cts t

[<Test>]
let ``canceled task 2``() =    
    let task = Task.TaskBuilderWithToken()
    let t = 
        task {
            let! v = Task.Factory.StartNew(fun () -> 0)
            return ()
        }
    checkCancelled t

[<Test>]
let ``return should return value``() =    
    let task = Task.TaskBuilderWithToken()
    let t = 
        task {            
            return 100
        }
    checkSuccess 100 t  

[<Test>]
let ``return! should accept task parametrized by CancellationToken``() =
    let task = Task.TaskBuilderWithToken()
    let t1 = 
        task {
            let! v = Task.Factory.StartNew(fun () -> 100)
            return v
        }

    let t2 = 
        task {            
            return! t1
        }
    checkSuccess 100 t2    

[<Test>]
let ``bind should chain two tasks``() =
    let task = Task.TaskBuilderWithToken()
    let t = 
        task {
            let! v1 = Task.Factory.StartNew(fun () -> 100)
            let! v2 = Task.Factory.StartNew(fun () -> v1 + 1)
            return v2
        }    
    checkSuccess 101 t

[<Test>]
let ``bind should chain two tasks parametrized by CancellationToken``() =
    let task = Task.TaskBuilderWithToken()
    let t1 = 
        task {
            return! Task.Factory.StartNew(fun () -> 100)
        }
    let t2 = 
        task {
            return! Task.Factory.StartNew(fun () -> 200)
        }
    let t = 
        task {
            let! v1 = t1
            let! v2 = t2
            return v1 + v2
        }
    checkSuccess 300 t

[<Test>]
let ``task should be delayed``() =
    let i = ref 0
    let task = Task.TaskBuilderWithToken()
    let t = 
        task {
            let body() =
                incr i
                "hello world"
            let! v = Task.Factory.StartNew(body)
            return v
        }    
    Assert.AreEqual(0, !i)
    checkSuccess "hello world" t
    Assert.AreEqual(1, !i)

let whileExpression (i: ref<int>) = 
    let task = Task.TaskBuilderWithToken(continuationOptions = TaskContinuationOptions.ExecuteSynchronously)    
    task {
        while !i > 0 do
            decr i
            do! Task.Factory.StartNew ignore
    }

[<Test>]
let ``while``() =
    let i = ref 10        
    let t = whileExpression i
    Task.run (fun () -> t CancellationToken.None)  |> ignore
    Assert.AreEqual(0, !i)


[<Test>]
let ``cancel while``() = 
    let i = ref 10
    let t = whileExpression i    
    checkCancelled t
    Assert.AreEqual(10, !i)

let tryFinallyExpression (i: ref<int>) = 
    let task = Task.TaskBuilderWithToken()
    task {
        let! v1 = Task.Factory.StartNew(fun () -> 100)
        try
            let! v2 = Task.Factory.StartNew(fun () -> v1 + 1)
            return v2
        finally
            incr i
    }

[<Test>]
let ``try finally should execute finally block``() =
    let i = ref 0    
    let t = tryFinallyExpression i
    checkSuccess 101 t
    Assert.AreEqual(1, !i)    

[<Test>]
let ``try finally should be cancellable``() =
    let i = ref 0    
    let t = tryFinallyExpression i    
    checkCancelled t 
    Assert.AreEqual(0, !i)

[<Test>]
let ``for``() =
    let i = ref 0
    let s = [1; 2; 3; 4]
    let task = Task.TaskBuilderWithToken(continuationOptions = TaskContinuationOptions.ExecuteSynchronously)
    let t =
        task {
            for x in s do
                i := !i + x
                do! Task.Factory.StartNew ignore
        }
    Task.run (fun () -> t CancellationToken.None) |> ignore
    Assert.AreEqual(10, !i)

#endif


