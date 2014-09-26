// First version copied from the F# Power Pack 
// https://raw.github.com/fsharp/powerpack/master/src/FSharp.PowerPack/AsyncOperations.fs

// (c) Microsoft Corporation 2005-2009. 
namespace FSharpx.Control

    open System
    open System.Threading
    open FSharpx.Functional

    /// Represents the reified result of an asynchronous computation
    [<NoEquality; NoComparison>]
    type AsyncResult<'T>  =
        |   AsyncOk of 'T
        |   AsyncException of exn
        |   AsyncCanceled of OperationCanceledException

        static member Commit(res:AsyncResult<'T>) = 
            Async.FromContinuations (fun (cont,econt,ccont) -> 
                   match res with 
                   | AsyncOk v -> cont v 
                   | AsyncException exn -> econt exn 
                   | AsyncCanceled exn -> ccont exn)

    /// When using .NET 4.0 you can replace this type by Task<'T>
    [<Sealed>]
    type AsyncResultCell<'T>() =
        let mutable result = None
        // The continuation for the result, if any
        let mutable savedConts = []
        
        let syncRoot = new obj()
                

        // Record the result in the AsyncResultCell.
        // Ignore subsequent sets of the result. This can happen, e.g. for a race between 
        // a cancellation and a success.
        member x.RegisterResult (res:AsyncResult<'T>,?reuseThread) =
            let grabbedConts = 
                lock syncRoot (fun () ->
                    if result.IsSome then  
                        []
                    else
                        result <- Some res;
                        // Invoke continuations in FIFO order 
                        // Continuations that Async.FromContinuations provide do QUWI/SynchContext.Post, 
                        // so the order is not overly relevant but still.                        
                        List.rev savedConts)
            // Run continuations outside the lock
            let reuseThread = defaultArg reuseThread false
            match grabbedConts with
            |   [] -> ()
            |   [cont] when reuseThread -> cont res
            |   otherwise ->
#if FX_NO_SYNC_CONTEXT
                    let postOrQueue cont = ThreadPool.QueueUserWorkItem(fun _ -> cont res) |> ignore
#else
                    let synchContext = System.Threading.SynchronizationContext.Current
                    let postOrQueue =
                        match synchContext with
                        |   null -> fun cont -> ThreadPool.QueueUserWorkItem(fun _ -> cont res) |> ignore
                        |   sc -> fun cont -> sc.Post((fun _ -> cont res), state=null)
#endif                        
                    grabbedConts |> List.iter postOrQueue

        /// Get the reified result 
        member private x.AsyncPrimitiveResult =
            Async.FromContinuations(fun (cont,_,_) -> 
                let grabbedResult = 
                    lock syncRoot (fun () ->
                        match result with
                        | Some res -> 
                            result
                        | None ->
                            // Otherwise save the continuation and call it in RegisterResult
                            savedConts <- cont::savedConts
                            None)
                // Run the action outside the lock
                match grabbedResult with 
                | None -> ()
                | Some res -> cont res) 
                           

        /// Get the result and commit it
        member x.AsyncResult =
            async { let! res = x.AsyncPrimitiveResult
                    return! AsyncResult.Commit(res) }


    [<AutoOpen>]
    module FileExtensions =

        let UnblockViaNewThread f =
            async { do! Async.SwitchToNewThread ()
                    let res = f()
                    do! Async.SwitchToThreadPool ()
                    return res }

        
        let private LinesToBytes ((lines:string array), encoder) =
            use memStrm = new System.IO.MemoryStream()
            use sWriter = new System.IO.StreamWriter(memStrm, encoder)
            lines |> Array.iter (fun line -> sWriter.WriteLine(line))
            do sWriter.Flush()
            memStrm.ToArray()

        type System.IO.File with
            static member AsyncOpenText(path)   = UnblockViaNewThread (fun () -> System.IO.File.OpenText(path))
            static member AsyncAppendText(path) = UnblockViaNewThread (fun () -> System.IO.File.AppendText(path))
            static member AsyncOpenRead(path)   = UnblockViaNewThread (fun () -> System.IO.File.OpenRead(path))
            static member AsyncOpenWrite(path)  = UnblockViaNewThread (fun () -> System.IO.File.OpenWrite(path))
#if FX_NO_FILE_OPTIONS
            static member AsyncOpen(path,mode,?access,?share,?bufferSize) =
#else
            static member AsyncOpen(path,mode,?access,?share,?bufferSize,?options) =
#endif
                let access = match access with Some v -> v | None -> System.IO.FileAccess.ReadWrite
                let share = match share with Some v -> v | None -> System.IO.FileShare.None
#if FX_NO_FILE_OPTIONS
#else
                let options = match options with Some v -> v | None -> System.IO.FileOptions.None
#endif
                let bufferSize = match bufferSize with Some v -> v | None -> 0x1000
                UnblockViaNewThread (fun () -> 
#if FX_NO_FILE_OPTIONS
                    new System.IO.FileStream(path,mode,access,share,bufferSize))
#else
                    new System.IO.FileStream(path,mode,access,share,bufferSize, options))
#endif

            static member OpenTextAsync(path)   = System.IO.File.AsyncOpenText(path)
            static member AppendTextAsync(path) = System.IO.File.AsyncAppendText(path)
            static member OpenReadAsync(path)   = System.IO.File.AsyncOpenRead(path)
            static member OpenWriteAsync(path)  = System.IO.File.AsyncOpenWrite(path)
#if FX_NO_FILE_OPTIONS
            static member OpenAsync(path,mode,?access,?share,?bufferSize) = 
                System.IO.File.AsyncOpen(path, mode, ?access=access, ?share=share,?bufferSize=bufferSize)
#else
            static member OpenAsync(path,mode,?access,?share,?bufferSize,?options) = 
                System.IO.File.AsyncOpen(path, mode, ?access=access, ?share=share,?bufferSize=bufferSize,?options=options)
#endif

#if FX_NO_FILE_OPTIONS
#else

            // Aims to take advantage of IO completion ports using FileStream.AsyncWrite and FileOptions.Asynchronous, so no FX_NO_FILE_OPTIONS version
            static member AsyncWriteAllBytes(path, bytes) =
                let bufferSize = 4096 // as per File.WriteAllBytes
                async{
                    use! fs = System.IO.File.AsyncOpen( path, System.IO.FileMode.Create, System.IO.FileAccess.Write, System.IO.FileShare.Read, bufferSize, System.IO.FileOptions.Asynchronous)
                    let! ret = fs.AsyncWrite bytes
                    return ret
                }

            static member AsyncWriteAllText(path, (txt:string), ?encoder) =
                let enc = match encoder with Some e -> e | None -> System.Text.Encoding.Default
                async{
                    let bs = enc.GetBytes txt
                    return! System.IO.File.AsyncWriteAllBytes(path, bs)
                }
            
            // Different memory profile to File.WriteAllLines, converts all lines to a byte[] before doing any writing, then a single write op.
            // Not good for writing huge files, but writing one line at a time, as per File.WriteAllLines has its own issues, 
            // e.g. sequential application of async IO (so lines are written in order)
            static member AsyncWriteAllLines(path, (lines:string array), ?encoder) = 
                let enc = match encoder with Some e -> e | None -> System.Text.Encoding.Default
                async{
                    let bs = LinesToBytes (lines, enc)
                    return! System.IO.File.AsyncWriteAllBytes(path, bs)
              }

            // Aims to take advantage of IO completion ports using FileStream.AsyncWrite and FileOptions.Asynchronous, so no FX_NO_FILE_OPTIONS version
            static member AsyncAppendAllBytes(path, bytes) =
                let bufferSize = 4096 // as per File.WriteAllBytes
                async{
                    use! fs = System.IO.File.AsyncOpen( path, System.IO.FileMode.Append, System.IO.FileAccess.Write, System.IO.FileShare.Read, bufferSize, System.IO.FileOptions.Asynchronous)
                    let! ret = fs.AsyncWrite bytes
                    return ret
                }

            static member AsyncAppendAllText(path, (txt:string), ?encoder) =
                let enc = match encoder with Some e -> e | None -> System.Text.Encoding.Default
                async{
                    let bs = enc.GetBytes txt
                    return! System.IO.File.AsyncAppendAllBytes(path, bs)
                }
            
            // Different memory profile to File.AppendAllLines, converts all lines to a byte[] before doing any writing, then a single write op.
            // Not good for writing huge files, but writing one line at a time, as per File.AppendAllLines has its own issues, 
            // e.g. sequential application of async IO (so lines are written in order)
            static member AsyncAppendAllLines(path, (lines:string array), ?encoder) = 
                let enc = match encoder with Some e -> e | None -> System.Text.Encoding.Default
                async{
                    let bs = LinesToBytes (lines, enc)
                    return! System.IO.File.AsyncAppendAllBytes(path, bs)
              }



#endif







    [<AutoOpen>]
    module StreamReaderExtensions =
        type System.IO.StreamReader with

            member s.AsyncReadToEnd () = FileExtensions.UnblockViaNewThread (fun () -> s.ReadToEnd())
            member s.ReadToEndAsync () = s.AsyncReadToEnd ()

#if FX_NO_WEB_REQUESTS
#else
    [<AutoOpen>]
    module WebRequestExtensions =
        open System
        open System.Net
        open Microsoft.FSharp.Control.WebExtensions

        let callFSharpCoreAsyncGetResponse (req: System.Net.WebRequest) = req.AsyncGetResponse()
        
        type System.Net.WebRequest with
            member req.AsyncGetResponse() = callFSharpCoreAsyncGetResponse req // this calls the FSharp.Core method
            member req.GetResponseAsync() = callFSharpCoreAsyncGetResponse req // this calls the FSharp.Core method
#endif
     
#if FX_NO_WEB_CLIENT
#else
    [<AutoOpen>]
    module WebClientExtensions =
        open System.Net
        open Microsoft.FSharp.Control.WebExtensions
        open System.ComponentModel
        open FSharpx
        
        let callFSharpCoreAsyncDownloadString (req: System.Net.WebClient) address = req.AsyncDownloadString address

        let fromEventPattern (event : IEvent<_, #AsyncCompletedEventArgs>) start result cancel =
            async {
                let userToken = new obj()
                start userToken

                let rec loop() =
                    async {
                        let! args = Async.AwaitEvent(event, cancel)
                        if args.UserState <> userToken then
                            return! loop()
                        else
                            let asyncResult =
                                if args.Cancelled then
                                    AsyncCanceled (new OperationCanceledException())
                                elif args.Error <> null then AsyncException args.Error
                                else  AsyncOk (result args)
                            return! AsyncResult.Commit(asyncResult)
                    }
                return! loop()
            }

        type WebClient with

            member private this.buildAsyncAction(event, start, result) =
                fromEventPattern event start result (fun () -> this.CancelAsync())

            member inline private this.buildAsyncAction<'t, 'a, 'd when 'd : delegate<'a, unit> and 'd :> Delegate and
                                                                        ^a : (member get_Result : unit -> 't) and
                                                                        'a :> AsyncCompletedEventArgs> (event: IEvent<'d, 'a>, start) =
                let result args = (^a : (member get_Result : unit -> 't) args)
                this.buildAsyncAction(event, start, result)

            member this.AsyncUploadValues(address, data) = this.AsyncUploadValues(address, null, data)

            member this.AsyncUploadValues(address, uploadMethod, data) =
                this.buildAsyncAction(
                    this.UploadValuesCompleted,
                    (fun token -> this.UploadValuesAsync(address, uploadMethod, data, token)))

            member this.AsyncUploadString(address, data) = this.AsyncUploadString(address, null, data)

            member this.AsyncUploadString(address, uploadMethod, data) =
                this.buildAsyncAction(
                    this.UploadStringCompleted,
                    (fun token -> this.UploadStringAsync(address, uploadMethod, data, token)))

            member this.AsyncUploadFile(address, fileName) = this.AsyncUploadFile(address, null, fileName)

            member this.AsyncUploadFile(address, uploadMethod, fileName) =
                this.buildAsyncAction(
                    this.UploadFileCompleted,
                    (fun token -> this.UploadFileAsync(address, uploadMethod, fileName, token)))

            member this.AsyncUploadData(address, data) = this.AsyncUploadData(address, null, data)

            member this.AsyncUploadData(address, uploadMethod, data) =
                this.buildAsyncAction(
                    this.UploadDataCompleted,
                    (fun token -> this.UploadDataAsync(address, uploadMethod, data, token)))

            member this.AsyncOpenWrite address = this.AsyncOpenWrite(address, null)

            member this.AsyncOpenWrite(address, uploadMethod) =
                this.buildAsyncAction(
                    this.OpenWriteCompleted,
                    (fun token -> this.OpenWriteAsync(address, uploadMethod, token)))

            member this.AsyncOpenRead address =
                this.buildAsyncAction(
                    this.OpenReadCompleted,
                    (fun token -> this.OpenReadAsync(address, token)))

            member this.AsyncDownloadFile(address, fileName) =
                this.buildAsyncAction(
                    this.DownloadFileCompleted,
                    (fun token -> this.DownloadFileAsync(address, fileName, token)),
                    konst ())

            member this.AsyncDownloadData address =
                this.buildAsyncAction(
                    this.DownloadDataCompleted,
                    (fun token -> this.DownloadDataAsync(address, token)))
#endif

