namespace FSharpx
#nowarn "40"

open System
open System.Collections
open System.Collections.Generic

(*
# Iteratee

The *Iteratee* module, part of the [FSharpx](http://github.com/fsharp/fsharpx) library, provides a set of types and functions for building compositional, input processing components.

## System.IO.Stream-based processing

The System.IO.Stream type should be familiar to anyone who has ever worked with I/O in .NET. Streams are the primary abstraction available for working with streams of data, whether over the file system (FileStream) or network protocols (NetworkStream). Streams also have a nice support structure in the form of TextReaders and TextWriters, among other, similar types.

A common scenario for I/O processing is parsing an HTTP request message. Granted, most will rely on ASP.NET, HttpListener, or WCF to do this for them. However, HTTP request parsing has a lot of interesting elements that are useful for demonstrating problem areas in inefficient resource usage using other techniques. For our running sample, we'll focus on parsing the headers of the following HTTP request message:

    let httpRequest : byte [] = @"GET /some/uri HTTP/1.1
    Accept:text/html,application/xhtml+xml,application/xml
    Accept-Charset:ISO-8859-1,utf-8;q=0.7,*;q=0.3
    Accept-Encoding:gzip,deflate,sdch
    Accept-Language:en-US,en;q=0.8
    Cache-Control:max-age=0
    Connection:keep-alive
    Host:stackoverflow.com
    If-Modified-Since:Sun, 25 Sep 2011 20:55:29 GMT
    Referer:http://www.bing.com/search?setmkt=en-US&q=don't+use+IEnumerable%3Cbyte%3E
    User-Agent:Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/535.4 (KHTML, like Gecko) Chrome/16.0.889.0 Safari/535.4

    <!DOCTYPE HTML PUBLIC ""-//W3C//DTD HTML 4.01//EN"" ""http://www.w3.org/TR/html4/strict.dtd"">
    <html>
    <head>
    ...
    </head>
    <body>
    ...
    </body>
    </html>"B

Using the standard Stream processing apis, you might write something like the following:

    let rec readConsecutiveLines (reader:System.IO.StreamReader) cont =
        if reader.EndOfStream then cont []
        else let line = reader.ReadLine()
             if System.String.IsNullOrEmpty(line) then cont []
             else readConsecutiveLines reader (fun tail -> cont (line::tail))

    let readFromStream() =
        let sw = System.Diagnostics.Stopwatch.StartNew()
        let result =
            [ for _ in 1..10000 do 
                use stream = new System.IO.MemoryStream(httpRequest)
                use reader = new System.IO.StreamReader(stream)
                yield readConsecutiveLines reader id ]
        sw.Stop()
        printfn "Stream read %d in %d ms" result.Length sw.ElapsedMilliseconds

    readFromStream()

What might be the problems with this approach?

1. Blocking the current thread

This is a synchronous use of reading from a Stream. In fact, `StreamReader` can only be used in a synchronous fashion. There are no methods that even offer the Asynchronous Programming Model (APM). For that, you'll need to read from the `Stream` in chunks and find the line breaks yourself. We'll get to more on this in a few minutes.

2. Poor composition

First off, the sample above is performing side effects throughout, and side-effects don't compose. You might suggest using standard function composition to create a complete message parser, but what if you don't know ahead of time what type of request body you'll receive? Can you pause processing to select the appropriate processor? You could certainly address this with some conditional branching, but where exactly do you tie that into your current logic? Also, the current parser breaks on lines using the StreamReader's built in logic. If you want to do parsing on individual lines, where would you tie in that logic?

3. Memory consumption

In this example, we are not taking any care about our memory use. To that end, you might argue that we should just use `StreamReader.ReadToEnd()` and then break up the string using a compositional parser like [FParsec](http://http://www.quanttec.com/fparsec/). If we don't care about careful use of memory, that's actually quite a good idea. However, in most network I/O situations -- such as writing high-performance servers -- you really want to control your memory use very carefully. `StreamReader` does allow you to specify the chunk size, so it's not a complete case against using `StreamReader`, which is why this is a last-place argument. And of course, you can certainly go after `Stream.Read()` directly.

## IEnumerable-based processing

How can we add better compositionality and refrain from blocking the thread? One way others have solved this problem is through the use of iterators. A common example can be found on [Stack Overflow](http://stackoverflow.com/questions/2630359/convert-stream-to-ienumerable-if-possible-when-keeping-laziness). Iterators allow you to publish a lazy stream of data, either one element at a time or in chunks, and through LINQ or F#'s `Seq` module, chain together processors for the data.

    // Read the stream byte by byte
    let readStreamByByte (stream: System.IO.Stream) =
        seq { while true do
                let x = stream.ReadByte()
                if (int x) < 0 then ()
                else yield x }

    // Read the stream by chunks
    let readStreamByChunk chunkSize (stream: System.IO.Stream) =
        let buffer = Array.zeroCreate<byte> chunkSize
        seq { while true do
                let bytesRead = stream.Read(buffer, 0, chunkSize)
                if bytesRead = 0 then ()
                else yield buffer }

    // When you are sure you want text by lines
    let readStreamByLines (reader: System.IO.StreamReader) =
        seq { while not reader.EndOfStream do
                yield reader.ReadLine() }

Three options are presented. In each, I'm using a `Stream` or `StreamReader`, but you could just as easily replace those with a `byte[]`, `ArraySegment<byte>`, or `SocketAsyncEventArgs`. What could be wrong with these options?

1. Lazy, therefore resource contentious

Iterators are pull-based, so you'll only retrieve a chunk when requested. This sounds pretty good for your processing code, but it's not a very good situation for your sockets. If you have data coming in, you want to get it moved out as quickly as possible to free up your allocated threads and pinned memory buffers for more incoming or outgoing traffic.

2. Lazy, therefore non-deterministic

Each of these items is lazy; therefore, it's impossible to know when you can free up resources. Who owns the `Stream` or `StreamReader` passed into each method? When is it safe to free the resource? If used immediately within a function, you may be fine, but you'd also be just as well off if you used a `list` or `array` comprehension and blocked until all bytes were read. (The one possible exception might be when using a [co-routine style async pattern](http://tomasp.net/blog/csharp-async.aspx).)

## A fold, by any other name

Looking back to our example of parsing the headers of an HTTP request message, we most likely want to return not just a set of strings but an abstract syntax tree represented as an F# discriminated union. A perfect candidate for taking in our iterator above and producing the desired result is our old friend [`Seq.fold`](http://msdn.microsoft.com/en-us/library/ee353471.aspx).

    val fold : ('State -> 'a -> 'State) -> 'State -> seq<'a> -> 'State

The left fold is a very useful function. It equivalent to the [`Enumerable.Aggregate`](http://msdn.microsoft.com/en-us/library/system.linq.enumerable.aggregate.aspx) extension method in LINQ. This function takes in allows for the incremental creation of a result starting with a seed value.

Looks like we're done here. However, there are still problems with stopping here:

1. Composition

You can certainly use function composition to generate an appropriate state incrementing function, but you would still have the problem of being able to pause to delay selecting the appropriate message body processor.

2. Early termination

Suppose you really only ever want just the message headers. How would you stop processing to free your resources as quickly as possible? Forget it. Fold is going to keep going until it runs out of chunks. Even if you have your fold function stop updating the state after the headers are complete, you won't get a result until the entire data stream has been processed.

Finally, we still haven't addressed the original issues with iterators.

## Iteratees

The iteratee itself is a data consumer. It consumes data in chunks until it has either consumed enough data to produce a result or receives an EOF.

An iteratee is based on the `fold` operator with two differences:

1. The iteratee may complete its processing early by returning a Done state. The iteratee may return the unused portion of any chunk it was passed in its Done state. This should not be confused with the rest of the stream not yet provided by the enumerator.

2. The iteratee does not pull data but receives chunks of data from an "enumerator". It returns a continuation to receive and process the next chunk. This means that an iteratee may be paused at any time, either by neglecting to pass it any additional data or by passing an Empty chunk.

*)
module Iteratee =
    open FSharpx
    
    /// A stream of chunks of data generated by an Enumerator.
    /// The stream can be composed of chunks of 'a, empty blocks indicating a wait, or an EOF marker.
    /// Be aware that when using #seq<_> types, you will need to check for both Seq.empty ([]) and Empty.
    type Stream<'a> =
        | Chunk of 'a
        | Empty
        | EOF
    
    /// The iteratee is a stream consumer that will consume a stream of data until either 
    /// it receives an EOF or meets its own requirements for consuming data. The iteratee
    /// will return Continue whenever it is ready to receive the next chunk. An iteratee
    /// is fed data by an Enumerator, which generates a Stream. 
    type Iteratee<'el,'a> =
        | Done of 'a * Stream<'el>
        | Error of exn
        | Continue of (Stream<'el> -> Iteratee<'el,'a>)
    
    /// An enumerator generates a stream of data and feeds an iteratee, returning a new iteratee.
    type Enumerator<'el,'a> = Iteratee<'el,'a> -> Iteratee<'el,'a>
    
    /// An enumeratee is an enumerator that produces an iteratee using another iteratee as input.
    /// Enumeratees can be used for tasks such as encoding or encrypting data.
    type Enumeratee<'elo,'eli,'a> = Iteratee<'eli,'a> -> Iteratee<'elo, Iteratee<'eli,'a>>
    
    [<AutoOpen>]
    module Primitives =
    
        let returnI x = Done(x,Empty)
        let empty<'a> : Iteratee<'a,_> = Done((),Empty)
        let doneI x s = Done(x,s)
        let continueI k = Continue k
        let throw e = Error e
    
        let bind f i =
            let inner bind = function
                | Done(x, extra) ->
                    match f x with
                    | Done(x',_) -> Done(x', extra)
                    | Error e    -> Error e
                    | Continue k -> k extra
                | Error e    -> Error e
                | Continue k -> Continue(bind << k)
            fix inner i

        let opt i =
            let inner opt = function
                | Done(x, s) -> Done(Some x, s)
                | Error e    -> Done(None, Empty)
                | Continue k -> Continue(opt << k)
            fix inner i

        let catchError h i =
            let rec step = function
                | Error e    -> h e
                | Continue k -> Continue <| fun s -> step (k s)
                | i          -> i
            in step i
    
        let tryFinally compensation i =
            let rec step = function 
                | Continue k -> Continue <| fun s -> step (k s)
                | i          -> compensation(); i
            in step i

        let rec enumEOF = function 
            | Done(x,_)  -> Done(x, EOF)
            | Error e    -> Error e
            | Continue k ->
                match k EOF with
                | Continue _ -> failwith "enumEOF: divergent iteratee"
                | i          -> enumEOF i

        let enumErr e = function _ -> Error e

        let run_ i =
            match enumEOF i with
            | Done(x,_)  -> Choice1Of2 x
            | Error e    -> Choice2Of2 e
            | Continue _ -> failwith "run: divergent iteratee"
        
        let run i =
            match run_ i with
            | Choice1Of2 x -> x
            | Choice2Of2 e -> raise e
        
    type IterateeBuilder() =
        member this.Return(x) = Done(x, Empty)
        member this.ReturnFrom(m:Iteratee<_,_>) = m
        member this.Bind(m, f) = bind f m
        member this.Zero() = empty<_>
        member this.Combine(comp1, comp2) = bind (fun () -> comp2) comp1
        member this.Delay(f) = bind f empty<_>
        member this.TryCatch(m, handler) = catchError handler m
        member this.TryFinally(m, compensation) = tryFinally compensation m
        member this.Using(res:#IDisposable, body) =
            this.TryFinally(body res, (fun () -> match res with null -> () | disp -> disp.Dispose()))
        member this.While(guard, m) =
            if not(guard()) then this.Zero() else
                this.Bind(m, (fun () -> this.While(guard, m)))
        member this.For(sequence:#seq<_>, body) =
            this.Using(sequence.GetEnumerator(),
                (fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current))))
    let iteratee = IterateeBuilder()
    
    let inline returnM x = returnI x
    let inline (>>=) m f = bind f m
    let inline (=<<) f m = bind f m
    /// Sequential application
    let inline (<*>) f m = f >>= fun f' -> m >>= fun m' -> returnM (f' m')
    /// Sequential application
    let inline ap m f = f <*> m
    let inline map f m = m >>= fun x -> returnM (f x)
    let inline (<!>) f m = map f m
    let inline lift2 f a b = returnM f <*> a <*> b
    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) x y = lift2 (fun _ z -> z) x y
    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) x y = lift2 (fun z _ -> z) x y
    /// Sequentially compose two iteratee actions, discarding any value produced by the first
    let inline (>>.) m f = m >>= (fun _ -> f)
    /// Left-to-right Kleisli composition
    let inline (>=>) f g = fun x -> f x >>= g
    /// Right-to-left Kleisli composition
    let inline (<=<) x = flip (>=>) x
    
    module List =
        open Operators
        
        let fold step seed =
            let f = Microsoft.FSharp.Collections.List.fold step
            let rec loop acc = function
                | Empty -> Continue (loop acc)
                | Chunk [] -> Continue (loop acc)
                | Chunk xs -> Continue (loop (f acc xs))
                | EOF -> Done(acc, EOF)
            Continue (loop seed)
        
        let length<'a> : Iteratee<'a list, int> =
            let rec step n = function
                | Empty | Chunk [] -> Continue (step n)
                | Chunk x -> Continue (step (n + x.Length))
                | EOF -> Done(n, EOF)
            in Continue (step 0)
        
        let peek<'a> : Iteratee<'a list, 'a option> =
            let rec inner =
                let rec step = function
                    | Empty | Chunk ([]:'a list) -> inner
                    | Chunk(x::xs) as s -> Done(Some x, s)
                    | EOF -> Done(None, EOF)
                Continue step
            in inner
        
        let head<'a> : Iteratee<'a list, 'a option> =
            let rec inner =
                let rec step = function
                    | Empty | Chunk ([]:'a list) -> inner
                    | Chunk(x::xs) -> Done(Some x, Chunk xs)
                    | EOF -> Done(None, EOF)
                Continue step
            in inner
        
        let drop n =
            let rec step n = function
                | Empty | Chunk [] -> Continue <| step n
                | Chunk str ->
                    if str.Length < n then
                        Continue <| step (n - str.Length)
                    else let extra = List.skip n str in Done((), Chunk extra)
                | EOF -> Done((), EOF)
            in if n <= 0 then empty<_> else Continue (step n)
        
        let private dropWithPredicate pred listOp =
            let rec step = function
                | Empty | Chunk [] -> Continue step
                | Chunk x ->
                    match listOp pred x with
                    | [] -> Continue step
                    | x' -> Done((), Chunk x')
                | EOF as s -> Done((), s)
            in Continue step

        let dropWhile pred = dropWithPredicate pred List.skipWhile
        let dropUntil pred = dropWithPredicate pred List.skipUntil
        
        let take n =
            let rec step before n = function
                | Empty | Chunk [] -> Continue <| step before n
                | Chunk str ->
                    if str.Length < n then
                        Continue <| step (before @ str) (n - str.Length)
                    else let str', extra = List.splitAt n str in Done(before @ str', Chunk extra)
                | EOF -> Done(before, EOF)
            in if n <= 0 then Done([], Empty) else Continue (step [] n)
        
        let private takeWithPredicate (pred:'a -> bool) listOp =
            let rec step before = function
                | Empty | Chunk [] -> Continue (step before)
                | Chunk str ->
                    match listOp pred str with
                    | str', [] -> Continue (step (before @ str'))
                    | str', extra -> Done(before @ str', Chunk extra)
                | EOF -> Done(before, EOF)
            in Continue (step [])
        
        let takeWhile pred = takeWithPredicate pred List.span
        let takeUntil pred = takeWithPredicate pred List.split
        
        let heads str =
            let rec loop count str =
                match count, str with
                | (count, []) -> Done(count, Empty)
                | (count, str) -> Continue (step count str)
            and step count str s =
                match str, s with
                | str, Empty -> loop count str
                | str, (Chunk []) -> loop count str
                | c::t, (Chunk (c'::t')) ->
                    if c = c' then step (count + 1) t (Chunk t') 
                    else Done(count, Chunk (c'::t'))
                | _, s -> Done(count, s)
            loop 0 str
        
        let many i =
            let rec inner cont = i >>= check cont
            and check cont = function
                | [] -> Done(cont [], Empty)
                | xs -> inner (fun tail -> cont (xs::tail))
            inner id

        let skipNewline =
            let crlf = ['\r';'\n']
            let lf = ['\n']
            heads crlf >>= fun n ->
                if n = 0 then
                    heads lf
                else Done(n, Empty)

        let readLine =
            let isNewline c = c = '\r' || c = '\n'
            takeUntil isNewline

        let readLines =
            let rec lines cont = readLine >>= fun xs -> skipNewline >>= check cont xs
            and check cont xs count =
                match xs, count with
                | [], 0 -> Done(cont [] |> List.map (fun chars -> String(Array.ofList chars)), EOF)
                | xs, 0 -> Done(cont [xs] |> List.map (fun chars -> String(Array.ofList chars)), EOF)
                | _ -> lines (fun tail -> cont (xs::tail))
            lines id
        
        (* ========= Enumerators ========= *)
        
        // val enumeratePure1Chunk :: 'a list -> Enumerator<'a list,'b>
        let enumeratePure1Chunk str i =
            match str, i with 
            | [], _ -> i
            | _, Done(_,_) -> i
            | _::_, Continue k -> k (Chunk str)
            | _ -> i
        
        // val enumeratePureNChunk :: 'a list -> int -> Enumerator<'a list,'b>
        let rec enumeratePureNChunk n str i =
            match str, i with
            | [], _ -> i
            | _, Done(_,_) -> i
            | _::_, Continue k ->
                let x, xs = List.splitAt n str in enumeratePureNChunk n xs (k (Chunk x))
            | _ -> i

        //val enumerate :: 'a list -> Enumerator<'a list,'b>
        let rec enumerate str i = 
            match str, i with
            | [], _ -> i
            | _, Done(_,_) -> i
            | x::xs, Continue k -> enumerate xs (k (Chunk [x]))
            | _ -> i

    module Binary =
        open Operators

        type BS = ByteString

        (* ========= Iteratees ========= *)

        let fold step seed =
            let f = ByteString.fold step
            let rec loop acc = function
                | Empty -> Continue (loop acc)
                | Chunk xs when ByteString.isEmpty xs -> Continue (loop acc)
                | Chunk xs -> Continue (loop (f acc xs))
                | EOF -> Done(acc, EOF)
            Continue (loop seed)

        let length = 
            let rec step n = function
                | Empty -> Continue (step n)
                | Chunk x when ByteString.isEmpty x -> Continue (step n)
                | Chunk x -> Continue (step (n + x.Count))
                | EOF as s -> Done(n, s)
            Continue (step 0)

        let rec peek =
            let rec step = function
                | Empty -> peek
                | Chunk x when ByteString.isEmpty x -> peek
                | Chunk x as s -> Done(Some(ByteString.head x), s)
                | s -> Done(None, s)
            Continue step

        let rec head =
            let rec step = function
                | Empty -> head 
                | Chunk x when ByteString.isEmpty x -> head
                | Chunk x -> Done(Some(ByteString.head x), Chunk(ByteString.tail x))
                | s -> Done(None, s)
            Continue step

        let drop n =
            let rec step n = function
                | Empty -> Continue <| step n
                | Chunk str when ByteString.isEmpty str -> Continue <| step n
                | Chunk str ->
                    if ByteString.length str < n then
                        Continue <| step (n - (ByteString.length str))
                    else let extra = ByteString.skip n str in Done((), Chunk extra)
                | EOF -> Done((), EOF)
            in if n <= 0 then empty<_> else Continue <| step n

        let private dropWithPredicate pred byteStringOp =
            let rec step = function
                | Empty -> Continue step
                | Chunk x when ByteString.isEmpty x -> Continue step
                | Chunk x ->
                    let x' = byteStringOp pred x in
                    if ByteString.isEmpty x' then Continue step
                    else Done((), Chunk x')
                | s -> Done((), s)
            Continue step

        let dropWhile pred = dropWithPredicate pred ByteString.skipWhile
        let dropUntil pred = dropWithPredicate pred ByteString.skipUntil

        let take n =
            let rec step before n = function
                | Empty -> Continue <| step before n
                | Chunk str when ByteString.isEmpty str -> Continue <| step before n
                | Chunk str ->
                    if ByteString.length str < n then
                        Continue <| step (ByteString.append before str) (n - (ByteString.length str))
                    else let str', extra = ByteString.splitAt n str in Done(ByteString.append before str', Chunk extra)
                | EOF -> Done(before, EOF)
            in if n <= 0 then Done(ByteString.empty, Empty) else Continue (step ByteString.empty n)

        let private takeWithPredicate (pred:'a -> bool) byteStringOp =
            let rec step before = function
                | Empty -> Continue <| step before
                | Chunk str when ByteString.isEmpty str -> Continue <| step before
                | Chunk str ->
                    match byteStringOp pred str with
                    | str', extra when ByteString.isEmpty extra -> Continue <| step (ByteString.append before str')
                    | str', extra -> Done(ByteString.append before str', Chunk extra)
                | EOF -> Done(before, EOF)
            Continue (step ByteString.empty)

        let takeWhile pred = takeWithPredicate pred ByteString.span
        let takeUntil pred = takeWithPredicate pred ByteString.split

        let heads str =
            let rec loop count str =
                if ByteString.isEmpty str then Done(count, Empty)
                else Continue (step count str)
            and step count str = function
                | Empty -> loop count str
                | Chunk x when ByteString.isEmpty x -> loop count str
                | Chunk x when not (ByteString.isEmpty str) ->
                    let c, t = ByteString.head str, ByteString.tail str
                    let c', t' = ByteString.head x, ByteString.tail x
                    if c = c' then step (count + 1) t (Chunk t') 
                    else Done(count, Chunk x)
                | s -> Done(count, s)
            loop 0 str

        let many i =
            let rec inner cont = i >>= check cont
            and check cont bs =
                if ByteString.isEmpty bs then
                    Done(cont [], Empty)
                else inner <| fun tail -> cont <| bs::tail
            inner id

        let skipNewline =
            let crlf = BS"\r\n"B
            let lf = BS"\n"B
            heads crlf >>= fun n ->
                if n = 0 then
                    heads lf
                else Done(n, Empty)

        let readLine = 
            let isNewline c = c = '\r'B || c = '\n'B
            takeUntil isNewline

        let readLines =
            let rec lines cont = readLine >>= fun bs -> skipNewline >>= check cont bs
            and check cont bs count =
                match bs, count with
                | bs, 0 when ByteString.isEmpty bs -> Done(cont [], EOF)
                | bs, 0 -> Done(cont [bs], EOF)
                | _ -> lines <| fun tail -> cont <| bs::tail
            lines id

        (* ========= Enumerators ========= *)

        // val enumeratePure1Chunk :: ByteString -> Enumerator<ByteString,'b>
        let enumeratePure1Chunk str i =
            if ByteString.isEmpty str then i
            else match i with
                 | Done(_,_) -> i
                 | Continue k -> k (Chunk str)
                 | _ -> i

        // val enumeratePureNChunk :: ByteString -> int -> Enumerator<ByteString,'b>
        let rec enumeratePureNChunk n str i =
            if ByteString.isEmpty str then i
            else match i with
                 | Done(_,_) -> i
                 | Continue k -> let s1, s2 = ByteString.splitAt n str in enumeratePureNChunk n s2 (k (Chunk s1))
                 | _ -> i

        // val enumerate :: ByteString -> Enumerator<ByteString,'b>
        let rec enumerate str i =
            if ByteString.isEmpty str then i
            else match i with
                 | Done(_,_) -> i
                 | Continue k -> let x, xs = ByteString.head str, ByteString.tail str in enumerate xs (k (Chunk (ByteString.singleton x)))
                 | _ -> i

        let enumStream bufferSize (stream:#System.IO.Stream) i =
            let buffer = Array.zeroCreate<byte> bufferSize
            let rec step = function Continue k -> read k | i -> i
            and read k =
                let result = stream.Read(buffer, 0, bufferSize) in
                if result = 0 then Continue k
                else step (k (Chunk(BS(buffer,0,result))))
            step i

        let enumStreamReader (reader:#System.IO.TextReader) i =
            let rec step i =
                match i with
                | Done(_,_) -> i
                | Continue k ->
                    let line = reader.ReadLine()
                    if line = null then i
                    else step (k (Chunk(ByteString.ofString line)))
                | _ -> i
            step i
