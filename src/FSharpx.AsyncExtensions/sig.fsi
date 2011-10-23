

namespace FSharp.Control
  type Agent<'T> = MailboxProcessor<'T>

namespace FSharp.Control
  type AutoCancelAgent<'T> =
    class
      interface System.IDisposable
      private new : mbox:Agent<'T> *
                    cts:System.Threading.CancellationTokenSource ->
                      AutoCancelAgent<'T>
      member Post : m:'T -> unit
      member
        PostAndAsyncReply : buildMessage:(AsyncReplyChannel<'a> -> 'T) *
                            ?timeout:int -> Async<'a>
      member
        PostAndReply : buildMessage:(AsyncReplyChannel<'a> -> 'T) * ?timeout:int ->
                         'a
      member
        PostAndTryAsyncReply : buildMessage:(AsyncReplyChannel<'a> -> 'T) *
                               ?timeout:int -> Async<'a option>
      member Receive : ?timeout:int -> Async<'T>
      member Scan : scanner:('T -> Async<'a> option) * ?timeout:int -> Async<'a>
      member
        TryPostAndReply : buildMessage:(AsyncReplyChannel<'a> -> 'T) *
                          ?timeout:int -> 'a option
      member TryReceive : ?timeout:int -> Async<'T option>
      member
        TryScan : scanner:('T -> Async<'a> option) * ?timeout:int ->
                    Async<'a option>
      member add_Error : Handler<System.Exception> -> unit
      member CurrentQueueLength : int
      [<CLIEventAttribute ()>]
      member Error : IEvent<Handler<System.Exception>,System.Exception>
      member remove_Error : Handler<System.Exception> -> unit
      static member
        Start : f:(MailboxProcessor<'T> -> Async<unit>) -> AutoCancelAgent<'T>
    end

namespace FSharp.Control
  type ConcurrentSetAgent<'T> =
    class
      new : unit -> ConcurrentSetAgent<'T>
      member AsyncAdd : v:obj -> Async<bool>
    end

namespace FSharp.Control
  type BatchProcessingAgent<'T> =
    class
      new : bulkSize:int * timeout:int -> BatchProcessingAgent<'T>
      member Enqueue : v:'T -> unit
      member add_BatchProduced : Handler<'T []> -> unit
      [<CLIEventAttribute ()>]
      member BatchProduced : IEvent<'T []>
      member remove_BatchProduced : Handler<'T []> -> unit
    end

namespace FSharp.Control
  type internal BlockingAgentMessage<'T> =
    | Add of 'T * AsyncReplyChannel<unit>
    | Get of AsyncReplyChannel<'T>
  type BlockingQueueAgent<'T> =
    class
      new : maxLength:int -> BlockingQueueAgent<'T>
      member AsyncAdd : v:'T * ?timeout:int -> Async<unit>
      member AsyncGet : ?timeout:int -> Async<'T>
      member Get : ?timeout:int -> 'T
      member Count : int
    end

namespace FSharp.Control
  type SlidingWindowAgent<'T> =
    class
      new : windowSize:int * ?cancelToken:System.Threading.CancellationToken ->
              SlidingWindowAgent<'T>
      member Enqueue : v:'T -> unit
      member add_WindowProduced : Handler<'T []> -> unit
      [<CLIEventAttribute ()>]
      member WindowProduced : IEvent<'T []>
      member remove_WindowProduced : Handler<'T []> -> unit
    end

namespace FSharp.Control
  module AsyncExtensions = begin
    type Async with
      static member Cache : input:Async<'T> -> Async<'T>
    type Async with
      static member StartDisposable : op:Async<unit> -> System.IDisposable
  end

namespace FSharp.Control
  type ObservableUpdate<'T> =
    | Next of 'T
    | Error of exn
    | Completed
  module Observable = begin
    val windowed : int -> System.IObservable<'T> -> System.IObservable<'T []>
    val guard :
      (unit -> unit) -> System.IObservable<'Args> -> System.IObservable<'Args>
    val asUpdates :
      System.IObservable<'T> -> System.IObservable<ObservableUpdate<'T>>
  end
  module ObservableExtensions = begin
    val internal synchronize : (((unit -> unit) -> unit) -> 'a) -> 'a
    type Async with
      static member
        GuardedAwaitObservable : ev1:System.IObservable<'T1> ->
                                   guardFunction:(unit -> unit) -> Async<'T1>
    type Async with
      static member AwaitObservable : ev1:System.IObservable<'T1> -> Async<'T1>
    type Async with
      static member
        AwaitObservable : ev1:System.IObservable<'T1> *
                          ev2:System.IObservable<'T2> -> Async<Choice<'T1,'T2>>
    type Async with
      static member
        AwaitObservable : ev1:System.IObservable<'T1> *
                          ev2:System.IObservable<'T2> *
                          ev3:System.IObservable<'T3> ->
                            Async<Choice<'T1,'T2,'T3>>
    type Async with
      static member
        AwaitObservable : ev1:System.IObservable<'T1> *
                          ev2:System.IObservable<'T2> *
                          ev3:System.IObservable<'T3> *
                          ev4:System.IObservable<'T4> ->
                            Async<Choice<'T1,'T2,'T3,'T4>>
  end

namespace FSharp.Control
  type AsyncSeq<'T> = Async<AsyncSeqInner<'T>>
  and AsyncSeqInner<'T> =
    | Nil
    | Cons of 'T * AsyncSeq<'T>
  module AsyncSeq = begin
    [<GeneralizableValueAttribute ()>]
    val empty<'T> : AsyncSeq<'T>
    val singleton : 'T -> AsyncSeq<'T>
    val append : AsyncSeq<'T> -> AsyncSeq<'T> -> AsyncSeq<'T>
    type AsyncSeqBuilder =
      class
        new : unit -> AsyncSeqBuilder
        member Bind : inp:Async<'T> * body:('T -> AsyncSeq<'U>) -> AsyncSeq<'U>
        member Combine : seq1:AsyncSeq<'T> * seq2:AsyncSeq<'T> -> AsyncSeq<'T>
        member Delay : f:(unit -> AsyncSeq<'T>) -> AsyncSeq<'T>
        member
          For : seq:seq<'T> * action:('T -> AsyncSeq<'TResult>) ->
                  AsyncSeq<'TResult>
        member
          For : seq:AsyncSeq<'T> * action:('T -> AsyncSeq<'TResult>) ->
                  AsyncSeq<'TResult>
        member Return : unit -> AsyncSeq<'c>
        member
          TryFinally : body:AsyncSeq<'T> * compensation:(unit -> unit) ->
                         AsyncSeq<'T>
        member
          TryWith : body:AsyncSeq<'a> * handler:(exn -> AsyncSeq<'a>) ->
                      AsyncSeq<'a>
        member
          Using : resource:'a * binder:('a -> AsyncSeq<'b>) -> AsyncSeq<'b>
                    when 'a :> System.IDisposable
        member While : gd:(unit -> bool) * seq:AsyncSeq<'T> -> AsyncSeq<'T>
        member Yield : v:'d -> AsyncSeq<'d>
        member YieldFrom : s:'b -> 'b
        member Zero : unit -> AsyncSeq<'a>
      end
    val asyncSeq : AsyncSeqBuilder
    val internal tryNext : AsyncSeq<'a> -> Async<Choice<AsyncSeqInner<'a>,exn>>
    val internal tryWith : AsyncSeq<'T> -> (exn -> AsyncSeq<'T>) -> AsyncSeq<'T>
    val internal tryFinally : AsyncSeq<'T> -> (unit -> unit) -> AsyncSeq<'T>
    val collect :
      ('T -> AsyncSeq<'TResult>) -> AsyncSeq<'T> -> AsyncSeq<'TResult>
    type AsyncBuilder with
      member For : seq:AsyncSeq<'T> * action:('T -> Async<unit>) -> Async<unit>
    val mapAsync : ('T -> Async<'TResult>) -> AsyncSeq<'T> -> AsyncSeq<'TResult>
    val chooseAsync : ('T -> Async<'R option>) -> AsyncSeq<'T> -> AsyncSeq<'R>
    val filterAsync : ('T -> Async<bool>) -> AsyncSeq<'T> -> AsyncSeq<'T>
    val lastOrDefault : 'T -> AsyncSeq<'T> -> Async<'T>
    val firstOrDefault : 'T -> AsyncSeq<'T> -> Async<'T>
    val scanAsync :
      ('TState -> 'T -> Async<'TState>) ->
        'TState -> AsyncSeq<'T> -> AsyncSeq<'TState>
    val iterAsync : ('T -> Async<unit>) -> AsyncSeq<'T> -> Async<unit>
    val pairwise : AsyncSeq<'T> -> AsyncSeq<'T * 'T>
    val foldAsync :
      ('TState -> 'T -> Async<'TState>) ->
        'TState -> AsyncSeq<'T> -> Async<'TState>
    val fold :
      ('TState -> 'T -> 'TState) -> 'TState -> AsyncSeq<'T> -> Async<'TState>
    val scan :
      ('TState -> 'T -> 'TState) -> 'TState -> AsyncSeq<'T> -> AsyncSeq<'TState>
    val map : ('T -> 'a) -> AsyncSeq<'T> -> AsyncSeq<'a>
    val iter : ('T -> unit) -> AsyncSeq<'T> -> Async<unit>
    val choose : ('T -> 'a option) -> AsyncSeq<'T> -> AsyncSeq<'a>
    val filter : ('T -> bool) -> AsyncSeq<'T> -> AsyncSeq<'T>
    val ofSeq : seq<'T> -> AsyncSeq<'T>
    type internal BufferMessage<'T> =
      | Get of AsyncReplyChannel<'T>
      | Put of 'T
    val internal ofObservableUsingAgent :
      System.IObservable<'a> ->
        (MailboxProcessor<BufferMessage<ObservableUpdate<'a>>> -> Async<unit>) ->
          AsyncSeq<'a>
    val ofObservableBuffered : System.IObservable<'a> -> AsyncSeq<'a>
    val ofObservable : System.IObservable<'a> -> AsyncSeq<'a>
    val toObservable : AsyncSeq<'a> -> System.IObservable<'a>
    val toBlockingSeq : AsyncSeq<'T> -> seq<'T>
    val cache : AsyncSeq<'T> -> AsyncSeq<'T>
    val zip : AsyncSeq<'T1> -> AsyncSeq<'T2> -> AsyncSeq<'T1 * 'T2>
    val takeWhileAsync : ('T -> Async<bool>) -> AsyncSeq<'T> -> AsyncSeq<'T>
    val skipWhileAsync : ('T -> Async<bool>) -> AsyncSeq<'T> -> AsyncSeq<'T>
    val takeWhile : ('T -> bool) -> AsyncSeq<'T> -> AsyncSeq<'T>
    val skipWhile : ('T -> bool) -> AsyncSeq<'T> -> AsyncSeq<'T>
    val take : int -> AsyncSeq<'T> -> AsyncSeq<'T>
    val skip : int -> AsyncSeq<'T> -> AsyncSeq<'T>
  end
  module AsyncSeqExtensions = begin
    val asyncSeq : AsyncSeq.AsyncSeqBuilder
    type AsyncBuilder with
      member For : seq:AsyncSeq<'T> * action:('T -> Async<unit>) -> Async<unit>
  end
  module Seq = begin
    val ofAsyncSeq : AsyncSeq<'T> -> seq<'T>
  end

namespace FSharp.IO
  module IOExtensions = begin
    type Stream with
      member AsyncReadSeq : ?bufferSize:int -> Control.AsyncSeq<byte []>
    type Stream with
      member AsyncWriteSeq : input:Control.AsyncSeq<byte []> -> Async<unit>
  end
namespace FSharp.Net
  module HttpExtensions = begin
    type HttpListener with
      member AsyncGetContext : unit -> Async<System.Net.HttpListenerContext>
    type HttpListener with
      static member
        Start : url:string *
                handler:(System.Net.HttpListenerRequest *
                         System.Net.HttpListenerResponse -> Async<unit>) *
                ?cancellationToken:System.Threading.CancellationToken -> unit
    type HttpListenerRequest with
      member AsyncInputString : Async<string>
    type HttpListenerResponse with
      member AsyncReply : s:string -> Async<unit>
    type HttpListenerResponse with
      member AsyncReply : typ:string * buffer:byte [] -> Async<unit>
  end
  module SocketExtensions = begin
    val internal asyncInvoke :
      (System.Net.Sockets.SocketAsyncEventArgs -> bool) *
      System.Net.Sockets.SocketAsyncEventArgs ->
        Async<System.Net.Sockets.SocketAsyncEventArgs>
    type Socket with
      member
        AsyncAccept : args:System.Net.Sockets.SocketAsyncEventArgs ->
                        Async<System.Net.Sockets.SocketAsyncEventArgs>
    type Socket with
      member
        AsyncConnect : args:System.Net.Sockets.SocketAsyncEventArgs ->
                         Async<System.Net.Sockets.SocketAsyncEventArgs>
    type Socket with
      member
        AsyncDisconnect : args:System.Net.Sockets.SocketAsyncEventArgs ->
                            Async<System.Net.Sockets.SocketAsyncEventArgs>
    type Socket with
      member
        AsyncReceive : args:System.Net.Sockets.SocketAsyncEventArgs ->
                         Async<System.Net.Sockets.SocketAsyncEventArgs>
    type Socket with
      member
        AsyncSend : args:System.Net.Sockets.SocketAsyncEventArgs ->
                      Async<System.Net.Sockets.SocketAsyncEventArgs>
  end

