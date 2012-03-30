

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
      interface System.IDisposable
      new : batchSize:int * timeout:int -> BatchProcessingAgent<'T>
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
  type internal CircularQueueMessage<'T> =
    | Enqueue of 'T [] * int * int
    | Dequeue of int * AsyncReplyChannel<'T []>
  type CircularQueueAgent<'T> =
    class
      new : maxLength:int -> CircularQueueAgent<'T>
      member AsyncDequeue : count:int * ?timeout:int -> Async<'T []>
      member Dequeue : count:int * ?timeout:int -> 'T []
      member Enqueue : segment:System.ArraySegment<'T> -> unit
      member Enqueue : value:'T [] -> unit
      member Enqueue : value:'T [] * offset:int * count:int -> unit
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

namespace FSharpx.Async
  module AssemblyInfo = begin
    exception ReturnException183c26a427ae489c8fd92ec21a0c9a59 of obj
    exception ReturnNoneException183c26a427ae489c8fd92ec21a0c9a59
  end

namespace FSharp.Control
  module AsyncExtensions = begin
    type Async with
      static member Cache : input:Async<'T> -> Async<'T>
    type Async with
      static member StartDisposable : op:Async<unit> -> System.IDisposable
    type Async with
      static member
        TryAwaitTask : task:System.Threading.Tasks.Task<'a> * ?timeout:int *
                       ?cancellationToken:System.Threading.CancellationToken ->
                         Async<'a option>
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
    val create : 'a -> System.IObservable<'a>
    val error : exn -> System.IObservable<'a>
    val FromEventHandler<'TEventArgs when 'TEventArgs :> System.EventArgs> :
      System.Action<System.EventHandler<System.EventArgs>> *
      System.Action<System.EventHandler<System.EventArgs>> ->
        System.IObservable<System.EventArgs>
    val FromEvent :
      System.Func<System.Action<'TEventArgs>,'TDelegate> *
      System.Action<'TDelegate> * System.Action<'TDelegate> ->
        System.IObservable<'TEventArgs> when 'TEventArgs :> System.EventArgs
    val ofSeq : seq<'TItem> -> System.IObservable<'TItem>
    val mapi :
      (int -> 'TSource -> 'TResult) ->
        System.IObservable<'TSource> -> System.IObservable<'TResult>
    val takeWhile :
      ('TSource -> bool) ->
        System.IObservable<'TSource> -> System.IObservable<'TSource>
    val combineLatest :
      System.IObservable<'TLeft> ->
        System.IObservable<'TRight> -> System.IObservable<'TLeft * 'TRight>
    type LinkedList<'T> with
      member pushBack : x:'T -> unit
    type LinkedList<'T> with
      member popFront : unit -> 'T
    val zip :
      System.IObservable<'TLeft> ->
        System.IObservable<'TRight> -> System.IObservable<'TLeft * 'TRight>
    val bufferWithTimeOrCount :
      System.TimeSpan ->
        int -> System.IObservable<'T> -> System.IObservable<seq<'T>>
    [<AbstractClassAttribute ()>]
    type internal BasicObserver<'a> =
      class
        interface System.IObserver<'a>
        new : unit -> BasicObserver<'a>
        abstract member Completed : unit -> unit
        abstract member Error : error:exn -> unit
        abstract member Next : value:'a -> unit
      end
    val invoke :
      ((unit -> unit) -> unit) ->
        System.IObservable<'a> -> System.IObservable<'a>
    val delay : int -> System.IObservable<'a> -> System.IObservable<'a>
    val synchronize : (((unit -> unit) -> unit) -> 'a) -> 'a
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
    val throttle : int -> System.IObservable<'T> -> System.IObservable<'T>
  end
  [<System.Runtime.CompilerServices.Extension ()>]
  type ObservableExtensions =
    class
      private new : unit -> ObservableExtensions
      [<System.Runtime.CompilerServices.Extension ()>]
      static member
        BufferWithTimeOrCount : source:System.IObservable<'TSource> *
                                timeSpan:System.TimeSpan * count:int ->
                                  System.IObservable<seq<'TSource>>
      [<System.Runtime.CompilerServices.Extension ()>]
      static member
        CombineLatest : left:System.IObservable<'TLeft> *
                        right:System.IObservable<'TRight> *
                        selector:System.Func<'TLeft,'TRight,'TResult> ->
                          System.IObservable<'TResult>
      [<System.Runtime.CompilerServices.Extension ()>]
      static member
        Delay : source:System.IObservable<'TSource> * milliseconds:int ->
                  System.IObservable<'TSource>
      [<System.Runtime.CompilerServices.Extension ()>]
      static member
        Merge : source:System.IObservable<'TSource> *
                sources:System.Collections.Generic.IEnumerable<System.IObservable<'TSource>> ->
                  System.IObservable<'TSource>
      [<System.Runtime.CompilerServices.Extension ()>]
      static member
        Merge : source:System.IObservable<'TSource> *
                sources:System.IObservable<'TSource> [] ->
                  System.IObservable<'TSource>
      [<System.Runtime.CompilerServices.Extension ()>]
      static member
        Scan : source:System.IObservable<'TSource> * seed:'TAccumulate *
               f:System.Func<'TAccumulate,'TSource,'TAccumulate> ->
                 System.IObservable<'TAccumulate>
      [<System.Runtime.CompilerServices.Extension ()>]
      static member
        Select : source:System.IObservable<'TSource> *
                 selector:System.Func<'TSource,'TResult> ->
                   System.IObservable<'TResult>
      [<System.Runtime.CompilerServices.Extension ()>]
      static member
        Select : source:System.IObservable<'TSource> *
                 selector:System.Func<'TSource,int,'TResult> ->
                   System.IObservable<'TResult>
      [<System.Runtime.CompilerServices.Extension ()>]
      static member
        SelectMany : source:System.IObservable<'TSource> *
                     collectionSelector:System.Func<'TSource,
                                                    System.Collections.Generic.IEnumerable<'TCollection>> *
                     resultSelector:System.Func<'TSource,'TCollection,'TResult> ->
                       System.IObservable<'TResult>
      [<System.Runtime.CompilerServices.Extension ()>]
      static member
        Subscribe : source:System.IObservable<'TSource> *
                    action:System.Action<'TSource> -> System.IDisposable
      [<System.Runtime.CompilerServices.Extension ()>]
      static member
        TakeWhile : source:System.IObservable<'TSource> *
                    f:System.Func<'TSource,bool> -> System.IObservable<'TSource>
      [<System.Runtime.CompilerServices.Extension ()>]
      static member
        Throttle : source:System.IObservable<'TSource> * dueTime:System.TimeSpan ->
                     System.IObservable<'TSource>
      [<System.Runtime.CompilerServices.Extension ()>]
      static member
        ToObservable : source:System.Collections.Generic.IEnumerable<'TItem> ->
                         System.IObservable<'TItem>
      [<System.Runtime.CompilerServices.Extension ()>]
      static member
        Where : source:System.IObservable<'TSource> *
                predicate:System.Func<'TSource,bool> ->
                  System.IObservable<'TSource>
      [<System.Runtime.CompilerServices.Extension ()>]
      static member
        Zip : left:System.IObservable<'TLeft> *
              right:System.IObservable<'TRight> *
              selector:System.Func<'TLeft,'TRight,'TResult> ->
                System.IObservable<'TResult>
    end
  type private CircularBuffer<'T> =
    class
      new : bufferSize:int -> CircularBuffer<'T>
      member Add : value:'T -> unit
      member Iter : f:('T -> unit) -> unit
    end
  type private BufferAgentMessage<'T> =
    | Add of System.IObserver<'T>
    | Remove of System.IObserver<'T>
    | Next of 'T
    | Completed
    | Error of exn
  module private BufferAgent = begin
    val start : int -> MailboxProcessor<BufferAgentMessage<'a>>
  end
  [<InterfaceAttribute ()>]
  type ISubject<'TIn,'TOut> =
    interface
      inherit System.IObservable<'TOut>
      inherit System.IObserver<'TIn>
    end
  type ReplaySubject<'T> =
    class
      interface ISubject<'T,'T>
      new : bufferSize:int -> ReplaySubject<'T>
      member OnCompleted : unit -> unit
      member OnError : error:exn -> unit
      member OnNext : value:'T -> unit
      member Subscribe : observer:System.IObserver<'T> -> System.IDisposable
    end
  and Subject<'T> =
    class
      inherit ReplaySubject<'T>
      new : unit -> Subject<'T>
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
  type CircularStream =
    class
      inherit System.IO.Stream
      new : maxLength:int -> CircularStream
      member
        AsyncRead : buffer:byte [] * offset:int * count:int * ?timeout:int ->
                      Async<int>
      override Close : unit -> unit
      override Flush : unit -> unit
      override Read : buffer:byte [] * offset:int * count:int -> int
      override Seek : offset:int64 * origin:System.IO.SeekOrigin -> int64
      override SetLength : value:int64 -> unit
      override Write : buffer:byte [] * offset:int * count:int -> unit
      override CanRead : bool
      override CanSeek : bool
      override CanWrite : bool
      override Length : int64
      override Position : int64
      override Position : int64 with set
    end

