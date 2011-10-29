(*
 * Copyright (c) 2008, Gregory Neverov
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. 
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. 
 * 3. Neither the name of the author nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission. 
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 *)

[<AutoOpen>]
module FSharpx.Stm.Core

open System
open System.Collections.Generic
open System.Threading

//[<AbstractClass>]
//type TVar internal() as this =
//    static let nextId = ref 0
//    [<DefaultValue>] val mutable internal id : int
//    do incr nextId
//    do this.id <- !nextId
//    interface IComparable<TVar> with
//        member this.CompareTo(other : TVar) = this.id.CompareTo(other.id)
//
//[<SealedAttribute>]
//type TVar<'a>(value : 'a, cmp : IEqualityComparer<'a>) as this =
//    inherit TVar()
//    [<DefaultValue>] val mutable internal value : 'a
//    [<DefaultValue>] val mutable internal cmp : IEqualityComparer<'a>
//    do this.value <- value
//    do this.cmp <- cmp
//    member x.UnsafeRead() = value
//
//[<AbstractClass>]
//type internal Entry() =
//    abstract member Location : TVar
//    abstract member IsValid : unit -> bool
//    abstract member Commit : unit -> unit
//    abstract member MergeNested : Entry -> unit
//
//[<SealedAttribute>]
//type private Entry<'a> private (location : TVar<'a>, hasOldValue : bool, oldValue : 'a, newValue : 'a) as this =
//    inherit Entry()
//    let mutable location = location
//    [<DefaultValue>] val mutable public hasOldValue : bool
//    [<DefaultValue>] val mutable public oldValue : 'a
//    [<DefaultValue>] val mutable public newValue : 'a
//    do this.hasOldValue <- hasOldValue
//    do this.oldValue <- oldValue
//    do this.newValue <- newValue
//
//    // read
//    public new(location : TVar<'a>) =
//        Entry<'a>(location, true, location.value, location.value)
//
//    // write
//    public new(location : TVar<'a>, value : 'a) =
//        Entry<'a>(location, false, Unchecked.defaultof<'a>, value)
//        
//    override this.Location = location :> TVar
//    override this.Commit() = location.value <- this.newValue
//    override this.MergeNested(entry) =
//        (entry :?> Entry<'a>).newValue <- this.newValue
//    override this.IsValid() =
//        not this.hasOldValue ||
//        location.cmp.Equals(location.value, this.oldValue)
//
//[<SealedAttribute>]
//type private ReferenceEqualityComparer<'a when 'a : equality and 'a : not struct>() =
//    interface IEqualityComparer<'a> with
//        member this.Equals(x, y) = x = y
//        member this.GetHashCode(obj) = obj.GetHashCode()
//
//[<SealedAttribute>]
//type private EquatableEqualityComparer<'a when 'a : equality and 'a : struct and 'a :> IEquatable<'a>>() =
//    interface IEqualityComparer<'a> with
//        member this.Equals(x, y) = x = y
//        member this.GetHashCode(obj) = obj.GetHashCode()
//
//[<SealedAttribute>]
//type private AnyEqualityComparer<'a when 'a : equality>() =
//    interface IEqualityComparer<'a> with
//        member this.Equals(x, y) = x = y
//        member this.GetHashCode(obj) = obj.GetHashCode()
//
//[<SealedAttribute>]
//type private RetryException() = inherit Exception()
//
//[<SealedAttribute>]
//type private CommitFailedException() = inherit Exception()
//
//[<SealedAttribute>]
//type TLog() as this =
//    static let mutable lock = obj()
//    [<DefaultValue>] val mutable internal outer : TLog
//    [<DefaultValue>] val mutable internal log : SortedDictionary<TVar, Entry>
//    do this.outer <- Unchecked.defaultof<TLog>
//    do this.log <- SortedDictionary<TVar, Entry>()
//
//    static member NewTVarClass<'a when 'a : equality and 'a : not struct>(value) =
//        TVar<'a>(value, ReferenceEqualityComparer<'a>())
//
//    static member NewTVarStruct<'a when 'a : equality and 'a : struct and 'a :> IEquatable<'a>>(value) =
//        TVar<'a>(value, EquatableEqualityComparer<'a>())
//
//    static member NewTVarBoxedStruct<'a when 'a : equality and 'a : struct>(value) =
//        TVar<'a>(value, AnyEqualityComparer<'a>())
//
//    static member NewTVar<'a>(value) =
//        let t = typeof<'a>
//        let ect =
//            if not t.IsValueType then
//                typeof<ReferenceEqualityComparer<_>>.GetGenericTypeDefinition()
//            elif typeof<IEquatable<'a>>.IsAssignableFrom(t) then
//                typeof<EquatableEqualityComparer<_>>.GetGenericTypeDefinition()
//            else typeof<AnyEqualityComparer<_>>.GetGenericTypeDefinition()
//        let eci = Activator.CreateInstance(ect.MakeGenericType(t)) :?> IEqualityComparer<'a>
//        TVar<'a>(value, eci)
//
//    member this.ReadTVar<'a>(location : TVar<'a>) =
//        let trans = this
//        let entry = ref Unchecked.defaultof<Entry>
//        let rec readLoop (trans : TLog) =
//            if trans.log.TryGetValue(location, entry) then true
//            elif trans = Unchecked.defaultof<TLog> then false
//            else readLoop trans.outer
//
//        if readLoop trans then
//            (!entry :?> Entry<'a>).newValue
//        else
//            let entry' = Entry<'a>(location)
//            this.log.Add(location, entry')
//            entry'.oldValue
//
//    member this.WriteTVar<'a>(location : TVar<'a>, value) =
//        let entry = ref Unchecked.defaultof<Entry>
//        if this.log.TryGetValue(location, entry) then
//            (!entry :?> Entry<'a>).newValue <- value
//        else
//            let entry' = Entry<'a>(location, value)
//            this.log.Add(location, entry')
//
//    // requires lock
//    member internal this.IsValid() =
//        this.IsValidSingle() &&
//        (this.outer = Unchecked.defaultof<TLog> || this.outer.IsValid())
//
//    // requires lock
//    member private this.IsValidSingle() =
//        Seq.forall (fun (entry : Entry) -> entry.IsValid()) this.log.Values
//
//    // requires lock
//    member internal this.Commit() =
//        if this.outer <> Unchecked.defaultof<TLog> then invalidOp "!"
//        for entry in this.log.Values do entry.Commit()
//        
//    member internal this.StartNested() =
//        let inner = TLog()
//        inner.outer <- this
//        inner
//
//    member internal this.MergeNested() =
//        for innerEntry in this.log.Values do
//            let outerEntry = ref Unchecked.defaultof<Entry>
//            if this.outer.log.TryGetValue(innerEntry.Location, outerEntry) then
//                innerEntry.MergeNested(!outerEntry)
//            else this.outer.log.Add(innerEntry.Location, innerEntry)
//
//    member internal this.Wait() = ()
//
//    member internal this.Unwait() = ()
//
//    member internal this.Lock() = Monitor.Enter(lock)
//
//    member internal this.Unlock() = Monitor.Exit(lock)
//
//    member internal this.Block() = Monitor.Wait(lock) |> ignore
//
//    member internal this.Signal() = Monitor.PulseAll(lock)
//
//    static member Atomic(p : StmAction) : unit =
//        TLog.Atomic<obj>(TLog.Ignore p) |> ignore
//
//    static member Atomic<'a>(p : StmAction<'a>) : 'a =
//        let trans = TLog()
//        let rec loop (trans : TLog) =
//            let mutable retry = false
//            try
//                let result = p trans
//                trans.Lock()
//                let isValid = trans.IsValid()
//                if isValid then
//                    trans.Commit()
//                    trans.Signal()
//                trans.Unlock()
//                if isValid then result
//                else attemptRetry retry trans
//            with
//            | :? RetryException -> attemptRetry true trans
//            | :? CommitFailedException -> attemptRetry retry trans
//            // cannot receive ThreadInterruptedException in catch handler
//            | :? ThreadInterruptedException as tie-> raise tie
//            | e ->
//                trans.Lock()
//                let isValid = trans.IsValid()
//                trans.Unlock()
//                if isValid then raise e
//                else attemptRetry retry trans
//        and attemptRetry retry trans =
//            if retry then
//                trans.Lock()
//                let isValid = trans.IsValid()
//                if isValid then
//                    let rec blockingLoop (trans : TLog) =
//                        trans.Block()
//                        if trans.IsValid() then
//                            blockingLoop trans
//                    trans.Wait()
//                    try
//                        blockingLoop trans
//                    finally
//                        trans.Unwait()
//                        trans.Unlock()
//                else trans.Unlock()
//            trans.log.Clear()
//            Thread.Sleep(0)
//            loop trans
//        loop trans
//
//    member this.Retry<'a>() : 'a = raise <| RetryException()
//
//    member this.OrElse(p : StmAction, q : StmAction) =
//        this.OrElse(TLog.Ignore p, TLog.Ignore q) |> ignore
//
//    member this.OrElse<'a>(p : StmAction<'a>, q : StmAction<'a>) : 'a =
//        let fst = this.StartNested()
//        try
//            let result = p fst
//            fst.Lock()
//            let isValid = fst.IsValid()
//            fst.Unlock()
//            if isValid then
//                fst.MergeNested()
//                result
//            else raise <| CommitFailedException()
//        with
//        | :? RetryException ->
//            let snd = this.StartNested()
//            try
//                let result = q snd
//                snd.Lock()
//                let isValid = snd.IsValid()
//                snd.Unlock()
//                if isValid then
//                    snd.MergeNested()
//                    result
//                else raise <| CommitFailedException()
//            with
//            | :? RetryException as re ->
//                this.Lock()
//                let isValid = fst.IsValidSingle() && snd.IsValidSingle() && this.IsValid()
//                this.Unlock()
//                if isValid then
//                    fst.MergeNested()
//                    snd.MergeNested()
//                    raise re
//                else raise <| CommitFailedException()
//            | :? CommitFailedException as cfe -> raise cfe
//            | :? ThreadInterruptedException as tie -> raise tie
//            | e ->
//                snd.Lock()
//                let isValid = snd.IsValid()
//                snd.Unlock()
//                if isValid then
//                    snd.MergeNested()
//                    raise e
//                else raise <| new CommitFailedException()
//        | :? CommitFailedException as cfe -> raise cfe
//        | :? ThreadInterruptedException as tie -> raise tie
//        | e ->
//            fst.Lock()
//            let isValid = fst.IsValid()
//            fst.Unlock()
//            if isValid then
//                fst.MergeNested()
//                raise e
//            else raise <| new CommitFailedException()
//
//    static member Ignore(p : StmAction) : StmAction<obj> =
//        fun trans -> p trans; null
//        
//and StmAction = TLog -> unit
//and StmAction<'a> = TLog -> 'a

type TVar<'a> = FSharpx.Stm.Core.TVar<'a>
type TLog = FSharpx.Stm.Core.TLog
type Stm<'a> = (TLog -> 'a)

module Stm =
    let newTVar (value : 'a) : TVar<'a> =
        TLog.NewTVar(value)
      
    let readTVar (ref : TVar<'a>) : Stm<'a> =
        fun trans -> trans.ReadTVar(ref)
      
    let writeTVar (ref : TVar<'a>) (value : 'a) : Stm<unit> =
        fun trans -> trans.WriteTVar(ref, value)
    
    let retry () : Stm<'a> = 
        fun trans -> trans.Retry<_>()
    
    let orElse (a : Stm<'a>) (b : Stm<'a>) : Stm<'a> = 
        fun trans -> trans.OrElse<_>((fun x -> a x), (fun x -> b x))
      
    let atomically (a : Stm<'a>) : 'a =
        TLog.Atomic<_>(fun x -> a x)
      
    type StmBuilder () =
        member b.Return(x) : Stm<_> = fun _ -> x
    
        member b.ReturnFrom(m) : Stm<_> = m
    
        member b.Bind(p : Stm<_>, rest : _ -> Stm<_>) : Stm<_> = fun trans -> rest (p trans) trans
    
        member b.Let(p, rest) : Stm<_> = rest p 
        
        member b.Delay(f : unit -> Stm<_>) : Stm<_> = fun trans -> f () trans
     
        member b.Combine(p, q) : Stm<_> = orElse p q
    
        member b.Zero() = retry ()
        
    let stm = new StmBuilder ()
    
    let ifM p x = if p then x else stm.Return(())
    
    let liftM f x = stm { let! x' = x in return f x' }
    
    let sequence (ms : seq<Stm<_> >) : Stm<seq<_> > =
        fun trans -> ms |> Seq.map (fun x -> x trans) |> Seq.cache
    
    let mapM f ms = ms |> Seq.map f |> sequence
    
    let sequence_ (ms : seq<Stm<_> >) : Stm<_> =
        fun trans -> ms |> Seq.iter (fun x -> x trans)
    
    let mapM_ f ms = ms |> Seq.map f |> sequence_
    