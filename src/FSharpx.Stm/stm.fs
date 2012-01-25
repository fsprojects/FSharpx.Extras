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
/// <summary>The Software Transactional Memory monad.</summary>
/// <see href="http://cs.hubfs.net/blogs/hell_is_other_languages/archive/2008/01/16/4565.aspx" />
module FSharpx.Stm.Core

open System
open System.Collections.Generic
open System.Threading

[<AbstractClass>]
type TVar() =
    static let nextId = ref 0
    let _id = Interlocked.Increment(nextId)
    member private __.Id = _id
    interface IComparable<TVar> with
        member __.CompareTo(other) = _id.CompareTo(other.Id)

[<Sealed>]
type TVar<'T> internal (value: 'T, cmp: IEqualityComparer<'T>) =
    inherit TVar()
    let mutable _value = value
    member internal __.Value 
        with get () = _value
        and set value = _value <- value
    member internal __.Comparer = cmp

type private IEntry =
    abstract Location : TVar
    abstract IsValid : unit -> bool
    abstract Commit : unit -> unit
    abstract MergeNested : IEntry -> unit

[<Sealed>]
type private Entry<'T> private (location: TVar<'T>, value: 'T, hasOldValue) =
    let _oldValue = location.Value
    let mutable _newValue = value
    new (location, value) = Entry(location, value, false)
    new (location) = Entry(location, location.Value, true)
    member internal __.OldValue = _oldValue
    member internal __.NewValue 
        with get () = _newValue
        and set value = _newValue <- value
    interface IEntry with
        member __.Location = location :> _
        member __.Commit() = location.Value <- _newValue
        member __.MergeNested(entry) = (entry :?> Entry<'T>).NewValue <- _newValue
        member __.IsValid() = not hasOldValue || location.Comparer.Equals( location.Value, _oldValue)

[<Sealed>]
type private ReferenceEqualityComparer<'T when 'T : not struct and 'T : equality>() =
    interface IEqualityComparer<'T> with
        member __.Equals(x, y) = obj.ReferenceEquals(x, y)
        member __.GetHashCode(x) = x.GetHashCode()

[<Sealed>]
type private EquatableEqualityComparer<'T when 'T :> IEquatable<'T> and 'T : struct and 'T : equality>() =
    interface IEqualityComparer<'T> with
        member __.Equals(x, y) = x.Equals(y)
        member __.GetHashCode(x) = x.GetHashCode()

[<Sealed>]
type private AnyEqualityComparer<'T when 'T : equality>() =
    interface IEqualityComparer<'T> with
        member __.Equals(x, y) = x.Equals(y)
        member __.GetHashCode(x) = x.GetHashCode()
        
type private RetryException() =
    inherit Exception()

type private CommitFailedException() =
    inherit Exception()

[<Sealed; AllowNullLiteral>]
type TLog private (outer) =
    static let locker = obj()
    let log = SortedDictionary<TVar,IEntry>()
    private new () = TLog(null)
    member private __.Log = log
    member private __.Outer = outer
    static member NewTVarClass(value) = TVar<_>(value, ReferenceEqualityComparer())
    static member NewTVarStruct(value) = TVar<_>(value, EquatableEqualityComparer())
    static member NewTVarBoxedStruct(value) = TVar<_>(value, AnyEqualityComparer())
    static member NewTVar(value: 'T) =
        let ty = typeof<'T>
        let ect =
            if not ty.IsValueType then typedefof<ReferenceEqualityComparer<_>>
            elif typeof<IEquatable<'T>>.IsAssignableFrom(ty) then typedefof<EquatableEqualityComparer<_>>
            else typedefof<AnyEqualityComparer<_>>
        let cmp = Activator.CreateInstance(ect.MakeGenericType(ty)) :?> _
        TVar<_>(value, cmp)
    member this.ReadTVar(location) =
        let rec loop (trans: TLog) =
            match trans.Log.TryGetValue(location) with
            | true, (:? Entry<_> as entry) -> entry.NewValue
            | _ -> 
                match trans.Outer with 
                | null -> 
                    let entry = Entry<_>(location)
                    log.Add(location, entry)
                    entry.OldValue
                | outer -> loop outer
        loop this
    member __.WriteTVar(location, value: 'T) =
        match log.TryGetValue(location) with
        | true, (:? Entry<'T> as entry) -> entry.NewValue <- value
        | _ ->
            let entry = Entry<_>(location, value)
            log.Add(location, entry)
    member private __.IsValidSingle() =
        log.Values |> Seq.forall (fun entry -> entry.IsValid())
    member internal this.IsValid() =
        this.IsValidSingle() && (obj.ReferenceEquals(outer, null) || outer.IsValid())
    member internal __.Commit() =
        match outer with
        | null -> for entry in log.Values do entry.Commit()
        | _ -> raise (InvalidOperationException())
    member internal this.StartNested() = TLog(this)
    member internal __.MergeNested() =
        for innerEntry in log.Values do
            match outer.Log.TryGetValue(innerEntry.Location) with
            | true, outerEntry -> innerEntry.MergeNested(outerEntry)
            | _ -> outer.Log.Add(innerEntry.Location, innerEntry)
    member internal __.Wait() = ()
    member internal __.UnWait() = ()
    member private __.Lock() = Monitor.Enter(locker)
    member private __.UnLock() = Monitor.Exit(locker)
    member private __.Block() = Monitor.Wait(locker) |> ignore
    member private __.Signal() = Monitor.PulseAll(locker)
    static member Atomic<'T>(p: TLog -> 'T) =
        let trans = TLog()
        let rec loop() =
            try
                let result = p trans
                trans.Lock()
                let isValid = trans.IsValid()
                if isValid then
                    trans.Commit()
                    trans.Signal()
                trans.UnLock()
                if isValid then result
                else cont()
            with
                | :? RetryException -> retry()
                | :? CommitFailedException
                | :? ThreadInterruptedException -> reraise()
                | _ ->
                    trans.Lock()
                    let isValid = trans.IsValid()
                    trans.UnLock()
                    if isValid then reraise()
                    else cont()
        and cont() =
            trans.Log.Clear()
            Thread.Sleep(0)
            loop()
        and retry() =
            trans.Lock()
            let isValid = trans.IsValid()
            if isValid then
                trans.Wait()
                try
                    let rec loop() =
                        trans.Block()
                        if trans.IsValid() then loop()
                    loop()
                finally
                    trans.UnWait()
                    trans.UnLock()
            else trans.UnLock()
            cont()
        loop()
    static member Atomic(p: TLog -> unit) = TLog.Atomic<_>(p) |> ignore
    member __.Retry() =
        raise (RetryException())
    member this.Retry() = this.Retry() |> ignore
    member this.OrElse<'T>(p: TLog -> 'T, q: TLog -> 'T) =
        let first = this.StartNested()
        try
            let result = p first
            first.Lock()
            let isValid = first.IsValid()
            first.UnLock()
            if isValid then 
                first.MergeNested()
                result
            else
                raise (CommitFailedException())
        with
            | :? RetryException ->
                let second = this.StartNested()
                try
                    let result = q second
                    second.Lock()
                    let isValid = second.IsValid()
                    if isValid then 
                        second.MergeNested()
                        result
                    else
                        raise (CommitFailedException())
                with 
                    | :? RetryException ->
                        this.Lock()
                        let isValid = first.IsValidSingle() && second.IsValidSingle() && this.IsValid()
                        this.UnLock()
                        if isValid then 
                            first.MergeNested()
                            second.MergeNested()
                            reraise()
                        else
                            raise (CommitFailedException())
                    | :? CommitFailedException 
                    | :? ThreadInterruptedException ->
                        reraise()
                    | _ ->
                        second.Lock()
                        let isValid = second.IsValid()
                        second.UnLock()
                        if isValid then
                            second.MergeNested()
                            reraise()
                        else
                            raise (CommitFailedException())
            | :? CommitFailedException
            | :? ThreadInterruptedException ->
                reraise()
            | _ ->
                first.Lock()
                let isValid = first.IsValid()
                first.UnLock()
                if isValid then
                    first.MergeNested()
                    reraise()
                else raise (CommitFailedException())
    member this.OrElse(p: TLog -> unit, q: TLog -> unit) = this.OrElse<_>(p, q) |> ignore

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
    
