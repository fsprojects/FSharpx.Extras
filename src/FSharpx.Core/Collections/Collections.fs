namespace FSharpx

open System
open System.Collections
open System.Collections.Generic
#if NET40
open System.Diagnostics.Contracts
#endif
open System.Runtime.CompilerServices
            
module Enumerator = 
 
    let singleton a = 
        let state = ref 0
        { new IEnumerator<_> with 
            member self.Current = if (state.Value = 1) then a else invalidOp "!"
          interface System.Collections.IEnumerator with
            member self.Current = if (state.Value = 1) then box a else invalidOp "!"
            member self.MoveNext() = state := state.Value + 1; state.Value = 1
            member self.Reset() = state := 0
          interface System.IDisposable with 
            member self.Dispose() = () }
  
    let bind (f:_ -> IEnumerator<'b>) (a:IEnumerator<'a>) =
        if a.MoveNext() then f (Some(a.Current)) else f(None) 
   
    let combine (a:IEnumerator<_>) b = 
        let current = ref a
        let first = ref true
        { new IEnumerator<_> with 
            member self.Current = current.Value.Current
          interface System.Collections.IEnumerator with
            member self.Current = box current.Value.Current
            member self.MoveNext() = 
                if current.Value.MoveNext() then true 
                elif first.Value then 
                    current := b
                    first := false
                    current.Value.MoveNext()
                else false
            member self.Reset() = 
                a.Reset(); b.Reset()
                first := true; current := a
          interface System.IDisposable with 
            member self.Dispose() = a.Dispose(); b.Dispose() }
    
    let empty<'a> : IEnumerator<'a> = 
        { new IEnumerator<_> with 
            member self.Current = invalidOp "!"
          interface System.Collections.IEnumerator with
            member self.Current = invalidOp "!"
            member self.MoveNext() = false
            member self.Reset() = ()
          interface System.IDisposable with 
            member self.Dispose() = ()}
    
    let delay f = 
        let en : Lazy<IEnumerator<_>> = lazy f()
        { new IEnumerator<_> with 
            member self.Current = en.Value.Current
          interface System.Collections.IEnumerator with
            member self.Current = box en.Value.Current
            member self.MoveNext() = en.Value.MoveNext()
            member self.Reset() = en.Value.Reset()
          interface System.IDisposable with 
            member self.Dispose() = en.Value.Dispose() }
   
    let toSeq gen = 
        { new IEnumerable<'T> with 
              member x.GetEnumerator() = gen()
          interface IEnumerable with 
              member x.GetEnumerator() = (gen() :> IEnumerator) }

    type EnumerableBuilder() = 
        member x.Delay(f) = delay f
        member x.YieldFrom(a) = a
        member x.Yield(a) = singleton a
        member x.Bind(a:IEnumerator<'a>, f:_ -> IEnumerator<'b>) = bind f a
        member x.Combine(a, b) = combine a b
        member x.Zero() = empty<_>
    let iter = new EnumerableBuilder()    

    let head (en:IEnumerator<'a>) =
        if en.MoveNext() then en.Current
        else invalidOp "!"

    let firstOrDefault def (en:IEnumerator<'a>) =
        if en.MoveNext() then en.Current
        else def

    let rec lastOrDefault def (en:IEnumerator<'a>) =
        if en.MoveNext() then
            lastOrDefault en.Current en
        else def

    let last (en:IEnumerator<'a>) =
        if en.MoveNext() then
            lastOrDefault en.Current en
        else invalidOp "!"

    let length (en:IEnumerator<'a>) =
        let rec loop acc =
            if en.MoveNext() then loop (acc+1)
            else acc
        loop 0
  
    let skip n (en:IEnumerator<'a>) =
        if n = 0 then en
        else
            let rec loop acc =
                if not (en.MoveNext()) then empty<_>
                elif n = acc then en
                else loop (acc+1)
            loop 1
  
    let take n (en:IEnumerator<'a>) =
        if n = 0 then empty<_>
        else
            let rec loop acc = iter {
                let! v = en
                match v with
                | None -> ()
                | Some v' ->
                    yield v'
                    if acc = n then ()
                    else yield! loop (acc+1) }
            loop 1

    // Implementing the zip function for enumerators
    let rec zip xs ys = iter {
        let! x = xs
        let! y = ys
        match x, y with 
        | Some(x), Some(y) ->
            yield x, y
            yield! zip xs ys 
        | _ -> () }
   
    let rec map f en = iter {
        let! x = en
        match x with 
        | Some(x) -> yield f x
                     yield! map f en
        | _ -> () }               

    /// Scan progressively folds over the enumerator, returning a new enumerator
    /// that lazily computes each state.
    let rec scan f state (en : IEnumerator<_>) = iter {
        yield state
        if en.MoveNext() then
            let state' = f state en.Current
            yield! scan f state' en }

    let private scanWithPredicate f state pred (en : IEnumerator<_>) = iter {
        yield state
        if en.MoveNext() then
            let state' = f state en.Current
            if pred state' then
                yield! scan f state' en }

    /// Scan progressively folds over the enumerator, returning a new enumerator
    /// that lazily computes each state while the provided predicate is true.
    let scanWhile f state pred en = scanWithPredicate f state pred en

    /// Scan progressively folds over the enumerator, returning a new enumerator
    /// that lazily computes each state until the provided predicate is true.
    let scanUntil f state pred en = scanWithPredicate f state (not << pred) en
   
module Seq =
    /// <summary>
    /// Adds an index to a sequence
    /// </summary>
    /// <param name="a"></param>
    let inline index a = Seq.mapi tuple2 a

    /// <summary>
    /// Returns the first element (with its index) for which the given function returns true.
    /// Return None if no such element exists.
    /// </summary>
    /// <param name="pred">Predicate</param>
    /// <param name="l">Sequence</param>
    let tryFindWithIndex pred l =
        l |> index |> Seq.tryFind (fun (_,v) -> pred v)

    let inline lift2 f l1 l2 = 
        seq {
            for i in l1 do
                for j in l2 do
                    yield f i j }
        

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Array = 
    let inline nth i arr = Array.get arr i
    let inline setAt i v arr = Array.set arr i v; arr

module List =
    /// Curried cons
    let inline cons hd tl = hd::tl
  
    let inline singleton x = [x]

    let inline lift2 f (l1: _ list) (l2: _ list) = 
        [ for i in l1 do
            for j in l2 do
                yield f i j ]

  
    let span pred l =
        let rec loop l cont =
            match l with
            | [] -> ([],[])
            | x::[] when pred x -> (cont l, [])
            | x::xs when not (pred x) -> (cont [], l)
            | x::xs when pred x -> loop xs (fun rest -> cont (x::rest))
            | _ -> failwith "Unrecognized pattern"
        loop l id
  
    let split pred l = span (not << pred) l
  
    let skipWhile pred l = span pred l |> snd
    let skipUntil pred l = split pred l |> snd
    let takeWhile pred l = span pred l |> fst
    let takeUntil pred l = split pred l |> fst
    
    let splitAt n l =
        let pred i = i >= n
        let rec loop i l cont =
            match l with
            | [] -> ([],[])
            | x::[] when not (pred i) -> (cont l, [])
            | x::xs when pred i -> (cont [], l)
            | x::xs when not (pred i) -> loop (i+1) xs (fun rest -> cont (x::rest))
            | _ -> failwith "Unrecognized pattern"
        loop 0 l id
  
    let skip n l = splitAt n l |> snd
    let take n l = splitAt n l |> fst

    let inline mapIf pred f =
        List.map (fun x -> if pred x then f x else x)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Dictionary =
    let tryFind key (d: IDictionary<_,_>) =
        match d.TryGetValue key with
        | true,v -> Some v
        | _ -> None

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<Extension>]
module NameValueCollection =
    open System.Collections.Specialized
    open System.Linq

    /// <summary>
    /// Returns a new <see cref="NameValueCollection"/> with the concatenation of two <see cref="NameValueCollection"/>s
    /// </summary>
    /// <param name="a"></param>
    /// <param name="b"></param>
    [<Extension>]
    [<CompiledName("Concat")>]
    let concat a b = 
        let x = NameValueCollection()
        x.Add a
        x.Add b
        x

    /// <summary>
    /// In-place add of a key-value pair to a <see cref="NameValueCollection"/>
    /// </summary>
    /// <param name="x"></param>
    /// <param name="a"></param>
    /// <param name="b"></param>
    let inline addInPlace (x: NameValueCollection) (a,b) = x.Add(a,b)

    /// Adds an element to a copy of an existing NameValueCollection
    let add name value (x: NameValueCollection) =
        let r = NameValueCollection x
        r.Add(name,value)
        r

    /// <summary>
    /// Returns a <see cref="NameValueCollection"/> as an array of key-value pairs.
    /// Note that keys may be duplicated.
    /// </summary>
    /// <param name="a"></param>
    [<Extension>]
    [<CompiledName("ToArray")>]
    let toArray (a: NameValueCollection) =
        a.AllKeys
        |> Array.collect (fun k -> a.GetValues k |> Array.map (fun v -> k,v))

    /// <summary>
    /// Returns a <see cref="NameValueCollection"/> as a sequence of key-value pairs.
    /// Note that keys may be duplicated.
    /// </summary>
    /// <param name="a"></param>
    [<Extension>]
    [<CompiledName("ToEnumerable")>]
    let toSeq (a: NameValueCollection) =
        a.AllKeys
        |> Seq.collect (fun k -> a.GetValues k |> Seq.map (fun v -> k,v))

    /// <summary>
    /// Returns a <see cref="NameValueCollection"/> as a list of key-value pairs.
    /// Note that keys may be duplicated.
    /// </summary>
    /// <param name="a"></param>
    let inline toList a = toSeq a |> Seq.toList

    /// <summary>
    /// Creates a <see cref="NameValueCollection"/> from a list of key-value pairs
    /// </summary>
    /// <param name="l"></param>
    let fromSeq l =
        let x = NameValueCollection()
        Seq.iter (addInPlace x) l
        x

    [<Extension>]
    [<CompiledName("ToLookup")>]
    let toLookup a =
        let s = toSeq a
        s.ToLookup(fst, snd)

    [<Extension>]
    [<CompiledName("AsDictionary")>]
    let asDictionary (x: NameValueCollection) =
        let notimpl() = raise <| NotImplementedException()
        let getEnumerator() =
            let enum = x.GetEnumerator()
            let wrapElem (o: obj) = 
                let key = o :?> string
                let values = x.GetValues key
                KeyValuePair(key,values)
            { new IEnumerator<KeyValuePair<string,string[]>> with
                member e.Current = wrapElem enum.Current
                member e.MoveNext() = enum.MoveNext()
                member e.Reset() = enum.Reset()
                member e.Dispose() = ()
                member e.Current = box (wrapElem enum.Current) }
        { new IDictionary<string,string[]> with
            member d.Count = x.Count
            member d.IsReadOnly = false 
            member d.Item 
                with get k = 
                    let v = x.GetValues k
                    if v = null
                        then raise <| KeyNotFoundException(sprintf "Key '%s' not found" k)
                        else v
                and set k v =
                    x.Remove k
                    for i in v do
                        x.Add(k,i)
            member d.Keys = upcast ResizeArray<string>(x.Keys |> Seq.cast)
            member d.Values = 
                let values = ResizeArray<string[]>()
                for i in 0..x.Count-1 do
                    values.Add(x.GetValues i)
                upcast values
            member d.Add v = d.Add(v.Key, v.Value)
            member d.Add(key,value) = 
                if key = null
                    then raise <| ArgumentNullException("key")
                if d.ContainsKey key
                    then raise <| ArgumentException(sprintf "Duplicate key '%s'" key, "key")
                d.[key] <- value
            member d.Clear() = x.Clear()
            member d.Contains item = x.GetValues(item.Key) = item.Value
            member d.ContainsKey key = x.[key] <> null
            member d.CopyTo(array,arrayIndex) = notimpl()
            member d.GetEnumerator() = getEnumerator()
            member d.GetEnumerator() = getEnumerator() :> IEnumerator
            member d.Remove (item: KeyValuePair<string,string[]>) = 
                if d.Contains item then
                    x.Remove item.Key
                    true
                else
                    false
            member d.Remove (key: string) = 
                let exists = d.ContainsKey key
                x.Remove key
                exists
            member d.TryGetValue(key: string, value: byref<string[]>) = 
                if d.ContainsKey key then
                    value <- d.[key]
                    true
                else false
            }

    [<Extension>]
    [<CompiledName("AsReadonlyDictionary")>]
    let asReadonlyDictionary x =
        let a = asDictionary x
        let notSupported() = raise <| NotSupportedException("Readonly dictionary")
        { new IDictionary<string,string[]> with
            member d.Count = a.Count
            member d.IsReadOnly = true
            member d.Item 
                with get k = a.[k]
                and set k v = notSupported()
            member d.Keys = a.Keys
            member d.Values = a.Values
            member d.Add v = notSupported()
            member d.Add(key,value) = notSupported()
            member d.Clear() = notSupported()
            member d.Contains item = a.Contains item
            member d.ContainsKey key = a.ContainsKey key
            member d.CopyTo(array,arrayIndex) = a.CopyTo(array,arrayIndex)
            member d.GetEnumerator() = a.GetEnumerator()
            member d.GetEnumerator() = a.GetEnumerator() :> IEnumerator
            member d.Remove (item: KeyValuePair<string,string[]>) = notSupported(); false
            member d.Remove (key: string) = notSupported(); false
            member d.TryGetValue(key: string, value: byref<string[]>) = a.TryGetValue(key, ref value)
        }                

    [<Extension>]
    [<CompiledName("AsLookup")>]
    let asLookup (this: NameValueCollection) =
        let getEnumerator() = 
            let e = this.GetEnumerator()
            let wrapElem (o: obj) = 
                let key = o :?> string
                let values = this.GetValues key :> seq<string>
                { new IGrouping<string,string> with
                    member x.Key = key
                    member x.GetEnumerator() = values.GetEnumerator()
                    member x.GetEnumerator() = values.GetEnumerator() :> IEnumerator }
  
            { new IEnumerator<IGrouping<string,string>> with
                member x.Current = wrapElem e.Current
                member x.MoveNext() = e.MoveNext()
                member x.Reset() = e.Reset()
                member x.Dispose() = ()
                member x.Current = box (wrapElem e.Current) }
                      
        { new ILookup<string,string> with
            member x.Count = this.Count
            member x.Item 
                with get key = 
                    match this.GetValues key with
                    | null -> Seq.empty
                    | a -> upcast a
            member x.Contains key = this.Get key <> null
            member x.GetEnumerator() = getEnumerator()
            member x.GetEnumerator() = getEnumerator() :> IEnumerator }
