namespace FSharpx

open System
open System.Collections
open System.Collections.Generic
#if NET40
open System.Diagnostics.Contracts
#endif
            
/// An ArraySegment with structural comparison and equality.
[<CustomEquality; CustomComparison; SerializableAttribute; StructAttribute>]
type ByteString =
    val Array: byte[]
    val Offset: int
    val Count: int
    new (array: byte[]) = { Array = array; Offset = 0; Count = array.Length }
    new (array: byte[], offset: int, count: int) = { Array = array; Offset = offset; Count = count }
    /// Compares two byte strings based on their structure.
    static member Compare (a:ByteString, b:ByteString) =
        let x,o,l = a.Array, a.Offset, a.Count
        let x',o',l' = b.Array, b.Offset, b.Count
        if x = x' && o = o' && l = l' then 0
        elif x = x' then
            if o = o' then if l < l' then -1 else 1
            else if o < o' then -1 else 1 
        else let left, right = x.[o..(o+l-1)], x'.[o'..(o'+l'-1)] in
             if left = right then 0 elif left < right then -1 else 1
    /// Compares two objects for equality. When both are byte strings, structural equality is used.
    override x.Equals(other) = 
        match other with
        | :? ByteString as other' -> ByteString.Compare(x, other') = 0
        | _ -> false
    /// Gets the hash code for the byte string.
    override x.GetHashCode() = hash x
    /// Gets an enumerator for the bytes stored in the byte string.
    member x.GetEnumerator() =
        if x.Count = 0 then
            { new IEnumerator<_> with 
                member self.Current = invalidOp "!"
              interface System.Collections.IEnumerator with
                member self.Current = invalidOp "!"
                member self.MoveNext() = false
                member self.Reset() = ()
              interface System.IDisposable with 
                member self.Dispose() = () }
        else
            let segment = x.Array
            let minIndex = x.Offset
            let maxIndex = x.Offset + x.Count - 1
            let currentIndex = ref <| minIndex - 1
            { new IEnumerator<_> with 
                member self.Current =
                    if !currentIndex < minIndex then
                        invalidOp "Enumeration has not started. Call MoveNext."
                    elif !currentIndex > maxIndex then
                        invalidOp "Enumeration already finished."
                    else segment.[!currentIndex]
              interface System.Collections.IEnumerator with
                member self.Current =
                    if !currentIndex < minIndex then
                        invalidOp "Enumeration has not started. Call MoveNext."
                    elif !currentIndex > maxIndex then
                        invalidOp "Enumeration already finished."
                    else box segment.[!currentIndex]
                member self.MoveNext() = 
                    if !currentIndex < maxIndex then
                        incr currentIndex
                        true
                    else false
                member self.Reset() = currentIndex := minIndex - 1
              interface System.IDisposable with 
                member self.Dispose() = () }
    interface System.IComparable with
        member x.CompareTo(other) =
            match other with
            | :? ByteString as other' -> ByteString.Compare(x, other')
            | _ -> invalidArg "other" "Cannot compare a value of another type."
    interface System.Collections.Generic.IEnumerable<byte> with
        /// Gets an enumerator for the bytes stored in the byte string.
        member x.GetEnumerator() = x.GetEnumerator()
        /// Gets an enumerator for the bytes stored in the byte string.
        member x.GetEnumerator() = x.GetEnumerator() :> IEnumerator
  
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ByteString =
    /// An active pattern for conveniently retrieving the properties of a ByteString.
    let (|BS|) (x:ByteString) = x.Array, x.Offset, x.Count
    
    let empty = ByteString()
    let singleton c = ByteString(Array.create 1 c, 0, 1)
    let create arr = ByteString(arr, 0, arr.Length)
    let findIndex pred (bs:ByteString) =
        Array.FindIndex(bs.Array, bs.Offset, bs.Count, Predicate<_>(pred))
    let ofArraySegment (segment:ArraySegment<byte>) = ByteString(segment.Array, segment.Offset, segment.Count)
    let ofSeq s = let arr = Array.ofSeq s in ByteString(arr, 0, arr.Length)
    let ofList l = ByteString(Array.ofList l, 0, l.Length)
    let ofString (s:string) = s.ToCharArray() |> Array.map byte |> create
    let toArray (bs:ByteString) =
        if bs.Count = 0 then Array.empty<_>
        else bs.Array.[bs.Offset..(bs.Offset + bs.Count - 1)]
    let toSeq (bs:ByteString) = bs :> seq<byte>
    let toList (bs:ByteString) = List.ofSeq bs
    let toString (bs:ByteString) = System.Text.Encoding.ASCII.GetString(bs.Array, bs.Offset, bs.Count)
    let isEmpty (bs:ByteString) = 
        #if NET40
        Contract.Requires(bs.Count >= 0)
        #else
        assert (bs.Count >= 0)
        #endif
        bs.Count <= 0
    let length (bs:ByteString) = 
        #if NET40
        Contract.Requires(bs.Count >= 0)
        #else
        assert (bs.Count >= 0)
        #endif
        bs.Count
    let index (bs:ByteString) pos =
        #if NET40
        Contract.Requires(bs.Offset + pos <= bs.Count)
        #else
        assert (bs.Offset + pos <= bs.Count)
        #endif
        bs.Array.[bs.Offset + pos]
    let head (bs:ByteString) =
        if bs.Count <= 0 then
          failwith "Cannot take the head of an empty byte string."
        else bs.Array.[bs.Offset]
    let tail (bs:ByteString) =
        #if NET40
        Contract.Requires(bs.Count >= 1)
        #else
        assert (bs.Count >= 1)
        #endif
        if bs.Count = 1 then empty
        else ByteString(bs.Array, bs.Offset+1, bs.Count-1)
    
    /// cons uses Buffer.SetByte and Buffer.BlockCopy for efficient array operations.
    /// Please note that a new array is created and both the head and tail are copied in,
    /// disregarding any additional bytes in the original tail array.
    let cons hd (bs:ByteString) =
        let x,o,l = bs.Array, bs.Offset, bs.Count in
        if l = 0 then singleton hd
        else let buffer = Array.zeroCreate<byte> (l + 1)
             Buffer.SetByte(buffer,0,hd)
             Buffer.BlockCopy(x,o,buffer,1,l)
             ByteString(buffer,0,l+1)
    
    /// append uses Buffer.BlockCopy for efficient array operations.
    /// Please note that a new array is created and both arrays are copied in,
    /// disregarding any additional bytes in the original, underlying arrays.
    let append a b = 
        if isEmpty a then b
        elif isEmpty b then a
        else let x,o,l = a.Array, a.Offset, a.Count
             let x',o',l' = b.Array, b.Offset, b.Count
             let buffer = Array.zeroCreate<byte> (l + l')
             Buffer.BlockCopy(x,o,buffer,0,l)
             Buffer.BlockCopy(x',o',buffer,l,l')
             ByteString(buffer,0,l+l')
    
    let fold f seed bs =
        let rec loop bs acc =
            if isEmpty bs then acc 
            else let hd, tl = head bs, tail bs
                 loop tl (f acc hd)
        loop bs seed
  
    let split pred (bs:ByteString) =
        if isEmpty bs then empty, empty
        else let index = findIndex pred bs
             if index = -1 then bs, empty
             else let count = index - bs.Offset
                  ByteString(bs.Array, bs.Offset, count),
                  ByteString(bs.Array, index, bs.Count - count)
    
    let span pred bs = split (not << pred) bs
    
    let splitAt n (bs:ByteString) =
        #if NET40
        Contract.Requires(n >= 0)
        #else
        assert (n >= 0)
        #endif
        if isEmpty bs then empty, empty
        elif n = 0 then empty, bs
        elif n >= bs.Count then bs, empty
        else let x,o,l = bs.Array, bs.Offset, bs.Count in ByteString(x,o,n), ByteString(x,o+n,l-n)
    
    let skip n bs = splitAt n bs |> snd
    let skipWhile pred bs = span pred bs |> snd
    let skipUntil pred bs = split pred bs |> snd
    let take n bs = splitAt n bs |> fst 
    let takeWhile pred bs = span pred bs |> fst
    let takeUntil pred bs = split pred bs |> fst 
