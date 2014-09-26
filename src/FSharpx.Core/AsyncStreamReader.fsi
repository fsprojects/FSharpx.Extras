// First version copied from the F# Power Pack 
// https://raw.github.com/fsharp/powerpack/master/src/FSharp.PowerPack/AsyncStreamReader.fsi

namespace FSharpx.Control

open System
open System.IO
open System.Text

/// <summary>
/// Implements a TextReader-like API that asynchronously reads characters from 
/// a byte stream in a particular encoding.
/// </summary>
[<Sealed>]
type AsyncStreamReader =

    /// Creates a new AsyncStreamReader for the given stream.  The 
    /// character encoding is set by encoding and the buffer size,
    /// in number of 16-bit characters, is set by bufferSize. 
    /// 
    /// Note that detectEncodingFromByteOrderMarks is a very
    /// loose attempt at detecting the encoding by looking at the first 
    /// 3 bytes of the stream.  It will recognize UTF-8, little endian
    /// unicode, and big endian unicode text, but that's it.  If neither
    /// of those three match, it will use the Encoding you provided.
    new : stream:Stream * encoding:Encoding * detectEncodingFromByteOrderMarks:bool * bufferSize:int -> AsyncStreamReader
    new : stream:Stream -> AsyncStreamReader
    new : stream:Stream * detectEncodingFromByteOrderMarks:bool -> AsyncStreamReader
    new : stream:Stream * encoding:System.Text.Encoding -> AsyncStreamReader
    new : stream:Stream * encoding:System.Text.Encoding * detectEncodingFromByteOrderMarks:bool -> AsyncStreamReader
    
    member Close : unit -> unit
    member CurrentEncoding : Encoding
    member BaseStream : Stream
    
    ///. DiscardBufferedData tells StreamReader to throw away its internal 
    ///. buffer contents.  This is useful if the user needs to seek on the
    /// underlying stream to a known location then wants the StreamReader 
    /// to start reading from this new point.  This method should be called
    /// very sparingly, if ever, since it can lead to very poor performance.
    /// However, it may be the only way of handling some scenarios where
    /// users need to re-read the contents of a StreamReader a second time. 
    member DiscardBufferedData : unit -> unit
    
    /// An async that produces true if the reader is at the end of stream and false otherwise
    ///
    /// Note that when the async is run it reflects the reader state at the time of running; multiple runs will 
    /// yield different results.
    member EndOfStream : Async<bool>
    
    /// Creates an async that produces next character from the stream without advancing the stream
    ///
    /// Note that when the async is run it reflects the reader state at the time of running; multiple runs will 
    /// yield different results.
    
    member Peek : unit -> Async<int> 
    
    /// Creates an async that reads next character from the stream
    ///
    /// Note that when the async is run it reflects the reader state at the time of running; multiple runs will 
    /// yield different results.
    member Read : unit -> Async<char>
    
    /// Creates an async that reads all the charactes that are avilable in the stream up to <c>count</c characters and puts them 
    /// into <c>buffer</c> starting at <c>index</c>. The async returns the number of characters that are read.
    ///
    /// Note that when the async is run it reflects the reader state at the time of running; multiple runs will 
    /// yield different results.
    member Read : buffer:char[] * index:int * count:int -> Async<int>
    
    /// Creates an async that reads exactly <c>count</c> characters from the stream unless end of stream is reached and puts them 
    /// into <c>buffer</c> starting at <c>index</c>. The async returns the number of characters that are read (if end-of-stream is not reached
    /// that will be <c>count</c>
    ///
    /// Note that when the async is run it reflects the reader state at the time of running; multiple runs will 
    /// yield different results.
    member ReadExactly : buffer:char[] * index:int * count:int -> Async<int>
    
    /// Creates an async that read all characters in the stream up to the end.
    ///
    /// Note that when the async is run it reflects the reader state at the time of running; multiple runs will 
    /// yield different results.
    member ReadToEnd : unit -> Async<string>
    
    /// Creates an async that reads next line from the stream
    ///
    /// Note that when the async is run it reflects the reader state at the time of running; multiple runs will 
    /// yield different results.
    member ReadLine : unit -> Async<string>
    
    interface IDisposable
    