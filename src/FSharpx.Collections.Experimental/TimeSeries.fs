/// Originally from https://bitbucket.org/colinbul/fsharpent
namespace FSharpx.Collections.Experimental

open System
open System.Collections
open System.Collections.Generic
open FSharpx.Collections

type Timeseries<'T>(startDate:DateTimeOffset, granularity:TimeSpan, position:int, values:seq<'T>) = 

    let buffer = RingBuffer<'T>(position, values)

    let mutable startDate = startDate

    let convertToGranularity (targetGran:TimeSpan) (currGran:TimeSpan) (values:seq<'T>) =
        if currGran = targetGran then 
            values 
        elif currGran > targetGran then
            let ratio = currGran.TotalMinutes / targetGran.TotalMinutes |> int
            values |> Seq.grow ratio
        else
            let ratio = targetGran.TotalMinutes / currGran.TotalMinutes |> int
            values |> Seq.contract ratio
    
    let convertToSeries (startDate:DateTimeOffset) (gran:TimeSpan) (seq:'T[]) =
        Array.mapi (fun i x -> startDate.AddMinutes((i |> float) * gran.TotalMinutes), x) seq

    let offset (a:DateTimeOffset) (b:DateTimeOffset) = 
        (b.Subtract(a).TotalMinutes / granularity.TotalMinutes) |> floor |> int
    
    new (startDate, granularity, values) = 
        Timeseries(startDate, granularity, 0, values)

    new (startDate, granularity, size) = 
        Timeseries(startDate, granularity, Array.zeroCreate size)
    
    static member Empty(startDate, granularity, size) = 
        Timeseries(startDate, granularity, Array.zeroCreate size)       

    member x.Buffer with get() = buffer
    member x.StartDate with get() = startDate and set(value) = startDate <- value
    member x.Granularity with get() = granularity

    member x.AsTimeseries(?startDate:DateTimeOffset) =
        let date = defaultArg startDate x.StartDate
        x.Buffer.ToArray() |> convertToSeries x.StartDate x.Granularity |> Array.filter (fun (d,_) -> d >= date)

    member x.ToGranularity (granularity:TimeSpan) =
        if granularity = x.Granularity then 
            x
        else 
            Timeseries(x.StartDate, granularity, x.Buffer.ToArray() |> convertToGranularity granularity x.Granularity)

    member x.Series
        with get() = x.Buffer.ToArray() |> convertToSeries x.StartDate x.Granularity

    /// Tries to advance the start date of the Timeseries to toDate.
    /// Returns None if toDate is before the start date of the Timeseries, 
    /// otherwise Some containing the start date of the Timeseries.
    member x.TryAdvance(toDate:DateTimeOffset) = 
        let offsetIndex = offset x.StartDate toDate            
        match x.Buffer.TryAdvance(offsetIndex) with
        | Some(_) ->
            if offsetIndex > 0 then
                x.Buffer.Normalize()
                x.StartDate <- toDate
            Some(x.StartDate)
        | None -> None
    
    /// Advances the start date of the Timeseries to toDate. Throws an 
    /// ArgumentException if toDate is before the Timeseries start date.
    member x.Advance(toDate:DateTimeOffset) = 
        match x.TryAdvance(toDate) with
        | Some(startDate) -> startDate
        | None -> invalidArg "toDate" "the toDate must be greater than or equal to the start date of the Timeseries"

    member x.Insert(op:('T -> 'T -> 'T), startDate:DateTimeOffset, gran:TimeSpan, dataToInsert:seq<'T>) =
        let data = dataToInsert |> convertToGranularity x.Granularity gran |> Seq.toArray
        let offsetIndex = offset x.StartDate startDate
        if offsetIndex < 0 then
            if abs offsetIndex < data.Length then
                x.Buffer.Insert(op, 0, data.[abs offsetIndex ..])
        else
            x.Buffer.Insert(op, offsetIndex, data)

    member x.Merge(f:('T -> 'T -> 'T), ts:Timeseries<'T>) =
        x.Insert(f, ts.StartDate, ts.Granularity, ts.Buffer.ToArray())

    member x.Clone() = 
        Timeseries<'T>(x.StartDate, x.Granularity, x.Buffer.Position, x.Buffer.ToArray())     