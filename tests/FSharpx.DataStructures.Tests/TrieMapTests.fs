module FSharpx.DataStructures.Tests.TrieMapTests

open System
open FSharpx.DataStructures
open FSharpx.DataStructures.TrieMap
open NUnit.Framework
open FsUnit

// Tests predicated on the assumption that standard F# Map container is good, and is used as a reference.

type TwinMaps<'Key, 'T when 'Key : equality and 'Key : comparison> = {
    TrieMap : TrieMap<'Key, 'T>
    Map : Map<'Key, 'T> }

let addToMaps key value twinMaps =
    { TrieMap = twinMaps.TrieMap |> TrieMap.add key value
      Map = twinMaps.Map |> Map.add key value }

let emptyMaps = { TrieMap = TrieMap.empty; Map = Map.empty }

let mapsInSynch twinMaps =
    Seq.map2 (=) (twinMaps.TrieMap |> Seq.sortBy fst) (twinMaps.Map |> Map.toSeq |> Seq.sortBy fst) |> Seq.filter (fun x -> x) |> Seq.length = twinMaps.Map.Count


[<Test>]
let ``a big bunch of distinct Adds should result in contents stored properly``() =
    List.init 10000 (fun _ -> (Guid.NewGuid().ToString(), Guid.NewGuid().ToString())) |> List.fold (fun maps (k, v) -> addToMaps k v maps) emptyMaps |> mapsInSynch |> should equal true

