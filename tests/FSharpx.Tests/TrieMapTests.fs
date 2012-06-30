module FSharpx.DataStructures.Tests.TrieMapTests

open System
open FSharpx.DataStructures
open NUnit.Framework
open FsUnit

// Tests predicated on the assumption that standard F# Map container is good, and is used as a reference.

type TwinMaps<'Key, 'T when 'Key : equality and 'Key : comparison> = {
    TrieMap_SU : TrieMap_SU.TrieMap<'Key, 'T>
    TrieMap_PU : TrieMap_PU.TrieMap<'Key, 'T>
    TrieMap_SS : TrieMap_SS.TrieMap<'Key, 'T>
    Map : Map<'Key, 'T> }

let addToMaps key value twinMaps =
    {
      TrieMap_SU = twinMaps.TrieMap_SU |> TrieMap_SU.TrieMap.add key value
      TrieMap_PU = twinMaps.TrieMap_PU |> TrieMap_PU.TrieMap.add key value
      TrieMap_SS = twinMaps.TrieMap_SS |> TrieMap_SS.TrieMap.add key value
      Map = twinMaps.Map |> Map.add key value }

let removeFromMaps key twinMaps =
    {
      TrieMap_SU = twinMaps.TrieMap_SU |> TrieMap_SU.TrieMap.remove key 
      TrieMap_PU = twinMaps.TrieMap_PU |> TrieMap_PU.TrieMap.remove key
      TrieMap_SS = twinMaps.TrieMap_SS |> TrieMap_SS.TrieMap.remove key
      Map = twinMaps.Map |> Map.remove key }

let emptyMaps = { TrieMap_SU = TrieMap_SU.TrieMap.empty; TrieMap_PU = TrieMap_PU.TrieMap.empty; TrieMap_SS = TrieMap_SS.TrieMap.empty; Map = Map.empty }

type AssignedHashTestKey (keyValue : int, keyHash : int) =
    member this.GetKey() = keyValue
    override x.GetHashCode() = keyHash
    override x.Equals(yobj) = 
        match yobj with
        | :? AssignedHashTestKey as kv -> kv.GetKey() = keyValue
        | _ -> false

let mapsInSynch twinMaps =
    let matchCount =
        Seq.zip3
            (Seq.map2 (=) (twinMaps.TrieMap_SU |> Seq.sortBy fst) (twinMaps.Map |> Map.toSeq |> Seq.sortBy fst))
            (Seq.map2 (=) (twinMaps.TrieMap_PU |> Seq.sortBy fst) (twinMaps.Map |> Map.toSeq |> Seq.sortBy fst))
            (Seq.map2 (=) (twinMaps.TrieMap_SS |> Seq.sortBy fst) (twinMaps.Map |> Map.toSeq |> Seq.sortBy fst))
        |> Seq.map (fun (a, b, c) -> a && b && c)
        |> Seq.filter (fun x -> x) |> Seq.length
    let keys = twinMaps.Map |> Map.toSeq |> Seq.map fst
    let getMatchCount =
        keys
        |> Seq.map
            (fun key ->
                let fromRef = twinMaps.Map.[key]
                let from_SU = twinMaps.TrieMap_SU.[key]
                let from_PU = twinMaps.TrieMap_PU.[key]
                let from_SS = twinMaps.TrieMap_SS.[key]
                (fromRef = from_SU) && (fromRef = from_PU) && (fromRef = from_SS))
        |> Seq.filter (fun x -> x) |> Seq.length
    (matchCount = twinMaps.Map.Count) && (matchCount = twinMaps.TrieMap_SU.Count) && (matchCount = twinMaps.TrieMap_PU.Count) && (getMatchCount = twinMaps.Map.Count)
    && (matchCount = (twinMaps.TrieMap_SU |> TrieMap_SU.TrieMap.toSeq |> Seq.length))
    && (matchCount = (twinMaps.TrieMap_PU |> TrieMap_PU.TrieMap.toSeq |> Seq.length))
    && (matchCount = (twinMaps.TrieMap_SS |> TrieMap_SS.TrieMap.toSeq |> Seq.length))


// The deletion test here is far from complete - neither of the tests really exhaustively attacks every deletion case.
[<Test>]
let ``a big bunch of distinct Adds and deletes that should result in contents stored properly``() =
    let numElements = 1777 // 1000 // 5
    let entries = List.init numElements (fun n -> (Guid.NewGuid().ToString() (* n.ToString() *), Guid.NewGuid().ToString()))
    let maps = entries |> List.fold (fun maps (k, v) -> addToMaps k v maps) emptyMaps
    maps |> mapsInSynch |> should equal true

    let allKeysRemaining = maps.Map |> Map.toSeq |> Seq.map fst |> Seq.toList

    let mapsAfterDelete =
        allKeysRemaining
        |> Seq.fold
            (fun maps k ->
                let newMaps = removeFromMaps k maps
                newMaps |> mapsInSynch |> should equal true
                newMaps)
           maps
    mapsAfterDelete |> mapsInSynch |> should equal true
