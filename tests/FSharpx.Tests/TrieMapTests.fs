module FSharpx.DataStructures.Tests.TrieMapTests

open System
open FSharpx.DataStructures
open FSharpx.DataStructures.TrieMap_SU
open FSharpx.DataStructures.TrieMap_PU
open NUnit.Framework
open FsUnit

// Tests predicated on the assumption that standard F# Map container is good, and is used as a reference.

type TwinMaps<'Key, 'T when 'Key : equality and 'Key : comparison> = {
    TrieMap_SU : TrieMap_SU<'Key, 'T>
    PTrieMap : TrieMap_PU<'Key, 'T>
    Map : Map<'Key, 'T> }

let addToMaps key value twinMaps =
    {
      TrieMap_SU = twinMaps.TrieMap_SU |> TrieMap_SU.add key value
      PTrieMap = twinMaps.PTrieMap |> TrieMap_PU.add key value
      Map = twinMaps.Map |> Map.add key value }

let removeFromMaps key twinMaps =
    {
      TrieMap_SU = twinMaps.TrieMap_SU |> TrieMap_SU.remove key 
      PTrieMap = twinMaps.PTrieMap |> TrieMap_PU.remove key
      Map = twinMaps.Map |> Map.remove key }

let emptyMaps = { TrieMap_SU = TrieMap_SU.empty; PTrieMap = TrieMap_PU.empty; Map = Map.empty }

type AssignedHashTestKey (keyValue : int, keyHash : int) =
    member this.GetKey() = keyValue
    override x.GetHashCode() = keyHash
    override x.Equals(yobj) = 
        match yobj with
        | :? AssignedHashTestKey as kv -> kv.GetKey() = keyValue
        | _ -> false

let mapsInSynch twinMaps =
    let matchCount =
        Seq.map2 (&&)
            (Seq.map2 (=) (twinMaps.TrieMap_SU |> Seq.sortBy fst) (twinMaps.Map |> Map.toSeq |> Seq.sortBy fst))
            (Seq.map2 (=) (twinMaps.PTrieMap |> Seq.sortBy fst) (twinMaps.Map |> Map.toSeq |> Seq.sortBy fst))
         |> Seq.filter (fun x -> x) |> Seq.length
    let keys = twinMaps.Map |> Map.toSeq |> Seq.map fst
    let getMatchCount =
        keys
        |> Seq.map
            (fun key ->
                let fromRef = twinMaps.Map.[key]
                let fromTM = twinMaps.TrieMap_SU.[key]
                let fromPTM = twinMaps.PTrieMap.[key]
                (fromRef = fromTM) && (fromRef = fromPTM))
        |> Seq.filter (fun x -> x) |> Seq.length
    (matchCount = twinMaps.Map.Count) && (matchCount = twinMaps.TrieMap_SU.Count) && (matchCount = twinMaps.PTrieMap.Count) && (getMatchCount = twinMaps.Map.Count)
    && (matchCount = (twinMaps.TrieMap_SU |> TrieMap_SU.toSeq |> Seq.length))
    && (matchCount = (twinMaps.PTrieMap |> TrieMap_PU.toSeq |> Seq.length))


[<Test>]
let ``a big bunch of distinct Adds and deletes that should result in contents stored properly``() =
    let numElements = 1000 // 5
    let entries = List.init numElements (fun n -> (Guid.NewGuid().ToString() (* n.ToString() *) , Guid.NewGuid().ToString()))
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
