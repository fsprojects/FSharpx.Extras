namespace FSharpx.TypeProviders

open System
open FSharpx.TypeProviders.JSONParser
open FSharpx.TypeProviders.DSL
open System.Collections.Generic
open FSharpx.TypeProviders.Inference

// ------------------------------------------------------------------------------------------------
// Infers the structure of JSON file from data
// ------------------------------------------------------------------------------------------------

module JSONInference = 
  let rec provideElement name multi (childs:seq<JSON>) = 
    CompoundProperty(name,multi,collectElements childs,collectProperties childs)

  and collectProperties (elements:seq<JSON>) =
    let props =
      [for el in elements do
        match el with
        | JObject map -> 
            for prop in !map do   // TODO: Don't tostring here
                match prop.Value with
                | Text t -> yield prop.Key, !t
                | Number n -> yield prop.Key, (!n).ToString()
                | Boolean b -> yield prop.Key, (!b).ToString()
                | _ -> ()              
        | _ -> ()]
    props
    |> Seq.groupBy fst
    |> Seq.map (fun (name, attrs) -> 
        SimpleProperty(
          name,
          attrs 
            |> Seq.map snd
            |> inferType,
          Seq.length attrs < Seq.length elements))

  and collectElements (elements:seq<JSON>)  =
    [ for el in elements do
        match el with
        | JObject map -> 
            for prop in !map do   // TODO: Don't tostring here                
                match prop.Value with
                | JObject child -> yield prop.Key, false, prop.Value
                | JArray childs -> 
                    for child in !childs do
                        yield prop.Key, true, child
                | _ -> ()              
        | _ -> ()]
    |> Seq.groupBy (fun (fst,_,_) -> fst)
    |> Seq.map (fun (name, values) -> 
            provideElement
                name 
                (values |> Seq.head |> (fun (_,snd,_) -> snd)) 
                (Seq.map (fun (_,_,third) -> third) values))