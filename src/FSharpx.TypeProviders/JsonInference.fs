namespace FSharpx.TypeProviders

open System
open FSharpx.JSON
open FSharpx.TypeProviders.DSL
open System.Collections.Generic
open FSharpx.TypeProviders.Inference

// ------------------------------------------------------------------------------------------------
// Infers the structure of JSON file from data
// ------------------------------------------------------------------------------------------------

module JSONInference = 
  let rec provideElement name multi (childs:seq<Document>) = 
    CompoundProperty(name,multi,collectElements childs,collectProperties childs)

  and collectProperties (elements:seq<Document>) =
    let props =
      [for el in elements do
        match el with
        | (:? JObject as jObject) -> 
            for prop in jObject.Properties do
                match prop.Value with
                | :? Text -> yield prop.Key, typeof<string>
                | (:? Number as n) -> yield prop.Key, if n.Value = Math.Round n.Value then typeof<int> else typeof<float>
                | :? Boolean -> yield prop.Key, typeof<bool>
                | _ -> ()              
        | _ -> ()]
    props
    |> Seq.groupBy fst
    |> Seq.map (fun (name, attrs) -> 
        SimpleProperty(
          name,
          attrs 
            |> Seq.map snd
            |> Seq.head,
          Seq.length attrs < Seq.length elements))

  and collectElements (elements:seq<Document>)  =
    [ for el in elements do
        match el with
        | (:? JObject as jObject) -> 
            for prop in jObject.Properties do            
                match prop.Value with
                | :? JObject -> yield prop.Key, false, prop.Value
                | (:? JArray as jArray) -> 
                    for child in jArray.Elements do
                        yield prop.Key, true, child
                | _ -> ()              
        | _ -> ()]
    |> Seq.groupBy (fun (fst,_,_) -> fst)
    |> Seq.map (fun (name, values) -> 
            provideElement
                name 
                (values |> Seq.head |> (fun (_,snd,_) -> snd)) 
                (Seq.map (fun (_,_,third) -> third) values))