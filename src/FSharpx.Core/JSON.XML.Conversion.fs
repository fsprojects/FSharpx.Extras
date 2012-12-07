namespace FSharpx.JSON

open System
open System.Text
open System.Xml.Linq
open System.Globalization

[<AutoOpen>]
module Conversion =
    let fromXml(xml:XDocument) =        
        let rec createJArray (elements:XElement seq) =
            elements
            |> Seq.fold (fun (jsonArray:JsonValue)  element -> jsonArray.AddElement(createJObject element)) emptyArray
        and createJObject (element:XElement) =            
            let jObject = 
                element.Attributes()
                |> Seq.fold (fun (jsonObject:JsonValue) attr -> jsonObject.AddStringProperty(attr.Name.LocalName,attr.Value)) emptyObject

            element.Elements()
            |> Seq.groupBy (fun x -> x.Name.LocalName)
            |> Seq.fold (fun (jsonObject:JsonValue) (key,childs) ->
                    match Seq.toList childs with
                    | child::[] -> jsonObject.AddProperty(FSharpx.Strings.singularize key,createJObject child) 
                    | childs -> jsonObject.AddProperty(FSharpx.Strings.pluralize key,createJArray (List.rev childs)))
                 jObject

        createJObject xml.Root
    

    type System.Xml.Linq.XDocument with
        member this.ToJson() = fromXml this

    type JsonValue with
        member this.GetDate() =
            match this with
            | JsonValue.String text -> 
                match text with
                | FSharpx.Strings.JsonDate d -> d

        member this.ToXml() = 
            match this with
            | JsonValue.Null -> null
            | JsonValue.Obj properties -> 
                properties 
                |> Seq.map (fun (k,v) -> 
                        match v with
                        | JsonValue.String t -> new XAttribute(XName.Get k, t) :> XObject
                        | JsonValue.Bool b  -> new XAttribute(XName.Get k, b) :> XObject
                        | JsonValue.NumDecimal number-> new XAttribute(XName.Get k, number) :> XObject
                        | JsonValue.NumDouble number-> new XAttribute(XName.Get k, number) :> XObject
                        | _ -> new XElement(XName.Get k, v.ToXml()) :> XObject) 
            | JsonValue.Array elements -> elements |> Seq.map (fun item -> new XElement(XName.Get "item", item.ToXml()) :> XObject)