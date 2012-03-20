// ----------------------------------------------------------------------------
// Original Xml type provider
// (c) Tomas Petricek - tomasP.net, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------
namespace FSharpx.TypeProviders

open System
open System.Xml.Linq
open FSharpx.TypeProviders.DSL
open System.Collections.Generic
open FSharpx.TypeProviders.Inference

// ------------------------------------------------------------------------------------------------
// Runtime objects
// ------------------------------------------------------------------------------------------------

type TypedXElement(element:XElement) =
  member x.Element = element

type TypedXDocument(document:XDocument) =
  member x.Document = document

// ------------------------------------------------------------------------------------------------
// Infers the structure of XML file from data
// ------------------------------------------------------------------------------------------------

module XmlInference = 
  let rec provideElement name (elements:seq<XElement>) = 
    CompoundProperty(name,true,collectElements elements,collectAttributes elements)

  and collectAttributes (elements:seq<XElement>) = 
    [ for el in elements do
        for attr in el.Attributes() do 
          yield attr.Name.LocalName, attr ]
    |> Seq.groupBy fst
    |> Seq.map (fun (name, attrs) -> 
        SimpleProperty(
          name,
          attrs |> Seq.map (fun (_, a) -> a.Value) |> inferType,
          Seq.length attrs < Seq.length elements))

  and collectElements (elements:seq<XElement>) =
    [ for el in elements do
        for child in el.Elements() do 
          yield child.Name.LocalName, child ]
    |> Seq.groupBy fst
    |> Seq.map (fun (n, v) -> provideElement n (Seq.map snd v))
