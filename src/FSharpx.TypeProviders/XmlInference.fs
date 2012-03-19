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
// Representation about inferred structure
// ------------------------------------------------------------------------------------------------

type ProvidedXAttribute = 
  ProvidedXAttribute of string * string * Type * bool

type ProvidedXElement = 
  ProvidedXElement 
    of string * string * ProvidedXElement seq * 
       ProvidedXAttribute seq

// ------------------------------------------------------------------------------------------------
// Infers the structure of XML file from data
// ------------------------------------------------------------------------------------------------

module Inference = 
  let rec provideElement name (elements:seq<XElement>) = 
    ProvidedXElement
      ( name, niceName name,
        collectElements elements,
        collectAttributes elements )

  and collectAttributes (elements:seq<XElement>) = 
    [ for el in elements do
        for attr in el.Attributes() do 
          yield attr.Name.LocalName, attr ]
    |> Seq.groupBy fst
    |> Seq.map (fun (name, attrs) -> 
        let typ = attrs |> Seq.map (fun (_, a) -> a.Value) |> inferType
        ProvidedXAttribute(name, niceName name, typ, Seq.length attrs < Seq.length elements) )

  and collectElements (elements:seq<XElement>) =
    [ for el in elements do
        for child in el.Elements() do 
          yield child.Name.LocalName, child ]
    |> Seq.groupBy fst
    |> Seq.map (fun (n, v) -> provideElement n (Seq.map snd v))
