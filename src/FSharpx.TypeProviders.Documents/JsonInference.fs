namespace FSharpx.TypeProviders

open System
open FSharpx.JSON
open FSharpx.Strings
open System.Collections.Generic
open FSharpx.TypeProviders.Inference

// ------------------------------------------------------------------------------------------------
// Infers the structure of JSON file from data
// ------------------------------------------------------------------------------------------------

module internal JSONInference = 
    let int32Min = decimal Int32.MinValue
    let int32Max = decimal Int32.MaxValue
    let int64Min = decimal Int64.MinValue
    let int64Max = decimal Int64.MaxValue
    
    let rec provideElement name multi (childs:seq<JsonValue>) = 
        CompoundProperty(name,multi,collectElements childs,collectProperties childs)

    and collectProperties (elements:seq<JsonValue>) =
        let props =
          [for el in elements do
            match el with
            | JsonValue.Obj properties -> 
                for (k,v) in properties do
                    match v with
                    | JsonValue.String text -> 
                            match text with
                            | JsonDate d -> yield k,typeof<System.DateTime>
                            | JsonString s -> yield k,typeof<string>
                    | JsonValue.NumDecimal n -> 
                            let t =
                                if (n <= int32Max) && (n >= int32Min) && (n = decimal (int n)) then typeof<int> else
                                if (n <= int64Max) && (n >= int64Min) && (n = decimal (int64 n)) then typeof<int64> else
                                typeof<decimal>
                            yield k, t
                    | JsonValue.NumDouble n -> yield k, typeof<float>
                    | JsonValue.Bool _ -> yield k, typeof<bool>
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

    and collectElements (elements:seq<JsonValue>)  =
        [ for el in elements do
            match el with
            | JsonValue.Obj properties -> 
                for (k,v) in properties do            
                    match v with
                    | JsonValue.Obj _ -> yield k, false, v
                    | JsonValue.Array a -> 
                        for child in a do
                            yield k, true, child
                    | _ -> ()              
            | _ -> ()]
        |> Seq.groupBy (fun (fst,_,_) -> fst)
        |> Seq.map (fun (name, values) -> 
                provideElement
                    name 
                    (values |> Seq.head |> (fun (_,snd,_) -> snd)) 
                    (Seq.map (fun (_,_,third) -> third) values))