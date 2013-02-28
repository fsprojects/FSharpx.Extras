// Copyright (c) Microsoft Corporation 2005-2013.
// This sample code is provided "as is" without warranty of any kind. 
// We disclaim all warranties, either express or implied, including the 
// warranties of merchantability and fitness for a particular purpose.

namespace FSharpx.TypeProviders.XrmProvider.Runtime.Common

open System
open System.Collections.Generic
open System.Linq.Expressions
open Microsoft.Xrm.Sdk
open Microsoft.Xrm.Sdk.Query

type XrmEntity(schemaName) =
    inherit Microsoft.Xrm.Sdk.Entity(schemaName)
    
    let aliasCache = new Dictionary<string,XrmEntity>(HashIdentity.Structural)

    member e.SchemaName = schemaName
    
    member e.GetAttribute<'T>(key) = 
        let defaultValue() =                        
            if typeof<'T> = typeof<string> then (box String.Empty) :?> 'T
            else Unchecked.defaultof<'T>
        if e.Attributes = null then defaultValue()
        elif e.Attributes.ContainsKey key then e.GetAttributeValue(key) 
        else defaultValue()            

    member e.HasValue(key) = e.Attributes.ContainsKey key

    /// creates a new XrmEntity from alias data in this entity
    member internal e.GetSubEntity(alias:string,logicalName) =
        match aliasCache.TryGetValue alias with
        | true, entity -> entity
        | false, _ -> 
            let logicalName = if logicalName <> "" then logicalName else e.LogicalName
            let newEntity = XrmEntity(logicalName)
            let pk = sprintf "%sid" logicalName
            // attributes names cannot have a period in them unless they are an alias
            let pred = match alias with
                       | "" -> (fun (kvp:KeyValuePair<string,_>) -> if kvp.Key.Contains(".") then None else Some kvp )
                       | alias -> let prefix = alias + "."
                                  (fun (kvp:KeyValuePair<string,obj>) -> 
                                    if kvp.Key.StartsWith prefix then 
                                        // strip the prefix so the entity appears normal in its new home
                                        let newValue =
                                            match kvp.Value with
                                            | :? Microsoft.Xrm.Sdk.AliasedValue as av -> av.Value
                                            | _ -> kvp.Value
                                        Some(KeyValuePair<string,_>(kvp.Key.Replace(prefix,""),newValue)) 
                                    else None) 
                        
            newEntity.Attributes.AddRange(e.Attributes |> Seq.choose pred)        
            
            // using left joins can mean parents don't exist, in which case this will return a blank, correctly named entity (should we return null here?)
            match newEntity.Attributes |> Seq.tryFind(fun a -> a.Key = pk) with
            | Some(KeyValue(_,v)) -> newEntity.Id <- match v with
                                                     | :? Guid as g -> g
                                                     | :? EntityReference as er -> er.Id
                                                     | _ -> failwith "Unspported projection or query"
            | None -> ()
            aliasCache.Add(alias,newEntity)
            newEntity

type RelationshipDirection = Children = 0 | Parents = 1 

type LinkData =
    { PrimaryEntity      : string
      PrimaryKey         : string
      ForeignEntity      : string
      ForeignKey         : string
      IntersectionEntity : string
      OuterJoin          : bool      
      RelDirection       : RelationshipDirection }
    with 
        member x.Reverse() = 
            { x with PrimaryEntity = x.ForeignEntity; PrimaryKey = x.ForeignKey; ForeignEntity = x.PrimaryEntity; ForeignKey = x.PrimaryKey }
 
type alias = string
type entity = string 

type Condition = 
    | And of (string * ConditionOperator * obj option) list * (Condition list) option  
    | Or of (string * ConditionOperator * obj option) list * (Condition list) option   

type XrmExp =
    | BaseEntity   of alias * entity                     // name of the initiating IQueryable entity - this isn't always the ultimate child that is selected 
    | SelectMany   of alias * alias * LinkData * XrmExp  // from alias, to alias and join data including to and from entity names
    | FilterClause of alias * Condition * XrmExp         // filter, possibly more than one per entity but each filter tree instance only ever includes the one entity
    | Projection   of Expression * XrmExp                // entire linq projection expression tree
    | Distinct     of XrmExp                             // distinct indicator, you can only perform distinct on the entire query when using the QueryExpression    
    | OrderBy      of alias * string * bool * XrmExp     // alias and attribute name, bool indicates descending sort
    | Skip         of int * XrmExp
    | Take         of int * XrmExp
    
type internal XrmQuery =
    { Filters       : Map<alias, Condition list>
      Links         : Map<alias, (alias * LinkData) list>
      Aliases       : Map<string, string>
      Ordering      : (alias * string * bool) list
      Projection    : Expression option
      Distinct      : bool
      UltimateChild : (string * string) option
      Skip          : int option
      Take          : int option }
    with
        static member Blank() = { Filters = Map.empty; Links = Map.empty; Aliases = Map.empty; Ordering = [];
                                  Projection = None; Distinct = false; UltimateChild = None; Skip = None; Take = None }

        static member ofXrmExp(exp,entityIndex: string ResizeArray) =
            let add key item map =
                match Map.tryFind key map with
                | None -> map |> Map.add key [item] 
                | Some(xs) -> map.Remove key |> Map.add key (item::xs)

            let rec convert (q:XrmQuery) = function
                | BaseEntity(a,e) -> match q.UltimateChild with
                                        | Some(a,e) -> q
                                        | None when q.Links.Count > 0 && q.Links.ContainsKey(a) = false -> 
                                                // the check here relates to the special case as descibed in the FilterClause below.
                                                // need to make sure the pre-tuple alias (if applicable) is not used in the projection,
                                                // but rather the later alias of the same object after it has been tupled.
                                                  { q with UltimateChild = Some(entityIndex.[0], e) }
                                        | None -> { q with UltimateChild = Some(a,e) }
                
                | SelectMany(a,b,link,rest) as e -> 
                    match (link.RelDirection,q.UltimateChild) with
                    | RelationshipDirection.Children,None when link.IntersectionEntity <> "" -> 
                        // special case here for many to manys - because an intersect is added the aliases on the 
                        // link and ultimate child here effectively get flipped around
                        convert { q with UltimateChild = Some(a,link.PrimaryEntity);
                                         Aliases = q.Aliases.Add(b,link.ForeignEntity).Add(a,link.PrimaryEntity);
                                         Links = q.Links |> add a (b,link) } rest
                    | RelationshipDirection.Children,None -> 
                        convert { q with UltimateChild = Some(a,link.ForeignEntity);
                                         Aliases = q.Aliases.Add(a,link.ForeignEntity).Add(b,link.PrimaryEntity);
                                         Links = q.Links |> add a (b,link.Reverse()) } rest
                    | _ ->   
                        convert { q with Aliases = q.Aliases.Add(b,link.ForeignEntity).Add(a,link.PrimaryEntity);
                                         Links = q.Links |> add b (a,link) } rest

                | FilterClause(a,c,rest) as e ->    
                    // if this alias is not in the tuple index, then really it is the same as index 0.
                    // this is because in the case of a query like 
                    //   for x in dc.x
                    //   where x.number = 1
                    //   for y in x.y
                    //   where x.string = "42"
                    // the first where clause still holds "x" as the alias, and then after the select many it gets changed to "_arg2" (or similar)
                    // the second where clause then appears with new alias, so here we need to detect this case and change the "x" to the correct value 
                    if entityIndex.Count > 0 && entityIndex.TrueForAll(fun x -> x <> a) then
                         convert { q with Filters = q.Filters |> add (entityIndex.[0]) c } rest
                    else convert { q with Filters = q.Filters |> add a c } rest 
                | Projection(exp,rest) as e ->  
                    match q.Projection with
                    | Some(_) -> failwith "the type provider currently only supports a single projection"
                    | None -> convert { q with Projection = Some exp } rest
                | Distinct(rest) ->
                    match q.Distinct with
                    | true -> failwith "distinct is applied to the entire query and can only be supplied once"                
                    | _    -> convert { q with Distinct = true } rest
                | OrderBy(alias,key,desc,rest) ->
                    convert { q with Ordering = (alias,key,desc)::q.Ordering } rest
                | Skip(amount, rest) -> 
                    match q.Skip with
                    | Some(_) -> failwith "skip may only be specified once"
                    | None -> convert { q with Skip = Some(amount) } rest
                | Take(amount, rest) -> 
                    match q.Take with
                    | Some(_) -> failwith "take may only be specified once"
                    | None -> convert { q with Take = Some(amount) } rest
            convert (XrmQuery.Blank()) exp


module Utilities =
    let resolveTuplePropertyName name (tupleIndex:string ResizeArray) =
        match name with // could do this by parsing the number from the end of the string..
        | "Item1" -> tupleIndex.[0] | "Item11" -> tupleIndex.[10]
        | "Item2" -> tupleIndex.[1] | "Item12" -> tupleIndex.[11]
        | "Item3" -> tupleIndex.[2] | "Item13" -> tupleIndex.[12]
        | "Item4" -> tupleIndex.[3] | "Item14" -> tupleIndex.[13]
        | "Item5" -> tupleIndex.[4] | "Item15" -> tupleIndex.[14]
        | "Item6" -> tupleIndex.[5] | "Item16" -> tupleIndex.[15]
        | "Item7" -> tupleIndex.[6] | "Item17" -> tupleIndex.[16]
        | "Item8" -> tupleIndex.[7] | "Item18" -> tupleIndex.[17]
        | "Item9" -> tupleIndex.[8] | "Item19" -> tupleIndex.[18]
        | "Item10"-> tupleIndex.[0] | "Item20" -> tupleIndex.[19]
        | _ -> failwith "currently only support up to 20 nested entity aliases"
