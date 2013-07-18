// Copyright (c) Microsoft Corporation 2005-2013.
// This sample code is provided "as is" without warranty of any kind. 
// We disclaim all warranties, either express or implied, including the 
// warranties of merchantability and fitness for a particular purpose.

namespace FSharpx.TypeProviders.XrmProvider.Runtime.Common

open System
open System.Collections.Generic
open System.ComponentModel
open System.Linq.Expressions
open Microsoft.Xrm.Sdk
open Microsoft.Xrm.Sdk.Query

/// Determines how attribute data is extracted when using data binding
type DataBindingMode =
    /// Attributes will appear with their raw types and values
    | NormalValues = 0
    /// Where possible, the formatted version of the attributes will be used. This means option sets and boolean options will be returned as strings, along with locallized datetimes, currency with correct symbols and so on.
    | FormattedValues = 1

[<System.Runtime.Serialization.DataContractAttribute(Name = "Entity", Namespace = "http://schemas.microsoft.com/xrm/2011/Contracts");System.Reflection.DefaultMemberAttribute("Item")>]
type XrmEntity(schemaName,dataBindingMode:DataBindingMode) =
    inherit Microsoft.Xrm.Sdk.Entity(schemaName)
    
    let aliasCache = new Dictionary<string,XrmEntity>(HashIdentity.Structural)

    let propertyChanged = Event<_,_>()

    member internal e.TriggerPropertyChange(name) = 
        propertyChanged.Trigger(e,PropertyChangedEventArgs(name))

    member e.SchemaName = schemaName
    
    member e.GetEnumValue(key) = 
        let os = e.GetAttributeValue<OptionSetValue>(key)
        os.Value     

    member e.GetAttribute<'T>(key) = 
        let defaultValue() =                        
            if typeof<'T> = typeof<string> then (box String.Empty) :?> 'T
            else Unchecked.defaultof<'T>
        if e.Attributes = null then defaultValue()
        elif e.Attributes.ContainsKey key then 
            e.GetAttributeValue(key) 
        else defaultValue()            

    member e.SetAttribute(key,value) =
        if not (e.Contains key) then e.Attributes.Add(KeyValuePair(key,value))
        else e.Attributes.[key] <- value
        e.TriggerPropertyChange key

    member e.GetFormattedValue(key) =
        match e.FormattedValues.TryGetValue key with
        | true, value -> value
        | _ -> String.Empty

    member e.HasValue(key) = e.Attributes.ContainsKey key
    
    static member internal FromEntity(e:Entity,dataBindingMode) =
        let entity = XrmEntity(e.LogicalName,dataBindingMode)        
        entity.Attributes <- e.Attributes
        entity.FormattedValues.AddRange(e.FormattedValues) // TODO: look at ways of avoiding this copy ...
        entity.Id <- e.Id  
        entity   

    /// creates a new XrmEntity from alias data in this entity
    member internal e.GetSubEntity(alias:string,logicalName) =
        match aliasCache.TryGetValue alias with
        | true, entity -> entity
        | false, _ -> 
            let logicalName = if logicalName <> "" then logicalName else e.LogicalName
            let newEntity = XrmEntity(logicalName,dataBindingMode)
            let pk = sprintf "%sid" logicalName
            // attributes names cannot have a period in them unless they are an alias
            let pred mutator = 
                match alias with
                | "" -> (fun (kvp:KeyValuePair<string,_>) -> if kvp.Key.Contains(".") then None else Some kvp )
                | alias -> let prefix = alias + "."
                           (fun (kvp:KeyValuePair<string,_>) -> 
                           if kvp.Key.StartsWith prefix then Some(KeyValuePair<string,_>(kvp.Key.Replace(prefix,""),mutator kvp.Value))
                           else None) 
                        
            newEntity.Attributes.AddRange(e.Attributes |> Seq.choose (pred (function :? Microsoft.Xrm.Sdk.AliasedValue as av -> av.Value | v -> v)))
            newEntity.FormattedValues.AddRange(e.FormattedValues |> Seq.choose (pred id)) 

            // using left joins can mean parents don't exist, in which case this will return a blank, correctly named entity (should we return null here?)
            match newEntity.Attributes |> Seq.tryFind(fun a -> a.Key = pk) with
            | Some(KeyValue(_,v)) -> newEntity.Id <- match v with
                                                     | :? Guid as g -> g
                                                     | :? EntityReference as er -> er.Id
                                                     | _ -> failwith "Unspported projection or query"
            | None -> ()
            aliasCache.Add(alias,newEntity)
            newEntity
    
    interface System.ComponentModel.INotifyPropertyChanged with
        [<CLIEvent>] member x.PropertyChanged = propertyChanged.Publish

    interface System.ComponentModel.ICustomTypeDescriptor with
        member e.GetComponentName() = TypeDescriptor.GetComponentName(e,true)
        member e.GetDefaultEvent() = TypeDescriptor.GetDefaultEvent(e,true)
        member e.GetClassName() = e.SchemaName
        member e.GetEvents(attributes) = TypeDescriptor.GetEvents(e,true)
        member e.GetEvents() = TypeDescriptor.GetEvents(e,null,true)
        member e.GetConverter() = TypeDescriptor.GetConverter(e,true)
        member e.GetPropertyOwner(_) = upcast e.Attributes 
        member e.GetAttributes() = TypeDescriptor.GetAttributes(e,true)
        member e.GetEditor(typeBase) = TypeDescriptor.GetEditor(e,typeBase,true)
        member e.GetDefaultProperty() = null
        member e.GetProperties()  = (e :> ICustomTypeDescriptor).GetProperties(null)
        member e.GetProperties(attributes) = 
            PropertyDescriptorCollection(e.Attributes.Keys 
                                         |> Seq.map(fun key -> AttributeCollectionPropertyDescriptor(e.Attributes.[key].GetType(),key,dataBindingMode)) 
                                         |> Seq.cast<PropertyDescriptor> |> Seq.toArray )

and internal AttributeCollectionPropertyDescriptor(attrType,attrKey,dataBindingMode) =
    inherit PropertyDescriptor(attrKey,null)
    override __.PropertyType with get() = attrType
    override __.SetValue(e,value) = (e :?> XrmEntity).SetAttribute(attrKey,value)        
    override __.GetValue(e) = 
        let e = e:?>XrmEntity
        match dataBindingMode with
        | DataBindingMode.FormattedValues ->
            match (attrType = typeof<bool>,e.FormattedValues.TryGetValue attrKey) with
            | false,(true,value) -> upcast value
            | _ -> match box(e.GetAttribute(attrKey)) with
                   | :? EntityReference as er -> upcast sprintf "%s : %s" er.LogicalName (er.Id.ToString())
                   | v -> v                   
        | _ -> e.GetAttribute(attrKey)
    override __.IsReadOnly with get() = false
    override __.ComponentType with get () = null
    override __.CanResetValue(_) = false
    override __.ResetValue(_) = ()
    override __.ShouldSerializeValue(_) = false
         
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
      ApproxCount   : bool
      Distinct      : bool
      UltimateChild : (string * string) option
      Skip          : int option
      Take          : int option }
    with
        static member Empty = { Filters = Map.empty; Links = Map.empty; Aliases = Map.empty; Ordering = [];
                                Projection = None; Distinct = false; ApproxCount=false; UltimateChild = None; Skip = None; Take = None }

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
                    if q.Projection.IsSome then failwith "the type provider only supports a single projection"
                    else convert { q with Projection = Some exp } rest
                | Distinct(rest) ->
                    if q.Distinct then failwith "distinct is applied to the entire query and can only be supplied once"                
                    else convert { q with Distinct = true } rest
                | OrderBy(alias,key,desc,rest) ->
                    convert { q with Ordering = (alias,key,desc)::q.Ordering } rest
                | Skip(amount, rest) -> 
                    if q.Skip.IsSome then failwith "skip may only be specified once"
                    else convert { q with Skip = Some(amount) } rest
                | Take(amount, rest) -> 
                    if q.Take.IsSome then failwith "take may only be specified once"
                    else convert { q with Take = Some(amount) } rest
            convert (XrmQuery.Empty) exp


module Utilities =
    let resolveTuplePropertyName name (tupleIndex:string ResizeArray) =
        match name with // could do this by parsing the number from the end of the string...
        | "Item1" -> tupleIndex.[0] | "Item11" -> tupleIndex.[10]
        | "Item2" -> tupleIndex.[1] | "Item12" -> tupleIndex.[11]
        | "Item3" -> tupleIndex.[2] | "Item13" -> tupleIndex.[12]
        | "Item4" -> tupleIndex.[3] | "Item14" -> tupleIndex.[13]
        | "Item5" -> tupleIndex.[4] | "Item15" -> tupleIndex.[14]
        | "Item6" -> tupleIndex.[5] | "Item16" -> tupleIndex.[15]
        | "Item7" -> tupleIndex.[6] | "Item17" -> tupleIndex.[16]
        | "Item8" -> tupleIndex.[7] | "Item18" -> tupleIndex.[17]
        | "Item9" -> tupleIndex.[8] | "Item19" -> tupleIndex.[18]
        | "Item10"-> tupleIndex.[9] | "Item20" -> tupleIndex.[19]
        | _ -> failwith "currently only support up to 20 nested entity aliases"
