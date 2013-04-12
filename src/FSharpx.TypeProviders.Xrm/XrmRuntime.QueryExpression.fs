// Copyright (c) Microsoft Corporation 2005-2013.
// This sample code is provided "as is" without warranty of any kind. 
// We disclaim all warranties, either express or implied, including the 
// warranties of merchantability and fitness for a particular purpose.

namespace FSharpx.TypeProviders.XrmProvider.Runtime.QueryExpression

open System
open System.Reflection
open System.Linq.Expressions
open System.Collections.Generic
open Microsoft.Xrm.Sdk.Query
open FSharpx.TypeProviders.XrmProvider.Runtime.Common
open FSharpx.TypeProviders.XrmProvider.Runtime.Patterns

module internal QueryExpressionTransformer =    
    /// Visitor has two uses - 1. extracting the attributes for each entity to retrieve
    /// 2. transform the projection epxression into something that will work with the "one entity to rule them all" that comes back
    type private ProjectionTransformer(tupleIndex:string ResizeArray,baseEntityParam:ParameterExpression,baseEntityAlias,aliasEntityDict:Map<string,string>) =
        inherit ExpressionVisitor()
        static let getSubEntityMi = typeof<XrmEntity>.GetMethod("GetSubEntity",BindingFlags.NonPublic ||| BindingFlags.Instance)

        let mutable singleEntityName = ""

        /// holds the columns to select for each entity appearing in the projection tree, or a blank list if all columns         
        member val ProjectionMap = Dictionary<string,string ResizeArray>()

        override x.VisitLambda(exp) = 
            if exp.Parameters.Count = 1 && exp.Parameters.[0].Type = typeof<XrmEntity> then
                // this is a speical case when there were no select manys and as a result the projection parameter is just the single entity rather than a tuple
                // this still includes cases where tuples are created by the user directly, that is fine - it is for avoiding linq auto generated tuples
                singleEntityName <- exp.Parameters.[0].Name
                match x.ProjectionMap.TryGetValue singleEntityName with
                | true, values -> ()
                | false, _ -> x.ProjectionMap.Add(singleEntityName,new ResizeArray<_>())
                let body = base.Visit exp.Body
                upcast Expression.Lambda(body,baseEntityParam) 
            else base.VisitLambda exp

        override x.VisitMethodCall(exp) =
            // match here on a tupled expression with GetAttribute/GetFormattedValue or a single entity with GetAttribute/GetFormattedValue
            let(|PropName|) (pi:PropertyInfo) = pi.Name
            match exp with
            | MethodCall(Some(ParamName name | PropertyGet(_,PropName name)),(MethodWithName "GetAttribute"),[String key]) 
            | MethodCall(Some(PropertyGet(Some(ParamName name| PropertyGet(_,PropName name)),PropertyWithName "FormattedValues")), MethodWithName "get_Item", [String key])
            | MethodCall(Some(Lambda(_,MethodCall(_,MethodWithName "GetFormattedValue",[String key] ))),(MethodWithName "Invoke"),[(ParamName name | PropertyGet(_,PropName name))]) -> 
                // add this attribute to the select list for the alias
                let alias = if tupleIndex.Count = 0 then "" else Utilities.resolveTuplePropertyName name tupleIndex
                match x.ProjectionMap.TryGetValue alias with
                | true, values -> values.Add key
                | false, _ -> x.ProjectionMap.Add(alias,new ResizeArray<_>(seq{yield key}))
            | _ -> ()
            base.VisitMethodCall exp

        override __.VisitParameter(exp) = 
            // special case as above
            if singleEntityName <> "" && exp.Type = typeof<XrmEntity> && exp.Name = singleEntityName then upcast baseEntityParam
            else base.VisitParameter exp 
                       
        override x.VisitMember(exp) = 
            // convert the member expression into a function call that retrieves the child entity from the result entity
            // only interested in anonymous objects that were created by the LINQ infrastructure
            // ignore other cases 
            if exp.Type = typeof<XrmEntity> && exp.Expression.Type.FullName.StartsWith "Microsoft.FSharp.Linq.RuntimeHelpers.AnonymousObject" then             
                let (alias,name) = 
                    if baseEntityAlias = "" then ("","")
                    else
                        let alias = Utilities.resolveTuplePropertyName exp.Member.Name tupleIndex
                        match x.ProjectionMap.TryGetValue alias with
                        | true, values -> ()
                        | false, _ -> x.ProjectionMap.Add(alias,new ResizeArray<_>())
                        if alias = baseEntityAlias then ("","")
                        else (alias,aliasEntityDict.[alias])
            
                // convert this expression into a GetSubEntity call with the correct alias
                upcast
                    Expression.Convert(
                        Expression.Call(baseEntityParam,getSubEntityMi,Expression.Constant(alias),Expression.Constant(name))
                            ,exp.Type)   
                                     
            else base.VisitMember exp 
    
    let convertExpression exp (entityIndex:string ResizeArray) =        
        // first convert the abstract query tree into a more useful format
        let xrmQuery = XrmQuery.ofXrmExp(exp,entityIndex)
                
        let rec populateFilter (cond:Condition) (filter:FilterExpression)  =        
            let (conditions,nestedConditions) =
                match cond with
                | And(conditions,nestedConditions) -> 
                    filter.FilterOperator <- LogicalOperator.And
                    conditions,nestedConditions
                | Or(conditions,nestedConditions) -> 
                    filter.FilterOperator <- LogicalOperator.Or
                    conditions,nestedConditions

            conditions |> List.iter ( fun (attr,op,value) -> 
                match value with
                | Some(:? Array) as a -> filter.AddCondition <| ConditionExpression(attr,op,a.Value :?> Array) // for some reason when passing an array to this method it needs to first be unboxed ?                
                | Some(value) when value.GetType().IsEnum -> filter.AddCondition(attr,op,Convert.ToInt32(value))
                | Some(value) -> filter.AddCondition(attr,op,value)
                | None -> filter.AddCondition(attr,op)) // handles using the Null and NotNull condition operators

            match nestedConditions with
            // note: when creating a filter using .AddFilter you must pass And or Or, but it is not set to the correct operator until the next stage
            | Some(l) -> l |> List.iter(fun cond -> populateFilter cond (filter.AddFilter(LogicalOperator.Or)) |> ignore ); filter
            | None -> filter
     
        let addFilter alias addFilter =
            match xrmQuery.Filters.TryFind alias with    
            | Some(v) -> v |> Seq.iter(fun c -> populateFilter c (addFilter()) |> ignore )
            | _ -> ()
    
        // note : the baseAlias here will always be "" when no criteria has been applied, because the LINQ tree never needed to refer to it     
        let baseAlias,baseEntity =
            match xrmQuery.UltimateChild with
            | Some(baseAlias,baseEntity) -> (baseAlias,baseEntity)
            | _ -> failwith ""              

        let (projectionDelegate,projectionAttributes) = 
            let param = Expression.Parameter(typeof<XrmEntity>,"result")
            match xrmQuery.Projection with
            | Some(proj) -> let transformer = ProjectionTransformer(entityIndex,param,baseAlias,xrmQuery.Aliases)
                            let newProjection = transformer.Visit(proj) :?> LambdaExpression
                            (Expression.Lambda(newProjection.Body,param).Compile(),transformer.ProjectionMap)
            | None -> 
                // this case happens when there are only where clauses with a single entity and a projection cotaining just the entity itself. Example:
                // for x in dc.john 
                // where x.y = 1
                // select x
                // this does not create a call to .Select() after .Where(), therefore in this case we must provide our own projection that simply selects the whole entity 
                let pmap = Dictionary<string,string ResizeArray>()
                pmap.Add(baseAlias, new ResizeArray<_>())
                (Expression.Lambda(param,param).Compile(),pmap)
        
        let createColumnSet alias = 
            match projectionAttributes.TryGetValue alias with
            | true, values when values.Count > 0 -> ColumnSet(values |> Seq.toArray)
            | _, _ ->  ColumnSet(true)

        let rec addLinks alias addLink =
            let aux (linkTo,linkData) = 
                let joinType = if linkData.OuterJoin then JoinOperator.LeftOuter else JoinOperator.Inner
                if linkData.IntersectionEntity <> "" then 
                    // in the case of many to many relationships, an additonal intersection link needs to be created that everything else is unaware of
                    let link = LinkEntity(linkData.PrimaryEntity,linkData.IntersectionEntity,linkData.PrimaryEntity+"id",linkData.PrimaryKey, joinType)
                    addLink link
                    let link2 = LinkEntity(linkData.IntersectionEntity,linkData.ForeignEntity,linkData.ForeignKey,linkData.ForeignEntity+"id",joinType)
                    link2.EntityAlias <- linkTo
                    link2.Columns <- createColumnSet linkTo
                    link.LinkEntities.Add link2
                     // add any filters to this link entity
                    addFilter linkTo (fun () -> link2.LinkCriteria.AddFilter(LogicalOperator.And))
                    // recusrively process links ..
                    addLinks linkTo (fun newLink -> link2.LinkEntities.Add newLink)
                else
                    let link = match linkData.RelDirection with
                                | RelationshipDirection.Parents ->  LinkEntity(linkData.ForeignEntity,linkData.PrimaryEntity,linkData.ForeignKey,linkData.PrimaryKey,joinType)
                                | RelationshipDirection.Children -> LinkEntity(linkData.PrimaryEntity,linkData.ForeignEntity,linkData.PrimaryKey,linkData.ForeignKey,joinType)
                                | _ -> failwith ""
                    link.EntityAlias <- linkTo
                    link.Columns <- createColumnSet linkTo
                    addLink link
                    addFilter linkTo (fun () -> link.LinkCriteria.AddFilter(LogicalOperator.And))
                    addLinks linkTo (fun newLink -> link.LinkEntities.Add newLink)

            match xrmQuery.Links.TryFind alias with
            | Some(v) -> v |> Seq.iter aux
            | _ -> ()    

        let query = QueryExpression(baseEntity)
        query.ColumnSet <- createColumnSet baseAlias
        query.Distinct <- xrmQuery.Distinct
        
        xrmQuery.Ordering |> List.iter( fun (alias,key,desc) ->
            if baseAlias <> "" && alias <> baseAlias then failwithf "sorting is only supported on the ultimate child entity (in this case, %s)" baseEntity
            query.AddOrder(key,if desc then OrderType.Descending else OrderType.Ascending))
         
         // check that skip and take make sense if they have been supplied
        match xrmQuery.Skip,xrmQuery.Take with
        | Some(_), None 
        | None, Some(_) -> failwith "paging requires both Skip and Take to be used in the query expression"
        | Some(skip), Some(take) when (skip % take) <> 0 -> failwith "paging requries the take amount to be evenly divisible by the skip amount"
        | Some(skip), Some(take) -> query.PageInfo.Count <- take 
                                    query.PageInfo.PageNumber <- (skip / take)  + 1
        | _,_ -> ()

        // add base filter(s)
        addFilter baseAlias (fun () -> query.Criteria.AddFilter(LogicalOperator.And)) 

        if entityIndex.Count > 0 then             
            // recusrively add all links and their criteria
            addLinks (entityIndex.Find(fun x -> x = baseAlias)) (fun newLink -> query.LinkEntities.Add newLink)                
            
        (query,projectionDelegate)
 