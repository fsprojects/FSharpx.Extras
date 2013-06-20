// Copyright (c) Microsoft Corporation 2005-2013.
// This sample code is provided "as is" without warranty of any kind. 
// We disclaim all warranties, either express or implied, including the 
// warranties of merchantability and fitness for a particular purpose.

namespace FSharpx.TypeProviders.XrmProvider.Runtime

open System
open System.Linq
open System.Net
open System.ServiceModel.Description
open System.Reflection
open System.Collections.Generic

open Microsoft.Xrm.Sdk
open Microsoft.Xrm.Sdk.Query
open Microsoft.Xrm.Sdk.Messages
open Microsoft.Xrm.Sdk.Metadata
open Microsoft.Xrm.Sdk.Client

open FSharpx.TypeProviders.XrmProvider.Runtime.QueryExpression                      
open FSharpx.TypeProviders.XrmProvider.Runtime.Common

module internal QueryImplementation = 
    open System.Linq
    open System.Linq.Expressions
    open System.Reflection
    open Patterns    
    
    type IWithOrgService = 
        abstract OrgService : IOrganizationService
        abstract XrmExpression : XrmExp  
        abstract TupleIndex : string ResizeArray // indexes where in the anonymous object created by the compiler during a select many that each entity alias appears
        abstract DataBindingMode : DataBindingMode
    
    let (|SourceWithQueryData|_|) = function Constant ((:? IWithOrgService as org), _)    -> Some org | _ -> None     
    let (|RelDirection|_|)        = function Constant ((:? RelationshipDirection as s),_) -> Some s   | _ -> None

    let executeQuery (org:IOrganizationService) xrmExp ti dataBindingMode =
       let (query,projector) = QueryExpressionTransformer.convertExpression xrmExp ti
       let results = org.RetrieveMultiple query
       seq { for e in results.Entities -> projector.DynamicInvoke(XrmEntity.FromEntity(e,dataBindingMode)) } :> System.Collections.IEnumerable
               
    type XrmQueryable<'T>(org:IOrganizationService,xrmQuery,tupleIndex,dataBindingMode) =       
        static member Create(entity,org,dataBindingMode) = XrmQueryable<'T>(org,BaseEntity("",entity),ResizeArray<_>(),dataBindingMode) :> IQueryable<'T> 
        interface IQueryable<'T>
        interface IQueryable with
            member x.Provider = XrmQueryProvider.Provider
            member x.Expression =  Expression.Constant(x,typeof<IQueryable<'T>>) :> Expression 
            member x.ElementType = typeof<'T>
        interface seq<'T> with 
             member x.GetEnumerator() = (Seq.cast<'T> (executeQuery org xrmQuery tupleIndex dataBindingMode)).GetEnumerator()
        interface System.Collections.IEnumerable with 
             member x.GetEnumerator() = (x :> seq<'T>).GetEnumerator() :> System.Collections.IEnumerator
        interface IWithOrgService with 
             member x.OrgService = org
             member x.XrmExpression = xrmQuery
             member x.TupleIndex = tupleIndex
             member x.DataBindingMode = dataBindingMode

    and XrmOrderedQueryable<'T>(org:IOrganizationService,xrmQuery,tupleIndex,dataBindingMode) =       
        static member Create(entity,org,dataBindingMode) = XrmQueryable<'T>(org,BaseEntity("",entity),ResizeArray<_>(),dataBindingMode) :> IQueryable<'T> 
        interface IOrderedQueryable<'T>
        interface IQueryable<'T> 
        interface IQueryable with 
            member x.Provider = XrmQueryProvider.Provider
            member x.Expression =  Expression.Constant(x,typeof<IOrderedQueryable<'T>>) :> Expression 
            member x.ElementType = typeof<'T>
        interface seq<'T> with 
             member x.GetEnumerator() = (Seq.cast<'T> (executeQuery org xrmQuery tupleIndex dataBindingMode)).GetEnumerator()
        interface System.Collections.IEnumerable with 
             member x.GetEnumerator() = (x :> seq<'T>).GetEnumerator() :> System.Collections.IEnumerator
        interface IWithOrgService with 
             member x.OrgService = org
             member x.XrmExpression = xrmQuery
             member x.TupleIndex = tupleIndex            
             member x.DataBindingMode = dataBindingMode

    and XrmQueryProvider() =
         static member val Provider = 
             { new System.Linq.IQueryProvider with 
                member provider.CreateQuery(e:Expression) : IQueryable = failwithf "CreateQuery, e = %A" e
                member provider.CreateQuery<'T>(e:Expression) : IQueryable<'T> =                     
                    match e with                    
                    | MethodCall(None, (MethodWithName "Skip" as meth), [SourceWithQueryData source; Int amount]) ->                                                
                        let ty = typedefof<XrmQueryable<_>>.MakeGenericType(meth.GetGenericArguments().[0]) 
                        ty.GetConstructors().[0].Invoke [| source.OrgService ; Skip(amount,source.XrmExpression) ; source.TupleIndex; source.DataBindingMode |] :?> IQueryable<_>                         

                    | MethodCall(None, (MethodWithName "Take" as meth), [SourceWithQueryData source; Int amount]) ->                                                
                        let ty = typedefof<XrmQueryable<_>>.MakeGenericType(meth.GetGenericArguments().[0]) 
                        ty.GetConstructors().[0].Invoke [| source.OrgService ; Take(amount,source.XrmExpression) ; source.TupleIndex; source.DataBindingMode |] :?> IQueryable<_>

                    | MethodCall(None, (MethodWithName "OrderBy" | MethodWithName "OrderByDescending" as meth), [SourceWithQueryData source; OptionalQuote (Lambda([ParamName param], XrmAttributeGet(entity,key,_))) ]) ->
                        let alias = if entity = "" then param else Utilities.resolveTuplePropertyName entity source.TupleIndex
                        let ty = typedefof<XrmOrderedQueryable<_>>.MakeGenericType(meth.GetGenericArguments().[0]) 
                        let ascending = meth.Name = "OrderBy"
                        let x = ty.GetConstructors().[0].Invoke [| source.OrgService ; OrderBy(alias,key,ascending,source.XrmExpression) ; source.TupleIndex; source.DataBindingMode |] 
                        x :?> IQueryable<_>

                    | MethodCall(None, (MethodWithName "ThenBy" | MethodWithName "ThenByDescending" as meth), [SourceWithQueryData source; OptionalQuote (Lambda([ParamName param], XrmAttributeGet(entity,key,_))) ]) ->
                        let alias = if entity = "" then param else Utilities.resolveTuplePropertyName entity source.TupleIndex
                        let ty = typedefof<XrmOrderedQueryable<_>>.MakeGenericType(meth.GetGenericArguments().[0]) 
                        let ascending = meth.Name = "ThenBy"
                        match source.XrmExpression with 
                        | OrderBy(_) ->
                            let x = ty.GetConstructors().[0].Invoke [| source.OrgService ; OrderBy(alias,key,ascending,source.XrmExpression) ; source.TupleIndex; source.DataBindingMode |] 
                            x :?> IQueryable<_>
                        | _ -> failwith (sprintf "'thenBy' operations must come immediately after a 'sortBy' operation in a query")

                    | MethodCall(None, (MethodWithName "Distinct" as meth), [ SourceWithQueryData source ]) ->
                        let ty = typedefof<XrmQueryable<_>>.MakeGenericType(meth.GetGenericArguments().[0])                            
                        ty.GetConstructors().[0].Invoke [| source.OrgService ; Distinct(source.XrmExpression) ; source.TupleIndex; source.DataBindingMode |] :?> IQueryable<_>

                    | MethodCall(None, (MethodWithName "Where" as meth), [ SourceWithQueryData source; OptionalQuote qual ]) ->
                        let (|Condition|_|) exp =   
                            // IMPORTANT : for now it is always assumed that the XRM property being checked on the server side is on the left hand side of the condition expression.
                            match exp with
                            | XrmSpecialOpArr(ti,op,key,value) -> Some(ti,(key,op,Some (box value)))                                
                            | XrmSpecialOp(ti,op,key,value) ->  Some(ti,(key,op,Some value))                                
                            // matches xrm attribute to constant with any operator eg c => c.name = "john", c => c.age > 42
                            | XrmCondOp(op,(XrmAttributeGet(ti,key,_)|XrmOptionSetGet(ti,key,_)),ConstantOrNullableConstant(c)) -> Some(ti,(key,op,c))                            
                            // if the left side is a memberexpresion it is likely referencing a property of an entity reference in a join or the Value / HasValue property of a nullable type.
                            // when accessing the value or ID property this is the same as simply checking the attribute as normal, the Value can be ignored.  HasValue should be translated into
                            // Null / Not null 
                            | PropertyGet(Some(XrmAttributeGet(ti,key,_)|XrmOptionSetGet(ti,key,_)),pi) when pi.Name = "HasValue" -> 
                                Some(ti,(key,ConditionOperator.NotNull,None))                            
                            | XrmCondOp(op,PropertyGet(Some(XrmAttributeGet(ti,key,_)|XrmOptionSetGet(ti,key,_)),pi),ConstantOrNullableConstant(c)) when pi.Name = "Value" -> 
                                // this is a special case for nullable enums, which you cannot use the nullable operators with due to restrictions in the type provider
                                // mechanics meaning there is no way to trick the compiler into treating the provided enum as a value type.
                                // this is a workaround allowing the nullable enums to be used in the format where  a.code.Value = Enum.Item
                                Some(ti,(key,op,c))
                            | XrmCondOp(op,PropertyGet(Some(XrmAttributeGet(ti,key,_)|XrmOptionSetGet(ti,key,_)),pi),Bool(c)) when pi.Name = "HasValue"  -> 
                                match c with
                                | true -> Some(ti,(key,ConditionOperator.NotNull,None))
                                | false -> Some(ti,(key,ConditionOperator.Null,None))
                            | XrmCondOp(op,(:? MemberExpression as methL),ConstantOrNullableConstant(c)) -> 
                                let ti,key =  
                                  match methL.Expression with
                                  | XrmAttributeGet(ti,key,ty) -> ti,key
                                  | _ -> failwith "unsuppored member expression on left side"    
                                Some(ti,(key,op,c))                        
                            // matches to another property getter, method call or new expression
                            | XrmCondOp(op,XrmAttributeGet(ti,key,_),(((:? MemberExpression) | (:? MethodCallExpression) | (:? NewExpression)) as meth)) ->                                 
                                Some(ti,(key,op,Some(Expression.Lambda(meth).Compile().DynamicInvoke())))
                            | _ -> None
        
                        let paramNames = HashSet<string>()
                        let rec filterExpression (exp:Expression)  =
                            let extendFilter conditions nextFilter = 
                                match exp with
                                | AndAlso(_) -> And(conditions,nextFilter)
                                | OrElse(_) -> Or(conditions,nextFilter)
                                | _ -> failwith ""                                
                            match exp with                            
                            | AndAlsoOrElse(AndAlsoOrElse(_,_) as left, (AndAlsoOrElse(_,_) as right)) as outer ->                                                                
                                extendFilter [] (Some ([filterExpression left; filterExpression right]))
                            | AndAlsoOrElse(AndAlsoOrElse(_,_) as left,Condition(ti,c) as cond) as outer ->
                                paramNames.Add ti |> ignore
                                extendFilter [c] (Some ([filterExpression left]))
                            | AndAlsoOrElse(Condition(ti1,c1) ,Condition(ti2,c2)) as outer ->    
                                paramNames.Add ti1 |> ignore
                                paramNames.Add ti2 |> ignore
                                extendFilter [c1;c2] None                                                                                                   
                            | Condition(ti,cond) -> 
                                paramNames.Add ti |> ignore
                                Condition.And([cond],None)
                            | _ -> failwith "Unsupported expression. Ensure all server-side objects appear on the left hand side of predicates."

                        match qual with
                        | Lambda([name],ex) -> 
                            // name here will either be the alias the user entered in the where clause if no joining / select many has happened before this
                            // otherwise, it will be the compiler-generated alias eg _arg2.  this might be the first method called in which case set the 
                            // base entity alias to this name. 
                            let filter = filterExpression ex
                            let xrmExpression = 
                                match source.XrmExpression with
                                | BaseEntity(alias,entity) when alias = "" -> 
                                    // special case here as above - this is the first call so replace the top of the tree here with the current base entity alias and the filter                                    
                                    FilterClause(name.Name,filter,BaseEntity(name.Name,entity))
                                | current -> 
                                    if paramNames.Count <> 1 then failwith "encountered more than one entity alias in single where expression"
                                    // the following case can happen with multiple where clauses when only a single entity is selected
                                    elif paramNames.First() = "" || source.TupleIndex.Count = 0 then FilterClause(name.Name,filter,current) 
                                    else FilterClause(Utilities.resolveTuplePropertyName (paramNames.First()) source.TupleIndex,filter,current)

                            let ty = typedefof<XrmQueryable<_>>.MakeGenericType(meth.GetGenericArguments().[0])                            
                            ty.GetConstructors().[0].Invoke [| source.OrgService ; xrmExpression; source.TupleIndex; source.DataBindingMode |] :?> IQueryable<_>
                        | _ -> failwith "only support lambdas in a where"

                    | MethodCall(None, (MethodWithName "Join" as meth), 
                                    [ SourceWithQueryData source;
                                      SourceWithQueryData dest
                                      OptionalQuote (Lambda([ParamName sourceAlias],PropertyGet(Some(XrmAttributeGet(sourceTi,sourceKey,_)),_))) 
                                      OptionalQuote (Lambda([ParamName destAlias],XrmAttributeGet(destTi,destKey,_))) 
                                      OptionalQuote projection ]) ->
                        let (BaseEntity(_,destEntity)) = dest.XrmExpression
                        let xrmExpression = 
                            match source.XrmExpression with
                            | BaseEntity(alias,entity) when alias = "" -> 
                                // special case here as above - this is the first call so replace the top of the tree here with the current base entity alias and the select many                                                                                                                                        
                                let data = { PrimaryKey = destKey; PrimaryEntity = destEntity; ForeignKey = sourceKey; ForeignEntity = entity; OuterJoin = false; RelDirection = RelationshipDirection.Parents; IntersectionEntity = "" }                                                                
                                if source.TupleIndex.Any(fun v -> v = sourceAlias) |> not then source.TupleIndex.Add(sourceAlias) 
                                if source.TupleIndex.Any(fun v -> v = destAlias) |> not then source.TupleIndex.Add(destAlias) 
                                SelectMany(destAlias,sourceAlias, data,BaseEntity(sourceAlias,entity))
                            | current -> 
                                let sourceAlias = if sourceTi <> "" then Utilities.resolveTuplePropertyName sourceTi source.TupleIndex else sourceAlias
                                if source.TupleIndex.Any(fun v -> v = sourceAlias) |> not then source.TupleIndex.Add(sourceAlias) 
                                if source.TupleIndex.Any(fun v -> v = destAlias) |> not then source.TupleIndex.Add(destAlias) 
                                // we don't actually have the "foregin" entity name here in a join as that information is "lost" further up the expression stack.
                                // however, it doesn't matter as that entity is always the parent in this syntax, and therefore the QueryExpression link entity is sitting on that parent entity. 
                                // as such specifying the entity name in the LinkEntity is actually optional so this still works ok without having to resolve it later..
                                let data = { PrimaryKey = destKey; PrimaryEntity = destEntity; ForeignKey = sourceKey; ForeignEntity = ""; OuterJoin = false; RelDirection = RelationshipDirection.Parents; IntersectionEntity = "" }                                
                                SelectMany(destAlias,sourceAlias,data,source.XrmExpression)  

                        let ty =
                            match projection with
                                | :? LambdaExpression as meth -> typedefof<XrmQueryable<_>>.MakeGenericType(meth.ReturnType)
                                | _ -> failwith "unsupported projection in join"
                        ty.GetConstructors().[0].Invoke [| source.OrgService ; xrmExpression; source.TupleIndex; source.DataBindingMode |] :?> IQueryable<_>
                    
                    | MethodCall(None, (MethodWithName "SelectMany" as meth),                     
                                    [ SourceWithQueryData source; 
                                      OptionalQuote (Lambda([ v1 ], inner )); 
                                      OptionalQuote (Lambda(projectionParams,_) as projection)  ]) ->
                        let ty =
                            match projection with
                                | :? LambdaExpression as meth -> typedefof<XrmQueryable<_>>.MakeGenericType(meth.ReturnType)
                                | _ -> failwith "unsupported projection in select many"      

                        // multiple SelectMany calls in sequence are represented in the same expression tree which must be parsed recursrively                      
                        let rec processSelectManys fromAlias inExp outExp =
                             let (|OptionalOuterJoin|) e =
                                match e with
                                | MethodCall(None, (!!), [inner]) -> (true,inner)
                                | _ -> (false,e)
                             match inExp with
                             | MethodCall(None, (MethodWithName "SelectMany"), [ createRelated ; OptionalQuote (Lambda([v1], inner)); OptionalQuote (Lambda(projectionParams,_)) ]) ->
                                let ex = processSelectManys projectionParams.[0].Name createRelated outExp                                
                                processSelectManys projectionParams.[1].Name inner ex
                             | OptionalOuterJoin(outerJoin,MethodCall(None,(MethodWithName "_CreateRelated"), [param; _; String PE; String PK; String FE; String FK; String IE; RelDirection dir; _ ])) ->                   
                                let toAlias =
                                    match param with
                                    | ParamName x -> x
                                    | PropertyGet(_,p) -> Utilities.resolveTuplePropertyName p.Name source.TupleIndex
                                    | _ -> failwith "unsupported parameter expression in CreatedRelated method call"
                                let data = { PrimaryKey = PK; PrimaryEntity = PE; ForeignKey = FK; ForeignEntity = FE; OuterJoin = outerJoin; RelDirection = dir; IntersectionEntity = IE }
                                let xrmExpression = 
                                    match outExp with
                                    | BaseEntity(alias,entity) when alias = "" -> 
                                        // special case here as above - this is the first call so replace the top of the tree here with the current base entity alias and the select many                                        
                                        SelectMany(fromAlias,toAlias,data,BaseEntity(toAlias,entity))                                            
                                    | current -> 
                                        SelectMany(fromAlias,toAlias,data,outExp)  
                                // add new aliases to the tuple index 
                                if source.TupleIndex.Any(fun v -> v = toAlias) |> not then  source.TupleIndex.Add(toAlias)
                                if source.TupleIndex.Any(fun v -> v = fromAlias) |> not then source.TupleIndex.Add(fromAlias)                                
                                xrmExpression
                             | _ -> failwith ""

                        let ex = processSelectManys projectionParams.[1].Name inner source.XrmExpression 
                        ty.GetConstructors().[0].Invoke [| source.OrgService; ex; source.TupleIndex; source.DataBindingMode|] :?> IQueryable<_>                        
                        
                    | MethodCall(None, (MethodWithName "Select" as meth), [ SourceWithQueryData source; OptionalQuote (Lambda([ v1 ], e) as lambda) ]) ->                        
                        let ty = typedefof<XrmQueryable<_>>.MakeGenericType((lambda :?> LambdaExpression).ReturnType )
                        if v1.Name.StartsWith "_arg" then
                            // this is the projection from a join - ignore 
                            ty.GetConstructors().[0].Invoke [| source.OrgService ; source.XrmExpression; source.TupleIndex; source.DataBindingMode|] :?> IQueryable<_>
                        else
                            ty.GetConstructors().[0].Invoke [| source.OrgService ; Projection(lambda,source.XrmExpression); source.TupleIndex; source.DataBindingMode|] :?> IQueryable<_>
                    | _ -> failwith "unrecongnised method call"
                    
                member provider.Execute(e:Expression) : obj = failwith "Execute not implemented"
                member provider.Execute<'T>(e:Expression) : 'T = 
                    let (|XrmQueryableParam|_|) = function Constant ((:? XrmQueryable<'T>  as query), _) -> Some query | _ -> None                     
                    match e with
                    | MethodCall(o, (MethodWithName "Single" as meth), [XrmQueryableParam(query)] ) ->   
                        match query |> Seq.toList with
                        | x::[] -> x
                        | _ -> raise <| InvalidOperationException("Encountered more than one element in the input sequence")                  
                    | _ -> failwith "Unuspported execution expression" }                    

type public XrmDataContext (orgService,user,password:string,domain,crmOnline,orgInstance:IOrganizationService) =   
    static let mutable  org : IOrganizationService = null 
    do
        if orgInstance = null then 
            let creds =
                let creds = new ClientCredentials()
                match crmOnline,user with
                | true, _ -> 
                    creds.Windows.ClientCredential <- CredentialCache.DefaultNetworkCredentials
                    creds.UserName.UserName <- user
                    creds.UserName.Password <- password
                | false, null | false, "" -> 
                    creds.Windows.ClientCredential <- CredentialCache.DefaultNetworkCredentials
                | x ->  
                    creds.Windows.ClientCredential <- new NetworkCredential(user,password,domain)
                    creds.UserName.UserName <- if (String.IsNullOrEmpty(domain)) then user else sprintf "%s\\%s" domain user
                    creds.UserName.Password <- password
                creds
            let uri = Uri(orgService)            
            let orgProxy = new OrganizationServiceProxy(uri, null, creds, (if crmOnline then Microsoft.Crm.Services.Utility.DeviceIdManager.LoadOrRegisterDevice() else null))
            org <- (orgProxy :> IOrganizationService)
        else
            org <-  orgInstance
    static member _CreateWithInstance(orgInstance) =    
        XrmDataContext(null,null,null,null,false,orgInstance)
    static member _Create(orgService,user,password,domain,crmOnline) =        
        XrmDataContext(orgService,user,password,domain,crmOnline,null)    
    static member _CreateRelated(inst:XrmEntity,entity,pe,pk,fe,fk,ie,direction,dbMode) : IQueryable<XrmEntity> =
        if direction = RelationshipDirection.Children then
            QueryImplementation.XrmQueryable<_>(org,FilterClause("__base__",Condition.And([fk,ConditionOperator.Equal, Some(box<|inst.Id.ToString())],None), BaseEntity("__base__",fe)),ResizeArray<_>(),dbMode) :> IQueryable<_> 
        else
            if inst.Contains fk |> not then raise (Exception("Cannot traverse to the parent as the foreign key attribute on this entity does not exist."))
            QueryImplementation.XrmQueryable<_>(org,FilterClause("__base__",Condition.And([pk,ConditionOperator.Equal, Some(box<|((inst.GetAttribute<EntityReference> fk).Id.ToString()))],None), BaseEntity("__base__",pe)),ResizeArray<_>(),dbMode) :> IQueryable<_> 
    static member _CreateEntities(entity,dataBindingMode) : IQueryable<XrmEntity> =  
        QueryImplementation.XrmQueryable.Create(entity,org,dataBindingMode) 
    static member _GetBlank(entity) : XrmEntity = XrmEntity(entity)
    static member _GetIndividual(entity,id,dataBindingMode) : XrmEntity =
        let e = org.Retrieve(entity,Guid.Parse(id),ColumnSet(true))
        XrmEntity.FromEntity(e,dataBindingMode)
    member x.OrganizationService with get() = org
        
