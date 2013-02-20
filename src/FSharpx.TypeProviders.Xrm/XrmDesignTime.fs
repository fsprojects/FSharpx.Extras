// Copyright (c) Microsoft Corporation 2005-2013.
// This sample code is provided "as is" without warranty of any kind. 
// We disclaim all warranties, either express or implied, including the 
// warranties of merchantability and fitness for a particular purpose.

namespace Samples.XrmProvider

open Samples.XrmProvider.Runtime
open Samples.XrmProvider.Runtime.Common

open System
open System.Net
open System.Reflection
open System.Collections.Generic
open System.ServiceModel.Description
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

open Microsoft.Xrm.Sdk
open Microsoft.Xrm.Sdk.Query
open Microsoft.Xrm.Sdk.Messages
open Microsoft.Xrm.Sdk.Metadata
open Microsoft.Xrm.Sdk.Client

open Samples.FSharp.ProvidedTypes

/// Determines how relationship names appear on generated types
type RelationshipNamingType =
    /// Relationships will be named with their schema name prefixed by 'Children of' or 'Parent of' and suffixed with the returned entity type name.
    | ParentChildPrefix = 0
    /// Relationships will be named with their schema name prefixed by 1:N, N:1 or N:N.
    | CrmStylePrefix = 1
    /// Relationships will be named only with their schema name.  You will need to examine the intelliense comments to determine which direction the relationships point.                         
    | SchemaNameOnly = 2

type private RelationshipType =
    | OneToMany
    | ManyToOne
    | ManyToMany

type internal XrmRuntimeInfo (config : TypeProviderConfig) =
    let runtimeAssembly = Assembly.LoadFrom(config.RuntimeAssembly)    
    member this.RuntimeAssembly = runtimeAssembly
    
[<TypeProvider>]
type XrmTypeProvider(config: TypeProviderConfig) as this = 
    inherit TypeProviderForNamespaces()
    let xrmRuntimeInfo = XrmRuntimeInfo(config)
    let ns = "Samples.XrmProvider"     
    let asm = Assembly.GetExecutingAssembly()
    
    let createOrgService uri creds =
        let uri = Uri(uri)        
        let orgProxy = new OrganizationServiceProxy(uri, null, creds, null);
        orgProxy :> IOrganizationService
    
    let extractDescription (input:Label) =
        if input <> null && input.LocalizedLabels.Count > 0 then                                 
            match 
                input.LocalizedLabels 
                |> Seq.cast<LocalizedLabel> 
                |> Seq.tryPick(fun l -> if l.Label <> "" then Some(l.Label) else None) with
            | Some(s) -> s
            | None -> "No description available"
        else "No description available" 
        
    let createTypes(orgService,user,pwd:string,domain,nullables,relationshipNameType,rootTypeName) =       
        let creds =
            let creds = new ClientCredentials()
            match user with
            | null | "" -> creds.Windows.ClientCredential <- CredentialCache.DefaultNetworkCredentials
            | x ->  creds.Windows.ClientCredential <- new NetworkCredential(user,pwd,domain)
            creds

        let org = createOrgService orgService creds
        
        let createRelationshipName (meta:RelationshipMetadataBase) relationshipType = 
            match relationshipNameType with
            | RelationshipNamingType.ParentChildPrefix -> 
                match meta, relationshipType with
                | :? OneToManyRelationshipMetadata as meta, OneToMany ->                    
                    sprintf "Children of %s (%s)" meta.SchemaName meta.ReferencingEntity
                | :? OneToManyRelationshipMetadata as meta, ManyToOne ->
                    sprintf "Parent of %s (%s)" meta.SchemaName meta.ReferencedEntity
                | :? ManyToManyRelationshipMetadata, ManyToMany ->
                    // todo: work out what side we are on and display the other
                    ("Many of %s" + meta.SchemaName)
                | _ -> failwith "invalid relationship type combination"
            | RelationshipNamingType.CrmStylePrefix -> 
                match meta, relationshipType with
                | :? OneToManyRelationshipMetadata, OneToMany ->
                    ("1:N " + meta.SchemaName)  
                | :? OneToManyRelationshipMetadata, ManyToOne ->
                    ("N:1 " + meta.SchemaName)
                | :? ManyToManyRelationshipMetadata, ManyToMany ->
                    ("N:N " + meta.SchemaName)
                | _ -> failwith "invalid relationship type combination"
            | RelationshipNamingType.SchemaNameOnly -> 
                meta.SchemaName
            | _ -> failwith "" // matching on enum

        let entities =   
            lazy         
                let ems = org.Execute(RetrieveAllEntitiesRequest()) :?> RetrieveAllEntitiesResponse
                [for em in ems.EntityMetadata -> em]
        
        let entityAttributes =
            lazy
                dict
                  [for e in entities.Force() do
                    yield( e.LogicalName, 
                        lazy
                            let emeta = org.Execute(RetrieveEntityRequest(LogicalName=e.LogicalName,EntityFilters=(EntityFilters.Attributes ||| EntityFilters.Relationships))) :?> RetrieveEntityResponse
                            let attr = match emeta.EntityMetadata.Attributes with null -> [] | xs -> Array.toList xs
                            let oneToMany =  match emeta.EntityMetadata.OneToManyRelationships with null -> [] | xs -> Array.toList xs
                            let manyToOne =  match emeta.EntityMetadata.ManyToOneRelationships with null -> [] | xs -> Array.toList xs
                            let manyToMany =  match emeta.EntityMetadata.ManyToManyRelationships with null -> [] | xs -> Array.toList xs
                            (attr,oneToMany,manyToOne,manyToMany))]
         
        let getEntityAttibtues logicalName = entityAttributes.Force().[logicalName].Force()
        let serviceType = ProvidedTypeDefinition( "XrmService", Some typeof<XrmDataContext>, HideObjectMethods = true)

        // first create all the types so we are able to recursively reference them in each other's definitions
        let baseTypes =
            lazy
                dict [ for entity in entities.Force() do  
                        let t = ProvidedTypeDefinition(entity.LogicalName, Some typeof<XrmEntity>, HideObjectMethods = true)
                        t.AddMember <| ProvidedConstructor([], InvokeCode = fun _ ->  <@@ new XrmEntity(entity.LogicalName)  @@>) 
                        let desc = extractDescription entity.Description
                        t.AddXmlDoc desc
                        yield entity.LogicalName,(t,desc) ]

        // add the attributes and relationships
        for KeyValue(key,(t,desc)) in baseTypes.Force() do 
            t.AddMembersDelayed(fun () -> 
                let makeNullable (a:AttributeMetadata) ty = 
                    if nullables && a.RequiredLevel.Value = AttributeRequiredLevel.None then typedefof<Nullable<_>>.MakeGenericType([|ty|])
                    else ty

                let (attr,oneToMany,manyToOne,manyToMany) = getEntityAttibtues key
                let attProps = 
                    [for a in attr do
                        let name = a.LogicalName  
                        let ty =
                            match a.AttributeType.HasValue with
                            | false -> typeof<string>
                            | true -> match a.AttributeType.Value  with
                                        // TODO: This list is not yet exhaustive and doesn't cover all special XRM types
                                        | AttributeTypeCode.BigInt           -> makeNullable a typeof<int64>
                                        | AttributeTypeCode.Boolean          -> makeNullable a typeof<bool>
                                        | AttributeTypeCode.DateTime         -> makeNullable a typeof<DateTime>
                                        | AttributeTypeCode.Decimal          -> makeNullable a typeof<Decimal>
                                        | AttributeTypeCode.Double           -> makeNullable a typeof<double>
                                        | AttributeTypeCode.Integer          -> makeNullable a typeof<int>
                                        | AttributeTypeCode.String           -> typeof<string>
                                        | AttributeTypeCode.Uniqueidentifier -> typeof<Guid>
                                        | AttributeTypeCode.Lookup           -> typeof<EntityReference>
                                        | AttributeTypeCode.Money            -> typeof<Money>
                                        | _                                  -> typeof<string>
                        
                        let prop = ProvidedProperty(name,ty,GetterCode = fun args -> 
                            let meth = typeof<XrmEntity>.GetMethod "GetAttribute"
                            let meth = meth.MakeGenericMethod [|ty|]
                            Expr.Call(args.[0],meth,[Expr.Value name]))  
                            
                        prop.AddXmlDocDelayed( fun () -> extractDescription a.Description )
                        yield prop ] 
                
                let relProps =
                    [for r in oneToMany do 
                        let name = r.SchemaName
                        let ty = typedefof<System.Linq.IQueryable<_>>
                        let ty = ty.MakeGenericType(fst (baseTypes.Force().[r.ReferencingEntity]))
                        let friendlyName = createRelationshipName r OneToMany
                        let prop = ProvidedProperty(friendlyName ,ty,GetterCode = fun args ->
                            let pe = r.ReferencedEntity     
                            let pk = r.ReferencedAttribute
                            let fe = r.ReferencingEntity 
                            let fk = r.ReferencingAttribute
                            <@@ XrmDataContext._CreateRelated((%%(args.[0]) : XrmEntity),name,pe,pk,fe,fk,"" ,RelationshipDirection.Children) @@> )

                        prop.AddXmlDocDelayed( fun () -> sprintf """<summary>One to many relationship between %s ---&lt; %s from %s to %s
                                                                           <para>Relationship name : %s</para></summary>"""
                                                            r.ReferencedEntity r.ReferencingEntity r.ReferencedAttribute r.ReferencingAttribute r.SchemaName)                        
                        yield prop] 
                    @
                    [for r in manyToOne do 
                        let name = r.SchemaName
                        let ty = typedefof<System.Linq.IQueryable<_>>
                        let ty = ty.MakeGenericType(fst (baseTypes.Force().[r.ReferencedEntity]))
                        let friendlyName = createRelationshipName r ManyToOne
                        let prop = ProvidedProperty(friendlyName ,ty,GetterCode = fun args -> 
                            let pe = r.ReferencedEntity    
                            let pk = r.ReferencedAttribute
                            let fe = r.ReferencingEntity 
                            let fk = r.ReferencingAttribute
                            <@@ XrmDataContext._CreateRelated((%%(args.[0]) : XrmEntity), name,pe,pk,fe,fk,"",RelationshipDirection.Parents) @@> )

                        prop.AddXmlDocDelayed( fun () -> sprintf """<summary>Many to one relationship between %s &gt;--- %s from %s to %s
                                                                             <para>Relationship name : %s</para></summary>""" 
                                                            r.ReferencedEntity r.ReferencingEntity r.ReferencedAttribute r.ReferencingAttribute r.SchemaName )                        
                        yield prop]
                    @
                    [for r in manyToMany do 
                        let name = r.SchemaName
                        let ty = typedefof<System.Linq.IQueryable<_>>
                        let ty = ty.MakeGenericType(fst (baseTypes.Force().[if key = r.Entity1LogicalName then r.Entity2LogicalName else r.Entity1LogicalName]))
                        let friendlyName = ("M:M " + name)
                        let prop = ProvidedProperty(friendlyName ,ty,GetterCode = fun args -> 
                            // this is a little different, need to work out which side of the relationship we are on 
                            let(pe,pk,fe,fk,ie) = 
                                if key = r.Entity1LogicalName then
                                    (r.Entity2LogicalName,
                                     r.Entity2IntersectAttribute,
                                     r.Entity1LogicalName,
                                     r.Entity1IntersectAttribute,
                                     r.IntersectEntityName)
                                else
                                    (r.Entity1LogicalName,
                                     r.Entity1IntersectAttribute,
                                     r.Entity2LogicalName,
                                     r.Entity2IntersectAttribute,
                                     r.IntersectEntityName)

                            <@@ XrmDataContext._CreateRelated((%%(args.[0]) : XrmEntity), name,pe,pk,fe,fk,ie,RelationshipDirection.Children) @@> )
                          
                        prop.AddXmlDocDelayed( fun () -> sprintf """<summary>Many to many relationship between %s &gt;--&lt; %s via intersection entity %s
                                                                            <para>Relationship name : %s</para> %s %s </summary>""" 
                                                            r.Entity1LogicalName r.Entity2LogicalName r.IntersectEntityName r.SchemaName r.Entity1IntersectAttribute r.Entity2IntersectAttribute )
                        
                        yield prop]
                attProps @ relProps)

        serviceType.AddMembersDelayed( fun () ->
            [ for (KeyValue(key,(t,desc))) in baseTypes.Force() do
                let name = key
                let ty = typedefof<System.Linq.IQueryable<_>>
                let ty = ty.MakeGenericType(t)
                let prop = (ProvidedProperty(key, ty, IsStatic=false, GetterCode = fun args -> <@@ XrmDataContext._CreateEntities(name) @@> ))
                prop.AddXmlDoc (sprintf "<summary>The set of %s entities.<para>Entity description : %s</para></summary>" key desc)
                serviceType.AddMember prop
                yield t ] )
                      
        let rootType = ProvidedTypeDefinition(xrmRuntimeInfo.RuntimeAssembly,ns,rootTypeName,baseType=Some typeof<obj>, HideObjectMethods=true)
        rootType.AddMembers [ serviceType ]
        rootType.AddMembersDelayed (fun () -> 
            [ let meth = 
                ProvidedMethod ("GetDataContext", [],
                                serviceType, IsStaticMethod=true,
                                InvokeCode = (fun _ -> 
                                    let meth = typeof<XrmDataContext>.GetMethod "_Create"                                                         
                                    Expr.Call(meth, [Expr.Value orgService;Expr.Value user;Expr.Value pwd;Expr.Value domain;])
                                    ))
              meth.AddXmlDoc "<summary>Retuns an instance of the CRM provider using the static parameters</summary>"
              yield meth
                                        
              let meth = ProvidedMethod ("GetDataContext", [ProvidedParameter("organizationService",typeof<IOrganizationService>)], 
                                                            serviceType, IsStaticMethod=true,
                                                            InvokeCode = (fun args ->
                                                                let meth = typeof<XrmDataContext>.GetMethod "_CreateWithInstance"
                                                                Expr.Call(meth, [args.[0]])));
              meth.AddXmlDoc "<summary>Retuns an instance of the CRM provider</summary>
                              <param name='organizationService'>An instance of the Microsoft Dynamics CRM 2011 organization service, this could be from a plugin or workflow</param>"
              yield meth

              let meth = ProvidedMethod ("GetDataContext", [ProvidedParameter("organizationServiceUrl",typeof<string>)], 
                                                            serviceType, IsStaticMethod=true,
                                                            InvokeCode = (fun args ->
                                                                let meth = typeof<XrmDataContext>.GetMethod "_Create"
                                                                Expr.Call(meth, [args.[0];Expr.Value "";Expr.Value "";Expr.Value ""])))
              meth.AddXmlDoc "<summary>Retuns an instance of the CRM provider</summary>
                              <param name='organizationServiceUrl'>The SOAP Endpoint address of the Microsoft Dynamics CRM 2011 organization service</param>"
              yield meth                                

              let meth = ProvidedMethod ("GetDataContext", [ProvidedParameter("organizationServiceUrl",typeof<string>);
                                                            ProvidedParameter("username",typeof<string>);
                                                            ProvidedParameter("password",typeof<string>);
                                                            ProvidedParameter("domain",typeof<string>)], 
                                                            serviceType, IsStaticMethod=true,
                                                            InvokeCode = (fun args ->
                                                                let meth = typeof<XrmDataContext>.GetMethod "_Create"
                                                                Expr.Call(meth, [args.[0];args.[1];args.[2];args.[3]])));
              meth.AddXmlDoc "<summary>Retuns an instance of the CRM provider</summary>
                              <param name='organizationServiceUrl'>The SOAP Endpoint address of the Microsoft Dynamics CRM 2011 organization service</param>
                              <param name='username'>The username for use with Windows Authentication</param>
                              <param name='password'>The password for use with Windows Authentication</param>
                              <param name='domain'>The domain name for use with Windows Authentication</param>"
              yield meth
            ])
        rootType
    
    let paramXrmType   = ProvidedTypeDefinition(xrmRuntimeInfo.RuntimeAssembly, ns, "XrmDataProvider", Some(typeof<obj>), HideObjectMethods = true)        
    let orgUri = ProvidedStaticParameter("organizationServiceUrl",typeof<string>)
    let user = ProvidedStaticParameter("username",typeof<string>,"")
    let pwd = ProvidedStaticParameter("password",typeof<string>,"")
    let domain = ProvidedStaticParameter("domain",typeof<string>,"")
    let nullables = ProvidedStaticParameter("useNullableValues",typeof<bool>,false)
    let relationships = ProvidedStaticParameter("relationshipNamingType",typeof<RelationshipNamingType>,RelationshipNamingType.ParentChildPrefix)
    let helpText = "<summary>Typed representation of a Microsoft Dynamics CRM Organization</summary>                    
                    <param name='organizationServiceUrl'>The SOAP Endpoint address of the Microsoft Dynamics CRM 2011 organization service</param>
                    <param name='username'>The username for use with Windows Authentication</param>
                    <param name='password'>The password for use with Windows Authentication</param>
                    <param name='domain'>The domain name for use with Windows Authentication</param>
                    <param name='useNullableValues'>If true, the provider will generate Nullable&lt;T&gt; for value types marked as not required in CRM. If false, value type attribtues selected that have no value will be returned as default(T).  In either case, missing strings are always represented by String.Empty, not null.</param>
                    <param name='relationshipNamingType'>Determines how the relationships appear on the generated types. See comments on the RelationshipNameType for details.</param>"
        
    do paramXrmType.DefineStaticParameters([orgUri;user;pwd;domain;nullables;relationships], fun typeName args -> 
        createTypes(args.[0] :?> string, args.[1] :?> string, args.[2] :?> string, args.[3] :?> string, args.[4] :?> bool, args.[5] :?> RelationshipNamingType,typeName))

    do paramXrmType.AddXmlDoc helpText               

    // add them to the namespace    
    do this.AddNamespace(ns, [paramXrmType])
                            
[<assembly:TypeProviderAssembly>] 
do()


