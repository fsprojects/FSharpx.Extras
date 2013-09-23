// Copyright (c) Microsoft Corporation 2005-2013.
// This sample code is provided "as is" without warranty of any kind. 
// We disclaim all warranties, either express or implied, including the 
// warranties of merchantability and fitness for a particular purpose.

namespace FSharpx.TypeProviders.XrmProvider

open FSharpx.TypeProviders.XrmProvider.Runtime
open FSharpx.TypeProviders.XrmProvider.Runtime.Common

open System
open System.IO
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
open Microsoft.Crm.Services.Utility

open Samples.FSharp.ProvidedTypes

/// Determines how relationship names appear on generated types
type RelationshipNamingType =
    /// Relationships will be named with their schema name prefixed by 'Children of' or 'Parent of' and suffixed with the returned entity type name.
    | ParentChildPrefix = 0
    /// Relationships will be named with their schema name prefixed by 1:N, N:1 or N:N.
    | CrmStylePrefix = 1
    /// Relationships will be named only with their schema name.  You will need to examine the intelliense comments to determine which direction the relationships point.                         
    | SchemaNameOnly = 2

type OptionSetEnum  =
    | Unused = 2147483647

type internal XrmRuntimeInfo (config : TypeProviderConfig) =
    let runtimeAssembly = Assembly.LoadFrom(config.RuntimeAssembly)    
    member this.RuntimeAssembly = runtimeAssembly   

type private RelationshipType =
    | OneToMany
    | ManyToOne
    | ManyToMany
    
type private OptionSetType =
    | Picklist of PicklistAttributeMetadata
    | State of StateAttributeMetadata   
    | Status of StatusAttributeMetadata 

[<TypeProvider>]
type XrmTypeProvider(config: TypeProviderConfig) as this =     
    inherit TypeProviderForNamespaces()
    let xrmRuntimeInfo = XrmRuntimeInfo(config)
    let ns = "FSharpx.TypeProviders.XrmProvider"     
    let asm = Assembly.GetExecutingAssembly()
    
    let createOrgService uri clientCreds deviceCreds =
        let uri = Uri(uri)        
        let orgProxy = new OrganizationServiceProxy(uri, null, clientCreds, deviceCreds);
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
        
    let createTypes(orgService,nullables,relationshipNameType,dataBindingMode,crmOnline,individualsAmount,credentialsFile,usernameParam,passwordParam,domainParam,rootTypeName) =       
        let absoluteCredentialsFile = 
            if String.IsNullOrWhiteSpace credentialsFile then credentialsFile 
            elif Path.IsPathRooted credentialsFile then Path.GetFullPath credentialsFile
            else Path.GetFullPath(Path.Combine(config.ResolutionFolder, credentialsFile))
        
        let (username,password,domain) =
            match absoluteCredentialsFile with
            | "" -> (usernameParam,passwordParam,domainParam)
            | _ -> 
                if not (File.Exists absoluteCredentialsFile) then failwithf "Could not find credentials file at %s" absoluteCredentialsFile
                let values = File.ReadAllLines absoluteCredentialsFile
                match values.Length with
                | 2 -> (values.[0],values.[1],"")
                | 3 -> (values.[0],values.[1],values.[2])
                | _ -> failwith "Credentials file should have between two to three lines, containing the username, password and domain respectively"

        let anyCredentialsSupplied = not (String.IsNullOrWhiteSpace username && String.IsNullOrWhiteSpace password && String.IsNullOrWhiteSpace domain)        
        
        let creds =
            let creds = new ClientCredentials()
            match crmOnline,username with
            | true, _ -> // CRM online uses windows live authentication so the name / password go on the outer set of properties
                creds.Windows.ClientCredential <- CredentialCache.DefaultNetworkCredentials
                creds.UserName.UserName <- username
                creds.UserName.Password <- password
            | false, null | false, "" ->  // use AD implicitly
                creds.Windows.ClientCredential <- CredentialCache.DefaultNetworkCredentials
            | x ->                  
                creds.Windows.ClientCredential <- new NetworkCredential(username,password,domain)
                creds.UserName.UserName <- if (String.IsNullOrEmpty(domain)) then username else sprintf "%s\\%s" domain username
                creds.UserName.Password <- password
            creds

        let org = createOrgService orgService creds (if crmOnline then DeviceIdManager.LoadOrRegisterDevice() else null)
        
        let createRelationshipName (meta:RelationshipMetadataBase) relationshipType = 
            match relationshipNameType with
            | RelationshipNamingType.ParentChildPrefix -> 
                match meta, relationshipType with
                | :? OneToManyRelationshipMetadata as meta, OneToMany -> sprintf "Children of %s (%s)" meta.SchemaName meta.ReferencingEntity
                | :? OneToManyRelationshipMetadata as meta, ManyToOne -> sprintf "Parent of %s (%s)" meta.SchemaName meta.ReferencedEntity
                | :? ManyToManyRelationshipMetadata, ManyToMany -> ("Many of %s" + meta.SchemaName) // todo: work out what side we are on and display the other
                | _ -> failwith "invalid relationship type combination"
            | RelationshipNamingType.CrmStylePrefix -> 
                match meta, relationshipType with
                | :? OneToManyRelationshipMetadata, OneToMany ->   ("1:N " + meta.SchemaName)  
                | :? OneToManyRelationshipMetadata, ManyToOne ->   ("N:1 " + meta.SchemaName)
                | :? ManyToManyRelationshipMetadata, ManyToMany -> ("N:N " + meta.SchemaName)
                | _ -> failwith "invalid relationship type combination"
            | RelationshipNamingType.SchemaNameOnly -> meta.SchemaName
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
                            let oneToMany = match emeta.EntityMetadata.OneToManyRelationships with null -> [] | xs -> Array.toList xs
                            let manyToOne = match emeta.EntityMetadata.ManyToOneRelationships with null -> [] | xs -> Array.toList xs
                            let manyToMany = match emeta.EntityMetadata.ManyToManyRelationships with null -> [] | xs -> Array.toList xs
                            (attr,oneToMany,manyToOne,manyToMany))]       
                      
        let getEntityAttibtues logicalName = entityAttributes.Force().[logicalName].Force()
        let serviceType = ProvidedTypeDefinition( "XrmService", Some typeof<XrmDataContext>, HideObjectMethods = true)
        
        // first create all the types so we are able to recursively reference them in each other's definitions
        let baseTypes =
            lazy
                dict [ for entity in entities.Force() do  
                        let name = entity.LogicalName
                        let t = ProvidedTypeDefinition(name, Some typeof<XrmEntity>, HideObjectMethods = false)
                        t.AddMemberDelayed(fun () -> ProvidedConstructor([],InvokeCode = fun _ -> <@@ new XrmEntity(name,dataBindingMode) @@>  ))
                        let desc = extractDescription entity.Description
                        t.AddXmlDoc desc
                        yield entity.LogicalName,(t,desc,entity.PrimaryNameAttribute) ]

        let getOptionset =
            let data = Dictionary<string,ProvidedTypeDefinition>()
            fun optionSetType ->
                let createType name (options:OptionMetadataCollection) = 
                    let t = ProvidedTypeDefinition(name,Some typeof<OptionSetEnum>)
                    t.AddMembers([for o in options -> ProvidedLiteralField(extractDescription o.Label,t,o.Value.Value)] )
                    let values = [|for o in options -> extractDescription o.Label|]
                    t.AddMember(ProvidedMethod("GetProvidedValues",[],typeof<string array>,InvokeCode = fun _ -> <@@ values @@> ))
                    serviceType.AddMember t
                    data.[name] <- t
                match optionSetType with
                | Picklist(attribute) -> 
                    if not (data.ContainsKey attribute.OptionSet.Name) then createType attribute.OptionSet.Name attribute.OptionSet.Options
                    data.[attribute.OptionSet.Name]
                | Status(attribute) -> 
                    if not (data.ContainsKey attribute.OptionSet.Name) then createType attribute.OptionSet.Name attribute.OptionSet.Options
                    data.[attribute.OptionSet.Name]
                | State(attribute) -> 
                    if not (data.ContainsKey attribute.OptionSet.Name) then createType attribute.OptionSet.Name attribute.OptionSet.Options
                    data.[attribute.OptionSet.Name]

        let createIndividualsType entity =
            let (et,_,pa) = baseTypes.Force().[entity]
            let t = ProvidedTypeDefinition(entity + "Individuals", Some typeof<obj>, HideObjectMethods = true)
            t.AddXmlDocDelayed(fun _ -> sprintf "A sample of %s individuals from the CRM organization as supplied in the static parameters" entity)
            t.AddMembersDelayed( fun _ -> 
                let q = QueryExpression(entity)
                q.ColumnSet <- new ColumnSet([|pa|])
                q.PageInfo.Count <- individualsAmount
                q.PageInfo.PageNumber <- 1
                org.RetrieveMultiple(q).Entities
                |> Seq.choose(fun e -> match box (e.GetAttributeValue pa) with
                                       | :? String as v when not(String.IsNullOrWhiteSpace v) -> 
                                            Some(ProvidedProperty(v,et,GetterCode = fun _ -> let id = e.Id.ToString()
                                                                                             <@@ XrmDataContext._GetIndividual(entity,id,dataBindingMode) @@> ))
                                       | _ -> None)
                |> Seq.toList )            
            t
            
        let baseCollectionTypes =
            lazy
                dict [ for entity in entities.Force() do  
                        let name = entity.LogicalName 
                        let (et,_,_) = baseTypes.Force().[name]
                        let ct = ProvidedTypeDefinition(entity.LogicalName + "Set", Some typeof<obj>,HideObjectMethods=true)
                        ct.AddInterfaceImplementationsDelayed( fun () -> [ProvidedTypeBuilder.MakeGenericType(typedefof<System.Linq.IQueryable<_>>,[et :> Type])])
                        let it = createIndividualsType name
                        let prop = ProvidedProperty("Individuals",it, GetterCode = fun _ -> <@@ new obj() @@> )
                        prop.AddXmlDoc(sprintf "A sample of %s individuals from the CRM organization as supplied in the static parameters" name)
                        let meth = ProvidedMethod("Create",[],et, IsStaticMethod = false, InvokeCode = fun _ -> <@@ XrmEntity(name,dataBindingMode) @@>)
                        meth.AddXmlDoc(sprintf "Creates a new instance of the %s entity" name)
                        ct.AddMembersDelayed( fun () -> [prop :> MemberInfo;meth :> MemberInfo])
                        yield entity.LogicalName,(ct,it) ]  
        
       // add the attributes and relationships
        for KeyValue(key,(t,desc,_)) in baseTypes.Force() do 
            t.AddMembersDelayed(fun () -> 
                let makeNullable (a:AttributeMetadata) ty = 
                    if nullables && a.RequiredLevel.Value = AttributeRequiredLevel.None then typedefof<Nullable<_>>.MakeGenericType([|ty|])
                    else ty

                let (attr,oneToMany,manyToOne,manyToMany) = getEntityAttibtues key
                let attProps = 
                    let createAttributeProperty ty name description isOptionSet =
                        let enumQ t e = // have to do a bit of reflection magic and type gymnastics here to get provided enums to work 
                            let (Quotations.Patterns.Call(None, enum, [_])) = <@ enum<OptionSetEnum>(0) @> 
                            Quotations.Expr.Call(ProvidedTypeBuilder.MakeGenericMethod(enum.GetGenericMethodDefinition(), [t]), [e])

                        let prop = 
                            ProvidedProperty(
                                name,ty,
                                GetterCode = (fun args -> 
                                    if isOptionSet then enumQ ty <@@ ((%%args.[0]:>XrmEntity).GetEnumValue name) @@>
                                    else let meth = typeof<XrmEntity>.GetMethod("GetAttribute").MakeGenericMethod([|ty|])
                                         Expr.Call(args.[0],meth,[Expr.Value name])),
                                SetterCode = (fun args ->
                                    let meth = typeof<XrmEntity>.GetMethod "SetAttribute"                                                                                                                
                                    if isOptionSet then // for some reason quoted version(s) of this simply refuse to work !                                          
                                        Expr.Call(args.[0],meth,[Expr.Value name; Expr.NewObject(typeof<OptionSetValue>.GetConstructor([|typeof<int>|]),[ <@@int (%%args.[1]:OptionSetEnum)@@>])])
                                    else Expr.Call(args.[0],meth,[Expr.Value name;Expr.Coerce(args.[1], typeof<obj>)])))
                            
                        prop.AddXmlDocDelayed( fun () -> extractDescription description )
                        prop
                    
                    let chooseAttribute (a:AttributeMetadata) =
                        match a.AttributeType.HasValue with
                        | false-> None
                        | true -> match a.AttributeType.Value  with
                                  | AttributeTypeCode.BigInt           -> Some (makeNullable a typeof<int64>,false)
                                  | AttributeTypeCode.Boolean          -> Some (makeNullable a typeof<bool>,false)
                                  | AttributeTypeCode.DateTime         -> Some (makeNullable a typeof<DateTime>,false)
                                  | AttributeTypeCode.Decimal          -> Some (makeNullable a typeof<Decimal>,false)
                                  | AttributeTypeCode.Double           -> Some (makeNullable a typeof<double>,false)
                                  | AttributeTypeCode.Integer          -> Some (makeNullable a typeof<int>,false)
                                  | AttributeTypeCode.String           -> Some (typeof<string>,false)
                                  | AttributeTypeCode.Uniqueidentifier -> Some (typeof<Guid>,false)
                                  | AttributeTypeCode.Lookup           -> Some (typeof<EntityReference>,false)
                                  | AttributeTypeCode.Money            -> Some (typeof<Money>,false)
                                  | AttributeTypeCode.State            -> Some (makeNullable a (getOptionset (State(a:?>StateAttributeMetadata)) :> Type), true)
                                  | AttributeTypeCode.Status           -> Some (makeNullable a (getOptionset (Status(a:?>StatusAttributeMetadata)) :> Type),true)
                                  | AttributeTypeCode.Picklist         -> Some (makeNullable a (getOptionset (Picklist(a:?>PicklistAttributeMetadata)) :> Type),true)
                                  | AttributeTypeCode.Virtual          -> None
                                  | _                                  -> Some (typeof<string>,false)
                                  |> Option.map(fun (ty,isOptionSet)  -> createAttributeProperty ty a.LogicalName a.Description isOptionSet )
                    
                    attr |> List.choose chooseAttribute

                let formattedValuesProp = 
                    let formattedType = ProvidedTypeDefinition(key + "Formatted",Some(typeof<obj>),HideObjectMethods=true)
                    formattedType.AddMemberDelayed( fun () -> ProvidedConstructor([ProvidedParameter("values",typeof<XrmEntity>)], 
                                                                InvokeCode = fun args ->  <@@ (%%args.[0]:XrmEntity) :> obj @@>  ))
                    formattedType.AddMembersDelayed( fun () ->
                        attr |> List.choose(fun at -> match at.AttributeType.Value with                                                                 
                                                      | AttributeTypeCode.Lookup     | AttributeTypeCode.Decimal  
                                                      | AttributeTypeCode.Customer   | AttributeTypeCode.Integer  
                                                      | AttributeTypeCode.Owner      | AttributeTypeCode.Memo
                                                      | AttributeTypeCode.BigInt     | AttributeTypeCode.Money    
                                                      | AttributeTypeCode.Boolean    | AttributeTypeCode.State          
                                                      | AttributeTypeCode.DateTime   | AttributeTypeCode.Status         
                                                      | AttributeTypeCode.Picklist ->
                                                        Some(ProvidedProperty(
                                                                at.LogicalName,typeof<string>,
                                                                GetterCode = (fun args -> 
                                                                    let key = at.LogicalName
                                                                    <@@ ((%%args.[0]:obj):?>XrmEntity).GetFormattedValue(key) @@>)))
                                                      | _ -> None ))
                                                    
                    serviceType.AddMember formattedType
                    ProvidedProperty("Formatted",formattedType, GetterCode = (fun args -> <@@ (%%args.[0]:XrmEntity):>obj @@> ))
                                            
                let relProps =
                    [for r in oneToMany do 
                        let name = r.SchemaName
                        let (et,_,_) = (baseTypes.Force().[r.ReferencingEntity])
                        let ty = typedefof<System.Linq.IQueryable<_>>
                        let ty = ty.MakeGenericType et
                        let friendlyName = createRelationshipName r OneToMany
                        let prop = ProvidedProperty(friendlyName ,ty,GetterCode = fun args ->
                            let pe = r.ReferencedEntity     
                            let pk = r.ReferencedAttribute
                            let fe = r.ReferencingEntity 
                            let fk = r.ReferencingAttribute
                            <@@ XrmDataContext._CreateRelated((%%(args.[0]) : XrmEntity),name,pe,pk,fe,fk,"" ,RelationshipDirection.Children,dataBindingMode) @@> )

                        prop.AddXmlDocDelayed( fun () -> sprintf """<summary>One to many relationship between %s ---&lt; %s from %s to %s
                                                                           <para>Relationship name : %s</para></summary>"""
                                                            r.ReferencedEntity r.ReferencingEntity r.ReferencedAttribute r.ReferencingAttribute r.SchemaName)                        
                        yield prop] 
                    @
                    [for r in manyToOne do 
                        let name = r.SchemaName
                        let (et,_,_) = (baseTypes.Force().[r.ReferencedEntity])
                        let ty = typedefof<System.Linq.IQueryable<_>>
                        let ty = ty.MakeGenericType et
                        let friendlyName = createRelationshipName r ManyToOne
                        let prop = ProvidedProperty(friendlyName ,ty,GetterCode = fun args -> 
                            let pe = r.ReferencedEntity    
                            let pk = r.ReferencedAttribute
                            let fe = r.ReferencingEntity 
                            let fk = r.ReferencingAttribute
                            <@@ XrmDataContext._CreateRelated((%%(args.[0]) : XrmEntity), name,pe,pk,fe,fk,"",RelationshipDirection.Parents,dataBindingMode) @@> )

                        prop.AddXmlDocDelayed( fun () -> sprintf """<summary>Many to one relationship between %s &gt;--- %s from %s to %s
                                                                             <para>Relationship name : %s</para></summary>""" 
                                                            r.ReferencedEntity r.ReferencingEntity r.ReferencedAttribute r.ReferencingAttribute r.SchemaName )
                        yield prop]
                    @
                    [for r in manyToMany do 
                        let name = r.SchemaName
                        let ty = typedefof<System.Linq.IQueryable<_>>
                        let (et,_,_) = (baseTypes.Force().[if key = r.Entity1LogicalName then r.Entity2LogicalName else r.Entity1LogicalName])
                        let ty = ty.MakeGenericType et
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

                            <@@ XrmDataContext._CreateRelated((%%(args.[0]) : XrmEntity), name,pe,pk,fe,fk,ie,RelationshipDirection.Children,dataBindingMode) @@> )
                          
                        prop.AddXmlDocDelayed( fun () -> sprintf """<summary>Many to many relationship between %s &gt;--&lt; %s via intersection entity %s
                                                                            <para>Relationship name : %s</para> %s %s </summary>""" 
                                                            r.Entity1LogicalName r.Entity2LogicalName r.IntersectEntityName r.SchemaName r.Entity1IntersectAttribute r.Entity2IntersectAttribute )
                        
                        yield prop]
                [formattedValuesProp] @ attProps @ relProps)

        serviceType.AddMembersDelayed( fun () ->
            [ for (KeyValue(key,(t,desc,_))) in baseTypes.Force() do
                let (ct,it) = baseCollectionTypes.Force().[key]                
                let prop = ProvidedProperty(key + "Set",ct, GetterCode = fun args -> <@@ XrmDataContext._CreateEntities(key,dataBindingMode) @@> )
                prop.AddXmlDoc (sprintf "<summary>The set of %s entities.<para>Entity description : %s</para></summary>" key desc)                
                yield t :> MemberInfo
                yield ct :> MemberInfo
                yield it :> MemberInfo
                yield prop :> MemberInfo ] )
                      
        let rootType = ProvidedTypeDefinition(xrmRuntimeInfo.RuntimeAssembly,ns,rootTypeName,baseType=Some typeof<obj>, HideObjectMethods=true)
        rootType.AddMembers [ serviceType ]
        rootType.AddMembersDelayed (fun () -> 
            [ let meth = 
                ProvidedMethod ("GetDataContext", [],
                                serviceType, IsStaticMethod=true,
                                InvokeCode = (fun _ -> 
                                    let meth = typeof<XrmDataContext>.GetMethod "_Create"
                                    Expr.Call(meth, [Expr.Value orgService;Expr.Value username;Expr.Value password;Expr.Value domain;Expr.Value crmOnline])
                                    ))
              meth.AddXmlDoc "<summary>Returns an instance of the CRM provider using the static parameters</summary>"
              
              if anyCredentialsSupplied && not config.IsHostedExecution && not config.IsInvalidationSupported then 
                meth.AddObsoleteAttribute("Compiled applications must supply credentials in the GetDataContext method when also supplying credentials as static parameters",true)

              yield meth
                                        
              let meth = ProvidedMethod ("GetDataContext", [ProvidedParameter("organizationService",typeof<IOrganizationService>)], 
                                                            serviceType, IsStaticMethod=true,
                                                            InvokeCode = (fun args ->
                                                                let meth = typeof<XrmDataContext>.GetMethod "_CreateWithInstance"
                                                                Expr.Call(meth, [args.[0];])));
              meth.AddXmlDoc "<summary>Retuns an instance of the CRM provider</summary>
                              <param name='organizationService'>An instance of the Microsoft Dynamics CRM 2011 organization service, this could be from a plugin or workflow</param>"
              yield meth

              let meth = ProvidedMethod ("GetDataContext", [ProvidedParameter("organizationServiceUrl",typeof<string>)], 
                                                            serviceType, IsStaticMethod=true,
                                                            InvokeCode = (fun args ->
                                                                let meth = typeof<XrmDataContext>.GetMethod "_Create"
                                                                Expr.Call(meth, [args.[0];Expr.Value "";Expr.Value "";Expr.Value ""; Expr.Value false])))
            
              if anyCredentialsSupplied && not config.IsHostedExecution && not config.IsInvalidationSupported then 
                  meth.AddObsoleteAttribute("Compiled applications must supply credentials in the GetDataContext method when also supplying credentials as static parameters")

              meth.AddXmlDoc "<summary>Retuns an instance of the CRM provider</summary>
                              <param name='organizationServiceUrl'>The SOAP Endpoint address of the Microsoft Dynamics CRM 2011 organization service</param>"
              yield meth                                

              let meth = ProvidedMethod ("GetDataContext", [ProvidedParameter("organizationServiceUrl",typeof<string>);
                                                            ProvidedParameter("username",typeof<string>);
                                                            ProvidedParameter("password",typeof<string>);
                                                            ProvidedParameter("domain",typeof<string>);
                                                            ProvidedParameter("crmOnline",typeof<bool>,false,crmOnline)], 
                                                            serviceType, IsStaticMethod=true,
                                                            InvokeCode = (fun args ->
                                                                let meth = typeof<XrmDataContext>.GetMethod "_Create"
                                                                Expr.Call(meth, [args.[0];args.[1];args.[2];args.[3];args.[4]])));
              meth.AddXmlDoc "<summary>Retuns an instance of the CRM provider</summary>
                              <param name='organizationServiceUrl'>The SOAP Endpoint address of the Microsoft Dynamics CRM 2011 organization service</param>
                              <param name='username'>The username for use with Windows Authentication</param>
                              <param name='password'>The password for use with Windows Authentication</param>
                              <param name='domain'>The domain name for use with Windows Authentication</param>
                              <param name='crmOnilne'>Set this to true if using a CRM Online deployment. This will change the authentication to use Windows Live.</param>"

              yield meth
            ])
        rootType
    
    let paramXrmType = ProvidedTypeDefinition(xrmRuntimeInfo.RuntimeAssembly, ns, "XrmDataProvider", Some(typeof<obj>), HideObjectMethods = true)
    
    let orgUri = ProvidedStaticParameter("OrganizationServiceUrl",typeof<string>)    
    let nullables = ProvidedStaticParameter("UseNullableValues",typeof<bool>,false)
    let relationships = ProvidedStaticParameter("RelationshipNamingType",typeof<RelationshipNamingType>,RelationshipNamingType.ParentChildPrefix)
    let bindingMode = ProvidedStaticParameter("DataBindingMode",typeof<DataBindingMode>,DataBindingMode.NormalValues)
    let crmOnline = ProvidedStaticParameter("CrmOnline",typeof<bool>,false)    
    let individualsAmount = ProvidedStaticParameter("IndividualsAmount",typeof<int>,1000)    
    let credentialsFile = ProvidedStaticParameter("CredentialsFile",typeof<string>,"")
    let user = ProvidedStaticParameter("Username",typeof<string>,"")
    let pwd = ProvidedStaticParameter("Password",typeof<string>,"")
    let domain = ProvidedStaticParameter("Domain",typeof<string>,"")
    let helpText = "<summary>Typed representation of a Microsoft Dynamics CRM Organization</summary>                    
                    <param name='OrganizationServiceUrl'>The SOAP Endpoint address of the Microsoft Dynamics CRM 2011 organization service</param>                    
                    <param name='UseNullableValues'>If true, the provider will generate Nullable&lt;T&gt; for value types marked as not required in CRM. If false, value type attribtues selected that have no value will be returned as default(T).  In either case, missing strings are always represented by String.Empty, not null.</param>
                    <param name='RelationshipNamingType'>Determines how the relationships appear on the generated types. See comments on the RelationshipNameType for details.</param>
                    <param name='DataBindingMode'>Determines how attributes are presented when using data binding.</param>
                    <param name='CrmOnline'>Set this to true if using a CRM Online deployment. This will change the authentication to use Windows Live.</param>
                    <param name='IndividualsAmount'>The amount of sample entities to project into the type system for each CRM entity type. Default 1000.</param>
                    <param name='CredentialsFile'>Path to plain text file that includes username, password and optionally the domain. In the case of CRM Online these would be the relevant Windows Live username and password.</param>
                    <param name='Username'>The username for use with Windows Authentication</param>
                    <param name='Password'>The password for use with Windows Authentication</param>
                    <param name='Domain'>The domain name for use with Windows Authentication</param>"
        
    do paramXrmType.DefineStaticParameters([orgUri;nullables;relationships;bindingMode;crmOnline;individualsAmount;credentialsFile;user;pwd;domain;], fun typeName args -> 
        createTypes(args.[0] :?> string,                  // OrganizationServiceUrl
                    args.[1] :?> bool,                    // useNullable 
                    args.[2] :?> RelationshipNamingType,  // relationship naming
                    args.[3] :?> DataBindingMode,         // data binding
                    args.[4] :?> bool,                    // CrmOnline 
                    args.[5] :?> int,                     // Indivudals Amount
                    args.[6] :?> string,                  // creds file 
                    args.[7] :?> string,                  // user name
                    args.[8] :?> string,                  // pasword 
                    args.[9] :?> String,                  // domain
                    typeName))

    do paramXrmType.AddXmlDoc helpText               

    // add them to the namespace    
    do this.AddNamespace(ns, [paramXrmType])
                            
[<assembly:TypeProviderAssembly>] 
do()


