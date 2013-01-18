// Copyright (c) Microsoft Corporation 2005-2013.
// This sample code is provided "as is" without warranty of any kind. 
// We disclaim all warranties, either express or implied, including the 
// warranties of merchantability and fitness for a particular purpose. 

namespace FSharpx.TypeProviders.Management.DesignTime

open System
open System.Linq.Expressions
open System.Management 
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Control
open Samples.FSharp.ProvidedTypes
open Microsoft.FSharp.Quotations

#nowarn "40"

/// Helpers to find the handles in type provider runtime DLL. 
type internal RuntimeInfo (config : TypeProviderConfig) =
   let runtimeAssembly = Assembly.LoadFrom(config.RuntimeAssembly)

   let RuntimeAPI = runtimeAssembly.GetType("FSharpx.TypeProviders.Management.Runtime.RuntimeAPI")
   member val InvokeInstanceManagementMethod = RuntimeAPI.GetMethod("InvokeManagementMethod")
   member val InvokeStaticManagementMethod = RuntimeAPI.GetMethod("InvokeStaticManagementMethod")
   member val GetManagementProp = RuntimeAPI.GetMethod("GetManagementProp")
   member val GetClassInstancesByName = RuntimeAPI.GetMethod("GetClassInstancesByName")
   member val GetManagementObjectFromReference = RuntimeAPI.GetMethod("GetManagementObjectFromReference")
   member val GetReferenceToManagementObject = RuntimeAPI.GetMethod("GetReferenceToManagementObject")
   member val TranslateDateTime = RuntimeAPI.GetMethod("TranslateDateTime")
   member val ArrayContains = RuntimeAPI.GetMethod("ArrayContains")
   member val Set = RuntimeAPI.GetMethod("Set")
   member val IntrinsicEventMethod = RuntimeAPI.GetMethod("IntrinsicEventMethod")
   member val ExtrinsicEventMethod = RuntimeAPI.GetMethod("ExtrinsicEventMethod")
   member val InstanceOperationClass = runtimeAssembly.GetType("FSharpx.TypeProviders.Management.Runtime.InstanceOperation`1")
   member val InstanceModificationClass = runtimeAssembly.GetType("FSharpx.TypeProviders.Management.Runtime.InstanceModification`1")
   member val DataContextClass = runtimeAssembly.GetType("FSharpx.TypeProviders.Management.Runtime.DataContext")
   member val MethodInvocationClass = runtimeAssembly.GetType("FSharpx.TypeProviders.Management.Runtime.MethodInvocation`1")
   member val WmiEventClass = runtimeAssembly.GetType("FSharpx.TypeProviders.Management.Runtime.WmiEvent`2")
   member val WmiCollectionClass = runtimeAssembly.GetType("FSharpx.TypeProviders.Management.Runtime.WmiCollection`1")
   member val RequiresWithin = runtimeAssembly.GetType("FSharpx.TypeProviders.Management.Runtime.RequiresWithin")
   member val DoesntRequireWithin = runtimeAssembly.GetType("FSharpx.TypeProviders.Management.Runtime.DoesntRequireWithin")
   member this.RuntimeAssembly = runtimeAssembly

// we used to have the following logic:
//    enums/in/out -> nonnull
//    props -> nullable 
module internal CimTypeHelpers =
   let cimToNetType = function
   | CimType.SInt8 -> typeof<sbyte>
   | CimType.UInt8 -> typeof<byte>
   | CimType.SInt16 -> typeof<int16>
   | CimType.UInt16 -> typeof<uint16>
   | CimType.SInt32 -> typeof<int32>
   | CimType.UInt32 -> typeof<uint32>
   | CimType.SInt64 -> typeof<int64>
   | CimType.UInt64 -> typeof<uint64>
   | CimType.Real32 -> typeof<float32>
   | CimType.Real64 -> typeof<float>
   | CimType.Boolean -> typeof<bool>
   | CimType.String -> typeof<string>
   | CimType.DateTime -> typeof<DateTimeOffset>
   | CimType.Char16 -> typeof<char>
   | CimType.Reference -> typeof<string> // Note: should be superceded by reference type where possible
   | CimType.Object -> typeof<obj>
   | _ -> failwith "Unknown cimtype" 

   let outputInfo(wmiProp:PropertyData,r:RuntimeInfo,d:System.Collections.Generic.Dictionary<_,_>,nullable) =
       let tyName = wmiProp.Qualifiers.["CIMTYPE"].Value :?> string
       match wmiProp.Type with
       | CimType.Reference when tyName.StartsWith("ref:") ->
           if wmiProp.IsArray then
               failwith (sprintf "Unexpected type: %A array" wmiProp.Qualifiers.["cim_type"].Value)
           let ty = d.[tyName.Substring(4)]
           r.GetManagementObjectFromReference.ReturnType.GetGenericTypeDefinition().MakeGenericType([|ty|]), 
               fun e -> Expr.Call(r.GetManagementObjectFromReference, [Expr.Coerce(e, typeof<string>)])
       | CimType.Object -> 
           let ty =
               if tyName.StartsWith("object:") then 
                   d.[tyName.Substring(7)]
               else typeof<obj>
           if wmiProp.IsArray then ty.MakeArrayType(), id
           else ty, id
       | ty -> 
           let conv = 
               match ty with
               | CimType.DateTime -> fun e -> Quotations.Expr.Call(r.TranslateDateTime, [e])
               | _ -> id
           let reprTy = cimToNetType ty
           (if wmiProp.IsArray then 
               reprTy.MakeArrayType()
           elif nullable && reprTy.IsValueType then
               typedefof<Nullable<_>>.MakeGenericType(reprTy)
           else
               reprTy), conv
                   
   let inputInfo(wmiProp:PropertyData, r:RuntimeInfo,d:System.Collections.Generic.Dictionary<_,_>) =
       let tyName = wmiProp.Qualifiers.["CIMTYPE"].Value :?> string
       match wmiProp.Type with
       | CimType.Reference when tyName.StartsWith("ref:") ->
           if wmiProp.IsArray then
               failwith (sprintf "Unexpected type: %A array" wmiProp.Qualifiers.["cim_type"].Value)
           let ty = d.[tyName.Substring(4)]
           ty, fun e -> Expr.Call(r.GetReferenceToManagementObject, [e])
       | CimType.Object -> 
           let ty =
               if tyName.StartsWith("object:") then 
                   d.[tyName.Substring(7)]
               else typeof<obj>
           if wmiProp.IsArray then ty.MakeArrayType(), id
           else ty, id
       | ty -> 
           cimToNetType ty, id

[<TypeProvider>]
type public WmiExtender(config : TypeProviderConfig) as this = 

   inherit TypeProviderForNamespaces()

   let runtimeInfo = RuntimeInfo(config)
   let thisAssembly = runtimeInfo.RuntimeAssembly
   let rootNamespace = "FSharpx.TypeProviders.Management"

   let getWmiClasses locale machineName nmspace = 
       let options = new ConnectionOptions()
       options.Locale <- locale
       
       let scope = ManagementScope(sprintf @"\\%s\%s" machineName nmspace,options)       
       let rootClass = new ManagementClass(scope, ManagementPath(""), ObjectGetOptions()) 
       // UseAmendedQualifiers ensures descriptions, values, value maps etc are provided
       let options = EnumerationOptions(EnumerateDeep=true , UseAmendedQualifiers=true)
       
       // The use of GetSubclasses is expensive, giving a 7second startup time for the local machine,
       // but we don't yet know a better way.
       seq { for c in rootClass.GetSubclasses(options) -> (c :?> ManagementClass) }
       
   let tryFindQualifier (wmiObj: ManagementBaseObject) qualifierName = 
       query { for q in seq { for x in wmiObj.Qualifiers -> x } do 
               where (q.Name = qualifierName)
               select (Some q)
               exactlyOneOrDefault }

   let tryFindPropQualifier (wmiProp: PropertyData) qualifierName = 
       query { for q in seq { for x in wmiProp.Qualifiers -> x }do 
               where (q.Name = qualifierName)
               select (Some q)
               exactlyOneOrDefault }

   let getPropXmlDoc (wmiProp: PropertyData) = 
       match tryFindPropQualifier wmiProp "Description" with 
       | Some descQualifier -> 
             match descQualifier.Value with 
             | :? string as s -> s
             | _ -> null
       | _ -> null

   let isEvent (wmiClass : ManagementClass) =
       wmiClass.Derivation |> Seq.cast<string> |> Seq.exists ((=) "__Event")

   let extractEnum className d (wmiProp:PropertyData) =
       let propName = wmiProp.Name
       let reprTy,converter = CimTypeHelpers.outputInfo(wmiProp, runtimeInfo, d, false)

       let baseTy = if wmiProp.IsArray then reprTy.GetElementType() else typeof<obj>
       let enumTy = ProvidedTypeDefinition(propName + "Values", baseType = Some baseTy, HideObjectMethods=true)

       let getValValueReprExpr (valValue : string) = 
           let elementRepr = if wmiProp.IsArray then reprTy.GetElementType() else reprTy
           let v = Convert.ChangeType(valValue, elementRepr) 
           Expr.Coerce( Expr.Value(v, elementRepr), typeof<obj>)

       let propTy = if wmiProp.IsArray then enumTy.MakeArrayType() else upcast enumTy

       let valNames = (wmiProp.Qualifiers.["Values"].Value :?> string[])
       let valueMap = 
           match tryFindPropQualifier wmiProp "ValueMap" with
           | Some valueMapQualifier -> 
               // Note: this type test fails for Win32_RegistryAction.Root, whose values are an int[] (even though the property type is int16) !?
               match valueMapQualifier.Value with
               | :? (string[]) as strArr -> strArr
               | :? System.Array as arr -> Array.init arr.Length (arr.GetValue >> string)
               | _ -> failwith "tryFindPropQualifier returned an unexpected value"
           | None -> [| for i in 0 .. valNames.Length - 1 -> string i |]

       // Win32_OperatingSystem.OperatingSystemSKU is anomalous, and has a single extra entry in valueMap...
       let valueMap =
           if valueMap.Length > valNames.Length then
               valueMap.[0 .. valNames.Length-1]
           else
               valueMap

       enumTy.AddXmlDocDelayed(fun () -> 
           sprintf "The set of valid values for the '%s' property of the '%s' class" propName className)
                     
       let namesAndVals =
           Array.zip valNames valueMap
           |> Array.filter (fun (_,v) -> v <> "..") // some properties have the non-numeric string ".." for a value, indicating that all remaining values are reserved

       try
           for valName, valValue in namesAndVals do 
               let valValueRepr = getValValueReprExpr valValue
               let isProp = ProvidedProperty("Is_"+valName.Replace(" ","_"),typeof<bool>,GetterCode=(fun args -> <@@ (%%(Expr.Coerce(args.[0], typeof<obj>))) = (%% valValueRepr : obj) @@>))
               isProp.AddXmlDocDelayed (fun () -> sprintf "Check if the property '%s' of class '%s' is value '%s', of underlying value '%s'" propName className valName valValue)
               enumTy.AddMember isProp
                     
               let valProp = ProvidedProperty(valName, enumTy, GetterCode=(fun _args -> valValueRepr), IsStatic=true)
               valProp.AddXmlDocDelayed (fun () -> sprintf "The value '%s', with underlying value '%s'" valName valValue)
               enumTy.AddMember valProp
       with _ -> () //there are some oddities which break this logic...
       enumTy, propTy, converter
       

   let particularClassType (d:Collections.Generic.Dictionary<_,_>) (companionD:Collections.Generic.Dictionary<_,ProvidedTypeDefinition>) (className, wmiClass: ManagementClass) =
       let baseTy = 
           lazy
               match wmiClass.Derivation |> Seq.cast<string> |> Seq.toList with
               | h::_ -> d.[h]
               | _ -> typeof<obj>
       let t = ProvidedTypeDefinition(className,Some typeof<obj>, HideObjectMethods=true)
       t.SetBaseTypeDelayed(lazy Some(baseTy.Value))
       
       d.[className] <- t :> Type
       t.AddXmlDocDelayed(fun () -> 
           match tryFindQualifier wmiClass "Description" with
           | Some q -> 
               match q.Value with 
               | :? string  as s -> s
               | _ -> null
           | None -> sprintf "Represents objects for the management class '%s'" wmiClass.ClassPath.ClassName)
       t.AddMembersDelayed (fun () -> 
           [ for wmiProp in wmiClass.Properties do
               let propName = wmiProp.Name

               // define only properties originating with this class, or where the type differs from the defintion at its origin
               if wmiProp.Origin = wmiClass.ClassPath.ClassName || wmiProp.Qualifiers.["CIMType"].Value <> (new ManagementClass(wmiClass.Scope, ManagementPath(wmiProp.Origin), ObjectGetOptions())).Properties.[wmiProp.Name].Qualifiers.["CIMType"].Value then
                   
                   let enTy,reprTy,converter =
                   
                       match tryFindPropQualifier wmiProp "Values" with
                       | Some _ ->
                           let e, pty, conv = extractEnum className d wmiProp
                           Some(e),pty,conv
                       | None -> 
                           let pty, conv = CimTypeHelpers.outputInfo(wmiProp, runtimeInfo, d, true)
                           None,pty,conv

                   match enTy with
                   | Some e -> yield (e:>MemberInfo)
                   | None -> ()

                   let p = ProvidedProperty(propName,reprTy,IsStatic=false, 
                                           GetterCode = (fun args -> converter (Expr.Call(runtimeInfo.GetManagementProp, [ Expr.Coerce(args.[0], typeof<ManagementBaseObject>); Expr.Value propName ]))) )
                   p.AddXmlDocDelayed(fun () -> getPropXmlDoc wmiProp)
                   yield (p :> MemberInfo) 

             // TODO: Consider whether to add an optional argument to static methods which allows a different scope to be specified
             //       For instance methods, a different scope can be used at runtime based on the constructor that was used...
             for wmiMeth in wmiClass.Methods do
               if wmiMeth.Origin = wmiClass.ClassPath.ClassName then
                   let isStatic = 
                       query { for q in seq { for x in wmiMeth.Qualifiers -> x } do 
                               where (q.Name = "Static")
                               select (q.Value :?> bool)
                               headOrDefault }
                   let methName = wmiMeth.Name
                   let parameters = 
                       [ match wmiMeth.InParameters with 
                           | null -> ()
                           | ps -> for inArg in ps.Properties -> 
                                   let reprTy, converter = CimTypeHelpers.inputInfo(inArg, runtimeInfo, d)
                                   let p = ProvidedParameter(inArg.Name, reprTy)
                                   p, converter ]
                   let inParamNamesExpr = Expr.NewArray(typeof<string>, [ for (p,_) in parameters -> Expr.Value p.Name ])

                   let outParameters =
                       if wmiMeth.OutParameters = null then []
                       else
                           [ for p in wmiMeth.OutParameters.Properties do
                               if p.Name <> "ReturnValue" then
                                   let reprTy, converter = CimTypeHelpers.outputInfo(p, runtimeInfo, d, false)
                                   yield ProvidedParameter(p.Name, reprTy.MakeByRefType(), true), converter ]
              
                   // TODO: apply retConv to return value?
                   let retTy, _retConv = 
                       if wmiMeth.OutParameters = null then typeof<System.Void>, id
                       else
                           let p = wmiMeth.OutParameters.Properties.["ReturnValue"] 
                           CimTypeHelpers.outputInfo(p,runtimeInfo,d,false)
                   let p = ProvidedMethod(methName,parameters @ outParameters |> List.map fst,retTy, 
                                           InvokeCode = (fun args -> 
                                               let argsWithoutObj = args |> Seq.skip 1
                                               let inArgs = argsWithoutObj |> Seq.take (parameters.Length)
                                               let outArgs = argsWithoutObj |> Seq.skip (parameters.Length)
                                               let inParamValuesExpr = Expr.NewArray(typeof<obj>, [ for a in inArgs -> Expr.Coerce(a, typeof<obj>) ])
                                               let call = 
                                                   if isStatic then
                                                       let ctx = Expr.PropertyGet(args.[0], args.[0].Type.GetProperty("Context"))
                                                       let scope = Expr.PropertyGet(ctx, ctx.Type.GetProperty("Scope"))
                                                       Expr.Call(runtimeInfo.InvokeStaticManagementMethod, [scope; Expr.Value wmiClass.ClassPath.Path; Expr.Value methName; inParamNamesExpr; inParamValuesExpr])                                                         
                                                   else
                                                       Expr.Call(runtimeInfo.InvokeInstanceManagementMethod, [ Expr.Coerce(args.[0], typeof<ManagementBaseObject>); Expr.Value methName; inParamNamesExpr; inParamValuesExpr])
                                               let callVar = Var("call", typeof<obj*ManagementBaseObject>)
                                               let outVars = <@ snd (%%Expr.Var(callVar) : obj*ManagementBaseObject) @>
                                               let body = 
                                                   outArgs
                                                   |> Seq.zip outParameters
                                                   // TODO: apply conv to output values?
                                                   |> Seq.fold (fun e ((p,_conv), v) ->
                                                       let ty = v.Type.GetElementType()
                                                       let name = p.Name
                                                       Expr.Sequential(Expr.Call(runtimeInfo.Set.MakeGenericMethod(ty), [v; Expr.Coerce(<@@ (%outVars).[name] @@>, ty)]), e))
                                                       (if retTy = typeof<System.Void> then <@@ () @@>
                                                           else Expr.Coerce(<@@ fst (%%Expr.Var( callVar) : obj*ManagementBaseObject) @@>, retTy))
                                               Expr.Let(callVar, call, body)))

                   p.AddXmlDocDelayed(fun () -> 
                       try wmiMeth.Qualifiers.["Description"].Value :?> string 
                       with _ -> null)
                   // TODO: verify safety of adding member to another type from within this delayed block
                   if isStatic then
                       companionD.[className].AddMember(p)
                   else
                       yield (p :> MemberInfo) 
            ])

       t 
          
   let fsharpPropNameForWmiClass (wmiClass: ManagementClass) = wmiClass.ToString().Split(':').[1]
   let fsharpClassNameForWmiClass wmiClass = fsharpPropNameForWmiClass wmiClass 

   let classesType wmiClasses =
       let t = ProvidedTypeDefinition("ServiceTypes",Some typeof<obj>)
       let d = System.Collections.Generic.Dictionary(System.StringComparer.InvariantCultureIgnoreCase)
       let companionD = System.Collections.Generic.Dictionary(System.StringComparer.InvariantCultureIgnoreCase)
       t.AddXmlDocDelayed(fun () -> "Contains the types for the Management Information schema")
       t.AddMembersDelayed(fun () -> 
           [ for wmiClass in wmiClasses do 
                 let className = fsharpClassNameForWmiClass wmiClass
                 yield particularClassType d companionD (className,wmiClass) 
           ])
       t, companionD

   let machineType (locale,typeNameForMachine, machineName, nmspace) =
       let t = ProvidedTypeDefinition(thisAssembly, rootNamespace, typeNameForMachine, Some runtimeInfo.DataContextClass)
       let wmiClasses = getWmiClasses locale machineName nmspace |> Seq.cache
       let theClassesType, companionD = classesType wmiClasses 

       t.AddMembersDelayed(fun () ->
           [ for wmiClass in  wmiClasses do 
               let allInstancesPropName = fsharpPropNameForWmiClass wmiClass
               let className = fsharpClassNameForWmiClass wmiClass
               let classType = theClassesType.GetNestedType className

               // don't expose instances/events for system classes
               if not (wmiClass.ClassPath.ClassName.StartsWith("__")) then

                   let classes = wmiClass.Derivation |> Seq.cast<string> |> set
                   
                   let isExtrinsicEvt = classes |> Set.contains "__ExtrinsicEvent"

                   if isExtrinsicEvt then
                       let extrEventTy = runtimeInfo.WmiEventClass.MakeGenericType(classType, runtimeInfo.DoesntRequireWithin)

                       let meth = runtimeInfo.ExtrinsicEventMethod.MakeGenericMethod(typeof<obj>)
                       let delTy = meth.GetParameters().[2].ParameterType
                       let arg = Var("arg", delTy.GetGenericArguments().[0])
                       let p = ProvidedProperty(allInstancesPropName, extrEventTy, GetterCode = (fun args -> Expr.Call (meth, [Expr.Coerce(args.[0], runtimeInfo.DataContextClass); Expr.Value wmiClass.ClassPath.ClassName; Expr.NewDelegate(delTy, [arg], Expr.Coerce(Expr.Var arg, typeof<obj>))]))) 
                       
                       p.AddXmlDocDelayed(fun () -> 
                           try wmiClass.Qualifiers.["Description"].Value :?> string 
                           with _ -> sprintf "WMI event '%s'" wmiClass.ClassPath.ClassName)

                       yield p :> MemberInfo

                   // suppress intrinsic events (since they show up as members on each class)
                   elif not (isEvent wmiClass) then
                       let seqOfClassType = runtimeInfo.WmiCollectionClass.MakeGenericType(classType)

                       let meth = runtimeInfo.GetClassInstancesByName.MakeGenericMethod(typeof<obj>)
                       let delTy = meth.GetParameters().[2].ParameterType                       
                       let arg = Var("arg", delTy.GetGenericArguments().[0])
                                               
                       let collTy = ProvidedTypeDefinition(className + "Collection", Some seqOfClassType)
                       companionD.[className] <- collTy
                       let collGetCtx = runtimeInfo.WmiCollectionClass.MakeGenericType(typeof<obj>).GetProperty("Context")
                       theClassesType.AddMember(collTy) 

                       for evName, ty in ["__InstanceCreation",runtimeInfo.InstanceOperationClass
                                          "__InstanceOperation",runtimeInfo.InstanceOperationClass
                                          "__InstanceDeletion",runtimeInfo.InstanceOperationClass
                                          "__InstanceModification",runtimeInfo.InstanceModificationClass
                                          "__MethodInvocation",runtimeInfo.MethodInvocationClass] do
                           let fullTy = runtimeInfo.WmiEventClass.MakeGenericType(ty.MakeGenericType(classType), runtimeInfo.RequiresWithin)
                           let meth = runtimeInfo.IntrinsicEventMethod.MakeGenericMethod(ty.MakeGenericType(typeof<obj>))
                           let ctor = ty.MakeGenericType(typeof<obj>).GetConstructors().[0]
                           let funcTy = typedefof<System.Func<_,_>>.MakeGenericType(typeof<ManagementBaseObject>, ty.MakeGenericType(typeof<obj>))
                           let var = Quotations.Var("arg", typeof<ManagementBaseObject>)
                           collTy.AddMember(ProvidedProperty(evName.Substring(2)+"Event", fullTy, GetterCode = fun args -> 
                               Quotations.Expr.Call(meth, [Quotations.Expr.PropertyGet(args.[0], collGetCtx); Quotations.Expr.Value (evName+"Event"); Quotations.Expr.Value className; Quotations.Expr.NewDelegate(funcTy, [var], Quotations.Expr.NewObject(ctor, [Quotations.Expr.Var var]))])))


                       let p = ProvidedProperty(allInstancesPropName, collTy, GetterCode = (fun args -> Expr.Call (meth, [ Expr.Coerce(args.[0], runtimeInfo.DataContextClass); Expr.Value allInstancesPropName; Expr.NewDelegate(delTy, [arg], Expr.Coerce(Expr.Var arg, typeof<obj>))] ))) 
                       p.AddXmlDocDelayed(fun () -> 
                           try wmiClass.Qualifiers.["Description"].Value :?> string 
                           with _ -> sprintf "All objects of the management class '%s'" wmiClass.ClassPath.ClassName)

                       yield  p :> MemberInfo

             let m1 = ProvidedMethod("GetDataContext",[ ], t, IsStaticMethod=true, 
                                     InvokeCode = (fun _args -> 
                                                       let fullMachineName = sprintf @"\\%s\%s" machineName nmspace
                                                       Expr.NewObject(runtimeInfo.DataContextClass.GetConstructors().[0], [ <@@ new ManagementScope(fullMachineName) @@> ])))
             let m2 = ProvidedMethod("GetDataContext",[ ProvidedParameter("machineName",typeof<string>) ], t, IsStaticMethod=true, 
                                   InvokeCode = (fun args -> Expr.NewObject(runtimeInfo.DataContextClass.GetConstructors().[0], [ <@@ new ManagementScope(sprintf @"\\%s\%s" ((%%args.[0]) : string) nmspace) @@> ])))
             yield m1 :> MemberInfo
             yield m2 :> MemberInfo
             yield theClassesType :> MemberInfo
           ])
       t  

   let remoteType = 
       let t = ProvidedTypeDefinition(thisAssembly, rootNamespace, "WmiProvider", Some typeof<obj>)
       t.DefineStaticParameters( [ ProvidedStaticParameter("MachineName", typeof<string>); 
                                   ProvidedStaticParameter("Namespace", typeof<string>, @"root\cimv2")
                                   ProvidedStaticParameter("Locale", typeof<string>, "MS_409") ], (fun typeName staticArgs -> 
              match staticArgs with 
              | [| :? string as machineName; :? string as nmSpace; :? string as locale |] -> 
                    machineType (locale, typeName, machineName, nmSpace)
              | _ -> failwith (sprintf "unexpected shape for static arguments: %A" staticArgs)))
       let helpText = 
          """<summary>Accesses management information using the schema of the indicated machine. Use 'localhost' for the local machine.</summary>
             <param name='MachineName'>The name of the remote machine to connect to. Use 'localhost' for the local machine.</param>
             <param name='Namespace'>The root WMI namespace (e.g. root\cimv2)</param>
             <param name='Locale'>The used language for WMI (default is 'MS_409' which is en-US)</param>"""
       t.AddXmlDoc(helpText)
       t

   do this.AddNamespace(rootNamespace, [remoteType])

[<assembly:TypeProviderAssembly>]
do()