namespace FSharpx.TypeProviders

// originally from http://d.hatena.ne.jp/otf/20110921/1316554520
// Starting to improving this

open System
open System.Collections.Generic
open System.Linq.Expressions
open System.Globalization
open System.Reflection
open System.Diagnostics
open Microsoft.FSharp.Core.CompilerServices
open Internal.Utilities.TypeProvider.Emit
open System.IO
open System.Text.RegularExpressions

[<assembly: TypeProviderAssembly>]
do()

type ProvidedType(name, ns, assembly) =
  inherit Type()
  let mutable IsErasedFlag = 1073741824
  let mutable attributes = (TypeAttributes.Public ||| TypeAttributes.Sealed ||| enum IsErasedFlag) 
  override x.GetConstructorImpl(flags: BindingFlags, binder: Binder, cc: CallingConventions, paramTypes: Type[], modifiers: ParameterModifier[]) =  null
  override x.GetMethodImpl(name: string, flags: BindingFlags, binder: Binder, cc: CallingConventions, paramTypes: Type[], modifiers: ParameterModifier[]) = null
  override x.GetPropertyImpl(name: string, flags: BindingFlags, binder: Binder, t: Type, paramTypes: Type[], modifiers: ParameterModifier[]) = null
  override x.GetAttributeFlagsImpl() = attributes
  override x.IsArrayImpl() = false
  override x.IsByRefImpl() = false
  override x.IsPointerImpl() = false
  override x.IsPrimitiveImpl() = false
  override x.IsCOMObjectImpl() = false
  override x.HasElementTypeImpl() = false
  override x.GetMembers(flags: BindingFlags) = [||]
  override x.GUID = Guid.Empty
  override x.InvokeMember(name: string, flags: BindingFlags, binder: Binder, instance: obj, parameters: obj[], modifiers: ParameterModifier[], culture: CultureInfo, names: string[]) = null
  override x.Module = null: Module
  override x.Assembly = assembly
  override x.FullName = sprintf "%s.%s" ns name
  override x.Namespace = ns
  override x.AssemblyQualifiedName = sprintf "%s.%s, %s" ns name assembly.FullName
  override x.BaseType = null
  override x.GetConstructors(flags: BindingFlags) = [||]
  override x.GetMethods(flags: BindingFlags) = [||]
  override x.GetField(name: string, flags: BindingFlags) = null
  override x.GetFields(flags: BindingFlags) = [||]
  override x.GetInterface(name: string, inherited: Boolean) =  null
  override x.GetInterfaces() = [||]
  override x.GetEvent(name: string, flags: BindingFlags) = null
  override x.GetEvents(flags: BindingFlags) =  [||]
  override x.GetProperties(flags: BindingFlags) = [||]
  override x.GetNestedTypes(flags: BindingFlags) = [||]
  override x.GetNestedType(name: string, flags: BindingFlags) = null
  override x.GetElementType() = null
  override x.UnderlyingSystemType = null
  override x.Name = name
  override x.GetCustomAttributes(inherited: Boolean) = [||]
  override x.GetCustomAttributes(t: Type, inherited: Boolean) = [||]
  override x.GetCustomAttributesData() = new ResizeArray<_>() :> IList<_>
  override x.IsDefined(t: Type, inherited: Boolean) = false
  override x.IsSecurityCritical with get() = false
  override x.IsSecuritySafeCritical with get() = false
  override x.IsSecurityTransparent with get() = false
  override x.GetDefaultMembers() = [||]
  override x.GetEnumValues() = [||] :> Array
  member x.IsErased
    with get() = (attributes &&& enum IsErasedFlag) <> TypeAttributes.NotPublic
    and set value =
      if value then
        attributes <- attributes ||| (enum IsErasedFlag)
      else
        attributes <- attributes &&& ~~~ (enum IsErasedFlag)

[<TypeProvider>]
type TypeProviderRoot() =
  let theAssembly = typeof<TypeProviderRoot>.Assembly
  let invalidate = new Event<EventHandler,EventArgs>()
  let ns = "FSharpx.TypeProviders"
  let regexType = ProvidedType ("Regex" , ns, theAssembly, IsErased = true)

  interface ITypeProvider with
    [<CLIEvent>]
    member x.Invalidate = invalidate.Publish
    member x.GetNamespaces() = [| x |] 
    member x.GetStaticParameters(typeWithoutArguments: Type) = 
      [| new ProvidedParameter("pattern", typeof<string>) |]
    member x.ApplyStaticArguments(typeWithoutArguments: Type, typeNameWithArguments: string,  staticArguments: obj[]) = 
      new Regex(staticArguments.[0] :?> string) |> ignore
      ProvidedType (typeNameWithArguments , ns, theAssembly, IsErased = true) :> Type
    member x.GetInvokerExpression(syntheticMethodBase: MethodBase, parameters: ParameterExpression[]) = null: Expression 
    member x.Dispose() = ()
  interface IProvidedNamespace with
    member x.NamespaceName with get() = ns
    member x.GetNestedNamespaces() = [||]
    member x.GetTypes() = [| regexType |]
    member x.ResolveTypeName(typeName: string) = regexType:>Type