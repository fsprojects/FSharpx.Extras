/// Starting to implement some helpers on top of ProvidedTypes API
module internal FSharpx.TypeProviders.Helper
open System
open System.IO
open FSharpx.Strings
open Samples.FSharp.ProvidedTypes

let findConfigFile resolutionFolder configFileName =
    if Path.IsPathRooted configFileName then 
        configFileName 
    else 
        Path.Combine(resolutionFolder, configFileName)

let erasedType<'a> assemblyName rootNamespace typeName = 
    ProvidedTypeDefinition(assemblyName, rootNamespace, typeName, Some(typeof<'a>))

let runtimeType<'a> typeName = ProvidedTypeDefinition(niceName typeName, Some typeof<'a>)

let seqType ty = typedefof<seq<_>>.MakeGenericType[| ty |]
let listType ty = typedefof<list<_>>.MakeGenericType[| ty |]
let optionType ty = typedefof<option<_>>.MakeGenericType[| ty |]

/// Implements invalidation of schema when the file changes
let watchForChanges (ownerType:TypeProviderForNamespaces) (fileName:string) = 
    if fileName.StartsWith("http", System.StringComparison.InvariantCultureIgnoreCase) then () else

    let path = Path.GetDirectoryName(fileName)
    let name = Path.GetFileName(fileName)
    let watcher = new FileSystemWatcher(Filter = name, Path = path)
    watcher.Changed.Add(fun _ -> ownerType.Invalidate())
    watcher.EnableRaisingEvents <- true

// Get the assembly and namespace used to house the provided types
let thisAssembly = System.Reflection.Assembly.GetExecutingAssembly()
let rootNamespace = "FSharpx"
let missingValue = "@@@missingValue###"

open System.Collections.Generic
open System.Reflection
open System.Text
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Core.CompilerServices
open FSharpx.Strings

let generate providedTypeGenerator (resolutionFolder: string) args =
    let cfg = new TypeProviderConfig(fun _ -> false)
    cfg.GetType().GetProperty("ResolutionFolder").GetSetMethod(nonPublic = true).Invoke(cfg, [| box resolutionFolder |]) |> ignore
    cfg.GetType().GetProperty("ReferencedAssemblies").GetSetMethod(nonPublic = true).Invoke(cfg, [| box ([||]: string[]) |]) |> ignore

    let typeProviderForNamespaces = new TypeProviderForNamespaces() 
    let providedTypeDefinition  = 
        providedTypeGenerator typeProviderForNamespaces cfg :> ProvidedTypeDefinition

    match args with
    | [||] -> providedTypeDefinition
    | args ->
        let typeName = providedTypeDefinition.Name + (args |> Seq.map (fun s -> ",\"" + s.ToString() + "\"") |> Seq.reduce (+))
        providedTypeDefinition.MakeParametricType(typeName, args |> Array.map box)

let generateWithRuntimeAssembly typeProviderConstructor providedTypeGeneratorSelector (runtimeAssembly: string) resolutionFolder args =
    let providedTypeGenerator (cfg: TypeProviderConfig) = seq {
        cfg.GetType().GetProperty("RuntimeAssembly").GetSetMethod(nonPublic = true).Invoke(cfg, [| box runtimeAssembly |]) |> ignore
        let tp = typeProviderConstructor cfg :> ITypeProvider
        for ns in tp.GetNamespaces() do
            for providedType in ns.GetTypes() do
                yield providedType :?> ProvidedTypeDefinition }
    generate (fun _ cfg -> providedTypeGeneratorSelector (providedTypeGenerator cfg)) resolutionFolder args

let private innerPrettyPrint (maxDepth: int option) exclude (t: ProvidedTypeDefinition) =        

    let ns = 
        [ t.Namespace
          "Microsoft.FSharp.Core"
          "Microsoft.FSharp.Core.Operators"
          "Microsoft.FSharp.Collections"
          "Microsoft.FSharp.Control"
          "Microsoft.FSharp.Text" ]
        |> Set.ofSeq

    let pending = new Queue<_>()
    let visited = new HashSet<_>()

    let add t =
        if not (exclude t) && visited.Add t then
            pending.Enqueue t

    let fullName (t: Type) =
        let fullName = t.FullName
        if fullName.StartsWith "FSI_" then
            fullName.Substring(fullName.IndexOf('.') + 1)
        else
            fullName

    let rec toString (t: Type) =
        match t with
        | t when t = null -> "<NULL>" // happens in the Freebase provider
        | t when t = typeof<bool> -> "bool"
        | t when t = typeof<obj> -> "obj"
        | t when t = typeof<int> -> "int"
        | t when t = typeof<int64> -> "int64"
        | t when t = typeof<float> -> "float"
        | t when t = typeof<float32> -> "float32"
        | t when t = typeof<string> -> "string"
        | t when t = typeof<Void> -> "()"
        | t when t = typeof<unit> -> "()"
        | t when t.IsArray -> (t.GetElementType() |> toString) + "[]"
        | :? ProvidedTypeDefinition as t ->
            add t
            t.Name.Split([| ',' |]).[0]
        | t when t.IsGenericType ->            
            let hasUnitOfMeasure = t.Name.Contains("[")
            let args =                 
                t.GetGenericArguments() 
                |> Seq.map (if hasUnitOfMeasure then (fun t -> t.Name) else toString)
                |> separatedBy ", "
            let name, reverse = 
                match t with
                | t when hasUnitOfMeasure -> toString t.UnderlyingSystemType, false
                | t when t.GetGenericTypeDefinition() = typeof<int seq>.GetGenericTypeDefinition() -> "seq", true
                | t when t.GetGenericTypeDefinition() = typeof<int list>.GetGenericTypeDefinition() -> "list", true
                | t when t.GetGenericTypeDefinition() = typeof<int option>.GetGenericTypeDefinition() -> "option", true
                | t when t.GetGenericTypeDefinition() = typeof<int ref>.GetGenericTypeDefinition() -> "ref", true
                | t when ns.Contains t.Namespace -> t.Name, false
                | t -> fullName t, false
            let name = name.Split([| '`' |]).[0]
            if reverse then
                args + " " + name 
            else
                name + "<" + args + ">"
        | t when ns.Contains t.Namespace -> t.Name
        | t -> fullName t

    let toSignature (parameters: ParameterInfo[]) =
        if parameters.Length = 0 then
            "()"
        else
            parameters 
            |> Seq.map (fun p -> p.Name + ":" + (toString p.ParameterType))
            |> separatedBy " -> "

    let sb = StringBuilder ()
    let print (str: string) =
        sb.Append(str) |> ignore
    let println() =
        sb.AppendLine() |> ignore
                
    let printMember (memberInfo: MemberInfo) =        

        let print str =
            print "    "                
            print str
            println()

        match memberInfo with

        | :? ProvidedConstructor as cons -> 
            print <| "new : " + 
                     (toSignature <| cons.GetParameters()) + " -> " + 
                     (toString memberInfo.DeclaringType)

        | :? ProvidedLiteralField as field -> 
            print <| "val " + field.Name + ": " + 
                     (toString field.FieldType) + " - " + (field.GetRawConstantValue().ToString())

        | :? ProvidedProperty as prop -> 
            print <| (if prop.IsStatic then "static " else "") + "member " + 
                     prop.Name + ": " + (toString prop.PropertyType) + 
                     " with " + (if prop.CanRead && prop.CanWrite then "get, set" else if prop.CanRead then "get" else "set")            

        | :? ProvidedMethod as m ->
            if m.Attributes &&& MethodAttributes.SpecialName <> MethodAttributes.SpecialName then
                print <| (if m.IsStatic then "static " else "") + "member " + 
                m.Name + ": " + (toSignature <| m.GetParameters()) + 
                " -> " + (toString m.ReturnType)

        | :? ProvidedTypeDefinition as t -> add t

        | _ -> ()

    add t

    let currentDepth = ref 0

    let stop() =
        match maxDepth with
        | Some maxDepth -> !currentDepth > maxDepth
        | None -> false

    while pending.Count <> 0 && not (stop()) do
        let pendingForThisDepth = new Queue<_>(pending)
        pending.Clear()
        while pendingForThisDepth.Count <> 0 do
            let t = pendingForThisDepth.Dequeue()
            match t with
            | t when FSharpType.IsRecord t-> "record "
            | t when FSharpType.IsModule t -> "module "
            | t when t.IsValueType -> "struct "
            | t when t.IsClass -> "class "
            | t -> ""
            |> print
            print (toString t)
            if t.BaseType <> typeof<obj> then
                print " : "
                print (toString t.BaseType)
            println()
            t.GetMembers() |> Seq.iter printMember
            println()
        currentDepth := !currentDepth + 1
    
    sb.ToString()

let prettyPrint t = innerPrettyPrint None (fun _ -> false) t
let prettyPrintWithMaxDepth maxDepth t = innerPrettyPrint (Some maxDepth) (fun _ -> false) t
let prettyPrintWithMaxDepthAndExclusions maxDepth exclusions t = 
    let exclusions = Set.ofSeq exclusions
    innerPrettyPrint (Some maxDepth) (fun t -> exclusions.Contains t.Name) t
