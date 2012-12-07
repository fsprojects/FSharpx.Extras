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


open System.Collections.Generic
open System.Reflection
open System.Text
open Microsoft.FSharp.Core.CompilerServices
open FSharpx.Strings

let generate typeProviderConstructor (resolutionFolder : string) (args: string array) =
    let cfg = new TypeProviderConfig(fun _ -> false)
    cfg.GetType().GetProperty("ResolutionFolder").GetSetMethod(nonPublic = true).Invoke(cfg, [| box resolutionFolder |]) |> ignore

    let typeProviderForNamespaces = new TypeProviderForNamespaces() 
    let genericTypeDefinition : ProvidedTypeDefinition = 
        typeProviderConstructor typeProviderForNamespaces cfg    

    let typeName = genericTypeDefinition.Name + (args |> Seq.map (fun s -> ",\"" + s + "\"") |> Seq.reduce (+))
    let concreteTypeDefinition = genericTypeDefinition.MakeParametricType(typeName, args |> Array.map box)

    concreteTypeDefinition

let prettyPrint t =        

    let pending = new Queue<_>()
    let visited = new HashSet<_>()

    let rec toString (t: Type) =
        match t with
        | t when t = typeof<obj> -> "obj"
        | t when t = typeof<int> -> "int"
        | t when t = typeof<int64> -> "int64"
        | t when t = typeof<float> -> "float"
        | t when t = typeof<float32> -> "float32"
        | t when t = typeof<string> -> "string"
        | t when t = typeof<Void> -> "()"
        | t when t = typeof<unit> -> "()"
        | :? ProvidedTypeDefinition as t ->
            if visited.Add t then
                pending.Enqueue t
            t.Name.Split([| ',' |]).[0]
        | t when t.IsGenericType ->            
            let args = 
                t.GetGenericArguments() 
                |> Seq.map toString
                |> separatedBy ", "
            let name = 
                if t.Name.Contains("[") then  // units of measure
                    toString t.UnderlyingSystemType
                else 
                    t.Name
            name.Split([| '`' |]).[0] + "<" + args + ">"
        | t -> t.Name

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
            print <| "new : " + (toSignature <| cons.GetParameters()) + " -> " + (toString memberInfo.DeclaringType)
        | :? ProvidedLiteralField as field -> 
            print <| "val " + field.Name + ": " + (toString field.FieldType) + " - " + (field.GetRawConstantValue().ToString())
        | :? ProvidedProperty as prop -> 
            print <| "member " + prop.Name + ": " + (toString prop.PropertyType) + " with " + (if prop.CanRead && prop.CanWrite then "get, set" else if prop.CanRead then "get" else "set")
        | :? ProvidedMethod as m ->
            if m.Attributes &&& MethodAttributes.SpecialName <> MethodAttributes.SpecialName then
                print <| "member " + m.Name + ": " + (toSignature <| m.GetParameters()) + " -> " + (toString m.ReturnType)
        | :? ProvidedTypeDefinition as t -> if visited.Add t then pending.Enqueue t  
        | _ -> ()

    pending.Enqueue t
    visited.Add t |> ignore

    while pending.Count <> 0 do
        let t = pending.Dequeue()
        print (toString t)
        println()
        t.GetMembers() |> Seq.iter printMember
        println()
    
    sb.ToString()
