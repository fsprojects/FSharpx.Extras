namespace FSharpx

//This uses some simple dynamic IL generation,
//and a clever technique desribed by Jon Skeet here:
//https://msmvps.com/blogs/jon_skeet/archive/2008/08/09/making-reflection-fly-and-exploring-delegates.aspx
module internal ReflectImpl =

    open System
    open System.Reflection
    open System.Reflection.Emit

    open Microsoft.FSharp.Reflection

    type Marker = class end

    let isOptionTy (t : Type) =
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ option>

    let isGetterMethod (declaringType : Type) (m : MethodInfo) =
        not m.IsStatic 
            && not m.IsGenericMethod 
            && m.GetParameters().Length = 0
            && m.DeclaringType.IsAssignableFrom declaringType

    let wantNonPublic bindingFlags =
        let bindingFlags = defaultArg bindingFlags BindingFlags.Public
        bindingFlags &&& BindingFlags.NonPublic = BindingFlags.NonPublic

    let pushParam (arg : int) (idx : int) (generator : ILGenerator) (paramType : Type) =
        generator.Emit(OpCodes.Ldarg, arg)
        generator.Emit(OpCodes.Ldc_I4, idx)
        generator.Emit(OpCodes.Ldelem_Ref)
        if paramType <> typeof<obj> then
            if paramType.IsValueType then
                generator.Emit(OpCodes.Unbox_Any, paramType)
            else
                generator.Emit(OpCodes.Castclass, paramType)

    let pushParams arg offset gen (paramTypes:seq<Type>) =
        paramTypes |> Seq.iteri (fun idx ty -> pushParam arg (idx + offset) gen ty)

        
    let preComputeConstructor (ctorInfo : ConstructorInfo) : obj [] -> obj =
        let meth = new DynamicMethod( "ctor", MethodAttributes.Static ||| MethodAttributes.Public,
                            CallingConventions.Standard, typeof<obj>, [| typeof<obj[]> |],
                            typeof<Marker>, true)

        let generator = meth.GetILGenerator()
        let paramTypes = ctorInfo.GetParameters() |> Seq.map (fun pi -> pi.ParameterType)

        pushParams 0 0 generator paramTypes
        generator.Emit(OpCodes.Newobj, ctorInfo)
        if ctorInfo.DeclaringType.IsValueType then generator.Emit(OpCodes.Box, ctorInfo.DeclaringType)
        generator.Emit(OpCodes.Ret)

        let dele = meth.CreateDelegate(typeof<Func<obj[],obj>>) :?> Func<obj[],obj>

        dele.Invoke

    let preComputeGetMethod (declaringType : Type) (getter : MethodInfo) : obj -> obj =
        assert isGetterMethod declaringType getter

        let meth = new DynamicMethod("methodEvaluator", MethodAttributes.Static ||| MethodAttributes.Public,
                                    CallingConventions.Standard, typeof<obj>, 
                                    [|typeof<obj>|], typeof<Marker>, true)
        let generator = meth.GetILGenerator()

        generator.Emit(OpCodes.Ldarg_0)
        generator.Emit(OpCodes.Unbox_Any, declaringType)
        generator.EmitCall(OpCodes.Call, getter, null)
        if getter.ReturnType.IsValueType then generator.Emit(OpCodes.Box, getter.ReturnType)

        generator.Emit OpCodes.Ret

        let dele = meth.CreateDelegate(typeof<Func<obj,obj>>) :?> Func<obj,obj>

        dele.Invoke


    // bundles multiple property getters in one dynamic method
    let preComputeGetMethods (declaringType : Type) (getters : MethodInfo []) : obj -> obj [] =
        assert (getters |> Array.forall (isGetterMethod declaringType))
        
        if getters.Length = 0 then (fun o -> [||]) else

        let meth = new DynamicMethod("propEvaluator", MethodAttributes.Static ||| MethodAttributes.Public,
                                            CallingConventions.Standard, typeof<obj []>, 
                                            [|typeof<obj>|], typeof<Marker>, true)
        let generator = meth.GetILGenerator()

        let unboxed = generator.DeclareLocal(declaringType)
        let arr = generator.DeclareLocal(typeof<obj []>)

        // unbox input
        generator.Emit(OpCodes.Ldarg_0)
        generator.Emit(OpCodes.Unbox_Any, declaringType)
        generator.Emit(OpCodes.Stloc, unboxed)

        // init obj array
        generator.Emit(OpCodes.Ldc_I4, getters.Length)
        generator.Emit(OpCodes.Newarr, typeof<obj>)
        generator.Emit(OpCodes.Stloc, arr)

        let computeGetter (idx : int) (m : MethodInfo) =
            // arr.[idx] <- p.GetValue(o) :> obj
            generator.Emit(OpCodes.Ldloc, arr)
            generator.Emit(OpCodes.Ldc_I4, idx)

            // call property getter
            generator.Emit(OpCodes.Ldloc, unboxed)
            generator.EmitCall(OpCodes.Call, m, null)
            if m.ReturnType.IsValueType then generator.Emit(OpCodes.Box, m.ReturnType)

            // store
            generator.Emit(OpCodes.Stelem_Ref)

        getters |> Seq.iteri computeGetter

        generator.Emit(OpCodes.Ldloc, arr)
        generator.Emit(OpCodes.Ret)

        let dele = meth.CreateDelegate(typeof<Func<obj,obj[]>>) :?> Func<obj,obj[]>

        dele.Invoke

    let preComputePropertyGetters bindingFlags (declaringType : Type) (props : PropertyInfo []) : obj -> obj [] =
        let getMethods = 
            props |> Array.map (fun p ->
                match p.GetGetMethod(wantNonPublic bindingFlags) with
                | null ->
                    sprintf "The type '%s' has private representation. You must specify BindingFlags.NonPublic to access private type representations." declaringType.Name
                    |> invalidArg "bindingFlags"
                | p -> p)

        preComputeGetMethods declaringType getMethods


    // F# type constructors
    
    let preComputeRecordContructor(recordType:Type,bindingFlags:BindingFlags option) : obj [] -> obj =
        assert FSharpType.IsRecord(recordType, ?bindingFlags=bindingFlags)
        let ctorInfo = FSharpValue.PreComputeRecordConstructorInfo(recordType,?bindingFlags=bindingFlags)
        preComputeConstructor ctorInfo
    
    let preComputeUnionConstructor(unionCaseInfo:UnionCaseInfo, bindingFlags:BindingFlags option) : obj [] -> obj =
        let methodInfo = FSharpValue.PreComputeUnionConstructorInfo(unionCaseInfo, ?bindingFlags=bindingFlags)
        let targetType = methodInfo.DeclaringType
        assert FSharpType.IsUnion(targetType, ?bindingFlags=bindingFlags)
        let meth = new DynamicMethod( "invoke", MethodAttributes.Static ||| MethodAttributes.Public,
                                        CallingConventions.Standard, typeof<obj>,
                                        [| typeof<obj[]> |], targetType, true )
        let paramTypes = methodInfo.GetParameters() |> Seq.map (fun pi -> pi.ParameterType)
        let generator = meth.GetILGenerator()

        let invoker =
            pushParams 0 0 generator paramTypes
            generator.Emit(OpCodes.Call, methodInfo)
            generator.Emit(OpCodes.Ret)
            meth.CreateDelegate(typeof<Func<obj[],obj>>) :?> Func<obj[],obj>

        invoker.Invoke

    let preComputeTupleConstructor(tuple : Type) : obj [] -> obj =
        let meth = new DynamicMethod( "ctor", MethodAttributes.Static ||| MethodAttributes.Public,
                                            CallingConventions.Standard, typeof<obj>, [| typeof<obj[]> |],
                                            tuple,
                                            true )
        let generator = meth.GetILGenerator()

        let rec traverse offset (tuple : Type) =
            let ctorInfo, nested = FSharpValue.PreComputeTupleConstructorInfo tuple
            let paramTypes = ctorInfo.GetParameters() |> Array.map (fun pi -> pi.ParameterType)

            match nested with
            | None -> pushParams 0 offset generator paramTypes
            | Some nested ->
                let n = paramTypes.Length
                pushParams 0 offset generator (Seq.take (n-1) paramTypes)
                traverse (offset + n - 1) nested

            generator.Emit(OpCodes.Newobj, ctorInfo)

        let invoker =
            traverse 0 tuple
            generator.Emit(OpCodes.Ret)
            meth.CreateDelegate(typeof<Func<obj[],obj>>) :?> Func<obj[],obj>
        invoker.Invoke

    // an implementation that curiously does not exist in Microsoft.FSharp.Reflection
    let preComputeExceptionConstructorInfo(exceptionType : Type, bindingFlags:BindingFlags option) : ConstructorInfo =
        assert FSharpType.IsExceptionRepresentation(exceptionType, ?bindingFlags = bindingFlags)
        let signature = FSharpType.GetExceptionFields(exceptionType, ?bindingFlags = bindingFlags) |> Array.map(fun f -> f.PropertyType)
        let ctors = 
            match bindingFlags with 
            | Some f -> exceptionType.GetConstructors (f ||| BindingFlags.Instance) 
            | None -> exceptionType.GetConstructors()

        match ctors |> Array.tryFind(fun ctor -> signature = (ctor.GetParameters() |> Array.map(fun p -> p.ParameterType))) with
        | None -> invalidArg "exnType" "The exception type is private. You must specify BindingFlags.NonPublic to access private type representations."
        | Some ctorInfo -> ctorInfo

    let preComputeExceptionConstructor(exceptionType : Type, bindingFlags:BindingFlags option) : obj [] -> obj =
        preComputeExceptionConstructorInfo(exceptionType, bindingFlags) |> preComputeConstructor


    // F# type readers

    let preComputeRecordReader (recordType:Type, bindingFlags:BindingFlags option) : obj -> obj [] =
        let fields = FSharpType.GetRecordFields(recordType, ?bindingFlags=bindingFlags)
        preComputePropertyGetters bindingFlags recordType fields

    let preComputeUnionReader(unionCase:UnionCaseInfo, bindingFlags:BindingFlags option) : obj -> obj [] =
        let fields = unionCase.GetFields()
        let declaringType = if fields.Length = 0 then unionCase.DeclaringType else fields.[0].DeclaringType
        preComputePropertyGetters bindingFlags declaringType fields

    let preComputeTupleReader (tuple : Type) : obj -> obj [] =
        let rec gather (tuple : Type) =
            let fields = tuple.GetProperties()  |> Seq.filter (fun p -> p.Name.StartsWith("Item") || p.Name = "Rest") 
                                                |> Seq.sortBy (fun p -> p.Name) //need: Items < 10 & "Item" < "Rest"
                                                |> Seq.toArray
            let partial = preComputePropertyGetters None tuple fields
            match tuple.GetProperty("Rest") with
            | null -> partial
            | rest ->
                let nested = gather rest.PropertyType
                fun (o:obj) ->
                    let values = partial o
                    Array.append values.[..values.Length-2] (nested values.[values.Length-1])

        if FSharpType.IsTuple tuple then
            gather tuple
        else
            invalidArg "tuple" <| sprintf "Type '%s' is not a tuple type." tuple.Name

    let preComputeExceptionReader(exnT : Type, bindingFlags:BindingFlags option) : obj -> obj [] =
        let fields = FSharpType.GetExceptionFields(exnT, ?bindingFlags = bindingFlags)
        preComputePropertyGetters bindingFlags exnT fields


    // fast union tag reader
    let preComputeUnionTagReader(union : Type, bindingFlags) : obj -> int =
        let nonPublic = wantNonPublic bindingFlags
        let bindingFlags = defaultArg bindingFlags (BindingFlags.Instance ||| BindingFlags.Public)

        if not <| FSharpType.IsUnion(union, bindingFlags) then
            invalidArg "union" <| sprintf "Type '%s' is not an F# union type." union.Name
        elif isOptionTy union then 
            (fun (obj:obj) -> match obj with null -> 0 | _ -> 1)
        else
            match union.GetProperty("Tag", bindingFlags) with
            | null ->
                match union.GetMethod("GetTag", bindingFlags, null, [| union |], null) with
                | null -> fun _ -> 0 // unary DU
                | meth -> 
                    let d = preComputeGetMethod union meth
                    fun (o : obj) -> d o :?> int
            | prop ->
                match prop.GetGetMethod nonPublic with
                | null ->
                    sprintf "The type '%s' has private representation. You must specify BindingFlags.NonPublic to access private type representations." union.Name
                    |> invalidArg "bindingFlags"
                | getter ->
                    let d = preComputeGetMethod union getter
                    fun (o : obj) -> d o :?> int


namespace FSharpx.Reflection

open System
open System.Reflection
open Microsoft.FSharp.Reflection

type FSharpValue =
    static member PreComputeRecordConstructorFast(recordType:Type,?bindingFlags:BindingFlags) =
        FSharpx.ReflectImpl.preComputeRecordContructor(recordType,bindingFlags)
    static member PreComputeUnionConstructorFast(unionCase:UnionCaseInfo, ?bindingFlags:BindingFlags) =
        FSharpx.ReflectImpl.preComputeUnionConstructor(unionCase,bindingFlags)
    static member PreComputeTupleConstructorFast(tupleType:Type) =
        FSharpx.ReflectImpl.preComputeTupleConstructor tupleType
    static member PreComputeExceptionConstructorFast(exceptionType:Type,?bindingFlags) =
        FSharpx.ReflectImpl.preComputeExceptionConstructor(exceptionType,bindingFlags)

    static member PreComputeRecordReaderFast(recordType:Type, ?bindingFlags:BindingFlags) : obj -> obj[] =
        FSharpx.ReflectImpl.preComputeRecordReader(recordType,bindingFlags)
    static member PreComputeUnionReaderFast(unionCase:UnionCaseInfo, ?bindingFlags:BindingFlags) : obj -> obj[] =
        FSharpx.ReflectImpl.preComputeUnionReader(unionCase, bindingFlags)
    static member PreComputeTupleReaderFast(tupleType:Type) : obj -> obj [] =
        FSharpx.ReflectImpl.preComputeTupleReader tupleType
    static member PreComputeExceptionReaderFast(exceptionType:Type,?bindingFlags) : obj -> obj [] =
        FSharpx.ReflectImpl.preComputeExceptionReader(exceptionType,bindingFlags)

    static member PreComputeExceptionConstructorInfo(exceptionType,?bindingFlags) : ConstructorInfo =
        FSharpx.ReflectImpl.preComputeExceptionConstructorInfo(exceptionType,bindingFlags)

    static member PreComputeUnionTagReaderFast(unionType:Type,?bindingFlags) : obj -> int =
        FSharpx.ReflectImpl.preComputeUnionTagReader(unionType,bindingFlags)