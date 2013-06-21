
namespace FSharpx

//This uses some simple dynamic IL generation,
//and a clever technique desribed by Jon Skeet here:
//https://msmvps.com/blogs/jon_skeet/archive/2008/08/09/making-reflection-fly-and-exploring-delegates.aspx
module internal ReflectImpl =

    open System
    open System.Reflection
    open Microsoft.FSharp.Reflection
    open System.Reflection.Emit

    let pushParams (generator:ILGenerator) (paramTypes:seq<Type>) =
        let castFromObject typ =
            if typ <> typeof<obj> then
                if typ.IsValueType then
                    generator.Emit(OpCodes.Unbox_Any, typ)
                else
                    generator.Emit(OpCodes.Castclass, typ)

        paramTypes
        |> Seq.iteri (fun i paramType -> 
                        generator.Emit(OpCodes.Ldarg, 0)
                        generator.Emit(OpCodes.Ldc_I4, i)
                        generator.Emit(OpCodes.Ldelem_Ref)
                        castFromObject paramType)     

    
    let preComputeRecordContructor(recordType:Type,bindingFlags:BindingFlags option) =
        assert FSharpType.IsRecord(recordType, ?bindingFlags=bindingFlags)
        let ctorInfo = FSharpValue.PreComputeRecordConstructorInfo(recordType,?bindingFlags=bindingFlags)
        let meth = new DynamicMethod( "ctor", MethodAttributes.Static ||| MethodAttributes.Public,
                                         CallingConventions.Standard, typeof<obj>, [| typeof<obj[]> |],
                                         recordType,
                                         true )
        let generator = meth.GetILGenerator()
        let paramTypes = ctorInfo.GetParameters() |> Seq.map (fun pi -> pi.ParameterType)
    
        let invoker =
            pushParams generator paramTypes
            generator.Emit(OpCodes.Newobj, ctorInfo)
            generator.Emit(OpCodes.Ret)
            meth.CreateDelegate(typeof<Func<obj[],obj>>) :?> Func<obj[],obj>
        invoker.Invoke
    
    let preComputeUnionConstructor(unionCaseInfo:UnionCaseInfo, bindingFlags:BindingFlags option) =
        let methodInfo = FSharpValue.PreComputeUnionConstructorInfo(unionCaseInfo, ?bindingFlags=bindingFlags)
        let targetType = methodInfo.DeclaringType
        assert FSharpType.IsUnion(targetType, ?bindingFlags=bindingFlags)
        let meth = new DynamicMethod( "invoke", MethodAttributes.Static ||| MethodAttributes.Public,
                                        CallingConventions.Standard, typeof<obj>,
                                        [| typeof<obj[]> |], targetType, true )
        let paramTypes = methodInfo.GetParameters() |> Seq.map (fun pi -> pi.ParameterType)
        let generator = meth.GetILGenerator()

        let invoker =
            pushParams generator paramTypes
            generator.Emit(OpCodes.Call, methodInfo)
            generator.Emit(OpCodes.Ret)
            meth.CreateDelegate(typeof<Func<obj[],obj>>) :?> Func<obj[],obj>

        invoker.Invoke

    type Marker = class end
    let getter<'Target, 'Return> (methodInfo:MethodInfo) =
         let del = Delegate.CreateDelegate(typeof<Func<'Target,'Return>>, methodInfo) :?> Func<'Target,'Return>
         fun target -> box <| del.Invoke(unbox target)
    let getterMethodInfo = typeof<Marker>.DeclaringType.GetMethod("getter",BindingFlags.Static ||| BindingFlags.NonPublic)

    let preComputeFieldReader (nonPublic:bool) (propertyInfo:PropertyInfo) :(obj -> obj) =
        let getMethod = propertyInfo.GetGetMethod(nonPublic)
        let target = propertyInfo.DeclaringType
        let returnType = propertyInfo.PropertyType
        let toInvoke = getterMethodInfo.MakeGenericMethod([| target; returnType |])
        let getter = toInvoke.Invoke(null, [| getMethod |]) 
        getter :?> (obj -> obj)

    let wantNonPublic bindingFlags =
        let bindingFlags = defaultArg bindingFlags BindingFlags.Public
        bindingFlags &&& BindingFlags.NonPublic = BindingFlags.NonPublic

    let preComputeFieldsReader fields bindingFlags =
        let readers = fields |> Array.map (preComputeFieldReader (wantNonPublic bindingFlags))
        fun (target:obj) -> readers |> Array.map ((|>) target)

    let preComputeRecordReader (recordType:Type, bindingFlags:BindingFlags option) =
        let fields = FSharpType.GetRecordFields(recordType, ?bindingFlags=bindingFlags)
        preComputeFieldsReader fields bindingFlags

    let preComputeUnionReader(unionCase:UnionCaseInfo, bindingFlags:BindingFlags option) =
        let fields = unionCase.GetFields()
        preComputeFieldsReader fields bindingFlags

namespace Microsoft.FSharp.Reflection

open System
open System.Reflection

type FSharpValue =
    static member PreComputeRecordConstructorFast(recordType:Type,?bindingFlags:BindingFlags) =
        FSharpx.ReflectImpl.preComputeRecordContructor(recordType,bindingFlags)
    static member PreComputeUnionConstructorFast(unionCase:UnionCaseInfo, ?bindingFlags:BindingFlags) =
        FSharpx.ReflectImpl.preComputeUnionConstructor(unionCase,bindingFlags)
    static member PreComputeRecordReaderFast(recordType:Type, ?bindingFlags:BindingFlags) : obj -> obj[] =
        FSharpx.ReflectImpl.preComputeRecordReader(recordType,bindingFlags)
    static member PreComputeUnionReaderFast(unionCase:UnionCaseInfo, ?bindingFlags:BindingFlags) : obj -> obj[] =
        FSharpx.ReflectImpl.preComputeUnionReader(unionCase, bindingFlags)

