// Copyright (c) Microsoft Corporation 2005-2013.
// This sample code is provided "as is" without warranty of any kind. 
// We disclaim all warranties, either express or implied, including the 
// warranties of merchantability and fitness for a particular purpose.

module internal FSharpx.TypeProviders.XrmProvider.Runtime.Patterns

open System
open System.Linq.Expressions
open System.Reflection

open Microsoft.Xrm.Sdk
open Microsoft.Xrm.Sdk.Query

let (|MethodWithName|_|)   (s:string) (m:MethodInfo)   = if s = m.Name then Some () else None
let (|PropertyWithName|_|) (s:string) (m:PropertyInfo) = if s = m.Name then Some () else None

let (|MethodCall|_|) (e:Expression) = 
    match e.NodeType, e with 
    | ExpressionType.Call, (:? MethodCallExpression as e) -> 
        Some ((match e.Object with null -> None | obj -> Some obj), e.Method, Seq.toList e.Arguments)
    | _ -> None

let (|NewArrayValues|_|) (e:Expression) = 
    match e.NodeType, e with 
    | ExpressionType.NewArrayInit, (:? NewArrayExpression as e) ->  Some(Expression.Lambda(e).Compile().DynamicInvoke() :?> Array)
    | _ -> None

let (|PropertyGet|_|) (e:Expression) = 
    match e.NodeType, e with 
    | ExpressionType.MemberAccess, ( :? MemberExpression as e) -> 
        match e.Member with 
        | :? PropertyInfo as p -> Some ((match e.Expression with null -> None | obj -> Some obj), p)
        | _ -> None
    | _ -> None

let (|Constant|_|) (e:Expression) = 
    match e.NodeType, e with 
    | ExpressionType.Constant, (:? ConstantExpression as ce) -> Some (ce.Value, ce.Type)
    | _ -> None

let (|ConstantOrNullableConstant|_|) (e:Expression) = 
    match e.NodeType, e with 
    | ExpressionType.Constant, (:? ConstantExpression as ce) -> Some(Some(ce.Value))
    | ExpressionType.Convert, (:? UnaryExpression as ue ) -> 
        match ue.Operand with
        | :? ConstantExpression as ce -> if ce.Value = null then Some(None) else Some(Some(ce.Value))
        | :? NewExpression as ne -> Some(Some(Expression.Lambda(ne).Compile().DynamicInvoke()))
        | _ -> failwith "unsupported nullable expression"
    | _ -> None

let (|Bool|_|)   = function Constant((:? bool   as b),_) -> Some b | _ -> None
let (|String|_|) = function Constant((:? string as s),_) -> Some s | _ -> None
let (|Int|_|)    = function Constant((:? int    as i),_) -> Some i | _ -> None
    
let (|ParamName|_|) (e:Expression) = 
    match e.NodeType, e with 
    | ExpressionType.Parameter, (:? ParameterExpression as pe) ->  Some pe.Name
    | _ -> None    
    
let (|Lambda|_|) (e:Expression) = 
    match e.NodeType, e with 
    | ExpressionType.Lambda, (:? LambdaExpression as ce) ->  Some (Seq.toList ce.Parameters, ce.Body)
    | _ -> None

let (|OptionalQuote|) (e:Expression) = 
    match e.NodeType, e with 
    | ExpressionType.Quote, (:? UnaryExpression as ce) ->  ce.Operand
    | _ -> e

let (|AndAlso|_|) (e:Expression) =
    match e.NodeType, e with
    | ExpressionType.AndAlso, ( :? BinaryExpression as be) -> Some(be.Left,be.Right)
    | _ -> None
    
let (|OrElse|_|) (e:Expression) =
    match e.NodeType, e with
    | ExpressionType.OrElse, ( :? BinaryExpression as be) -> Some(be.Left,be.Right)
    | _ -> None
    
let (|AndAlsoOrElse|_|) (e:Expression) =
    match e.NodeType, e with
    | ExpressionType.OrElse,  ( :? BinaryExpression as be) 
    | ExpressionType.AndAlso, ( :? BinaryExpression as be)  -> Some(be.Left,be.Right)
    | _ -> None

let (|XrmAttributeGet|_|) = function 
    | MethodCall(Some(o),(MethodWithName "GetAttribute" as meth),[String key]) -> 
        match o with
        | :? MemberExpression as m  -> Some(m.Member.Name,key,meth.ReturnType) 
        | _ -> Some(String.Empty,key,meth.ReturnType) 
    | _ -> None

let (|XrmOptionSetGet|_|) = function
    | MethodCall(None, MethodWithName "ToEnum", [MethodCall(Some(o),(MethodWithName "GetEnumValue" as meth), [String key])]) ->
        match o with
        | :? ParameterExpression  as m -> Some(m.Name,key,meth.ReturnType )
        | :? MemberExpression as m  -> Some(m.Member.Name,key,meth.ReturnType) 
        | _ -> failwith "unsupported optionset access! the squirrels are in the system!"
    | _ -> None

let (|XrmSpecialOpArr|_|) = function
    | MethodCall(None,(|=|), [XrmAttributeGet(ti,key,_); NewArrayValues values] ) -> Some(ti,ConditionOperator.In,   key,values)
    | MethodCall(None,(|<>|),[XrmAttributeGet(ti,key,_); NewArrayValues values] ) -> Some(ti,ConditionOperator.NotIn,key,values)
    | _ -> None
    
let (|XrmSpecialOp|_|) = function
    | MethodCall(None,(=%), [XrmAttributeGet(ti,key,_); right]) -> Some(ti,ConditionOperator.Like,   key,Expression.Lambda(right).Compile().DynamicInvoke())
    | MethodCall(None,(<>%),[XrmAttributeGet(ti,key,_); right]) -> Some(ti,ConditionOperator.NotLike,key,Expression.Lambda(right).Compile().DynamicInvoke())
    // String  methods
    | MethodCall(Some(XrmAttributeGet(ti,key,t)), MethodWithName "Contains", [right]) when t = typeof<string> -> 
        // Although "contains" appears in the ConditionOperator enum, it is not actually implemented server side so make this a like instead
        Some(ti,ConditionOperator.Like,key,box (sprintf "%%%O%%" (Expression.Lambda(right).Compile().DynamicInvoke())))
    | MethodCall(Some(XrmAttributeGet(ti,key,t)), MethodWithName "StartsWith", [right]) when t = typeof<string> -> 
        Some(ti,ConditionOperator.BeginsWith,key,Expression.Lambda(right).Compile().DynamicInvoke())
    | MethodCall(Some(XrmAttributeGet(ti,key,t)), MethodWithName "EndsWith", [right]) when t = typeof<string> -> 
        Some(ti,ConditionOperator.EndsWith,key,Expression.Lambda(right).Compile().DynamicInvoke())
    | _ -> None
                
let (|XrmCondOp|_|) (e:Expression) = 
    match e.NodeType, e with 
    | ExpressionType.Equal,              (:? BinaryExpression as ce) -> Some (ConditionOperator.Equal,        ce.Left,ce.Right)
    | ExpressionType.LessThan,           (:? BinaryExpression as ce) -> Some (ConditionOperator.LessThan,     ce.Left,ce.Right)
    | ExpressionType.LessThanOrEqual,    (:? BinaryExpression as ce) -> Some (ConditionOperator.LessEqual,    ce.Left,ce.Right)
    | ExpressionType.GreaterThan,        (:? BinaryExpression as ce) -> Some (ConditionOperator.GreaterThan,  ce.Left,ce.Right)
    | ExpressionType.GreaterThanOrEqual, (:? BinaryExpression as ce) -> Some (ConditionOperator.GreaterEqual, ce.Left,ce.Right)
    | ExpressionType.NotEqual,           (:? BinaryExpression as ce) -> Some (ConditionOperator.NotEqual,     ce.Left,ce.Right)
    | _ -> None
