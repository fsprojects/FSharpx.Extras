// Copyright (c) Microsoft Corporation 2005-2013.
// This sample code is provided "as is" without warranty of any kind. 
// We disclaim all warranties, either express or implied, including the 
// warranties of merchantability and fitness for a particular purpose. 


/// Contains runtime functionality (generally extensions to System.Management.dll)
/// to support the code and types provided statically by WmiProvider.DesignTime.fs
namespace FSharpx.TypeProviders.Management.Runtime

open System
open System.Management

module internal RuntimeHelpers = 
    /// Converts a given datetime in DMTF format to DateTimeOffset object.
    let ToDateTime(dmtfDate:string) =
        match dmtfDate with 
        | null -> raise (new ArgumentNullException("dmtfDate"))
        | s when s.Length <> 25 -> raise (new ArgumentOutOfRangeException("dmtfDate"))
        | _ -> 
        try 
            let year = 
                match dmtfDate.Substring(0, 4) with 
                | "****" -> DateTime.MinValue.Year
                | s -> Int32.Parse s

            let month = 
                match dmtfDate.Substring(4, 2) with 
                | "**" -> DateTime.MinValue.Month
                | s -> Int32.Parse s
            
            let day = 
                 match dmtfDate.Substring(6, 2) with 
                 | "**" -> DateTime.MinValue.Day
                 | s -> Int32.Parse s
            
            let hour = 
                match dmtfDate.Substring(8, 2) with 
                | "**" -> DateTime.MinValue.Hour
                | s -> Int32.Parse s
            
            let minute = 
                match dmtfDate.Substring(10, 2) with 
                | "**" -> DateTime.MinValue.Minute
                | s -> Int32.Parse s
            
            let second = 
                match dmtfDate.Substring(12, 2) with 
                | "**" -> DateTime.MinValue.Second
                | s -> Int32.Parse s

            let ticks = 
                match dmtfDate.Substring(15, 6) with 
                | "******" -> 0L
                | s -> (Int64.Parse(s) * (TimeSpan.TicksPerMillisecond / 1000L))
            
            if (year < 0 || month < 0 || day < 0 || hour < 0 || minute < 0 || second < 0 || ticks < 0L) then
                raise (new ArgumentOutOfRangeException())
            let offset = 
                match dmtfDate.Substring(22, 3) with 
                | "******" -> DateTimeOffset.MinValue.Offset
                | _ -> 
                    let s = dmtfDate.Substring(21, 4)
                    let UTCOffset = Int32.Parse(s)
                    TimeSpan.FromMinutes (float UTCOffset)
                    //let tickOffset = TimeZone.CurrentTimeZone.GetUtcOffset(datetime)
                    //let OffsetMins = tickOffset.Ticks / TimeSpan.TicksPerMinute
                    //let OffsetToBeAdjusted = int (OffsetMins - int64 UTCOffset)
                    //datetime.AddMinutes(double OffsetToBeAdjusted)
            let datetime = new DateTimeOffset(year, month, day, hour, minute, second, offset)
            let datetime = datetime.AddTicks(ticks)
            datetime
        with e -> 
            raise (new ArgumentOutOfRangeException("dmtfDate", e.Message))

/// Represents a connection to the WMI service. 
type public DataContext public (scope:ManagementScope) =
    let event = Event<_>()
    /// Represents the WMI scope associated with the service
    member val Scope = scope
    [<CLIEvent>]
    /// Occurs immediately before an underlying WMI query is executed 
    member __.QueryExecuted = event.Publish
    member internal __.OnQueryExecuted(s:string) = event.Trigger s

// Hard code the intrinsic events (just the instance events at the moment)
//  TODO: should we handle {Class, Namespace}{Operation, Modification, Creation, Deletion}?
//        At the very least, we could trigger an invalidation at design time...
//        We are also ignoring AggregateEvent, TimerEvent, Win32_NTLogEvent for now. 

// Creation/Deletion/Operation
/// The data associated with the creation or deletion event for a WMI instance
type public InstanceOperation<'t>(o:ManagementBaseObject) =
    member __.TargetInstance = o.["TargetInstance"] :?> 't
    member __.SecurityDescriptor = o.["SECURITY_DESCRIPTOR"] :?> byte
    member __.TimeCreated = o.["TIME_CREATED"] :?> uint64

// Modification
/// The data associated with the modification event for a WMI instance
type public InstanceModification<'t>(o:ManagementBaseObject) =
    inherit InstanceOperation<'t>(o)
    member __.PreviousInstance = o.["PreviousInstance"] :?> 't

// MethodInvocation
/// The data associated with the invocation event for a WMI instance
type MethodInvocation<'t>(o) =
    inherit InstanceOperation<'t>(o)
    member __.Method = o.["Method"] :?> string
    member __.Parameters = o.["Parameters"] :?> ManagementBaseObject
    member __.PreCall = o.["PreCall"] :?> bool

/// Represents a binary operation  in a WMI query
type public BinaryCmp =
    | LT | GT | EQ | NE | LE | GE 
    member this.Invert() = 
        match this with
        | LT -> GE
        | GT -> LE
        | EQ -> NE
        | NE -> EQ
        | LE -> GT
        | GE -> LT
    override this.ToString() =
        match this with
        | LT -> "<"
        | GT -> ">"
        | EQ -> "="
        | NE -> "<>"
        | LE -> "<="
        | GE -> ">="

/// Represents a unary operation in a WMI query
type UnaryCmp =
    | IsNull 
    | IsNotNull
    member this.Invert() =
        match this with 
        | IsNull -> IsNotNull
        | IsNotNull -> IsNull
    override this.ToString() =
        match this with
        | IsNull -> "IS NULL"
        | IsNotNull -> "IS NOT NULL"        

/// Represents a condition in a WMI query
type Condition =
    | And of Condition * Condition
    | Or of Condition * Condition
    | Binary of BinaryCmp * string * string
    | Unary of UnaryCmp * string
    | Simple of string 
    member this.Invert() =
        match this with
        | Simple s -> failwithf "Condition %s is not invertible" s
        | And(a,b) -> Or(a.Invert(), b.Invert())
        | Or(a,b) -> And(a.Invert(), b.Invert())
        | Unary(u,s) -> Unary(u.Invert(),s)
        | Binary(b,s1,s2) -> Binary(b.Invert(),s1,s2)
    override this.ToString() =
        match this with
        | Simple s -> s
        | And(s1,s2) -> sprintf "(%O AND %O)" s1 s2
        | Or(s1,s2) -> sprintf "(%O OR %O)" s1 s2
        | Binary(b,s1,s2) -> sprintf "%s %O %s" s1 b s2
        | Unary(u,s) -> sprintf "%s %O" s u

/// Represents WMI query
type IWmiQueryable<'t,'q when 'q :> IWmiQueryable<'t,'q>> =
    abstract Where : c:Condition -> 'q

[<AbstractClass; Sealed>]
/// A phantom type to indicate a logical property of a WMI query
type RequiresWithin = class end
[<AbstractClass; Sealed>]
/// A phantom type to indicate a logical property of a WMI query
type DoesntRequireWithin = class end

type internal EventInfo = 
    { evName : string
      within : int option
      isa : string option
      condition : Condition option }

/// The operations used at runtime by the provided types
type RuntimeAPI() = 

    static member InvokeManagementMethod(wmiObj:ManagementBaseObject, methName:string, inParamNames:string[], inParamVals:obj[]) =
        let wmiObj = wmiObj :?> ManagementObject
        let inParams = wmiObj.GetMethodParameters(methName)
        for (paramName, paramVal) in Seq.zip inParamNames inParamVals do 
            inParams.[paramName] <- paramVal
        let outParams = wmiObj.InvokeMethod(methName, inParams, null)  // TODO: CONSIDER: InvokeMethodOptions.InfiniteTimeout
        if outParams = null then
            null, outParams
        else 
            let retVal = outParams.Properties.["ReturnValue"].Value
            outParams.Properties.Remove("ReturnValue")
            retVal, outParams
        
    static member InvokeStaticManagementMethod(scope, wmiClassPath:string, methName:string, inParamNames:string[], inParamVals:obj[]) =
        use wmiClass = new ManagementClass(wmiClassPath, Scope=scope)
        RuntimeAPI.InvokeManagementMethod(wmiClass, methName, inParamNames, inParamVals)


    static member GetManagementProp(wmiObj:ManagementBaseObject,propName:string) = 
        wmiObj.[propName]
       
    static member GetReferenceToManagementObject(wmiObj:ManagementBaseObject) = (wmiObj :?> ManagementObject).Path.Path

    static member GetManagementObjectFromReference(ref:string) = lazy (new ManagementObject(ref) :> obj)

    static member TranslateDateTime(obj:obj) = 
        match obj with
        | null -> null
        | _ -> RuntimeHelpers.ToDateTime(obj :?> string) |> box

    static member ArrayContains(arr:'a[], v:'a) =
        arr |> Array.exists (fun v' -> v = v')

    static member Set([<System.Runtime.InteropServices.Out>]x:byref<'t>,v) = x <- v

/// The operations used at runtime by the provided types
module internal QueryHelpers =
    open Quotations.Patterns
    open Quotations.DerivedPatterns
    open Linq.NullableOperators

    let (|NullableValue|_|) (p:System.Reflection.PropertyInfo) =
        if p.DeclaringType.IsGenericType && p.DeclaringType.GetGenericTypeDefinition() = typedefof<System.Nullable<_>> && p.Name = "Value" then 
            Some()
        else None

    let (|NullableHasValue|_|)(p:System.Reflection.PropertyInfo) =
        if p.DeclaringType.IsGenericType && p.DeclaringType.GetGenericTypeDefinition() = typedefof<System.Nullable<_>> && p.Name = "HasValue" then 
            Some()
        else None

    let rec (|Unwrap|) = function
    | Coerce(Unwrap e,_) 
    | SpecificCall <@ LanguagePrimitives.IntrinsicFunctions.UnboxGeneric @> (None, [_], [Unwrap e]) 
    | SpecificCall <@ fun (x:System.Nullable<_>) -> x.get_Value() @> (Some (Unwrap e), [_], [])
    | PropertyGet(Some(Unwrap e), NullableValue, [])
    | e -> e

    let rec processFilter v = function
    | SpecificCall <@ (=) @> (None,[_],[Unwrap l; Unwrap r])
    | SpecificCall <@ (?=) @> (None,[_],[Unwrap l; Unwrap r]) ->
        Binary(EQ, processProj true v l, processProj true v r)
    | SpecificCall <@ (<>) @> (None,[_],[Unwrap l; Unwrap r])
    | SpecificCall <@ (?<>) @> (None,[_],[Unwrap l; Unwrap r]) ->
        Binary(NE, processProj true v l, processProj true v r)
    | SpecificCall <@ (>) @> (None,[_],[Unwrap l; Unwrap r]) 
    | SpecificCall <@ (?>) @> (None,[_],[Unwrap l; Unwrap r]) ->
        Binary(GT, processProj true v l, processProj true v r)
    | SpecificCall <@ (<) @> (None,[_],[Unwrap l; Unwrap r])
    | SpecificCall <@ (?<) @> (None,[_],[Unwrap l; Unwrap r]) ->
        Binary(LT, processProj true v l, processProj true v r)    
    | SpecificCall <@ (>=) @> (None,[_],[Unwrap l; Unwrap r]) 
    | SpecificCall <@ (?>=) @> (None,[_],[Unwrap l; Unwrap r]) ->
        Binary(GE, processProj true v l, processProj true v r)
    | SpecificCall <@ (<=) @> (None,[_],[Unwrap l; Unwrap r])
    | SpecificCall <@ (?<=) @> (None,[_],[Unwrap l; Unwrap r]) ->
        Binary(LE, processProj true v l, processProj true v r)
    | PropertyGet(Some(Unwrap e), NullableHasValue, []) ->
        Unary(IsNotNull, processProj true v e)
    | SpecificCall <@ not @> (None,[],[Unwrap e]) ->
        (processFilter v e).Invert()
    | AndAlso(f1, f2) -> And(processFilter v f1, processFilter v f2)
    | OrElse(f1, f2) -> Or(processFilter v f1, processFilter v f2)
    | Unwrap e ->
        try
            let e' = processProj true v e
            Binary(EQ, e',"TRUE")
        with _ -> failwithf "Unsupported filter (%A)." e

    and processProj topMost v = function
    | Value(:? string as v,_) when topMost -> v.Replace(@"\",@"\\").Replace("\"","\"\"") |> sprintf "\"%s\""
    | Value((:? int | :? int64 | :? uint32 | :? uint64 | :? byte | :? sbyte | :? int16 | :? uint16 | :? float | :? float32) as v,_) when topMost -> v.ToString()
    | Value(:? bool as v,_) when topMost -> v.ToString().ToUpperInvariant()
    | PropertyGet(Some (Unwrap(Var v')), p, []) when v = v' -> 
        sprintf "%s" p.Name
    | PropertyGet(Some (Unwrap e), p, []) -> 
        sprintf "%s.%s" (processProj false v e) p.Name
    | SpecificCall <@ RuntimeAPI.GetManagementProp @>(None, [], [Unwrap(Var v'); Value(:? string as s,_)]) when v = v' -> 
        sprintf "%s" s        
    | SpecificCall <@ RuntimeAPI.GetManagementProp @>(None, [], [Unwrap(e); Value(:? string as s,_)]) -> 
        sprintf "%s.%s" (processProj false v e) s
    | e -> failwithf "Unsupported projection (%A)." e

    let rec varCount v = function
    | Quotations.ExprShape.ShapeVar v' when v = v' -> 1
    | Quotations.ExprShape.ShapeVar _ -> 0
    | Quotations.ExprShape.ShapeLambda (_,e) -> varCount v e
    | Quotations.ExprShape.ShapeCombination(_,l) -> List.sumBy (varCount v) l

    // Make pattern matching easier by removing extra lets
    let rec removeSuperfluousLets = function
    | Let(v, e, b) when varCount v b = 1 -> (* NB: bodies assumed to force evaluation bound var *)
        b.Substitute(fun v' -> if v = v' then Some e else None)
        |> removeSuperfluousLets
    | Let(v, (Value _ | Var _ | PropertyGet _ as e), b) -> (* NB: Property getters are assumed to be side-effect free *)
        b.Substitute(fun v' -> if v = v' then Some e else None)
        |> removeSuperfluousLets
    | Quotations.ExprShape.ShapeCombination(o,l) -> Quotations.ExprShape.RebuildShapeCombination(o, l |> List.map removeSuperfluousLets)
    | Quotations.ExprShape.ShapeLambda(v, e) ->
        Quotations.Expr.Lambda(v, e |> removeSuperfluousLets)
    | e -> e

/// Represents collection that supports WMI queries
type public WmiCollection<'t>(ctx:DataContext, collClass:string, cond:Condition option, trans:System.Func<ManagementBaseObject,'t>) =

    let query = 
        let q = Management.SelectQuery(collClass)
 
        match cond with
        | Some c -> q.Condition <- c.ToString()
        | None -> ()           

        q

    let queryResults =
        seq { use searcher = new ManagementObjectSearcher(ctx.Scope, query)
              ctx.OnQueryExecuted(query.QueryString)
              for o in searcher.Get() do
                  yield trans.Invoke(o) }

    interface IWmiQueryable<'t, WmiCollection<'t>> with
        member __.Where(c) =
            WmiCollection(ctx, collClass, Some(Option.fold (fun c1 c2 -> And(c1,c2)) c cond), trans)

    member __.Select(f:System.Func<'t,'u>) = 
        WmiCollection(ctx, collClass, cond, fun x -> x |> trans.Invoke |> f.Invoke)

    interface seq<'t> with
        member __.GetEnumerator() =
            queryResults.GetEnumerator()
        member __.GetEnumerator() =
            queryResults.GetEnumerator() :> System.Collections.IEnumerator

    member __.Context = ctx
    member __.WmiQueryString = query.QueryString

/// Represents an event that supports WMI event queries
type WmiEvent<'t,'e> internal (ctx:DataContext, evInfo : EventInfo, trans : System.Func<ManagementBaseObject, 't>) =
   
    let mutable subCount = 0
    let query = 
        let q = WqlEventQuery(evInfo.evName)

        evInfo.within |> Option.iter (fun i -> q.WithinInterval <- TimeSpan.FromSeconds(float i))

        let isaCond = evInfo.isa |> Option.map (sprintf "TargetInstance ISA \"%s\"" >> Simple)
        q.Condition <-
            match isaCond, evInfo.condition with
            | Some isa, Some c -> And(isa, c).ToString()
            | Some c, None 
            | None, Some c -> c.ToString()
            | None, None   -> null

        q

    let watcher = // construct only when needed (during subscription)
        lazy             
            new ManagementEventWatcher(ctx.Scope, query)

    interface IWmiQueryable<'t, WmiEvent<'t,'e>> with
        member __.Where(c) = 
            new WmiEvent<'t,'e>(ctx, { evInfo with condition = Some(evInfo.condition |> Option.fold (fun c1 c2 -> And(c1,c2)) c) }, trans)

    interface IObservable<'t> with 
        member __.Subscribe(f) = 
            if typeof<'e> = typeof<RequiresWithin> then 
                failwith "Intrinsic events require \"within\" clauses.  Consider using an event query: wmiEvent { for e in <event> do within <n> }"

            let watcher = watcher.Value
            let h = EventArrivedEventHandler(fun _ e -> f.OnNext(trans.Invoke e.NewEvent))
            if subCount = 0 then
                ctx.OnQueryExecuted(watcher.Query.QueryString)
                watcher.Start()
            subCount <- subCount + 1
            watcher.EventArrived.AddHandler h
            { new System.IDisposable with
                member __.Dispose() = 
                    watcher.EventArrived.RemoveHandler h
                    subCount <- subCount - 1
                    if subCount = 0 then
                        watcher.Stop() }

    interface IDisposable with
        member __.Dispose() = 
            if watcher.IsValueCreated then
                watcher.Value.Dispose()

    member __.AddWithin(i) =
        if Option.isSome evInfo.within then
            failwith "Only one within clause is allowed"
        new WmiEvent<'t,DoesntRequireWithin>(ctx, { evInfo with within = Some i }, trans)

    member __.Select(f:System.Func<'t, 'u>) =
        new WmiEvent<'u,'e>(ctx, evInfo, fun x -> x |> trans.Invoke |> f.Invoke)

    member __.WmiQueryString = query.QueryString
        

type RuntimeAPI with
    static member IntrinsicEventMethod<'t>(ctx, evName, className, conv) =
        new WmiEvent<'t,RequiresWithin>(ctx, { evName = evName; within = None; isa = Some className; condition = None }, conv)

    static member ExtrinsicEventMethod<'t>(ctx, evName, conv) =
        new WmiEvent<'t,DoesntRequireWithin>(ctx, { evName = evName; within = None; isa = None; condition = None }, conv)

    static member GetClassInstancesByName<'t>(ctx, name:string, conv) = 
        WmiCollection(ctx, name, None, conv)

open Quotations.Patterns
open Quotations.DerivedPatterns

/// The query syntax builder for WMI event queries
type WmiEventBuilder() =
    member __.For(_:WmiEvent<'t,'e>, _:'t->'t) : WmiEvent<'t,'e> = failwith "Not intended for runtime use"
    member __.Yield(x) = x
    [<CustomOperation("where", MaintainsVariableSpace = true)>]
    member __.Where(_:WmiEvent<'t,DoesntRequireWithin>, [<ProjectionParameter>]f:'t -> bool) : WmiEvent<'t,DoesntRequireWithin> = ignore f; failwith "Not intended for runtime use"
    [<CustomOperation("select")>]
    member __.Select(e:WmiEvent<'t,DoesntRequireWithin>, [<ProjectionParameter>]f) = e.Select f
    [<CustomOperation("within", MaintainsVariableSpace = true)>]
    member __.Within(e:WmiEvent<'t,RequiresWithin>, i:int) = e.AddWithin i
    member __.Quote() = ()
    member this.Run(q:Quotations.Expr<WmiEvent<'t,DoesntRequireWithin>>) : WmiEvent<'t,DoesntRequireWithin> =
        let eval e = e |> Linq.RuntimeHelpers.LeafExpressionConverter.EvaluateQuotation

        let (|Where|_|) = function
        | SpecificCall <@ fun (qb:WmiEventBuilder) -> qb.Where @> (Some _,[_],[ev; proj]) -> Some(ev,proj) 
        | _ -> None

        let (|Select|_|) = function
        | SpecificCall <@ fun (qb:WmiEventBuilder) -> qb.Select @> (Some _,[_;_],[ev; proj]) -> Some(ev,proj) 
        | _ -> None

        let (|Within|_|) = function
        | SpecificCall <@ fun (qb:WmiEventBuilder) -> qb.Within @> (Some _,[_],[ev; Value(:? int as i,_)]) -> Some(ev,i) 
        | _ -> None
        
        let (|For|_|) = function
        | SpecificCall <@ fun (qb:WmiEventBuilder) -> qb.For @> (Some _,[_;_],[ev; proj]) -> Some(ev,proj) 
        | _ -> None
        
        let (|Yield|_|) = function
        | SpecificCall <@ fun (qb:WmiEventBuilder) -> qb.Yield @> (Some _,[_],[arg]) -> Some(arg) 
        | _ -> None

        let rec processQuery = function
        | Where(ev, Lambda(v, proj)) ->
            let c = QueryHelpers.processFilter v proj
            let ev = processQuery ev            
            ev.GetType().GetInterface(typedefof<IWmiQueryable<_,WmiEvent<'t,_>>>.FullName).GetMethod("Where").Invoke(ev, [|c|])
        | Within(ev,i) & Call(_,m,_) ->
            m.Invoke(this, [|processQuery ev; i|])
        | Select(ev,f) & Call(_,m,_) -> 
            m.Invoke(this, [|processQuery ev; eval f|])
        | For(ev, Lambda(v, Yield (Var v'))) when v = v' ->
            eval ev
        | e -> failwithf "Unsupported query (%A)." e

        q |> QueryHelpers.removeSuperfluousLets |> processQuery |> unbox

/// The query syntax builder for WMI collection queries
type WmiQueryBuilder() =
    member __.For(_:WmiCollection<'t>, _:'t->'t) : WmiCollection<'t> = failwith "Not intended for runtime use"
    member __.Yield(x) = x
    [<CustomOperation("where", MaintainsVariableSpace = true)>]
    member __.Where(_:WmiCollection<'t>, [<ProjectionParameter>]f:'t -> bool) : WmiCollection<'t> = ignore f; failwith "Not intended for runtime use"
    [<CustomOperation("select")>]
    member __.Select(e:WmiCollection<'t>, [<ProjectionParameter>]f) = e.Select(f)
    [<CustomOperation("within", MaintainsVariableSpace = true)>]
    member __.Quote() = ()
    member this.Run(q:Quotations.Expr<WmiCollection<'t>>) : WmiCollection<'t> =

        let eval e = e |> Linq.RuntimeHelpers.LeafExpressionConverter.EvaluateQuotation

        let (|Where|_|) = function
        | SpecificCall <@ fun (qb:WmiQueryBuilder) -> qb.Where @> (Some _,[_],[coll; proj]) -> Some(coll,proj) 
        | _ -> None

        let (|Select|_|) = function
        | SpecificCall <@ fun (qb:WmiQueryBuilder) -> qb.Select @> (Some _,[_;_],[coll; proj]) -> Some(coll,proj) 
        | _ -> None

        let (|For|_|) = function
        | SpecificCall <@ fun (qb:WmiQueryBuilder) -> qb.For @> (Some _,[_],[coll; proj]) -> Some(coll,proj) 
        | _ -> None
        
        let (|Yield|_|) = function
        | SpecificCall <@ fun (qb:WmiQueryBuilder) -> qb.Yield @> (Some _,[_],[arg]) -> Some(arg) 
        | _ -> None

        let rec processQuery = function
        | Where(coll, Lambda(v, proj)) ->
            let c = QueryHelpers.processFilter v proj
            let coll = processQuery coll            
            coll.GetType().GetInterface(typedefof<IWmiQueryable<_,WmiCollection<'t>>>.FullName).GetMethod("Where").Invoke(coll, [|c|])
        | Select(coll,f) & Call(_,m,_) -> 
            m.Invoke(this, [|processQuery coll; eval f|])
        | For(coll, Lambda(v, Yield (Var v'))) when v = v' ->
            eval coll
        | e -> failwithf "Unsupported query (%A)." e

        q |> QueryHelpers.removeSuperfluousLets |> processQuery |> unbox

namespace System.Management

open FSharpx.TypeProviders.Management.Runtime

[<AutoOpen>]
module WmiQuery =
    /// The query syntax builder for WMI collection queries
    let wmiQuery = WmiQueryBuilder()
    /// The query syntax builder for WMI event queries
    let wmiEvent = WmiEventBuilder()
  
// This declaration points the compiler to the right design-time DLL for the selected runtime DLL.
open Microsoft.FSharp.Core.CompilerServices
[<assembly:TypeProviderAssembly("FSharpx.TypeProviders.Management.DesignTime")>]

do()