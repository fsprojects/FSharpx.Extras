namespace FSharpx.Core.Tests
  
module ReflectionRecordTest =

    open NUnit.Framework
    open Microsoft.FSharp.Reflection
    open System.Reflection //for bindingflags
    open FSharpx.Reflection

    open FSharpx.Reflection

    //some test types

    type Straightforward =
            { S : string
              I : int
              F : float
              O : obj } //object is somewhat special cased the constructor code

    type Mutable =
        { mutable Sm : string
          mutable Im : int
          mutable Fm : float }

    type internal Internal = internal { Si : string }

    type private Private = private { Fp : float }

    type Generic<'a> = { Generic : 'a }

    [<Test>]
    let ``should construct Straightforward record``() =
        let ctor = FSharpValue.PreComputeRecordConstructorFast(typeof<Straightforward>)
        let data = [| box "string"; box 12; box 15.12; new obj() |]

        let result = ctor data
        Assert.IsInstanceOf(typeof<Straightforward>, result)
        let primResult = result :?> Straightforward
        Assert.AreEqual("string", primResult.S)
        Assert.AreEqual(12, primResult.I)
        Assert.AreEqual(15.12, primResult.F)
        Assert.AreEqual(data.[3], primResult.O)


    [<Test>]
    let ``should read Straightforward record``() =
        let dtor = FSharpValue.PreComputeRecordReaderFast typeof<Straightforward>        
        let data = { S = "string"; I = 12; F = 15.12; O = new obj() }

        let result = dtor data
        Assert.AreEqual(data.S, unbox<string> result.[0])
        Assert.AreEqual(data.I, unbox<int> result.[1])
        Assert.AreEqual(data.F, unbox<float> result.[2])
        Assert.AreSame(data.O, result.[3])

    [<Test>]
    let ``should construct Mutable record``() =
        let ctor = FSharpValue.PreComputeRecordConstructorFast(typeof<Mutable>)
        let data = [| box "string"; box 12; box 15.12 |]

        let result = ctor data
        let primResult = result :?> Mutable
        Assert.AreEqual("string", primResult.Sm)
        Assert.AreEqual(12, primResult.Im)
        Assert.AreEqual(15.12, primResult.Fm)


    [<Test>]
    let ``should read Mutable record``() =
        let dtor = FSharpValue.PreComputeRecordReaderFast typeof<Mutable>        
        let data = { Sm = "string"; Im = 12; Fm = 15.12 }

        let result = dtor data
        Assert.AreEqual(data.Sm, unbox<string> result.[0])
        Assert.AreEqual(data.Im, unbox<int> result.[1])
        Assert.AreEqual(data.Fm, unbox<float> result.[2])

    [<Test>]
    let ``should construct Internal record``() =
        let ctor = FSharpValue.PreComputeRecordConstructorFast(typeof<Internal>, BindingFlags.NonPublic)
        let data = [| box "string" |]

        let result = ctor data
        let primResult = result :?> Internal
        Assert.AreEqual("string", primResult.Si)


    [<Test>]
    let ``should read Internal record``() =
        let dtor = FSharpValue.PreComputeRecordReaderFast(typeof<Internal>, BindingFlags.NonPublic)
        let data = { Si = "string" }

        let result = dtor data
        Assert.AreEqual(data.Si, unbox<string> result.[0])

    [<Test>]
    let ``should construct Private record``() =
        let ctor = FSharpValue.PreComputeRecordConstructorFast(typeof<Private>, BindingFlags.NonPublic)
        let data = [| box 1.2 |]

        let result = ctor data
        let primResult = result :?> Private
        Assert.AreEqual(1.2, primResult.Fp)


    [<Test>]
    let ``should read Private record``() =
        let dtor = FSharpValue.PreComputeRecordReaderFast(typeof<Private>, BindingFlags.NonPublic)
        let data = { Fp = 2.1 }

        let result = dtor data
        Assert.AreEqual(data.Fp, unbox<float> result.[0])

    [<Test>]
    let ``should construct Generic record``() =
        let ctor = FSharpValue.PreComputeRecordConstructorFast(typeof<Generic<int>>)
        let data = [| box 1 |]

        let result = ctor data :?> Generic<int>
        Assert.AreEqual(1, result.Generic)


    [<Test>]
    let ``should read Generic record``() =
        let dtor = FSharpValue.PreComputeRecordReaderFast(typeof<Generic<int>>)
        let data = { Generic = 2 }

        let result = dtor data
        Assert.AreEqual(data.Generic, unbox<int> result.[0])
        
module ReflectionUnionTest =

    open NUnit.Framework
    open Microsoft.FSharp.Reflection
    open FSharpx.Reflection
    open System.Reflection //for bindingflags

    open FSharpx.Reflection

    type Straightforward =
        | Empty
        | S of string
        | I of int * string

    type Singleton = Single

    type internal Internal = internal | Si of string

    type internal Private = private | Fp of float

    type Generic<'a> = | Generic of 'a

    type LargeDU = A | B | C | E | F | H | Eye | J | K | L | M

    let straightforwardCases = FSharpType.GetUnionCases typeof<Straightforward>
    let singletonCase = (FSharpType.GetUnionCases typeof<Singleton>).[0]
    let internalCase = (FSharpType.GetUnionCases(typeof<Internal>, BindingFlags.NonPublic)).[0]
    let privateCase = (FSharpType.GetUnionCases(typeof<Private>, BindingFlags.NonPublic)).[0]
    let genericCase = (FSharpType.GetUnionCases typeof<Generic<int>>).[0]


    let getTagReaderComparer<'T> bindingFlags =
        let fastReader = FSharpValue.PreComputeUnionTagReaderFast(typeof<'T>, ?bindingFlags = bindingFlags)
        let reader = FSharpValue.PreComputeUnionTagReader(typeof<'T>, ?bindingFlags = bindingFlags)
        fun (x : 'T) -> Assert.AreEqual(reader x, fastReader x)

    [<Test>]
    let ``should construct Straightforward union case without arguments``() =
        let ctor = FSharpValue.PreComputeUnionConstructorFast straightforwardCases.[0]
        let resultObj = ctor [| |]
        let result = resultObj :?> Straightforward
        Assert.AreEqual(Empty, result)

    [<Test>]
    let ``should read Straightforward union case without arguments``() =
        let dtor = FSharpValue.PreComputeUnionReaderFast straightforwardCases.[0]        
        let result = dtor Empty
        Assert.IsEmpty result

    [<Test>]
    let ``should construct Straightforward union case with one argument``() =
        let ctor = FSharpValue.PreComputeUnionConstructorFast straightforwardCases.[1]
        let resultObj = ctor [| "string" |]
        let result = resultObj :?> Straightforward
        Assert.AreEqual(S "string", result)

    [<Test>]
    let ``should read Straightforward union case with one argument``() =
        let dtor = FSharpValue.PreComputeUnionReaderFast straightforwardCases.[1]    
        let result = dtor (S "string")
        Assert.AreEqual([| box "string" |], result)

    [<Test>]
    let ``should construct Straightforward union case with two arguments``() =
        let ctor = FSharpValue.PreComputeUnionConstructorFast straightforwardCases.[2]
        let resultObj = ctor [| 12; "string" |]
        let result = resultObj :?> Straightforward
        Assert.AreEqual(I (12,"string"), result)

    [<Test>]
    let ``should read Straightforward union case with two arguments``() =
        let dtor = FSharpValue.PreComputeUnionReaderFast straightforwardCases.[2]    
        let result = dtor (I (12,"string"))
        Assert.AreEqual([| box 12; box "string" |], result)

    [<Test>]
    let ``should construct Singleton union case without arguments``() =
        let ctor = FSharpValue.PreComputeUnionConstructorFast singletonCase
        let resultObj = ctor [| |]
        let result = resultObj :?> Singleton
        Assert.AreEqual(Single, result)
        Assert.AreSame(Single, result) //singletons union cases are singletons

    [<Test>]
    let ``should read Singleton union case without arguments``() =
        let dtor = FSharpValue.PreComputeUnionReaderFast singletonCase
        let result = dtor Single
        Assert.IsEmpty result
    
    [<Test>]
    let ``should construct Internal union case``() =
        let ctor = FSharpValue.PreComputeUnionConstructorFast(internalCase, BindingFlags.NonPublic)
        let resultObj = ctor [| "string" |]
        let result = resultObj :?> Internal
        Assert.AreEqual(Si "string", result)

    [<Test>]
    let ``should read Internal union case``() =
        let dtor = FSharpValue.PreComputeUnionReaderFast(internalCase, BindingFlags.NonPublic)
        let result = dtor (Si "string")
        Assert.AreEqual([| box "string" |], result)

    [<Test>]
    let ``should not read Internal union case with BindingFlags.Public``() =
        Assert.Throws<System.ArgumentException>(new TestDelegate(fun () -> FSharpValue.PreComputeUnionReaderFast(internalCase) |> ignore))
        |> ignore

    [<Test>]
    let ``should construct Private union case``() =
        let ctor = FSharpValue.PreComputeUnionConstructorFast(privateCase, BindingFlags.NonPublic)
        let resultObj = ctor [| 1.23 |]
        let result = resultObj :?> Private
        Assert.AreEqual(Fp 1.23, result)

    [<Test>]
    let ``should read Private union case``() =
        let dtor = FSharpValue.PreComputeUnionReaderFast(privateCase, BindingFlags.NonPublic)
        let result = dtor (Fp 1.12)
        Assert.AreEqual([| box 1.12 |], result)

    [<Test>]
    let ``should construct Generic union case``() =
        let ctor = FSharpValue.PreComputeUnionConstructorFast genericCase
        let resultObj = ctor [| 2 |]
        let result = resultObj :?> Generic<int>
        Assert.AreEqual(Generic 2, result)

    [<Test>]
    let ``should read Generic union case``() =
        let dtor = FSharpValue.PreComputeUnionReaderFast genericCase       
        let result = dtor (Generic 2)
        Assert.AreEqual([| box 2 |], result)
        

    [<Test>]
    let ``should read Straightforward union tag``() =
        let tester = getTagReaderComparer<Straightforward> None
        tester <| Empty
        tester <| S ""
        tester <| I(42,"")

    [<Test>]
    let ``should read Internal union tag`` () =
        let tester = getTagReaderComparer<Internal> (Some BindingFlags.NonPublic)
        tester <| Si ""

    [<Test>]
    let ``should read Private union tag`` () =
        let tester = getTagReaderComparer<Private> (Some BindingFlags.NonPublic)
        tester <| Fp 42.0

    [<Test>]
    let ``should read Generic union tag`` () =
        let tester = getTagReaderComparer<Generic<int * string>> (Some BindingFlags.NonPublic)
        tester <| Generic (42,"")

    [<Test>]
    let ``should read Large union tag`` () =
        let tester = getTagReaderComparer<LargeDU> (Some BindingFlags.NonPublic)
        tester A ; tester C ; tester H ; tester M


module ReflectionExceptionTest =

    open System.Reflection
    open NUnit.Framework
    open FSharpx.Reflection

    exception SimpleExn

    exception PublicExn of string * obj

    exception private PrivateExn of string * int

    [<Test>]
    let ``should construct simple exception`` () =
        let ctor = FSharpValue.PreComputeExceptionConstructorFast typeof<SimpleExn>
        Assert.AreEqual(SimpleExn, ctor [||])

    [<Test>]
    let ``should construct public exception`` () =
       let ctor = FSharpValue.PreComputeExceptionConstructorFast typeof<PublicExn> 
       Assert.AreEqual(PublicExn("", box 42), ctor [| box "" ; box 42 |])

    [<Test>]
    let ``should construct private exception`` () =
        let ctor = FSharpValue.PreComputeExceptionConstructorFast(typeof<PrivateExn>, BindingFlags.NonPublic)
        Assert.AreEqual(PrivateExn("", 42), ctor [| box "" ; box 42 |])

    [<Test>]
    let ``should read simple exception`` () =
        let dtor = FSharpValue.PreComputeExceptionReaderFast typeof<SimpleExn>
        Assert.AreEqual([||], dtor SimpleExn)

    [<Test>]
    let ``should read pubic exception`` () =
        let dtor = FSharpValue.PreComputeExceptionReaderFast typeof<PublicExn>
        Assert.AreEqual([|box "" ; box 42|], dtor <| PublicExn("", box 42))

    [<Test>]
    let ``should read private exception`` () =
        let dtor = FSharpValue.PreComputeExceptionReaderFast(typeof<PrivateExn>, BindingFlags.NonPublic)
        Assert.AreEqual([|box ""; box 42|], dtor <| PrivateExn("", 42))


module ReflectionTupleTest =

    open System
    open NUnit.Framework
    open FSharpx.Reflection
    
    [<Test>]
    let ``should construct unitary tuple`` () =
        let ctor = FSharpValue.PreComputeTupleConstructorFast typeof<Tuple<int>>
        Assert.AreEqual(Tuple<_>(42), ctor [| box 42 |])

    [<Test>]
    let ``should read unitary tuple`` () =
        let dtor = FSharpValue.PreComputeTupleReaderFast typeof<Tuple<int>>
        Assert.AreEqual([| box 42 |], dtor <| Tuple<_>(42))

    [<Test>]
    let ``should construct pair`` () =
        let ctor = FSharpValue.PreComputeTupleConstructorFast typeof<int * string>
        Assert.AreEqual((42,""), ctor [| box 42 ; "" |])

    [<Test>]
    let ``should read pair`` () =
        let dtor = FSharpValue.PreComputeTupleReaderFast typeof<int * string>
        Assert.AreEqual([| box 42 ; box "" |], dtor (42, ""))

    [<Test>]
    let ``should construct large tuple`` () =
        let ctor = FSharpValue.PreComputeTupleConstructorFast typeof<int * string * bool * (int * string) * int * int * int * int * int>
        Assert.AreEqual((42,"",false,(10," "),1,2,3,4,5), ctor [| box 42; "" ; false ; (10," ") ; 1 ; 2 ; 3 ; 4 ; 5 |])

    [<Test>]
    let ``should read large tuple`` () =
        let dtor = FSharpValue.PreComputeTupleReaderFast typeof<int * string * bool * (int * string) * int * int * int * int * int>
        Assert.AreEqual(([| 42; "" ; false ; (10," ") ; 1 ; 2 ; 3 ; 4 ; 5 |] : obj []), dtor (42,"",false,(10," "),1,2,3,4,5))

//make sure it is faster!
module ReflectionPerformanceTest =

    open NUnit.Framework
    open Microsoft.FSharp.Reflection
    open System.Reflection //for bindingflags
    open FSharpx.Reflection

    open FSharpx.Reflection

    let [<Literal>] numRepeats = 400000
    let [<Literal>] expectedImprovementTestor = 1L //conservative

    type MyRecord =
        { S : string
          i : int
          f : float }

    let repeat f = 
        let stopwatch = new System.Diagnostics.Stopwatch()
        stopwatch.Start()
        for i in 1..numRepeats do f i |> ignore
        stopwatch.Stop()
        stopwatch.ElapsedMilliseconds

    let fastRecordCtor = FSharpValue.PreComputeRecordConstructorFast typeof<MyRecord>
    let standardRecordCtor = FSharpValue.PreComputeRecordConstructor typeof<MyRecord>

    let fastRecordReader = FSharpValue.PreComputeRecordReaderFast typeof<MyRecord>
    let standardRecordReader = FSharpValue.PreComputeRecordReader typeof<MyRecord>

    [<Test>]
    let ``should construct record faster than F# reflection``() =
        let fast = repeat (fun i -> fastRecordCtor [| "2"; i; 3. |] :?> MyRecord)
        let standard = repeat (fun i -> standardRecordCtor [| "2"; i; 3. |] :?> MyRecord)
        printf "FSharpx is %ix faster" (standard/fast)
        Assert.True(fast < standard/expectedImprovementTestor)

    [<Test>]
    let ``should read record faster than F# reflection``() =
        let fast = repeat (fun i -> fastRecordReader { S = "2"; i = i; f = 3.0 })
        let standard = repeat (fun i -> standardRecordReader { S = "2"; i = i; f = 3.0 })
        printf "FSharpx is %ix faster" (standard/fast)       
        Assert.True(fast < standard/expectedImprovementTestor)

    type MyUnion =
        | Empty
        | One of int
        | Two of string * int

    let unionCases = FSharpType.GetUnionCases typeof<MyUnion>

    let fastUnionCtor = FSharpValue.PreComputeUnionConstructorFast unionCases.[2]
    let standardUnionCtor = FSharpValue.PreComputeUnionConstructor unionCases.[2]

    let fastUnionReader = FSharpValue.PreComputeUnionReaderFast unionCases.[2]
    let standardUnionReader = FSharpValue.PreComputeUnionReader unionCases.[2]

    let standardTagReader = FSharpValue.PreComputeUnionTagReader typeof<MyUnion>
    let fastTagReader = FSharpValue.PreComputeUnionTagReaderFast typeof<MyUnion>

    [<Test>]
    let ``should construct 2-case union faster than F# reflection``() =
        let fast = repeat (fun i -> fastUnionCtor [| "3"; i |] :?> MyUnion)
        let standard = repeat (fun i -> standardUnionCtor [| "3"; i |] :?> MyUnion)
        printf "Fsharpx is %ix faster" (standard/fast)
        Assert.True(fast < standard/expectedImprovementTestor)

    [<Test>]
    let ``should read 2-case union faster than F# reflection``() =
        let fast = repeat (fun i -> fastUnionReader (Two ("s",i)))
        let standard = repeat (fun i -> standardUnionReader (Two ("s",i)))
        printf "FSharpx is %ix faster" (standard/fast)
        Assert.True(fast < standard/expectedImprovementTestor)

    [<Test>]
    let ``should read union tags faster than F# reflection`` () =
        let fast = repeat (fun i -> fastTagReader (Two ("s",i)))
        let standard = repeat (fun i -> standardTagReader (Two ("s",i)))
        printf "FSharpx is %ix faster" (standard/fast)
        Assert.True(fast < standard/expectedImprovementTestor)


    let standardTupleCtor = FSharpValue.PreComputeTupleConstructor typeof<int * int * string>
    let fastTupleCtor = FSharpValue.PreComputeTupleConstructorFast typeof<int * int * string>
    
    let standardTupleReader = FSharpValue.PreComputeTupleReader typeof<int * int * string>
    let fastTupleReader = FSharpValue.PreComputeTupleReaderFast typeof<int * int * string>

    [<Test>]
    let ``should construct tuples faster than F# reflection`` () =
        let fast = repeat (fun i -> fastTupleCtor [|i ; i ; "s"|])
        let standard = repeat (fun i -> standardTupleCtor [|i ; i ; "s"|])
        printf "FSharpx is %ix faster" (standard/fast)
        Assert.True(fast < standard/expectedImprovementTestor)

    [<Test>]
    let ``should read tuples faster than F# reflection`` () =
        let fast = repeat (fun i -> fastTupleReader (i,i,"s"))
        let standard = repeat (fun i -> standardTupleReader (i,i,"s"))
        printf "FSharpx is %ix faster" (standard/fast)
        Assert.True(fast < standard/expectedImprovementTestor)


        
        
