namespace $rootnamespace$.Tests

open NUnit.Framework
open FsUnit

type LightBulb(state) =
    member x.On = state
    override x.ToString() =
        match x.On with
        | true  -> "On"
        | false -> "Off"

[<TestFixture>] 
type ``Given a LightBulb that has had its state set to true`` ()=
    let lightBulb = new LightBulb(true)

    [<Test>] member test.
     ``when I ask whether it is On it answers true.`` ()=
            lightBulb.On |> should be True

    [<Test>] member test.
     ``when I convert it to a string it becomes "On".`` ()=
            string lightBulb |> should equal "On"

[<TestFixture>]
type ``Given a LightBulb that has had its state set to false`` ()=
    let lightBulb = new LightBulb(false)
    
    [<Test>] member test.
     ``when I ask whether it is On it answers false.`` ()=
            lightBulb.On |> should be False
    
    [<Test>] member test.
     ``when I convert it to a string it becomes "Off".`` ()=
            string lightBulb |> should equal "Off"