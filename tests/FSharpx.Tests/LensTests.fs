﻿module FSharpx.LensTests

open NUnit.Framework

open FSharpx.Lens.Operators

type Car = {
    Make: string
    Model: string
} with 
    static member make = 
        { Lens.Get = fun (x: Car) -> x.Make
          Set = fun v (x: Car) -> { x with Make = v } }
    static member model = 
        { Lens.Get = fun (x: Car) -> x.Model
          Set = fun v (x: Car) -> { x with Model = v } }

type Employee = {
    Name: string
    Salary: int
    Car: Car
} with
    static member salary =
        { Lens.Get = fun (x: Employee) -> x.Salary
          Set = fun v (x: Employee) -> { x with Salary = v } }
    static member car = 
        { Lens.Get = fun (x: Employee) -> x.Car
          Set = fun v (x: Employee) -> { x with Car = v } }

let giveRaise v = Lens.update ((+) v) Employee.salary

let hondaAccura = { Make = "Honda"; Model = "Accura" }
let bmwE90 = { Make = "BMW"; Model = "E90" }
let tom = { Name = "Tom"; Salary = 4000; Car = bmwE90 }
let dick = { Name = "Dick"; Salary = 3000; Car = hondaAccura }

[<Test>]
let update() =
    let tom1 = { tom with Salary = tom.Salary + 1000 }
    let tom2 = tom |> Lens.update ((+) 1000) Employee.salary
    let tom3 = tom |> Employee.salary.Update ((+) 1000)
    Assert.AreEqual(tom1, tom2)
    Assert.AreEqual(tom1, tom3)

[<Test>]
let updateCompose() =
    let tom1 = { tom with Car = { tom.Car with Model = "Z4" } }
    let employeeCarModel = Employee.car >>| Car.model
    let employeeCarModelAlt = Car.model |<< Employee.car
    let tom2 = tom |> employeeCarModel.Set "Z4"
    let tom3 = tom |> employeeCarModelAlt.Set "Z4"
    let tom4 = employeeCarModel |> Lens.set "Z4" tom
    let all = [tom1;tom2;tom3;tom4]
    for i in all do for j in all do Assert.AreEqual(i, j)

[<Test>]
let pluseq() =
    let giveRaise = Employee.salary += 1000
    let tom1 = { tom with Salary = tom.Salary + 1000 }
    let tom2 = tom |> Employee.salary += 1000
    Assert.AreEqual(tom1, tom2)

[<Test>]
let setValueOperator() =
    let tom1 = { tom with Salary = 1000 }
    let tom2 = tom |> (Employee.salary =! 1000)
    Assert.AreEqual(tom1, tom2)

[<Test>]
let stateMonad() =
    let getSalary = Lens.getState Employee.salary
    let modSalary = Lens.updateState Employee.salary
    let setSalary = Lens.setState Employee.salary
    let modify = 
        State.state {
            let! s = getSalary
            do! setSalary 1000
            do! modSalary ((+) 100)
            return s
        }
    let r,tom1 = modify tom
    Assert.AreEqual(tom.Salary, r)
    Assert.AreEqual(1100, tom1.Salary)

open FSharpx.Lens.StateOperators

[<Test>]
let stateMonadOperators() =
    let modify = 
        State.state {
            do! Employee.salary =! 1000
            do! Employee.salary += 100
        }
    let tom1 = modify tom |> snd
    Assert.AreEqual(1100, tom1.Salary)

type LensProperties =
    /// If the view does not change, neither should the source.
    static member GetSet (l: Lens<_,_>) a = l.Set (l.Get a) a = a

    /// Updates should be "translated exactly" - i.e., to a source
    /// structure for which get yields exactly the updated target structure
    static member SetGet (l: Lens<_,_>) a b = l.Get (l.Set b a) = b

    /// Each update should completely overwrite the effect of the
    /// previous one. Thus, the effect of two sets in a row
    /// should be the same as just the second.
    static member SetSet (l: Lens<_,_>) a b c =
        let p = l.Set b (l.Set a c)
        let s = l.Set b c
        p = s

let checkLens lens = 
    FsCheck.Check.Quick (LensProperties.GetSet lens)
    FsCheck.Check.Quick (LensProperties.SetGet lens)
    FsCheck.Check.Quick (LensProperties.SetSet lens)

[<Test>] 
let LensId() = checkLens Lens.id

[<Test>] 
let LensFst() = checkLens Lens.fst

[<Test>] 
let LensSnd() = checkLens Lens.snd

[<Test>] 
let LensFstSnd() = checkLens (Lens.fst >>| Lens.snd)

[<Test>]
let LensIgnore() = checkLens Lens.ignore

[<Test>]
let LensCodiag() = checkLens Lens.codiag