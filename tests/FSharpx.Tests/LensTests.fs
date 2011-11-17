module FSharpx.LensTests

open System
open NUnit.Framework
open FSharpx.Lens.Operators

type Car = {
    Make: string
    Model: string
    Mileage: int
} with 
    static member make = 
        { Lens.Get = fun (x: Car) -> x.Make
          Set = fun v (x: Car) -> { x with Make = v } }
    static member model = 
        { Lens.Get = fun (x: Car) -> x.Model
          Set = fun v (x: Car) -> { x with Model = v } }
    static member mileage = 
        { Lens.Get = fun (x: Car) -> x.Mileage
          Set = fun v (x: Car) -> { x with Mileage = v } }

type Editor = {
    Name: string
    Salary: int
    Car: Car
} with
    static member salary =
        { Lens.Get = fun (x: Editor) -> x.Salary
          Set = fun v (x: Editor) -> { x with Salary = v } }
    static member car = 
        { Lens.Get = fun (x: Editor) -> x.Car
          Set = fun v (x: Editor) -> { x with Car = v } }

type Book = {
    Name: string
    Author: string
    Editor: Editor
} with
    static member editor =
        { Lens.Get = fun (x: Book) -> x.Editor
          Set = fun v (x: Book) -> { x with Editor = v } }

let giveRaise v = Lens.update ((+) v) Editor.salary

let hondaAccura = { Make = "Honda"; Model = "Accura"; Mileage = 1000 }
let bmwE90 = { Make = "BMW"; Model = "E90"; Mileage = 0 }
let tom = { Name = "Tom"; Salary = 4000; Car = bmwE90 }
let dick = { Name = "Dick"; Salary = 3000; Car = hondaAccura }
let aBook = { Name = "Ficciones"; Author = "Jorge Luis Borges"; Editor = tom }

[<Test>]
let update() =
    let tom1 = { tom with Salary = tom.Salary + 1000 }
    let tom2 = tom |> Lens.update ((+) 1000) Editor.salary
    let tom3 = tom |> Editor.salary.Update ((+) 1000)
    Assert.AreEqual(tom1, tom2)
    Assert.AreEqual(tom1, tom3)

[<Test>]
let updateCompose() =
    let tom1 = { tom with Car = { tom.Car with Model = "Z4" } }
    let EditorCarModel = Editor.car >>| Car.model
    let EditorCarModelAlt = Car.model |<< Editor.car
    let tom2 = tom |> EditorCarModel.Set "Z4"
    let tom3 = tom |> EditorCarModelAlt.Set "Z4"
    let tom4 = EditorCarModel |> Lens.set "Z4" tom
    let all = [tom1;tom2;tom3;tom4]
    for i in all do for j in all do Assert.AreEqual(i, j)

[<Test>]
let pluseq() =
    let giveRaise = Editor.salary += 1000
    let tom1 = { tom with Salary = tom.Salary + 1000 }
    let tom2 = tom |> Editor.salary += 1000
    let tom3 = giveRaise tom
    Assert.AreEqual(tom1, tom2)
    Assert.AreEqual(tom1, tom3)

[<Test>]
let setValueOperator() =
    let tom1 = { tom with Salary = 1000 }
    let tom2 = tom |> (Editor.salary =! 1000)
    Assert.AreEqual(tom1, tom2)

[<Test>]
let stateMonad() =
    let getSalary = Lens.getState Editor.salary
    let modSalary = Lens.updateState Editor.salary
    let setSalary = Lens.setState Editor.salary
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
            do! Editor.salary =! 1000
            do! Editor.salary += 100
        }
    let tom1 = modify tom |> snd
    Assert.AreEqual(1100, tom1.Salary)

[<Test>]
let stateMonadOperators2() =
    let modify =
        State.state {
            let! oldSalary = Lens.getState Editor.salary
            do! Editor.salary += 1000
            return oldSalary
        }
    let oldSalary, promotedTom = modify tom
    printfn "Tom used to make %d, after promotion he now makes %d" oldSalary promotedTom.Salary
    Assert.AreEqual(4000, oldSalary)
    Assert.AreEqual(5000, promotedTom.Salary)

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

let checkLens name lens = 
    let tname = sprintf "%s: %s" name
    FsCheck.Check.Quick (tname "GetSet", LensProperties.GetSet lens)
    FsCheck.Check.Quick (tname "SetGet", LensProperties.SetGet lens)
    FsCheck.Check.Quick (tname "SetSet", LensProperties.SetSet lens)

[<Test>] 
let LensId() = checkLens "Id" Lens.id

[<Test>] 
let LensFst() = checkLens "fst" Lens.fst

[<Test>] 
let LensSnd() = checkLens "snd" Lens.snd

[<Test>] 
let LensFstSnd() = checkLens "fst composed with snd" (Lens.fst >>| Lens.snd)

[<Test>]
let LensIgnore() = checkLens "ignore" Lens.ignore

[<Test>]
let LensCodiag() = checkLens "codiag" Lens.codiag

[<Test>]
let LensChoice() = checkLens "choice" (Car.make .|. Car.model)

[<Test>]
let LensProduct() = checkLens "product" (Car.make *** Car.model)

type Product = {
    Name: string
    PriceWithTax: int
    PriceWithoutTax: int
} with 
    static member name =
        { Get = fun (x: Product) -> x.Name 
          Set = fun v (x: Product) -> { x with Name = v } }
    static member priceWithTax =
        { Get = fun (x: Product) -> x.PriceWithTax
          Set = fun v (x: Product) -> { x with PriceWithTax = v } }
    static member priceWithoutTax =
        { Get = fun (x: Product) -> x.PriceWithoutTax
          Set = fun v (x: Product) -> { x with PriceWithoutTax = v } }

let productPrice = 
    Lens.cond (Product.name.Get >> Strings.contains "book") 
        Product.priceWithoutTax // true
        Product.priceWithTax // false

[<Test>]
let LensCond() = checkLens "cond" productPrice