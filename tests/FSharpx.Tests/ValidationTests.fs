module FSharpx.Tests.ValidationTests

open System
open FsUnit
open Microsoft.FSharp.Core
open FSharpx
open FSharpx.CSharpTests
open FSharpx.Choice
open FSharpx.Validation
open NUnit.Framework

let validator pred error value =
    if pred value
        then Choice1Of2 value
        else Choice2Of2 (NonEmptyList.singleton error)

let (==) = LanguagePrimitives.PhysicalEquality
let inline (!=) a b = not (a == b)

let nonNull e = validator ((!=) null) e
let notEqual a = validator ((<>) a)

let validateAddressLines =
    validator 
        (fun (a: Address) -> a.Line1 != null || a.Line2 == null) 
        "Line1 is empty but Line2 is not"

let validateAddress (a: Address) = 
    returnM a
    <* nonNull "Post code can't be null" a.Postcode
    <* validateAddressLines a

open FSharpx.Nullable

let greaterThan o = validator ((<?) o)

let validateOrder (o: Order) =
    let nameNotNull = nonNull "Product name can't be null" o.ProductName
    let positiveCost n = greaterThan (0m).n (sprintf "Cost for product '%s' can't be negative" n) o.Cost
    nameNotNull >>= positiveCost |> Choice.map (konst o)

(*    validation {
        let! name = nonNull "Product name can't be null" o.ProductName
        let! _ = greaterThan (0m).n (sprintf "Cost for product '%s' must be positive" name) o.Cost
        return o
    } *)
    

let validateOrders c = seqValidator validateOrder c
    
[<Test>]
let ValidateCustomer() = 
    let customer = 
        Customer(
            Surname = "foo",
            Address = Address(Postcode = "1424"),
            Orders = ResizeArray([
                                    Order(ProductName = "Foo", Cost = (5m).n)
                                    Order(ProductName = "Bar", Cost = (-1m).n)
                                    Order(ProductName = null , Cost = (-1m).n)
                     ]))
    let result = 
        returnM customer
        <* nonNull "Surname can't be null" customer.Surname
        <* notEqual "foo" "Surname can't be foo" customer.Surname
        <* validateAddress customer.Address
        <* validateOrders customer.Orders
    match result with
    | Success c -> failwithf "Valid customer: %A" c
    | Failure errors -> 
        printfn "Invalid customer. Errors:\n%A" errors
        errors.Length |> should equal 3
        errors |> should contain "Cost for product 'Bar' can't be negative"
        errors |> should contain "Product name can't be null"
        errors |> should contain "Surname can't be foo"

[<Test>]
let ``using ap``() =
  let customer = Customer()
  let result = 
    returnM (konst2 customer)
    |> Validation.ap (nonNull "Surname can't be null" customer.Surname)
    |> Validation.ap (notEqual "foo" "Surname can't be foo" customer.Surname)
  match result with
  | Success c -> failwithf "Valid customer: %A" c
  | Failure errors -> 
      printfn "Invalid customer. Errors:\n%A" errors
      errors.Length |> should equal 1
      errors |> should contain "Surname can't be null"

[<Test>]
let ``validation with monoid``() =
  let v = Validation.CustomValidation(Monoid.IntSumMonoid)
  // count the number of broken rules
  let validator pred value =
      if pred value
          then Choice1Of2 value
          else Choice2Of2 1
  let notEqual a = validator ((<>) a)
  let lengthNotEquals l = validator (fun (x: string) -> x.Length <> l)
  let validateString x = 
    Choice.returnM x
    |> v.apl (notEqual "hello" x)
    |> v.apl (lengthNotEquals 5 x)
  match validateString "hello" with
  | Success c -> failwithf "Valid string: %s" c
  | Failure e -> Assert.AreEqual(2, e)

