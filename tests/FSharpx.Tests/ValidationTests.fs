﻿module FSharpx.Tests.ValidationTests

open System
open FsUnitTyped
open Microsoft.FSharp.Core
open FSharpx.Collections
open FSharpx.CSharpTests
open FSharpx
open FSharpx.Functional
open FSharpx.Choice
#nowarn "FS0044"
open FSharpx.Validation
open FSharpx.Nullable
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


let greaterThan o = validator ((<?) o)

let lengthNotEquals l = validator (fun (x: string) -> x.Length <> l) "Invalid length"

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
        errors.Length |> shouldEqual 3
        errors |> shouldContain "Cost for product 'Bar' can't be negative"
        errors |> shouldContain "Product name can't be null"
        errors |> shouldContain "Surname can't be foo"

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
      errors.Length |> shouldEqual 1
      errors |> shouldContain "Surname can't be null"

[<Test>]
let ``validation with sum monoid``() =
    let v = Validation.CustomValidation (Monoid.sum())
    // count the number of broken rules
    let intValidator x = Choice.mapSecond (konst 1) x
    let notEqual a = notEqual a "" >> intValidator
    let lengthNotEquals l = lengthNotEquals l >> intValidator
    let validateString x = 
        Choice.returnM x
        |> v.apl (notEqual "hello" x)
        |> v.apl (lengthNotEquals 5 x)
    match validateString "hello" with
    | Success c -> failwithf "Valid string: %s" c
    | Failure e -> Assert.AreEqual(2, e)

[<Test>]
let ``validation with unit monoid``() =
    // using the unit monoid to avoid the overhead of concatenating error lists.
    let v = Validation.CustomValidation Monoid.unit

    // convert validator errors to unit
    let unitValidator x = Choice.mapSecond ignore x
    let notEqual a = notEqual a "" >> unitValidator
    let lengthNotEquals l = lengthNotEquals l >> unitValidator
    let validateString x = 
        Choice.returnM x
        |> v.apl (notEqual "hello" x)
        |> v.apl (lengthNotEquals 5 x)
    match validateString "hello" with
    | Success c -> failwithf "Valid string: %s" c
    | Failure () -> ()

[<Test>]
let ``using sequenceIgnore``() =
    let vsError = [ Choice1Of2 "ok"; Choice2Of2 (NonEmptyList.singleton "err") ]
    let vsOk = [ Choice1Of2 "ok1"; Choice1Of2 "ok2" ]

    let vError = Validation.sequenceIgnore vsError
    match vError with
    | Choice2Of2 errors ->
        CollectionAssert.AreEqual(errors, [ "err" ])
    | _ ->
        failwith "Validation must not succeed if there are errors"

    let vOk = Validation.sequenceIgnore vsOk
    match vOk with
    | Choice1Of2 () -> ()
    | Choice2Of2 _ -> failwith "Validation failed on success values"

[<Test>]
let ``using mapMIgnore``() =
    let okAndErr = [ "ok"; "err" ]
    let oks = [ "ok1"; "ok2" ]

    let validate = validator ((<>) "err") "error!"

    let vError = Validation.mapMIgnore validate okAndErr
    let vOk = Validation.mapMIgnore validate oks

    match vError with
    | Choice2Of2 errors ->
        CollectionAssert.AreEqual(errors, [ "error!" ])
    | _ ->
        failwith "Validation must not succeed if there are errors"

    match vOk with
    | Choice1Of2 () -> ()
    | Choice2Of2 _ -> failwith "Validation failed on success values"

