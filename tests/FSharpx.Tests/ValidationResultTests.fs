module FSharpx.Tests.ValidationResultTests

// copy of ValidationTests adjusted for Validation.Result


open FsUnitTyped
open Microsoft.FSharp.Core
open FSharpx.Collections
open FSharpx.CSharpTests
open FSharpx
open FSharpx.Validation.Result
open FSharpx.Nullable
open NUnit.Framework

let validator pred error value =
    if pred value
        then Ok value
        else Result.Error (NonEmptyList.singleton error)

let (==) = LanguagePrimitives.PhysicalEquality
let inline (!=) a b = not (a == b)

let nonNull e = validator ((!=) null) e
let notEqual a = validator ((<>) a)

let validateAddressLines =
    validator 
        (fun (a: Address) -> a.Line1 != null || a.Line2 == null) 
        "Line1 is empty but Line2 is not"

let validateAddress (a: Address) = 
    Ok a
    <* nonNull "Post code can't be null" a.Postcode
    <* validateAddressLines a


let greaterThan o = validator ((<?) o)

let lengthNotEquals l = validator (fun (x: string) -> x.Length <> l) "Invalid length"

let validateOrder (o: Order) =
    let nameNotNull = nonNull "Product name can't be null" o.ProductName
    let positiveCost n = greaterThan (0m).n (sprintf "Cost for product '%s' can't be negative" n) o.Cost
    Result.bind positiveCost nameNotNull |> Result.map (konst o)

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
        Ok customer
        <* nonNull "Surname can't be null" customer.Surname
        <* notEqual "foo" "Surname can't be foo" customer.Surname
        <* validateAddress customer.Address
        <* validateOrders customer.Orders
    match result with
    | Ok c -> failwithf "Valid customer: %A" c
    | Error errors -> 
        printfn "Invalid customer. Errors:\n%A" errors
        errors.Length |> shouldEqual 3
        errors |> shouldContain "Cost for product 'Bar' can't be negative"
        errors |> shouldContain "Product name can't be null"
        errors |> shouldContain "Surname can't be foo"

[<Test>]
let ``using ap``() =
  let customer = Customer()
  let result = 
    Ok (konst2 customer)
    |> ap (nonNull "Surname can't be null" customer.Surname)
    |> ap (notEqual "foo" "Surname can't be foo" customer.Surname)
  match result with
  | Ok c -> failwithf "Valid customer: %A" c
  | Error errors -> 
      printfn "Invalid customer. Errors:\n%A" errors
      errors.Length |> shouldEqual 1
      errors |> shouldContain "Surname can't be null"

[<Test>]
let ``validation with sum monoid``() =
    let v = CustomValidation (Monoid.sum())
    // count the number of broken rules
    let intValidator x = Result.mapError (konst 1) x
    let notEqual a = notEqual a "" >> intValidator
    let lengthNotEquals l = lengthNotEquals l >> intValidator
    let validateString x = 
        Ok x
        |> v.apl (notEqual "hello" x)
        |> v.apl (lengthNotEquals 5 x)
    match validateString "hello" with
    | Ok c -> failwithf "Valid string: %s" c
    | Result.Error e -> Assert.AreEqual(2, e)

[<Test>]
let ``validation with unit monoid``() =
    // using the unit monoid to avoid the overhead of concatenating error lists.
    let v = CustomValidation Monoid.unit

    // convert validator errors to unit
    let unitValidator x = Result.mapError ignore x
    let notEqual a = notEqual a "" >> unitValidator
    let lengthNotEquals l = lengthNotEquals l >> unitValidator
    let validateString x = 
        Ok x
        |> v.apl (notEqual "hello" x)
        |> v.apl (lengthNotEquals 5 x)
    match validateString "hello" with
    | Ok c -> failwithf "Valid string: %s" c
    | Result.Error () -> ()

[<Test>]
let ``using sequenceIgnore``() =
    let vsError = [ Ok "ok"; Error (NonEmptyList.singleton "err") ]
    let vsOk = [ Ok "ok1"; Ok "ok2" ]

    let vError = sequenceIgnore vsError
    match vError with
    | Error errors ->
        CollectionAssert.AreEqual(errors, [ "err" ])
    | _ ->
        failwith "Validation must not succeed if there are errors"

    let vOk = sequenceIgnore vsOk
    match vOk with
    | Ok () -> ()
    | Error _ -> failwith "Validation failed on success values"

[<Test>]
let ``using mapMIgnore``() =
    let okAndErr = [ "ok"; "err" ]
    let oks = [ "ok1"; "ok2" ]

    let validate = validator ((<>) "err") "error!"

    let vError = mapMIgnore validate okAndErr
    let vOk = mapMIgnore validate oks

    match vError with
    | Error errors ->
        CollectionAssert.AreEqual(errors, [ "error!" ])
    | _ ->
        failwith "Validation must not succeed if there are errors"

    match vOk with
    | Ok () -> ()
    | Error _ -> failwith "Validation failed on success values"

