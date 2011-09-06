module FSharpx.Tests.Validation

open System
open FSharpx
open FSharpx.CSharpTests
open FSharpx.Validation
open NUnit.Framework
open Microsoft.FSharp.Core

let validator pred error value =
    if pred value
        then Choice1Of2 value
        else Choice2Of2 [error]

let (==) = LanguagePrimitives.PhysicalEquality
let inline (!=) a b = not (a == b)

let nonNull e = validator ((!=) null) e
let notEqual a = validator ((<>) a)

let validateAddressLines =
    validator 
        (fun (a: Address) -> a.Line1 != null || a.Line2 == null) 
        "Line1 is empty but Line2 is not"

//let inline konst a _ = a
//let inline konst2 a _ _ = a

let validateAddress (a: Address) = 
    puree a
    <* nonNull "Post code can't be null" a.Postcode
    <* validateAddressLines a

open FSharpx.Nullable

let greaterThan o = validator ((<?) o)

type ValidationBuilder() =
    member x.Bind(m,f) = m >>= f
    member x.Return a = Choice1Of2 a

let validation = ValidationBuilder()

let validateOrder (o: Order) =
    let nameNotNull = nonNull "Product name can't be null" o.ProductName
    let positiveCost n = greaterThan (0m).n (sprintf "Cost for product '%s' can't be negative" n) o.Cost
    nameNotNull >>= positiveCost |> Validation.map (fun _ -> o)

(*    validation {
        let! name = nonNull "Product name can't be null" o.ProductName
        let! _ = greaterThan (0m).n (sprintf "Cost for product '%s' must be positive" name) o.Cost
        return o
    } *)
    

let validateOrders c = seqValidator validateOrder c
    
[<Test>]
let Validation() = 
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
        puree customer
        <* nonNull "Surname can't be null" customer.Surname
        <* notEqual "foo" "Surname can't be foo" customer.Surname
        <* validateAddress customer.Address
        <* validateOrders customer.Orders
    match result with
    | Choice1Of2 c -> printfn "Valid customer: %A" c
    | Choice2Of2 errors -> printfn "Invalid customer. Errors:\n%A" errors