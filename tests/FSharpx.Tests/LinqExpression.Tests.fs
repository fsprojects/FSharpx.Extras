module FSharpx.Tests.LinqExpressionTests

open System
open FSharpx
open NUnit.Framework
open FsUnit
open FSharpx.Linq
open System.Linq.Expressions

type WeatherInfoRequest = {
    City : string }

type WeatherInfoResponse = {
    City : string 
    Temperature :string }

type IWeatherInfoService =
    abstract HandleRequest: WeatherInfoRequest -> WeatherInfoResponse

[<Test>]
let ``It should convert a quoted lambda with 1 parameter to a Linq lambda``() = 
    let weatherInfoRequest = { City = "Hamburg" } : WeatherInfoRequest
    let quote = <@ fun (service:IWeatherInfoService) -> service.HandleRequest(weatherInfoRequest) @>
    let lambda = toLinqExpression quote

    lambda.GetType() |> should equal typeof<Expression<Func<IWeatherInfoService,WeatherInfoResponse>>>    