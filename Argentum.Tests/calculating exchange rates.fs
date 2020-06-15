module Argentum.Tests.``calculating exchange rates``

open Argentum.ExchangeRates
open Argentum.Model
open Argentum.Tests.Builders
open FsUnit
open Xunit
open Swensen.Unquote

[<Fact>]
let ``Exchange rate between the same account is 1``() =
    let time = withDate 2020 06 13
    let currency = CurrencyRef "EUR"

    test <@ exchangeRate time currency currency [] = (amount1 1) @>

[<Fact>]
let ``Can get exchange rate for exact date``() =
    let expectedPrice = amount2 5000 10000
    let time = withDate 2020 06 13
    
    let prices = [
        (withPrice "USD" time (expectedPrice.Value))
    ]

    test <@ exchangeRate time (CurrencyRef "USD") (CurrencyRef "EUR") prices
                = expectedPrice  @>
    
[<Fact>]
let ``Can get exchange rate if time is before any prices``() =
    let expectedPrice = amount2 5000 10000
    let time = withDate 2020 06 13
    
    let prices = [
        (withPrice "USD" (time |> addDays 3) (expectedPrice.Value))
    ]

    test <@ exchangeRate time (CurrencyRef "USD") (CurrencyRef "EUR") prices
                = expectedPrice  @>
    
[<Fact>]
let ``Can get exchange rate if time is between prices``() =
    let somePrice = amount2 6000 10000
    let expectedPrice = amount2 5000 10000
    let time = withDate 2020 06 13
    
    let prices = [
        (withPrice "USD" (time |> addDays -3) (expectedPrice.Value))
        (withPrice "USD" (time |> addDays 3) (somePrice.Value))
    ]

    test <@ exchangeRate time (CurrencyRef "USD") (CurrencyRef "EUR") prices
                = expectedPrice  @>
    
[<Fact>]
let ``Can get exchange rate if time is after all prices``() =
    let expectedPrice = amount2 5000 10000
    let time = withDate 2020 06 13
    
    let prices = [
        (withPrice "USD" (time |> addDays -3) (expectedPrice.Value))
    ]

    test <@ exchangeRate time (CurrencyRef "USD") (CurrencyRef "EUR") prices
                = expectedPrice  @>
