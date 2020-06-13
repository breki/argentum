module Argentum.Tests.``calculating exchange rates``

open System
open Argentum.ExchangeRates
open Argentum.Model
open FsUnit
open Xunit
open Swensen.Unquote

let withPrice currencyId time amount =
    { Id = Guid.NewGuid(); Commodity = CurrencyRef currencyId
      Currency = CurrencyRef "EUR"
      Time = time
      Source = UserPrice
      PriceType = None; Value = amount2 (amount*10000. |> int) 10000  }

let withDate year month day = DateTime(year, month, day)

let addDays days (time: DateTime) = time.AddDays (days |> float)

[<Fact>]
let ``Can get exchange rate for exact date``() =
    let expectedPrice = amount2 5000 10000
    let time = withDate 2020 06 13
    
    let prices = [
        (withPrice "USD" time (expectedPrice |> amountFloat))
    ]

    test <@ exchangeRate time (CurrencyRef "USD") (CurrencyRef "EUR") prices
                = expectedPrice  @>
    
[<Fact>]
let ``Can get exchange rate if time is before any prices``() =
    let expectedPrice = amount2 5000 10000
    let time = withDate 2020 06 13
    
    let prices = [
        (withPrice "USD" (time |> addDays 3) (expectedPrice |> amountFloat))
    ]

    test <@ exchangeRate time (CurrencyRef "USD") (CurrencyRef "EUR") prices
                = expectedPrice  @>
    
[<Fact>]
let ``Can get exchange rate if time is between prices``() =
    let somePrice = amount2 6000 10000
    let expectedPrice = amount2 5000 10000
    let time = withDate 2020 06 13
    
    let prices = [
        (withPrice "USD" (time |> addDays -3) (expectedPrice |> amountFloat))
        (withPrice "USD" (time |> addDays 3) (somePrice |> amountFloat))
    ]

    test <@ exchangeRate time (CurrencyRef "USD") (CurrencyRef "EUR") prices
                = expectedPrice  @>
    
[<Fact>]
let ``Can get exchange rate if time is after all prices``() =
    let expectedPrice = amount2 5000 10000
    let time = withDate 2020 06 13
    
    let prices = [
        (withPrice "USD" (time |> addDays -3) (expectedPrice |> amountFloat))
    ]

    test <@ exchangeRate time (CurrencyRef "USD") (CurrencyRef "EUR") prices
                = expectedPrice  @>
