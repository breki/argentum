module Argentum.Tests.Builders

open Argentum.Model
open System

let newId() = Guid.NewGuid()

let withPrice currencyId time amount =
    { Id = newId(); Commodity = CurrencyRef currencyId
      Currency = CurrencyRef "EUR"
      Time = time
      Source = UserPrice
      PriceType = None; Value = amount2 (amount*10000. |> int) 10000  }

let withDate year month day = DateTime(year, month, day)

let addDays days (time: DateTime) = time.AddDays (days |> float)
