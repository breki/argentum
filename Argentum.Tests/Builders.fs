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


let withAccount accountType =
    { Name = "some account"; Id = newId(); Type = accountType
      Commodity = CurrencyRef "EUR"; CommodityScu = 100
      Description = None; Code = None; Slots = [||]
      ParentAccount = None }

let withTransaction amount time (fromAccount: Account) (toAccount: Account) =
    let split1 = { Id = newId(); Memo = None; Action = None
                   ReconciledState = Reconciled; Value = amount
                   Quantity = amountNegative amount
                   Account = fromAccount.Id
                   Slots = [||] }
    let split2 = { Id = newId(); Memo = None; Action = None
                   ReconciledState = Reconciled; Value = amount
                   Quantity = amount
                   Account = toAccount.Id
                   Slots = [||] }

    { Id = newId(); Currency = CurrencyRef "EUR"; DateEntered = time
      DatePosted = time; Description = ""; Slots = [||]
      Splits = [| split1; split2 |] }
