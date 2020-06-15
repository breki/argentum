module Argentum.Tests.``calculating balance``

open System
open Argentum.ExchangeRates
open Argentum.Model
open Argentum.Tests.Builders
open FsUnit
open Xunit
open Swensen.Unquote

let calculateBalance transactions accounts prices =
    invalidOp "todo"

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

[<Fact>]
let ``Assets increase balance``() =
    let assets = withAccount AssetAccount
    let opening = withAccount EquityAccount
    let accounts = [ assets; opening ]
    
    let baseTime = withDate 2020 06 14
    let assetsAmount = amount1 100
    
    let prices = []
    
    let transactions = [ withTransaction assetsAmount baseTime opening assets ]
    
    let balance = calculateBalance transactions accounts prices
    
    test <@ balance = assetsAmount @>