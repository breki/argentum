module Argentum.Tests.``calculating total balances``

open Argentum.TransactionMath
open Argentum.Model
open Argentum.Tests.Builders
open FsUnit
open Xunit
open Swensen.Unquote

let baseCurrency = CurrencyRef "EUR"

[<Fact>]
let ``can calculate balance of transactions on time``() =
    let assets = withAccount AssetAccount
    let opening = withAccount EquityAccount
    let accounts = [ assets; opening ] |> toAccountsMap
    
    let baseTime = withDate 2020 06 14
    let assetsAmount = amount1 100
    
    let prices = []
    
    let transactions =
        [ withTransaction assetsAmount (baseTime |> addDays 1) opening assets ]
    
    let balance =
        calculateBalanceOnTime
            (baseTime |> addDays 2) transactions accounts prices baseCurrency
    
    test <@ balance = assetsAmount @>
