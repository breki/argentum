module Argentum.Tests.``calculating balance``

open System
open Argentum.ExchangeRates
open Argentum.Model
open Argentum.Tests.Builders
open FsUnit
open Xunit
open Swensen.Unquote

let getAccount (accountId: AccountId) (accounts: Accounts) =
    accounts.[accountId]

let transactionNetBalance
    (transaction: Transaction) (accounts: Accounts) prices =
    // for each split
    transaction.Splits
    |> Array.map (fun split ->
        // get its account
        let account = accounts |> getAccount split.Account
        // based on the account type,
        // decide how its value contributes to the balance
        match account.Type with
        | AssetAccount -> split.Value
        | EquityAccount -> amount0
        | _ -> invalidOp "todo"
        )
    |> sumAmounts

let calculateBalanceOfTransactions transactions accounts prices: Amount =
    transactions
    |> List.fold
           (fun sum transaction ->
                let netBalance =
                    transactionNetBalance transaction accounts prices
                sum |> addAmount netBalance
            )
           amount0

let calculateBalanceOnTime
    time (transactions: Transaction list) accounts prices =
        
    transactions
    // filter transactions on or before the time
    |> List.filter (fun t -> t.DatePosted <= time)
    // group transactions by days
    |> List.groupBy (fun t -> t.DatePosted.Date)
    // calculate balance for each days
    |> List.map
           (fun (day, dayTransactions) ->
                    (day, calculateBalanceOfTransactions
                              dayTransactions accounts prices))
    // calculate sum of all balances
    |> List.fold
           (fun sum (_, dayBalance) -> sum |> addAmount dayBalance)
           amount0

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
    let accounts = [ assets; opening ] |> toAccountsMap
    
    let baseTime = withDate 2020 06 14
    let assetsAmount = amount1 100
    
    let prices = []
    
    let transactions =
        [ withTransaction assetsAmount (baseTime |> addDays 1) opening assets ]
    
    let balance =
        calculateBalanceOnTime (baseTime |> addDays 2) transactions accounts prices
    
    test <@ balance = assetsAmount @>