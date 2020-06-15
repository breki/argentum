module Argentum.Tests.``calculating balance``

open System
open Argentum.ExchangeRates
open Argentum.Model
open Argentum.Tests.Builders
open FsUnit
open Xunit
open Swensen.Unquote

let transactionNetBalance
    (transaction: Transaction) (accounts: Accounts) prices baseCurrency =
    // for each split
    transaction.Splits
    |> Array.map (fun split ->
        // get its account
        let account = accounts |> getAccount split.Account
        // based on the account type,
        // decide how its value contributes to the balance
        match account.Type with
        | AssetAccount -> split.Value
        | BankAccount -> split.Value
        | CashAccount -> split.Value
        | CreditAccount -> split.Value
        | EquityAccount -> amount0
        | ExpenseAccount -> amount0
        | IncomeAccount -> amount0
        | LiabilityAccount -> split.Value
        | ReceivableAccount -> split.Value
        | RootAccount -> amount0
        )
    |> sumAmounts

let calculateBalanceOfTransactions
    transactions accounts prices baseCurrency: Amount =
    transactions
    |> List.fold
           (fun sum transaction ->
                let netBalance =
                    transactionNetBalance
                        transaction accounts prices baseCurrency
                sum |> addAmount netBalance
            )
           amount0

let calculateBalanceOnTime
    time (transactions: Transaction list) accounts prices baseCurrency =
        
    transactions
    // filter transactions on or before the time
    |> List.filter (fun t -> t.DatePosted <= time)
    // group transactions by days
    |> List.groupBy (fun t -> t.DatePosted.Date)
    // calculate balance for each days
    |> List.map
           (fun (day, dayTransactions) ->
                    (day, calculateBalanceOfTransactions
                              dayTransactions accounts prices baseCurrency))
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

let baseCurrency = CurrencyRef "EUR"

type NetBalanceEffect = Positive | Neutral | Negative

let testNetBalanceOfAccountType accountType expectedEffect =
    let split = { withAccount accountType with Type = accountType }
    let opposite = withAccount EquityAccount
    let accounts = [ split; opposite ] |> toAccountsMap
    
    let baseTime = withDate 2020 06 14
    let splitAmount = amount1 100
    
    let prices = []
    
    let transaction =
        withTransaction splitAmount (baseTime |> addDays 1) opposite split
    
    let balance =
        transactionNetBalance transaction accounts prices baseCurrency
    
    match expectedEffect with
    | Positive -> test <@ balance = splitAmount @>
    | Neutral -> test <@ balance = amount0 @>
    | Negative -> test <@ balance = (splitAmount |> amountNegative) @>

[<Fact>]
let ``assets increase balance``() =
    testNetBalanceOfAccountType AssetAccount Positive

[<Fact>]
let ``bank assets increase balance``() =
    testNetBalanceOfAccountType BankAccount Positive

[<Fact>]
let ``cash assets increase balance``() =
    testNetBalanceOfAccountType CashAccount Positive

[<Fact>]
let ``credit assets increase balance``() =
    testNetBalanceOfAccountType CreditAccount Positive

[<Fact>]
let ``equity assets are neutral``() =
    testNetBalanceOfAccountType EquityAccount Neutral

[<Fact>]
let ``expenses are neutral``() =
    testNetBalanceOfAccountType ExpenseAccount Neutral

[<Fact>]
let ``income is neutral``() =
    testNetBalanceOfAccountType IncomeAccount Neutral

[<Fact>]
let ``liability is positive``() =
    testNetBalanceOfAccountType LiabilityAccount Positive

[<Fact>]
let ``receivable is positive``() =
    testNetBalanceOfAccountType ReceivableAccount Positive

[<Fact>]
let ``root account is neutral``() =
    testNetBalanceOfAccountType RootAccount Neutral

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
    
