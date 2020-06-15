module Argentum.Tests.``transaction math``

open Argentum.TransactionMath
open Argentum.Model
open Argentum.Tests.Builders
open FsUnit
open Xunit
open Swensen.Unquote
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
