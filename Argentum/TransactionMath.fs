module Argentum.TransactionMath

open Argentum.Model

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
