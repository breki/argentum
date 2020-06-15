module Argentum.Model

open System

type Amount (dividend: int, divisor: int) =
    member this.Dividend = dividend
    member this.Divisor = divisor

    member this.Value = (float this.Dividend) / (float this.Divisor)
    
    override this.Equals (o: obj) =
        let otherAmount = (o :?> Amount)
        this.Value = otherAmount.Value 

    override this.GetHashCode() =
        let hash1 = (13 * 7) + this.Dividend.GetHashCode()
        (hash1 * 7) + this.Divisor.GetHashCode()        
    
    interface IComparable with
        member this.CompareTo (o: obj) =
            this.Value.CompareTo((o :?> Amount).Value)
        

let amount2 dividend divisor = Amount(dividend, divisor)
let amount1 dividend = 1 |> amount2 dividend  
let amount0 = amount1 0
let amountNegative (amount: Amount) = amount2 -amount.Dividend amount.Divisor

let addAmount (amountA: Amount) (amountB: Amount) =
    match amountA.Divisor, amountB.Divisor with
    | (divisorA, divisorB) when divisorA = divisorB ->
        amount2 (amountA.Dividend + amountB.Dividend) divisorA
    | (divisorA, divisorB) when divisorA % divisorB = 0 ->
        let multiplier = divisorA / divisorB
        amount2 (amountA.Dividend + amountB.Dividend * multiplier) divisorA
    | (divisorA, divisorB) when divisorB % divisorA = 0 ->
        let multiplier = divisorB / divisorA
        amount2 (amountA.Dividend * multiplier + amountB.Dividend) divisorB
    | (divisorA, divisorB) ->
        let sumDivisor = divisorA * divisorB
        amount2
            (amountA.Dividend * divisorB + amountB.Dividend * divisorA)
            sumDivisor

let sumAmounts (amounts: Amount seq): Amount =
    amounts |> Seq.fold (fun sum amount -> sum |> addAmount amount) amount0

type SlotValue =
    | SlotString of string
    | SlotNumeric of Amount
    | SlotDate of DateTime
    | SlotGuid of Guid
    | SlotOfSlots of Slot[]
and Slot = {
    Key: string
    Value: SlotValue
}

type CurrencyId = string

type CurrencyDef = {
    Version: Version
    Id: CurrencyId
}

type CommodityRef =  
    | CurrencyRef of CurrencyId

type TemplateDef = {
    Version: Version
    Name: string
    XCode: string
    Fraction: int
}

type Commodity =
    | Currency of CurrencyDef
    | CommodityTemplate of TemplateDef

type AccountId = Guid

type AccountType =
    | AssetAccount
    | BankAccount
    | CashAccount
    | CreditAccount
    | EquityAccount
    | ExpenseAccount
    | IncomeAccount
    | LiabilityAccount
    | ReceivableAccount
    | RootAccount

type Account = {
    Name: string
    Id: AccountId
    Type: AccountType
    Commodity: CommodityRef
    CommodityScu: int
    Description: string option
    Code: string option
    Slots: Slot[]
    ParentAccount: AccountId option
}

type Accounts = Map<AccountId, Account>

let toAccountsMap (accounts: Account seq): Accounts =
    accounts
    |> Seq.map (fun account -> (account.Id, account))
    |> Map.ofSeq

let getAccount (accountId: AccountId) (accounts: Accounts) =
    accounts.[accountId]

type ReconciledState =
    | NotReconciled
    | Reconciled

type Split = {
    Id: Guid
    Memo: string option
    Action: string option
    ReconciledState: ReconciledState
    Value: Amount
    Quantity: Amount
    Account: AccountId
    Slots: Slot[]
}

type Transaction = {
    Id: Guid
    Currency: CommodityRef
    DatePosted: DateTime 
    DateEntered: DateTime 
    Description: string
    Slots: Slot[]
    Splits: Split[]
}

type PriceSource =
    | UserPrice
    | UserPriceEditor
    | UserTransferDialog

type PriceType =
    | Unknown
    | Transaction

type Price = {
    Id: Guid
    Commodity: CommodityRef
    Currency: CommodityRef
    Time: DateTime
    Source: PriceSource
    PriceType: PriceType option
    Value: Amount
}

type PriceDb = Price list

type Book = {
    Commodities: Commodity list
    PriceDb: PriceDb option
    Accounts: Account list
    Transactions: Transaction list
}
