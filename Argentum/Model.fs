module Argentum.Model

open System

type Amount = {
    Dividend: int
    Divisor: int
}

let amount2 dividend divisor = { Dividend = dividend; Divisor = divisor }
let amount1 dividend = 1 |> amount2 dividend  

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
    Id: CurrencyId
    QuoteSource: string
}

type TemplateDef = {
    Id: string
    Name: string
    XCode: string
    Fraction: int
}

type Commodity =
    | Currency of CurrencyDef
    | Template

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
    Commodity: Commodity
    CommodityScu: int
    Description: string
    Slots: Slot[]
    ParentAccount: Account option
}

type ReconciledState =
    | NotReconciled
    | Reconciled

type Split = {
    Id: Guid
    ReconciledState: ReconciledState
    Value: Amount
    Quantity: Amount
    Account: AccountId
}

type Transaction = {
    Id: Guid
    Currency: CurrencyId
    DatePosted: DateTime 
    DateEntered: DateTime 
    Description: string
    Slots: Slot[]
    Splits: Split[]
}
