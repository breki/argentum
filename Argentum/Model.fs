module Argentum.Model

open System

type Amount = {
    Dividend: int
    Divisor: int
}

type SlotValue =
    | SlotString of string
    | SlotDate of DateTime
    | SlotGuid of Guid

type Slot = {
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
