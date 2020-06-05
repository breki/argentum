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
    Slots: Slot[]
    ParentAccount: AccountId option
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

type PriceDb = Price seq
