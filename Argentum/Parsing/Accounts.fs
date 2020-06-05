module Argentum.Parsing.Accounts

open System
open System.Globalization
open Argentum.Model
open Argentum.Parsing.XmlParsing
open Argentum.Parsing.Core
open Argentum.Parsing.Slots

let parseAccount context =
    
    context
    |> expectElementAndMove "account"
    >>= expectElementAndMove "name"
    >>= readElementTextAndMove (fun name _ -> name)
    >>= expectEndElementAndMove
    >>= parseAccountRef "id"
    >>= expectElementAndMove "type"
    >>= readElementTextAndMove
          (fun typeStr state ->
              let accType =
                  match typeStr with
                  | "ASSET" -> AssetAccount
                  | "BANK" -> BankAccount
                  | "CASH" -> CashAccount
                  | "CREDIT" -> CreditAccount
                  | "EQUITY" -> EquityAccount
                  | "EXPENSE" -> ExpenseAccount
                  | "INCOME" -> IncomeAccount
                  | "LIABILITY" -> LiabilityAccount
                  | "RECEIVABLE" -> ReceivableAccount
                  | "ROOT" -> RootAccount
                  | _ ->
                    sprintf "Unsupported account type '%s'." typeStr
                    |> invalidOp
                  
              (accType, state))
    >>= expectEndElementAndMove
    >>= parseCommodityRef "commodity" pushToState
    >>= expectElementAndMove "commodity-scu"
    >>= readElementTextAndMove
          (fun scuStr state ->
            (Int32.Parse(scuStr, CultureInfo.InvariantCulture), state))
    >>= expectEndElementAndMove
    >>= parseConditional "description"
          (fun context ->
              context
              |> moveNext
              >>= readElementTextAndMove
                    (fun description state -> (Some description, state))
              >>= expectEndElementAndMove
          )
          (fun state -> (None, state))
    >>= parseSlots pushToState
    >>= parseAccountRefOptional "parent"
    >>= mapValue
        (fun (parent, (slots, (description, (scu, (commodity,
                                                   (accType, (id, name)))))))
          ->
            { Name = name; Type = accType; Id = id
              Commodity = commodity; CommodityScu = scu
              Description = description
              Slots = slots; ParentAccount = parent
             } |> Some |> Ok
          )

let parseAccountsList context =
    context
    |> parseList "account" parseAccount 
