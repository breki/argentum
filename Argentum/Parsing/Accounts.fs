﻿module Argentum.Parsing.Accounts

open System
open System.Globalization
open Argentum.Model
open Argentum.Parsing.XmlParsing
open Argentum.Parsing.Core
open Argentum.Parsing.Slots

let parseAccount context =
    
    context
    |> expectElementAndMove "account"
    >>= expectAndReadElementText "name" (fun name _ -> name)
    >>= parseAccountRef "id"
    >>= expectAndReadElementText "type"
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
    >>= parseCommodityRef "commodity" pushToState
    >>= expectAndReadElementText "commodity-scu"
          (fun scuStr state ->
            (Int32.Parse(scuStr, CultureInfo.InvariantCulture), state))
    >>= parseConditional "description"
          (fun context ->
              context
              |> expectAndReadElementText "description"
                    (fun description state -> (Some description, state))
          )
          (fun state -> (None, state))
    >>= parseConditional "code"
          (fun context ->
              context
              |> expectAndReadElementText "code"
                    (fun code state -> (Some code, state))
          )
          (fun state -> (None, state))
    >>= parseConditional "slots"
          (fun context -> context |> parseSlots pushToState)
          (fun state -> ([||], state))
    >>= parseAccountRefOptional "parent"
    >>= expectEndElementWithName "account" >>= moveNext
    >>= mapValue
        (fun (parent, (slots, (code, (description, (scu, (commodity,
                                                        (accType,
                                                         (id, name))))))))
          ->
            { Name = name; Type = accType; Id = id
              Commodity = commodity; CommodityScu = scu
              Description = description; Code = code
              Slots = slots; ParentAccount = parent
             } |> Some |> Ok
          )

let parseAccountsList context =
    context
    |> parseList "account" parseAccount 
