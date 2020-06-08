﻿module Argentum.Tests.XmlTests.``parsing transactions``

open System
open Argentum.Model
open Argentum.Parsing.XmlParsing
open Argentum.Parsing.Core
open Argentum.Parsing.Slots
open FsUnit
open Xunit
open Swensen.Unquote
open Argentum.Tests.XmlTests.ParsingHelpers

let parseTransaction context =
    let parseSplit context =
        context
        |> expectElementAndMove "split"
        >>= expectAndReadElementText "id" (fun id _ -> Guid.Parse id)
        >>= expectAndReadElementText "reconciled-state"
              (fun reconciledStr state ->
                  let reconciled  =
                      match reconciledStr with
                      | "n" -> NotReconciled
                      | "c" -> Reconciled
                      | _ ->
                          sprintf
                            "Unsupported reconciled state '%s'." reconciledStr
                          |> invalidOp
              
                  (reconciled, state)
              )
        >>= expectAndReadElementText "value"
                (fun text state ->
                    match parseAmount text with
                    | Ok amount -> (amount, state)
                    | Error error ->
                        sprintf "Invalid amount '%s': %s." text error
                        |> invalidOp
                )        
        >>= expectAndReadElementText "quantity"
                (fun text state ->
                    match parseAmount text with
                    | Ok amount -> (amount, state)
                    | Error error ->
                        sprintf "Invalid amount '%s': %s." text error
                        |> invalidOp
                )        
        >>= parseAccountRef "account"
        >>= parseConditional "slots"
              (fun context -> context |> parseSlots pushToState)
              (fun state -> ([||], state))        
        >>= mapValue
                (fun (slots, (account, (quantity, (value, (reconciled, id))))) ->
                    { Id = id; ReconciledState = reconciled
                      Value = value; Quantity = quantity
                      Account = account; Slots = slots } |> Some |> Ok
                 )
        >>= expectEndElementWithName "split" >>= moveNext
        
    let parseSplits
        (stateUpdate: Split list -> 'T -> Split list * 'U)
        (context: ParseContext<'T>)
        : ParseResult<Split list * 'U> =
        let (_, state) = context
            
        let splits =
            context
            |> expectElementAndMove "splits"
            >>= parseList "split" parseSplit
        match splits with
        | Ok (reader, splits) -> 
            let newState = stateUpdate splits state
            Ok (reader, newState)
        | Error error -> Error error
        
    context
    |> expectElementAndMove "transaction"
    >>= expectAndReadElementText "id" (fun id _ -> Guid.Parse id)
    >>= parseCommodityRef "currency" pushToState
    >>= parseTime "date-posted" pushToState
    >>= parseTime "date-entered" pushToState
    >>= expectAndReadElementText "description" pushToState
    >>= parseConditional "slots"
          (fun context -> context |> parseSlots pushToState)
          (fun state -> ([||], state))
    >>= parseSplits pushToState
    >>= mapValue
            (fun (splits, (slots, (description, (dateEntered,
                                         (datePosted, (currency, id)))))) ->
                {
                  Id = id; Currency = currency
                  DatePosted = datePosted; DateEntered = dateEntered
                  Description = description; Slots = slots
                  Splits = splits |> List.toArray }
                |> Some |> Ok
             )

[<Fact>]
let ``Can parse transaction``() =
    let xml = @"
      <gnc:transaction version='2.0.0'>
        <trn:id type='guid'>59ff0ea65fac4df1968ebd0d7fb1d5e3</trn:id>
        <trn:currency>
          <cmdty:space>CURRENCY</cmdty:space>
          <cmdty:id>EUR</cmdty:id>
        </trn:currency>
        <trn:date-posted>
          <ts:date>2018-11-25 15:02:13 +0000</ts:date>
        </trn:date-posted>
        <trn:date-entered>
          <ts:date>2018-11-25 15:02:31 +0000</ts:date>
        </trn:date-entered>
        <trn:description></trn:description>
        <trn:splits>
          <trn:split>
            <split:id type='guid'>cbf9f1e13f2a4ce3a7892d6855f9e167</split:id>
            <split:reconciled-state>n</split:reconciled-state>
            <split:value>402840/100</split:value>
            <split:quantity>402840/100</split:quantity>
            <split:account type='guid'>b1cb20c3604344db8e53d7142370e8bb</split:account>
          </trn:split>
          <trn:split>
            <split:id type='guid'>65c5a470dca64a7c961cddae7f6235dd</split:id>
            <split:reconciled-state>n</split:reconciled-state>
            <split:value>-402840/100</split:value>
            <split:quantity>-402840/100</split:quantity>
            <split:account type='guid'>5d7398633947af8ba50018145eb824e0</split:account>
          </trn:split>
        </trn:splits>
      </gnc:transaction>
"
    
    let doc = buildXml xml |> withReader
  
    let expectedTransaction =
          {
          Id = Guid.Parse("59ff0ea65fac4df1968ebd0d7fb1d5e3")
          Currency = CurrencyRef "EUR"
          DatePosted = DateTime(2018,11,25,16,02,13)
          DateEntered = DateTime(2018,11,25,16,02,31)
          Description = ""
          Splits = [|
            { Id = Guid.Parse "cbf9f1e13f2a4ce3a7892d6855f9e167"
              ReconciledState = NotReconciled
              Value = amount2 402840 100
              Quantity = amount2 402840 100
              Account = Guid.Parse "b1cb20c3604344db8e53d7142370e8bb"
              Slots = [||] };
            { Id = Guid.Parse "65c5a470dca64a7c961cddae7f6235dd"
              ReconciledState = NotReconciled
              Value = amount2 -402840 100
              Quantity = amount2 -402840 100
              Account = Guid.Parse "5d7398633947af8ba50018145eb824e0"
              Slots = [||] }
          |]
          Slots = [||]
          } |> Some |> Ok
  
    test <@ parseTransaction doc |> parsedValue = expectedTransaction @>

[<Fact>]
let ``Can parse transaction with slots``() =
    let xml = @"
      <gnc:transaction version='2.0.0'>
        <trn:id type='guid'>59ff0ea65fac4df1968ebd0d7fb1d5e3</trn:id>
        <trn:currency>
          <cmdty:space>CURRENCY</cmdty:space>
          <cmdty:id>EUR</cmdty:id>
        </trn:currency>
        <trn:date-posted>
          <ts:date>2018-11-25 15:02:13 +0000</ts:date>
        </trn:date-posted>
        <trn:date-entered>
          <ts:date>2018-11-25 15:02:31 +0000</ts:date>
        </trn:date-entered>
        <trn:description></trn:description>
        <trn:slots>
          <slot>
            <slot:key>date-posted</slot:key>
            <slot:value type='gdate'>
              <gdate>2019-03-29</gdate>
            </slot:value>
          </slot>
          <slot>
            <slot:key>account</slot:key>
            <slot:value type='guid'>d7c7bf631292de49fa063c118a9a0f5d</slot:value>
          </slot>
        </trn:slots>
        <trn:splits>
          <trn:split>
            <split:id type='guid'>cbf9f1e13f2a4ce3a7892d6855f9e167</split:id>
            <split:reconciled-state>n</split:reconciled-state>
            <split:value>402840/100</split:value>
            <split:quantity>402840/100</split:quantity>
            <split:account type='guid'>b1cb20c3604344db8e53d7142370e8bb</split:account>
          </trn:split>
          <trn:split>
            <split:id type='guid'>65c5a470dca64a7c961cddae7f6235dd</split:id>
            <split:reconciled-state>n</split:reconciled-state>
            <split:value>-402840/100</split:value>
            <split:quantity>-402840/100</split:quantity>
            <split:account type='guid'>5d7398633947af8ba50018145eb824e0</split:account>
          </trn:split>
        </trn:splits>
      </gnc:transaction>
"
    
    let doc = buildXml xml |> withReader
  
    let expectedTransaction =
          {
          Id = Guid.Parse("59ff0ea65fac4df1968ebd0d7fb1d5e3")
          Currency = CurrencyRef "EUR"
          DatePosted = DateTime(2018,11,25,16,02,13)
          DateEntered = DateTime(2018,11,25,16,02,31)
          Description = ""
          Splits = [|
            { Id = Guid.Parse "cbf9f1e13f2a4ce3a7892d6855f9e167"
              ReconciledState = NotReconciled
              Value = amount2 402840 100
              Quantity = amount2 402840 100
              Account = Guid.Parse "b1cb20c3604344db8e53d7142370e8bb"
              Slots = [||] };
            { Id = Guid.Parse "65c5a470dca64a7c961cddae7f6235dd"
              ReconciledState = NotReconciled
              Value = amount2 -402840 100
              Quantity = amount2 -402840 100
              Account = Guid.Parse "5d7398633947af8ba50018145eb824e0"
              Slots = [||] }
          |]
          Slots = [| { Key = "date-posted"
                       Value = SlotDate(DateTime(2019, 03, 29)) };
                        { Key = "account"
                          Value = SlotGuid(Guid.Parse
                                        ("d7c7bf631292de49fa063c118a9a0f5d")) }
                    |]
          } |> Some |> Ok
  
    test <@ parseTransaction doc |> parsedValue = expectedTransaction @>

[<Fact>]
let ``Can parse splits with slots``() =
    let xml = @"
      <gnc:transaction version='2.0.0'>
        <trn:id type='guid'>59ff0ea65fac4df1968ebd0d7fb1d5e3</trn:id>
        <trn:currency>
          <cmdty:space>CURRENCY</cmdty:space>
          <cmdty:id>EUR</cmdty:id>
        </trn:currency>
        <trn:date-posted>
          <ts:date>2018-11-25 15:02:13 +0000</ts:date>
        </trn:date-posted>
        <trn:date-entered>
          <ts:date>2018-11-25 15:02:31 +0000</ts:date>
        </trn:date-entered>
        <trn:description></trn:description>
        <trn:splits>
          <trn:split>
            <split:id type='guid'>cbf9f1e13f2a4ce3a7892d6855f9e167</split:id>
            <split:reconciled-state>n</split:reconciled-state>
            <split:value>402840/100</split:value>
            <split:quantity>402840/100</split:quantity>
            <split:account type='guid'>b1cb20c3604344db8e53d7142370e8bb</split:account>
            <split:slots>
              <slot>
                <slot:key>sched-xaction</slot:key>
                <slot:value type='frame'>
                  <slot>
                    <slot:key>credit-formula</slot:key>
                    <slot:value type='string'></slot:value>
                  </slot>
                </slot:value>
              </slot>
            </split:slots>
          </trn:split>
          <trn:split>
            <split:id type='guid'>65c5a470dca64a7c961cddae7f6235dd</split:id>
            <split:reconciled-state>n</split:reconciled-state>
            <split:value>-402840/100</split:value>
            <split:quantity>-402840/100</split:quantity>
            <split:account type='guid'>5d7398633947af8ba50018145eb824e0</split:account>
          </trn:split>
        </trn:splits>
      </gnc:transaction>
"
    
    let doc = buildXml xml |> withReader
  
    let expectedTransaction =
          {
          Id = Guid.Parse("59ff0ea65fac4df1968ebd0d7fb1d5e3")
          Currency = CurrencyRef "EUR"
          DatePosted = DateTime(2018,11,25,16,02,13)
          DateEntered = DateTime(2018,11,25,16,02,31)
          Description = ""
          Splits = [|
            { Id = Guid.Parse "cbf9f1e13f2a4ce3a7892d6855f9e167"
              ReconciledState = NotReconciled
              Value = amount2 402840 100
              Quantity = amount2 402840 100
              Account = Guid.Parse "b1cb20c3604344db8e53d7142370e8bb"
              Slots = [|
                {
                  Key = "sched-xaction"
                  Value = SlotOfSlots
                      ( [| { Key = "credit-formula"; Value = SlotString "" } |])
                }
              |] };
            { Id = Guid.Parse "65c5a470dca64a7c961cddae7f6235dd"
              ReconciledState = NotReconciled
              Value = amount2 -402840 100
              Quantity = amount2 -402840 100
              Account = Guid.Parse "5d7398633947af8ba50018145eb824e0"
              Slots = [||] }
          |]
          Slots = [||]
          } |> Some |> Ok
  
    test <@ parseTransaction doc |> parsedValue = expectedTransaction @>
    