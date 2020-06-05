﻿module Argentum.Tests.XmlTests.``parsing transactions``

open System
open Argentum.Model
open Argentum.Parsing.XmlParsing
open Argentum.Parsing.Core
open FsUnit
open Xunit
open Swensen.Unquote
open Argentum.Tests.XmlTests.ParsingHelpers

let parseTransaction context =
    let (reader, _) = context
    
    let x = 
      context
      |> expectElementAndMove "transaction"
      >>= expectAndReadElementText "id" (fun id _ -> id)
      >>= parseCommodityRef "currency" pushToState
      >>= parseTime "date-posted" pushToState
      >>= parseTime "date-entered" pushToState
      >>= expectAndReadElementText "description" pushToState
    
    let transaction = {
          Id = Guid.NewGuid(); Currency = CurrencyRef "EUR"
          DatePosted = DateTime(); DateEntered = DateTime()
          Description = ""; Slots = [||]; Splits = [||] }
    
    Ok (reader, Some transaction)


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
              Account = Guid.Parse "b1cb20c3604344db8e53d7142370e8bb" };
            { Id = Guid.Parse "65c5a470dca64a7c961cddae7f6235dd"
              ReconciledState = NotReconciled
              Value = amount2 -402840 100
              Quantity = amount2 -402840 100
              Account = Guid.Parse "5d7398633947af8ba50018145eb824e0" }
          |]
          Slots = [||]
          } |> Some |> Ok
  
    test <@ parseTransaction doc |> parsedValue = expectedTransaction @>