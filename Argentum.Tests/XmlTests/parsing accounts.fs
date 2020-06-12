module Argentum.Tests.XmlTests.``parsing accounts``

open System
open Argentum.Model
open Argentum.Parsing.Accounts
open FsUnit
open Xunit
open Swensen.Unquote
open Argentum.Tests.XmlTests.ParsingHelpers

[<Fact>]
let ``Can parse root account``() =
    let xml = @"
    <gnc:account version='2.0.0'>
      <act:name>Root Account</act:name>
      <act:id type='guid'>a6cb26f377e4ecbd6d5122484a4e283b</act:id>
      <act:type>ROOT</act:type>
      <act:commodity>
        <cmdty:space>CURRENCY</cmdty:space>
        <cmdty:id>EUR</cmdty:id>
      </act:commodity>
      <act:commodity-scu>100</act:commodity-scu>
      <act:slots>
        <slot>
          <slot:key>favorite</slot:key>
          <slot:value type='string'>false</slot:value>
        </slot>
      </act:slots>
    </gnc:account>
    "

    let doc = buildXml xml |> withReader
    
    let expectedAccount =
      {
        Name = "Root Account"; Type = RootAccount
        Id = Guid.Parse("a6cb26f377e4ecbd6d5122484a4e283b")
        Commodity = CurrencyRef "EUR"; CommodityScu = 100
        Description = None; Code = None; ParentAccount = None
        Slots = [|
          { Key = "favorite"; Value = SlotString "false" }
        |]
      } |> Some |> Ok

    test <@ parseAccount doc |> parsedValue = expectedAccount @>
  
[<Fact>]
let ``Can parse non-root account``() =
    let xml = @"
    <gnc:account version='2.0.0'>
      <act:name>Assets</act:name>
      <act:id type='guid'>11fa67ab80d2e7862fecf31fc390c5c2</act:id>
      <act:type>ASSET</act:type>
      <act:commodity>
        <cmdty:space>CURRENCY</cmdty:space>
        <cmdty:id>EUR</cmdty:id>
      </act:commodity>
      <act:commodity-scu>100</act:commodity-scu>
      <act:description>Assets</act:description>
      <act:slots>
        <slot>
          <slot:key>color</slot:key>
          <slot:value type='string'>#1469EB</slot:value>
        </slot>
        <slot>
          <slot:key>favorite</slot:key>
          <slot:value type='string'>false</slot:value>
        </slot>
        <slot>
          <slot:key>placeholder</slot:key>
          <slot:value type='string'>true</slot:value>
        </slot>
      </act:slots>
      <act:parent type='guid'>a6cb26f377e4ecbd6d5122484a4e283b</act:parent>
    </gnc:account>
    "

    let doc = buildXml xml |> withReader
    
    let expectedAccount =
      {
        Name = "Assets"; Type = AssetAccount
        Id = Guid.Parse("11fa67ab80d2e7862fecf31fc390c5c2")
        Commodity = CurrencyRef "EUR"; CommodityScu = 100
        Description = Some "Assets"; Code = None
        ParentAccount = Guid.Parse("a6cb26f377e4ecbd6d5122484a4e283b") |> Some
        Slots = [|
          { Key = "color"; Value = SlotString "#1469EB" }
          { Key = "favorite"; Value = SlotString "false" }
          { Key = "placeholder"; Value = SlotString "true" }
        |]
      } |> Some |> Ok

    test <@ parseAccount doc |> parsedValue = expectedAccount @>
