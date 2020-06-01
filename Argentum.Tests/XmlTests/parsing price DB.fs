module Argentum.Tests.XmlTests.``parsing price DB``

open System
open Argentum.Model
open Argentum.Parsing.XmlParsing
open FsUnit
open Xunit
open Swensen.Unquote
open Argentum.Tests.XmlTests.ParsingHelpers

let parsePrice context: ParseResult<Price option> =
  let (reader, _) = context
  Ok (reader, None)

[<Fact>]
let ``Can parse price``() =
    let xml = @"
      <price>
        <price:id type='guid'>30a1f0bde0854b848231a86ec1e1a4a3</price:id>
        <price:commodity>
          <cmdty:space>CURRENCY</cmdty:space>
          <cmdty:id>CAD</cmdty:id>
        </price:commodity>
        <price:currency>
          <cmdty:space>CURRENCY</cmdty:space>
          <cmdty:id>EUR</cmdty:id>
        </price:currency>
        <price:time>
          <ts:date>2020-02-04 10:59:00 +0000</ts:date>
        </price:time>
        <price:source>user:price</price:source>
        <price:value>6815/10000</price:value>
      </price>
        "
    let doc = buildXml xml |> withReader

    let expectedPrice =
      {
        Id = Guid.Parse("30a1f0bde0854b848231a86ec1e1a4a3")
        Commodity = CurrencyRef "CAD"
        Currency = CurrencyRef "EUR"
        Time = DateTime(2020, 02, 04, 10, 59, 00)
        Source = UserPrice
        PriceType = None
        Value = amount2 6815 10000
      } |> Some |> Ok
    
    test <@ parsePrice doc |> parsedValue = expectedPrice @>