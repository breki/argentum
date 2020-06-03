module Argentum.Tests.XmlTests.``parsing price DB``

open System
open Argentum.Model
open Argentum.Parsing.Prices  
open FsUnit
open Xunit
open Swensen.Unquote
open Argentum.Tests.XmlTests.ParsingHelpers


let priceDbXml = @"
<gnc:pricedb version='1'>
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
      <ts:date>2020-06-03 06:28:00 +0000</ts:date>
    </price:time>
    <price:source>user:price</price:source>
    <price:value>6815/10000</price:value>
  </price>
  <price>
    <price:id type='guid'>0721e697c7194f5582769d24164d3e70</price:id>
    <price:commodity>
      <cmdty:space>CURRENCY</cmdty:space>
      <cmdty:id>CAD</cmdty:id>
    </price:commodity>
    <price:currency>
      <cmdty:space>CURRENCY</cmdty:space>
      <cmdty:id>EUR</cmdty:id>
    </price:currency>
    <price:time>
      <ts:date>2019-12-08 10:59:00 +0000</ts:date>
    </price:time>
    <price:source>user:price</price:source>
    <price:value>6558/10000</price:value>
  </price>
</gnc:pricedb>"

[<Fact>]
let ``Can parse price without type``() =
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
          <ts:date>2020-06-03 06:28:00 +0000</ts:date>
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
        Time = DateTime(2020, 06, 03, 08, 28, 00)
        Source = UserPrice
        PriceType = None
        Value = amount2 6815 10000
      } |> Some |> Ok
    
    test <@ parsePrice doc |> parsedValue = expectedPrice @>
    
[<Fact>]
let ``Can parse price with type``() =
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
          <ts:date>2020-06-03 06:28:00 +0000</ts:date>
        </price:time>
        <price:source>user:xfer-dialog</price:source>
        <price:type>transaction</price:type>
        <price:value>6815/10000</price:value>
      </price>
        "
    let doc = buildXml xml |> withReader

    let expectedPrice =
      {
        Id = Guid.Parse("30a1f0bde0854b848231a86ec1e1a4a3")
        Commodity = CurrencyRef "CAD"
        Currency = CurrencyRef "EUR"
        Time = DateTime(2020, 06, 03, 08, 28, 00)
        Source = UserTransferDialog
        PriceType = Some Transaction
        Value = amount2 6815 10000
      } |> Some |> Ok
    
    test <@ parsePrice doc |> parsedValue = expectedPrice @>
    
[<Fact>]
let ``Can parse whole price db``() =
    let doc = buildXml priceDbXml |> withReader

    let expectedPrices =
        [
          {
            Id = Guid.Parse("30a1f0bde0854b848231a86ec1e1a4a3")
            Commodity = CurrencyRef "CAD"
            Currency = CurrencyRef "EUR"
            Time = DateTime(2020, 06, 03, 08, 28, 00)
            Source = UserPrice
            PriceType = None
            Value = amount2 6815 10000
          };
          {
            Id = Guid.Parse("0721e697c7194f5582769d24164d3e70")
            Commodity = CurrencyRef "CAD"
            Currency = CurrencyRef "EUR"
            Time = DateTime(2019, 12, 08, 11, 59, 00)
            Source = UserPrice
            PriceType = None
            Value = amount2 6558 10000
          }      
        ] |> Ok
    
    test <@ parsePriceDb doc |> parsedValue = expectedPrices @>
