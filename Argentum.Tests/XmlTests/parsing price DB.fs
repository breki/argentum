module Argentum.Tests.XmlTests.``parsing price DB``

open System
open Argentum.Model
open Argentum.Parsing.XmlParsing
open Argentum.Parsing.Core
open FsUnit
open Xunit
open Swensen.Unquote
open Argentum.Tests.XmlTests.ParsingHelpers

let parseCommodityRef
    expectedElementName
    (stateUpdate: CommodityRef -> 'T -> 'U)
    (context: ParseContext<'T>)
    : ParseResult<'U> =
    
    let parseCommodityRefBasedOnSpace (context: ParseContext<string>) =
        let (_, space) = context
        match space with
        | "CURRENCY" ->
            context
            |> expectElement "id" >>= moveNext
            >>= readElementText (fun id _ -> id) >>= moveNext
            >>= expectEndElement >>= moveNext
            >>= skipToElementEnd
            >>= (fun (reader, id) ->
                    let commodityRef = CurrencyRef id 
                    Ok (reader, commodityRef))
        | _ ->
            sprintf "Commodity space '%s' is not supported." space
            |> invalidOp
            
    let (reader, state) = context
  
    let commodityRef =
        context
        |> expectElement expectedElementName >>= moveNext
        >>= expectElement "space" >>= moveNext
        >>= readElementText (fun space _ -> space ) >>= moveNext
        >>= expectEndElement >>= moveNext
        >>= parseCommodityRefBasedOnSpace
      
    match commodityRef with
    | Ok (_, commodityRef) ->
        let newState = state |> stateUpdate commodityRef
        Ok (reader, newState)
    | Error error -> Error error   

let parsePrice context: ParseResult<Price option> =
    context
    |> expectElement "price" >>= moveNext
    >>= expectElement "id"
    >>= readAttributeResult "type"
        (fun idType _ ->
          match idType with
          | "guid" -> Ok None
          | _ -> Error "Unsupported price ID type.") >>= moveNext
    >>= readElementText (fun id _ -> Guid.Parse id) >>= moveNext
    >>= expectEndElement >>= moveNext
    >>= parseCommodityRef "commodity"
        (fun commodityRef state -> (commodityRef, state))
    >>= parseCommodityRef "currency"
        (fun commodityRef state -> (commodityRef, state))
    >>= parseTime "time"
        (fun dateTime state -> (dateTime, state))
    >>= expectElement "source" >>= moveNext
    >>= readElementText
            (fun sourceText state ->
                let source =
                    match sourceText with
                    | "user:price" -> UserPrice
                    | "user:xfer-dialog" -> UserTransferDialog
                    | "user:price-editor" -> UserPriceEditor
                    | _ ->
                        sprintf "Price source '%s' is not supported." sourceText
                        |> invalidOp
                        
                (source, state)
            ) >>= moveNext
    >>= expectEndElement >>= moveNext
    >>= parseConditional "type"
        (fun context ->
            context
            |> moveNext
            >>= readElementText
                    (fun typeText state ->
                        let priceType =
                            match typeText with
                            | "transaction" -> Some Transaction
                            | "unknown" -> Some Unknown
                            | _ ->
                                sprintf
                                    "Price type '%s' is not supported." typeText
                                |> invalidOp
                        (priceType, state)
                    ) >>= moveNext
            >>= expectEndElement >>= moveNext)
        (fun state -> (None, state))
    >>= expectElement "value" >>= moveNext
    >>= readElementTextResult
            (fun text state ->
                match parseAmount text with
                | Ok amount -> Ok (amount, state)
                | Error error -> Error error) >>= moveNext
    >>= expectEndElement >>= moveNext
    |> mapValue
           (fun (amount, (priceType, (source, (dateTime,
                                               (currency, (commodity, id))))))
                ->
                    { Id = id; Commodity = commodity; Currency = currency
                      Time = dateTime; Source = source; PriceType = priceType
                      Value = amount }
                     |> Some |> Ok
                    )

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