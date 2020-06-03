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
            |> expectElement "id"
            >>= readElementText (fun id _ -> id)
            >>= expectEndElement
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
        |> expectElement expectedElementName
        >>= expectElement "space"
        >>= readElementText (fun space _ -> space )
        >>= expectEndElement
        >>= parseCommodityRefBasedOnSpace
      
    match commodityRef with
    | Ok (_, commodityRef) ->
        let newState = state |> stateUpdate commodityRef
        Ok (reader, newState)
    | Error error -> Error error   

let parseDate
    expectedElementName
    (stateUpdate: DateTime -> 'T -> 'U)
    (context: ParseContext<'T>)
    : ParseResult<'U> =
                
    let (reader, state) = context
    
    let dateTime =
        context
        |> expectElement expectedElementName
        >>= expectElement "date"
        >>= readElementText (fun dateTimeStr _ -> DateTime.Parse(dateTimeStr))
        >>= expectEndElement
        >>= expectEndElement
        
    match dateTime with
    | Ok (_, commodityRef) ->
        let newState = state |> stateUpdate commodityRef
        Ok (reader, newState)
    | Error error -> Error error
    

let parsePrice context: ParseResult<Price option> =
    context
    |> expectElement "price"
    >>= expectElement "id"
    >>= readAttributeResult "type"
        (fun idType _ ->
          match idType with
          | "guid" -> Ok None
          | _ -> Error "Unsupported price ID type.")
    >>= readElementText (fun id _ -> Guid.Parse id)
    >>= expectEndElement
    >>= parseCommodityRef "commodity"
        (fun commodityRef state -> (commodityRef, state))
    >>= parseCommodityRef "currency"
        (fun commodityRef state -> (commodityRef, state))
    >>= parseDate "time"
        (fun dateTime state -> (dateTime, state))
    >>= expectElement "source"
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
            )
    >>= expectEndElement
    >>= expectElement "value"
    >>= readElementTextResult
            (fun text state ->
                match parseAmount text with
                | Ok amount -> Ok (amount, state)
                | Error error -> Error error)
    >>= expectEndElement
    |> mapValue
           (fun (amount, (source, (dateTime, (currency, (commodity, id)))))
                ->
                    { Id = id; Commodity = commodity; Currency = currency
                      Time = dateTime; Source = source; PriceType = None
                      Value = amount }
                     |> Some |> Ok
                    )

[<Fact(Skip="todo igor: read time in the right timezone")>]
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