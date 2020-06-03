module Argentum.Parsing.Prices

open System
open Argentum.Model
open Argentum.Parsing.XmlParsing
open Argentum.Parsing.Core


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
    >>= moveNext
    |> mapValue
           (fun (amount, (priceType, (source, (dateTime,
                                               (currency, (commodity, id))))))
                ->
                    { Id = id; Commodity = commodity; Currency = currency
                      Time = dateTime; Source = source; PriceType = priceType
                      Value = amount }
                     |> Some |> Ok
                    )

let parsePriceDb (context: ParseContext<'T>): ParseResult<Price list> =
    context
    |> expectElement "pricedb" >>= moveNext
    >>= parseList "price" parsePrice
    >>= expectEndElement
