module Argentum.Parsing.Prices

open System
open Argentum.Model
open Argentum.Parsing.XmlParsing
open Argentum.Parsing.Core


let parsePrice context: ParseResult<Price option> =
    context
    |> expectElementAndMove "price"
    >>= expectElement "id"
    >>= readAttributeResult "type"
        (fun idType _ ->
          match idType with
          | "guid" -> Ok None
          | _ -> Error "Unsupported price ID type.")
    >>= readElementText (fun id _ -> Guid.Parse id)
    >>= parseCommodityRef "commodity" pushToState
    >>= parseCommodityRef "currency" pushToState
    >>= parseTime "time" pushToState
    >>= expectAndReadElementText "source"
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
    >>= parseConditional "type"
        (fun context ->
            context
            |> expectAndReadElementText "type"
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
                    )
                )
        (fun state -> (None, state))
    >>= expectElement "value"
    >>= readElementTextResult
            (fun text state ->
                parseAmount text |> Result.map (fun amount -> (amount, state))
                ) 
    >>= mapValue
           (fun (amount, (priceType, (source, (dateTime,
                                               (currency, (commodity, id))))))
                ->
                    { Id = id; Commodity = commodity; Currency = currency
                      Time = dateTime; Source = source; PriceType = priceType
                      Value = amount }
                     |> Some |> Ok
                    )
    >>= expectEndElementWithName "price" >>= moveNext

let parsePriceDb (context: ParseContext<'T>): ParseResult<PriceDb> =
    context
    |> expectElementAndMove "pricedb"
    >>= parseList "price" parsePrice
    >>= expectEndElementAndMove
