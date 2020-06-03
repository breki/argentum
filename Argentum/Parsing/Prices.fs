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
          | _ -> Error "Unsupported price ID type.") >>= moveNext
    >>= readElementTextAndMove (fun id _ -> Guid.Parse id)
    >>= expectEndElementAndMove
    >>= parseCommodityRef "commodity" pushToState
    >>= parseCommodityRef "currency" pushToState
    >>= parseTime "time" pushToState
    >>= expectElementAndMove "source"
    >>= readElementTextAndMove
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
    >>= expectEndElementAndMove
    >>= parseConditional "type"
        (fun context ->
            context
            |> moveNext
            >>= readElementTextAndMove
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
            >>= expectEndElementAndMove)
        (fun state -> (None, state))
    >>= expectElementAndMove "value"
    >>= readElementTextResult
            (fun text state ->
                match parseAmount text with
                | Ok amount -> Ok (amount, state)
                | Error error -> Error error) >>= moveNext
    >>= expectEndElementAndMove
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
    |> expectElementAndMove "pricedb"
    >>= parseList "price" parsePrice
    >>= expectEndElementAndMove
