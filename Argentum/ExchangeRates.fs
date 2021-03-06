﻿module Argentum.ExchangeRates

open Argentum.Model

let exchangeRate time fromCommodity toCommodity (prices: Price seq) =
    match fromCommodity, toCommodity with
    | (x, y) when x = y -> amount1 1
    | _ ->
        let (pricesBefore, pricesAfter) =
            prices
            |> Seq.filter (fun price ->
                price.Commodity = fromCommodity && price.Currency = toCommodity)
            |> Seq.toList
            |> List.partition (fun price -> price.Time < time)
        
        match (pricesBefore |> List.rev), pricesAfter with
        | ([], []) -> invalidOp "Cannot find any prices."
        | ([], firstPriceAfter :: _) -> firstPriceAfter.Value
        | (lastPriceBefore :: _, _) -> lastPriceBefore.Value
