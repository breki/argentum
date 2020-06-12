module Argentum.Parsing.Commodities

open System
open Argentum.Model
open Argentum.Parsing.XmlParsing

let parseCommodity context: ParseResult<Commodity option> =
    let parseCommodityBasedOnSpace context =
        let (_, (space, _)) = context
        match space with
        | "CURRENCY" ->
            context
            |> expectAndReadElementText "id" pushToState
            >>= skipIfElement "name"
            >>= skipIfElement "xcode"
            >>= skipIfElement "fraction"
            >>= expectElementAndSkipToNext "get_quotes"
            >>= expectElementAndSkipToNext "quote_source"
            >>= expectElementAndSkipToNext "quote_tz"
            >>= expectEndElementWithName "commodity" >>= moveNext
            >>= (fun (reader, (id, (_, (version)))) ->
                    let commodity
                        = Currency { Version = Version(version); Id = id }
                    Ok (reader, Some commodity))
        | "template" ->
            let (reader, _) = context
            context
            |> skipIfElement "id"
            >>= skipIfElement "name"
            >>= skipIfElement "xcode"
            >>= skipIfElement "fraction"
            >>= expectEndElementWithName "commodity" >>= moveNext
            |> ignore
            Ok (reader, None)
        | _ ->
            sprintf "Commodity space '%s' is not supported." space
            |> invalidOp
    
    context
    |> expectElement "commodity"
    >>= readAttribute "version" (fun version _ -> version) >>= moveNext
    >>= expectAndReadElementText "space" pushToState
    >>= parseCommodityBasedOnSpace

