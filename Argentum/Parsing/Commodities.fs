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
            |> expectElementAndMove "id"
            >>= readElementTextAndMove pushToState
            >>= expectEndElementAndMove
            >>= skipToElementEnd
            >>= (fun (reader, (id, (_, (version)))) ->
                    let commodity
                        = Currency { Version = Version(version); Id = id }
                    Ok (reader, Some commodity))
        | "template" ->
            let (reader, _) = context
            Ok (reader, None)
        | _ ->
            sprintf "Commodity space '%s' is not supported." space
            |> invalidOp
    
    context
    |> expectElement "commodity"
    >>= readAttribute "version" (fun version _ -> version) >>= moveNext
    >>= expectElementAndMove "space"
    >>= readElementTextAndMove pushToState
    >>= expectEndElementAndMove
    >>= parseCommodityBasedOnSpace

