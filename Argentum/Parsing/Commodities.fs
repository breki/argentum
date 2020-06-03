module Argentum.Parsing.Commodities

open System
open Argentum.Model
open Argentum.Parsing.XmlParsing

let parseCommodity context: ParseResult<Commodity option> =
    let parseCommodityBasedOnSpace (context: ParseContext<string list>) =
        let (_, state) = context
        let space = state.[0]
        match space with
        | "CURRENCY" ->
            context
            |> expectElement "id"
            >>= moveNext
            >>= readElementText (fun id state -> id :: state) >>= moveNext
            >>= expectEndElementAndMove
            >>= skipToElementEnd
            >>= (fun (reader, state) ->
                    let version = state.[2]
                    let id = state.[0]
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
    >>= readAttribute "version" (fun version _ -> [ version ])
    >>= moveNext
    >>= expectElement "space"
    >>= moveNext
    >>= readElementText (fun space state -> space :: state) >>= moveNext
    >>= expectEndElementAndMove
    >>= parseCommodityBasedOnSpace

