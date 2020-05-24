module Argentum.Tests.XmlTests.``parsing commodities``

open System
open Argentum.Model
open Argentum.Parsing.XmlParsing
open FsUnit
open Xunit
open Swensen.Unquote
open Argentum.Tests.XmlTests.ParsingHelpers

let parseCommodity context: ParseResult<Commodity option> =
    let parseCommodityBasedOnSpace (context: ParseContext<string list>) =
        let (_, state) = context
        let space = state.[0]
        match space with
        | "CURRENCY" ->
            context
            |> expectElement "id"
            >>= readElementText (fun id state -> id :: state)
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
    >>= expectElement "space"
    >>= readElementText (fun space state -> space :: state)
    >>= parseCommodityBasedOnSpace

[<Fact>]
let ``Can parse currency``() =
    let xml = @"
        <gnc:commodity version='2.0.0'>
          <cmdty:space>CURRENCY</cmdty:space>
          <cmdty:id>CAD</cmdty:id>
          <cmdty:get_quotes/>
          <cmdty:quote_source>currency</cmdty:quote_source>
          <cmdty:quote_tz/>
        </gnc:commodity>
        "
    let doc = buildXml xml |> withReader

    let expectedCurrency =
        Currency { Version = Version(2, 0, 0); Id = "CAD" }
        |> Some

    test <@ parseCommodity doc |> parsedValue = Ok expectedCurrency @>

[<Fact>]
let ``Ignores commodity template``() =
    let xml = @"
        <gnc:commodity version='2.0.0'>
          <cmdty:space>template</cmdty:space>
          <cmdty:id>template</cmdty:id>
          <cmdty:name>template</cmdty:name>
          <cmdty:xcode>template</cmdty:xcode>
          <cmdty:fraction>1</cmdty:fraction>
        </gnc:commodity>
        "
    let doc = buildXml xml |> withReader

    test <@ parseCommodity doc |> parsedValue = Ok None @>
