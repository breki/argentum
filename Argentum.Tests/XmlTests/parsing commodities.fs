module Argentum.Tests.XmlTests.``parsing commodities``

open System
open Argentum.Model
open Argentum.Parsing.XmlParsing
open FsUnit
open Xunit
open Swensen.Unquote
open Argentum.Tests.XmlTests.ParsingHelpers

let parseCommodity context: ParseResult<Commodity> =
    // fetch version attribute value
    // expect cmdty:space tag
    // fetch its value, it should be CURRENCY
    // expect cmdty:id tag
    // fetch its value, it is the ID of the currency
    // expect cmdty:get_quotes tag
    // expect <cmdty:quote_source>currency</cmdty:quote_source>
    // expect <cmdty:quote_tz/>
    // expect end of tag
    context
    |> expectElement "commodity"
    >>= readAttribute "version" (fun version _ -> version)
    >>= skipToElementEnd
    |> ignore

    let (reader, _) = context
    let commodity = Currency { Version = Version(2, 0, 0); Id = "CAD" }
    Ok (reader, commodity)

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
        Currency {
                Version = Version(2, 0, 0)
                Id = "CAD"
            }

    test <@ parseCommodity doc |> parsedValue = Ok expectedCurrency @>
