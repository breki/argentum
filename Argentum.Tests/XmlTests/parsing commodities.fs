module Argentum.Tests.XmlTests.``parsing commodities``

open System
open Argentum.Model
open Argentum.Parsing.Commodities
open FsUnit
open Xunit
open Swensen.Unquote
open Argentum.Tests.XmlTests.ParsingHelpers

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
