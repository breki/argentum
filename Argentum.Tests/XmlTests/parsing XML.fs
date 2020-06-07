module Argentum.Tests.XmlTests.``parsing XML``

open Argentum.Parsing.XmlParsing
open Argentum.Tests.XmlTests.ParsingHelpers
open FsUnit
open Xunit
open Swensen.Unquote

[<Fact>]
let ``Reads element text when non-empty``() =
    let xml = @"<a>some text</a>"
    let doc = buildXml xml |> withReader
    
    test <@
             doc
             |> expectAndReadElementText "a" (fun text _ -> text)
             |> parsedValue = Ok "some text"
         @>
    

[<Fact>]
let ``Returns empty string when element text is empty``() =
    let xml = @"<a></a>"
    let doc = buildXml xml |> withReader
    
    test <@
             doc
             |> expectAndReadElementText "a" (fun text _ -> text)
             |> parsedValue = Ok ""
         @>
    

