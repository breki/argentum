module Argentum.Tests.XmlTests.ParsingHelpers

open System.IO
open System.Xml
open Argentum.Parsing.XmlParsing

let buildXml xml =
    let prefix = @"<?xml version='1.0' encoding='utf-8' ?>
<gnc-v2
     xmlns:gnc='http://www.gnucash.org/XML/gnc'
     xmlns:act='http://www.gnucash.org/XML/act'
     xmlns:book='http://www.gnucash.org/XML/book'
     xmlns:cd='http://www.gnucash.org/XML/cd'
     xmlns:cmdty='http://www.gnucash.org/XML/cmdty'
     xmlns:price='http://www.gnucash.org/XML/price'
     xmlns:slot='http://www.gnucash.org/XML/slot'
     xmlns:split='http://www.gnucash.org/XML/split'
     xmlns:sx='http://www.gnucash.org/XML/sx'
     xmlns:trn='http://www.gnucash.org/XML/trn'
     xmlns:ts='http://www.gnucash.org/XML/ts'
     xmlns:fs='http://www.gnucash.org/XML/fs'
     xmlns:bgt='http://www.gnucash.org/XML/bgt'
     xmlns:recurrence='http://www.gnucash.org/XML/recurrence'
     xmlns:lot='http://www.gnucash.org/XML/lot'
     xmlns:addr='http://www.gnucash.org/XML/addr'
     xmlns:billterm='http://www.gnucash.org/XML/billterm'
     xmlns:bt-days='http://www.gnucash.org/XML/bt-days'
     xmlns:bt-prox='http://www.gnucash.org/XML/bt-prox'
     xmlns:cust='http://www.gnucash.org/XML/cust'
     xmlns:employee='http://www.gnucash.org/XML/employee'
     xmlns:entry='http://www.gnucash.org/XML/entry'
     xmlns:invoice='http://www.gnucash.org/XML/invoice'
     xmlns:job='http://www.gnucash.org/XML/job'
     xmlns:order='http://www.gnucash.org/XML/order'
     xmlns:owner='http://www.gnucash.org/XML/owner'
     xmlns:taxtable='http://www.gnucash.org/XML/taxtable'
     xmlns:tte='http://www.gnucash.org/XML/tte'
     xmlns:vendor='http://www.gnucash.org/XML/vendor'>
"
    let suffix = @"</gnc-v2>"

    let fullXml = prefix + xml + suffix
    use textReader = new StringReader(fullXml)
    
    let settings = XmlReaderSettings()
    settings.IgnoreComments <- true
    settings.IgnoreWhitespace <- true
    settings.IgnoreProcessingInstructions <- true
    
    let xmlReader = XmlReader.Create(textReader, settings)
    xmlReader.Read() |> ignore
    xmlReader.Read() |> ignore
    xmlReader.Read() |> ignore
    xmlReader

let withReader reader: ParseContext<unit> = (reader, ())

let parsedValue (result: ParseResult<'T>): Result<'T, string> =
    match result with
    | Ok (_, value) -> Ok value
    | Error err -> Error err

let parsedOptionalValue (result: ParseResult<'T option>)
    : Result<'T option, string> =
    match result with
    | Ok (_, value) -> Ok value
    | Error err -> Error err

