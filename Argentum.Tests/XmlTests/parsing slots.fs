module Argentum.Tests.XmlTests.``parsing slots``

open System.IO
open System.Xml
open Argentum.Model
open FsUnit
open Xunit
open Swensen.Unquote

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

let parseSlot (reader: XmlReader): Result<Slot, string> =
    let slotKey =
        match reader.NodeType with
        | XmlNodeType.Element ->
            match reader.LocalName with
            | "key" -> Ok reader.Value
            | elementName ->
                sprintf "Expected 'key' element, got '%s'" elementName
                |> Error
        | nodeType ->
            sprintf "Expected XML element element, got '%A'" nodeType
            |> Error

    match slotKey with
    | Error err -> Error err
    | Ok slotKey ->
        if reader.Read() then
            match reader.NodeType with
            | XmlNodeType.Element ->
                match reader.LocalName with
                | "value" ->
                    Ok { Key = slotKey; Value = SlotString(reader.Value)}
                | elementName ->
                    sprintf "Expected 'key' element, got '%s'" elementName
                    |> Error
            | nodeType ->
                sprintf "Expected XML element element, got '%A'" nodeType
                |> Error
        else
            Error "Unexpected end of XML"

[<Fact>]
let ``Can parse string slot``() =
    let xml = @"
        <slot:key>color</slot:key>
        <slot:value type='string'>#1469EB</slot:value>"
    let doc = buildXml xml
      
    test <@ parseSlot doc = Ok {Key = "color"; Value = SlotString("#1469EB")} @>
   