module Argentum.Tests.XmlTests.``parsing slots``

open System
open System.Globalization
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
    xmlReader

let readAttribute (attributeName: string) (reader: XmlReader)
    : Result<string, string> =
    match reader.GetAttribute(attributeName) with
    | null ->
        sprintf "Attribute '%s' is missing." attributeName |> Error
    | value -> Ok value

let expectNode
    (expectedType: XmlNodeType)
    (nodeResultFunc: XmlReader -> Result<'T, string>)
    (reader: XmlReader)
    : Result<'T, string> =
    if reader.Read() then
        match reader.NodeType with
        | nodeType when nodeType = expectedType -> nodeResultFunc reader
        | nodeType ->
            sprintf
                "Expected XML node type '%A', got '%A'" expectedType nodeType
            |> Error    
    else
        Error "Unexpected end of XML"

let expectElement
    expectedElementName
    (elementResultFunc: XmlReader -> Result<'T, string>)
    (reader: XmlReader) : Result<'T, string> =
        
    if reader.Read() then
        match reader.NodeType with
        | XmlNodeType.Element -> Ok()
        | nodeType ->
            sprintf
                "Expected XML node type '%A', got '%A'"
                XmlNodeType.Element nodeType
            |> Error    
    else
        Error "Unexpected end of XML"
    
    |> Result.bind (fun () ->
        match reader.LocalName with
        | elementName when elementName = expectedElementName ->
            elementResultFunc reader
        | elementName ->
            sprintf "Expected '%s' element, got '%s'"
                expectedElementName elementName
            |> Error)
let expectEndElement (value: 'T) (reader: XmlReader)
    : Result<'T, string> =
    expectNode XmlNodeType.EndElement (fun _ -> Ok value) reader

let readElementText (reader: XmlReader): Result<string, string> =
    expectNode
        XmlNodeType.Text
        (fun reader -> expectEndElement (reader.Value) reader)
        reader

let parseSlotValue slotType reader =
    match slotType with
    | "string" ->
        readElementText reader
        |> Result.bind (fun value -> SlotString(value) |> Ok)
    | "guid" ->
        readElementText reader
        |> Result.bind (fun value -> SlotGuid(Guid.Parse(value)) |> Ok)
    | "gdate" ->
        reader
        |> expectElement "gdate" (fun reader ->
            reader |> readElementText
            |> Result.bind (fun gdateStr -> 
                let (success, parsedDate) =
                        DateTime.TryParse
                            (gdateStr, CultureInfo.InvariantCulture,
                                DateTimeStyles.AssumeLocal)
                match success with
                | true -> SlotDate(parsedDate) |> Ok
                | false -> sprintf "Invalid date '%s'" gdateStr |> Error)
        )
    | unknown -> sprintf "Unsupported slot type '%s'." unknown |> Error

let parseSlot (reader: XmlReader): Result<Slot, string> =
    reader 
    |> expectElement "key" (fun reader -> readElementText reader)
    |> Result.bind (fun slotKey ->
        expectElement "value"
            (fun reader -> 
                readAttribute "type" reader
                |> Result.bind (fun slotType -> 
                    parseSlotValue slotType reader
                    |> Result.bind (fun value -> 
                        Ok { Key = slotKey; Value = value})
                )
            )
            reader
        )

[<Fact>]
let ``Can parse string slot``() =
    let xml = @"
        <slot:key>color</slot:key>
        <slot:value type='string'>#1469EB</slot:value>"
    let doc = buildXml xml
      
    test <@ parseSlot doc = Ok {Key = "color"; Value = SlotString("#1469EB")} @>

[<Fact>]
let ``Can parse date slot``() =
    let xml = @"
        <slot:key>color</slot:key>
        <slot:value type='gdate'>
            <gdate>2019-04-30</gdate>
        </slot:value>"
    let doc = buildXml xml
      
    test <@ parseSlot doc = Ok {
        Key = "color"
        Value = SlotDate(DateTime(2019, 04, 30))
    } @>

[<Fact>]
let ``Can parse GUID slot``() =
    let xml = @"
        <slot:key>color</slot:key>
        <slot:value type='guid'>5dd0adfe20184e33aff8a52b011dc65b</slot:value>"
    let doc = buildXml xml
      
    test <@ parseSlot doc = Ok {
        Key = "color"
        Value = SlotGuid(Guid("5dd0adfe20184e33aff8a52b011dc65b"))
    } @>

[<Fact>]
let ``Identifies invalid date slot structure``() =
    let xml = @"
        <slot:key>color</slot:key>
        <slot:value type='gdate'>
            <something>2019-04-30</something>
        </slot:value>"
    let doc = buildXml xml
      
    test <@ parseSlot doc = Error "Expected 'gdate' element, got 'something'" @>

[<Fact>]
let ``Identifies invalid date slot date value``() =
    let xml = @"
        <slot:key>color</slot:key>
        <slot:value type='gdate'>
            <gdate>xxsds</gdate>
        </slot:value>"
    let doc = buildXml xml
      
    test <@ parseSlot doc = Error "Invalid date 'xxsds'" @>
