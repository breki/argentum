module Argentum.Tests.XmlTests.``parsing slots``

open System
open System.Globalization
open Argentum.Model
open Argentum.XmlParsing
open Argentum.Tests.XmlTests.ParsingHelpers
open FsUnit
open Xunit
open Swensen.Unquote

let rec parseSlot<'T> (context: ParseContext<'T>): ParseResult<Slot> =
    let parseSlotValue (context: ParseContext<string>)
        : (ParseResult<SlotValue>) =
            
        let (_, valueType) = context
        match valueType with
        | "string" ->
            context |> readElementText
            |> mapValue (fun value -> SlotString(value) |> Ok)
        | "guid" ->
            context |> readElementText
            |> mapValue (fun value -> SlotGuid(Guid.Parse(value)) |> Ok)
        | "gdate" ->
            context
            |> expectElement "gdate"
            >>= readElementText
            |> mapValue (fun gdateStr -> 
                let (success, parsedDate) =
                        DateTime.TryParse
                            (gdateStr, CultureInfo.InvariantCulture,
                                DateTimeStyles.AssumeLocal)
                match success with
                | true -> SlotDate(parsedDate) |> Ok
                | false -> sprintf "Invalid date '%s'" gdateStr |> Error)
        | "frame" ->
            context
//            |> expectElement "slot"
            |> parseSlot
            |> mapValue (fun innerSlot -> SlotOfSlots [| innerSlot |] |> Ok)
        | unknown ->
            sprintf "Unsupported slot type '%s'." unknown |> Error

    let readSlotValue ((_, slotKey): ParseContext<string>) =
        context
        |> readAttribute "type"
        >>= parseSlotValue
        |> mapValue (fun value -> Ok { Key = slotKey; Value = value})
    
    context
    |> expectElement "slot"
    >>= expectElement "key"
    >>= readElementText
    >>= expectElement "value"
    >>= readSlotValue

[<Fact>]
let ``Can parse string slot``() =
    let xml = @"
    <slot>
        <slot:key>color</slot:key>
        <slot:value type='string'>#1469EB</slot:value>
    </slot>"
    let doc = buildXml xml |> withReader
      
    test <@ parseSlot doc |> parsedValue = Ok {
        Key = "color"; Value = SlotString("#1469EB")
    } @>

[<Fact>]
let ``Can parse date slot``() =
    let xml = @"
    <slot>
        <slot:key>color</slot:key>
        <slot:value type='gdate'>
            <gdate>2019-04-30</gdate>
        </slot:value>
    </slot>"
    let doc = buildXml xml |> withReader
      
    test <@ parseSlot doc |> parsedValue = Ok {
        Key = "color"
        Value = SlotDate(DateTime(2019, 04, 30))
    } @>

[<Fact>]
let ``Can parse GUID slot``() =
    let xml = @"
    <slot>
        <slot:key>color</slot:key>
        <slot:value type='guid'>5dd0adfe20184e33aff8a52b011dc65b</slot:value>
    </slot>"
    let doc = buildXml xml |> withReader
      
    test <@ parseSlot doc |> parsedValue = Ok {
        Key = "color"
        Value = SlotGuid(Guid("5dd0adfe20184e33aff8a52b011dc65b"))
    } @>

[<Fact>]
let ``Identifies invalid date slot structure``() =
    let xml = @"
    <slot>
        <slot:key>color</slot:key>
        <slot:value type='gdate'>
            <something>2019-04-30</something>
        </slot:value>
    </slot>"
    let doc = buildXml xml |> withReader
      
    test <@ parseSlot doc |> parsedValue
                = Error "Expected 'gdate' element, got 'something'" @>

[<Fact>]
let ``Identifies invalid date slot date value``() =
    let xml = @"
    <slot>
        <slot:key>color</slot:key>
        <slot:value type='gdate'>
            <gdate>xxsds</gdate>
        </slot:value>
    </slot>"
    let doc = buildXml xml |> withReader
      
    test <@ parseSlot doc |> parsedValue = Error "Invalid date 'xxsds'" @>

[<Fact>]
let ``Can parse a hierarchy of slots``() =
    let xml = @"
    <slot>
      <slot:key>e44783b10c214f3cba6de0d98bb6983f</slot:key>
      <slot:value type='frame'>
        <slot>
          <slot:key>color</slot:key>
          <slot:value type='string'>#1469EB</slot:value>
        </slot>
      </slot:value>
    </slot>"
    let doc = buildXml xml |> withReader
      
    test <@ parseSlot doc |> parsedValue = Ok {
        Key = "e44783b10c214f3cba6de0d98bb6983f"
        Value = SlotOfSlots
                    ([| { Key = "color"
                          Value = SlotString("#1469EB") } |])
    } @>
