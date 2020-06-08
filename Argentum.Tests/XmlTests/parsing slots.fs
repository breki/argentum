module Argentum.Tests.XmlTests.``parsing slots``

open System
open Argentum.Model
open Argentum.Parsing.Slots
open Argentum.Tests.XmlTests.ParsingHelpers
open FsUnit
open Xunit
open Swensen.Unquote

[<Fact>]
let ``Can parse string slot``() =
    let xml = @"
    <slot>
        <slot:key>color</slot:key>
        <slot:value type='string'>#1469EB</slot:value>
    </slot>"
    let doc = buildXml xml |> withReader
      
    let expectedSlot = Some { Key = "color"; Value = SlotString("#1469EB")}
    test <@ parseSlot doc |> parsedOptionalValue = Ok expectedSlot @>

[<Fact>]
let ``Can parse numeric slot``() =
    let xml = @"
    <slot>
      <slot:key>0</slot:key>
      <slot:value type='numeric'>15/1</slot:value>
    </slot>"
    let doc = buildXml xml |> withReader
      
    let expectedSlot = Some {
        Key = "0"; Value = SlotNumeric(amount1 15)
    }
    test <@ parseSlot doc |> parsedOptionalValue = Ok expectedSlot @>

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

    let expectedSlot = Some {
        Key = "color"
        Value = SlotDate(DateTime(2019, 04, 30))
    }
    
    let actualValue = parseSlot doc |> parsedOptionalValue
    test <@ actualValue = Ok expectedSlot @>

[<Fact>]
let ``Can parse GUID slot``() =
    let xml = @"
    <slot>
        <slot:key>color</slot:key>
        <slot:value type='guid'>5dd0adfe20184e33aff8a52b011dc65b</slot:value>
    </slot>"
    let doc = buildXml xml |> withReader

    let expectedSlot = Some {
        Key = "color"
        Value = SlotGuid(Guid("5dd0adfe20184e33aff8a52b011dc65b"))
    }       
    test <@ parseSlot doc |> parsedOptionalValue = Ok expectedSlot @>

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
      
    test <@ parseSlot doc |> parsedOptionalValue
                = Error "Expected 'gdate' element, got Element 'something'" @>

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
      
    test <@ parseSlot doc |> parsedOptionalValue
                = Error "Invalid date 'xxsds'" @>

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
      
    let expectedSlot = Some {
        Key = "e44783b10c214f3cba6de0d98bb6983f"
        Value = SlotOfSlots
                    ([| { Key = "color"
                          Value = SlotString("#1469EB") } |])
    }
    
    test <@ parseSlot doc |> parsedOptionalValue = Ok expectedSlot @>

[<Fact>]
let ``Can parse a list of slots``() =
    let xml = @"
    <slot>
      <slot:key>e44783b10c214f3cba6de0d98bb6983f</slot:key>
      <slot:value type='frame'>
        <slot>
          <slot:key>0</slot:key>
          <slot:value type='numeric'>15/1</slot:value>
        </slot>
        <slot>
          <slot:key>1</slot:key>
          <slot:value type='numeric'>15/1</slot:value>
        </slot>
      </slot:value>
    </slot>"
    let doc = buildXml xml |> withReader

    let expectedSlot = Some {
        Key = "e44783b10c214f3cba6de0d98bb6983f"
        Value = SlotOfSlots
                    ([| { Key = "0"; Value = SlotNumeric(amount1 15) };
                        { Key = "1"; Value = SlotNumeric(amount1 15) }
                    |])
    }       
    test <@ parseSlot doc |> parsedOptionalValue = Ok expectedSlot @>
