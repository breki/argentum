module Argentum.Parsing.Slots

open System
open System.Globalization
open Argentum.Model
open Argentum.Parsing.XmlParsing
open Argentum.Parsing.Core

let rec parseSlotList
    (slots: Slot list)
    (parseSlot: ParseContext<'T> -> ParseResult<Slot option>)
    context
    : ParseResult<Slot list> =
    let (reader, _) = context

    // is there another slot to parse?
    match parseSlot context with
    // if we found another slot...
    | Ok (_, Some slot) ->
        // parse the rest of the list, with our slot put at the front
        parseSlotList (slot :: slots) parseSlot context
    // if there are no more slots to parse, 
    | Ok (_, None) ->
        // return what has been collected so far
        Ok (reader, slots)
    | Error error -> Error error

let rec parseSlot<'T> (context: ParseContext<'T>): ParseResult<Slot option> =
    let parseSlotValue (context: ParseContext<string>)
        : (ParseResult<SlotValue>) =
            
        let (_, valueType) = context
        match valueType with
        | "string" ->
            context |> readElementText
            |> mapValue (fun value -> SlotString(value) |> Ok)
        | "numeric" ->
            context |> readElementText
            |> mapValue (fun strNumeric ->
                strNumeric
                |> parseAmount
                |> Result.bind (fun amount -> amount |> SlotNumeric |> Ok)
                )
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
            match parseSlotList [] parseSlot context with
            | Ok (reader, childrenSlotsReversed) ->
                let childrenSlots =
                    childrenSlotsReversed
                    |> Array.ofList
                    |> Array.rev
                
                Ok (reader, SlotOfSlots childrenSlots)
            | Error error -> Error error
        | unknown ->
            sprintf "Unsupported slot type '%s'." unknown |> Error

    let readSlotValue ((_, slotKey): ParseContext<string>) =
        context
        |> readAttribute "type"
        >>= parseSlotValue
        |> mapValue (fun value -> Ok { Key = slotKey; Value = value})
    
    let parseIfSlot context =
        context
        |> expectElement "key"
        >>= readElementText
        >>= expectElement "value"
        >>= readSlotValue
        >>= expectEndElement
    
    context
    |> parseIfElement "slot" parseIfSlot

