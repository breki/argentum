module Argentum.Parsing.Slots

open System
open System.Globalization
open Argentum.Model
open Argentum.Parsing.XmlParsing
open Argentum.Parsing.Core

let parseSlotList
    (parseSlot: ParseContext<'T> -> ParseResult<Slot option>)
    context
    : ParseResult<Slot list> =
    context |> parseList "slot" parseSlot

let rec parseSlot<'T> (context: ParseContext<'T>): ParseResult<Slot option> =
    let parseSlotValue (context: ParseContext<string>)
        : (ParseResult<SlotValue>) =
            
        let (_, valueType) = context
        match valueType with
        | "string" ->
            context
            |> readElementText (fun value _ -> SlotString(value))
            >>= moveNext
            >>= expectEndElement
        | "numeric" ->
            context
            |> readElementTextResult (fun strNumeric _ ->
                strNumeric
                |> parseAmount
                |> Result.bind (fun amount -> amount |> SlotNumeric |> Ok))
            >>= moveNext
            >>= expectEndElement
        | "guid" ->
            context
            |> readElementText(fun value _ -> SlotGuid(Guid.Parse(value)))
            >>= moveNext
            >>= expectEndElement            
        | "gdate" ->
            context
            |> expectElement "gdate"
            >>= moveNext
            >>= readElementTextResult (fun gdateStr _ -> 
                let (success, parsedDate) =
                        DateTime.TryParse
                            (gdateStr, CultureInfo.InvariantCulture,
                                DateTimeStyles.AssumeLocal)
                match success with
                | true -> SlotDate(parsedDate) |> Ok
                | false -> sprintf "Invalid date '%s'" gdateStr |> Error)
            >>= moveNext
            >>= expectEndElement            
        | "frame" ->
            match parseSlotList parseSlot context with
            | Ok (reader, childrenSlots) ->              
                Ok (reader, SlotOfSlots (childrenSlots |> Array.ofList))
            | Error error -> Error error
        | unknown ->
            sprintf "Unsupported slot type '%s'." unknown |> Error

    let readSlotValue ((_, slotKey): ParseContext<string>) =
        context
        |> readAttribute "type" (fun typ _ -> typ) >>= moveNext
        >>= parseSlotValue
        |> mapValue (fun value -> Ok { Key = slotKey; Value = value})
    
    let parseIfSlot context =
        context
        |> moveNext
        >>= expectElement "key" >>= moveNext
        >>= readElementText (fun value _ -> value) >>= moveNext
        >>= expectEndElement >>= moveNext
        >>= expectElement "value"
        >>= readSlotValue >>= moveNext
        >>= expectEndElement >>= moveNext
    
    context
    |> parseIfElement "slot" parseIfSlot

