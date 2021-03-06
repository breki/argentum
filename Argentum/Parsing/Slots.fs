﻿module Argentum.Parsing.Slots

open System
open System.Globalization
open System.Xml
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
        | "numeric" ->
            context
            |> readElementTextResult (fun strNumeric _ ->
                strNumeric
                |> parseAmount
                |> Result.bind (fun amount -> amount |> SlotNumeric |> Ok))
        | "guid" ->
            context
            |> readElementText(fun value _ -> SlotGuid(Guid.Parse(value)))
        | "gdate" ->
            context
            |> moveNext
            >>= expectElement "gdate"
            >>= readElementTextResult (fun gdateStr _ -> 
                let (success, parsedDate) =
                        DateTime.TryParse
                            (gdateStr, CultureInfo.InvariantCulture,
                                DateTimeStyles.AssumeLocal)
                match success with
                | true -> SlotDate(parsedDate) |> Ok
                | false -> sprintf "Invalid date '%s'" gdateStr |> Error)
            >>= expectEndElementWithName "value" >>= moveNext
            >>= expectEndElementWithName "slot"
        | "frame" ->
            let slotListParsed = 
                context
                |> moveNext
                >>= parseSlotList parseSlot
            match slotListParsed with
            | Ok (reader, childrenSlots) ->
                context |> moveNext |> ignore
                Ok (reader, SlotOfSlots (childrenSlots |> Array.ofList))
            | Error error -> Error error
        | unknown ->
            sprintf "Unsupported slot type '%s'." unknown |> Error

    let readSlotValue ((_, slotKey): ParseContext<string>) =
        context
        |> readAttribute "type" (fun typ _ -> typ)
        >>= parseSlotValue           
        >>= mapValue (fun value -> Ok { Key = slotKey; Value = value})
    
    let parseIfSlot context =
        context
        |> moveNext
        >>= expectAndReadElementText "key" (fun value _ -> value)
        >>= expectElement "value"
        >>= readSlotValue
        >>= expectEndElementWithName "slot"
        >>= moveNext
    
    context
    |> parseIfElement "slot" parseIfSlot

let parseSlots
    (stateUpdate: Slot[] -> 'T -> 'U)
    context: ParseResult<'U> =
    let (_, state) = context
        
    context
    |> expectElementAndMove "slots"
    >>= (fun context ->
                parseSlotList parseSlot context
                |> Result.map (
                    fun (reader, slots) ->
                        let slotsArray = slots |> List.toArray
                        let newState = stateUpdate slotsArray state
                        (reader, newState))
            )
    >>= expectEndElementAndMove
