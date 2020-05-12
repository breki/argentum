module Argentum.XmlParsing

open System.Xml

type ParsingXXX<'T> = XmlReader * 'T

type ParsingContext<'T> = Result<(ParsingXXX<'T>), string>

let readAttribute (attributeName: string) (context: ParsingContext<'T>)
    : ParsingContext<string> =
    match context with
    | Error err -> Error err
    | Ok (reader, _) ->
        match reader.GetAttribute(attributeName) with
        | null ->
            sprintf "Attribute '%s' is missing." attributeName |> Error
        | value -> Ok (reader, value)

let expectNode
    (expectedType: XmlNodeType)
    (context: ParsingContext<'T>)
    : ParsingContext<'T> =

    match context with
    | Error err -> Error err
    | Ok (reader, parseValue) ->
        if reader.Read() then
            match reader.NodeType with
            | nodeType when nodeType = expectedType -> Ok(reader, parseValue)
            | nodeType ->
                sprintf
                    "Expected XML node type '%A', got '%A'"
                        expectedType nodeType
                |> Error
        else
            Error "Unexpected end of XML"

let expectElement expectedElementName (context: ParsingContext<'T>)
     : ParsingContext<'T> =

    match context with
    | Error err -> Error err
    | Ok (reader, parseValue) ->
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
                Ok(reader, parseValue)
            | elementName ->
                sprintf "Expected '%s' element, got '%s'"
                    expectedElementName elementName
                |> Error)
    
let expectEndElement (context: ParsingContext<'T>): ParsingContext<'T> =
    expectNode XmlNodeType.EndElement context

let readElementText (context: ParsingContext<'T>): ParsingContext<string> =
    let readNodeValue (context: ParsingContext<'T>): ParsingContext<string> =
        match context with
        | Error err -> Error err
        | Ok (reader, _) -> Ok (reader, reader.Value)
    
    expectNode XmlNodeType.Text context
    |> readNodeValue
    |> expectEndElement

let map mapFunc (context: ParsingContext<'T>): ParsingContext<'U> =
    match context with
    | Error err -> Error err
    | Ok (reader, parseValue) ->
        match mapFunc parseValue with
        | Ok newValue -> Ok (reader, newValue)
        | Error err -> Error err

let (>>=)
    (context: ParsingContext<'T>)
    (parsingFunc: ParsingXXX<'T> -> ParsingContext<'U>)
    : ParsingContext<'U> =
    context |> Result.bind parsingFunc
