module Argentum.XmlParsing

open System.Xml

type ParseContext<'T> = XmlReader * 'T

type ParseResult<'T> = Result<(ParseContext<'T>), string>

let (>>=)
    (result: ParseResult<'T>)
    (parsingFunc: ParseContext<'T> -> ParseResult<'U>)
    : ParseResult<'U> =
    result |> Result.bind parsingFunc

let readAttribute (attributeName: string) ((reader, _): ParseContext<'T>)
    : ParseResult<string> =
    match reader.GetAttribute(attributeName) with
    | null ->
        sprintf "Attribute '%s' is missing." attributeName |> Error
    | value -> Ok (reader, value)

let expectNode
    (expectedType: XmlNodeType)
    ((reader, parseValue): ParseContext<'T>)
    : ParseResult<'T> =

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

let expectElement
    expectedElementName
    ((reader, parseValue): ParseContext<'T>)
     : ParseResult<'T> =

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
    
let expectEndElement (context: ParseContext<'T>): ParseResult<'T> =
    expectNode XmlNodeType.EndElement context

let readElementText (context: ParseContext<'T>): ParseResult<string> =
    let readNodeValue ((reader, _): ParseContext<'T>): ParseResult<string> =
        Ok (reader, reader.Value)
    
    expectNode XmlNodeType.Text context
    >>= readNodeValue
    >>= expectEndElement

let map mapFunc (result: ParseResult<'T>): ParseResult<'U> =
    match result with
    | Error err -> Error err
    | Ok (reader, parseValue) ->
        match mapFunc parseValue with
        | Ok newValue -> Ok (reader, newValue)
        | Error err -> Error err
