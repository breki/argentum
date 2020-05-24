module Argentum.Parsing.XmlParsing

open System.Xml

type ParseContext<'T> = XmlReader * 'T

type ParseResult<'T> = Result<(ParseContext<'T>), string>

let (>>=)
    (result: ParseResult<'T>)
    (parsingFunc: ParseContext<'T> -> ParseResult<'U>)
    : ParseResult<'U> =
    result |> Result.bind parsingFunc

/// <summary>
/// Reads the value of the specified attribute and then using this value it
/// updates the parsing context state using the provided function.
/// </summary>
let readAttribute
    (attributeName: string)
    (stateUpdate: string -> 'T -> 'U)
    ((reader, state): ParseContext<'T>)
    : ParseResult<'U> =
    match reader.GetAttribute(attributeName) with
    | null ->
        sprintf "Attribute '%s' is missing." attributeName |> Error
    | value -> Ok (reader, stateUpdate value state)

let moveNext ((reader: XmlReader), _): ParseResult<unit> =
    if reader.Read() then Ok (reader, ())
    else Result.Error "Unexpected end of XML"

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

let parseIfElement
    expectedElementName
    (parseFunc: ParseContext<'T> -> ParseResult<'U>) 
    (context: ParseContext<'T>)
     : ParseResult<'U option> =

    let (reader, _) = context
    
    if reader.Read() then
        match reader.NodeType with
        | XmlNodeType.Element ->
            match reader.LocalName with
            | elementName when elementName = expectedElementName ->
                match parseFunc context with
                | Ok (reader, value) -> Ok (reader, Some value)
                | Error error -> Error error
            | _ -> Ok(reader, None)
        | _ -> Ok(reader, None)
    else
        Ok(reader, None)
    
let expectEndElement (context: ParseContext<'T>): ParseResult<'T> =
    expectNode XmlNodeType.EndElement context

let skipToElementEnd (context: ParseContext<'T>): ParseResult<'T> =
    let (reader, value) = context
    reader.Skip() |> ignore
    Ok (reader, value)

/// <summary>
/// Reads the current XML element's inner text and then using this value it
/// updates the parsing context state using the provided function.
/// </summary>
let readElementText
    (stateUpdate: string -> 'T -> 'U)
    (context: ParseContext<'T>)
    : ParseResult<'U> =
    let readNodeValue ((reader, state): ParseContext<'T>): ParseResult<'U> =
        let nodeValue = reader.Value
        let newState = stateUpdate nodeValue state
        Ok (reader, newState)
    
    expectNode XmlNodeType.Text context
    >>= readNodeValue
    >>= expectEndElement

/// <summary>
/// Reads the current XML element's inner text and then using this value it
/// updates the parsing context state using the provided function. The
/// function returns a Result.
/// </summary>
let readElementTextResult
    (stateUpdate: string -> 'T -> Result<'U, string>)
    (context: ParseContext<'T>)
    : ParseResult<'U> =
    let readNodeValue ((reader, state): ParseContext<'T>): ParseResult<'U> =
        let nodeValue = reader.Value
        match stateUpdate nodeValue state with
        | Ok newState -> Ok (reader, newState)
        | Error error -> Error error
    
    expectNode XmlNodeType.Text context
    >>= readNodeValue
    >>= expectEndElement

let mapValue mapFunc (result: ParseResult<'T>): ParseResult<'U> =
    match result with
    | Error err -> Error err
    | Ok (reader, parseValue) ->
        match mapFunc parseValue with
        | Ok newValue -> Ok (reader, newValue)
        | Error err -> Error err
