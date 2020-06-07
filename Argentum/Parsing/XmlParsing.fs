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
/// Returns a new state by creating a tuple of the value and the old state.
/// </summary>
let pushToState value state = (value, state)

let updateState stateUpdate context = 
    context
    |> (fun (reader, state) -> Ok (reader, stateUpdate state))
    
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

/// <summary>
/// Reads the value of the specified attribute and then using this value it
/// updates the parsing context state using the provided function.
/// </summary>
let readAttributeResult
    (attributeName: string)
    (stateUpdate: string -> 'T -> Result<'U, string>)
    ((reader, state): ParseContext<'T>)
    : ParseResult<'U> =
    match reader.GetAttribute(attributeName) with
    | null ->
        sprintf "Attribute '%s' is missing." attributeName |> Error
    | value ->
        stateUpdate value state
        |> Result.map (fun newState -> (reader, newState))

let moveNext ((reader: XmlReader), state): ParseResult<'T> =
    if not reader.EOF then
        reader.Read() |> ignore
        Ok (reader, state)
    else Result.Error "Unexpected end of XML"

let expectNode
    (expectedType: XmlNodeType)
    ((reader, parseValue): ParseContext<'T>)
    : ParseResult<'T> =

    if not reader.EOF then
        match reader.NodeType with
        | nodeType when nodeType = expectedType -> Ok(reader, parseValue)
        | nodeType ->
            sprintf
                "Expected XML node type '%A', got %A ('%A')"
                    expectedType nodeType reader.Name
            |> Error
    else
        Error "Unexpected end of XML"

let expectElement
    expectedElementName
    ((reader, parseValue): ParseContext<'T>)
     : ParseResult<'T> =

    if not reader.EOF then
        match reader.NodeType with
        | XmlNodeType.Element -> Ok()
        | nodeType ->
            sprintf
                "Expected XML node type %A ('%A'), got %A ('%A')"
                    XmlNodeType.Element expectedElementName nodeType reader.Name
            |> Error    
    else
        Error "Unexpected end of XML"

    |> Result.bind (fun () ->
        match reader.LocalName with
        | elementName when elementName = expectedElementName ->
            Ok(reader, parseValue)
        | elementName ->
            sprintf "Expected '%s' element, got %A '%s'"
                expectedElementName reader.NodeType elementName
            |> Error)

/// <summary>
/// If the current element is of expected name, parse it using the
/// provided function. Otherwise modify the state using the provided function. 
/// </summary>
/// <param name="expectedElementName">
/// The name of the element which should be parsed.
/// </param>
/// <param name="parseElement">
/// The parsing function which should be called if the element is there.
/// </param>
/// <param name="stateUpdateIfNoElement">
/// The parsing state update function which should be called in the element
/// is not there.
/// </param>
/// <param name="context">The parsing context.</param>
/// <returns>
/// The parsing result.
/// </returns>
let parseConditional
    expectedElementName
    (parseElement: ParseContext<'T> -> ParseResult<'U>) 
    (stateUpdateIfNoElement: 'T -> 'U) 
    (context: ParseContext<'T>)
     : ParseResult<'U> =
    let (reader, _) = context
    
    let whenNotFound() = context |> updateState stateUpdateIfNoElement
    
    if not reader.EOF then
        match reader.NodeType with
        | XmlNodeType.Element ->
            match reader.LocalName with
            | elementName when elementName = expectedElementName ->
                parseElement context
            | _ -> whenNotFound()
        | _ -> whenNotFound()
    else whenNotFound()

let parseIfElement
    expectedElementName
    (parseFunc: ParseContext<'T> -> ParseResult<'U>) 
    (context: ParseContext<'T>)
     : ParseResult<'U option> =

    let (reader, _) = context
    
    if not reader.EOF then
        match reader.NodeType with
        | XmlNodeType.Element ->
            match reader.LocalName with
            | elementName when elementName = expectedElementName ->
                parseFunc context
                |> Result.map (fun (reader, value) -> (reader, Some value))
            | _ -> Ok(reader, None)
        | _ -> Ok(reader, None)
    else
        Ok(reader, None)
    
/// <summary>
/// Expects the current node type to be an end element.
/// </summary>
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
//    let readNodeValue ((reader, state): ParseContext<'T>): ParseResult<'U> =
//        let nodeValue = reader.Value
//        let newState = stateUpdate nodeValue state
//        Ok (reader, newState)
//    
//    expectNode XmlNodeType.Text context
//    >>= readNodeValue
    
    let (reader, state) = context
    let text = reader.ReadElementContentAsString()
    let newState = stateUpdate text state
    Ok (reader, newState)

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
        stateUpdate nodeValue state
        |> Result.map (fun newState -> (reader, newState))
    
    expectNode XmlNodeType.Text context
    >>= readNodeValue

let mapValue
    (mapFunc: 'T -> Result<'U, string>)
    (context: ParseContext<'T>)
    : ParseResult<'U> =
    let reader, parseValue = context
    mapFunc parseValue |> Result.map (fun newValue -> (reader, newValue))


/// <summary>
/// Parses a list of items of the same type.
/// </summary>
/// <param name="itemElementName">
/// The name of the XML element representing a single item.
/// </param>
/// <param name="itemParser">
/// The function that parses the individual item.
/// </param>
/// <param name="context">The parsing context.</param>
/// <returns>
/// The parsing result with a list of 0 or more parsed items.
/// </returns>
let rec parseList
    (itemElementName: string)
    itemParser
    (context: ParseContext<'T>)
    : ParseResult<'U list> =
        
    let rec parseListInternal
        (items: 'U list)
        (itemElementName: string)
        itemParser
        (context: ParseContext<'T>)
        : ParseResult<'U list> =
            
        let (reader, _) = context
            
        match reader.NodeType, reader.LocalName with
        | XmlNodeType.Element, x when x = itemElementName ->       
            match itemParser context with
            | Ok (_, Some item) ->
                parseListInternal (item :: items) itemElementName itemParser context
            | Ok (_, None) -> Ok (reader, items)
            | Error error -> Error error
        | _ -> Ok (reader, items)

    context
    |> parseListInternal [] itemElementName itemParser
    >>= mapValue (fun reversedList -> reversedList |> List.rev |> Ok) 

let expectElementAndMove expectedElementName context =
    context |> expectElement expectedElementName >>= moveNext

/// <summary>
/// Expects the current node type to be an end element and moves forward.
/// </summary>
let expectEndElementAndMove context =
    context |> expectEndElement >>= moveNext

let readElementTextAndMove
    (stateUpdate: string -> 'T -> 'U)
    context =
    context |> readElementText stateUpdate >>= moveNext

let expectAndReadElementText
    expectedElementName
    (stateUpdate: string -> 'T -> 'U)
    context =
    context
    |> expectElementAndMove expectedElementName
    >>= readElementTextAndMove stateUpdate
    >>= expectEndElementAndMove
