module Argentum.Parsing.Core

open System
open Argentum.Model
open Argentum.Parsing.XmlParsing

let parseAmount (strAmount: string): Result<Amount, string> =
    match strAmount.IndexOf "/" with
    | i when i >= 0 ->
        let dividendStr = strAmount.Substring(0, i)
        let divisorStr = strAmount.Substring(i + 1)
        let (dividendOk, dividend) = Int32.TryParse dividendStr
        let (divisorOk, divisor) = Int32.TryParse divisorStr
        
        match dividendOk, divisorOk with
        | true, true -> amount2 dividend divisor |> Ok
        | _ -> sprintf "Invalid amount value '%s'." strAmount |> Error
    | _ -> sprintf "Invalid amount value '%s'." strAmount |> Error


/// <summary>
/// Parses the XML element containing a datetime entry.
/// </summary>
/// <param name="expectedElementName">
/// The name of the XML tag containing the ts:date entry.
/// </param>
/// <param name="stateUpdate">
/// The function which updates the parsing state using the parsed datetime,
/// which is converted to local time.
/// </param>
/// <param name="context">The parsing context.</param>
/// <remarks>
/// Datetimes in GnuCash are represented in UTC time. This function converts
/// it to local time.
/// </remarks>
/// <returns>
/// The parsing result with the updated state.
/// </returns>
let parseTime
    expectedElementName
    (stateUpdate: DateTime -> 'T -> 'U)
    (context: ParseContext<'T>)
    : ParseResult<'U> =
                
    let (reader, state) = context
    
    let dateTime =
        context
        |> expectElement expectedElementName >>= moveNext
        >>= expectElement "date" >>= moveNext
        >>= readElementText (fun dateTimeStr _ -> DateTime.Parse(dateTimeStr))
        >>= moveNext
        >>= expectEndElement >>= moveNext
        >>= expectEndElement >>= moveNext
        
    match dateTime with
    | Ok (_, commodityRef) ->
        let newState = state |> stateUpdate commodityRef
        Ok (reader, newState)
    | Error error -> Error error

let parseCommodityRef
    expectedElementName
    (stateUpdate: CommodityRef -> 'T -> 'U)
    (context: ParseContext<'T>)
    : ParseResult<'U> =
    
    let parseCommodityRefBasedOnSpace (context: ParseContext<string>) =
        let (_, space) = context
        match space with
        | "CURRENCY" ->
            context
            |> expectElement "id" >>= moveNext
            >>= readElementText (fun id _ -> id) >>= moveNext
            >>= expectEndElement >>= moveNext
            >>= skipToElementEnd
            >>= (fun (reader, id) ->
                    let commodityRef = CurrencyRef id 
                    Ok (reader, commodityRef))
        | _ ->
            sprintf "Commodity space '%s' is not supported." space
            |> invalidOp
            
    let (reader, state) = context
  
    let commodityRef =
        context
        |> expectElement expectedElementName >>= moveNext
        >>= expectElement "space" >>= moveNext
        >>= readElementText (fun space _ -> space ) >>= moveNext
        >>= expectEndElement >>= moveNext
        >>= parseCommodityRefBasedOnSpace
      
    match commodityRef with
    | Ok (_, commodityRef) ->
        let newState = state |> stateUpdate commodityRef
        Ok (reader, newState)
    | Error error -> Error error   
