module Argentum.Tests.XmlTests.``parsing whole files``

open System.Collections.Generic
open System.IO
open System.Xml
open Argentum.Model
open Argentum.Parsing.XmlParsing
open Argentum.Parsing.Commodities
open Argentum.Parsing.Accounts
open Argentum.Parsing.Transactions
open Argentum.Parsing.Prices
open FsUnit
open Xunit
open Swensen.Unquote

type Parser<'T> = ParseContext<'T> -> ParseResult<'T>

let rec parseElements
    (parsingRoutes: IDictionary<string, Parser<'T>>)  
    (context: ParseContext<'T>): ParseResult<'T> =
    let (reader, _) = context
    
    match reader.NodeType with
    | XmlNodeType.Element ->
        let elementName = reader.LocalName 
        match parsingRoutes.TryGetValue elementName with
        | (true, parser) ->
            let parsingResult = context |> parser
            match parsingResult with
            | Ok context -> parseElements parsingRoutes context
            | Error error -> Error error
        | (false, _) ->
            let result = context |> skipToNext
            match result with
            | Ok context -> parseElements parsingRoutes context
            | Error error -> Error error
    | XmlNodeType.EndElement -> Ok context
    | unexpectedNodeType ->
        sprintf
            "Expected element type, but got %A ('%s')"
            unexpectedNodeType reader.LocalName
        |> invalidOp

let handleCommodity (context: ParseContext<Book>): ParseResult<Book> =
    match context |> parseCommodity with
    | Ok (_, Some commodity) ->
        let (reader, book) = context
        let updatedBook =
            { book with Commodities = commodity :: book.Commodities }
        Ok (reader, updatedBook)
    | Ok (_, None) -> Ok context
    | Error error -> Error error

let handlePriceDb (context: ParseContext<Book>): ParseResult<Book> =
    match context |> parsePriceDb with
    | Ok (_, priceDb) ->
        let (reader, book) = context
        let updatedBook =
            { book with PriceDb = Some priceDb }
        Ok (reader, updatedBook)
    | Error error -> Error error

let handleAccount (context: ParseContext<Book>): ParseResult<Book> =
    match context |> parseAccount with
    | Ok (_, Some account) ->
        let (reader, book) = context
        let updatedBook =
            { book with Accounts = account :: book.Accounts }
        Ok (reader, updatedBook)
    | Ok (_, None) -> Ok context
    | Error error -> Error error

let handleTransaction (context: ParseContext<Book>): ParseResult<Book> =
    match context |> parseTransaction with
    | Ok (_, Some transaction) ->
        let (reader, book) = context
        let updatedBook =
            { book with Transactions = transaction :: book.Transactions }
        Ok (reader, updatedBook)
    | Ok (_, None) -> Ok context
    | Error error -> Error error

let parseXmlFile (fileName: string): Result<Book, string> = 
    let settings = XmlReaderSettings()
    settings.IgnoreComments <- true
    settings.IgnoreWhitespace <- true
    settings.IgnoreProcessingInstructions <- true
    
    use fileReader = new StreamReader(fileName)
    use xmlReader = XmlReader.Create(fileReader, settings)

    let context =
        (xmlReader,
         { Commodities = []; PriceDb = None; Accounts = []; Transactions = []})
    
    let parsingRoutes: IDictionary<string, Parser<Book>> = dict [
        ("commodity", handleCommodity)
        ("pricedb", handlePriceDb)
        ("account", handleAccount)
        ("transaction", handleTransaction)
    ]
        
    let parseResult =
        context
        |> moveNext
        >>= expectXmlDeclarationAndMove
        >>= expectElementAndMove "gnc-v2"
        >>= expectElementAndSkipToNext "count-data"
        >>= expectElementAndMove "book"
        >>= parseElements parsingRoutes
        >>= expectEndElementWithName "book" >>= moveNext
        >>= expectEndElementWithName "gnc-v2" >>= moveNext
    
    match parseResult with
    | Ok (_, book) -> Ok book 
    | Error error -> Error error

[<Fact>]
let ``Can parse whole XML file``() =
    
    let bookResult = parseXmlFile @"c:\temp\igor-gnucash"
    match bookResult with
    | Ok book ->
        test <@ book.Commodities |> List.length = 5 @>
        test <@ book.PriceDb |> Option.isSome @>
        test <@ book.PriceDb |> Option.get |> List.length = 40 @>
        test <@ book.Accounts |> List.length = 120 @>
        test <@ book.Transactions |> List.length = 10 @>
    | Error error -> invalidOp error 
