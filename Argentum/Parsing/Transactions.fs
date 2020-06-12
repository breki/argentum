module Argentum.Parsing.Transactions

open System
open Argentum.Model
open Argentum.Parsing.XmlParsing
open Argentum.Parsing.Core
open Argentum.Parsing.Slots

let parseTransaction context =
    let parseSplit context =
        context
        |> expectElementAndMove "split"
        >>= expectAndReadElementText "id" (fun id _ -> Guid.Parse id)
        >>= parseConditional "memo"
            
        >>= expectAndReadElementText "reconciled-state"
              (fun reconciledStr state ->
                  let reconciled  =
                      match reconciledStr with
                      | "n" -> NotReconciled
                      | "c" -> Reconciled
                      | _ ->
                          sprintf
                            "Unsupported reconciled state '%s'." reconciledStr
                          |> invalidOp
              
                  (reconciled, state)
              )
        >>= expectAndReadElementText "value"
                (fun text state ->
                    match parseAmount text with
                    | Ok amount -> (amount, state)
                    | Error error ->
                        sprintf "Invalid amount '%s': %s." text error
                        |> invalidOp
                )        
        >>= expectAndReadElementText "quantity"
                (fun text state ->
                    match parseAmount text with
                    | Ok amount -> (amount, state)
                    | Error error ->
                        sprintf "Invalid amount '%s': %s." text error
                        |> invalidOp
                )        
        >>= parseAccountRef "account"
        >>= parseConditional "slots"
              (fun context -> context |> parseSlots pushToState)
              (fun state -> ([||], state))        
        >>= mapValue
                (fun (slots, (account, (quantity, (value, (reconciled, id))))) ->
                    { Id = id; ReconciledState = reconciled
                      Value = value; Quantity = quantity
                      Account = account; Slots = slots } |> Some |> Ok
                 )
        >>= expectEndElementWithName "split" >>= moveNext
        
    let parseSplits
        (stateUpdate: Split list -> 'T -> Split list * 'U)
        (context: ParseContext<'T>)
        : ParseResult<Split list * 'U> =
        let (_, state) = context
            
        let splits =
            context
            |> expectElementAndMove "splits"
            >>= parseList "split" parseSplit
            >>= expectEndElementWithName "splits" >>= moveNext
        match splits with
        | Ok (reader, splits) -> 
            let newState = stateUpdate splits state
            Ok (reader, newState)
        | Error error -> Error error
        
    context
    |> expectElementAndMove "transaction"
    >>= expectAndReadElementText "id" (fun id _ -> Guid.Parse id)
    >>= parseCommodityRef "currency" pushToState
    >>= parseTime "date-posted" pushToState
    >>= parseTime "date-entered" pushToState
    >>= expectAndReadElementText "description" pushToState
    >>= parseConditional "slots"
          (fun context -> context |> parseSlots pushToState)
          (fun state -> ([||], state))
    >>= parseSplits pushToState
    >>= expectEndElementWithName "transaction" >>= moveNext
    >>= mapValue
            (fun (splits, (slots, (description, (dateEntered,
                                         (datePosted, (currency, id)))))) ->
                {
                  Id = id; Currency = currency
                  DatePosted = datePosted; DateEntered = dateEntered
                  Description = description; Slots = slots
                  Splits = splits |> List.toArray }
                |> Some |> Ok
             )


