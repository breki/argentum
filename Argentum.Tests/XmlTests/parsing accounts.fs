module Argentum.Tests.XmlTests.``parsing accounts``

open System
open System.Globalization
open Argentum.Model
open Argentum.Parsing.XmlParsing
open Argentum.Parsing.Core
open Argentum.Parsing.Slots
open FsUnit
open Xunit
open Swensen.Unquote
open Argentum.Tests.XmlTests.ParsingHelpers

let parseAccount context =
    
    let x =
        context
        |> expectElementAndMove "account"
        >>= expectElementAndMove "name"
        >>= readElementTextAndMove (fun name _ -> name)
        >>= expectEndElementAndMove
        >>= expectElementAndMove "id"
        >>= readElementTextAndMove (fun idStr state -> (Guid.Parse idStr, state))
        >>= expectEndElementAndMove
        >>= expectElementAndMove "type"
        >>= readElementTextAndMove
              (fun typeStr state ->
                  let accType =
                      match typeStr with
                      | "ASSET" -> AssetAccount
                      | "BANK" -> BankAccount
                      | "CASH" -> CashAccount
                      | "CREDIT" -> CreditAccount
                      | "EQUITY" -> EquityAccount
                      | "EXPENSE" -> ExpenseAccount
                      | "INCOME" -> IncomeAccount
                      | "LIABILITY" -> LiabilityAccount
                      | "RECEIVABLE" -> ReceivableAccount
                      | "ROOT" -> RootAccount
                      | _ ->
                        sprintf "Unsupported account type '%s'." typeStr
                        |> invalidOp
                      
                  (accType, state))
        >>= expectEndElementAndMove
        >>= parseCommodityRef "commodity" pushToState
        >>= expectElementAndMove "commodity-scu"
        >>= readElementTextAndMove
              (fun scuStr state ->
                (Int32.Parse(scuStr, CultureInfo.InvariantCulture), state))
        >>= expectEndElementAndMove
        >>= parseSlots pushToState
    
    let y =
        x |> mapValue
            (fun x ->
                let (slots, (scu, (commodity, (accType, (id, name))))) = x
                
                { Name = name; Type = accType; Id = id
                  Commodity = commodity; CommodityScu = scu
                  Description = None
                  Slots = slots; ParentAccount = None
                 } |> Some |> Ok
              )
          
    y
  
[<Fact>]
let ``Can parse root account``() =
    let xml = @"
<gnc:account version='2.0.0'>
  <act:name>Root Account</act:name>
  <act:id type='guid'>a6cb26f377e4ecbd6d5122484a4e283b</act:id>
  <act:type>ROOT</act:type>
  <act:commodity>
    <cmdty:space>CURRENCY</cmdty:space>
    <cmdty:id>EUR</cmdty:id>
  </act:commodity>
  <act:commodity-scu>100</act:commodity-scu>
  <act:slots>
    <slot>
      <slot:key>favorite</slot:key>
      <slot:value type='string'>false</slot:value>
    </slot>
  </act:slots>
</gnc:account>
    "

    let doc = buildXml xml |> withReader
    
    let expectedAccount =
      {
        Name = "Root Account"; Type = RootAccount
        Id = Guid.Parse("a6cb26f377e4ecbd6d5122484a4e283b")
        Commodity = CurrencyRef "EUR"; CommodityScu = 100
        Description = None; ParentAccount = None
        Slots = [|
          { Key = "favorite"; Value = SlotString "false" }
        |]
      } |> Some |> Ok

    test <@ parseAccount doc |> parsedValue = expectedAccount @>