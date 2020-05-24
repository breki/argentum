module Argentum.Parsing.Core

open System
open Argentum.Model

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
