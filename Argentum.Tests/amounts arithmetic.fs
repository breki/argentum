module Argentum.Tests.``amounts arithmetic``

open Argentum.ExchangeRates
open Argentum.Model
open Argentum.Tests.Builders
open FsUnit
open Xunit
open Swensen.Unquote

[<Fact>]
let ``amounts are same if dividends and divisors are the same``() =
    test <@ amount2 100 200 = amount2 100 200 @>

[<Fact>]
let ``amounts are same if ratios between dividend and divisor are the same``() =
    test <@ amount2 200 400 = amount2 100 200 @>
    
