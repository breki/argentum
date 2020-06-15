module Argentum.Tests.``amounts arithmetic``

open Argentum.Model
open FsUnit
open Xunit
open Swensen.Unquote

let addAmount (amountA: Amount) (amountB: Amount) =
    match amountA.Divisor, amountB.Divisor with
    | (divisorA, divisorB) when divisorA = divisorB ->
        amount2 (amountA.Dividend + amountB.Dividend) divisorA
    | (divisorA, divisorB) when divisorA % divisorB = 0 ->
        let multiplier = divisorA / divisorB
        amount2 (amountA.Dividend + amountB.Dividend * multiplier) divisorA
    | (divisorA, divisorB) when divisorB % divisorA = 0 ->
        let multiplier = divisorB / divisorA
        amount2 (amountA.Dividend * multiplier + amountB.Dividend) divisorB
    | (divisorA, divisorB) ->
        let sumDivisor = divisorA * divisorB
        amount2
            (amountA.Dividend * divisorB + amountB.Dividend * divisorA)
            sumDivisor
[<Fact>]
let ``amounts are same if dividends and divisors are the same``() =
    test <@ amount2 100 200 = amount2 100 200 @>

[<Fact>]
let ``amounts with different dividends are not the same``() =
    test <@ amount2 100 200 <> amount2 150 200 @>

[<Fact>]
let ``amounts with different divisors are not the same``() =
    test <@ amount2 100 200 <> amount2 100 250 @>

[<Fact>]
let ``amounts are same if ratios between dividend and divisor are the same``() =
    test <@ amount2 200 400 = amount2 100 200 @>
    
[<Fact>]
let ``when adding amounts with the same divisor the sum retains the divisor``() =
    test <@ amount2 100 100 |> addAmount (amount2 50 100) = amount2 150 100 @>

[<Fact>]
let ``when adding amounts with divisible divisors the sum uses the larger divisor (1)``() =
    test <@ amount2 100 100 |> addAmount (amount2 50 200) = amount2 250 200 @>
    
[<Fact>]
let ``when adding amounts with divisible divisors the sum uses the larger divisor (2)``() =
    test <@ amount2 50 200 |> addAmount (amount2 100 100) = amount2 250 200 @>
    
[<Fact>]
let ``when adding amounts with indivisible divisors the sum uses the multiplier divisor``() =
    test <@ amount2 2 3 |> addAmount (amount2 2 5) = amount2 (2*5 + 2*3) 15 @>
    