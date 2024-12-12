module Day11

open Utils
open System
open System.Collections.Generic

let data = [ [ 125; 17 ]; [ 1950139; 0; 3; 837; 6116; 18472; 228700; 45 ] ][1]

let rec stonesAfterBlinking (stones: LinkedList<uint64>) blinks =
    let blink (stoneNode: LinkedListNode<uint64>) =
        let numOfDigits =
            if stoneNode.Value = 1uL then
                1.
            else
                stoneNode.Value |> float |> log10 |> ceil

        match stoneNode.Value = 0uL, numOfDigits % 2.0 with
        | true, _ -> stoneNode.Value <- 1uL
        | _, 0.0 ->
            let divisor = 10.0 ** (numOfDigits / 2.0) |> uint64
            let struct (a, b) = UInt64.DivRem(stoneNode.Value, divisor)
            stoneNode.List.AddBefore(stoneNode, a) |> ignore
            stoneNode.Value <- b
        | _, _ -> stoneNode.Value <- 2024uL * stoneNode.Value

    match blinks with
    | 0 -> stones
    | _ ->
        printfn $"blink {75 - blinks}"
        (asNodeSequence stones |> Seq.iter blink) |> ignore
        stonesAfterBlinking stones (blinks - 1)

let resultCache: Dictionary<(int * bigint), bigint> =
    new Dictionary<(int * bigint), bigint>()

let rec countStonesAfterBlinks blinks stone =

    if blinks = 0 then
        1I
    else
        match resultCache.TryGetValue((blinks, stone)) with
        | true, cachedResult -> cachedResult
        | false, _ ->
            let numOfDigits = stone |> string |> String.length

            let count =
                match stone = 0I, numOfDigits % 2 with
                | true, _ -> countStonesAfterBlinks (blinks - 1) 1I
                | _, 0 ->
                    let divisor = 10I ** (numOfDigits / 2)
                    let struct (a, b) = bigint.DivRem(stone, divisor)

                    (countStonesAfterBlinks (blinks - 1) a)
                    + (countStonesAfterBlinks (blinks - 1) b)

                | _, _ -> countStonesAfterBlinks (blinks - 1) (2024I * stone)

            resultCache.TryAdd((blinks, stone), count) |> ignore
            count




let quantityOfStonesOld () =
    let li = data |> List.map uint64 |> (fun x -> new LinkedList<uint64>(x))
    stonesAfterBlinking li 25 |> _.Count

let quantityOfStones () =
    data |> List.map bigint |> List.map (countStonesAfterBlinks 75) |> List.sum
