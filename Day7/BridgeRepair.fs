module Day7

open System.Text.RegularExpressions
open System.IO
open System
open Utils

let data =
    let readFile filename =
        Path.Combine(__SOURCE_DIRECTORY__, filename) |> System.IO.File.ReadAllLines

    readFile <| [ "TestInput.txt"; "RealInput.txt" ][0]

let parsingRegex = new Regex("\d+")


type Calibration =
    { Testcase: uint64
      Operands: uint64 list }



let parseLine l =
    let parsedValues =
        parsingRegex.Matches l |> List.ofSeq |> List.map (_.Value >> uint64)

    if List.length parsedValues = 0 then
        failwith "Unexpected empty line"

    { Testcase = parsedValues[0]
      Operands = parsedValues |> List.tail }



// because the operands are ascending, the smallest result will be for sums, the next smallest for  *,+,...,+
let hasSolution calibration =
    let operandQty = List.length calibration.Operands
    let operands = calibration.Operands |> List.sort


    let calculate operands operators =
        let rec h operands operators acc =
            match operands, operators with
            | a :: td, o :: tt -> h td tt (o acc a)
            | [], [] -> acc
            | _, _ -> failwith "Operator and operand list not of correct length"

        h (List.tail operands) operators operands[0]

    // creates operator possibilites that generate values in a ascending order
    // Ordering first by number of multiplication and then by how rightward the multiplication is
    // And because being more rightward means being used on larger numbers (because the operands are also sorted
    // in ascending order) it means yielding a larger value
    let getOperatorPossibilites n = getCombinations false n [ (+); (*) ]

    getOperatorPossibilites (operandQty - 1)
    |> List.map (calculate operands)
    |> List.filter ((=) calibration.Testcase)
    |> List.length
    |> (fun l -> l > 0)


let totalCalibrationResult () =
    data
    |> Array.map parseLine
    |> Array.filter hasSolution
    |> Array.map _.Testcase
    |> Array.sum
