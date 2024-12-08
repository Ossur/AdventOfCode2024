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



let parseLine l =
    let parsedValues = parsingRegex.Matches l |> List.ofSeq |> List.map (_.Value >> int)

    if List.length parsedValues = 0 then
        failwith "Unexpected empty line"

    let a :: b = parsedValues
    {| Testcase = a; Operands = b |}


let totalCalibrationResult () =
    data |> printlist |> Array.map parseLine |> printlist
