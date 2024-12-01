module Day1

open System.Text.RegularExpressions
open System.IO

let testData =
    System.IO.File.ReadAllLines
    <| Path.Combine(__SOURCE_DIRECTORY__, "TestInput.txt")

let realData =
    System.IO.File.ReadAllLines
    <| Path.Combine(__SOURCE_DIRECTORY__, "RealInput.txt")

let parseRegex = new Regex("^(\d+) +(\d+)$", RegexOptions.Compiled)

let parseLine line =
    let parsing = parseRegex.Match(line)

    if parsing.Success then
        Some(parsing.Groups[1].Value |> int64, parsing.Groups[2].Value |> int64)
    else
        None

let parseLines = Seq.map parseLine >> Seq.choose id >> List.ofSeq >> List.unzip

let parsedTestData = testData |> parseLines
let parsedRealData = realData |> parseLines


let getDistance (x, y) = x - y |> abs

let getTotalDistance list1 list2 =
    List.zip (List.sort list1) (List.sort list2) |> List.map getDistance |> Seq.sum

let testdataDistance = parsedTestData ||> getTotalDistance

let realdataDistanceGet () = parsedRealData ||> getTotalDistance

let getSimilarity list1 list2 =
    let freqMap = List.countBy id list2

    let getSim x =
        freqMap
        |> List.tryFind (fst >> ((=) x))
        |> Option.defaultValue (0L, 0)
        |> snd
        |> int64

    list1 |> List.map (fun x -> x * getSim x) |> List.sum

let testDataSimilarity = parsedTestData ||> getSimilarity
let realDataSimilarityGet () = parsedRealData ||> getSimilarity
