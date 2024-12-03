module Day2


open System.Text.RegularExpressions
open System.IO

let testData =
    System.IO.File.ReadAllLines
    <| Path.Combine(__SOURCE_DIRECTORY__, "TestInput.txt")

let realData =
    System.IO.File.ReadAllLines
    <| Path.Combine(__SOURCE_DIRECTORY__, "RealInput.txt")

let parseRegex = new Regex(@"\d+", RegexOptions.Compiled)

let parseLine line =
    seq {
        let ms = parseRegex.Matches(line)

        for m in ms do
            yield m
    }
    |> Seq.map (_.Value >> int)
    |> Array.ofSeq

let parsedTestData = testData |> Seq.map parseLine

let parsedRealData = realData |> Seq.map parseLine

let rec isSafe (report: int array) =
    match report with
    | [||]
    | [| _ |] -> true
    | [| a; b |] -> a <> b && abs (a - b) <= 3
    | [| a; b; c |] -> (b > min a c) && (b < max a c) && isSafe [| a; b |] && isSafe [| b; c |]
    | _ -> report |> Array.windowed 3 |> Array.forall isSafe

let safeCountInTestData = parsedTestData |> Seq.filter isSafe |> Seq.length

let safeCountInRealDataGet () =
    parsedRealData |> Seq.filter isSafe |> Seq.length

let isSafeWithDampener report =
    let dampenedVersions =
        seq {
            for i in [ 0 .. ((Array.length report) - 1) ] do
                yield Array.removeAt i report
        }

    isSafe report || (dampenedVersions |> Seq.filter isSafe |> Seq.isEmpty |> not)

let safeWithDampenerCountInTestData =
    parsedTestData |> Seq.filter isSafeWithDampener |> Seq.length

let safeWithDampenerCountInRealData () =
    parsedRealData |> Seq.filter isSafeWithDampener |> Seq.length

//printfn $"{safeCountInTestData}"
//printfn $"{safeWithDampenerCountInTestData}"
