module Day3


open System.Text.RegularExpressions
open System.IO

let ( testData, realData ) = 
    let readFile filename = 
        Path.Combine(__SOURCE_DIRECTORY__, filename) |> System.IO.File.ReadAllLines 
    ( readFile "TestInput.txt" , readFile "RealInput.txt" ) 


let parse (data: string array) : (int64 * int64) seq =
    let parseRegex = new Regex(@"mul\((\d+),(\d+)\)", RegexOptions.Compiled)

    let parseLine (line: string) : (int64 * int64) seq =
        seq {
            for m in parseRegex.Matches(line) do
                yield int64 m.Groups[1].Value, int64 m.Groups[2].Value
        }

    data |> Seq.ofArray |> Seq.map parseLine |> Seq.concat


let getMultiplicationSum = Seq.fold (fun a (n1, n2) -> a + (n1 * n2)) 0L

let testDataMultiplicationSum = parse testData |> getMultiplicationSum

let realDataMultiplicationSumGet () = parse realData |> getMultiplicationSum

let parseAlsoDo (data: string array) : (int64 * int64) seq =
    let parseRegex =
        new Regex(@"(mul\((\d+),(\d+)\)|don't\(\).*?($|do\(\)))", RegexOptions.Singleline)

    let parseAll (line: string) : (int64 * int64) seq =
        parseRegex.Matches(line)
        |> Seq.filter (_.Groups[1].Value[0..2] >> ((=) "mul"))
        |> Seq.map (fun m -> int64 m.Groups[2].Value, int64 m.Groups[3].Value)


    data |> Array.reduce (+) |> parseAll


let testDataAlsoDoSum = parseAlsoDo testData |> getMultiplicationSum

let realDataAlsoDoSum () =
    parseAlsoDo realData |> getMultiplicationSum

