module Day3


open System.Text.RegularExpressions
open System.IO

let testData =
    System.IO.File.ReadAllLines
    <| Path.Combine(__SOURCE_DIRECTORY__, "TestInput.txt")

let realData =
    System.IO.File.ReadAllLines
    <| Path.Combine(__SOURCE_DIRECTORY__, "RealInput.txt")


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
printfn $"{testDataMultiplicationSum}"

let realDataMultiplicationSumGet () = parse realData |> getMultiplicationSum
// printfn $"{realDataMultiplicationSum ()}"

let parseAlsoDo (data: string array) : (int64 * int64) seq =
    let parseRegex =
        new Regex(@"(mul\((\d+),(\d+)\)|don't\(\).*?($|do\(\)))", RegexOptions.Singleline)

    let parseAll (line: string) : (int64 * int64) seq =
        seq {
            let matches =
                parseRegex.Matches(line) |> Seq.filter (_.Groups[1].Value[0..2] >> ((=) "mul"))

            for m in matches do
                yield int64 m.Groups[2].Value, int64 m.Groups[3].Value

        }

    data |> Array.reduce (+) |> parseAll



let testDataAlsoDoSum = parseAlsoDo testData |> getMultiplicationSum
printfn $"{testDataAlsoDoSum}"

let realDataAlsoDoSum () =
    parseAlsoDo realData |> getMultiplicationSum

printfn $"{realDataAlsoDoSum ()}"
