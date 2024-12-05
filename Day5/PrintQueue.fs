module Day5

open System.Text.RegularExpressions

open System.IO

let (testData, realData) =
    let readFile filename =
        Path.Combine(__SOURCE_DIRECTORY__, filename) |> System.IO.File.ReadAllLines

    (readFile "TestInput.txt", readFile "RealInput.txt")

type RuleOrPages =
    | Pages of {| pages: int list; middle: int |}
    | Rule of {| before: int; after: int |}


let parseInput dataLines =

    let digitRegex = new Regex("\d+", RegexOptions.Compiled)

    let listToRuleOrPage l : RuleOrPages =
        let len = List.length l

        if ((len % 2) = 0) then
            Rule {| before = l[0]; after = l[1] |}
        else
            Pages {| pages = l; middle = l[len / 2] |}

    dataLines
    |> Array.filter (String.length >> (=) 0 >> not)
    |> Array.map (fun x -> [ for m in (digitRegex.Matches(x)) -> m.Value |> int ])
    |> Array.map listToRuleOrPage




let sumOfCorrectMiddlePages () =
    for i in parseInput testData do
        printfn $"{i}"
