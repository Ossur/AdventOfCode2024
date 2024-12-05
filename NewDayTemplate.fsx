open System.Text.RegularExpressions
open System.IO

let data =
    let readFile filename =
        Path.Combine(__SOURCE_DIRECTORY__, filename) |> System.IO.File.ReadAllLines

    readFile <| [ "TestInput.txt"; "RealInput.txt" ][1]

let parsingRegex = new Regex("\d+")
