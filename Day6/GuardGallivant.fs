module Day6

open System.Text.RegularExpressions
open System.IO
open Utils

let data =
    let readFile filename =
        Path.Combine(__SOURCE_DIRECTORY__, filename) |> System.IO.File.ReadAllLines

    readFile <| [ "TestInput.txt"; "RealInput.txt" ][0]

let parsingRegex = new Regex("\d+")

let quantityOfVisits () =
    let map = data |> to2dCharArray
    map
