module Day18

open System.Text.RegularExpressions
open System.IO
open System
open Utils

let data =
    let readFile filename =
        Path.Combine(__SOURCE_DIRECTORY__, filename) |> System.IO.File.ReadAllLines

    readFile <| [ "TestInput.txt"; "RealInput.txt" ][0]

let parsingRegex = new Regex("\d+")

// hugmynd: búa til tré með exit sem root node, síðan búa til grid af trjánóðunum og eyða þeim út,
// nota svo bara bfs til að finna stystu leið

let grid = Array2D.create 70 70 '.'



let quantityOfVisits = "[Not solved]"

let minimumNumberOfStepsAfterKilobyte () = "not solved"
