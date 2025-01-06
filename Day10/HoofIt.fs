module Day10

open System.Text.RegularExpressions
open System.IO
open System
open Utils

let data =
    let readFile filename =
        Path.Combine(__SOURCE_DIRECTORY__, filename) |> System.IO.File.ReadAllLines

    readFile <| [ "TestInput.txt"; "RealInput.txt" ][0]

let map = Utils.asIntArray2d data |> Utils.printArray2D "%d"


let rec getReachableTopsFrom (i, j) =
    if map[i, j] = 9 then
        [ i, j ]
    else
        [ i + 1, j; i, j + 1; i - 1, j; i, j - 1 ]
        |> List.filter (Utils.isWithinBounds map)
        |> List.filter (fun (k, l) -> map[k, l] - 1 = map[i, j])
        |> List.map getReachableTopsFrom
        |> List.collect id

let rec getScore (i, j) =
    if map[i, j] = 9 then
        1
    else
        [ i + 1, j; i, j + 1; i - 1, j; i, j - 1 ]
        |> List.filter (Utils.isWithinBounds map)
        |> List.filter (fun (k, l) -> map[k, l] - 1 = map[i, j])
        |> List.map getScore
        |> List.sum

let getTrailheadScores () =
    [ for i in [ 0 .. map.MaxI ] do
          for j in [ 0 .. map.MaxJ ] -> (i, j) ]
    |> List.filter (fun coords -> map[coords] = 0)
    |> List.map (getReachableTopsFrom >> List.distinct >> List.length)
    |> List.sum


let getTrailheadRatings () =
    [ for i in [ 0 .. map.MaxI ] do
          for j in [ 0 .. map.MaxJ ] -> (i, j) ]
    |> List.filter (fun coords -> map[coords] = 0)
    |> List.map getScore
    |> List.sum
