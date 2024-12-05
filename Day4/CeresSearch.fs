module Day4

open System.Text.RegularExpressions
open System.IO
open System

let (testData, realData) =
    let readFile filename =
        Path.Combine(__SOURCE_DIRECTORY__, filename) |> System.IO.File.ReadAllLines

    (readFile "TestInput.txt", readFile "RealInput.txt")

// original solution that looked so elegant in my head but is actually insane
// turns out diagonal lines are not so straight forward...
// by far the slowest solution in terms of performance...
let getAllLines (data: string array) =

    let length, height = String.length (data[0]), Array.length data
    let maxL, maxH = length - 1, height - 1

    let verticalLines =
        [ for x in [ 0..maxL ] -> [| for y in [ 0..maxH ] -> data[y][x] |] |> String ]

    let backLeaninDiagonalLines =
        let getBackLeaningLine (startX, startY) =
            let length = min (maxL - startX) (maxH - startY)
            let endX, endY = startX + length, startY + length
            let coords = List.zip [ startX..endX ] [ startY..endY ]

            [| for x, y in coords -> data[y][x] |] |> String

        // up and left edges
        let startingPoints =
            [ for x in 0..maxL -> (x, 0) ] @ [ for y in 1 .. (maxH - 1) -> (0, y) ]

        startingPoints |> List.map getBackLeaningLine

    let forwardLeaninDiagonalLines =
        let getForwardLeaningLine (startX, startY) =
            let length = min (maxL - startX) startY
            let endX, endY = startX + length, startY - length
            let coords = List.zip [ startX..endX ] (List.rev [ endY..startY ])
            // Note: [startY..endY] returns an empty list when start > end, unfortunately

            [| for x, y in coords -> data[y][x] |] |> String

        // down and left edges
        let startingPoints =
            [ for x in 0..maxL -> (x, maxH) ] @ [ for y in 1 .. (maxH - 1) -> (0, y) ]

        startingPoints |> List.map getForwardLeaningLine

    List.ofArray data
    @ [ " " ] // for formatting
    @ verticalLines
    @ [ "  " ]
    @ backLeaninDiagonalLines
    @ [ "  " ]
    @ forwardLeaninDiagonalLines

let countXmasString data =
    let lines = getAllLines data

    let regex1 = new Regex(@"SAMX", RegexOptions.Compiled)
    let regex2 = new Regex(@"XMAS", RegexOptions.Compiled)

    let count (line: string) =
        regex1.Count line |> (+) <| regex2.Count line

    //for s in lines do
    //    printfn $"{s} {count s}"

    lines |> List.map count |> List.sum

let countXmasForm (data: string array) =
    let length, height = String.length (data[0]), Array.length data
    let maxL, maxH = length - 1, height - 1


    let isXmasForm (x, y) =
        let forwardDiagIsMas () =
            let diag = data[y + 1][x - 1], data[y - 1][x + 1]
            diag = ('M', 'S') || diag = ('S', 'M')

        let backwardDiagIsMas () =
            let diag = data[y - 1][x - 1], data[y + 1][x + 1]
            diag = ('M', 'S') || diag = ('S', 'M')

        data[y][x] = 'A' && backwardDiagIsMas () && forwardDiagIsMas ()

    List.allPairs [ 1 .. (maxL - 1) ] [ 1 .. (maxH - 1) ]
    |> List.filter isXmasForm
    |> List.length

// straightforward implementation... take all the Xs and look around them
// (Which actually turns out to be twice as slow as the complicated one above)
let countXmasString2 (data: string array) =
    let length, height = String.length (data[0]), Array.length data
    let maxL, maxH = length - 1, height - 1

    let isXmas (sx, sy) (ex, ey) =
        let len = max (abs (ex - sx)) (abs (ey - sy))
        let xIncr = (ex - sx) / len
        let yIncr = (ey - sy) / len

        [| for i in [ 1..len ] do
               "XMAS"[i] = data[(sy + (yIncr * i))][(sx + (xIncr * i))] |]
        |> Array.reduce (&&)

    let countXmas (x, y) =
        let isWithinBounds (x, y) =
            x >= 0 && x <= maxL && y >= 0 && y <= maxH

        [ (x, y + 3) // down
          (x, y - 3) // up
          (x + 3, y) // right
          (x - 3, y) // left
          (x + 3, y + 3) // down right diagonal
          (x - 3, y + 3) // down left diagonal
          (x - 3, y - 3) // up left diagonal
          (x + 3, y - 3) ] // up right diagonal
        |> List.filter isWithinBounds
        |> List.filter (isXmas (x, y))
        |> List.length

    List.allPairs [ 0..maxL ] [ 0..maxH ]
    |> List.filter (fun (x, y) -> data[y][x] = 'X')
    |> List.map countXmas
    |> List.sum

let countXmasStringTest = countXmasString testData
let countXmasStringReal () = countXmasString2 realData

let countXmasFormTest = countXmasForm testData
let countXmasFormReal () = countXmasForm realData
