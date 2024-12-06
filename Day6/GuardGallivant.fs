module Day6

open System.Text.RegularExpressions
open System.IO
open Utils

let data =
    let readFile filename =
        Path.Combine(__SOURCE_DIRECTORY__, filename) |> System.IO.File.ReadAllLines

    readFile <| [ "TestInput.txt"; "RealInput.txt" ][0]

let print = false

let parsingRegex = new Regex("\d+")

type Direction =
    | Up
    | Down
    | Left
    | Right

type Guard =
    { Coords: int * int
      Turning: Direction }

let getDirChar d =
    match d with
    | Up -> '^'
    | Down -> 'v'
    | Left -> '<'
    | Right -> '>'

let isGuardCharacter c =
    Array.contains c [| '^'; '>'; 'v'; '<' |]


let toGuard (c, coords) =
    match c with
    | '^' -> { Coords = coords; Turning = Up }
    | '>' -> { Coords = coords; Turning = Right }
    | 'v' -> { Coords = coords; Turning = Down }
    | '<' -> { Coords = coords; Turning = Left }
    | _ -> failwith $"{c} is not a guard character!"


let getGuard map =
    map.Array |> array2dFilterIj isGuardCharacter |> List.exactlyOne |> toGuard

let turnRight dir =
    match dir with
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

let turnGuard g =
    let { Turning = dir; Coords = c } = g

    { Turning = (turnRight dir)
      Coords = c }


let getCoordsInFront { Turning = dir; Coords = (i, j) } =
    match dir with
    | Up -> i - 1, j
    | Right -> i, j + 1
    | Down -> i + 1, j
    | Left -> i, j - 1

let getCoordsRightBehind { Turning = dir; Coords = (i, j) } =
    match dir with
    | Up -> i + 1, j + 1
    | Right -> i - 1, j + 1
    | Down -> i - 1, j - 1
    | Left -> i - 1, j + 1

let getCoordBehind { Turning = dir; Coords = (i, j) } =
    match dir with
    | Up -> i + 1, j
    | Right -> i, j - 1
    | Down -> i - 1, j
    | Left -> i, j + 1


let moveGuard g map pathmark =
    let currentCoords & (ci, cj) = g.Coords
    let nextCoords & (ni, nj) = getCoordsInFront g

    if (isWithinBounds map nextCoords) && map.Array[ni, nj] = '#' then
        turnGuard g
    else
        map.Array[ci, cj] <- pathmark g map

        { Turning = g.Turning
          Coords = nextCoords }


let rec moveUntilOutOfMap g map pathmark =

    let guardAfterNextStep = moveGuard g map pathmark

    if isWithinBounds map guardAfterNextStep.Coords |> not then
        ()
    else
        moveUntilOutOfMap guardAfterNextStep map pathmark

let quantityOfVisits () =

    let map = asArray2D data

    if print then
        printArray2D map
        printfn "¬¬¬¬¬¬¬¬¬¬¬¬"

    let guard = getGuard map

    moveUntilOutOfMap guard map (fun _ _ -> 'X')

    if print then
        printArray2D map

    map.Array |> array2dFilterIj ((=) 'X') |> List.length

let obstructionPossibilities () =

    let map = asArray2D data

    if print then
        printArray2D map
        printfn "¬¬¬¬¬¬¬¬¬¬¬¬"

    let guard = getGuard map

    let chooseMark g map =

        let ci, cj = g.Coords
        let ri, rj = getCoordsRightBehind g
        let bi, bj = getCoordBehind g
        let markToTheRightBehind = map.Array[ri, rj]
        let currentMark = map.Array[ci, cj]
        let markBehind = map.Array[bi, bj]

        if
            (markBehind = (getDirChar g.Turning)
             && markToTheRightBehind = (g.Turning |> turnRight |> getDirChar))
        then
            'O'
        else
            getDirChar g.Turning

    moveUntilOutOfMap guard map chooseMark

    map.Array |> array2dFilterIj ((=) 'O') |> List.length
