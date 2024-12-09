module Day6

open System.Text.RegularExpressions
open System.IO
open Utils
open System.Collections.Generic

let data =
    let readFile filename =
        Path.Combine(__SOURCE_DIRECTORY__, filename) |> System.IO.File.ReadAllLines

    readFile <| [ "TestInput.txt"; "RealInput.txt" ][1]

let printMaps = false
let printProgress = false

let parsingRegex = new Regex("\d+")

type Direction =
    | Up
    | Down
    | Left
    | Right


type Guard =
    { Coords: int * int
      Turning: Direction
      // optimization that turns minutes into seconds, with list solution takes over 10 mins
      Steps: HashSet<(Direction * (int * int))> }



let getDirChar d =
    match d with
    | Up -> '^'
    | Down -> 'v'
    | Left -> '<'
    | Right -> '>'

let getDirection d =
    match d with
    | '^' -> Up
    | 'v' -> Down
    | '<' -> Left
    | '>' -> Right
    | _ -> failwith $"{d} is not a guard character!"

let isGuardCharacter c =
    Array.contains c [| '^'; '>'; 'v'; '<' |]


let toGuard (c, coords) =
    { Steps = new HashSet<(Direction * (int * int))>()
      Turning = getDirection c
      Coords = coords }


let getGuard map =
    map.Array |> array2dFilterIj isGuardCharacter |> List.exactlyOne |> toGuard

let turnRight dir =
    match dir with
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

let turnGuard g =
    g.Steps.Add(g.Turning, g.Coords) |> ignore

    { Turning = turnRight g.Turning
      Steps = g.Steps
      Coords = g.Coords }


let getCoordsInFront { Turning = dir; Coords = (i, j) } =
    match dir with
    | Up -> i - 1, j
    | Right -> i, j + 1
    | Down -> i + 1, j
    | Left -> i, j - 1


let moveGuard g map =
    let nextCoords & (ni, nj) = getCoordsInFront g

    if (isWithinBounds map nextCoords) && map.Array[ni, nj] = '#' then
        turnGuard g
    else
        g.Steps.Add(g.Turning, g.Coords) |> ignore

        { Turning = g.Turning
          Coords = nextCoords
          Steps = g.Steps }


// returns true if moved out of the map
// returns false if in a loop
let rec moveUntilOutOfMap g map =

    let guardAfterNextStep = moveGuard g map

    if isWithinBounds map guardAfterNextStep.Coords |> not then
        true, guardAfterNextStep
    else if g.Steps.Contains((guardAfterNextStep.Turning, guardAfterNextStep.Coords)) then
        false, guardAfterNextStep
    else
        moveUntilOutOfMap guardAfterNextStep map


let quantityOfVisits () =

    let map = asArray2D data

    if printMaps then
        printArray2D map
        printfn "¬¬¬¬¬¬¬¬¬¬¬¬"

    let movedOut, guard = moveUntilOutOfMap (getGuard map) map

    if printMaps then
        printArray2D map

    if not movedOut then
        -1
    else
        guard.Steps
        |> List.ofSeq
        |> List.map (fun (_, c) -> c)
        |> List.distinct
        |> List.length

// DOESNT WORK
// TODO try to run simulation with an obstacle added on each part of the path, count the steps and once the
// steps reach 10000000 just assume it is good enough for that version of the map
let obstructionPossibilities () =

    let map = asArray2D data

    if printMaps then
        printArray2D map
        printfn "¬¬¬¬¬¬¬¬¬¬¬¬"

    let guardStart = getGuard map
    let movedOut, guard = moveUntilOutOfMap guardStart map

    if not movedOut then
        failwith "Something wrong, map doesn't have a path out"

    let possibleObstructions =
        guard.Steps
        |> List.ofSeq
        |> List.map (fun (_, c) -> c)
        |> List.filter ((<>) guardStart.Coords)
        |> List.distinct


    let mutable c = 0

    let doesObstructEndInGuardLoop (ci, cj) =

        map.Array[ci, cj] <- '#'
        let movedOut, _ = moveUntilOutOfMap (getGuard map) map
        map.Array[ci, cj] <- '.'

        if printProgress then
            c <- c + 1
            printfn $"Trying obstruct at {ci},{cj}, {c}th placement to try - does it loop?? {not movedOut}"

        not movedOut

    possibleObstructions |> List.filter doesObstructEndInGuardLoop |> List.length
