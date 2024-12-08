module Day8

open System.IO
open System
open Utils
open System.Collections.Generic

let data =
    let readFile filename =
        Path.Combine(__SOURCE_DIRECTORY__, filename) |> System.IO.File.ReadAllLines

    readFile <| [ "TestInput.txt"; "RealInput.txt" ][1]


let map = Utils.asArray2D data


let nodesAndTheirType =
    [ for i in [ 0 .. map.MaxI ] do
          for j in [ 0 .. map.MaxJ ] do
              if map[i, j] <> '.' then
                  yield map[i, j], (i, j) ]

let nodesByAntennaType =
    nodesAndTheirType
    |> List.groupBy fst
    |> List.map (fun (k, kvs) -> (k, List.map snd kvs))
    |> Map.ofList

let getAntinodesOfPair ((ai, aj), (bi, bj)) =
    let aToBiLen, aToBjLen = bi - ai, bj - aj
    let bToAiLen, bToAjLen = ai - bi, aj - bj
    [ (bi + aToBiLen, bj + aToBjLen); (ai + bToAiLen, aj + bToAjLen) ]

let getAntinodesOfPair2 ((ai, aj), (bi, bj)) =
    let aToBVector_i, aToBVector_j = bi - ai, bj - aj
    let bToAVector_i, bToAVector_j = ai - bi, aj - bj

    let maxNumOfIterFromB =
        min (map.Height / (abs aToBVector_i)) (map.Width / (abs aToBVector_j))

    let maxNumOfIterFromA =
        min (map.Height / (abs bToAVector_i)) (map.Width / (abs bToAVector_j))

    [ for m in [ 1..maxNumOfIterFromA ] -> ai + m * aToBVector_i, aj + m * aToBVector_j ]
    @ [ for m in [ 1..maxNumOfIterFromB ] -> bi + m * bToAVector_i, bj + m * bToAVector_j ]

let getAntinodesOfAllPairs antinodesOfPairFunction =
    getAllPossiblePairings
    >> List.map antinodesOfPairFunction
    >> List.reduce (@)
    >> List.filter (isWithinBounds map)

let getAntinodes antinodesOfPairFunction =

    nodesByAntennaType
    |> Map.values
    |> List.ofSeq
    |> List.map (getAntinodesOfAllPairs antinodesOfPairFunction)
    |> List.reduce (@)
    |> List.distinct
(*|> printlist*)




let antinodeQuanity () =
    getAntinodes getAntinodesOfPair |> List.length

let antinodeQuanity2 () =
    getAntinodes getAntinodesOfPair2 |> List.length
