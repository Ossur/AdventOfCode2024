module Day8

open System.IO
open System
open Utils
open System.Collections.Generic

let data =
    let readFile filename =
        Path.Combine(__SOURCE_DIRECTORY__, filename) |> System.IO.File.ReadAllLines

    readFile <| [ "TestInput.txt"; "RealInput.txt" ][1]


let getAntinodes data =
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


    let getAntinodesOfAllPairs =
        getAllPossiblePairings
        >> List.map getAntinodesOfPair
        >> List.reduce (@)
        >> List.filter (isWithinBounds map)

    nodesByAntennaType
    |> Map.values
    |> List.ofSeq
    |> List.map getAntinodesOfAllPairs
    |> List.reduce (@)
    |> List.distinct
    |> printlist




let antinodeQuanity () = data |> getAntinodes |> List.length
