module Day12

open System.Text.RegularExpressions
open System.IO
open System.Collections.Generic
open Utils

let data =
    let readFile filename =
        Path.Combine(__SOURCE_DIRECTORY__, filename) |> System.IO.File.ReadAllLines

    readFile <| [ "TestInput.txt"; "RealInput.txt" ][1]

type Regions =
    { Plant: char
      Areas: HashSet<int * int> list }

let a = new HashSet<int>([ 1; 2 ])

let addPlotToRegions regions plot =
    let isNeighbouringRegion plot (region: HashSet<int * int>) =
        let pi, pj = plot

        [ (pi + 1, pj); (pi, pj + 1); (pi - 1, pj); (pi, pj - 1) ]
        |> List.map region.Contains
        |> List.reduce (||)

    let neighbouringRegions = List.filter (isNeighbouringRegion plot) regions


    match neighbouringRegions with
    | [] -> (new HashSet<int * int>([ plot ])) :: regions
    | [ r ] ->
        r.Add(plot) |> ignore
        regions
    | r :: rs ->
        r.Add(plot) |> ignore
        rs |> List.iter r.UnionWith |> ignore
        List.except rs regions


let parseIntoContinousRegions data =

    let regionsByPlant = new Dictionary<char, HashSet<int * int> list>()
    let map = asArray2D data

    for i in [ 0 .. map.MaxI ] do
        for j in [ 0 .. map.MaxJ ] do
            let plant = map[i, j]

            match regionsByPlant.TryGetValue(plant) with
            | true, regions -> regionsByPlant[plant] <- addPlotToRegions regions (i, j)
            | false, _ -> regionsByPlant[plant] <- [ new HashSet<int * int>([ i, j ]) ]

    regionsByPlant

let getQtyOfAdjacentDifferentPlots (region: HashSet<int * int>) plot =
    let pi, pj = plot

    [ (pi + 1, pj); (pi, pj + 1); (pi - 1, pj); (pi, pj - 1) ]
    |> List.filter (region.Contains >> not)
    |> List.length


let getTotalNumberOfFences region =
    region |> Seq.map (getQtyOfAdjacentDifferentPlots region) |> Seq.sum

let calculatePrice region =
    let area = Seq.length region
    let perimeter = getTotalNumberOfFences region
    area * perimeter

let getTotalFencePrice () =
    data
    |> parseIntoContinousRegions
    |> _.Values
    |> Seq.collect id // flatten into a list of regions
    |> Seq.map calculatePrice
    |> Seq.sum
