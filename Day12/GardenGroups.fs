module Day12

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

type PlotSet = HashSet<int * int>

let addPlotToRegions regions plot =
    let isNeighbouringRegion plot (region: PlotSet) =
        let pi, pj = plot

        [ (pi + 1, pj); (pi, pj + 1); (pi - 1, pj); (pi, pj - 1) ]
        |> List.map region.Contains
        |> List.reduce (||)

    let neighbouringRegions = List.filter (isNeighbouringRegion plot) regions


    match neighbouringRegions with
    | [] -> (PlotSet([ plot ])) :: regions
    | [ r ] ->
        r.Add(plot) |> ignore
        regions
    | r :: rs ->
        r.Add(plot) |> ignore
        rs |> List.iter r.UnionWith |> ignore
        List.except rs regions


let parseIntoContinousRegions data =

    let regionsByPlant = new Dictionary<char, PlotSet list>()
    let map = asArray2D data

    for i in [ 0 .. map.MaxI ] do
        for j in [ 0 .. map.MaxJ ] do
            let plant = map[i, j]

            match regionsByPlant.TryGetValue(plant) with
            | true, regions -> regionsByPlant[plant] <- addPlotToRegions regions (i, j)
            | false, _ -> regionsByPlant[plant] <- [ PlotSet([ i, j ]) ]

    regionsByPlant



let getQtyOfAdjacentDifferentPlots (region: PlotSet) plot =
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

let continousRegions = data |> parseIntoContinousRegions

let getTotalFencePrice () =
    continousRegions
    |> _.Values
    |> Seq.collect id // flatten into a list of regions
    |> Seq.map calculatePrice
    |> Seq.sum


let getQtyOfGaplessHorizontalLines plots =
    let isNextInStraightLine (ai, aj) (bi, bj) = ai = bi && (aj + 1) = bj

    let rec h plots cnt =
        match plots with
        | a :: b :: _ -> h (List.tail plots) (if isNextInStraightLine a b then cnt else cnt + 1)
        | [ _ ] -> cnt + 1
        | [] -> cnt

    h (plots |> List.ofSeq |> List.sort) 0

let getQtyOfGaplessVerticalLines plots =
    plots |> Seq.map (fun (i, j) -> j, i) |> getQtyOfGaplessHorizontalLines


type PlotPairSet = HashSet<(int * int) * (int * int)>

let calculateDiscountPrice (region: PlotSet) =
    let area = Seq.length region

    let areasWithWestFence =
        region |> Seq.filter (fun (pi, pj) -> region.Contains((pi, pj - 1)) |> not)

    let areasWithEastFence =
        region |> Seq.filter (fun (pi, pj) -> region.Contains((pi, pj + 1)) |> not)

    let areasWithNorthFence =
        region |> Seq.filter (fun (pi, pj) -> region.Contains((pi - 1, pj)) |> not)

    let areasWithSouthFence =
        region |> Seq.filter (fun (pi, pj) -> region.Contains((pi + 1, pj)) |> not)

    let verticalSidesCnt =
        [ areasWithEastFence; areasWithWestFence ]
        |> List.map getQtyOfGaplessVerticalLines
        |> List.sum

    let horizontalSidesCnt =
        [ areasWithNorthFence; areasWithSouthFence ]
        |> List.map getQtyOfGaplessHorizontalLines
        |> List.sum


    let sides = verticalSidesCnt + horizontalSidesCnt
    area * sides

let getDiscountedFencePrice () =
    continousRegions
    |> _.Values
    |> Seq.collect id // flatten into a list of regions
    |> Seq.map calculateDiscountPrice
    |> Seq.sum
