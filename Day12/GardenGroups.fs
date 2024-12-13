module Day12

open System.IO
open System.Collections.Generic
open Utils

let data =
    let readFile filename =
        Path.Combine(__SOURCE_DIRECTORY__, filename) |> System.IO.File.ReadAllLines

    readFile <| [ "TestInput.txt"; "RealInput.txt" ][0]

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


let getAllAdjacentAndDiagonallyAdjacentPlots (region: PlotSet) = // including fictional plots outside map
    let getAdjacentPlots (pi, pj) =
        [ (pi + 1, pj)
          (pi, pj + 1)
          (pi - 1, pj)
          (pi, pj - 1)
          (pi + 1, pj + 1)
          (pi - 1, pj + 1)
          (pi + 1, pj - 1)
          (pi - 1, pj - 1) ]

    region
    |> Seq.collect getAdjacentPlots
    |> Seq.filter (region.Contains >> not)
    |> PlotSet

let isWithinRegion (region: PlotSet) plot =
    let pi, pj = plot

    let hasPlotLeftOf () =
        region |> Seq.exists (fun (i, j) -> i = pi && j < pj)

    let hasPlotRightOf () =
        region |> Seq.exists (fun (i, j) -> i = pi && j > pj)

    let hasPlotAbove () =
        region |> Seq.exists (fun (i, j) -> j = pj && i < pi)

    let hasPlotBelow () =
        region |> Seq.exists (fun (i, j) -> j = pj && i > pi)
    // note: defining as functions enables lazy loading below
    hasPlotAbove () && hasPlotBelow () && hasPlotLeftOf () && hasPlotRightOf ()

let getQtyOfStraightLines (plots: PlotSet) =

    let mutable horizCount = 0
    let mutable lastWasHorizontallyAdj = false
    let horizonatllySorted = plots |> Seq.sort

    for ((ai, aj), (bi, bj)) in horizonatllySorted |> Seq.pairwise do
        let isHorizontallyAdj = (ai = bi) && ((aj + 1) = bj)

        match isHorizontallyAdj, lastWasHorizontallyAdj with
        | true, false ->
            horizCount <- 1 + horizCount
            lastWasHorizontallyAdj <- true
        | false, _ -> lastWasHorizontallyAdj <- false
        | _, _ -> ()

    let mutable vertiCount = 0
    let mutable lastWasVerticallyAdj = false
    let verticallySorted = plots |> Seq.sortBy (fun (i, j) -> j, i)

    for ((ai, aj), (bi, bj)) in (verticallySorted |> Seq.pairwise) do
        let isVerticallyAdj = (aj = bj) && ((ai + 1) = bi)

        match isVerticallyAdj, lastWasVerticallyAdj with
        | true, false ->
            vertiCount <- 1 + vertiCount
            lastWasVerticallyAdj <- true
        | false, _ -> lastWasVerticallyAdj <- false
        | _ -> ()

    printfn
        "%A\n%A%A\n%A%A"
        (plots |> List.ofSeq)
        (horizonatllySorted |> List.ofSeq)
        horizCount
        (verticallySorted |> List.ofSeq)
        vertiCount

    horizCount + vertiCount

let calculateDiscountPrice region =
    let area = Seq.length region

    let innerBoundPlots, outerBoundsPlots =
        region
        |> getAllAdjacentAndDiagonallyAdjacentPlots
        |> filter2 (isWithinRegion region)

    let outerSides = outerBoundsPlots |> PlotSet |> getQtyOfStraightLines

    let innerSides =
        innerBoundPlots
        |> PlotSet
        |> getAllAdjacentAndDiagonallyAdjacentPlots
        |> getQtyOfStraightLines

    let sides = outerSides + innerSides
    area * sides

let getDiscountedFencePrice () =
    continousRegions
    |> _.Values
    |> Seq.collect id // flatten into a list of regions
    |> Seq.map calculateDiscountPrice
    |> Seq.sum
