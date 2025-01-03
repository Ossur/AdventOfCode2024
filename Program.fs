// For more information see https://aka.ms/fsharp-console-apps
module Program

open System.Diagnostics
open System.Text.RegularExpressions
open System

[<EntryPoint>]
let main args =

    let inputRegex = new Regex(".*?(\d+).*?", RegexOptions.Compiled)

    // run a function with some very naive/basic performance metrics
    let run f =
        let getMemory () =
            let proc = Process.GetCurrentProcess()
            proc.WorkingSet64 / 1024L / 1024L

        System.GC.Collect()
        let memBefore = getMemory ()
        let sw = Stopwatch.StartNew()
        let result = f ()
        sw.Stop()
        let memAfter = getMemory ()
        printfn "Time: %f ms, Memory: %d MB" sw.Elapsed.TotalMilliseconds (memAfter - memBefore)
        result

    let printSolution arg =

        let matching = inputRegex.Match arg

        if not matching.Success then
            printfn "Please input a number!"
        else
            let dayNum = matching.Groups[1].Value |> int
            printfn $"Running day {dayNum}"

            match dayNum with
            | 1 ->
                printfn $"The total distance between the lists was {run Day1.realdataDistanceGet},"
                printfn $" but the similarity was {run Day1.realDataSimilarityGet}"

            | 2 ->
                printfn $"The total number of safe reports are {run Day2.safeCountInRealDataGet},"
                printfn $" but with the problem dampener they are {run Day2.safeWithDampenerCountInRealData}"

            | 3 ->
                printfn $"The sum of the multiplication results are {run Day3.realDataMultiplicationSumGet}, "
                printfn $" but with taking enable instructions into account it is only {run Day3.realDataAlsoDoSum}"

            | 4 ->
                printfn $"In the puzzle, 'XMAS' appears {run Day4.countXmasStringReal} times"
                printfn $" but the real XMAS only {run Day4.countXmasFormReal} times"

            | 5 ->
                printfn
                    $"The sum of the middle pages from the correctly ordered sets is {run Day5.sumOfCorrectMiddlePages}"

                printfn
                    $" but the sum of the middlepages from the corrected sets is {run Day5.sumOfIncorrectOrderedMiddlePages}"
            | 6 ->
                printfn $"The guard visited {run Day6.quantityOfVisits} places"

                printfn
                    $" but there are obstacle possiblilities at {run Day6.obstructionPossibilities} places on the map"

            | 7 -> printfn $"{run Day7.totalCalibrationResult2}"

            | 8 ->
                printfn $"The total number of anitondes is {run Day8.antinodeQuanity}"
                printfn $"  but the actual number of antidoes is {run Day8.antinodeQuanity2}"

            | 9 ->
                printfn $"The checksum for the rearranged disk is {run Day9.getChecksumOfRearranged}"

                printfn
                    $"  but the checksum for the unfragmented is is {run Day9.getChecksumOfRearrangedWithoutFragmentation}"

            | 11 -> printfn $"Quantity of stones after 25 blinks are {run Day11.quantityOfStones}"

            | 12 ->
                printfn $"The total price for fences is {run Day12.getTotalFencePrice}"
                printfn $"but with discount it is {run Day12.getDiscountedFencePrice}"
            | 18 -> 
                printfn $"The minimum number of steps to exit the memory after first 1024 bytes of curruption is
                {run Day18.minimumNumberOfStepsAfterKilobyte}"

            | _ -> printfn "¬ I haven't solved this day yet"


    let rec repl () =
        Console.ReadLine() |> printSolution
        repl ()

    match Array.length args with
    | 0 ->
        printfn $"Merry christmas! Press a number to run a day in the advent calendar!"
        repl ()
    | _ -> printSolution args[0]

    0
