// For more information see https://aka.ms/fsharp-console-apps
module Program

open System.Diagnostics
open System.Text.RegularExpressions
open System

[<EntryPoint>]
let main args =

    let inputRegex = new Regex(".*(\d+).*", RegexOptions.Compiled)

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

            | 7 -> printfn $"{run Day7.totalCalibrationResult}"

            | 8 -> printfn $"The total number of anitondes is {run Day8.antinodeQuanity}"
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
