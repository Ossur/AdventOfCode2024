// For more information see https://aka.ms/fsharp-console-apps
module Program

open System.Text.RegularExpressions
open System

open Day1

[<EntryPoint>]
let main _ =
    printfn $"Merry christmas! Press a number to run a day in the advent calendar!"
    let inputRegex = new Regex(".*(\d+).*", RegexOptions.Compiled)

    let rec repl () =
        let matching = Console.ReadLine() |> inputRegex.Match

        if not matching.Success then
            printfn "Please input a number!"
        else
            match matching.Groups[1].Value |> int with
            | 1 ->
                let part1, part2 = Day1.realdataDistanceGet (), Day1.realDataSimilarityGet ()
                printfn $"The total distance between the lists was {part1}, but the similarity was {part2}"
            | _ -> printfn "I haven't solved this day yet"

        repl()

    repl()
    0
