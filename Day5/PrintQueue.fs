module Day5

open System.Text.RegularExpressions
open System.IO

let data =
    let readFile filename =
        Path.Combine(__SOURCE_DIRECTORY__, filename) |> System.IO.File.ReadAllLines

    readFile <| [ "TestInput.txt"; "RealInput.txt" ][1]

type Pages = { pages: int array; middle: int }
type Rule = { before: int; after: int }

let parseInput (dataLines: string array) : (Pages list * Rule list) =

    let digitRegex = new Regex("\d+", RegexOptions.Compiled)

    let listToRuleOrPages (accPages: Pages list, accRules: Rule list) (l: int array) =
        let len = Array.length l

        if ((len % 2) = 0) then
            let newRule = { before = l[0]; after = l[1] }
            (accPages, newRule :: accRules)
        else
            let newPages = { pages = l; middle = l[len / 2] }
            (newPages :: accPages, accRules)

    dataLines
    |> List.ofArray
    |> List.filter (String.length >> (=) 0 >> not)
    |> List.map (fun x -> [| for m in (digitRegex.Matches(x)) -> m.Value |> int |])
    |> List.fold listToRuleOrPages ([], [])


let (pages, rules) = parseInput data

let getNumsThatShouldBeAfterN (rules: Rule list) n =
    let numsAfter =
        rules
        |> List.groupBy _.before
        |> List.map (fun (k, rules) -> (k, rules |> List.map (fun rule -> rule.after) |> Array.ofList))
        |> Map.ofList

    Map.tryFind n numsAfter |> Option.defaultValue [||]

let getNumsThatShouldBeAfter = getNumsThatShouldBeAfterN rules

let listHasNoViolation (arr: int array) =
    match Array.length arr with // The head of the list must not be in any of the numbers' numsAfter list
    | 0
    | 1 -> true // empty or single element list cannot violate
    | _ ->
        arr[1..]
        |> Array.forall (getNumsThatShouldBeAfter >> Array.contains arr[0] >> not)

let pageHasNoViolation p =

    let lastIdx = (Array.length p.pages) - 1

    [| for i in [ 0..lastIdx ] -> p.pages[i..lastIdx] |] // the sublists of nth to last page
    |> Array.forall listHasNoViolation

let sumOfCorrectMiddlePages () =

    pages |> List.filter pageHasNoViolation |> List.map _.middle |> List.sum


let sumOfIncorrectOrderedMiddlePages () =

    let swap i j (arr: int array) =
        let t = arr[i]
        arr[i] <- arr[j]
        arr[j] <- t
        arr

    let rec fixIfViolates i j (li: int array) : int array =
        let violates = getNumsThatShouldBeAfter li[j] |> Array.contains li[i]

        if violates then
            //let fixedList = List.removeAt i li |> (List.insertAt j li[i])
            swap i j li |> ignore

        let maxIdx = (Array.length li) - 1

        if j = maxIdx then
            if i = (maxIdx - 1) then // last iteration
                li
            else
                fixIfViolates (i + 1) (i + 2) li
        else
            fixIfViolates i (j + 1) li


    let correctIncorrectPage (p: Pages) =
        let fixedPages = fixIfViolates 0 1 p.pages

        { pages = fixedPages
          middle = fixedPages[(Array.length fixedPages) / 2] }


    pages
    |> List.filter (pageHasNoViolation >> not)
    |> List.map correctIncorrectPage
    |> List.map _.middle
    |> List.sum
