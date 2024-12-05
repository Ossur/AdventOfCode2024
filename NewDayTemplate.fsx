
open System.Text.RegularExpressions
open System.IO

let ( testData, realData ) = 
    let readFile filename = 
        Path.Combine(__SOURCE_DIRECTORY__, filename) |> System.IO.File.ReadAllLines 
    ( readFile "TestInput.txt" , readFile "RealInput.txt" ) 

let parsingRegex = new Regex("\d+")

