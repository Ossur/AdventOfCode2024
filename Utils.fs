module Utils

let printlist li =
    for i in li do
        printfn $"{i}"

    li

let to2dCharArray (strings: string array) =
    let rows = Seq.length strings
    let cols = String.length (Seq.head strings)
    Array2D.init rows cols (fun i j -> strings[i][j])
