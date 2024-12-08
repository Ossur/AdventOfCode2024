module Utils

open System

let printlist li =
    for i in li do
        printfn "%A" i

    li

type Array2DWithMetadata<'T> =
    { Array: 'T array2d
      Height: int
      Width: int
      MaxI: int
      MaxJ: int }

    member this.Item(i, j) = this.Array[i, j]


let asArray2D (strings: string array) =
    let arr2D =
        try
            array2D strings
        with _ ->
            failwith "Strings are not all of the same length"

    { Array = arr2D
      Height = Array2D.length1 arr2D
      Width = Array2D.length2 arr2D
      MaxJ = Array2D.length1 arr2D - 1
      MaxI = Array2D.length2 arr2D - 1 }

let isWithinBounds arr (i, j) =
    i >= 0 && arr.Width > i && j >= 0 && arr.Height > j

let getAllPossiblePairings indexedCollection =
    let maxIdx = (List.length indexedCollection) - 1

    [ for i in [ 0..maxIdx ] do
          for j in [ i + 1 .. maxIdx ] do
              yield indexedCollection[i], indexedCollection[j] ]


let printArray2D (arr: Array2DWithMetadata<char>) =
    for j in [ 0 .. (arr.Height - 1) ] do
        let s = [| for i in [ 0 .. (arr.Width - 1) ] -> arr.Array[j, i] |] |> String
        printfn $"{s}"



let array2dFilterIj (predicate: 'a -> bool) (arr: 'a array2d) : ('a * (int * int)) list =
    let w, h = Array2D.length2 arr, Array2D.length1 arr

    [ for i in [ 0 .. (h - 1) ] do
          for j in [ 0 .. (w - 1) ] do
              if predicate arr[i, j] then
                  yield (arr[i, j], (i, j)) ]
