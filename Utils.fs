module Utils

open System
open System.Collections.Generic

let printlist li =
    for i in li do
        printfn "%A" i

    li

let printmap (m: Dictionary<'k, 'v>) =
    for k in m.Keys do
        printfn "%A %A" k m[k]


type Array2DWithMetadata<'T> =
    { Array: 'T array2d
      Height: int
      Width: int
      MaxI: int
      MaxJ: int }

    member this.Item(i, j) = this.Array[i, j]


let sequencesToArray2D (seqArr: 'a seq array) =
    let arr2D =
        try
            array2D seqArr
        with _ ->
            failwith "Lines are not all of the same length"

    { Array = arr2D
      Height = Array2D.length1 arr2D
      Width = Array2D.length2 arr2D
      MaxJ = Array2D.length1 arr2D - 1
      MaxI = Array2D.length2 arr2D - 1 }

let asArray2D (strings: string array) =
    let arr2D =
        try
            array2D strings
        with _ ->
            failwith "Lines are not all of the same length"

    { Array = arr2D
      Height = Array2D.length1 arr2D
      Width = Array2D.length2 arr2D
      MaxJ = Array2D.length1 arr2D - 1
      MaxI = Array2D.length2 arr2D - 1 }

let isWithinBounds arr (i, j) =
    i >= 0 && arr.Width > i && j >= 0 && arr.Height > j



// Filter, but returns a tuple of the list of true and false evaluated values
// Returns: (truelist, falselist)
let filter2 predicate (s: 'a seq) : ('a list * 'a list) =
    let accumulate (accT, accF) i =
        if predicate i then i :: accT, accF else accT, i :: accF

    Seq.fold accumulate ([], []) s


let getAllPossiblePairings indexedCollection =
    let maxIdx = (List.length indexedCollection) - 1

    [ for i in [ 0..maxIdx ] do
          for j in [ i + 1 .. maxIdx ] do
              yield indexedCollection[i], indexedCollection[j] ]

let rec getCombinations uniquePicks combinationLength pool =


    match combinationLength with
    | 1 -> pool |> List.map (fun x -> [ x ])
    | _ ->
        pool
        |> List.collect (fun x ->
            getCombinations
                uniquePicks
                (combinationLength - 1)
                (if uniquePicks then
                     (List.removeAt (List.findIndex ((=) x) pool) pool)
                 else
                     pool)
            |> List.map (fun c -> x :: c))


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

let asNodeSequence (list: LinkedList<'T>) : LinkedListNode<'T> seq =
    let iterate (node: LinkedListNode<'T>) =
        if node <> null then Some(node, node.Next) else None

    list.First |> Seq.unfold iterate

let collect (mapping: 'T -> 'T array) (list: LinkedList<'T>) =
    for n in (asNodeSequence list) do
        match mapping n.Value with
        | [||] -> list.Remove(n)
        | [| a |] -> n.Value <- a
        | vals ->
            n.Value <- vals[vals.Length - 1]

            vals[0 .. vals.Length - 2]
            |> Array.iter (fun x -> list.AddBefore(n, x) |> ignore)

    list
