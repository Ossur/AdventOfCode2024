module Day9

open System.IO
open System
open Utils

let data =
    let readFile filename =
        Path.Combine(__SOURCE_DIRECTORY__, filename) |> System.IO.File.ReadAllLines

    readFile <| [ "TestInput.txt"; "RealInput.txt" ][1]


let parseAndExpandDiskMap () =
    let parsed = data[0] |> Seq.map (string >> int) |> List.ofSeq
    let len = List.length parsed

    // if the list ends on a space, just exclude it
    let trimmedAndParsed = if (len % 2) = 1 then parsed else parsed[0 .. (len - 2)]

    trimmedAndParsed
    |> List.mapi (fun idx n -> List.replicate n (if idx % 2 = 0 then idx / 2 else -1))
    |> List.collect id
    |> Array.ofList


let compressFully (disk: int array) =
    let rec moveBlockIfPossible spaceIdx blockIdx =
        let swap i j =
            let t = disk[i]
            disk[i] <- disk[j]
            disk[j] <- t

        if spaceIdx >= blockIdx then
            disk
        else
            match disk[spaceIdx], disk[blockIdx] with
            | -1, -1 -> moveBlockIfPossible spaceIdx (blockIdx - 1)
            | -1, _ ->
                swap spaceIdx blockIdx
                moveBlockIfPossible (spaceIdx + 1) (blockIdx - 1)
            | _, -1 -> moveBlockIfPossible (spaceIdx + 1) (blockIdx - 1)
            | _, _ -> moveBlockIfPossible (spaceIdx + 1) blockIdx

    moveBlockIfPossible 0 ((Array.length disk) - 1)

let compressWithoutFragmentation (disk: int array) =
    let diskLen = Array.length disk

    let rec moveBlockIfPossible spaceIdxStart blockIdxEnd =

        let blockIdxStart =
            let rec h i =
                if i >= 0 && disk[i] = disk[blockIdxEnd] then
                    h (i - 1)
                else
                    i + 1

            h blockIdxEnd

        let spaceIdxEnd =
            let rec h i =
                if i < diskLen && disk[i] = disk[spaceIdxStart] then
                    h (i + 1)
                else
                    i - 1

            h spaceIdxStart

        let swapRange (is, ie) (js, je) =
            let swap i j =
                let t = disk[i]
                disk[i] <- disk[j]
                disk[j] <- t

            List.zip [ is..ie ] [ js..je ] |> List.iter ((<||) swap)


        if spaceIdxEnd >= blockIdxStart then
            if blockIdxStart = 0 then
                disk
            else
                moveBlockIfPossible 0 (blockIdxStart - 1)
        else
            match disk[spaceIdxEnd], disk[blockIdxStart] with
            | -1, -1 -> moveBlockIfPossible spaceIdxStart (blockIdxStart - 1)
            | -1, _ ->
                let blockLen = blockIdxEnd - blockIdxStart
                let spaceLen = spaceIdxEnd - spaceIdxStart

                if spaceLen >= blockLen then
                    let movedSpaceIdxEnd = spaceIdxStart + blockLen
                    swapRange (spaceIdxStart, movedSpaceIdxEnd) (blockIdxStart, blockIdxEnd)
                    moveBlockIfPossible 0 (blockIdxStart - 1)
                else
                    moveBlockIfPossible (spaceIdxEnd + 1) blockIdxEnd

            | _, -1 -> moveBlockIfPossible (spaceIdxEnd + 1) (blockIdxStart - 1)
            | _, _ -> moveBlockIfPossible (spaceIdxEnd + 1) blockIdxEnd

    moveBlockIfPossible 0 ((Array.length disk) - 1)


let compressAndGetChecksum compressionAlgo =
    parseAndExpandDiskMap ()
    |> compressionAlgo
    |> Array.map (max 0) // change the -1 to 0 for the multiplication
    |> Array.mapi (fun i num -> (bigint i) * (bigint num))
    |> Array.sum


let getChecksumOfRearranged () = compressAndGetChecksum compressFully

let getChecksumOfRearrangedWithoutFragmentation () =
    compressAndGetChecksum compressWithoutFragmentation
