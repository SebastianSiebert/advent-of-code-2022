open System
open System.IO

let readLines filePath = seq { yield! File.ReadLines(filePath) }

let checkLineNotEmpty line = line |> String.IsNullOrWhiteSpace |> not

let mapCharToInt c =
    let number = int c
    match number with
    | _ when number >= 97 -> number - 96
    | _ when number >= 65 -> number - 64 + 26
    | _ -> 0

let lineToValue line = line |> Seq.map mapCharToInt |> Seq.toList

let findItem rucksack =
    let fst::snd::_ = rucksack
    let value = fst |> List.filter (fun e -> List.contains e snd)
    value[0]
    
let makeDistinctAndSort elfTrio = elfTrio |> Array.toList |> List.map List.distinct
let findBadge elfTrio = elfTrio |> List.concat |> List.countBy id |> List.filter (fun (_, values) -> values = 3) |> List.map fst

let prepareValues = readLines >> Seq.filter checkLineNotEmpty >> Seq.map lineToValue
"input.txt" |> prepareValues |> Seq.map (List.splitInto 2) |> Seq.map findItem |> Seq.sum |> printfn "Part1 = %i"

"input.txt" |> prepareValues |> Seq.chunkBySize 3 |> Seq.map makeDistinctAndSort |> Seq.map findBadge |> Seq.concat |> Seq.sum |> printfn "Part2 = %i"