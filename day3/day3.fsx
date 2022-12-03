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
    let fst::snd::rest = rucksack
    let value = fst |> List.filter (fun e -> List.contains e snd)
    value[0]
    
let makeDistinctAndSort elfTrio =
    List.map (fun e -> e |> List.distinct |> List.sort) elfTrio
    
let findBadge elfTrio =
    elfTrio |> List.concat |> List.groupBy (fun e -> e) |> List.filter (fun (_, values) -> List.length values = 3) |> List.map (fun (key, _) -> key)

let prepareValues = readLines >> Seq.filter checkLineNotEmpty >> Seq.map lineToValue
"input.txt" |> prepareValues |> Seq.map (fun e -> List.splitInto 2 e) |> Seq.map findItem |> Seq.sum |> printfn "Part1 = %i"

"input.txt" |> prepareValues |> Seq.chunkBySize 3 |> Seq.map Array.toList |> Seq.map makeDistinctAndSort |> Seq.map findBadge |> Seq.concat |> Seq.sum |> printfn "Part2 = %i"